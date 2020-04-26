(ns datomic-gql-starter.lacinia.generate
  (:require [inflections.core :as inflections :refer [plural singular]]
           ;[datomic.api :as d] ;datomic.api
           ; [datomic.client.api :as d]                      ;datomic.client.api
            [datomic-gql-starter.lacinia.make-rules :as rules :refer [find-all-entities get-args-type]]
            [datomic-gql-starter.utils.transformations :refer [pascal-keyword camel-keyword
                                                               rmv-ns resolve-symbol make-list
                                                               make-input-name make-input-key make-query-name]]

            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [cuerdas.core :as str]
            [clojure.spec.alpha :as s]
            [catchpocket.generate.core :as cg]
            [datomic-gql-starter.lacinia.make-config-files :as refs-enums]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          api-conf max-results]]
            [clojure.set :as set]))

(def datomic-api (System/getenv "DATOMIC_API"))

(if (= datomic-api "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

(def apis
  (let [api-conf (f/get-conf :api-conf)
        all-entities (find-all-entities)
        apis (when api-conf (read-string api-conf))]
    (reduce-kv (fn [m k v] (assoc m k (if (empty? v) all-entities v)))
               {} apis)))

(def queries
  (:queries apis))

(def inserts
  (:inserts apis))

;______________________________________________________________________________________
;                   queries
;______________________________________________________________________________________



(s/def :query/operator #{'or 'and 'not})

(defn make-arg-spec [entity]
  (set (map rmv-ns (rules/find-all-fields entity))))

(defn make-nested-spec [arg-spec]
  (s/map-of :query/operator arg-spec))

(defn make-query-spec [nested-spec-name arg-spec-name]
  (s/+ (s/alt :arg arg-spec-name :nested nested-spec-name)))

(defmacro make-specs []
  "[:country/arg :country/args :country/nested :country/nested :country/query]
   [:artist/arg :artist/args :artist/nested :artist/nested :artist/query]"
  (mapv (fn [entity#]
          (let [arg-spec-name# (keyword (str entity# "/arg"))
                arg-spec# (make-arg-spec entity#)
                nested-spec-name# (keyword (str entity# "/nested"))
                args-spec-name# (keyword (str entity# "/args"))
                query-spec-name# (keyword (str entity# "/query"))]
            (vector
              `(s/def ~arg-spec-name# '~arg-spec#)
              `(s/def ~args-spec-name# (s/coll-of ~arg-spec-name#))
              `(s/def ~nested-spec-name# vector?)
              `(s/def ~nested-spec-name# (make-nested-spec ~query-spec-name#))
              `(s/def ~query-spec-name# (make-query-spec ~arg-spec-name# ~nested-spec-name#)))))
    (find-all-entities)))


(defmacro make-inputs []
  "add-countries-input
   add-artists-input
   add-languages-input"
  (mapv (fn [entity#]
          (let [input-name# (make-input-name entity#)
                arg-types# (get-args-type  entity# cg/datomic-to-lacinia)
                _# (rmv-ns '_)]
            (vector `(def ~input-name#
                       {:fields (into {}
                                  (map (fn [[field# lacinia-type#]]
                                         (hash-map (camel-keyword field#)
                                           (hash-map :type (cond
                                                             (keyword? lacinia-type#) lacinia-type#
                                                             (map? lacinia-type#)
                                                             (let [ref-name# (str (singular (:ref lacinia-type#)))]
                                                               (make-list
                                                                 (make-input-key ref-name#)))
                                                             :else (symbol lacinia-type#)))))
                                    (quote ~arg-types#)))}))))
    (find-all-entities)))


(defn make-query [entity base-name]
  (let [query-name# (pascal-keyword base-name)
        args (rules/query-args entity cg/datomic-to-lacinia)
        resolver (keyword (plural entity))
        description ""
        result-type (make-query-name entity)]
    {query-name# {:args args :description description :resolve resolver :type result-type}}))


(defmacro make-queries-resolvers []
 "[[Countries countries]
 [Artists artists]
 [Languages languages]
 [Releases releases]
 ...."
  (mapv (fn [entity#]
          (let [base-name# (plural entity#)
                query-name# (symbol (str/capital base-name#))
                resolver-name# (symbol base-name#)
                _# (rmv-ns '_)]
            (vector
              `(def ~query-name# (make-query ~entity# ~base-name#))
              `(defn ~resolver-name# [context# values# ~_#]
                 (rules/make-resolver
                   (d/db (:stillsuit/connection context#))
                   context#
                   ~entity#
                   values#)))))
    queries))



;______________________________________________________________________________________
;                   mutations
;______________________________________________________________________________________

(defn make-insert-mutation [entity name]
  (let [mutation-name# (keyword (str/camel name))
        input-name (make-input-key entity)
        args {(pascal-keyword (plural entity))
              {:type (make-list input-name)}}
        resolve (keyword name)
        type (make-query-name entity)]
    {mutation-name# {:args args :description "" :resolve resolve :type type}}))

(defn make-transaction-data [context entity arg-types data]
  (for [values data]
    (into {}
      (for [[field lacinia-type] arg-types]
        (let [arg (keyword (name field))
              value (arg values)
              arg-name (name arg)]
          ;#p [values field lacinia-type value]
          (if value
            (vector
              field
              (cond
                (string? lacinia-type)
                (rules/get-enum-value context entity arg-name value)
                (map? lacinia-type)
                (let [entity (singular arg-name)]
                  (make-transaction-data context entity
                    (get-args-type  entity cg/datomic-to-lacinia)
                    value))
                :else value))
            (when (= lacinia-type :JavaUUID)
              (vector field  (java.util.UUID/randomUUID)))))))))


(defn make-insert-resolver [context entity arg-types data]
  (let [conn (:stillsuit/connection context)
        transaction-data (make-transaction-data context entity arg-types data)
        result (if (= datomic-api "client")
                 (d/transact
                   conn
                   {:tx-data (into [] transaction-data)})
                 @(d/transact
                    conn
                    (into [] transaction-data)))]
    (mapv #(d/pull (:db-after result) '[*] (val %)) (:tempids result))))


(defmacro make-insert-inputs-mutations-resolvers []
  "add-countries-resolver
   add-countries-mutation
   add-artists-resolver
   add-artists-mutation]"
  (mapv (fn [entity#]
          (let [entity-plural# (plural entity#)
                name# (str "add-" entity-plural#)
                resolver-name# (symbol (str name# "-resolver"))
                mutation-name# (symbol (str name# "-mutation"))
                arg-types# (get-args-type  entity# cg/datomic-to-lacinia)
                _# (rmv-ns '_)]
            (vector
              `(defn ~resolver-name# [context# {data#  (keyword (str/capital ~entity-plural#))} ~_#]
                 (make-insert-resolver context# ~entity# (quote ~arg-types#) data#))
              `(def ~mutation-name#
                 (make-insert-mutation ~entity# ~name#)))))
    inserts))

(when queries
  (make-specs)
  (make-inputs)
  (make-queries-resolvers)
  (make-insert-inputs-mutations-resolvers))

;______________________________________________________________________________________
;                   maps
;______________________________________________________________________________________


(def mutation-inputs
  (->> (for [entity (find-all-entities)]
         (let [input-key (make-input-key entity)
               input-name (make-input-name entity)]
           (hash-map input-key (resolve-symbol input-name))))
    (apply merge)))

(def mutation-resolvers
  (->> (for [entity inserts]
         (let [resolver-name (str "add-" (plural entity) "-resolver")
               resolver-key (keyword (str "add-" (plural entity)))]
           (hash-map resolver-key (resolve-symbol  resolver-name))))
    (apply merge)))

(def mutation-maps
  (->> (for [entity inserts]
         (let [mutation-name (str "add-" (plural entity) "-mutation")]
           (resolve-symbol mutation-name)))
    (apply merge)))

(def query-resolvers
  (into {}
    (map
      (fn [entity]
        (let [resolver-name (plural entity)]
          (hash-map (keyword resolver-name) (resolve-symbol resolver-name))))
      queries)))

(def query-maps
  (apply merge
    (map
      (fn [entity]
        (let [query-name (str/capital (plural entity))]
          (resolve-symbol query-name)))
      queries)))


(def resolver-maps
  (->> query-resolvers
    (merge mutation-resolvers)))





