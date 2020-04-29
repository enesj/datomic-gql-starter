(ns datomic-gql-starter.lacinia.generate
  (:require [inflections.core :as inflections :refer [plural singular]]
           ;[datomic.api :as d] ;datomic.api
           ; [datomic.client.api :as d]                      ;datomic.client.api
            [datomic-gql-starter.lacinia.make-rules :as rules :refer [all-entities args-type]]
            [datomic-gql-starter.utils.make-names :refer [pascal-keyword camel-keyword
                                                          rmv-ns resolve-symbol make-list
                                                          make-result-type make-args-name
                                                          make-input-name make-input-key
                                                          make-query-key make-query-name
                                                          make-query-resolver-key make-query-resolver-name
                                                          make-insert-key make-insert-name
                                                          make-insert-resolver-key make-insert-resolver-name
                                                          make-update-key make-update-name
                                                          make-update-resolver-name make-update-resolver-key]]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [clojure.spec.alpha :as s]
            [catchpocket.generate.core :as cg]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          api-conf max-results]]))

(def datomic-api (System/getenv "DATOMIC_API"))

(if (= datomic-api "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

(def apis
  (let [api-conf (f/get-conf :api-conf)
        all-entities all-entities
        apis (when api-conf (read-string api-conf))]
    (reduce-kv (fn [m k v] (assoc m k (if (empty? v) all-entities v)))
               {} apis)))

(def queries
  (:queries apis))

(def inserts
  (:inserts apis))


;______________________________________________________________________________________
;                   specs
;______________________________________________________________________________________

(s/def :query/operator #{'or 'and 'not})

(defn make-arg-spec [entity]
  (set (map rmv-ns (rules/all-fields entity))))

(defn make-nested-spec [arg-spec]
  (s/map-of :query/operator arg-spec))

(defn make-query-spec [nested-spec-name arg-spec-name]
  (s/+ (s/alt :arg arg-spec-name :nested nested-spec-name)))

(defmacro make-specs []
  "[:country/arg :country/args :country/nested :country/query]
   [:artist/arg :artist/args :artist/nested :artist/query]"
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
    all-entities))

;______________________________________________________________________________________
;                   inputs
;______________________________________________________________________________________

(defmacro make-inputs []
  "country-input
   artist-input
   language-input"
  (mapv (fn [entity#]
          (let [input-name# (make-input-name entity#)
                arg-types# (args-type  entity# cg/datomic-to-lacinia)
                _# (rmv-ns '_)]
            (vector `(def ~input-name#
                       {:fields (into {}
                                  (map (fn [[field# lacinia-type#]]
                                         (hash-map (camel-keyword field#)
                                           (hash-map :type (cond
                                                             (keyword? lacinia-type#) lacinia-type#
                                                             (map? lacinia-type#)
                                                             (let [ref-name# (singular (:ref lacinia-type#))]
                                                               (make-list
                                                                 (make-input-key ref-name#)))
                                                             :else (symbol lacinia-type#)))))
                                    (quote ~arg-types#)))}))))
    all-entities))

;______________________________________________________________________________________
;                   queries
;______________________________________________________________________________________

(defn make-query [entity]
  (let [query-key# (make-query-key entity)
        args (rules/query-args entity cg/datomic-to-lacinia)
        resolver (make-query-resolver-key entity)
        description ""
        result-type (make-result-type entity)]
    {query-key# {:args args :description description :resolve resolver :type result-type}}))


(defmacro make-queries-resolvers []
 "Countries countries
  Artists artists
  Languages languages
  Releases releases
 ...."
  (mapv (fn [entity#]
          (let [query-name# (make-query-name entity#)
                resolver-name# (make-query-resolver-name entity#)
                _# (rmv-ns '_)]
            (vector
              `(def ~query-name# (make-query ~entity#))
              `(defn ~resolver-name# [context# values# ~_#]
                 (rules/make-query-resolver
                   (d/db (:stillsuit/connection context#))
                   context#
                   ~entity#
                   values#)))))
    queries))

;______________________________________________________________________________________
;                   updates
;______________________________________________________________________________________

(defn make-update [entity]
  (let [update-key# (make-update-key entity)
        args (rules/update-args entity cg/datomic-to-lacinia)
        resolver (make-update-resolver-key entity)
        result-type (make-result-type entity)]
    {update-key# {:args args :description "" :resolve resolver :type result-type}}))

(defmacro make-update-mutations-resolvers []
  (mapv (fn [entity#]
          (let [update-name# (make-update-name entity#)
                resolver-name# (make-update-resolver-name entity#)
                _# (rmv-ns '_)]
            (vector
              `(def ~update-name# (make-update ~entity#))
              `(defn ~resolver-name# [context# values# ~_#]
                 (rules/make-update-resolver
                   (d/db (:stillsuit/connection context#))
                   context#
                   ~entity#
                   values#)))))
    queries))


;______________________________________________________________________________________
;                   inserts
;______________________________________________________________________________________

(defn make-insert-mutation [entity]
  (let [mutation-key# (make-insert-key entity)
        input-name (make-input-key entity)
        args {(make-args-name entity)
              {:type (make-list input-name)}}
        resolver-key (make-insert-resolver-key entity)
        result-type (make-result-type entity)]
    {mutation-key# {:args args :description "" :resolve resolver-key :type result-type}}))



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
                    (args-type  entity cg/datomic-to-lacinia)
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
    ;#p transaction-data
    (mapv #(d/pull (:db-after result) '[*] (val %))
      (remove #(not= entity (rules/dbid-to-ns (val %))) (:tempids result)))))


(defmacro make-insert-mutations-resolvers []
  "insert-countries-resolver
   insert-countries-mutation
   insert-artists-resolver
   insert-artists-mutation]"
  (mapv (fn [entity#]
          (let [
                resolver-name# (make-insert-resolver-name entity#)
                mutation-name# (make-insert-name entity#)
                arg-types# (args-type  entity# cg/datomic-to-lacinia)
                _# (rmv-ns '_)]
            (vector
              `(defn ~resolver-name# [context# {data#  (make-args-name ~entity#)} ~_#]
                 (make-insert-resolver context# ~entity# (quote ~arg-types#) data#))
              `(def ~mutation-name#
                 (make-insert-mutation ~entity#)))))
    inserts))

;______________________________________________________________________________________
;                   make all
;______________________________________________________________________________________

(when apis
  (make-specs)
  (make-inputs)
  (make-queries-resolvers)
  (make-queries-resolvers)
  (make-update-mutations-resolvers)
  (make-insert-mutations-resolvers))

;______________________________________________________________________________________
;                   maps
;______________________________________________________________________________________

(def inputs
  (->> (for [entity all-entities]
         (let [input-name (make-input-name entity)
               input-key (make-input-key entity)]
           (hash-map input-key (resolve-symbol input-name))))
    (apply merge)))

(def insert-resolvers
  (->> (for [entity inserts]
         (let [resolver-name (make-insert-resolver-name entity)
               resolver-key (make-insert-resolver-key entity)]
           (hash-map resolver-key (resolve-symbol  resolver-name))))
    (apply merge)))

(def insert-maps
  (->> (for [entity inserts]
         (let [mutation-name (make-insert-name entity)]
           (resolve-symbol mutation-name)))
    (apply merge)))

(def update-resolvers
  (->> (for [entity inserts]
         (let [resolver-name (make-update-resolver-name entity)
               resolver-key (make-update-resolver-key entity)]
           (hash-map resolver-key (resolve-symbol  resolver-name))))
    (apply merge)))

(def update-maps
  (->> (for [entity inserts]
         (let [mutation-name (make-update-name entity)]
           (resolve-symbol mutation-name)))
    (apply merge)))

(def mutation-maps (merge insert-maps update-maps))

(def query-resolvers
  (->> (for [entity queries]
         (let [resolver-name (plural entity)
               resolver-key (make-query-resolver-key entity)]
           (hash-map resolver-key (resolve-symbol  resolver-name))))
    (apply merge)))

(def query-maps
  (->> (for [entity queries]
         (let [query-name (make-query-name entity)]
           (resolve-symbol query-name)))
    (apply merge)))

(def resolver-maps
  (->> query-resolvers
    (merge insert-resolvers)
    (merge update-resolvers)))






