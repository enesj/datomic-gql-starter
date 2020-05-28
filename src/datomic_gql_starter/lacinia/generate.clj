(ns datomic-gql-starter.lacinia.generate
  (:require [inflections.core :as inflections :refer [plural singular]]
            [db :refer [d-with transact! pull d-db]]
            [datomic-gql-starter.lacinia.utils :refer [camel-keyword rmv-ns resolve-symbol make-list all-entities all-fields get-enum-value dbid-to-ns args-type]]
            [datomic-gql-starter.lacinia.resolvers :as resolvers]
            [datomic-gql-starter.utils.make-names :refer [make-result-type make-args-name
                                                          make-input-name make-input-key
                                                          make-query-key make-query-name
                                                          make-query-resolver-key make-query-resolver-name
                                                          make-insert-key make-insert-name
                                                          make-insert-resolver-key make-insert-resolver-name
                                                          make-update-key make-update-name
                                                          make-update-resolver-name make-update-resolver-key
                                                          make-deletion-key make-deletion-resolver-key
                                                          make-deletion-name make-deletion-resolver-name]]
            [clojure.spec.alpha :as s]
            [datomic-gql-starter.catchpocket.generate.core :refer [datomic-to-lacinia]]
            [datomic-gql-starter.utils.fern :as f]))

(def apis
  (let [api-conf (f/get-conf :api-conf)
        apis (if api-conf (read-string api-conf) {:entities
                                                  all-entities
                                                  :queries
                                                  []
                                                  :inserts
                                                  []
                                                  :updates
                                                  []
                                                  :deletions
                                                  []})]
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
  (set (map rmv-ns (all-fields entity))))

(defn make-nested-spec [arg-spec]
  (s/map-of :query/operator arg-spec))

(defn make-query-spec [nested-spec-name arg-spec-name]
  (s/+ (s/alt :arg arg-spec-name :nested nested-spec-name)))

(defmacro make-specs
  "[:country/arg :country/args :country/nested :country/query]
   [:artist/arg :artist/args :artist/nested :artist/query]"
  []
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

(defmacro make-inputs
  "country-input
   artist-input
   language-input"
  []
  (mapv (fn [entity#]
          (let [input-name# (make-input-name entity#)
                arg-types# (args-type  entity# datomic-to-lacinia)
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
        args (resolvers/query-args entity datomic-to-lacinia)
        resolver (make-query-resolver-key entity)
        description ""
        result-type (make-result-type entity)]
    {query-key# {:args args :description description :resolve resolver :type result-type}}))

(defmacro make-queries-resolvers
  "Countries countries
  Artists artists
  Languages languages
  Releases releases
 ...."
  []
  (mapv (fn [entity#]
          (let [query-name# (make-query-name entity#)
                resolver-name# (make-query-resolver-name entity#)
                _# (rmv-ns '_)]
            (vector
              `(def ~query-name# (make-query ~entity#))
              `(defn ~resolver-name# [context# values# ~_#]
                 (resolvers/make-query-resolver
                   (d-db (:stillsuit/connection context#))
                   context#
                   ~entity#
                   values#)))))
    queries))

;______________________________________________________________________________________
;                   updates
;______________________________________________________________________________________

(defn make-update [entity]
  (let [update-key# (make-update-key entity)
        args (resolvers/update-args entity datomic-to-lacinia)
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
                 (resolvers/make-update-resolver
                   (d-db (:stillsuit/connection context#))
                   context#
                   ~entity#
                   values#)))))
    queries))

;______________________________________________________________________________________
;                   deletions
;______________________________________________________________________________________


(defn make-deletion [entity]
  (let [update-key# (make-deletion-key entity)
        args (resolvers/delete-args entity datomic-to-lacinia)
        resolver (make-deletion-resolver-key entity)
        result-type (make-result-type entity)]
    {update-key# {:args args :description "" :resolve resolver :type result-type}}))

(defmacro make-delete-mutations-resolvers []
  (mapv (fn [entity#]
          (let [delete-name# (make-deletion-name entity#)
                resolver-name# (make-deletion-resolver-name entity#)
                _# (rmv-ns '_)]
            (vector
              `(def ~delete-name# (make-deletion ~entity#))
              `(defn ~resolver-name# [context# values# ~_#]
                 (resolvers/make-deletion-resolver
                   (d-db (:stillsuit/connection context#))
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
              {:type (make-list input-name)}
              :_preview {:type 'Boolean}}
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
          (if value
            (vector
              field
              (cond
                (string? lacinia-type)
                (get-enum-value context entity arg-name value)
                (map? lacinia-type)
                (let [entity (singular arg-name)]
                  (make-transaction-data context entity
                    (args-type  entity datomic-to-lacinia)
                    value))
                :else value))
            (when (= lacinia-type :JavaUUID)
              (vector field  (java.util.UUID/randomUUID)))))))))

(defn make-insert-resolver [context entity arg-types data]
  (let [conn (:stillsuit/connection context)
        db (d-db conn)
        tx-data (make-transaction-data context entity arg-types data)
        result (if (:_preview data)
                 (d-with tx-data db)
                 (transact! tx-data))
        tempids (:tempids result)
        db-after (:db-after result)]
    (mapv #(pull db-after '[*] (val %))
      (remove #(not= entity (dbid-to-ns db-after (val %))) tempids))))

(defmacro make-insert-mutations-resolvers
  "insert-countries-resolver
   insert-countries-mutation
   insert-artists-resolver
   insert-artists-mutation]"
  []
  (mapv (fn [entity#]
          (let [
                resolver-name# (make-insert-resolver-name entity#)
                mutation-name# (make-insert-name entity#)
                arg-types# (args-type  entity# datomic-to-lacinia)
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
  (make-update-mutations-resolvers)
  (make-insert-mutations-resolvers)
  (make-delete-mutations-resolvers))

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


(def deletion-resolvers
  (->> (for [entity inserts]
         (let [resolver-name (make-deletion-resolver-name entity)
               resolver-key (make-deletion-resolver-key entity)]
           (hash-map resolver-key (resolve-symbol  resolver-name))))
    (apply merge)))

(def deletion-maps
  (->> (for [entity inserts]
         (let [mutation-name (make-deletion-name entity)]
           (resolve-symbol mutation-name)))
    (apply merge)))


(def mutation-maps (merge insert-maps update-maps
                     deletion-maps))

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
    (merge update-resolvers)
    (merge deletion-resolvers)))






