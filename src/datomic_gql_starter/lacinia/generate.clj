(ns datomic-gql-starter.lacinia.generate
  (:require [clojure.spec.alpha :as s]
            [datomic-gql-starter.stillsuit.lib.util :refer [datomic-to-lacinia]]
            [datomic-gql-starter.lacinia.resolvers :as resolvers]
            [datomic-gql-starter.lacinia.utils
             :refer
             [all-entities all-fields args-type camel-keyword dbid-to-ns get-enum-value make-list resolve-symbol rmv-ns]]
            [datomic-gql-starter.utils.fern :as f]
            [datomic-gql-starter.utils.make-names
             :refer
             [make-args-name make-deletion-key make-deletion-name make-deletion-resolver-key make-deletion-resolver-name make-input-key make-input-name
              make-insert-key make-insert-resolver-name make-insert-resolver-key make-insert-name make-query-key make-query-name make-query-resolver-key
              make-query-resolver-name make-result-type make-update-key make-update-name make-update-resolver-key make-update-resolver-name]]
            [db :refer [d-db d-with pull transact!]]
            [inflections.core :as inflections :refer [plural singular]]))

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

(defmacro make-update-resolvers []
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

(defmacro make-delete-resolvers []
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

(defn make-insert [entity]
  (let [insert-key# (make-insert-key entity)
        input-name (make-input-key entity)
        args {(make-args-name entity)
              {:type (make-list input-name)}
              :_preview {:type 'Boolean}}
        resolver (make-insert-resolver-key entity)
        result-type (make-result-type entity)]
    {insert-key# {:args args :description "" :resolve resolver :type result-type}}))

(defmacro make-insert-resolvers
  []
  (mapv (fn [entity#]
          (let [
                insert-name# (make-insert-name entity#)
                resolver-name# (make-insert-resolver-name entity#)
                arg-types# (args-type entity# datomic-to-lacinia)
                _# (rmv-ns '_)]
            (vector
              `(defn ~insert-name# [context# {data# (make-args-name ~entity#) preview# :_preview} ~_#]
                 (resolvers/make-insert-resolver
                   (d-db (:stillsuit/connection context#))
                   context#
                   ~entity#
                   (quote ~arg-types#)
                   data#
                   preview#))
              `(def ~resolver-name#
                 (make-insert ~entity#)))))
    inserts))

;______________________________________________________________________________________
;                   make all
;______________________________________________________________________________________

(when apis
  (make-specs)
  (make-inputs)
  (make-queries-resolvers)
  (make-update-resolvers)
  (make-insert-resolvers)
  (make-delete-resolvers))

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
         (let [resolver-name (make-insert-name entity)
               resolver-key (make-insert-resolver-key entity)]
           (hash-map resolver-key (resolve-symbol  resolver-name))))
    (apply merge)))

(def insert-maps
  (->> (for [entity inserts]
         (let [mutation-name (make-insert-resolver-name entity)]
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






