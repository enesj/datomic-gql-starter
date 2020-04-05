(ns datomic-gql-starter.lacinia.make
  (:require [inflections.core :as inflections]
           ;[datomic.api :as d] ;datomic.api
           ; [datomic.client.api :as d]                      ;datomic.client.api
            [datomic-gql-starter.utils.config :as config]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [cuerdas.core :as str]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          api-conf]]
            [clojure.set :as set]))

(def datomic-api (System/getenv "DATOMIC_API"))

(if (= datomic-api "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

(defn rmv-ns [param]
  (symbol (name param)))

(defn resolve-symbol [symbol-name]
  (->>
    (ns-resolve *ns* symbol-name)
    deref))

(defn get-enum-value [context entity field value]
  (when value (let [enum (keyword (str entity "_" field))
                    result (->> context
                             :stillsuit/enum-map
                             enum
                             :stillsuit/lacinia-to-datomic
                             value)]
                result)))

(def queries
  (let [api-conf (f/get-conf :api-conf)
        queries (when api-conf (:queries (read-string (f/get-conf :api-conf))))]
    (if (empty? queries) (config/find-all-entities) queries)))

(def inserts
  (let [api-conf (f/get-conf :api-conf)
        inserts (when api-conf  (:inserts (read-string (f/get-conf :api-conf))))]
    (if (empty? inserts) (config/find-all-entities) inserts)))

;______________________________________________________________________________________
;                   queries
;______________________________________________________________________________________


(defn test-arg-values [having-fields missing-fields  special-fields args]
  (let [having-missing (set/intersection having-fields missing-fields)
        nonexistent (set/difference special-fields (set (map (comp keyword name) args)))]
    (cond-> []
      (not-empty having-missing) (conj (str "Same fields in HAVING and MISSING: " having-missing "! "))
      (not-empty nonexistent) (conj (str "Nonexistent fields: " nonexistent "! ")))))

(defn is-fulltext? [field db]
  (let [fulltext? (d/q '[:find [?e ...]
                         :in $ ?field
                         :where [?field :db/fulltext ?e]]
                    db field)]
    (when (first fulltext?) (name field))))

(defn resolve-query [db context entity values args]
  (let [missing-fields (set (map keyword (:_missing values)))
        having-fields (set (map keyword (:_having values)))
        special-fields (reduce into [ missing-fields having-fields])
        test-values (test-arg-values having-fields missing-fields  special-fields args)]
    ;#p [args values]
    (when values
      (if (empty? test-values)
        (let [remove-fields (into #{:_having :_missing :_limit } special-fields)
              base-values (into {} (remove #(remove-fields (key %)) values))

              fulltext-fields (remove nil? (map (comp #(is-fulltext? % db) keyword
                                                  #(str/join "/" [(name entity) %]) name key) base-values))

              base-no-fulltext (into {} (remove #((set (map keyword fulltext-fields)) (key %)) base-values))
              all-values (apply merge base-no-fulltext (map #(hash-map (keyword %) '_) (:_having values)))
              limit (:_limit values)
              vals (reduce-kv (fn [m k v]
                                (assoc m (keyword entity (name k))
                                         (if (keyword? v) (get-enum-value context entity (name k) v)
                                                          v)))
                     {} all-values)
              fulltext-vals (mapv (fn [value]
                                    [(concat '(fulltext $) [(keyword entity value) ((keyword value) values)])
                                     ['[?e _ _ _]]])
                              fulltext-fields)
              missing-vals (mapv (fn [value] [(concat '(missing? $ ?e) [(keyword entity value)])])
                             (:_missing values))]
          ;#p [fulltext-fields base-no-fulltext base-no-fulltext]
          (let [
                filter (as-> vals $
                         (mapv #(vector '?e (key %) (val %)) $)
                         (into (vector '(any ?e)) $)
                         (into $ fulltext-vals)
                         (into $ missing-vals)
                         (vector $))]


            ;#p filter
            (let [eid (if (= datomic-api "client")
                        (d/q {:query  '[:find (pull ?e [*])
                                        :in $ %
                                        :where (any ?e)]
                              :args   [db filter]
                              :limit  (or limit -1)})
                        (d/q '[:find (pull ?e [*] )
                               :in $ %
                               :where (any ?e)]
                              db filter))]
              ;#p eid
              (config/query-ellipsis eid))))
        (resolve-as nil {:message (apply str test-values)
                         :status  404})))))

(defn make-query [entity base-name arg-types]
  (let [query-name# (keyword (str/pascal base-name))
        args (into {:_having   {:type '(list String)} :_missing {:type '(list String)} :_limit {:type :JavaLong}}
               (for [[field lacinia-type] arg-types]
                 {(keyword (str/camel (name field)))
                  (hash-map :type (if (keyword? lacinia-type) lacinia-type (symbol lacinia-type)))}))
        resolve (keyword (inflections/plural entity))
        description ""
        result-type (list (symbol "list") (keyword (str/pascal entity)))]
    {query-name# {:args args :description description :resolve resolve :type result-type}}))

(defn get-args-type [args#]
  (let [enums# (config/find-enums)
        arg-types# (mapv
                     (fn [arg#]
                       (let [[type# ref?#] (config/get-attr-type arg#)
                             type-modified# (if ref?#
                                              (if (= (arg# enums#) :ref)
                                                nil
                                                (str/snake (str arg#)))
                                              type#)]
                         [arg# type-modified#]))
                     args#)]
    (remove (comp not second) arg-types#)))


(defmacro make-queries-resolvers []
  (mapv (fn [entity#]
          (let [base-name# (inflections/plural entity#)
                query-name# (symbol (str/capital base-name#))
                resolver-name# (symbol base-name#)
                args# (config/find-all-fields entity#)
                arg-types# (get-args-type args#)
                _# (rmv-ns '_)]
            (vector
              `(def ~query-name# (make-query ~entity# ~base-name# (quote ~arg-types#)))
              `(defn ~resolver-name# [context# values# ~_#]
                 (resolve-query
                   (d/db (:stillsuit/connection context#))
                   context#
                   ~entity#
                   values#
                   ~args#)))))
    queries))
;______________________________________________________________________________________
;                   mutations
;______________________________________________________________________________________

(defn make-insert-mutation [entity name]
  (let [mutation-name# (keyword (str/camel name))
        input-name (keyword (str/pascal (str entity "-input")))
        args {(keyword (str/pascal (inflections/plural entity)))
              {:type (list (symbol "list") input-name)}}
        resolve (keyword name)
        type (list (symbol "list") (keyword  (str/pascal (str entity))))]
    {mutation-name# {:args args :description "" :resolve resolve :type type}}))

(defn make-insert-resolver [context entity arg-types data]
  (let [conn (:stillsuit/connection context)
        transaction-data (for [values data]
                           (into {}
                             (for [[field lacinia-type] arg-types]
                               (let [arg (keyword (name field))
                                     value (arg values)
                                     arg-name (name arg)]
                                 (if value
                                   (vector
                                     field
                                     (if (string? lacinia-type)
                                       (get-enum-value context entity arg-name value)
                                       value))
                                   (when (= lacinia-type :JavaUUID) (vector field (java.util.UUID/randomUUID))))))))
        result (d/transact
                 conn
                 {:tx-data (into [] transaction-data)})]
    (mapv #(d/pull (:db-after result) '[*] (val %)) (:tempids result))))


(defmacro make-insert-inputs-mutations-resolvers []
  (mapv (fn [entity#]
          (let [args# (config/find-all-fields entity#)

                entity-plural# (inflections/plural entity#)
                name# (str "add-" entity-plural#)
                resolver-name# (symbol (str name# "-resolver"))
                input-name# (symbol (str name# "-input"))
                mutation-name# (symbol (str name# "-mutation"))
                arg-types# (get-args-type args#)
                _# (rmv-ns '_)]
            ;#p entity-plural#
            (vector `(def ~input-name#
                       {:fields (into {}
                                  (map (fn [[field# lacinia-type#]]
                                         (hash-map (keyword (str/camel (name field#)))
                                           (hash-map :type (if (keyword? lacinia-type#) lacinia-type# (symbol lacinia-type#)))))
                                    (quote ~arg-types#)))})
              `(defn ~resolver-name# [context# {data#  (keyword (str/capital ~entity-plural#))} ~_#]
                 (make-insert-resolver context# ~entity# (quote ~arg-types#) data#))
              `(def ~mutation-name#
                 (make-insert-mutation ~entity# ~name#)))))
    inserts))

(when queries
  (make-queries-resolvers)
  (make-insert-inputs-mutations-resolvers))

;______________________________________________________________________________________
;                   maps
;______________________________________________________________________________________


(def mutation-inputs
  (->> (for [entity inserts]
         (let [input-key (keyword (str/pascal (str entity "-input")))
               input-name (symbol (str "add-" (inflections/plural entity) "-input"))]
           ;#p input-name
           (hash-map input-key (resolve-symbol input-name))))
    (apply merge)))

(def mutation-resolvers
  (->> (for [entity inserts]
         (let [resolver-name (str "add-" (inflections/plural entity) "-resolver")
               resolver-key (keyword (str "add-" (inflections/plural entity)))]
           (hash-map resolver-key (resolve-symbol (symbol resolver-name)))))
    (apply merge)))

(def mutation-maps
  (->> (for [entity inserts]
         (let [mutation-name (str "add-" (inflections/plural entity) "-mutation")]
           (resolve-symbol (symbol mutation-name))))
    (apply merge)))

(def query-resolvers
  (into {}
    (map
      (fn [entity]
        (let [resolver-name (inflections/plural entity)]
          (hash-map (keyword resolver-name) (resolve-symbol (symbol resolver-name)))))
      queries)))

(def query-maps
  (apply merge
    (map
      (fn [entity]
        (let [query-name (str/capital (inflections/plural entity))]
          (resolve-symbol (symbol query-name))))
      queries)))


(def resolver-maps
  (->> query-resolvers
    (merge mutation-resolvers)))





