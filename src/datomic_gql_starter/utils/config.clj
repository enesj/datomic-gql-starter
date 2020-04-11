(ns datomic-gql-starter.utils.config
  (:require [datomic-gql-starter.utils.db :as db-utils :refer [db conn]]
            [cuerdas.core :as str]
    ;[catchpocket.generate.core :as cg]
    ;[catchpocket.generate.core :as g]
    ;[catchpocket.lib.config :as cf]
    ;[datomic-gql-starter.utils.refs-enums :as refs-enums]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          db-link root-dir api-conf max-results]]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def datomic-api (System/getenv "DATOMIC_API"))

(defn query-ellipsis [x]
  ;#p x
  (if (coll? (first x))
    (mapv first x)
    x))

(if (= (System/getenv "DATOMIC_API") "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

(def rules '[[(remove-ns ?ns)
              (not [(clojure.string/starts-with? ?ns "db")])
              (not [(clojure.string/starts-with? ?ns "sys")])
              (not [(clojure.string/starts-with? ?ns "fressian")])
              (not [(clojure.string/starts-with? ?ns "deprecated")])]])

(defn get-attr-type [attr datomic-to-lacinia]
  (let [type (-> (d/q '[:find ?t
                        :in $ ?v
                        :where
                        [?e :db/ident ?v]
                        [?e :db/valueType ?type-id]
                        [?type-id :db/ident ?t]]
                   db attr)
               ffirst
               datomic-to-lacinia)]
    [type (= type :catchpocket.generate.core/ref)]))

(defn find-all-refs []
  "gets all entities from DB of type ':dbex.type/ref' "
  (-> (d/q '[:find ?ident
             :in $ %
             :where
             [?e :db/ident ?ident]
             [?e :db/valueType :db.type/ref]
             [(namespace ?ident) ?ns]
             (remove-ns ?ns)]
        db rules)
    query-ellipsis))

(defn find-enums []
  (let [refs (find-all-refs)]
    (into (sorted-map)
      (for [ref refs]
        (vector ref
          (->>
            (d/q '[:find  (first ?a)
                   :in $ ?ref
                   :where
                   [_ ?ref ?w]
                   [?w :db/ident ?a]]
              db ref)
            (#(if (not-empty %) :enum :ref))))))))

(defn find-all-entities []
  (->>
    (d/q '[:find ?ns
           :in $ %
           :where
           [?e :db/ident ?ident]
           [?e :db/valueType]
           [(namespace ?ident) ?ns]
           (remove-ns ?ns)]
      db rules)
    query-ellipsis
    (remove #(str/includes? % "."))
    vec))


(defn find-all-fields [entity]
  (->>
    (d/q '[:find ?ident
           :in $ ?entity
           :where
           [?e :db/ident ?ident]
           [?e :db/valueType]
           [(namespace ?ident) ?ns]
           [(= ?ns ?entity)]]
      db entity)
    query-ellipsis))

(defn get-args-type [entity datomic-to-lacinia]
  (let [args (find-all-fields entity)
        enums (find-enums)
        arg-types (mapv
                    (fn [arg]
                      (let [[type ref?] (get-attr-type arg datomic-to-lacinia)
                            type-modified (if ref?
                                            (if (= (arg enums) :ref)
                                              nil
                                              (str/snake (str arg)))
                                            type)]
                        [arg type-modified]))
                    args)]
    (remove (comp not second) arg-types)))


(defn query-args [entity datomic-to-lacinia]
  (let [arg-types (get-args-type  entity datomic-to-lacinia)]
    ;#p arg-types
    (into {:_having   {:type '(list String)} :_missing {:type '(list String)} :_limit {:type :JavaLong}}
      (for [[field lacinia-type] arg-types]
        {(keyword (str/camel (name field)))
         (hash-map :type (if (string? lacinia-type)
                           (symbol lacinia-type)
                           (list (symbol "list") (symbol lacinia-type))))}))))

(defn get-enum-value [context entity field value]
  (when value (let [enum (keyword (str/snake (str entity "_" field)))
                    result (->> context
                             :stillsuit/enum-map
                             enum
                             :stillsuit/lacinia-to-datomic
                             value)]
                result)))

(defn test-arg-values [having-fields missing-fields  special-fields args]
  (let [having-missing (set/intersection having-fields missing-fields)
        nonexistent (set/difference special-fields (set (map (comp keyword name) args)))]
    (cond-> []
      (not-empty having-missing) (conj (str "Same fields in HAVING and MISSING: " (mapv symbol (into '() having-missing )) "! "))
        (not-empty nonexistent) (conj (str "Nonexistent fields: " (mapv symbol (into '() nonexistent )) "! ")))))

(defn is-fulltext? [field db]
  (let [fulltext? (d/q '[:find [?e ...]
                         :in $ ?field
                         :where [?field :db/fulltext ?e]]
                    db field)]
    (when (first fulltext?) (name field))))

(defn split-map [values]
  (reduce-kv (fn [m k v]
                 (if (and (coll? v) (> (count v) 1))
                   (assoc-in m [:double k] v)
                   (assoc-in m [:single k] v)))
    {} values))

(defn update-vals [m f]
  (reduce-kv (fn [m k v]
               (assoc m k (if (coll? v) (f v) v))) {} m))

(defn get-double-vales [vals entity]
  (remove nil?
    (reduce-kv
       (fn [m k v]
         (let [attribute-val (symbol (str "?" (name k)))
               attribute (keyword entity (name k))]
           ( apply conj m ['?e attribute attribute-val] [(list '>= attribute-val (first v) )] [[(list '<= attribute-val (second v))]])))
       [] vals)))

(defn make-rules [db context entity values]
  (let [args (find-all-fields entity)
        missing-fields (set (map keyword (:_missing values)))
        having-fields (set (map keyword (:_having values)))
        special-fields (reduce into [missing-fields having-fields])
        test-values (test-arg-values having-fields missing-fields  special-fields args)]
    (if (empty? test-values)
      (let [remove-fields (into #{:_having :_missing :_limit} special-fields)
            base-values (into {} (remove #(remove-fields (key %)) values))
            split-values (split-map base-values)
            single-values (update-vals (:single split-values) first)
            double-values (get-double-vales (:double split-values) entity)

            fulltext-fields (remove nil? (map (comp #(is-fulltext? % db) keyword
                                                #(str/join "/" [(name entity) %]) name key) single-values))
            base-no-fulltext (into {} (remove #((set (map keyword fulltext-fields)) (key %)) single-values))
            all-values (apply merge base-no-fulltext (map #(hash-map (keyword %) '_) (:_having values)))
            vals (reduce-kv (fn [m k v]
                              (assoc m (keyword entity (name k))
                                       (if (keyword? v) (get-enum-value context entity (name k) v)
                                                        v)))
                   {} all-values)
            ;_ #p all-values
            fulltext-vals (mapv (fn [value]
                                  [(concat '(fulltext $) [(keyword entity value) (first ((keyword value) values))])
                                   ['[?e _ _ _]]])
                            fulltext-fields)
            missing-vals (mapv (fn [value] [(concat '(missing? $ ?e) [(keyword entity value)])])
                           (:_missing values))
            filter (as-> vals $
                     (mapv #(if (= (val %) '_)
                              (vector '?e (key %))
                              (vector '?e (key %) (val %)))
                       $)
                     (into $ double-values)
                     #p $
                     (into $ fulltext-vals)
                     (into $ missing-vals))]

        ;(when (not-empty vals) #p vals)
        ;(when (not-empty fulltext-vals) #p fulltext-vals)
        ;(when (not-empty filter) #p  filter)

        [nil  filter])
      [test-values nil])))


(defn resolve-query [db context entity values]
  (let [[test-values filter] (make-rules db context entity values)
        rules (vector (into (vector '(any ?e)) filter))]
    #p entity
    (if-not test-values
     (let [limit (or (:_limit values) max-results)
           results (if (= datomic-api "client")
                     (d/q {:query '[:find (pull ?e [*])
                                    :in $ %
                                    :where (any ?e)]
                           :args  [db rules]
                           :limit limit})
                     (d/q '[:find (pull ?e [*])
                            :in $ %
                            :where (any ?e)]
                       db rules))
           modified-results (query-ellipsis results)]
       (if (coll? modified-results)
         (take limit modified-results)
         modified-results))
     (resolve-as nil {:message (apply str test-values)
                      :status  404}))))





