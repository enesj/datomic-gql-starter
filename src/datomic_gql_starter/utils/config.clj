(ns datomic-gql-starter.utils.config
  (:require [datomic-gql-starter.utils.db :as db-utils :refer [db conn]]
            [cuerdas.core :as str]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          db-link root-dir api-conf max-results]]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def datomic-api (System/getenv "DATOMIC_API"))

(defn query-ellipsis [x]
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
    (into {:_limit {:type :JavaLong}}
      (for [[field lacinia-type] arg-types]
        {(keyword (str/camel (name field)))
         (hash-map :type (list (symbol "list") (symbol lacinia-type)))}))))
           ;(if (string? lacinia-type)
           ;  (symbol lacinia-type)
           ;  (list (symbol "list") (symbol lacinia-type))))}))))

(defn get-enum-value [context entity field value]
  (when value (let [enum (keyword (str/snake (str entity "_" field)))
                    result (->> context
                             :stillsuit/enum-map
                             enum
                             :stillsuit/lacinia-to-datomic
                             value)]
                result)))

(defn is-fulltext? [k entity db]
  (let [field (keyword (str/join "/" [entity (name k)]))
        fulltext? (d/q '[:find [?e ...]
                         :in $ ?field
                         :where [?field :db/fulltext ?e]]
                    db field)]
    (when (first fulltext?) (name field))))

(defn get-filter-types [values db entity]
  (reduce-kv (fn [m k v]
               (if (and (coll? v) (> (count v) 1))
                 (if (> (count v) 2)
                   (assoc-in m [:or k] v)
                   (if (empty? (remove nil? v))
                     (assoc-in m [:has k] v)
                     (assoc-in m [:range k] v)))
                 (if (or (and (coll? v) (first v))
                       (and (not (coll? v))  v))
                   (if (is-fulltext? k entity db)
                     (assoc-in m [:fulltext k] v)
                     (assoc-in m [:equal k] v))
                   (assoc-in m [:missing k] v))))
    {} values))

(defn get-or-filter [k v entity context]
  (let [val (mapv #(if (keyword? %)
                     (get-enum-value context entity (name k) %)
                     %) (remove nil? v))]
    (let [attribute (keyword entity (name k))]
      (vector (cons 'or (mapv #(vector '?e attribute %) val))))))

(defn get-range-filter [k v entity m context]
  (let [
        v (if (keyword? v)
            (get-enum-value context entity (name k) v)
            v)
        attribute-val (symbol (str "?" (name k)))
        attribute (keyword entity (name k))]
    (remove nil? (apply conj m ['?e attribute attribute-val]
                   (when (first v) [(list '>= attribute-val (first v))])
                   (when (second v) [[(list '<= attribute-val (second v))]])))))

(defn get-has-filter [k entity]

  (let [attribute (keyword entity (name k))]
    (vector '?e attribute)))

(defn get-missing-filter [k entity]
  (let [attribute (keyword entity (name k))]
    [(concat '(missing? $ ?e) [attribute])]))

(defn get-equal-filter [k v entity context]
  (let [v (if (keyword? v)
            (get-enum-value context entity (name k) v)
            v)]
    (vector '?e
      (keyword entity (name k))
      v)))

(defn get-fulltext-filter [k v entity]
    [(concat '(fulltext $) [(keyword entity  (name k)) v])
     ['[?e _ _ _]]])

(defn get-filter-type [filter entity context db]
  ;#p filter
  (let [[k v] (first filter)]
    ;#p [k v]
    (cond
      (or (not (coll? v)) (= (count v) 1))
      (let [v (if (coll? v) (first v) v)]
        #p (is-fulltext? k entity db)
        (if v
          (if (is-fulltext? k entity db)
            (get-fulltext-filter k v entity)
            (get-equal-filter k (if (coll? v) (first v) v) entity context))
          (get-missing-filter k entity)))

      (= (count v) 2)
      (if (empty? (remove nil? v))
        (get-has-filter k entity)
        (get-range-filter k v entity [] context))

      (> (count v) 2) (get-or-filter k v entity context)
      :else [k v])))

(defn get-equal-filters [vals entity context]
    (for [[k v] vals]
      (get-equal-filter k (if (coll? v) (first v) v) entity context)))

(defn get-range-filters [vals entity context]
    (reduce-kv
       (fn [m k v]
         (get-range-filter k v entity m context))
       [] vals))

(defn get-or-filters [vals entity context]
  (vec (remove nil?
         (reduce-kv
           (fn [m k v]
             (get-or-filter k v entity context))
           [] vals))))

(defn get-has-filters [vals entity]
  (for [val vals]
    (get-has-filter (key val) entity)))

(defn get-missing-filters [vals entity]
  (for [val vals]
    (get-missing-filter  (key val) entity)))

(defn get-fulltext-filters [vals entity]
  (mapv (fn [val]
          (let [[k [v]]  val]
            (get-fulltext-filter k v entity)))
    vals))

(defn make-rules [db context entity values]
  (let [all-filters (get-filter-types (into {} (remove #(= :_limit (key %)) values)) db entity)
        equal-filters (get-equal-filters (:equal all-filters) entity context)
        range-filters (get-range-filters (:range all-filters) entity context)
        or-filters (get-or-filters (:or all-filters) entity context)
        missing-filters (get-missing-filters (:missing all-filters) entity)
        has-filters (get-has-filters (:has all-filters) entity)
        fulltext-filters (get-fulltext-filters (:fulltext all-filters) entity)
        ;_ #p equal-filters
        ;_ #p  range-filters
        filter (-> equal-filters
                 (into range-filters)
                 (into missing-filters)
                 (into has-filters)
                 (into or-filters)
                 (into fulltext-filters))]
    #p all-filters
    #p filter
    (def context1 context)
    (def db1 db)
    filter))


(defn resolve-query [db context entity values]
  (let [filter (make-rules db context entity values)
        rules (vector (into (vector '(any ?e)) filter))]
    ;#p rules
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
         modified-results))))






