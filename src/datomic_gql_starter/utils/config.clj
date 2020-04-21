(ns datomic-gql-starter.utils.config
  (:require [datomic-gql-starter.utils.db :as db-utils :refer [db conn]]
            [cuerdas.core :as str]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          db-link root-dir api-conf max-results]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.zip :as z]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(set! s/*explain-out* expound/printer)

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
  "gets all entities from DB of type ':db.type/ref' "
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
    (into {:_limit {:type :JavaLong} :_composition {:type 'String}}
      (for [[field lacinia-type] arg-types]
        {(keyword (str/camel (name field)))
         (hash-map :type (list (symbol "list") (symbol lacinia-type)))}))))

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
    [ '?e attribute]))

(defn get-missing-filter [k entity]
  (let [attribute (keyword entity (name k))]
    (concat '(missing? $ ?e) [attribute])))

(defn get-equal-filter [k v entity context]
  (let [v (if (keyword? v)
            (get-enum-value context entity (name k) v)
            v)]
    ['?e
     (keyword entity (name k))
     v]))

(defn get-fulltext-filter [k v entity]
    [(concat '(fulltext $) [(keyword entity  (name k)) v])
     ['[?e _ _ _]]])

(defn get-filter-type [arg entity context db]
  (let [[k v] (first arg)]
    (cond
      (or (not (coll? v)) (= (count v) 1))
      (let [v (if (coll? v) (first v) v)]
        (if (or v (not-empty v))
          (if (is-fulltext? k entity db)
            (vary-meta (get-fulltext-filter k v entity) assoc :type :full)
            (vary-meta (get-equal-filter k (if (coll? v) (first v) v) entity context) assoc :type :equal))
          (vary-meta (get-missing-filter k entity) assoc :type :missing)))

      (empty? v)
      (vary-meta (get-missing-filter k entity) assoc :type :missing)

      (= (count v) 2)
      (if (empty? (remove nil? v))
        (vary-meta (get-has-filter k entity) assoc :type :has)
        (vary-meta (get-range-filter k v entity [] context) assoc :type :range))

      (> (count v) 2) (vary-meta (get-or-filter k v entity context) assoc :type :or)
      :else [k v])))


(defn make-simple-filter [arg values entity context db]
  (if-let [val (select-keys values [(keyword arg)])]
    (when-not (empty? val)
      (get-filter-type val entity context db))))


(defn update-operator [operator filters]
  (with-meta filters {:operator operator}))

(defn coerce-missing-filter [ filters result]
      (meta (first (second result)))
  (if (and (= (count result) 2)
        (= (meta (first (second result))) {:type :missing}))
      [(first result) '[?e] (second result)]
      result))

(defn apply-operator [operators filters]
  (let [operator (first operators)
        result (cons operator
                 (for [filter filters]
                   (let [type (or (:type (meta filter)) :subfilter)]
                     (case type
                       :range (reduce (fn [result val]
                                        (into result (list 'or-join ['?e] (cons 'and val))))
                                [] [filter])
                       :or (first filter)
                       :equal filter
                       :full filter
                       :has filter
                       :missing [filter]
                       :subfilter filter))))
        missing-coerced (coerce-missing-filter filters result)]
    (if (> (count operators) 1)
      missing-coerced
      [[missing-coerced]])))



(defn make-composite-filter [operators arg values entity context db]
  (let [[operator fields] (first arg)
        operators (conj operators operator)
        ;_ #p [arg "/" operator "/" operators]
        filters   (for [field fields]
                    (if (map? field)
                        (make-composite-filter operators field values entity context db)
                        (make-simple-filter field values entity context db)))
        composite-filter (apply-operator operators filters)]
    ;#p composite-filter
    composite-filter))

(defn compose-filters [composition-list values entity context db]
    (remove nil? (reduce
                   (fn [filters arg]
                     (let [composite? (map? arg)
                           filter
                           (if composite?
                             (reduce (fn [result val] (into result val))
                               (make-composite-filter nil arg values entity context db))
                             (let [simple-filter (make-simple-filter arg values entity context db)
                                   type (:type (meta simple-filter))]
                               (case type
                                 :range
                                 simple-filter
                                 :or
                                 simple-filter
                                 :missing
                                 [[simple-filter]]
                                 [simple-filter])))]
                       (into filters filter)))
                   [] composition-list)))


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
  (let [all-filters (get-filter-types (into {} (remove #(#{:_limit :_composition} (key %)) values)) db entity)
        equal-filters (get-equal-filters (:equal all-filters) entity context)
        range-filters (get-range-filters (:range all-filters) entity context)
        or-filters (get-or-filters (:or all-filters) entity context)
        missing-filters (get-missing-filters (:missing all-filters) entity)
        has-filters (get-has-filters (:has all-filters) entity)
        fulltext-filters (get-fulltext-filters (:fulltext all-filters) entity)
        filter (-> equal-filters
                 (into range-filters)
                 (into missing-filters)
                 (into has-filters)
                 (into or-filters)
                 (into fulltext-filters))]
    filter))

(defn find-symbols [coll]
  (let [cc (z/zipper coll? seq nil coll)]
    (loop [x cc
           result []]
      (if-not (z/end? x)
        (recur (z/next x)
          (if (= (type (z/node x)) clojure.lang.Symbol)
            (conj result (z/node x))
            (set result)))
        result))))


(defn get-rules [db context entity values]
  ;#p (when (= entity "track") [values entity])
  (let [query-spec (keyword (str entity "/query"))
        args (remove #{'_limit '_composition} (mapv (comp symbol name key) values))
        composition (try (read-string (or (:_composition values) (str args)))
                         (catch Exception e (str "caught exception: " (.getMessage e))))
        not-defined (set/difference (find-symbols composition) (into (set args) #{'or 'not 'and}))
        arg-error (when (not-empty  not-defined)
                    (str "Value is not defined for arguments: " not-defined))
        query-error (when (= ::s/invalid (s/conform query-spec composition))
                      (s/explain-str query-spec composition))
        errors (or arg-error query-error)]
    ;#p (when (= entity "track") [query-spec composition args])
    [errors (compose-filters composition values entity context db)]))

(defn resolve-query [db context entity values]
  (let [[errors rules] (get-rules db context entity values)]
    (if errors
      (resolve-as nil {:message errors :status  404})
      (let [rules (vector (into (vector '(any ?e)) rules))
            limit (or (:_limit values) max-results)
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
          modified-results)))))
;(resolve-as nil {:message "testing"
  ;                 :status  404}))






