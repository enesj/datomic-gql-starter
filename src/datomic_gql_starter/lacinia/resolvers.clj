(ns datomic-gql-starter.lacinia.resolvers
  (:require [db :refer [db d-with transact! q]]
            [inflections.core :refer [singular]]
            [cuerdas.core :as str]
            [datomic-gql-starter.lacinia.utils :refer  [enum-value query-ellipsis camel-keyword namespaced-keyword args-type]]
            [datomic-gql-starter.utils.make-names :refer [make-input-key]]
            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [datomic-gql-starter.utils.fern :refer [max-results]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.zip :as z]
            [clojure.set :as set]))

(set! s/*explain-out* expound/printer)

(defn make-args [default entity datomic-to-lacinia]
  (let [arg-types (args-type entity datomic-to-lacinia)]
    (into default
      (for [[field lacinia-type] arg-types]
        {(camel-keyword (name field))
         (hash-map :type
           (cond
             (keyword? lacinia-type) (list (symbol "list") lacinia-type)
             (map? lacinia-type)
             (make-input-key (singular (:ref lacinia-type)))
             :else (list (symbol "list") (symbol lacinia-type))))}))))

(defn query-args
  [entity datomic-to-lacinia]
  (make-args {:_limit {:type :JavaLong} :_composition {:type 'String}} entity datomic-to-lacinia))


(defn update-args
  [entity datomic-to-lacinia]
  (make-args {:_limit {:type :JavaLong} :_update {:type (make-input-key entity)} :_preview {:type 'Boolean}
              :_composition {:type 'String}} entity datomic-to-lacinia))

(defn delete-args
  [entity datomic-to-lacinia]
  (make-args {:_limit {:type :JavaLong} :_delete {:type (make-input-key entity)} :_preview {:type 'Boolean}
              :_composition {:type 'String}} entity datomic-to-lacinia))




(defn is-fulltext? [k entity db]
  (let [field (keyword (str/join "/" [entity (name k)]))
        fulltext? (q '[:find ?e
                       :in $ ?field
                       :where [?field :db/fulltext ?e]]
                     db field)]
    (when (seq fulltext?) (name field))))

(defn get-or-filter [k v entity context e]
  (let [val (mapv #(if (keyword? %)
                     (enum-value context entity (name k) %)
                     %) (remove nil? v))
        attribute (namespaced-keyword entity k)]
      (vector (cons 'or (mapv #(vector e attribute %) val)))))

(defn get-range-filter [k v entity m context e]
  (let [v (if (keyword? v)
            (enum-value context entity (name k) v)
            v)
        attribute-val (symbol (str "?" (name k)))
        attribute (namespaced-keyword entity k)]
    (remove nil? (apply conj m [e attribute attribute-val]
                   (when (first v) [(list '>= attribute-val (first v))])
                   (when (second v) [[(list '<= attribute-val (second v))]])))))

(defn get-has-filter [k entity e]
  (let [attribute (namespaced-keyword entity k)]
    [e attribute]))

(defn get-missing-filter [k entity e]
  (let [attribute (namespaced-keyword entity k)]
    (concat '(missing? $) [e] [attribute])))

(defn get-equal-filter [k v entity context e]
  (let [v (if (keyword? v)
            (enum-value context entity (name k) v)
            v)]
    [e (namespaced-keyword entity k) v]))

(defn get-fulltext-filter [k v entity e]
  [(concat '(fulltext $) [(namespaced-keyword entity k) v])
   [`[~e _ _ _]]])

(declare get-rules)

(defn get-input-filter
  [k v entity e context param]
  ['or-join [e]
   (reduce (fn [result val]
             (into result [val]))
     ['and ['?e (namespaced-keyword entity k) param]]
     (second (let [sub-entity (name k)
                   values v]
               (get-rules db context sub-entity values param))))])

(defn get-filter-type [arg entity context db e]
  (let [[k v] (first arg)
        param (symbol (str "?" (name k)))]
    (cond
      (map? v)
      (vary-meta (get-input-filter k v entity e context param) assoc :type :input)
      (or (not (coll? v)) (= (count v) 1))
      (let [v (if (coll? v) (first v) v)]
        (if (or v (not-empty v))
          (if (is-fulltext? k entity db)
            (vary-meta (get-fulltext-filter k v entity e) assoc :type :full)
            (vary-meta (get-equal-filter k (if (coll? v) (first v) v) entity context e) assoc :type :equal))
          (vary-meta (get-missing-filter k entity e) assoc :type :missing)))

      (empty? v)
      (vary-meta (get-missing-filter k entity e) assoc :type :missing)

      (= (count v) 2)
      (if (empty? (remove nil? v))
        (vary-meta (get-has-filter k entity e) assoc :type :has)
        (vary-meta (get-range-filter k v entity [] context e) assoc :type :range))

      (> (count v) 2) (vary-meta (get-or-filter k v entity context e) assoc :type :or)
      :else [k v])))

(defn make-simple-filter [arg values entity context db e]
  (when-let [val (select-keys values [(keyword arg)])]
    (when (seq val)
      (get-filter-type val entity context db e))))

(defn coerce-missing-filter [result]
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
                       :input filter
                       :missing [filter]
                       :subfilter filter))))
        missing-coerced (coerce-missing-filter result)]
    (if (> (count operators) 1)
      missing-coerced
      [[missing-coerced]])))

(defn make-composite-filter [operators arg values entity context db e]
  (let [[operator fields] (first arg)
        operators (conj operators operator)
        filters (for [field fields]
                  (if (map? field)
                    (make-composite-filter operators field values entity context db e)
                    (make-simple-filter field values entity context db e)))
        composite-filter (apply-operator operators filters)]
    composite-filter))

(defn compose-filters [composition-list values entity context db e]
  (remove nil? (reduce
                 (fn [filters arg]
                   (let [composite? (map? arg)
                         filter
                         (if composite?
                           (reduce into
                             (make-composite-filter nil arg values entity context db e))
                           (let [simple-filter (make-simple-filter arg values entity context db e)
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

(defn get-rules [db context entity values e]
  (let [entity (singular entity)
        query-spec (keyword (str entity "/query"))
        args (vec (remove #{'_limit '_composition '_update '_preview } (mapv (comp symbol name key) values)))
        composition (try (read-string (or (:_composition values) (str args)))
                         (catch Exception e (str "caught exception: " (.getMessage e))))
        not-defined (set/difference (find-symbols composition) (into (set args) #{'or 'not 'and}))
        arg-error (when (not-empty not-defined)
                    (str "Value is not defined for arguments: " not-defined))
        query-error (when (= ::s/invalid (s/conform query-spec composition))
                      (s/explain-str query-spec composition))
        errors (or arg-error query-error)]
    [errors (compose-filters composition values entity context db e)]))


(defn make-query-resolver [db context entity values]
  (let [[errors rules] (get-rules db context entity values '?e)]
    (if errors
      (resolve-as nil {:message errors :status 404})
      (let [rules (vector (into (vector '(any ?e)) rules))
            limit (or (:_limit values) max-results)
            results (q '[:find (pull ?e [*])
                         :in $ %
                         :where (any ?e)]
                      db rules)
            modified-results (query-ellipsis results)]
        (take limit modified-results)))))

(defn make-update-tx-data
  [query-result updates entity]
  (let [update (into {} (filter val updates))
        remove (vec (remove val updates))
        db-ids (map :db/id query-result)
        namespaced-update  (reduce-kv (fn [m k v] (assoc m (namespaced-keyword entity k) v))
                             {} update)
        remove-data
        (when (seq remove) (first (mapv (fn [v]
                                          (let [field (namespaced-keyword entity (first v))]
                                            (for [query-result query-result]
                                              [:db/retract (:db/id query-result) field (field query-result)])))
                                    remove)))
        update-data (when (seq update) (for [id db-ids]
                                         (merge {:db/id id} namespaced-update)))]
    (concat update-data (filter last remove-data))))

(defn make-update-resolver
  [db context entity values]
  (let [updates (:_update values)
        preview (:_preview values)
        query-result (make-query-resolver db context entity values)]
    (if updates
      (let [tx-data  (make-update-tx-data query-result updates entity)]
        (if preview
          (let [db-after  (:db-after (d-with tx-data db))]
            (make-query-resolver db-after context entity values))
          (let [db-after  (:db-after (transact! tx-data))]
            (make-query-resolver db-after context entity values))))
      query-result)))


(defn make-deletion-resolver
  [db context entity values]
  (let [preview (:_preview values)
        query-result (make-query-resolver db context entity values)
        db-ids (map :db/id query-result)
        tx-data  (mapv  #(vector :db/retractEntity %)   db-ids)]
       (if preview
         (let [db-after  (:db-after (d-with tx-data db))]
              (make-query-resolver db-after context entity values))
         (let [db-after  (:db-after (transact! tx-data))]
              (make-query-resolver db-after context entity values)))))