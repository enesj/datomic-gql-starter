(ns datomic-gql-starter.lacinia.make-rules
  (:require [datomic-gql-starter.utils.db :as db-utils :refer [db conn]]
            [inflections.core :as inflections :refer [plural singular]]
            [cuerdas.core :as str]
            [datomic-gql-starter.utils.make-names :refer [pascal-keyword camel-keyword
                                                          make-input-key
                                                          make-input-name make-list
                                                          rmv-ns resolve-symbol
                                                          query-ellipsis add-namespace remove-ns-rules]]

            [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          db-link root-dir api-conf max-results]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.zip :as z]
            [clojure.set :as set]))

(set! s/*explain-out* expound/printer)

(def datomic-api (System/getenv "DATOMIC_API"))

(if (= (System/getenv "DATOMIC_API") "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))


(defn get-unique [entity]
  (->> (d/q '[:find ?ident
              :in $ ?ns
              :where
              [?e :db/ident ?ident]
              [?e :db/unique]
              [(namespace ?ident) ?ns]]
          db entity)
    ffirst))


(defn dbid-to-ns [dbid]
  (->> (d/q '[:find   ?ns
              :in $ ?e
              :where
              [?e ?a]
              [?a :db/unique]
              [?a :db/ident ?v]
              [(namespace ?v) ?ns]]
          db dbid)
    ffirst))


(defn find-all-refs []
  "gets all entities from DB of type ':db.type/ref' "
  (-> (d/q '[:find ?ident
             :in $ %
             :where
             [?e :db/ident ?ident]
             [?e :db/valueType :db.type/ref]
             [(namespace ?ident) ?ns]
             (remove-ns ?ns)]
        db remove-ns-rules)
    query-ellipsis))

(def all-refs (find-all-refs))

(defn find-enums []
  (into (sorted-map)
    (for [ref all-refs]
      (vector ref
        (->>
          (d/q '[:find (first ?a)
                 :in $ ?ref
                 :where
                 [_ ?ref ?w]
                 [?w :db/ident ?a]]
            db ref)
          (#(if (not-empty %) :enum :ref)))))))

(def enums (find-enums))

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
    [type (when  (= type :catchpocket.generate.core/ref) (attr enums))]))

(def attr-type (memoize get-attr-type))

(defn find-all-entities []
  (->>
    (d/q '[:find ?ns
           :in $ %
           :where
           [?e :db/ident ?ident]
           [?e :db/valueType]
           [(namespace ?ident) ?ns]
           (remove-ns ?ns)]
       db remove-ns-rules)
    query-ellipsis
    (remove #(str/includes? % "."))
    vec))

(def all-entities (find-all-entities))


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

(def all-fields (memoize find-all-fields))


(defn get-args-type [entity datomic-to-lacinia]
  (let [args (all-fields entity)]
    (mapv
      (fn [arg]
        (let [[type ref?] (attr-type arg datomic-to-lacinia)
              type-modified (case ref?
                              :ref
                               {:ref (name arg)}
                               :enum
                               (str/snake (str arg))
                              type)]
          [arg type-modified]))
      args)))

(def args-type (memoize get-args-type))


(defn make-args [default entity datomic-to-lacinia]
  (let [arg-types (args-type entity datomic-to-lacinia)]
    (into default
      (for [[field lacinia-type] arg-types]
        {(camel-keyword (name field))
         (hash-map :type
           (cond
             (keyword? lacinia-type) (list (symbol "list") lacinia-type)
             (map? lacinia-type)
             (keyword (str/pascal (str (singular (:ref lacinia-type)) "-input")))
             :else (list (symbol "list") (symbol lacinia-type))))}))))

(defn query-args
  [entity datomic-to-lacinia]
  (make-args {:_limit {:type :JavaLong} :_composition {:type 'String}} entity datomic-to-lacinia))


(defn update-args
  [entity datomic-to-lacinia]
  (make-args {:_limit {:type :JavaLong} :_update {:type (make-input-key entity)} :_preview {:type 'Boolean}
              :_composition {:type 'String}} entity datomic-to-lacinia))


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

(defn get-or-filter [k v entity context e]
  (let [val (mapv #(if (keyword? %)
                     (get-enum-value context entity (name k) %)
                     %) (remove nil? v))]
    (let [attribute (add-namespace entity k)]
      (vector (cons 'or (mapv #(vector e attribute %) val))))))

(defn get-range-filter [k v entity m context e]
  (let [v (if (keyword? v)
            (get-enum-value context entity (name k) v)
            v)
        attribute-val (symbol (str "?" (name k)))
        attribute (add-namespace entity k)]
    (remove nil? (apply conj m [e attribute attribute-val]
                   (when (first v) [(list '>= attribute-val (first v))])
                   (when (second v) [[(list '<= attribute-val (second v))]])))))

(defn get-has-filter [k entity e]
  (let [attribute (add-namespace entity k)]
    [e attribute]))

(defn get-missing-filter [k entity e]
  (let [attribute (add-namespace entity k)]
    (concat '(missing? $) [e] [attribute])))

(defn get-equal-filter [k v entity context e]
  (let [v (if (keyword? v)
            (get-enum-value context entity (name k) v)
            v)]
    [e
     (add-namespace entity k)
     v]))

(defn get-fulltext-filter [k v entity e]
  [(concat '(fulltext $) [(add-namespace entity k) v])
   [`[~e _ _ _]]])

(declare get-rules)

(defn get-filter-type [arg entity context db e]
  (let [[k v] (first arg)]
    ;#p [k v]
    (cond
      (map? v)
      ['or-join [e]
       (reduce (fn [result val]
                 (into result [val]))
         '[and [?e :release/artists ?artists]]
         (second (let [entity (name k)
                       values v]
                   (get-rules db context entity values (symbol (str "?"(name k)))))))]
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
  (if-let [val (select-keys values [(keyword arg)])]
    (when-not (empty? val)
      (get-filter-type val entity context db e))))

(defn coerce-missing-filter [filters result]
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
  ;#p composition-list
  (remove nil? (reduce
                 (fn [filters arg]
                   (let [composite? (map? arg)
                         filter
                         (if composite?
                           (reduce (fn [result val] (into result val))
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
        args (into [] (remove #{'_limit '_composition '_update '_preview } (mapv (comp symbol name key) values)))
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
            ;_ #p rules
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

(defn make-update-resolver
  [db context entity values]
  (let [updates (:_update values)
        preview (:_preview values)
        query-result (make-query-resolver db context entity values)
        tx-data   (for [id (map :db/id query-result)]
                      (merge {:db/id id} (reduce-kv (fn [m k v ] (assoc m (add-namespace entity k) v))
                                           {} updates)))
        ;_    #p tx-data
        db-after  (:db-after (if (= datomic-api "client")
                               (d/with
                                  db
                                 {:tx-data (into [] tx-data)})
                               (d/with
                                  db
                                 (into [] tx-data))))
        result  (make-query-resolver db-after context entity values)]
    ;#p result
    result))



