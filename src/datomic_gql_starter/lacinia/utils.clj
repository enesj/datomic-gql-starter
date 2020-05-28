(ns datomic-gql-starter.lacinia.utils
  (:require
    [db :refer [db q]]
    [cuerdas.core :as str]
    [datomic-gql-starter.utils.fern :as f :refer [catchpocket-conf]]
    [datomic-gql-starter.catchpocket.lib.config :as cf]
    [inflections.core :refer [singular plural]]
    [clojure.java.io :as io]))

(def remove-ns-rules '[[(remove-ns ?ns)
                        (not [(clojure.string/starts-with? ?ns "db")])
                        (not [(clojure.string/starts-with? ?ns "sys")])
                        (not [(clojure.string/starts-with? ?ns "fressian")])
                        (not [(clojure.string/starts-with? ?ns "deprecated")])]])

(defn query-ellipsis [x]
  (if (coll? (first x))
    (mapv first x)
    x))

(defn rmv-ns [param]
  (symbol (name param)))

(defn resolve-symbol [symbol-name]
  (let [symbol (if (symbol? symbol-name)
                 symbol-name
                 (symbol symbol-name))]
    (->>
      (ns-resolve *ns* symbol)
      deref)))

(defn pascal-keyword
  [arg]
  (keyword (str/pascal arg)))

(defn camel-keyword
  [field]
  (keyword (str/camel (name field))))

(defn make-list
  [body]
  (list (symbol "list") body))

(defn namespaced-keyword
  [entity field]
  (keyword entity (name field)))

(defn get-unique [entity]
  (->> (q '[:find ?ident
            :in $ ?ns
            :where
            [?e :db/ident ?ident]
            [?e :db/unique]
            [(namespace ?ident) ?ns]]
         db entity)
    ffirst))

(defn dbid-to-ns [db dbid]
  (->> (q '[:find   ?ns
            :in $ ?e
            :where
            [?e ?a]
            [?a :db/unique]
            [?a :db/ident ?v]
            [(namespace ?v) ?ns]]
         db dbid)
    ffirst))

(defn find-ref
  [entity]
  (->> (q '[:find ?e
            :in $ ?entity
            :where [_ ?entity ?e]]
         db entity)
    ffirst
    (dbid-to-ns db)))

(defn find-all-refs
  "gets all entities from DB of type ':db.type/ref' "
  []
  (-> (q '[:find ?ident
           :in $ %
           :where
           [?e :db/ident ?ident]
           [?e :db/valueType :db.type/ref]
           [(namespace ?ident) ?ns]
           (remove-ns ?ns)]
        db remove-ns-rules)
    query-ellipsis))

(def all-refs (find-all-refs))

(defn find-refs-enums []
  (into (sorted-map)
    (for [ref all-refs]
      (vector ref
        (->>
          (q '[:find (first ?a)
               :in $ ?ref
               :where
               [_ ?ref ?w]
               [?w :db/ident ?a]]
            db ref)
          (#(if (not-empty %) :enum :ref)))))))

(def refs-enums (find-refs-enums))

(defn find-all-entities []
  (->>
    (q '[:find ?ns
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
    (q '[:find ?ident
         :in $ ?entity
         :where
         [?e :db/ident ?ident]
         [?e :db/valueType]
         [(namespace ?ident) ?ns]
         [(= ?ns ?entity)]]
      db entity)
    query-ellipsis))

(def all-fields (memoize find-all-fields))

(defn get-enum-value [context entity field value]
  (when value (let [enum (keyword (str/snake (str entity "_" field)))
                    result (->> context
                             :stillsuit/enum-map
                             enum
                             :stillsuit/lacinia-to-datomic
                             value)]
                result)))

(def enum-value (memoize get-enum-value))

(defn snake-keyword
  "converts keyword to 'snake_case'"
  [kw]
  (keyword
    (str/join
      "_"
      [(str/snake (namespace kw))
       (str/snake (name kw))])))

(defn check-reference-to [ref]
  (let [all-entities (set all-entities)
        ref1 (last (str/split ref #"-" 0))
        ref2 (singular ref)
        ref3 (singular ref1)]
    (cond
      (all-entities ref) ref
      (all-entities ref1) ref1
      (all-entities ref2) ref2
      (all-entities ref3) ref3
      :else nil)))

(defn make-refs
  "Generates :catchpocket/references part of 'catchpocket-config.edn'"
  [data]
  (->>
    (for [ref data]
      (let [[entity reference] (str/split (str ref) #"/")
            reference-to (or (find-ref ref) (check-reference-to reference))]
        {ref
         (zipmap [:catchpocket/reference-to :catchpocket/backref-name]
           [(keyword (singular reference-to))
            (snake-keyword (keyword (plural (apply str (rest entity)))))])}))
    (into (sorted-map))))

(defn make-enums
  "Generates :catchpocket/enums part of 'catchpocket-config.edn'"
  [enums]
  (let [enum-keys (mapv snake-keyword enums)
        enum-vals  (mapv #(hash-map :catchpocket.enum/attributes (hash-set %)
                            :catchpocket.enum/scan? true) enums)]
    (into (sorted-map)
      (zipmap enum-keys enum-vals))))

(defn get-refs-enums
  "takes only enums or refs from 'catchpocket-config.edn' depending of 'type' argument"
  [refs-enums]
  ;(let [res-cnf (dissoc (edn/read-string (slurp res-cnf-file)) :description)]
  {:catchpocket/references (make-refs
                             (->> refs-enums
                               (mapv #(when (= (second %) :ref) (first %)))
                               (remove nil?)))

   :catchpocket/enums      (make-enums
                             (->> refs-enums
                               (mapv #(when (= (second %) :enum) (first %)))
                               (remove nil?)))})

(def config (if (.exists (io/as-file catchpocket-conf))
              (cf/construct-config catchpocket-conf)
              (get-refs-enums refs-enums)))

(defn ref-type [attr]
  (->> config
    :catchpocket/references
    attr
    :catchpocket/reference-to))

(defn get-attr-type [attr datomic-to-lacinia]
  (let [type (-> (q '[:find ?t
                      :in $ ?v
                      :where
                      [?e :db/ident ?v]
                      [?e :db/valueType ?type-id]
                      [?type-id :db/ident ?t]]
                   db attr)
               ffirst
               datomic-to-lacinia)]
    [(if  (= (name type) "ref") (ref-type attr) type)
     (when  (= (name type) "ref") (attr refs-enums))]))

(def attr-type (memoize get-attr-type))

(defn get-args-type [entity datomic-to-lacinia]
  (let [args (all-fields entity)]
    (mapv
      (fn [arg]
        (let [[type ref?] (attr-type arg datomic-to-lacinia)
              type-modified (case ref?
                              :ref
                              {:ref (name type)}
                              :enum
                              (str/snake (str arg))
                              type)]
          [arg type-modified]))
      args)))

(def args-type (memoize get-args-type))