(ns datomic-gql-starter.utils.make-names
  (:require [cuerdas.core :as str]
            [inflections.core :as inflections :refer [plural singular]]))

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

(defn query-ellipsis [x]
  (if (coll? (first x))
    (mapv first x)
    x))

(defn add-namespace
  [entity field]
  (keyword entity (name field)))


(def remove-ns-rules '[[(remove-ns ?ns)
                        (not [(clojure.string/starts-with? ?ns "db")])
                        (not [(clojure.string/starts-with? ?ns "sys")])
                        (not [(clojure.string/starts-with? ?ns "fressian")])
                        (not [(clojure.string/starts-with? ?ns "deprecated")])]])

(defn make-input-key
  [entity]
  (pascal-keyword (str entity "-input")))

(defn make-input-name
  [entity]
  (symbol (str entity "-input")))

(defn make-result-type
  [entity]
  (make-list (pascal-keyword entity)))

(defn make-query-key
  [entity]
  (pascal-keyword (plural entity)))

(defn make-query-name
  [entity]
  (symbol (str/capital (plural entity))))

(defn make-query-resolver-name
  [entity]
  (symbol (plural entity)))

(defn make-query-resolver-key
  [entity]
  (keyword (plural entity)))



(defn make-update-key
  [entity]
  (keyword (str/camel (str "update-" (plural entity)))))

(defn make-update-name
  [entity]
  (symbol (str "update-" (str/capital (plural entity)))))

(defn make-update-resolver-name
  [entity]
  (symbol (str "update-" (plural entity))))

(defn make-update-resolver-key
  [entity]
  (keyword (str/camel (str "update-" (str (plural entity))))))


(defn make-insert-key
  [entity]
  (keyword (str/camel (str "insert-" (plural entity)))))

(defn make-insert-name
  [entity]
  (symbol (str "insert-" (str/capital (plural entity)))))

(defn make-insert-resolver-name
  [entity]
  (symbol (str "insert-" (plural entity))))

(defn make-insert-resolver-key
  [entity]
  (keyword (str/camel (str "insert-" (str (plural entity))))))

(defn make-args-name
  [entity]
  (pascal-keyword (plural entity)))