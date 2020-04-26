(ns datomic-gql-starter.utils.transformations
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
  (symbol (str "add-" (plural entity) "-input")))

(defn make-query-name
  [entity]
  (make-list (pascal-keyword entity)))