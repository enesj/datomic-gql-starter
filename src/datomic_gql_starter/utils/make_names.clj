(ns datomic-gql-starter.utils.make-names
  (:require [cuerdas.core :as str]
            [datomic-gql-starter.lacinia.utils :refer [make-list pascal-keyword]]
            [inflections.core :refer [plural]]))

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


(defn make-deletion-key
  [entity]
  (keyword (str/camel (str "delete-" (plural entity)))))

(defn make-deletion-name
  [entity]
  (symbol (str "delete-" (str/capital (plural entity)))))

(defn make-deletion-resolver-name
  [entity]
  (symbol (str "delete-" (plural entity))))

(defn make-deletion-resolver-key
  [entity]
  (keyword (str/camel (str "delete-" (str (plural entity))))))


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
