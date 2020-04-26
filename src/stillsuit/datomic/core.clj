(ns stillsuit.datomic.core
  "Implementation functions for dealing with datomic interactions."
  (:require [clojure.tools.logging :as log]
            [cuerdas.core :as str]
            [datomic-gql-starter.lacinia.make-rules :as rules])
            ;[datomic.api :as d]) ;datomic.api
            ;[datomic.client.api :as d]) ;datomic.client.api
  (:import (java.util UUID)))

(if (= (System/getenv "DATOMIC_API") "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

(defn find-ident [attr db]
  (cond
    (map? attr)
    (:db/ident (d/pull db '[:db/ident] (val (first attr))))
    (vector? attr)
    (into #{} (mapv #(:db/ident (d/pull db '[:db/ident] (val (first %)))) attr))
    :else attr))

(defn reverse-to-direct [attribute]
  (->>
    (str/split (str attribute) "_")
    str/join
    rest
    str/join
    keyword))

(defn get-ref-attribute [entity attribute lacinia-type args context]
  (let [db (d/db (:stillsuit/connection context))
        direct-attribute (reverse-to-direct attribute)
        dbid (:db/id entity)
        rule ['(any ?e) ['?e direct-attribute dbid]]
        filter-entity (namespace attribute)
        [errors filter-rules] (when args (rules/get-rules db context filter-entity args '?e))]
    (if-not errors
      (let [rules  (vector (into  rule filter-rules))]
        ;#p (when args [rules])
        (if (and (= clojure.lang.PersistentList (type lacinia-type)) (= (first lacinia-type) 'list))
          (->> (d/q '[:find ?e
                      :in $ %
                      :where (any ?e)]
                 db  rules)
            (map #(hash-map :db/id (first %))))
          (attribute (d/pull db (vector attribute) (:db/id entity)))))
      {:error errors})))

(defn get-enum-attribute [entity attribute lacinia-type context]
  (-> (get-ref-attribute entity attribute lacinia-type nil context)
      (find-ident (d/db (:stillsuit/connection context)))))

;(defn entity? [thing]
;  (instance? datomic.Entity thing))

;(defn datomic-db? [thing]
;  (instance? datomic.db.Db thing))

;; TODO: handle errors here
(defn coerce-to-datomic-type
  "Given an input value as a string (lacinia type 'ID) and a datomic value type,
  coerce the input to the proper type."
  [^String input datomic-type]
  (let [xform (case datomic-type
                :db.type/string identity
                :db.type/instant #(java.util.Date/parse %)
                :db.type/bigdec bigdec
                :db.type/bigint bigint
                :db.type/long #(Long/parseLong %)
                :db.type/int #(Integer/parseInt %)
                :db.type/keyword keyword
                :db.type/uuid #(UUID/fromString %)
                (do
                  (log/errorf "Unknown datomic type %s encountered, returning '%s' as string"
                              datomic-type input)
                  identity))]
    (xform input)))


(defn get-entity-by-eid
  [db eid]
  (d/pull db '[*]
          (Long/parseLong eid)))

(defn get-entity-by-unique-attribute
  [db attribute-ident value]
  (if-let [attr-ent (d/pull db '[{:db/valueType [:db/ident]}] attribute-ident)]
    (if-let [coerced (if (string? value)
                       (coerce-to-datomic-type value (:db/ident (:db/valueType attr-ent)))
                       value)]
      (d/pull db '[*] [attribute-ident coerced])
      ;; Else coercion failed
      (log/warnf "Unable to coerce input '%s' to type %s in (get-entity-by-unique-attribute %s)"
                 value attr-ent attribute-ident))
    ;; Else atttribute not found
    (log/warnf "Attribute %s not found in (get-entity-by-unique-attribute)" attribute-ident)))

(defn guess-entity-ns
  "Given a random entity, iterate through its attributes and look for one that is marked
  as :db.unique/identity. Return the namespace of that attribute as a string."
  [entity connection]
  (let [
        db               (d/db connection)
        unique           (fn [attr-kw]
                           (let [attr-ent (d/pull db '[*] attr-kw)]
                             (when (some? (:db/unique attr-ent))
                               attr-kw)))
        unique-attribute (some->> entity
                                  keys
                                  (remove #(= (namespace %) "db"))
                                  (some unique))]
    (if (some? unique-attribute)
      (namespace unique-attribute)
      (do (log/warnf "Could not find unique attribute for:\n\n Field resolution probably won't work!!")
                     ;(d/touch entity))
          nil))))
