(ns datomic-gql-starter.stillsuit.datomic.core
  "Implementation functions for dealing with datomic interactions."
  (:require [clojure.tools.logging :as log]
            [cuerdas.core :as str]
            [inflections.core :refer [singular]]
            [datomic-gql-starter.lacinia.utils :as utils]
            [datomic-gql-starter.catchpocket.generate.core :as cgc]
            [datomic-gql-starter.catchpocket.generate.datomic :as cgd]
            [db :refer [pull d-db q]]
            [datomic-gql-starter.lacinia.resolvers :as resolvers])
  (:import (java.util UUID)))

(defn find-ident [attr db]
  (cond
    (map? attr)
    (:db/ident (pull db '[:db/ident] (val (first attr))))
    (vector? attr)
    (set (mapv #(:db/ident (pull db '[:db/ident] (val (first %)))) attr))
    :else attr))

(defn reverse-to-direct [attribute]
  (->>
    (str/split (str attribute) "_")
    str/join
    rest
    str/join
    keyword))

(defn get-sub-entity
  [attribute]
  (if (second (utils/get-attr-type attribute cgc/datomic-to-lacinia))
      (singular (name attribute))
      (namespace attribute)))

(def attr-entities (cgd/attr-entities db/db))

(defn get-cardinality
  [field]
  (->> (first (filter #(= field (:db/ident %)) attr-entities))
    :db/cardinality))

(defn get-ref-attribute [entity attribute args context]
  (let [db (d-db (:stillsuit/connection context))
        direct-attribute (reverse-to-direct attribute)
        sub-entity (get-sub-entity attribute)
        dbid (:db/id entity)
        rule ['(any ?e) (if (= direct-attribute attribute)
                          [dbid direct-attribute '?e]
                          ['?e direct-attribute dbid])]
        [errors filter-rules] (when args (resolvers/get-rules db context sub-entity args '?e))
        rules (vector (into rule filter-rules))
        cardinality (get-cardinality attribute)
        type (utils/get-attr-type direct-attribute cgc/datomic-to-lacinia)]
    (if-not errors
        (if (= :ref (second type))
          (if (= cardinality :db.cardinality/one)
            (->> (q '[:find ?e
                      :in $ %
                      :where (any ?e)]
                   db  rules)
              ffirst
              (hash-map :db/id))
            (->> (q '[:find ?e
                      :in $ %
                      :where (any ?e)]
                   db  rules)
              (map #(hash-map :db/id (first %)))))
          (or
            (attribute entity)
            (when-not (and (= (namespace attribute) (utils/dbid-to-ns db (:db/id entity))) (> (count entity) 1))
                      (attribute (pull db (vector attribute) (:db/id entity))))))
      {:error errors})))

(defn get-enum-attribute [entity attribute  context]
  (-> (get-ref-attribute entity attribute  nil context)
      (find-ident (d-db (:stillsuit/connection context)))))

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
  pull db '[*]
          (Long/parseLong eid))

(defn get-entity-by-unique-attribute
  [db attribute-ident value]
  (if-let [attr-ent (pull db '[{:db/valueType [:db/ident]}] attribute-ident)]
    (if-let [coerced (if (string? value)
                       (coerce-to-datomic-type value (:db/ident (:db/valueType attr-ent)))
                       value)]
      (pull db '[*] [attribute-ident coerced])
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
        db               (d-db connection)
        unique           (fn [attr-kw]
                           (let [attr-ent (pull db '[*] attr-kw)]
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
