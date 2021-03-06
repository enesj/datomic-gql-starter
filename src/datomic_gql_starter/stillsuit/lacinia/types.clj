(ns datomic-gql-starter.stillsuit.lacinia.types
  "Implementation functions relating to lacinia types."
  (:require [cuerdas.core :as str]
            [datomic-gql-starter.stillsuit.datomic.core :as datomic]))

(defn lacinia-type
  "Given an entity, infer its lacinia type."
  [entity config connection]
  ;(println "dddd"  (type entity))
  (let [entity-ns (datomic/guess-entity-ns entity connection)
        xform     (:stillsuit/ns-to-str config #(-> % str/kebab str/capitalize))]
    (some-> entity-ns xform keyword)))
