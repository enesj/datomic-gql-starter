(ns datomic-gql-starter.utils.refs-enums
  (:require [clojure.tools.logging :as log]
            [clojure.edn :as edn]
            [cuerdas.core :as str])
  (:use inflections.core))

(def descriptions {:catchpocket-config
                   (str/collapse-whitespace
                     "This file contains map with keys equal to idents of all entities from DB of type 'db.type/ref'.")})

(defn snake-keyword [kw]
  "converts keyword to 'snake case'"
  (keyword
    (str/join
      "_"
      [(str/snake (namespace kw))
       (str/snake (name kw))])))


(defn check-reference-to [ref all-entities]
  (let [all-entities (set all-entities)
        ref1 (last (str/split ref #"-" 0))
        ref2 (singular ref)
        ref3 (singular ref1)]
    (cond
      (all-entities ref) ref
      (all-entities ref1) ref1
      (all-entities ref2) ref2
      (all-entities ref3) ref3
      :default nil)))

(defn make-refs [data all-entities]
  "Generates :catchpocket/references part of 'catchpocket-config.edn'"
  (->>
    (for [rel data]
      (let [[backref-name referenece-to] (str/split (str rel) #"/")
            referenece-to (check-reference-to referenece-to all-entities)]
        {rel
         (zipmap [:catchpocket/reference-to :catchpocket/backref-name]
           [(keyword (singular referenece-to))
            (snake-keyword (keyword  (plural (apply str (rest backref-name)))))])}))
    (into (sorted-map))))

(defn make-enums [enums]
  "Generates :catchpocket/enums part of 'catchpocket-config.edn'"
  (let [enum-keys (mapv #(snake-keyword %) enums)
        enum-vals  (mapv #(hash-map :catchpocket.enum/attributes (hash-set %)
                            :catchpocket.enum/scan? true) enums)]
    (into (sorted-map)
      (zipmap enum-keys enum-vals))))

(defn get-refs-enums [res-cnf all-entities]
  "takes only enums or refs from 'catchpocket-config.edn' depending of 'type' argument"
  ;(let [res-cnf (dissoc (edn/read-string (slurp res-cnf-file)) :description)]
  {:catchpocket/references (make-refs
                             (->> res-cnf
                               (mapv #(when (= (second %) :ref) (first %)))
                               (remove nil?))
                             all-entities)
   :catchpocket/enums (make-enums
                        (->> res-cnf
                          (mapv #(when (= (second %) :enum) (first %)))
                          (remove nil?)))})

(defn save-refs-config [res-cnf-file refs-map]
  "prints content of 'refs-map' to 'refs-config.edn' and adds ':description' key"
  (with-open
    [w (clojure.java.io/writer res-cnf-file)]
    (clojure.pprint/pprint
      (assoc refs-map :description (:catchpocket-config descriptions))
      w)))


(defn check-refs-config [res-cnf-file refs-schema]
  "checks existance and validity of 'res-config.edn' and makes modications to it when needed
   creates this file if not exists"
  (let [refs-cnf (dissoc (edn/read-string (slurp res-cnf-file)) :description)
        refs-cnf-keys (set (keys refs-cnf))
        refs-schema-keys (set (keys refs-schema))
        cnf-refs-dif (clojure.set/difference refs-cnf-keys refs-schema-keys)
        refs-cnf-dif (clojure.set/difference refs-schema-keys refs-cnf-keys)]
    (if (every? #{:enum :ref :_missing :new}  (vals refs-cnf))
      (if (every? empty? [cnf-refs-dif refs-cnf-dif])
          "OK"
          (let [refs-schema-new (into (sorted-map) (merge refs-cnf
                                                          (zipmap cnf-refs-dif (repeat :_missing))
                                                          (zipmap refs-cnf-dif (repeat :new))))]
            (save-refs-config res-cnf-file refs-schema-new)
            "Modified"))
      "Error")))

