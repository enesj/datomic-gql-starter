(ns datomic-gql-starter.lacinia.make-config-files
  (:require [datomic-gql-starter.utils.db :as db-utils :refer [db conn]]
            [cuerdas.core :as str]
            [catchpocket.generate.core :as cg]
            [catchpocket.generate.core :as g]
            [catchpocket.lib.config :as cf]
            [datomic-gql-starter.lacinia.make-rules :as rules]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          db-link root-dir api-conf]]
            [clojure.java.io :as io])
  (:use inflections.core))

(def descriptions {:catchpocket-config
                   (str/collapse-whitespace
                     "This file contains map with keys equal to indents of all entities from DB of type 'db.type/ref'.")})

(defn snake-keyword [kw]
  "converts keyword to 'snake_case'"
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
      (let [[backref-name reference-to] (str/split (str rel) #"/")
            reference (check-reference-to reference-to all-entities)]
        {rel
         (zipmap [:catchpocket/reference-to :catchpocket/backref-name]
           [(keyword (singular reference))
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

(defn make-catchpocket-config [refs-file-cfg catchpocket-conf stillsuit-conf entities db]
  "Generates 'catchpocket-config.edn' file"
  (with-open
    [w (clojure.java.io/writer catchpocket-conf)]
    (clojure.pprint/pprint
      (merge
        {:catchpocket/datomic-uri  db
         :catchpocket/schema-file stillsuit-conf}
        (get-refs-enums refs-file-cfg entities)
        {:stillsuit/compile? true})
      w)))

(defn make-api-config [entities]
  (let [apis-map
        {:entities
         entities
         :queries
         []
         :inserts
         []}]
    (with-open
      [w (clojure.java.io/writer api-conf)]
      (clojure.pprint/pprint
        apis-map
        w))))

(defn make-apis [] (make-api-config (rules/find-all-entities)))
(defn make-catchpocket [] (make-catchpocket-config (rules/find-enums) catchpocket-conf stillsuit-conf (rules/find-all-entities) db-link))
(defn make-stillsuit [] (g/generate-and-write! (cf/construct-config catchpocket-conf) conn db))

(defn update-config-files []
  (let [root (io/as-file root-dir)]
    (when-not (.exists root)
      (.mkdirs root))
    (make-apis)
    (make-catchpocket)
    (make-stillsuit)))

(defn repair-config-files
  ([]
   (let [root (io/as-file root-dir)]
     (when-not (.exists root)
       (.mkdirs root))
     (when-not (.exists (io/as-file api-conf))
       (make-apis))
     (when-not (.exists (io/as-file catchpocket-conf))
       (make-catchpocket)
       (make-stillsuit))
     (when-not (.exists (io/as-file stillsuit-conf))
       (make-stillsuit)))))

(repair-config-files)



