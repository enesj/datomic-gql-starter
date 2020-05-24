(ns datomic-gql-starter.lacinia.make-config-files
  (:require [db :refer [db conn]]
            [cuerdas.core :as str]
            [catchpocket.generate.core :as g]
            [catchpocket.lib.config :as cf]
            [datomic-gql-starter.lacinia.resolvers :as resolvers :refer [all-entities enums]]
            [datomic-gql-starter.utils.fern :as f :refer [ catchpocket-conf stillsuit-conf
                                                          db-link-peer root-dir api-conf]]
            [clojure.java.io :as io])
  (:refer inflections.core))

(defn snake-keyword
  "converts keyword to 'snake_case'"
  [kw]
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

(defn make-refs
  "Generates :catchpocket/references part of 'catchpocket-config.edn'"
  [data all-entities]
  (->>
    (for [rel data]
      (let [[backref-name reference-to] (str/split (str rel) #"/")
            reference (check-reference-to reference-to all-entities)]
        {rel
         (zipmap [:catchpocket/reference-to :catchpocket/backref-name]
           [(keyword (singular reference))
            (snake-keyword (keyword  (plural (apply str (rest backref-name)))))])}))
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
  [res-cnf all-entities]
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

(defn make-catchpocket-config
  "Generates 'catchpocket-config.edn' file"
  [refs-file-cfg catchpocket-conf stillsuit-conf entities db]
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
         []
         :updates
         []
         :deletions
         []}]
    (with-open
      [w (clojure.java.io/writer api-conf)]
      (clojure.pprint/pprint
        apis-map
        w))))

(defn make-apis [] (make-api-config all-entities))
(defn make-catchpocket [] (make-catchpocket-config enums catchpocket-conf stillsuit-conf all-entities db-link-peer))
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



