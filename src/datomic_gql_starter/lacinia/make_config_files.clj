(ns datomic-gql-starter.lacinia.make-config-files
  (:require [db :refer [db conn db-uri]]
            [datomic-gql-starter.catchpocket.generate.core :as g]
            [datomic-gql-starter.catchpocket.lib.config :as cf]
            [datomic-gql-starter.lacinia.utils :refer [all-entities refs-enums get-refs-enums]]
            [datomic-gql-starter.utils.fern :refer [catchpocket-conf stillsuit-conf root-dir api-conf]]
            [clojure.java.io :as io]))


(defn make-catchpocket-config
  "Generates 'catchpocket-config.edn' file"
  [refs-enums catchpocket-conf stillsuit-conf db]
  (with-open
    [w (clojure.java.io/writer catchpocket-conf)]
    (clojure.pprint/pprint
      (merge
        {:catchpocket/datomic-uri  db
         :catchpocket/schema-file stillsuit-conf}
        (get-refs-enums refs-enums)
        {:stillsuit/compile? true})
      w)))

(defn make-api-config []
  (let [apis-map
        {:entities
         all-entities
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

(defn make-catchpocket [] (make-catchpocket-config refs-enums catchpocket-conf stillsuit-conf  db-uri))
(defn make-stillsuit [] (g/generate-and-write! (cf/construct-config catchpocket-conf) conn db))

(defn update-config-files []
  (let [root (io/as-file root-dir)]
    (when-not (.exists root)
      (.mkdirs root))
    (make-api-config)
    (make-catchpocket)
    (make-stillsuit)))

(defn repair-config-files
  ([]
   (let [root (io/as-file root-dir)]
     (when-not (.exists root)
       (.mkdirs root))
     (when-not (.exists (io/as-file api-conf))
       (make-api-config))
     (when-not (.exists (io/as-file catchpocket-conf))
       (make-catchpocket)
       (make-stillsuit))
     (when-not (.exists (io/as-file stillsuit-conf))
       (make-stillsuit)))))

(repair-config-files)



