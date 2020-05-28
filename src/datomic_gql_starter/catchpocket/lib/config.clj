(ns datomic-gql-starter.catchpocket.lib.config
  (:require [datomic-gql-starter.stillsuit.lib.util :as su]))

(def default-config {:catchpocket/version           "0.1"
                     :catchpocket/schema-file       "target/catchpocket/schema.edn"
                     :catchpocket/zprint?           true
                     :stillsuit/db-id-name          :dbId
                     :stillsuit/datomic-entity-type :DatomicEntity})

(defn construct-config
  ([config-file]
   (construct-config config-file nil))
  ([config-file overrides]
   (let [cmd-line (su/load-edn-file config-file)
         cmd-line (update-in cmd-line [:catchpocket/references]
                     #(->> (filterv (fn [x] (:catchpocket/reference-to (val  x))) %)
                        (into {})))
         ;cmd-line (into sorted-map cmd-line)
         merged   (su/deep-map-merge default-config cmd-line overrides)]
     ;(println merged)
     merged)))
