(ns datomic-gql-starter.core
  (:require [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh refresh-all]]
            [com.walmartlabs.lacinia.pedestal :as lacinia]
            [mount.core :refer [defstate] :as mount]
            [io.pedestal.http :as http]
            [stillsuit.core :as stillsuit]
            [stillsuit.lib.util :as u]
            [datomic-gql-starter.lacinia.make :as make]
            [datomic-gql-starter.utils.fern :refer [refs-conf catchpocket-conf stillsuit-conf
                                                    db-name root-dir api-conf]]
            [catchpocket.generate.core :as g]
            [catchpocket.lib.config :as cf]
            [datomic-gql-starter.utils.refs-enums :as refs-enums]
            [datomic-gql-starter.utils.db :as db-utils]
            [hawk.core :as hawk]
            [clojure.java.io :as io]))



(defn service-map
  [schema connection]
  (try
    (let [decorated (stillsuit/decorate #:stillsuit{:schema     schema
                                                    :connection connection
                                                    :resolvers  make/resolver-maps})]
      (lacinia/service-map (:stillsuit/schema decorated)
                           {:graphiql    true
                            :app-context (:stillsuit/app-context decorated)}))
    (catch Exception e
      (log/error e))))

(defn- smap []
  (try
    (let [stillsuit-config (u/load-edn-file stillsuit-conf)
          custom-config (-> stillsuit-config
                          (assoc-in [:queries] make/query-maps)
                          (assoc-in [:input-objects] make/mutation-inputs)
                          (assoc-in [:mutations] make/mutation-maps))
          s-map (-> (service-map custom-config db-utils/conn)
                  (assoc ::http/resource-path "/public"))]
      (log/infof "Connecting to datomic at %s..." db-name)
      s-map)
    (catch Exception e
      (log/error e))))

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

(defn make-catchpocket-config [refs-conf catchpocket-conf stillsuit-conf entities db]
  "Generataes 'catchpocket-config.edn' file"
  (let [refs-file-cfg (read-string (slurp refs-conf))
        res-cfg (merge (db-utils/find-enums) refs-file-cfg)]
    (with-open
      [w (clojure.java.io/writer catchpocket-conf)]
      (clojure.pprint/pprint
        (merge
          {:catchpocket/datomic-uri  db
           :catchpocket/schema-file stillsuit-conf}
          (refs-enums/get-refs-enums res-cfg entities)
          {:stillsuit/compile? true})
        w))))

(defn prepare-data
  ([]
   (let [enums (db-utils/find-enums)
         root (io/as-file root-dir)
         entities (vec (db-utils/find-all-entities))]
     (when-not (.exists root)
       (.mkdirs root))
     (when-not (.exists (io/as-file api-conf))
       (make-api-config entities))
     (when-not (.exists (io/as-file refs-conf))
       (refs-enums/save-refs-config refs-conf (into (sorted-map) enums)))
     (when-not (.exists (io/as-file catchpocket-conf))
       (make-catchpocket-config refs-conf catchpocket-conf stillsuit-conf entities db-name))
     (when-not (.exists (io/as-file stillsuit-conf))
       (g/generate-and-write! (cf/construct-config catchpocket-conf) db-utils/conn db-utils/db)))))


(defn serve-gql
  []
  (log/info {:serve "Serving graphiql at: http://localhost:8888/graphiql/index.html"})
  (log/infof "GraphQL Voyager:     http://localhost:8888/voyager/index.html")
  (log/infof "Ready.")
  (prepare-data)
  (load "lacinia/make")
  (-> (smap)
      http/create-server
      http/start))

(defstate stillsuit-app
  :start (serve-gql)
  :stop (http/stop stillsuit-app))


(defn go []
  (mount/start)
  :ready)

(defn stop []
  (mount/stop)
  :stoped)

(defn reset []
  ;(refresh-all)
  (mount/stop)
  (refresh :after 'datomic-gql-starter.core/go))

(comment
  (go)
  (reset)
  (stop))


(defn -main []
  (mount/start))




