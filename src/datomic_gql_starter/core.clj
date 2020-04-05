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
                                                      db-link root-dir api-conf]]
              [datomic-gql-starter.utils.db :as db-utils]
              [datomic-gql-starter.utils.config :as config]))

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
      (log/infof "Connecting to datomic at %s..." db-link)
      s-map)
    (catch Exception e
      (log/error e))))


(defn serve-gql
  []
  (log/info {:serve "Serving graphiql at: http://localhost:8888/graphiql/index.html"})
  (log/infof "GraphQL Voyager:     http://localhost:8888/voyager/index.html")
  (log/infof "Ready.")
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
  (mount/stop)
  (refresh :after 'datomic-gql-starter.core/go))

(defn reset-config []
  (config/update-config-files)
  (reset))

(comment
  (go)
  (reset)
  (stop))


(defn -main []
  (mount/start))




