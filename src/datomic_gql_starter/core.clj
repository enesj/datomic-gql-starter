(ns datomic-gql-starter.core
  (:require [clojure.tools.logging :as log]
            [datomic-gql-starter.lacinia.make-config-files :as config-files]
            [clojure.tools.namespace.repl :refer [refresh-all]]
            [com.walmartlabs.lacinia.pedestal :as lacinia-pedestal]
            [mount.core :refer [defstate] :as mount]
            [io.pedestal.http :as http]
            [datomic-gql-starter.stillsuit.core :as stillsuit]
            [datomic-gql-starter.stillsuit.lib.util :as u]
            [db :refer [conn] :as db]
            [datomic-gql-starter.lacinia.generate :as generate]
            [datomic-gql-starter.utils.fern :refer [ stillsuit-conf]]))



(defn service-map
  [schema connection]
  (try
    (let [decorated (stillsuit/decorate #:stillsuit{:schema     schema
                                                    :connection connection
                                                    :resolvers  generate/resolver-maps})]
      (lacinia-pedestal/service-map (:stillsuit/schema decorated)
                           {:graphiql    true
                            :app-context (:stillsuit/app-context decorated)}))
    (catch Exception e
      (log/error e))))

(defn- smap []
  (try
    (let [stillsuit-config (u/load-edn-file stillsuit-conf)
          custom-config (-> stillsuit-config
                          (update-in [:queries] #(merge % generate/query-maps))
                          (assoc-in [:input-objects] generate/inputs)
                          (assoc-in [:mutations] generate/mutation-maps))
          s-map (-> (service-map custom-config conn)
                  (assoc ::http/resource-path "/public"))]
      ;(log/infof "Connecting to datomic at %s..." db-link-peer)
      s-map)
    (catch Exception e
           (log/error e))))

(defn serve-gql
  []
  (log/info {:serve "Serving graphiql at: http://localhost:8888/graphiql/index.html"})
  (log/infof "GraphQL Voyager:     http://localhost:8888/voyager/index.html")
  (log/infof "Ready.")
  (load "lacinia/generate")
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
  :stopped)

(defn reset []
  (mount/stop)
  (refresh-all :after 'datomic-gql-starter.core/go))

(defn reset-config []
  (refresh-all)
  (config-files/update-config-files)
  (reset))

(comment
  (go)
  (reset)
  (stop))

(defn -main []
  (mount/start))




