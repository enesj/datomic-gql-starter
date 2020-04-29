  (ns datomic-gql-starter.core
    (:require [clojure.tools.logging :as log]
              [clojure.tools.namespace.repl :refer [refresh refresh-all]]
              [com.walmartlabs.lacinia.pedestal :as lacinia-pedestal]
              [mount.core :refer [defstate] :as mount]
              [io.pedestal.http :as http]
              [stillsuit.core :as stillsuit]
              [stillsuit.lib.util :as u]
              [datomic-gql-starter.lacinia.make-config-files :as refs-enums]
              [datomic-gql-starter.lacinia.generate :as generate]
              [datomic-gql-starter.utils.fern :refer [refs-conf catchpocket-conf stillsuit-conf
                                                      db-link root-dir api-conf]]
              [datomic-gql-starter.utils.db :as db-utils]))

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
    (let [test-schema (u/load-edn-file "resources/dev/test-schema.edn")
          stillsuit-config (u/load-edn-file stillsuit-conf)
          custom-config (-> stillsuit-config
                          (update-in [:queries] #(merge % generate/query-maps))
                          (assoc-in [:input-objects] generate/inputs)
                          (assoc-in [:mutations] generate/mutation-maps))
          config-wth-test-queries (merge-with (comp #(into {} %) concat)
                                    custom-config test-schema)
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
  :stoped)

(defn reset []
  (mount/stop)
  (refresh :after 'datomic-gql-starter.core/go))

(defn reset-config []
  (refs-enums/update-config-files)
  (reset))

(comment
  (go)
  (reset)
  (stop))


(defn -main []
  (mount/start))




