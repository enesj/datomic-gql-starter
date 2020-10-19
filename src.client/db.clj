(ns db
  (:require [datomic-gql-starter.utils.fern :as f]
            [datomic.client.api :as d]))

(def profile :client)

(def cfg {:server-type :peer-server
          :access-key (f/fern-e 'access-key)
          :secret (f/fern-e 'secret)
          :endpoint "localhost:8998"
          :validate-hostnames false})

(def conn (d/connect (d/client cfg) {:db-name (f/fern-e 'db-name)}))


(def db (d/db conn))

(def q d/q)

(def pull d/pull)

(def d-db d/db)

(def with-db d/with-db)

(defn d-with [tx-data db]
  (d/with
    (d/with-db conn)
    {:tx-data (vec tx-data)}))

(defn transact!
  [tx-data]
  (d/transact
    conn
    {:tx-data (vec tx-data)}))

(def db-list (d/list-databases (d/client cfg) {}))

(defn seattle-test []
  (when-not ((set db-list) (f/fern-e 'db-name))
      (do
        (println "Database " (f/fern-e 'db-name) "doesn't exist!")
        (System/exit 0))))

(seattle-test)







