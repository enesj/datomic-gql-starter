(ns db
  (:require [datomic-gql-starter.utils.fern :as f]
            [datomic.api :as d]))

(def profile :test)

(def db-uri (str f/db-link-peer f/db-name))

(def conn (d/connect db-uri))

(def db (d/as-of (d/db conn) #inst "2017-08-23T10:22:22.000-00:00"))

(def q d/q)

(def pull d/pull)

(def d-db d/db)


(def with-db nil)

(defn d-with [tx-data db]
  (d/with
    db
    (vec tx-data)))

(defn transact!
  [tx-data]
  @(d/transact
     conn
     (vec tx-data)))


(defn install-seattle []
  (let [seattle-uri (str f/db-link-peer "seattle")
        seattle-schema (read-string (slurp "resources/seattle/seattle-schema.edn"))
        seattle-data0 (read-string (slurp "resources/seattle/seattle-data0.edn"))
        seattle-data1 (read-string (slurp "resources/seattle/seattle-data1.edn"))]
    (d/create-database seattle-uri)
    (let [seattle-conn (d/connect seattle-uri)]
      @(d/transact seattle-conn seattle-schema)
      @(d/transact seattle-conn seattle-data0)
      @(d/transact seattle-conn seattle-data1))))

(def db-list (d/get-database-names (str f/db-link-peer "*")))

(defn seattle-test []
  (when-not ((set db-list) (f/fern-e 'db-name))
    (if (and (= (f/fern-e 'db-name) "seattle"))
      (do
        (println "Installing seattle database.")
        (install-seattle))
      (do
        (println "Database " (f/fern-e 'db-name) "doesn't exist!")
        (System/exit 0)))))

(seattle-test)







