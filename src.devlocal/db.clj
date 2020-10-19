(ns db
  (:require [datomic-gql-starter.utils.fern :as f]
            [datomic.client.api :as d]))
(def profile :devlocal)

(def cfg {:server-type :dev-local
          :system      "samples"
          :storage-dir "C:\\projects\\backend\\datomic-gql-starter\\data"})

(def conn (d/connect (d/client cfg) {:db-name (f/fern-e 'db-name)}))

(def db (d/db conn))

(def q d/q)

(def pull d/pull)

(def d-db d/db)

(def db-uri (str "local-dev:" (f/fern-e 'db-name)))

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

;(defn install-seattle []
;  (let [seattle-uri (str f/db-link-free "seattle")
;        seattle-schema (read-string (slurp "resources/seattle/seattle-schema.edn"))
;        seattle-data0 (read-string (slurp "resources/seattle/seattle-data0.edn"))
;        seattle-data1 (read-string (slurp "resources/seattle/seattle-data1.edn"))]
;    (d/create-database seattle-uri)
;    (let [seattle-conn (d/connect seattle-uri)]
;      @(d/transact seattle-conn seattle-schema)
;      @(d/transact seattle-conn seattle-data0)
;      @(d/transact seattle-conn seattle-data1))))
;
;
;(defn seattle-test []
;  (when-not ((set db-list) (f/fern-e 'db-name))
;    (if (and (= (f/fern-e 'db-name) "seattle"))
;      (do
;        (println "Installing seattle database.")
;        (install-seattle))
;      (do
;        (println "Database " (f/fern-e 'db-name) "doesn't exist!")
;        (System/exit 0)))))
;
;(seattle-test)







