(ns datomic-gql-starter.utils.db
  (:require [datomic-gql-starter.utils.fern :as f]))

(def datomic-api (System/getenv "DATOMIC_API"))

(if (= datomic-api "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

(def db-uri f/db-link)

(def cfg {:server-type :peer-server
          :access-key (f/fern-e 'access-key)
          :secret (f/fern-e 'secret)
          :endpoint "localhost:8998"
          :validate-hostnames false})

(defn install-seattle []
  (let [seattle-uri (str (subs db-uri 0 28) "/seattle")
        seattle-schema (read-string (slurp "resources/seattle/seattle-schema.edn"))
        seattle-data0 (read-string (slurp "resources/seattle/seattle-data0.edn"))
        seattle-data1 (read-string (slurp "resources/seattle/seattle-data1.edn"))]
    (d/create-database seattle-uri)
    (let [seattle-conn (d/connect seattle-uri)]
      @(d/transact seattle-conn seattle-schema)
      @(d/transact seattle-conn seattle-data0)
      @(d/transact seattle-conn seattle-data1))))

(def db-list (if (= datomic-api "client")
               ((resolve 'd/list-databases) ((resolve 'd/client) cfg) {})
               ((resolve 'd/get-database-names) (str (subs db-uri 0 28) "/*"))))

(defn seattle-test []
  (when-not ((set db-list) (f/fern-e 'db-name))
    (if (and (= (f/fern-e 'db-name) "seattle") (not= datomic-api "client"))
      (do
        (println "Installing seattle database.")
        (install-seattle))
      (do
        (println "Database " (f/fern-e 'db-name) "doesn't exist!")
        (System/exit 0)))))

(seattle-test)

(def conn (if (= datomic-api "client")
            (d/connect ((resolve 'd/client) cfg) {:db-name (f/fern-e 'db-name)})
            (d/connect db-uri)))


(def db (d/db conn))





