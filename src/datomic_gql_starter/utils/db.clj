(ns datomic-gql-starter.utils.db
  (:require [catchpocket.generate.core :as cg]
            [stillsuit.lib.util :as u]
            ;[datomic.api :as d] ;datomic.api
            [datomic.client.api :as d]
            [cuerdas.core :as str]
            [hawk.core :as hawk]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf]]
            [clojure.java.io :as io]))


(defn query-ellipsis [x]
  (if (vector? (first x))
    (mapv first x)
    x))

(def cfg {:server-type :peer-server
          :access-key "enes"
          :secret "enes"
          :endpoint "localhost:8998"
          :validate-hostnames false})

(def client (d/client cfg))

(def conn (d/connect client {:db-name (f/fern-e 'db-name)})) ; datomic.client/api

(def db (d/db conn))

(def rules '[[(remove-ns ?ns)
              (not [(clojure.string/starts-with? ?ns "db")])
              (not [(clojure.string/starts-with? ?ns "sys")])
              (not [(clojure.string/starts-with? ?ns "fressian")])
              (not [(clojure.string/starts-with? ?ns "deprecated")])]])

(defn get-attr-type [attr]
  (let [type (-> (d/q '[:find ?t
                        :in $ ?v
                        :where
                        [?e :db/ident ?v]
                        [?e :db/valueType ?type-id]
                        [?type-id :db/ident ?t]]
                   db attr)
               ffirst
               cg/datomic-to-lacinia)]
    [type (= type :catchpocket.generate.core/ref)]))

(defn find-all-refs []
  "gets all entities from DB of type ':dbex.type/ref' "
  (-> (d/q {:query '[:find ?ident
                     :in $ %
                     :where
                     [?e :db/ident ?ident]
                     [?e :db/valueType :db.type/ref]
                     [(namespace ?ident) ?ns]
                     (remove-ns ?ns)]
            :args  [db rules]})
      query-ellipsis))

(defn find-enums []
  (let [refs (find-all-refs)]
    (into {}
      (for [ref refs]
        (vector ref
          (->>
            (d/q '[:find  (first ?a)
                   :in $ ?ref
                   :where
                   [_ ?ref ?w]
                   [?w :db/ident ?a]]
              db ref)
            (#(if (not-empty %) :enum :ref))))))))

(defn find-all-entities []
  (->>
    (d/q '[:find ?ns
           :in $ %
           :where
           [?e :db/ident ?ident]
           [?e :db/valueType]
           [(namespace ?ident) ?ns]
           (remove-ns ?ns)]
      db rules)
    query-ellipsis
    (remove #(str/includes? % "."))))


(defn find-all-fields [entity]
  (->>
    (d/q '[:find ?ident
           :in $ ?entity
           :where
           [?e :db/ident ?ident]
           [?e :db/valueType]
           [(namespace ?ident) ?ns]
           [(= ?ns ?entity)]]
      db entity)
    query-ellipsis))

(hawk/watch! [{:paths   [refs-conf]
               :handler (fn [ctx e]
                          (let [catchpocket (io/as-file catchpocket-conf)
                                stillsuit (io/as-file stillsuit-conf)]
                            (println "deleting 'catchpocket-conf' and 'stillsuit-conf'"
                              (when (.exists catchpocket)
                                (io/delete-file catchpocket))
                              (when (.exists stillsuit)
                                (io/delete-file stillsuit))
                              ctx)))}])


