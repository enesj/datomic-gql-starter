(ns datomic-gql-starter.utils.config
  (:require [catchpocket.generate.core :as cg]
            [datomic-gql-starter.utils.db :as db-utils :refer [db conn]]
            [cuerdas.core :as str]
            [catchpocket.generate.core :as g]
            [catchpocket.lib.config :as cf]
            [datomic-gql-starter.utils.refs-enums :as refs-enums]
            [datomic-gql-starter.utils.fern :as f :refer [refs-conf catchpocket-conf stillsuit-conf
                                                          db-link root-dir api-conf]]
            [clojure.java.io :as io]))

(defn query-ellipsis [x]
  (if (vector? (first x))
    (mapv first x)
    x))

(if (= (System/getenv "DATOMIC_API") "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

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
  (-> (d/q '[:find ?ident
             :in $ %
             :where
             [?e :db/ident ?ident]
             [?e :db/valueType :db.type/ref]
             [(namespace ?ident) ?ns]
             (remove-ns ?ns)]
        db rules)
    query-ellipsis))

(defn find-enums []
  (let [refs (find-all-refs)]
    (into (sorted-map)
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
    (remove #(str/includes? % "."))
    vec))


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

(defn make-catchpocket-config [refs-file-cfg catchpocket-conf stillsuit-conf entities db]
  "Generates 'catchpocket-config.edn' file"
    (with-open
      [w (clojure.java.io/writer catchpocket-conf)]
      (clojure.pprint/pprint
        (merge
          {:catchpocket/datomic-uri  db
           :catchpocket/schema-file stillsuit-conf}
          (refs-enums/get-refs-enums refs-file-cfg entities)
          {:stillsuit/compile? true})
        w)))


(defn make-apis [] (make-api-config (find-all-entities)))
(defn make-catchpocket [] (make-catchpocket-config (find-enums) catchpocket-conf stillsuit-conf (find-all-entities) db-link))
(defn make-stillsuit [] (g/generate-and-write! (cf/construct-config catchpocket-conf) conn db))

(defn update-config-files []
  (let [root (io/as-file root-dir)]
    (when-not (.exists root)
      (.mkdirs root))
    (make-apis)
    (make-catchpocket)
    (make-stillsuit)))

(defn repair-config-files
  ([]
   (let [root (io/as-file root-dir)]
     (when-not (.exists root)
       (.mkdirs root))
     (when-not (.exists (io/as-file api-conf))
       (make-apis))
     (when-not (.exists (io/as-file catchpocket-conf))
       (make-catchpocket)
       (make-stillsuit))
     (when-not (.exists (io/as-file stillsuit-conf))
       (make-stillsuit)))))

(repair-config-files)

