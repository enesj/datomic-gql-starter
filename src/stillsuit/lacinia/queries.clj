(ns stillsuit.lacinia.queries
  "Implementation functions for creating top-level stillsuit queries."
  (:require [com.walmartlabs.lacinia.resolve :as resolve]
            [stillsuit.datomic.core :as datomic]
            [clojure.tools.logging :as log]
            ;[datomic.api :as d] ;datomic.api
            ;[datomic.client.api :as d] ;datomic.client.api
            [stillsuit.lacinia.types :as types]
            [com.walmartlabs.lacinia.schema :as schema])
  (:import (com.walmartlabs.lacinia.resolve ResolverResult)))


(if (= (System/getenv "DATOMIC_API") "client")
  (require '[datomic.client.api :as d])
  (require '[datomic.api :as d]))

(defn stillsuit-entity-id-query
  [{:stillsuit/keys [entity-id-query-name datomic-entity-type]}]
  {:type        datomic-entity-type
   :args        {:eid {:type        '(non-null ID)
                       :description "The `:db/id` of the entity"}}
   :resolve     :stillsuit/resolve-by-entity-id
   :description "Return the current time."})

(def entity-id-query-resolver
  ^ResolverResult
  (fn entity-id-query-resolver-fn
    [{:stillsuit/keys [config connection] :as context} {:keys [eid] :as args} value]
    (if-let [db (some-> connection d/db)]
      (let [ent      (datomic/get-entity-by-eid db eid)
            ent-type (types/lacinia-type ent config connection)]
        (log/infof "Entity" ent)
        (when (some? ent)
          (resolve/resolve-as
           (schema/tag-with-type ent ent-type))))
      ;; Else no db
      (resolve/resolve-as nil {:message (format "Can't get db value from connection %s!" (str connection))}))))

(defn stillsuit-unique-attribute-query
  [{:stillsuit/keys [entity-id-query-name datomic-entity-type]}]
  {:type        datomic-entity-type
   :args        {:id {:type        '(non-null ID)
                      :description "The `:db/id` of the entity"}}
   :resolve     [:stillsuit/query-by-unique-id datomic-entity-type]
   :description "Get a `%s` entity by specifying its `%` attribute."})

;; TODO: specs - ensure only one arg
(defn unique-attribute-query-resolver
  "Catchpocket interface to a generic query, expected to be referenced as a resolver:
  :resolve [:stillsuit/resolve-by-unique-id {:stillsuit/attribute :example/attribute
                                             :stillsuit/type      :LaciniaTypeName}]"
  [{:stillsuit/keys [attribute lacinia-type]}]
  ^resolve/ResolverResult
  (fn unique-attribute-query-resolver-fn
    [{:stillsuit/keys [connection] :as ctx} args value]
    (if-let [db (some-> connection d/db)]
      (let [arg (some-> args vals first)
            result-raw (datomic/get-entity-by-unique-attribute db attribute arg)
            result (if (:db/id result-raw) result-raw nil)]
        (resolve/resolve-as
         (schema/tag-with-type result lacinia-type)))
      ;; Else no db
      (resolve/resolve-as nil {:message (format "Can't get db value from connection %s!" (str connection))}))))

(defn resolver-map
  [{:stillsuit/keys [entity-id-query-name query-by-unique-id-name]}]
  {:stillsuit/resolve-by-entity-id entity-id-query-resolver
   :stillsuit/query-by-unique-id   unique-attribute-query-resolver})

(defn attach-queries [schema config]
  (let [{:stillsuit/keys [entity-id-query-name query-by-unique-id-name]} config]
    (-> schema
        (assoc-in [:queries entity-id-query-name]
                  (stillsuit-entity-id-query config))
        (assoc-in [:queries query-by-unique-id-name]
                  (stillsuit-unique-attribute-query config)))))
