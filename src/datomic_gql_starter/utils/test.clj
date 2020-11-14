(ns datomic-gql-starter.utils.test
  (:require
    [clojure.test :refer [is]]
    [clj-http.client :as client]
    [venia.core :as v]
    [io.pedestal.http :as http]
    [com.walmartlabs.lacinia.pedestal :as lp]
    [clojure.core.async :refer [timeout alt!! chan put!]]
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [com.walmartlabs.lacinia.util :as util]
    [com.walmartlabs.lacinia.schema :as schema]
    [com.walmartlabs.lacinia.resolve :refer [resolve-as]]
    [gniazdo.core :as g]
    [io.pedestal.log :as log]
    [cheshire.core :as cheshire]))



(defn get-url
  [path]
  (client/get (str "http://localhost:8888" path) {:throw-exceptions false}))

(defn send-request
  "Sends a GraphQL request to the server and returns the response."
  ([query]
   (send-request :get query))
  ([method query]
   (send-request method query nil))
  ([method query vars]
   (-> {:method method
        :url "http://localhost:8888/graphql"
        :throw-exceptions false}
     (cond->
       (= method :get)
       (assoc-in [:query-params :query] query)

       (= method :post)
       (assoc-in [:headers "Content-Type"] "application/graphql")

       (= method :post-json)
       (->
         (assoc-in [:headers "Content-Type"] "application/json")
         (assoc :method :post
                :body query))

       ;; :post-bad is like :post, but without setting the content type
       (#{:post :post-bad} method)
       (assoc :body query
              :method :post)

       vars
       (assoc-in [:query-params :variables] (cheshire/generate-string vars)))
     client/request
     (update :body #(try
                      (cheshire/parse-string % true)
                      (catch Exception t
                        %))))))


(defn send-json-request
  ([method json]
   (send-json-request method json "application/json; charset=utf-8"))
  ([method json content-type]
   (send-json-request method "/graphql" json content-type))
  ([method path json content-type]
   (-> {:method method
        :url (str "http://localhost:8888" path)
        :throw-exceptions false
        :headers {"Content-Type" content-type}}
     (cond->
       json (assoc :body (cheshire/generate-string json)))
     client/request
     (update :body
       #(try
          (cheshire/parse-string % true)
          (catch Exception t %))))))

(defn send-json-string-request
  ([method json]
   (send-json-string-request method json "application/json; charset=utf-8"))
  ([method json content-type]
   (-> {:method method
        :url "http://localhost:8888/graphql"
        :throw-exceptions false
        :headers {"Content-Type" content-type}
        :body json}
     client/request
     (update :body
       #(try
          (cheshire/parse-string % true)
          (catch Exception t %))))))

(defn composition-to-string [query]
  "Converts the value of '_composition' argument to string "
  (let [composition? (get-in query [0 1 :_composition])]
    (if composition?
      (update-in query [0 1 :_composition]
        #(when % (str %)))
      query)))

(defn make-gql-query [clojure-data-query]
  "Generates graphql query string from Clojure data"
  (v/graphql-query {:venia/queries clojure-data-query}))

(defn run-query [gql-query]
  "Sends a GraphQL request to the server and returns data from the response."
  (-> gql-query
    composition-to-string
    make-gql-query
    send-request
    :body
    :data
    first
    val))

(defn make-gql-mutation [clojure-data-query]
  "Generates graphql query string from Clojure data"
  (v/graphql-query {:venia/operation {:operation/type :mutation
                                      :operation/name ""}
                    :venia/queries   clojure-data-query}))

(defn run-mutation [query-data]
  "Sends a GraphQL request to the server and returns data from the response."
  (-> query-data
    composition-to-string
    make-gql-mutation
    send-request
    :body
    :data
    first
    val))

(comment
  (require '[vlaaad.reveal :as reveal])
  (add-tap (reveal/ui)))
