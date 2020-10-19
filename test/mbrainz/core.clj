(ns mbrainz.core
  (:require [clojure.test :refer :all]
            [venia.core :as v]
            [test-utils :refer [send-request]]))

(defn get-gql-query [venia-query]
  (v/graphql-query {:venia/queries venia-query}))

(defn get-data [gql-query]
  (val (first (:data (:body (send-request (get-gql-query gql-query)))))))

(def artist-by-start-year
  ""
  [[:Artists {:startYear "1959"}
    [:start_year
     :name
     :country]]])

(def artist-by-start-year-range
  "Returns artists that has start year between 1959 and 1962.
  Every search with tuple of two elements is treated as search for values in an open interval like this: [lower-bound upper-bound]"
  [[:Artists {:startYear ["1959" "1962"]}
    [:start_year
     :name
     :country]]])

(def artist-by-start-year-limited
  "Every generated query has special parameter '_limit' that limits number of returned results"
  [[:Artists {:_limit "10" :startYear ["1959" "1962"]}
    [:start_year
     :name
     :country]]])

(def artist-by-name
  "Returns multiple results when is ran on 'datomic-on-perm' because 'name' field has db/fulltext attribute set to 'true'.
   Returns single value on 'datomic-client' and 'dev-local'"
  [[:Artists {:name  ["Bill Evans" "Bill Evans"]}
    [:start_year
     :name
     :country]]])

(def artist-by-name-exact
  "If you want to do exact search on fulltext field you can do it by defining range with same lower and upper bound"
  [[:Artists {:name  ["Bill Evans" "Bill Evans"]}
    [:start_year
     :name
     :country]]])





