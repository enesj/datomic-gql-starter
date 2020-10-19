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
  "Returns artists that has startYear between 1959 and 1962.
  Every search with tuple of two elements is treated as search for values in an open interval [lower-bound upper-bound]"
  [[:Artists {:startYear ["1959" "1962"]}
    [:start_year
     :name
     :country]]])

(def artist-by-start-year-range-limited
  "Every generated query has special parameter '_limit' that limits number of returned results.
   If limit is omitted, maximal number of result is 1000 which is Datomic limit"
  [[:Artists {:_limit "10" :startYear ["1959" "1962"]}
    [:start_year
     :name
     :country]]])

(def artist-by-start-years
  "Returns artists that has startYear 1959 or 1962 or 1972.
   Every search with tuple of more then two elements is treated as search for values equal to any element of tuple"
  [[:Artists {:startYear ["1959" "1962" "1972"]}
    [:start_year
     :name
     :country]]])

(def artist-by-two-start-years
  "Returns artists that has startYear 1959 or 1962.
   if you need to search any of two possible values, to avoid range search zou have to add third parameter that is equal to one of first two or is 'nil'"
  [[:Artists {:startYear ["1959" "1962" nil]}
    [:start_year
     :name
     :country]]])

(def artist-that-has-start-year
  "Returns artists that has country US and any non nil value for startYear.
  Tuple [nil nil] is used for searching all non-nil values"
  [[:Artists {:_limit    "20"
              :country   'US
              :startYear [nil nil]}
    [:start_year
     :name
     :country]]])

(def artist-by-name
  "Returns multiple results when is ran on Datomic-on-perm because 'name' field has db/fulltext attribute set to 'true'.
   Returns single value on Datomic-client and Datomic-dev-local"
  [[:Artists {:name  "Bill Evans"}
    [:start_year
     :name
     :country]]])

(def artist-by-name-exact
  "If you want to do exact search on fulltext field you can do it by defining range with same lower and upper bound"
  [[:Artists {:name  ["Bill Evans" "Bill Evans"]}
    [:start_year
     :name
     :country]]])


(def artist-start-year-country-composition
  "According to the Datomic documentation order of :where clauses affects query performances.
   This means that you should put most restrictive clauses first to improve performances of query.
   Every generated query has an special parameter '_composition' that is string representation of vector.
   When  this parameter is used order of :where clauses in Datomic query will be the same as order of parameters in this vector.
   One of use of this special parameter is to define execution order of clause
   All clauses entered in '_composition' parameter are combined with 'and'.

   Because we consider startYear = 1972 more restrictive clause than country =  US in this query we will find first
   all Artists that has startYear 1972 and in returned results all Artists that has country US."

  [[:Artists {:country   'US
              :startYear "1972"
              :_composition "[startYear country]"}
    [:start_year
     :name
     :country]]])








