(ns mbrainz.core-test
  (:require [clojure.test :refer :all]
            [venia.core :as v]
            [test-utils :refer [send-request]]))

(defn get-gql-query [venia-query]
  (v/graphql-query {:venia/queries venia-query}))

(defn get-data [gql-query]
  (:data (:body (send-request (get-gql-query gql-query)))))



(deftest Queries
  (let [artists1 (:Artists (:data (:body (send-request "{ Artists(_limit: \"20\", name: \"Bill\", country: [US, CA, GB], type: PERSON, sortName: \"Sage, Bill Le\", endMonth: [null, null], startYear: [\"1900\", null], endYear: null, _composition: \"[name country type startYear ]\") { end_month  start_year sort_name  sort_name name type country }}"))))
        artist2 (:Artists (:data (:body (send-request "{ Artists(_limit: \"20\", name: \"Bill\", country: [US, CA, GB], type: PERSON, sortName: \"Sage, Bill Le\", endMonth: [null, null], startYear: [\"1900\", null], endYear: null, _composition: \"[name country type endMonth startYear ]\") { end_month start_year sort_name  sort_name name type country }}"))))
        artist3 (:Artists (:data (:body (send-request "{ Artists(_limit: \"20\", name: \"Bill\", country: [US, CA, GB], type: PERSON, sortName: \"Sage, Bill Le\", endMonth: [null, null], startYear: [\"1900\", null], endYear: null, _composition: \"[name country type sortName endMonth startYear endYear]\") { end_month  start_year sort_name  sort_name name type country }}"))))
        artist4 (:Artists (:data (:body (send-request "{ Artists(_limit: \"20\", name: \"Bill\", country: [US, CA, GB], type: PERSON, sortName: \"Sage, Bill Le\", endMonth: [null, null], startYear: [\"1900\", null], endYear: null, _composition: \"[name startYear {or [sortName type country endMonth startYear]}]\") { end_month  start_year sort_name  sort_name name type country }}"))))
        artist5 (:Artists (:data (:body (send-request "{ Artists(_limit: \"20\", name: \"Bill\", country: [US, CA, GB], type: PERSON, sortName: \"Sage, Bill Le\", endMonth: [null, null], startYear: [\"1900\", null], endYear: null, _composition: \"[name {not [sortName country endMonth startYear {or [type endMonth endYear]}]} ]\") { end_month  start_year sort_name  sort_name name type country }}"))))
        artist6 (:Artists (:data (:body (send-request "{ Artists(_limit: \"20\", name: \"Bill\", country: [US, CA, GB], type: PERSON, sortName: \"Sage, Bill Le\", endMonth: [null, null], startYear: [\"1950\", null], endYear: null, _composition: \"[name {or [sortName endMonth startYear {not [type endYear]}]}]\") { end_month  start_year sort_name  sort_name name type country }}"))))
        releases1 (:Releases (:data (:body (send-request "{ Releases(language: ENG, artists: {type: GROUP, endYear: \"2010\"}, _composition: \"[language artists]\") { name artists { name type end_year } }}"))))]

    (is (= (first artists1)
          {:name       "Bill Dana",
           :start_year "1924",
           :type       "PERSON",
           :end_month  nil,
           :sort_name  "Dana, Bill",
           :country    "US",}))
    (is (= (last artists1)
          {:name       "Bill Le Sage",
           :start_year "1927",
           :type       "PERSON",
           :end_month  "10",
           :sort_name  "Sage, Bill Le",
           :country    "GB",}))
    (is (= (count artists1) 10))
    (is (= (first artist2)
          {:name       "Bill Monroe",
           :start_year "1911",
           :type       "PERSON",
           :end_month  "9",
           :sort_name  "Monroe, Bill",
           :country    "US",}))
    (is (= (last artist2)
          {:name       "Bill Le Sage",
           :start_year "1927",
           :type       "PERSON",
           :end_month  "10",
           :sort_name  "Sage, Bill Le",
           :country    "GB",}))
    (is (= (count artist2) 4))
    (is (= (count artist3) 1))
    (is (= (count artist4) 12))
    (is (= (first artist4)
          {:name       "Bill Dana",
           :start_year "1924",
           :type       "PERSON",
           :end_month  nil,
           :sort_name  "Dana, Bill",
           :country    "US",}))
    (is (= (last artist4)
          {:name       "Bill Cosby",
           :start_year "1937",
           :type       "PERSON",
           :end_month  nil,
           :sort_name  "Cosby, Bill",
           :country    "US",}))
    (is (= (first artist5)
          {:name       "Bill Black's Combo",
           :start_year "1959",
           :type       "GROUP",
           :end_month  nil,
           :sort_name  "Black, Bill, Combo",
           :country    "US",}))
    (is (= (last artist5)
          {:name       "Bill Fay",
           :start_year nil,
           :type       "PERSON",
           :end_month  nil,
           :sort_name  "Fay, Bill",
           :country    "GB",}))
    (is (= (count artist5) 20))
    (is (= (first artist6)
          {:name       "Bill Monroe",
           :start_year "1911",
           :type       "PERSON",
           :end_month  "9",
           :sort_name  "Monroe, Bill",
           :country    "US",}))
    (is (= (last artist6)
          {:name       "Bill Le Sage",
           :start_year "1927",
           :type       "PERSON",
           :end_month  "10",
           :sort_name  "Sage, Bill Le",
           :country    "GB",}))

    (is (= (count artist6) 12))
    (is (= releases1
          [{:name "Smothers Brothers Comedy Hour", :artists [{:name "The Smothers Brothers", :type "GROUP", :end_year "2010"}]}
           {:name "Cluster 71", :artists [{:name "Cluster", :type "GROUP", :end_year "2010"}]}]))
    (is (= (count releases1) 2))))










