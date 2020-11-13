(ns datomic-gql-starter.examples.queries
  "Examples of queries for 'mbrainz' database "
  (:require [clojure.test :refer :all]
            [datomic-gql-starter.lacinia.utils :as utils]
            [venia.core :as v]))


(comment
  (require '[vlaaad.reveal :as reveal])
  (add-tap (reveal/ui)))

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
    utils/send-request
    :body
    :data
    first
    val))


(def artist-by-start-year
  "Find artists that has startYear 1959"
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

(def artist-by-start-year-open-range
  "Returns artists that has startYear greater then 1959.
	We can replace upper or lower bound with 'nil' if we want to search for 'greater than' or 'lesser than' values . "
  [[:Artists {:startYear ["1959" nil]}
    [:start_year
     :name
     :country]]])

(def artist-by-start-year-limited
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

(def artist-by-start-years-two
  "Returns artists that has startYear 1959 or 1962.
	 if you need to search any of two possible values, to avoid range search you have to add third parameter that is equal to one of first two or is 'nil'"
  [[:Artists {:startYear ["1959" "1962" nil]}
    [:start_year
     :name
     :country]]])

(def artist-any-start-year
  "Returns artists that has country US and any non nil value for startYear.
	Tuple [nil nil] is used for searching all non-nil values.
	US in this example is 'enum' and should have single quote: ' prepended to it in order to prevent evaluation"
  [[:Artists {:_limit    "20"
              :country   'US
              :startYear [nil nil]}
    [:start_year
     :name
     :country
     :country]]])

(def artist-by-name
  "Returns multiple results when is ran on Datomic-on-perm because 'name' field has db/fulltext attribute set to 'true'.
	 Returns single value on Datomic-client and Datomic-dev-local"
  [[:Artists {:name "Bill Evans"}
    [:start_year
     :name
     :country]]])

(def artist-by-name-exact
  "If you want to do exact search on fulltext field you can do it by defining range with same lower and upper bound"
  [[:Artists {:name ["Bill Evans" "Bill Evans"]}
    [:start_year
     :name
     :country]]])


(def artist-start-year-country-composition
  "According to the Datomic documentation order of :where clauses affects query performances.
	 This means that you should put most restrictive clauses first to improve performances of query.

	 Every generated query has an special parameter '_composition' as vector
	 that should be converted to string before sending gql request: ['startYear 'country] -> (str ['startYear 'country]).
	 Function 'composition-to-string [query]' from this namespace is doing this conversion.

	 When  this parameter is used order of :where clauses in Datomic query will be the same as order of parameters in this vector.
	 One of use of this special parameter is to define execution order of clause
	 All clauses entered in '_composition' parameter are combined with 'and'.

	 Because we consider startYear = 1972 more restrictive clause than country =  US in this query we will find first
	 all Artists that has startYear 1972 and in returned results all Artists that has country US."

  [[:Artists {:country      'US
              :startYear    "1972"
              :_composition '[startYear country]}
    [:start_year
     :name
     :country]]])


(def artist-select-parameters-with-composition
  "You can use '_composition' parameter to select wich of previously defined parameters will be actualy executed.
	 This query will use only 'startYear '1972'' as search criteria because 'country 'US'' is not included in  '_composition' parameter."

  [[:Artists {:country      'US
              :startYear    "1972"
              :_composition '[startYear]}
    [:start_year
     :name
     :country]]])

(def artist-composition-with-or
  "Parameters declared in '_composition' parameter are combined with 'AND' by default,
	but you can combine them with 'OR' by putting map with key 'or' and value [parameter1, parameter2 ...] in '_composition' vector"

  [[:Artists {:country      'US
              :startYear    "1972"
              :_composition '[{or [country startYear]}]}
    [:start_year
     :name
     :country]]])

(def artist-composition-with-or-and
  "You can also use combination of 'AND' and 'OR'. This query will return all 'artists' that have 'startYear = 1972' or 'country US'
	but only if they have 'type PERSON'"

  [[:Artists {:country      'US
              :startYear    "1972"
              :type         'PERSON
              :_composition '[type {or [country startYear]}]}
    [:start_year
     :name
     :type
     :country]]])


(def artist-composition-with-or-and-not
  "Onother operator that you can use in '_composition' parameter is 'OR'.
	This query will return all 'artists' that have 'startYear = 1972' or 'country US'
	but only if they have 'type PERSON' and 'gender not equal 'MALE'"

  [[:Artists {:country      'US
              :startYear    "1972"
              :type         'PERSON
              :gender       'MALE
              :_composition '[type {not [gender]} {or [country startYear]}]}
    [:start_year
     :name
     :type
     :gender
     :country]]])

(def artist-composition-deep
  "You can nest 'OR' and 'NOT' operators arbitrarily like this.
	 This query will find all artists that has 'type MALE' or doesn't have 'country US' but only if they have 'startYear between 1959 1965'
	 Without 'startYear' clause in front of this '_composition' this query will return error 'Insufficient Binding',
	 see this link https://docs.datomic.com/cloud/query/query-data-reference.html#insufficient-binding-for-a-not-clause.
	 When you want to search all artists for the same criteria it would be best to put 'name [null null]' (see 'artist-any-start-year' query) at beginning of '_composition',
	 because we can make an assumption that all artists have 'name' field."

  [[:Artists {:country      'US
              :startYear    ["1959" "1965"]
              :type         'PERSON
              :gender       'MALE
              :_composition '[startYear {or [type {not [country]}]}]}
    [:start_year
     :name
     :type
     :gender
     :country]]])

(def artist-composition-complex
  "This is example of more complex '_composition' clauses.
	This query will return  all artists that have:
	'startYear 1959' or 'endYear 2011'
	and
	'type 'PERSON or 'country different from US' "

  [[:Artists {:country      'US
              :startYear    "1959"
              :endYear      "2011"
              :type         'PERSON
              :gender       'MALE
              :_composition '[{or [endYear startYear]} {or [type {not [country]}]}]}
    [:start_year
     :end_year
     :name
     :type
     :gender
     :country]]])

(def artist-no-start-year
  "In query 'artist-any-start-year' we have seen that we can use 'range' clause [nii nil] to get all artists that have any value for startYear.
	If we want to do the opposite - find all artists that haven't startYear we will use '[nil]' value for  startYear.
	Here again we have to start the '_composition' with some other clause, because it is internally implemented with 'datomic not operator'.
	In this case we start '_composition' with 'name [nil nil]' or  'find all artists with name defined'"

  [[:Artists {:_limit       "20"
              :name         [nil nil]
              :startYear    [nil]
              :_composition '[name startYear]}
    [:start_year
     :name
     :type
     :gender
     :country]]])


(def artist-releases
  "You can list releases for an artist or artists"
  [[:Artists {:name "Bill Cosby"}
    [:start_year
     :name
     [:_releases
      [:name :year]]
     :type
     :gender
     :country]]])

(def artist-releases-tracks-duration
  "Or tracks duration of every release per artist"
  [[:Artists {:name "Bill Cosby"}
    [:name
     [:_releases
      [:name :year
       [:media
        [:track_count
         [:tracks
          [:duration]]]]]]
     :type
     :gender
     :country]]])

(def artist-releases-tracks-duration-filter
  "Or tracks duration that are bigger then 500000, of every release per artist"
  [[:Artists {:name "Bill Cosby"}
    [:name
     [:_releases
      [:name :year
       [:media
        [:track_count
         [:tracks {:duration ["500000" nil]}
          [:duration]]]]]]
     :type
     :gender
     :country]]])

(def artist-releases-tracks-media-duration-filter
  "Or tracks duration that are bigger then 10000 and media has 7,8 or 9 tracks, of every artist's release "
  [[:Artists {:name "Bill Cosby"}
    [:name
     [:_releases
      [:name :year
       [:media {:trackCount ["7" "8" "9"]}
        [:track_count
         [:tracks {:duration ["10000" nil]}
          [:artist_credit :duration :name]]]]]]
     :type
     :gender
     :country]]])

(def releases-by-artist
  "You can list releases by filtering artists parameter"
  [[:Releases {:language 'ENG,
               :artists  {:type 'PERSON, :endYear "2010"},}
    [:name
     [:artists
      [:name :type :end_year]]]]])


(def labels
  [[:Labels {:startYear "1920"}
    [:name :start_year]]])








