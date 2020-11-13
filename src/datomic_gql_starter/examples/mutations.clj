(ns datomic-gql-starter.examples.mutations
  "Examples of mutations for 'mbrainz' database "
  (:require [clojure.test :refer :all]
            [datomic-gql-starter.lacinia.utils :as utils]
            [venia.core :as v]))


(defn composition-to-string [query]
  "Converts the value of '_composition' argument to string "
  (let [composition? (get-in query [0 1 :_composition])]
    (if composition?
      (update-in query [0 1 :_composition]
        #(when % (str %)))
      query)))

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
    utils/send-request
    :body
    :data
    first
    val))

(def update-artist-start-day
  "All mutations have one special parameter ':_preview'. When this parameter is set to 'true' mutation will
	return updated data without actually modifying db, so you can preview result of mutation before updating database.

	Parameter ':_update' defines which fields should be updated to which values. When it is omitted mutation returns just result of
	query, so you can see what will be updated width this mutation before running it.

	This mutation updates startDay to 5 for artists that have startYear 1959"
  [[:updateArtists {:startYear    "1959"
                    :endDay       [nil]
                    :_composition '[startYear endDay]
                    :_update      {:startDay "5"}
                    :_preview     true}
    [:start_year
     :start_day
     :end_day
     :name
     :country]]])

(def update-artist-remove-start-day
  "You can also delete an attribute from an entity, by updating it to 'nil' value."
  [[:updateArtists {:startYear "1959"
                    :_update   {:startDay nil}
                    :_preview  true}
    [:start_year
     :start_day
     :end_day
     :name
     :country]]])

(def update-artist-add-start-day
  "Every update returns the result of the same query that selected entities for update.
	If you were updating a field that was used as query parameter, returned result will have a changed set of entities.

	In this case it will return an empty vector, because after update there is no more Artists without startDay for startYear = 1959"

  [[:updateArtists {:startYear "1959"
                    :startDay  [nil]
                    :_update   {:startDay "8"}
                    :_preview  true}
    [:start_year
     :start_day
     :end_day
     :name
     :country]]])

(def insert-artists
  "Insert artists"
  [[:insertArtists {:Artists  [{:name      "Enes Jakic"
                                :country   'US
                                :gender    'MALE
                                :type      'GROUP
                                :startYear "1959"
                                :startDay  "14"}
                               {:name      "Irma Jakic"
                                :country   'US
                                :gender    'FEMALE
                                :type      'GROUP
                                :startYear "1993"
                                :startDay  "15"}
                               {:name      "Zlatan Jakic"
                                :country   'US
                                :gender    'MALE
                                :type      'GROUP
                                :startYear "2001"
                                :startDay  "27"}]
                    :_preview true}
    [:db_id
     :start_year
     :start_day
     :end_day
     :name
     :country]]])

(def insert-releases-artists
  "Insert releases with artists"
  [[:insertReleases {:Releases  [{:name "LA Familia"
                                  :language  'BOS
                                  :artists [{:name      "Enes Jakic"
                                             :country   'US
                                             :gender    'MALE
                                             :type      'GROUP
                                             :startYear "1959"
                                             :startDay  "14"}
                                            {:name      "Irma Jakic"
                                             :country   'US
                                             :gender    'FEMALE
                                             :type      'GROUP
                                             :startYear "1993"
                                             :startDay  "15"}
                                            {:name      "Zlatan Jakic"
                                             :country   'US
                                             :gender    'MALE
                                             :type      'GROUP
                                             :startYear "2001"
                                             :startDay  "27"}]}]
                     :_preview true}
    [:name
     :language
     [:artists [:name :gender]]]]])

(def delete-labels
  "Deletion queries always returns an empty vector. If 'preview' is true they don't update database. Can be used to report an execution error. "
  [[:deleteLabels {:startYear "1920"
                   :_preview  true}
    [:start_year
     :start_day
     :end_day
     :name
     :country]]])
