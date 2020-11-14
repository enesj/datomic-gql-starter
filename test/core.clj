(ns core
  (:require [clojure.test :refer :all]
            [datomic-gql-starter.examples.mutations :as mutations]
            [datomic-gql-starter.examples.queries :as queries]
            [datomic-gql-starter.utils.test :refer [run-mutation run-query]]
            [db :refer [profile]]))

(def query-hashes-per {:artist-releases-tracks-duration-filter -599682531,
                       :artist-composition-with-or-and 985987761,
                       :artist-by-name-exact -518408110,
                       :artist-composition-with-or-and-not 2015599218,
                       :artist-composition-complex -869280560,
                       :artist-by-start-years -224865239,
                       :releases-by-artist 1453565246,
                       :artist-any-start-year 902465945,
                       :artist-by-start-year -458206026,
                       :artist-by-start-years-two -112422524,
                       :artist-no-start-year 247876919,
                       :artist-releases-tracks-duration -354763485,
                       :artist-composition-deep -4303918,
                       :artist-by-start-year-limited 222974068,
                       :artist-start-year-country-composition -1024402097,
                       :artist-composition-with-or 1012501357,
                       :artist-by-start-year-open-range -720640919,
                       :artist-releases 1007188207,
                       :artist-by-name -784265910,
                       :artist-by-start-year-range -218292979,
                       :artist-select-parameters-with-composition -126934530,
                       :artist-releases-tracks-media-duration-filter -1940611863})

(def query-hashes  {:artist-releases-tracks-duration-filter 1083332457,
                    :artist-composition-with-or-and -2132410605,
                    :artist-by-name-exact -518408110,
                    :artist-composition-with-or-and-not 232272096,
                    :artist-composition-complex -1911872483,
                    :artist-by-start-years -425507446,
                    :releases-by-artist -377841966,
                    :artist-any-start-year -2112380707,
                    :artist-by-start-year 1543769258,
                    :artist-by-start-years-two 743704214,
                    :artist-no-start-year 1902589999,
                    :artist-releases-tracks-duration 1306507507,
                    :artist-composition-deep -529059505,
                    :artist-by-start-year-limited -1958763612,
                    :artist-start-year-country-composition -226062991,
                    :artist-composition-with-or 758973407,
                    :artist-by-start-year-open-range 1802552463,
                    :artist-releases -584434062,
                    :artist-by-name -518408110,
                    :artist-by-start-year-range 1494223402,
                    :artist-select-parameters-with-composition 1139058720,
                    :artist-releases-tracks-media-duration-filter 697117567})

(def mutation-hashes-peer {:update-artist-start-day 1702949591,
                           :update-artist-remove-start-day -232537431,
                           :update-artist-add-start-day -2017569654,
                           :insert-artists -1813631293,
                           :insert-releases-artists 545734359,
                           :delete-labels -2017569654})

(def mutation-hashes  {:update-artist-start-day 62777853,
                       :update-artist-remove-start-day 863301782,
                       :update-artist-add-start-day -2017569654,
                       :insert-artists -2017569654,
                       :insert-releases-artists -2017569654,
                       :delete-labels -2017569654})

(defn get-query-hash [query]
  ( ->> query
       name
       (str "queries/")
       symbol
       resolve
       deref
       run-query
       hash))

(defn get-mutation-hash [query]
  ( ->> query
    name
    (str "mutations/")
    symbol
    resolve
    deref
    run-mutation
    hash))

(deftest query-tests
  (doseq [query-hash (if (= profile :devlocal)
                       query-hashes
                       query-hashes-per)]
    (let [[query q-hash] query-hash]
      (testing (str "Query: " (name query))
        (is (= (get-query-hash query) q-hash))))))

(deftest mutation-tests
  (doseq [mutation-hash (if (= profile :devlocal)
                          mutation-hashes
                          mutation-hashes-peer)]
    (let [[mutation m-hash] mutation-hash]
      (testing (str "Mutation: " (name mutation))
        (is (= (get-mutation-hash mutation) m-hash))))))





(comment
  (def mutations ['update-artist-start-day 'update-artist-remove-start-day 'update-artist-add-start-day 'insert-artists
                  'insert-releases-artists 'delete-labels])

  (for [mutation mutations]
    [(keyword mutation) (-> (deref (resolve (symbol (str "mutations/" mutation))))
                          run-mutation
                          hash)])
  (def queries ['artist-by-start-year 'artist-by-start-year-range 'artist-by-start-year-open-range 'artist-by-start-year-limited
                'artist-by-start-years 'artist-by-start-years-two 'artist-any-start-year 'artist-by-name 'artist-by-name-exact
                'artist-start-year-country-composition 'artist-select-parameters-with-composition 'artist-composition-with-or
                'artist-composition-with-or-and 'artist-composition-with-or-and-not 'artist-composition-deep 'artist-composition-complex
                'artist-no-start-year 'artist-releases 'artist-releases-tracks-duration 'artist-releases-tracks-duration-filter
                'artist-releases-tracks-media-duration-filter 'releases-by-artist])

  (for [query queries]
    [(keyword query) (-> (deref (resolve (symbol (str "queries/" query))))
                       run-query
                       hash)]))
