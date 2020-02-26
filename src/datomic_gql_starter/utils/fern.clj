(ns datomic-gql-starter.utils.fern
  (:require [fern.easy :as fe]
            [fern :as f]
            [mount.core :refer [defstate] :as mount]
            [mem-files.core :as mem-files]))

(defmethod fern/literal 'concat
  [_ & args]
  (apply str args))


(def e (fe/file->environment "config.fern"))

(defn fern-e [setting]
  (f/evaluate e setting))

(def refs-conf  (fern-e 'refs-conf))
(def root-dir (fern-e 'root))
(def catchpocket-conf (fern-e 'catchpocket-conf))
(def stillsuit-conf (fern-e 'stillsuit-conf))
(def db-name (fern-e 'db))
(def api-conf (fern-e 'api-conf))


(def refresher
   (let [interval-ms 1000
         keys-files  {:api-conf (fern-e 'api-conf)}]
     (mem-files/start interval-ms keys-files)))

(defn get-conf [k]
  (let [files @refresher]
    (assert (contains? files k) (str "File " k " not registered."))
    (clojure.core/get files k)))


(comment
  (get-conf :api-conf))
