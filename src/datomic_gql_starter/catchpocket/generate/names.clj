(ns datomic-gql-starter.catchpocket.generate.names
  (:require [cuerdas.core :as str]))

(def all-name-styles
  "Set of every possible `style` value for the `(keyword-part->type)` function"
  #{:snake_case :Snake_Case :SNAKE_CASE :CamelCase :camelCase})

(defn keyword-part->type
  "Transform a keyword from sausage-case to another style."
  [kw style]
  (if-not (all-name-styles style)
    kw
    (let [capitalize? (#{:Snake_Case :CamelCase :camelCase} style)
          underjoin?  (#{:Snake_Case :snake_case :SNAKE_CASE} style)
          up-first?   (#{:Snake_Case :CamelCase} style)
          uppercase?  (#{:SNAKE_CASE} style)
          parts       (-> kw
                          str/kebab
                          (str/split #"[^A-Za-z0-9]+"))
          xform       (cond capitalize? str/capital
                            uppercase?  str/upper
                            :else       identity)
          xform-head  (cond up-first?  str/capital
                            uppercase? str/upper
                            :else      str/lower)
          separator   (if underjoin? "_" "")
          camels      (->> parts
                           (remove str/blank?)
                           (map xform))
          complete    (concat [(-> camels first xform-head)]
                              (rest camels))]
      (->> complete
           (str/join separator)
           keyword))))

(defn lacinia-type-name
  "Given a datomic attribute keyword such as `:my-thing/attribute-name` and a catchpocket config
  map, convert the keyword's namespace to a lacinia type name suchs as `:MyThing`."
  [attribute-kw {:keys [:catchpocket/names]}]
  ;(println "attr" attribute-kw)
  (keyword-part->type
    (namespace attribute-kw)
    (get names :objects :CamelCase)))

(defn lacinia-field-name
  [attribute-kw {:keys [:catchpocket/names]}]
  (keyword-part->type
    (name attribute-kw)
    (get names :fields :snake_case)))

(defn enum-value-name
  [datomic-kw {:keys [:catchpocket/names] :as config}]
  (keyword-part->type
    (name datomic-kw)
    (get names :enums :SNAKE_CASE)))

(defn query-name
  "Given a "
  [kw {:keys [:catchpocket/names]}]
  (let [obj-style (get names :objects :CamelCase)
        query-cf  (get names :queries obj-style)]
    (cond
      (all-name-styles query-cf)
      (keyword-part->type kw query-cf)

      (ifn? query-cf)
      (query-cf kw))))


(defn db-id-name
  "Return the lacinia name for the :db/id datomic field, which is used in the interface definition
  for entities."
  [config]
  (lacinia-field-name :db-id config))
