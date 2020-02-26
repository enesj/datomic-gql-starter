(ns catchpocket.lib.config
  (:require [stillsuit.lib.util :as su]))

(def default-config "catchpocket/defaults.edn")

(defn construct-config
  ([config-file]
   (construct-config config-file nil))
  ([config-file overrides]
   (let [defaults (su/load-edn-resource default-config)
         cmd-line (su/load-edn-file config-file)
         cmd-line (update-in cmd-line [:catchpocket/references]
                     #(->> (filterv (fn [x] (:catchpocket/reference-to (val  x))) %)
                        (into {})))
         ;cmd-line (into sorted-map cmd-line)
         merged   (su/deep-map-merge defaults cmd-line overrides)]
     ;(println merged)
     merged)))
