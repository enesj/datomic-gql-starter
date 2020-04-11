(defproject datomic-gql-starter "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :pedantic? :warn
  :license {:name "EPL"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [
                 [com.cognitect/fern "0.1.5"]
                 [org.clojure/core.rrb-vector "0.0.13"]     ; suppressing Boxed math warning, with  Clojure 1.10
                 [com.walmartlabs/lacinia-pedestal "0.12.0"
                  :exclusions [com.walmartlabs/lacinia org.flatland/ordered]]
                 [com.walmartlabs/lacinia "0.35.0"]
                 [org.clojure/tools.cli "0.4.2"]
                 [funcool/cuerdas "2.1.0"]
                 [zprint "0.5.3"]
                 [inflections "0.13.2" :exclusions [commons-codec]]
                 [mount "0.1.16"]
                 [org.apache.logging.log4j/log4j-core "2.11.1"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.11.1"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.namespace "1.0.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [me.dryewo/mem-files "0.1.1"]
                 [hawk "0.2.11"]]

  :min-lein-version "2.8.1"

  :main datomic-gql-starter.core

  :source-paths ["src"]
  :resource-paths ["resources"]

  :profiles {:client  {:dependencies [[com.datomic/client-pro "0.8.28" :exclusions [org.slf4j/slf4j-nop org.slf4j/slf4j-log4j12
                                                                                    org.eclipse.jetty/jetty-client
                                                                                    org.eclipse.jetty/jetty-http
                                                                                    org.eclipse.jetty/jetty-util]]]}
             :peer {:dependencies [[com.datomic/datomic-pro "0.9.6045" :exclusions [org.slf4j/slf4j-nop org.slf4j/slf4j-log4j12]]]}
             :free {:dependencies [[com.datomic/datomic-free "0.9.5697" :exclusions [org.slf4j/slf4j-nop org.slf4j/slf4j-log4j12]]]}
             :test {:resource-paths ["test/resources"]}})

