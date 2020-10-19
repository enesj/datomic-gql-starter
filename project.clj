(defproject datomic-gql-starter "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :pedantic? :warn
  :license {:name "EPL"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [
                 [com.cognitect/fern "0.1.5"]
                 [org.clojure/core.rrb-vector "0.0.13"]     ; suppressing Boxed math warning, with  Clojure 1.10
                 [com.walmartlabs/lacinia-pedestal "0.13.0"
                  :exclusions [com.walmartlabs/lacinia com.walmartlabs/lacinia org.slf4j/slf4j-api org.flatland/ordered org.clojure/tools.reader]]
                 [com.walmartlabs/lacinia "0.36.0" :exclusions [org.clojure/tools.logging]]
                 [org.clojure/tools.cli "0.4.2"]
                 [funcool/cuerdas "2.1.0"]
                 [stylefruits/gniazdo "1.1.3" :exclusions [org.eclipse.jetty.websocket/websocket-api org.eclipse.jetty/jetty-http]]
                 [clj-http "3.10.1"]
                 [zprint "0.5.3"]
                 [inflections "0.13.2" :exclusions [commons-codec]]
                 [mount "0.1.16"]
                 [expound "0.8.4"]
                 [vincit/venia "0.2.5"]
                 [org.apache.logging.log4j/log4j-core "2.11.1"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.11.1"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.namespace "1.0.0" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.logging "0.4.1"]
                 [me.dryewo/mem-files "0.1.1"]
                 [hawk "0.2.11"]]

  :min-lein-version "2.8.1"

  :main datomic-gql-starter.core

  ;:source-paths ["src"]
  :resource-paths ["resources"]

  :repl-options {
                 ;; If nREPL takes too long to load it may timeout,
                 ;; increase this to wait longer before timing out.
                 ;; Defaults to 30000 (30 seconds)
                 :timeout 500000}


  :profiles {:client   {:dependencies [[com.datomic/client-pro "0.9.41" :exclusions [org.slf4j/slf4j-nop org.slf4j/slf4j-log4j12
                                                                                     org.eclipse.jetty/jetty-client
                                                                                     org.eclipse.jetty/jetty-http
                                                                                     org.eclipse.jetty/jetty-util]]]
                        :source-paths ["src" "src.client"]}
             :peer     {:dependencies [[com.datomic/datomic-pro "1.0.6202" :exclusions [org.slf4j/slf4j-nop org.slf4j/slf4j-log4j12]]]
                        :source-paths ["src" "src.peer"]}
             :devlocal {:dependencies [[com.datomic/dev-local "0.9.203"]]
                        :source-paths ["src" "src.devlocal"]}
             :test     {:dependencies [[com.datomic/datomic-pro "1.0.6202" :exclusions [org.slf4j/slf4j-nop org.slf4j/slf4j-log4j12]]]
                        :source-paths ["src" "src.test"]}})

