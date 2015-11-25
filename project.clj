(defproject qiss "0.1.0-SNAPSHOT"
  :description "qiss is short and simple: a q-like programming language for the JVM"
  :url "https://github.com/natemc/qiss"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [instaparse "1.4.1"]
                 [midje "1.6.3"]
                 [rhizome "0.2.5"]
                 ; [gorillalabs/sparkling "1.2.4-SNAPSHOT"]
                  [yieldbot/flambo "0.7.1"]

                 ;; clojurescript from here down
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.0"]
                 [im.chit/purnam "0.5.2"]
                 [org.clojure/clojurescript "1.7.122"]
                 [prismatic/dommy "1.1.0"]
                 [testdouble/clojurescript.csv "0.2.0"]]
  :plugins [[lein-cljsbuild "1.1.0"]
            [lein-figwheel "0.4.0"]]
  :source-paths ["src"]
  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  :cljsbuild {
              :builds [{:id "dev"
                        :figwheel { :on-jsload "qiss.core/on-js-reload" }
                        :source-paths ["src"]
                        :compiler {:asset-path "js/compiled/out"
                                   :main qiss.core
                                   :output-dir "resources/public/js/compiled/out"
                                   :output-to "resources/public/js/compiled/qiss.js"
;;                                   :optimizations :whitespace ;; fig doesn't like
;;                                   :pretty-print true
                                   :source-map-timestamp true}}
                       ;; invoke me via - 'lein figwheel docker' from inside the container
                       ;; see - http://blog.nickol.us/2015/07/20/using-figwheel-with-docker/
                       {:id "docker"
                        :figwheel { :on-jsload "qiss.core/on-js-reload"
                                    :websocket-host "192.168.99.100" }
                        :source-paths ["src"]
                        :compiler {:asset-path "js/compiled/out"
                                   :main qiss.core
                                   :output-dir "resources/public/js/compiled/out"
                                   :output-to "resources/public/js/compiled/qiss.js"
;;                                   :optimizations :whitespace ;; fig doesn't like
;;                                   :pretty-print true
                                   :source-map-timestamp true}}

                       {:id "opt"
                        :source-paths ["src"]
                        :compiler {:main qiss.core
                                   :optimizations :advanced
                                   :output-to "resources/public/js/compiled/qiss.js"
                                   :pretty-print false}}]}
  :figwheel {
             ;; :http-server-root "public" ;; default and assumes "resources"
             ;; :server-port 3449 ;; default
             ;; :server-ip "127.0.0.1"

             :css-dirs ["resources/public/css"] ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process
             ;; :nrepl-port 7888

             ;; Server Ring Handler (optional)
             ;; if you want to embed a ring handler into the figwheel http-kit
             ;; server, this is for simple ring servers, if this
             ;; doesn't work for you just run your own server :)
             ;; :ring-handler hello_world.server/handler

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log"
             }
  :main qiss.core
  :target-path "target/%s"
  :aot [#".*" flambo.function qiss.core clojure.tools.logging.impl sparkling.serialization sparkling.destructuring]
  :profiles {:dev {:aot [#".*" qiss.core clojure.tools.logging.impl sparkling.serialization sparkling.destructuring]}
             :uberjar {:aot :all}
             :provided {:dependencies [[org.apache.spark/spark-core_2.10 "1.5.0"]
                                       [org.apache.spark/spark-streaming_2.10 "1.5.0"]
                                       [org.apache.spark/spark-streaming-kafka_2.10 "1.5.0"]
                                       [org.apache.spark/spark-streaming-flume_2.10 "1.5.0"]
                                       [org.apache.spark/spark-sql_2.10 "1.5.0"]]}})
