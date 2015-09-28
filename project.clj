(defproject qiss "0.1.0-SNAPSHOT"
  :description "qiss is simple and succinct: a q-like programming language for the JVM"
  :url "https://github.com/natemc/qiss"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [testdouble/clojurescript.csv "0.2.0"]
                 [instaparse "1.4.1"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.0"]
                 [midje "1.6.3"]
                 [im.chit/purnam "0.5.2"]
                 [rhizome "0.2.5"]]
  :plugins [[lein-cljsbuild "1.1.0"]
            [lein-npm "0.4.0"]]
  :cljsbuild {
              :builds [{
                        ; The path to the top-level ClojureScript source directory:
                        :source-paths ["src"]
                        ; The standard ClojureScript compiler options:
                        ; (See the ClojureScript compiler documentation for details.)
                        :compiler {
                                   :output-to "war/javascript/main.js" ; default: target/cljsbuild-main.js
                                   :optimizations :whitespace
                                   :pretty-print true
                                   }
                        }]
              }
  :main ^:skip-aot qiss.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
