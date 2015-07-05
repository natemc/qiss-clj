(defproject qiss "0.1.0-SNAPSHOT"
  :description "qiss is simple and succinct: a q-like programming language for the JVM"
  :url "https://github.com/natemc/qiss"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.4.0"]
                 [midje "1.6.3"]
                 [rhizome "0.2.5"]]
  :main ^:skip-aot qiss.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
