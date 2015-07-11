
(ns hello-world.core
  (:require [cljs.nodejs :as nodejs])) 

;;(defonce conn
;; (repl/connect "http://localhost:9000/repl"))

;; (enable-console-print!)
(nodejs/enable-util-print!)

(println "Hello world!")

(defn  -main [& args]
  (println "Hello world!"))

(set! *main-cli-fn* -main)

;; ADDED
(defn foo [a b]
  (+ a b))
