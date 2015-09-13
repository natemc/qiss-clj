(ns qiss.core-macros
  (:require
    ; [qiss.core :refer [atomize]]
            [midje.sweet :refer :all])
  (:gen-class))

;; clojurescript only supports macros fron clojure files
;;  http://stackoverflow.com/questions/9888463/defmacro-not-defined-in-clojurescript

(defn bool? "is x a boolean?" [x]
  (instance? java.lang.Boolean x))
                     ; how in the world is this going to get clojurescripterized?

;; bf bool function, i.e., function to use with two bool args
;; of ordinary function, to use with any other args
(defmacro promote-bools [bf of x y]
  "If x and y are both bool, apply bf; otherwise apply of"
  `(let [f# (fn [a# b#]
              (if (bool? a#)
                (if (bool? b#)
                  (~bf a# b#)
                  (~of (if a# 1 0) b#))
                (if (bool? b#)
                  (~of a# (if b# 1 0))
                  (~of a# b#))))]
     ((atomize f#) ~x ~y)))