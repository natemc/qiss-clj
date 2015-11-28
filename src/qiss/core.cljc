(ns qiss.core
  #?@(:clj [(:require [clojure-csv.core :as csv]
                      [clojure.java.io :as io]
                      [clojure.stacktrace :as st] ;; handy from the repl
                      [clojure.string :as str]
                      [instaparse.core :as insta]
                      [instaparse.viz :as instav])
            (:gen-class)])
  #?(:cljs (:require [clojure.string :as str]
                     [dommy.core :as dom :refer-macros [sel sel1]]
                     [goog.net.XhrIo :as xhr]
                     [goog.string :as gstring :refer [format]]
                     [instaparse.core :as insta]
                     [instaparse.viz :as instav]
                     [testdouble.cljs.csv :as csv])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transducers
;; NOT USED.  These are just here to think about.
(defn mapt [f] ;; function to apply to each ingested item
  (fn ([rf]    ;; reducing function, e.g., conj.  rf is the process
       (fn ([] (rf)) ;; identity
         ([x] (rf x)) ;; completion
         ([x y] (rf x (f y))))))) ;; apply f to y and reduce into x
(defn filtert [p] ;; predicate
  (fn ([rf]       ;; reducing function
       (fn ([] (rf)) ;; identity
         ([x] (rf x)) ;; completion
         ([x y] (if (p y) (rf x y) x)))))) ;; include y if p y
(defn tduce [xform f init coll]
  (let [xf (xform f)]
    (xf (reduce xf init coll))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#?(:cljs (enable-console-print!)) ;; println -> js/console.log
#?(:cljs (defn on-js-reload [])
;;           (swap! genv update-in [:__figwheel_counter] inc)))
         )
#?(:cljs (defn response-text [x] (.getResponseText (.-target x))))

;; Backlog
;;   @ 3&4 args on keyed tables
;;   More .
;;   builtins
;;   update, insert
;;   conditional $
;;   error messages
;;   nulls
;;   k-ish console output
;;   java interop
;;   attributes
;;   ej, ij
;;   enable UDFs to modify the global env
;;   dot notation for dictionaries incl locals
;;   time types
;;   aj
;;   \t and do (see clojure's time and dotimes functions)
;;   system
;;   aj
;;   something like functional query but easier to use

;; functions that will differ btwn JVM and JS
(defn bool? "is x a boolean?" [x]
  #?(:clj  (instance? java.lang.Boolean x)
     :cljs (= (type x) js/Boolean)))
(declare lose-env)
(defn err "throw x" [& x]
  (throw #?(:clj (Exception. (str/join ["'" (str/join " " (map lose-env x))]))
            :cljs (js/Error. (str/join ["'" (str/join " " (map lose-env x))])))))
(defn exit
  "exit this process"
  ([] (exit 0))
  ([x] #?(:clj (System/exit x))))
;; ACK js is all Number.  Use meta to distinguish?
#?(:cljs (defn float? [x] (not= x (.floor js/Math x))))
(defn index-of
  "The first index in x where e appears, or (count x) if e does not
  exist in x"
  [x e]
;;  (when (:stream e) (err "nyi"))
  #?(:clj  (let [i (.indexOf x e)] (if (< i 0) (count x) i))
           :cljs (loop [i 0 p x]
                   (if (or (empty? p) (= e (first p)))
                     i
                     (recur (inc i) (rest p))))))
;; there's not runtime eval in clojurescript
;; Really? see
;; https://github.com/clojure/clojurescript/wiki/Bootstrapping-the-Compiler
;; http://swannodette.github.io/2015/07/29/clojurescript-17/
(defn k-new
  "create instance of java class x using params in y"
  ([x] #?(:clj (eval (read-string (str "(new " (name x) ")")))))
  ([x y] #?(:clj (eval (read-string (str "(new "
                                         (name x)
                                         " "
                                         (str/join " " y)
                                         ")"))))))
(defn kstring? [s]
  #?(:clj   (every? char? s)
      :cljs (every? #(and (string? %) (= 1 (count %))) s)))
#?(:cljs (defn log [x] (.log js/console x)))
(defn now []
  #?(:clj  (.getTime (java.util.Date.))
     :cljs (.getTime (js/Date.))))
(declare string)
(defn read-lines "read file x as a vector of strings, one string per line" [x]
  #?(:clj (with-open [r (io/reader (string x))] (vec (line-seq r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qiss data structure ctors, accessors, mutators
(defn make-dict ([] (make-dict [] [])) ([k v] {:k k :v v}))
(defn make-keyed-table [k v] {:k k :v v :kt true})
(defn make-table [c d] {:k c :v d :t true})
(defn dict? [x] (and (map? x) (:k x) (:v x) (not (:t x)) (not (:kt x))))
(defn dict-key [x] (:k x))
(defn dict-val [x] (:v x))
(defn keyed-table? [x] (and (map? x) (:k x) (:v x) (:kt x)))
(defn lambda? [x] (and (map? x) (:f x)))
(defn lambda-body [x] (:exprs x))
(defn lambda-code [x] (:f x))
(defn lambda-callable [e x]
  (let [c (lambda-code x)]
    (if (:pass-global-env x) (partial c e) c)))
(defn lambda-env [x] (:env x))
(defn lambda-formals [x] (:formals x))
(defn lambda-rank [x] (:rank x))
(defn lambda-text [x] (:text x))
(declare stream?)
(defn map-snippet [n orig]
  (reduce (fn [m k] (assoc m k (lose-env (k orig))))
          {}
          (take n (keys orig))))
(defn lose-env [x]
  (cond (vector? x) (mapv lose-env x)
        (stream? x) {:stream true}
        (lambda? x) (assoc x :env (lose-env (:env x)))
        (and (map? x) (< 5 (count x))) (map-snippet 2 x)
        :else       x))
;;    (if (lambda? x) (dissoc x :env) x)))
(defn snapshot? [x] (:snapshot x))
(defn snapshot-aware? [x r] ;; lambda, rank
  (and (:snapshot-aware x) (some #{r} (:snapshot-aware x))))
(defn snapshot-event [x] (:event x))
(defn snapshot-final? [x] (:final x))
(defn snapshot-source [x] (:source x))
(defn snapshot-time [x] (:time x))
(defn snapshot-value [x] (if (snapshot? x) (:value x) x))
(def last-event-id (atom 0))
(defn make-snapshot [source x]
  ;; sometimes you make a snapshot but you already have one:
  ;; {x}@>=!3 / lambdas are snapshot-aware in case their innards are
  (if (snapshot? x)
    x
    {:event    (swap! last-event-id inc)
     :snapshot true
     :source   source
     :time     (now)
     :value    x}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn derive-snapshot [old new-value]
  "Create a new snapshot will the same attributes as old except value new-value"
  ;; Currently the effect of derive-snapshot is to propagate the source
  ;; and timestamp of the snapshot of an initial event to all dependent
  ;; snapshots.
  (cond (snapshot? new-value) new-value
        (snapshot? old)       (assoc old :value new-value)
        :else (err "derive-snapshot's first arg must be a snapshot; got" old)))
(defn make-snapshot-final [x]
  ;; assert snapshot? x
  (assoc x :final true))
(defn swallow [x] (assoc x :swallow true))
(defn swallow? [x] (:swallow x))
(defn stream? [x] (:stream x))
(defn stream-aware? [x r] ;; lambda, rank
  (and (:stream-aware x) (some #{r} (:stream-aware x))))
(defn stream-sources [x] (:sources x))
(defn table? [x] (and (map? x) (:k x) (:v x) (:t x)))
(defn add-to-dict [d k v] (assoc d :k (conj (:k d) k) :v (conj (:v d) v)))
(defn catv "concat x and y into a vector" [x y] (vec (concat x y)))
(defn removev "remove the ith element(s) of v" [v i]
  (if (vector? i)
    (loop [x v j (sort i) k 0 r []]
      (cond (empty? x)      r
            (= k (first j)) (recur (next x) (next j) (inc k) r)
            :else           (recur (next x) j (inc k) (conj r (first x)))))
    (catv (subvec v 0 i) (subvec v (+ 1 i) (count v)))))
(defn remove-from-dict [d k]
  "Remove the elements of d's key and value corresponding to key element k"
  (let [i (index-of (:k d) k)]
    (if (= i (count (:k d)))
      d
      (make-dict (removev (:k d) i) (removev (:v d) i)))))
(defn cols "the names of the columns of table (or keyed table) x" [x]
  (cond (table? x)       (dict-key x)
        (keyed-table? x) (catv (cols (dict-key x)) (cols (dict-val x)))
        :else            (err "cols cannot be applied to" x)))
(defn keycols "the names of the columns of keyed table x" [x]
  (if (keyed-table? x)
    (cols (dict-key x))
    (err "keycols cannot be applied to " x)))
(defn d-from-t "dict from table" [x] (dissoc x :t))
(defn t-from-d "table from dict" [x] (assoc x :t true))

;; keyed tables
(declare any?)
(declare except)
(declare index)
(declare kcount)
(declare til-count)
(defn unkey-table [x]
  "The unkeyed form of x (x must be a table or keyed table)"
  (if (table? x)
    x
    (let [k (dict-key x) v (dict-val x)]
      (make-table (catv (dict-key k) (dict-key v))
                  (catv (dict-val k) (dict-val v))))))
(defn key-table-by-colname [x y]
  "Key the table (or keyed table) y using the column named by x"
  (let [i (index-of (dict-key y) x)]
    (if (= i (count (dict-key y)))
      (err "mismatch: key col" x y)
      (make-keyed-table (make-table [x] [((dict-val y) i)])
                        (make-table (removev (dict-key y) i)
                                    (removev (dict-val y) i))))))
(defn key-table-by-colnames [x y]
  "Key the table (or keyed table) y using the columns named by x"
  (let [i (mapv #(index-of (dict-key y) %) x)]
    (if (any? (mapv (partial = (count (dict-key y))) i))
      (err "mismatch: key cols" x y)
      (make-keyed-table
       (make-table x (index (dict-val y) i))
       (make-table (except (dict-key y) x)
                   (index (dict-val y)
                          (except (til-count (dict-key y)) i)))))))
(defn key-table-by-long [x y]
  "Key the table (or keyed table) y using the first x columns"
  (cond (= x 0) y
        (< x 0) (err "lhs of ! must be >=0 when keying a table")
        (<= (count (dict-key y)) x) (err "can't key" x "cols from" y)
        :else (make-keyed-table (make-table (vec (take x (dict-key y)))
                                            (vec (take x (dict-val y))))
                                (make-table (vec (drop x (dict-key y)))
                                            (vec (drop x (dict-val y)))))))
(defn key-table [x y]
  "Key table y with the columns indicated by x"
  (cond (table? x)   (if (= (kcount x) (kcount y))
                       (make-keyed-table x y)
                       (err "length" x y))
        (vector? x)  (key-table-by-colnames x y)
        (number? x)  (key-table-by-long x y)
        (keyword? x) (key-table-by-colname x y)
        :else        (err "nyi" x "!" y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handy code
(declare invoke)
(defonce DEBUG false)
(defn null!
  "Like 0N! but variadic, e.g., (null! msg thing) => thing"
  [& x]
  (if DEBUG
    ;; when printing functions, don't print the whole env
    (apply println (map lose-env x)))
  (last x))

(defn all? "is every x truthy?" [x] (if (every? (fn [x] x) x) true false))
(defn any? "is any x truthy?" [x] (if (some (fn [x] x) x) true false))
(defn bool-to-long "convert x, if boolean, to long" [x]
  (cond (coll? x) (mapv bool-to-long x)
        (bool? x) (if x 1 0)
        :else     x))
(defn compose [f g] ;; compose f and g
  "Compose two qiss functions"
  {:f       (fn [e & x]
              (let [[e2 y] (invoke e g x)]
                (last (invoke e2 f [y]))))
   :pass-global-env true
   :formals (:formals g)
   :rank    (:rank g)
   :text    (str (:text f) "comp" (:text g))})
(defn except "the elements in x not in y" [x y]
  (let [p (cond (vector? y) #(some #{%} y)
                (dict?   y) #(some #{%} (dict-val y))
                :else       #(= % y))]
    (vec (remove p x))))
(defn in "true for those elements of x that exist in y" [x y]
  (if (vector? x)
    (mapv #(in % y) x)
    (if (some #{x} y) true false)))
(defn inter
  "Intersection of 2 vectors.  Preserves the order per x"
  [x y] (vec (filter #(some #{%} y) x)))
(declare index)
(defn klast "the last element of x" [x]
  (cond (vector? x) (last x)
        (table? x)  (index x (- (count x) 1))
        (map? x)    (klast (dict-val x))
        :else       (err "can't apply last to" x)))
(defn last-index-of
  "The last index in x where e appears, or (count x) if e does not
  exist in x"
  [x e] (let [i (index-of (vec (reverse x)) e)]
          (if (= (count x) i)
            i
            (- (count x) (+ 1 i)))))
(defn like [x pattern]
  (let [p (re-pattern (string pattern))
        h (fn h [i]
            (cond (keyword? i) (not (nil? (re-find p (name i))))
                  (kstring? i) (not (nil? (re-find p (str/join i))))
                  :else        (mapv h i)))]
    (h x)))
(defn push [x v]
  "Prepend x to v"
  (vec (cons x (map identity v))))
(defn raze "flatten one level" [x] (vec (mapcat #(if (coll? %) % [%]) x)))
(defn string [x]
  "make a string from one of string, keyword, vector<char>"
  (cond (string? x)  x
        (keyword? x) (name x)
        :else        (str/join x))) ;; vector of char
(declare kcount)
(defn til [x] (vec (range x)))
(defn til-count "0..count[x]-1" [x] (vec (range (kcount x))))
(defn union
  "Union of 2 vectors.  Preserves the order of x and the items added
  from y retain their relative order as well"
  [x y] (vec (distinct (concat x y))))
(defn where
  "All elements of x must be integers.  For each element of x (vector
  or dict), concat that many copies of that element's index"
  ([x] (vec (flatten (if (vector? x)
                       (map-indexed #(repeat %2 %1) (bool-to-long x))
                       (map #(repeat %2 %1) (dict-key x) (dict-val x))))))
  ([e x] [e (where x)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare eq)
(declare findv)
(declare index)
(declare less)
(defn atomize
  "From a dyadic function that takes atoms, create a dyadic function
  that automatically vectorizes"
  [f] ;; not the full structure but just the code - is this a problem?
  (fn self[x y]
    (cond (vector? x) (cond (vector? y) (if (= (count x) (count y))
                                          (mapv self x y)
                                          (err "length" f x y))
                            (dict? y)   (if (= (count x) (count (dict-key y)))
                                          (make-dict (dict-key y)
                                                     (mapv self x (dict-val y)))
                                          (err "length" f x y))
                            (coll? y)   (err "nyi" f x y)
                            :else       (mapv #(self % y) x))
          (dict? x)   (cond (vector? y) (if (= (count (dict-key x)) (count y))
                                          (make-dict (dict-key x)
                                                     (mapv self (dict-val x) y))
                                          (err "length" f x y))
                            (dict? y) ;; is this really so complicated?
                            (let [p (findv (dict-key x) (dict-key y)) ;; pos y in x
                                  n (index (dict-val y) ;; new items from y
                                           (where (eq p (count (dict-key x)))))
                                  o (where (less p (count (dict-key x))))];; overlap
                              (make-dict (union (dict-key x) (dict-key y))
                                         (reduce
                                          (fn [v [i b]]
                                            (assoc v i (f (v i) b)))
                                          (vec (concat (dict-val x) n))
                                          (map vector
                                               (index p o)
                                               (index (dict-val y) o)))))
                            (coll? y)   (err "nyi" f x y)
                            :else       (make-dict (dict-key x)
                                                   (mapv #(self % y) (dict-val x))))
          (table? x)  (t-from-d (self (d-from-t x) y)) ;; TODO more cases?
          (keyed-table? x) (make-keyed-table (dict-key x) (self (dict-val x) y))
          (coll? x)   (err "nyi" f x y)
          ;; x is atom
          (vector? y) (mapv #(self x %) y)
          (dict? y)   (make-dict (dict-key y) (mapv #(self x %) (dict-val y)))
          (table? y)  (make-table (cols y) (mapv #(self x %) (dict-val y)))
          :else       (f x y))))

;; AFAIK it is impossible to write promote-bools as a macro
;; in a platform-independent way.
;; You can't have a macro that depends on JS-specific stuff.
;; In this case, bool? is platform specific.
;; Hence, I rewrote it as a function below.
;;
;; bf bool function, i.e., function to use with two bool args
;; of ordinary function, to use with any other args
;; (defmacro promote-bools [bf of x y]
;;   "If x and y are both bool, apply bf;
;;    otherwise promote x and/or y to long and then apply of"
;;   `(let [f# (fn [a# b#]
;;               (if (bool? a#)
;;                 (if (bool? b#)
;;                   (~bf a# b#)
;;                   (~of (if a# 1 0) b#))
;;                 (if (bool? b#)
;;                   (~of a# (if b# 1 0))
;;                   (~of a# b#))))]
;;      ((atomize f#) ~x ~y)))

;; bf bool function, i.e., function to use with two bool args
;; of ordinary function, to use with any other args
(defn promote-bools [bf of x y]
  "If x and y are both bool, apply bf;
   otherwise promote x and/or y to long and then apply of"
  (let [f (fn [a b] (if (bool? a)
                      (if (bool? b)
                        (bf a b)
                        (of (if a 1 0) b))
                      (if (bool? b)
                        (of a (if b 1 0))
                        (of a b))))]
    ((atomize f) x y)))
(defn amp
  "&x (where)      and    x&y (atomic min)"
  ([x]  (if (snapshot? x)
          (if (snapshot-value x)
            (derive-snapshot x true)
            (swallow x))
          (where x)))
  ([x y] (promote-bools (fn [& b] (and b)) min x y)))
(defn div "atomic integer division" [x y] ((atomize quot) x y))
(declare group)
(defn eq
  "=x (group)      and    x=y (atomic equals)"
  ([x] (group x))
  ([x y] ((atomize =) x y))) ;; TODO: promote long => double, fuzzy doubles
(defn fdiv
  "%x (reciprocal) and x%y (atomic floating-point division)"
  ([x] (if (coll? x) (mapv #(double (/ %)) x)
           (double (/ x))))
  ([x y] ((atomize #(double (/ %1 %2))) x y)))
(declare into-stream)
(defn ge
  "atomic >="
  ([x] (into-stream x)) ;; monadic ">=x" creates a stream from x
  ([x y] ((atomize >=) x y)))
(defn greater
  ">x (idesc)      and    x>y (atomic >)"
  ([x] ;; idesc
   (cond (vector? x)      (vec (sort-by x (comp - compare) (til-count x)))
         (dict?   x)      (index (dict-key x) (greater (dict-val x)))
         (table?  x)      (greater (vec (apply (partial map vector)
                                               (dict-val x))))
         (keyed-table? x) (index (dict-key x) (greater (dict-val x)))
         :else            (err "can't >" x)))
  ([x y] ((atomize >) x y)))
(defn kmod "atomic mod" [x y] ((atomize mod) x y))
(declare wait)
(defn le "atomic <="
  ([x] (wait x)) ;; monadic "<=x" waits for stream x to complete
  ([x y] ((atomize <=) x y)))
(defn less
  "<x (iasc)      and    x<y (atomic <)"
  ([x] ;; iasc
   (cond (vector? x)      (vec (sort-by x (til-count x)))
         (dict?   x)      (index (dict-key x) (less (dict-val x)))
         (table?  x)      (less (vec (apply (partial map vector) (dict-val x))))
         (keyed-table? x) (index (dict-key x) (less (dict-val x)))
         :else            (err "can't <" x)))
  ([x y] ((atomize <) x y)))
(defn minus
  "-x (unary negation) and x-y (atomic minus)"
  ([x] (- x))
  ([x y] ((atomize -) x y)))
(defn neq
  "x<>y (atomic not-equals)"
  ([x y] ((atomize not=) x y))) ;; TODO: promote long => double, fuzzy doubles
(defn pipe
  "|x (reverse) and x|y (atomic max)"
  ([x] (vec (reverse x)))
  ([x y] (promote-bools (fn [& b] (or b)) max x y)))
(declare flip)
(defn plus
  "+x (flip) and x+y (atomic addition)"
  ([x] (flip x))
  ([x y] (promote-bools #(+ (map bool-to-long [%1 %2])) + x y)))
(defn sv [x y]
  "string from vector"
  (vec (str/join (if (vector? x) (str/join x) x)
                 (mapv str/join y))))
(defn tilde
  "~x (not) and x~y (match, i.e., non-atomic equals)"
  ([x] ;; not
   (cond (vector? x)      (mapv tilde x)
         (dict? x)        (make-dict (dict-key x) (tilde (dict-val x)))
         (table? x)       (make-table (cols x) (tilde (dict-val x)))
         (keyed-table? x) (make-keyed-table (dict-key x) (tilde (dict-val x)))
         :else            (if (= 0 x) true (not x))))
  ([x y] (= x y))) ;; match
(defn first-from-snapshot [x]
  ;; :extract true is a hack to work with wait
  (assoc (make-snapshot-final x) :extract true))
(defn times
  "*x (first) and x*y (atomic multiplication)"
  ([x] (cond (snapshot? x) (first-from-snapshot x)
             :else         (first x)))
  ([x y] ((atomize *) x y)))
(defn vs [x y]
  "vector from string"
  (mapv vec (str/split (str/join y)
                       (re-pattern (if (vector? x) (str/join x) (str x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-atomic operators
(defn bang
  "!x (the key of x) and x!y (key y using x)"
  ([x] (cond (number? x)      (vec (range x))
             (vector? x)      (til-count x)
             (dict? x)        (dict-key x)
             (keyed-table? x) (dict-key x)
             (snapshot? x)    (snapshot-time x)
             :else (err "nyi: monadic ! on" x)))
  ([x y] (cond (table? y)               (key-table x y)
               (keyed-table? y)         (key-table x (unkey-table y))
               (vector? x)              (if (vector? y)
                                          (if (= (count x) (count y))
                                            (make-dict x y)
                                            (err "length" x y))
                                          (err "mismatch" x "!" y))
               (or (coll? x) (coll? y)) (err "type" x y)
               :else                    (make-dict [x] [y]))))
(defn flip [x]
  "For a vector of vectors, switch the rows and columns.  For a dict,
  make a table; from a table, make a dict."
  (cond (vector? x) (apply mapv vector x)
        (table? x)  (d-from-t x)
        (dict? x)   (if (and (all? (mapv keyword? (dict-key x)))
                             (all? (mapv vector? (dict-val x)))
                             (apply = (mapv count (dict-val x))))
                      (t-from-d x)
                      (err "can only flip column dicts"))
        :else       (err "nyi: flip" x)))
(defn group [x]
  (cond (vector? x) (let [k (vec (distinct x))]
                      (make-dict k
                                  (reduce (fn [v [i g]]
                                            (let [j (index-of k g)]
                                              (assoc v j (conj (v j) i))))
                                          ;; couldn't make nested transients work
                                          (vec (repeat (count k) []))
                                          (map vector (iterate inc 0) x))))
        (dict? x)   (index (dict-key x) (group (dict-val x)))
        :else       (err "can't group " x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-stream
   ;; a stream is an observeable and an observer
  ([sources on-next]
   (make-stream sources on-next (fn [] nil)))
  ([sources on-next on-done]
   (let [subs   (atom [])
         ondone (fn []
                  (let [s @subs]
                    (when-let [i (on-done)]
                      (doseq [k (if (vector? i) i [i]) x s]
                        ((:on-next x) k)))
                    (doseq [x s] ((:on-done x))))
                  (reset! subs nil))
         onnext (fn [ss]
                  (when-let [i (on-next ss)]
                    (let [j (if (vector? i) i [i])]
                      (doseq [k j s @subs] ((:on-next s) k))
                      (when (snapshot-final? (last j))
                        (ondone)))))]
     {:on-done ondone
      :on-next onnext
      :sources sources
      :stream  true
      :sub     #(swap! subs conj %)
      :unsub   #(swap! subs except %)})))
(defn join-stream-to-stream [s t]
  (let [obs-s     (atom nil)
        obs-t     (atom nil)
        subs      (atom [])
        on-next   #(doseq [x @subs] ((:on-next x) %))
        on-done-t (fn []
                    (reset! obs-t nil)
                    (doseq [x @subs] ((:on-done x)))
                    (reset! subs nil))
        on-done-s (fn []
                    (reset! obs-s nil)
                    ((:sub t) (reset! obs-t
                                      {:on-done on-done-t :on-next on-next}))
                    nil)
        ;; we have a potential problem: sources needs to change through time.
        r         {:sources (vec (distinct (mapcat :sources [s t])))
                   :stream  true
                   :sub     #(swap! subs conj %)
                   :unsub   #(when (= [] (swap! subs except %))
                               ((:unsub s) @obs-s)
                               ((:unsub t) @obs-t))}]
    ((:sub s) (reset! obs-s {:on-done on-done-s :on-next on-next}))
    r))
(defn join-stream-to-vec [s v]
  (let [on-done (fn [] (mapv #(make-snapshot 0 %) v))
        on-next identity
        r       (make-stream (stream-sources s) on-next on-done)]
    ((:sub s) r)
    r))
(defn join-vec-to-stream [v s]
  (let [items   (mapv #(make-snapshot 0 %) v)
        started (atom 0) ;; 1 => first time we got an event from s
        on-done (fn [] (when (= 1 (swap! started inc)) items))
        on-next #(if (not= 1 (swap! started inc)) % (conj items %))
        r       (make-stream (stream-sources s) on-next on-done)]
    ((:sub s) r)
    r))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn join
  ",x (enlist) and x,y (join)"
  ([x] [x])
  ([x y] (cond (vector? x)    (cond (vector? y) (vec (concat x y))
                                    (stream? y) (join-vec-to-stream x y)
                                    :else       (conj x y))
               (stream? x)    (cond (stream? y) (join-stream-to-stream x y)
                                    (vector? y) (join-stream-to-vec x y)
                                    :else       (join-stream-to-vec x [y]))
               (dict? x)      (if (dict? y)
                                ((atomize (fn [_ y] y)) x y)
                                (err "can't join" x y))
               (table? x)     (err "nyi: join tables")
               (vector? y)    (vec (cons x y))
               (stream? y)    (join-vec-to-stream [x] y)
               (or (coll? x)
                   (coll? y)) (err "can't join" x y)
               :else          [x y])))

(declare findv)
(declare ktake)
(defn take-from-vec [n x]
  "n#x where n is an integer and x is a vector"
  (if (<= 0 n)
    (mapv #(x (mod % (count x))) (range n))
    (mapv #(x (- (count x) 1 (mod % (count x))))
          (reverse (range (- n))))))
(defn take-from-dict [n x]
  "n#x where n is an integer, keyword, or vector of keywords, and x is a dict"
  (cond (number? n) (make-dict (ktake n (dict-key x)) (ktake n (dict-val x)))
        (keyword? n) (take-from-dict [n] x)
        ;; TODO: introduce nulls like q does in this case?
        (not (every? #(some #{%} (dict-key x)) n)) (err "mismatch: #" n x)
        :else (make-dict n (index (dict-val x) (findv (dict-key x) n)))))
(defn take-from-table [n x]
  "n#x where n is an integer, keyword, or vector of keywords, and x is a table"
  (cond (number? n) (make-table (cols x)
                                (mapv (partial ktake n) (dict-val x)))
        (keyword? n) (take-from-table [n] x)
        (not (every? #(some #{%} (cols x)) n)) (err "mismatch: #" n x)
        :else (make-table n (index (dict-val x) (findv (cols x) n)))))
(defn take-from-keyed-table [n x]
  "n#x where n is an integer, keyword, or vector of keywords, and x is
  a keyed table"
  (if (number? n)
    (make-keyed-table (ktake n (dict-key x)) (ktake n (dict-val x)))
    (take-from-table n (unkey-table x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn take-from-stream [n x]
  (let [w       (atom []) ; window for overtaking
        obs     (atom nil)
        on-done (fn []
                  ((:unsub x) @obs)
                  (let [ww @w]
                    (mapv #(ww (mod % (count ww))) (range (count ww) n))))
        on-next (fn [ss]
                  (let [ww (swap! w #(if (<= n (count %)) % (conj % ss)))]
                    (if (not= (count ww) n) ss (make-snapshot-final ss))))
        s       (make-stream (:sources x) on-next on-done)
        ;; override sub to call on-done immediately if 0#
        os      (assoc s :sub #(if (= n 0) ((:on-done %)) ((:sub s) %)))]
    ((:sub x) (reset! obs os))
    os))
(defn take-last-from-stream [n x]
  (let [w       (atom []) ; window
        on-done (fn []
                  (let [ww @w]
                    (mapv #(ww (- (count ww) 1 (mod % (count ww))))
                          (reverse (range n)))))
        on-next (fn [ss]
                  (swap! w #(conj (if (< (count %) n) % (vec (drop 1 %))) ss))
                  nil)
        s       (make-stream (:sources x) on-next on-done)]
    ((:sub x) s)
    s))
(defn kcount [x]
  "#x (count)  returns 1 for atoms"
  (cond (vector? x)      (count x)
        (dict? x)        (count (dict-val x))
        (table? x)       (count (first (dict-val x)))
        (keyed-table? x) (count (first (dict-val (dict-key x))))
        :else            1))
(defn matrix-from-vector [m n v] ;; rows cols vec
  (mapv (fn [r]
          (mapv (fn [c] (v (mod (+ (* r n) c) (count v))))
                (range n)))
        (range m)))
(defn fixed-rows-flex-cols [m v]
  ;; This is not exactly how k does it.
  ;; k gives t cols until you don't have enough elements
  ;; which sometimes yields fewer than m rows.
  ;; This gives fewer than t cols earlier so you always
  ;; get m rows (unless count[v]<m).
  (if (<= (count v) m)
    (mapv (fn [x] [x]) v)
    (let [c (/ (count v) (float m))
          t (int (Math/ceil c))             ;; cols in big rows
          u (int (Math/floor c))]           ;; cols in small rows
      (if (= t u)
        (matrix-from-vector m t v)
        (let [s (- (int (/ 1 (- c (int c)))) 1) ;; # of small rows
              b (- m s)]                        ;; # of big rows
          (catv (matrix-from-vector b t v)
                (matrix-from-vector s u (vec (drop (* b t) v)))))))))
(defn fixed-cols-flex-rows [n v]
  (if (<= (count v) n)
    [v]
    (let [r (/ (count v) (float n))
          t (int (Math/ceil r))      ;; rows
          u (int (Math/floor r))]    ;; full rows
      (if (= t u)
        (matrix-from-vector t n v)
        (catv (matrix-from-vector u n v)
              (matrix-from-vector 1
                                  (- (count v) (* u n))
                                  (vec (drop (* u n) v))))))))
(defn fixed-cols-from-stream [n x]
  (let [w       (atom []) ; window for collecting snapshots
        on-done (fn []
                  (let [ww @w]
                    (when (< 0 (count ww))
                      (make-snapshot 0 ww))))
        on-next (fn [ss]
                  (when (< 0 n)
                    (let [ww (swap! w conj (snapshot-value ss))]
                      (when (= (count ww) n)
                        (reset! w [])
                        (derive-snapshot ss ww)))))
        s       (make-stream (:sources x) on-next on-done)
        ;; override sub to call on-done immediately if 0#
        os      (assoc s :sub #(if (= n 0) ((:on-done %)) ((:sub s) %)))]
    ((:sub x) os)
    os))
(defn reshape-from-stream [x y]
  (let [[m n] x]
    (cond (< m 0) (fixed-cols-from-stream n y)
          (< n 0) (err "cannot take unbounded cols from a stream")
          :else   (take-from-stream m (fixed-cols-from-stream n y)))))
(defn reshape-from-vector [x y]
  (if (= 0 (count y))
    []
    (let [[m n] x]
      (cond (< m 0) (fixed-cols-flex-rows n y)
            (< n 0) (fixed-rows-flex-cols m y)
            :else   (matrix-from-vector m n y)))))
(defn reshape [x y]
  (cond (= 1 (count x))
        (err "does reshape with length 1 vector on lhs make sense?")
        (< 1 (apply + (map #(if (< % 0) 1 0) x)))
        (err "reshape can take at most one infinite dimension")
        (< 2 (count x))
        (err "nyi: reshape with lhs longer than 2")
        (vector? y) (reshape-from-vector x y)
        (stream? y) (reshape-from-stream x y)
        :else
        (err "nyi: reshape only supports vector on rhs")))
(defn ktake [x y]
  "x#y take from y the elements specified by x"
  ;; TODO: support stream for x?
  (if (and (coll? x) (not (empty? x)) (every? number? x))
    (reshape (mapv #(if (bool? %) (if % 1 0) %) x) y)
    (let [n (if (bool? x) (if x 1 0) x)]
      (cond (not (coll? y))  (vec (repeat n y))
            (vector? y)      (take-from-vec n y)
            (dict? y)        (take-from-dict n y)
            (table? y)       (take-from-table n y)
            (keyed-table? y) (take-from-keyed-table n y)
            (stream? y)      (if (< n 0)
                               (take-last-from-stream (- n) y)
                               (take-from-stream n y))
            :else            (err "nyi: # on " y)))))
(defn pound
  "#x (count) and x#y (take)"
  ([x] (kcount x))
  ([x y] (ktake x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $ operator
(declare stringify)
(defn dollar
  ([x] (if (coll? x)
         (mapv dollar x)
         (vec (stringify x))))
  ([x y] (err "nyi: dyadic $")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ? operator
(defn msome [p & c]
  "like some but the predicate p takes multiple args"
  (when (not (any? (map empty? c)))
    (or (apply p (map first c))
        (recur p (map next c)))))

(defn index-of-colwise [c & r] ;; c is vector of cols, r is row to find
  "Search c, a collection of columns (i.e., equal-length vectors), for
  a row matching r.  Return the index in (each column of) c where r
  was matched or, if no match was found, the length of (each column
  of) c."
  (if-let [k (apply msome
                    (fn [i & q] (when (= r q) i))
                    (iterate inc 0)
                    c)]
    k
    (count (first c))))
(defn findv [x y]
  "Find the position(s) of y in vector x"
  (cond (not (coll? y))  (index-of x y)
        (vector? y)      (mapv #(findv x %) y)
        (dict?   y)      (make-dict (dict-key y) (findv x (dict-val y)))
        (table?  y)      (make-table (dict-key y) (findv x (dict-val y)))
        (keyed-table? y) (make-keyed-table (dict-key y) (findv x (dict-val y)))
        :else            (err "findv" x y)))
(defn find-table [x y]
  "Find the row number(s) of y in x"
  (if (and (or (dict? y) (table? y))
           (every? #(some #{%} (cols x)) (dict-key y)))
    (let [f (partial index-of-colwise (dict-val x))
          v (dict-val y)]
      (if (dict? y) (apply f v) (apply mapv f v)))
    (err "mismatch: ?" x y)))
(declare index)
(defn krand [x y]
  "If y is a vector, return x random elements from y.  If y is an
  integer, return x random integers from 0 (inclusive) to
  y (exclusive).  If y is floating point, return x random floating
  point numbers from 0 (inclusive) to y (exclusive)."
  ;; TODO: x < 0 => take without replacement
  (if (vector? y)
    (index y (vec (repeatedly x #(rand-int (count y)))))
    (vec (if (float? y)
           (repeatedly x #(rand y))
           (repeatedly x #(rand-int y))))))
(defn ques
  "?x (distinct) and x?y (find or rand depending on the arguments)"
  ([x] (vec (distinct x)))
  ([x y] (cond (vector? x)      (findv x y)
               (dict? x)        (index (dict-key x) (ques (dict-val x) y))
               (table? x)       (find-table x y)
               (keyed-table? x) (index (dict-key x) (ques (dict-val x) y))
               :else            (krand x y)))
  ([x y z] nil)) ; TODO: vector cond

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; _ operator
(defn deltas [x]
  "0-':x"
  (vec (cons (first x) (map - (next x) (drop-last x)))))
(defn cut [x y]
  "y x+!'1_0-':x,#y"
  (let [i (mapv range (next (deltas (conj x (kcount y)))))]
    (index y (mapv (fn [p q] (mapv #(+ p %) q)) x i))))
(defn drop-from-stream [n x]
  (let [i       (atom 0)
        on-next (fn [ss]
                  (let [j (swap! i #(+ 1 (min n %)))]
                    (when (< n j) ss)))
        s       (make-stream (:sources x) on-next)]
    ((:sub x) s)
    s))
(defn drop-last-from-stream [n x]
  (let [w       (atom []) ; window
        on-next (fn [ss]
                  (let [ww (swap! w #(if (<= (count %) n)
                                       (conj % ss)
                                       (conj (vec (drop 1 %)) ss)))]
                    (when (< n (count ww)) (first ww))))
        s       (make-stream (:sources x) on-next)]
    ((:sub x) s)
    s))
(defn kdrop [x y]
  "Remove the first x elements from y (negative x => drop from the back)"
  (let [o (if (<= 0 x) (partial drop x) (partial drop-last (- x)))]
    (cond (vector? y)      (vec (o y))
          (dict? y)        (apply make-dict (map o [(dict-key y) (dict-val y)]))
          (table? y)       (make-table (dict-key y) (mapv o (dict-val y)))
          (keyed-table? y) (make-keyed-table (kdrop x (dict-key y))
                                             (kdrop x (dict-val y)))
          (stream? y)      (if (<= 0 x)
                             (drop-from-stream x y)
                             (drop-last-from-stream (- x) y))
          :else            (err "nyi: _ (drop) on" x y))))
(defn kremove [x y]
  "Remove the yth element from x"
  (cond (vector? x) (removev x y)
        (dict? x)   (apply make-dict
                           (map #(removev % (findv (dict-key x) y))
                                [(dict-key x) (dict-val x)]))
        (table? x)  (if (keyword? y)
                      (t-from-d (kremove (d-from-t x) y))
                      (make-table (dict-key x)
                                  (mapv #(removev % y) (dict-val x))))
        :else (err "nyi: _ (remove) for" x y)))
(defn under
  "_x (floor) and x _y (drop or remove depending on the arguments)"
  ([x] (long (Math/floor x))) ;; floor
  ([x y] (if (coll? y)
           (if (vector? x) (cut x y) (kdrop x y))
           (kremove x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adverbs
;; TODO: How should adverbs work with streams?
(declare make-func-stream)
(defn each [f] ; TODO: atomize when f is dyadic? e.g., 3+'1 2 3
  "Create a function from f that loops over all arguments (like
  clojure's map).  The valence/arity of the created function matches
  the arity of f."
  (fn [e & x]
    (let [g (lambda-callable e f)]
      (if (some stream? x)
        (if (not= 1 (count x))
          (err "nyi: multi-arg ' with streams")
          (make-func-stream e f x))
        (if-not (apply = (mapv count x))
          (err "length: collections of unequal length in '")
          (apply (partial mapv (fn [& a] (apply g a))) x))))))
(defn each-left [f]
  "Create a dyadic function from f (which must be dyadic) that loops
  over its lhs argument"
  (when-not (some #{2} (:rank f))
    (err "rank: \\: can only be applied to dyadic functions"))
  (fn [e x y]
    (let [g #((lambda-callable e f) % y)] 
      (if (stream? x)
        (make-func-stream e {:f g} [x])
        (mapv g x)))))
(defn stream-delta [f]
  (fn [p q]
    (if (nil? p)
      q
      (derive-snapshot q (apply f (map snapshot-value [q p]))))))
(defn each-prior [f]
  "Create a dyadic function from f (which must be dyadic) that loops
  over a list passing each element and its prior, e.g., deltas is 0-':x"
  (when-not (some #{2} (:rank f))
    (err "rank: /: can only be applied to dyadic functions"))
  (fn [e & x]
    (let [g (lambda-callable e f)]
      (if (= 1 (count x))
        (let [j (first x)]
          (if-not (stream? j)
            (vec (cons (first j) (map g (next j) (drop-last j))))
            (let [p       (atom nil)
                  on-next (fn [ss]
                            (let [d ((stream-delta g) @p ss)]
                              (reset! p ss)
                              d))
                  s       (make-stream (:sources j) on-next)]
              ((:sub j) s)
              s)))
        (let [[i j] x]
          (if-not (stream? j)
            (vec (map g j (cons i (drop-last j))))
            (let [p       (atom (if (snapshot? i) i (make-snapshot 0 i)))
                  on-next (fn [ss]
                            (let [d ((stream-delta g) @p ss)]
                              (reset! p ss)
                              d))
                  s       (make-stream (:sources j) on-next)]
              ((:sub j) s)
              s)))))))
(defn each-right [f]
  "Create a dyadic function from f (which must be dyadic) that loops
  over its rhs argument"
  (when-not (some #{2} (:rank f))
    (err "rank: /: can only be applied to dyadic functions"))
  (fn [e x y]
    (let [g #((lambda-callable e f) x %)]
      (if (stream? y)
        (make-func-stream e {:f g} [y])
        (mapv g y)))))
(defn stream-reducer [f]
  (fn [p q]
    (if (nil? p)
      q
      (derive-snapshot q (apply f (map snapshot-value [p q]))))))
(defn over [f]
  "Create a dyadic function from f (which must be dyadic) that
  performs a reduce (aka fold left) over its rhs argument"
  (when-not (some #{2} (:rank f))
    (err "rank: / can only be applied to dyadic functions"))
  (fn [e & x]
    (let [g (lambda-callable e f)]
      (if (= 1 (count x))
        (let [j (first x)]
          (if-not (stream? j)
            (if (:identity f)
              (reduce g (:identity f) j)
              (reduce g j))
            (let [a       (atom (:identity f)) ;; accumulator
                  on-done (fn []
                            (assoc (make-snapshot-final @a) :extract true))
                  on-next (fn [ss] (swap! a (stream-reducer g) ss) nil)
                  s       (make-stream (:sources j) on-next on-done)]
              ((:sub j) s)
              s)))
        (let [[i j] x]
          (if-not (stream? j)
            (reduce g i j)
            (let [a       (atom (if (snapshot? i) i (make-snapshot 0 i)))
                  on-done (fn []
                            (assoc (make-snapshot-final @a) :extract true))
                  on-next (fn [ss] (swap! a (stream-reducer g) ss) nil)
                  s       (make-stream (:sources j) on-next on-done)]
              ((:sub j) s)
              s)))))))
(defn scan [f]
  "Create a dyadic function from f (which must be dyadic) that
  performs reductions (a fold left but returning all intermediate
  results) over its rhs argument"
  (when-not (some #{2} (:rank f))
    (err "rank: \\ can only be applied to dyadic functions"))
  (fn [e & x]
    (let [g (lambda-callable e f)]
      (if (= 1 (count x))
        (let [j (first x)]
          (if-not (stream? j)
            (if (:identity f)
              (vec (drop 1 (reductions g (:identity f) j)))
              (vec (reductions g j)))
            (let [a       (atom (:identity f))
                  on-next #(swap! a (stream-reducer g) %)
                  s       (make-stream (:sources j) on-next)]
              ((:sub j) s)
              s)))
        (let [[i j] x]
          (if-not (stream? j)
            (vec (drop 1 (reductions g i j)))
            (let [a       (atom (if (snapshot? i) i (make-snapshot 0 i)))
                  on-next #(swap! a (stream-reducer g) %)
                  s       (make-stream (:sources j) on-next)]
              ((:sub j) s)
              s)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ operator
(declare invoke)
(defn at-xform-monadic
  "x is a vector.  Replace the elements of x at positions i with the
  results of applying f to those elements"
  [e x i f]
  ;; TODO: preserve env modifications by passing them back?
  ;; Is that necessary?  All of @'s args have been eval'd already.
  (let [h (fn self [r j]
            (if (coll? j)
              (reduce self r j)
              (let [[_ rr] (invoke e f [(r j)])]
                (assoc! r j rr))))]
    (persistent! (h (transient x) i))))
(defn at-xform-update-table [t i c] ;; table index content
  (if (keyword? i)
    (assoc t :v (assoc (:v t) (findv (:k t) i) c))
    (let [a (if (dict? c) c (make-dict (cols t)
                                       (vec (repeat (count (cols t)) c))))]
      (reduce (fn [t [k v]]
                (let [p (findv (:k t) k)]
                  (assoc t :v (assoc (:v t) p (assoc ((:v t) p) i v)))))
              t
              (map vector (dict-key a) (dict-val a))))))
(defn at-xform-monadic-table [e x i f]
  (let [h (fn self [r j]
            (if (coll? j)
              (reduce self r j)
              (let [[_ rr] (invoke e f [(index r j)])]
                (at-xform-update-table r j rr))))]
    (h x i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn at-xform-dyadic [e x i f y]
  (let [h (fn self [r [j b]]
            (if (coll? j)
              (reduce self r (if (coll? b)
                               (map vector j b)
                               (map #(vector % b) j)))
              (let [[_ rr] (invoke e f [(r j) b])]
                (assoc! r j rr))))]
    (persistent! (h (transient x) [i y]))))
(defn at-xform-dyadic-table [e x i f y]
  (let [h (fn self [r [j b]]
            (if (coll? j)
              (reduce self r (if (coll? b)
                               (map vector j b)
                               (map #(vector % b) j)))
              (let [[_ rr] (invoke e f [(index r j) b])]
                (at-xform-update-table r j rr))))]
    (h x [i y])))
(defn at-xform
  "Replace the elements of x at positions i with the results of
  applying f to those elements"
  ([e x i f]
   ;; check that f is monadic here?
   (cond (vector? x)       (cond (not (coll? i))   (at-xform-monadic e x i f)
                                 (empty? i)        x
                                 (vector? i)       (at-xform-monadic e x i f)
                                 :else             (err "nyi at-xform" x i))
         (dict? x)         (make-dict (dict-key x)
                                      (at-xform e
                                                (dict-val x)
                                                (findv (dict-key x) i) f))
         (table? x)        (at-xform-monadic-table e x i f)
         (keyed-table? x)  (err "nyi")
         :else             (err "internal error at-xform" x i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; "Replace the elements of x at positions i with the results of
  ;; applying f (which must be dyadic) to those elements and (the
  ;; corresponding elements of) y"
  ([e x i f y] ;; y conforms to i, not x
   ;; check that f is dyadic here?
   (cond (vector? x)       (cond (not (coll? i))   (at-xform-dyadic e x i f y)
                                 (empty? i)        x
                                 (vector? i)       (at-xform-dyadic e x i f y)
                                 :else             (err "nyi at-xform" x i))
         (dict? x)         (make-dict (dict-key x)
                                      (at-xform e
                                                (dict-val x)
                                                (findv (dict-key x) i) f y))
         (table? x)        (at-xform-dyadic-table e x i f y)
         (keyed-table? x)  (err "nyi")
         :else             (err "internal error at-xform" x i))))
(defn at
  "type"
  ([_ x] (err "nyi: monadic @"))
  ;; (condp #(= (type %1) %2) x ; type
  ;;          java.lang.Boolean -1
  ;;          java.lang.Long -7
  ;;          java.lang.Double -9
  ;;          java.lang.Character -10
  ;;          java.lang.String 10
  ;;          clojure.lang.Keyword -11
  ;;          0))
;;  "index at top level"
  ([e x y] (last (invoke e x [y])))
;;  "selectively transform at top level"
  ([e x y z] (at-xform e x y z))
;;  "selectively transform at top level with rhs for xform function"
  ([e x y z a] (at-xform e x y z a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; . operator
(defn massoc
  "multi-assoc: replace the elements of x at positions i with the
  respective elements of c"
  [x i c]
  (persistent! (reduce (fn [a [b c]] (assoc! a b c))
                       (transient x)
                       (mapv vector i c))))
(defn dot-xform-monadic [e x i f]
  "Implements .[x;i;f] where x is a vector"
  (if (empty? i)
    (last (invoke e f [x]))
    (let [j (first i)]
      (if (not (coll? j))
        (assoc x j (dot-xform-monadic e (x j) (next i) f))
        (massoc x j
                (mapv #(dot-xform-monadic e %1 (next i) f)
                      (index x j)))))))
(defn dot-xform-dyadic [e x i f y]
  "Implements .[x;i;f;y] where x is a vector"
  (if (empty? i)
    (last (invoke e f [x y]))
    (let [j (first i)]
      (if (not (coll? j))
        (assoc x j (dot-xform-dyadic e (x j) (next i) f y))
        (massoc x j
                (mapv #(dot-xform-dyadic e %1 (next i) f %2)
                      (index x j)
                      y))))))
(defn dot-xform
  "Implements .[x;i;f] where x is a vector or dict"
  ([e x i f]
   (cond (vector? x)       (cond (not (coll? i))
                                 (err "index for . must be a container")
                                 (empty? i)        x
                                 (vector? i)       (dot-xform-monadic e x i f)
                                 :else             (err "nyi dot-xform" x i))
         (dict? x)         (make-dict (dict-key x)
                                      (dot-xform e
                                                (dict-val x)
                                                (vec (cons (findv (dict-key x)
                                                                  (first i))
                                                           (next i)))
                                                f))
         (table? x)        (err "nyi")
         (keyed-table? x)  (err "nyi")
         :else             (err "internal error dot-xform" x i)))
;;  "Implements .[x;i;f;y] where x is a vector or dict"
  ([e x i f y]
   (cond (vector? x)       (cond (not (coll? i))
                                 (err "index for . must be a container")
                                 (empty? i)        x
                                 (vector? i)       (dot-xform-dyadic e x i f y)
                                 :else             (err "nyi dot-xform" x i))
         (dict? x)         (make-dict (dict-key x)
                                      (dot-xform e
                                                (dict-val x)
                                                (vec (cons (findv (dict-key x)
                                                                  (first i))
                                                           (next i)))
                                                f
                                                y))
         (table? x)        (err "nyi")
         (keyed-table? x)  (err "nyi")
         :else             (err "internal error dot-xform" x i))))
(defn dot
  ". x        (value)
   x . i      (index at depth)
   .[x;i;f]   (selective transformation)
   .[x;i;f;y] (selective transformation w/rhs)"
  ([e x] (cond (vector? x)      x
               (dict? x)        (dict-val x)
               (keyed-table? x) (dict-val x)
               (keyword? x)     (if-let [u (e x)] u (err x "not found"))
               :else            (err "nyi: . on" x)))
  ([e x y] (last (invoke e x y)))
  ([e x y z] (dot-xform e x y z))
  ([e x y z a] (dot-xform e x y z a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing
(defn parse-double "parse the string x as a double" [x]
  (if (coll? x) (mapv parse-double x)
                #?(:clj (Double/parseDouble x)
                   :cljs (js/parseFloat x))))
(defn parse-long "parse the string x as a long" [x]
  (if (coll? x) (mapv parse-long x)
                #?(:clj (Long/parseLong x)
                   :cljs (js/parseInt x))))
(defn parse-symbol "parse the string x as a symbol (clojure keyword)" [x]
  (if (coll? x) (mapv keyword x) (keyword x)))
(defn parse-data
  "parse y according to character x:
  J long
  F double
  S symbol
  * don't parse"
  [x y]
  (cond (= \J x) (parse-long y)
        (= \F x) (parse-double y)
        (= \S x) (parse-symbol y)
        (= \* x) y
        :else (err "nyi: parse type" (str x))))
(defn rcsv [c f] ;; col types and file name
  "Read csv file f with column types specified by c per the parse-data function"
  #?(:clj (let [d (csv/parse-csv (slurp (string f)))] ;; assumes header line
            (mapv parse-data (string c) (flip (vec d))))))
(defn rcsvh [c f] ;; col types and file name
  "Read csv file f, whose first line is a header, with column types
  specified by c per the parse-data function"
 #?(:clj (let [[h & d] (csv/parse-csv (slurp (string f)))] ;; assumes header line
           (make-table (mapv keyword h) (mapv parse-data (string c) (flip (vec d)))))))
(defn wcsv [f t] ;; output file and table
  "Write to csv file f the content of table t"
  #?(:clj (let [make-string (fn [x] (mapv #(if (keyword? %) (name %) (str %)) x))
                d (cons (mapv name (cols t)) (map make-string (flip (dict-val t))))]
            (with-open [w (io/writer (string f))]
              (.write w (csv/write-csv d))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ops {
          (keyword "~") {:f tilde  :text "~" :rank [1 2]}
          :! {:f bang :text "!" :rank [1 2] :snapshot-aware [1]}
          :at {:f at :pass-global-env true :text "@" :rank [1 2 3 4]}
          :dot {:f dot :pass-global-env true :text "." :rank [1 2 3 4]
                :snapshot-aware [2]}
          :+ {:f plus :text "+" :rank [1 2]}
          :- {:f minus :text "-" :rank [1 2]}
          :* {:f times :text "*" :rank [1 2] :snapshot-aware [1]}
          :% {:f fdiv :text "%" :rank [1 2]}
          :# {:f pound :text "#" :rank [1 2] :stream-aware [2]}
          :$ {:f dollar :text "$" :rank [1 2]}
          (keyword ",") {:f join :identity []
                         :text "," :rank [1 2] :stream-aware [2]}
          :& {:f amp :text "&" :rank [1 2] :snapshot-aware [1]}
          :| {:f pipe :text "|" :rank [1 2]}
          :_ {:f under :text "_" :rank [1 2] :stream-aware [2]}
          := {:f eq :text "=" :rank [1 2]}
          :<> {:f neq :text "<>" :rank [2]}
          :< {:f less :text "<" :rank [1 2]}
          :> {:f greater :text ">" :rank [1 2]}
          :<= {:f le :text "<=" :rank [1 2]
               :stream-aware [1]} ;; monadic form waits for stream
          :>= {:f ge :text ">=" :rank [1 2]} ; monadic form creates stream
          :? {:f ques :text "?" :rank [1 2 3]}
          })
(def adverbs {:' each
              (keyword "\\:") each-left
              (keyword "':")  each-prior
              (keyword "/:")  each-right
              (keyword "/")   over
              (keyword "\\")  scan})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn map-from-tuples [x]
  "Like into but handles tuples not just pairs, because sometimes
  parse gives you back tuples."
  ;; vpn means vector-preserving (ast) nodes
  (let [vpn [:actuals :aggs :by :dcols :exprs :formals :where]]
    (reduce (fn [m t]
              (let [[p & q] t]
                (assoc m p (if (or (not= 1 (count q))
                                   (some #{p} vpn))
                             q
                             (first q)))))
            {}
            x)))
(defn strmax [x y]
  "The maximum of two strings"
  (cond (nil? x)            y
        (nil? y)            x
        (< 0 (compare x y)) x
        :else               y))
(defn implicit-args [x] ; exprs
  "Figure out the implicit arguments of x, which is a series of
  expressions that make up a lambda"
  (let [a ["x" "y" "z"]
        mia (fn self [x] ; max implicit arg
              (cond (or (not (coll? x)) (empty? x) (= :lambda (first x))) nil
                    (= :id (first x)) (if (some #{(second x)} a)
                                        (second x)
                                        nil)
                    :else (reduce strmax (mapv self x))))
        n (mod (+ 1 (index-of a (mia x))) (+ 1 (count a)))]
    (vec (take n a))))
(defn kdestructure [a x]
  (if (< (count x) (count a))
    (err "destructuring mismatch" a x)
    (reduce (fn [e [p q]]
              (let [t (first p)]
                (cond (= :hole t) e
                      (= :id t)   (assoc e (keyword (second p)) q)
                      (= :did t)  (let [id (keyword (second p))]
                                    (if (not= :_ id)
                                      (assoc e id q)))
                      (= :varg t) (merge e (kdestructure (next p) q))
                      (or (= :targ t) (= :targs t))
                      (merge e (kdestructure (next p) (dict-val (flip q))))
                      (or (= :darg t) (= :ktarg t))
                      (merge e
                             (kdestructure [(second p)] [(dict-key q)])
                             (kdestructure [(second (next p))] [(dict-val q)]))
                      :else (err "nyi: destrucuring" a x))))
            {}
            (zipmap a x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn args [f] ;; f is parse tree
  "Create a function that will assign f's formals"
  (def simple (fn [a] {:formals a :rank [(count a)]}))
  (if-let [a (:formals f)]
    (if (all? (mapv #(= :id (first %)) a))
      (simple (mapv #(keyword (second %)) a))
      {:formals (partial kdestructure a) :rank [(count a)]})
    (simple (mapv keyword (implicit-args (:exprs f))))))

(declare kresolve)
(declare resolve-full-expr)

;; TODO think through updating closed-over variables
;; vs locals vs globals ...
(defn feval [tu f]
  "Returns a function for evaluating function f which appears in
  translation unit tu (tu is for debugging/error messages)"
  (fn [e & x]
    (if (not (some #{(count x)} (lambda-rank f)))
      (err "rank" (lambda-text f) (vec x))
      (let [e2 (merge e
                      (lambda-env f)
                      (if (coll? (lambda-formals f))
                        (zipmap (lambda-formals f) x)
                        ((lambda-formals f) x)))] ;; formals is destructuring func
        (last (reduce (fn [[e3 _] p] (resolve-full-expr tu e3 p))
                      [e2 nil]
                      (lambda-body f)))))))
(defn index-table [t i]
  "Index table t per index i, which may be row number(s) or column name(s)"
  (cond (or (and (vector? i) (keyword? (first i)))
            (keyword? i))
        (index (d-from-t t) i)
        (or (and (vector? i) (or (empty? i)
                                 (number? (first i))))
            (number? i))
        (let [d (d-from-t t)
              r (make-dict (dict-key d) (mapv #(index % i) (dict-val d)))]
          (if (vector? i)
            (t-from-d r)
            r))
        :else (mapv (partial index-table t) i)))
(declare apply-constraints)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn index-keyed-table [t i]
  "A keyed table containing the rows from t specified by i"
  (cond (or (dict? i) (table? i)) (index-table (dict-val t)
                                               (find-table (dict-key t) i))
        ;; user supplied just the value of a dict we hope conforms
        (and (coll? i) (= (count i) (count (keycols t))))
        (index-keyed-table t (make-dict (keycols t) i))
        (and (not (coll? i)) (= 1 (count (keycols t))))
        (index-keyed-table t (make-dict (keycols t) [i]))
        :else (err "nyi: index keyed table" t i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: unify index, invoke, resolve-at, resolve-call, and resolve-juxt
(defn index [x i]
  "The elements of x specified by i"
  (cond (= :hole i)      x
        (snapshot? i)    (let [v (snapshot-value i)]
                               (if (= true v)
                                 x
                                 (index x v)))
        (table? x)       (index-table x i)
        (keyed-table? x) (index-keyed-table x i)
        (dict? i)        (make-dict (dict-key i) (index x (dict-val i)))
        (vector? i)      (mapv (partial index x) i)
        (coll? i)        (reduce index x i) ;; TODO: fix
        (vector? x)      (x i)
        (dict? x)        ((dict-val x) (index-of (dict-key x) i))
        (snapshot? x)    (derive-snapshot x (index (snapshot-value x) i))
        ;;; object? doesn't work; it just tests if the ctor is Object
        :else            #?(:clj  (err "can't index" x "with" i)
                            :cljs (aget x (name i)))))
(defn index-deep [x i]
  "The elements of x specified by i, where x is nested and the
  dimensions of i create a cross-product index over x's dimensions"
  (cond (snapshot? i) (derive-snapshot i
                                       (index-deep x
                                                   (mapv #(derive-snapshot i %)
                                                         (snapshot-value i))))
        (empty? i)    x
        :else         (let [j (first i)
                            p (index x j)]
                        (if (or (= :hole j) (and (vector? j) (< 1 (count j))))
                          (mapv #(index-deep %1 (next i)) p)
                          (index-deep p (next i))))))
(defn invoke-partial [e f a]
  "Partially apply f to a (which must have fewer elements than f's
  formals or contain holes), creating a new function that will invoke
  f when the remaining arguments are supplied"
  ;; We could introduce a new partial type, but that means lots of
  ;; places need to handle it.  Instead, we synthesize a lambda that
  ;; wraps the original function.  That means for variadics (ie
  ;; operators especially . and @) we select the arity based on the
  ;; arguments present when the partial is bound, not when the partial
  ;; is completed later.  This appears to be the behavior of k anyhow.
  (let [max-rank (apply max (lambda-rank f))
        min-rank (apply min (lambda-rank f))
        args     (cond (> (count a) max-rank) (err "rank")
                       (< (count a) min-rank)
                       ;; add holes if needed
                       (concat a (repeat (- min-rank (count a)) :hole))
                       :else a)
        formals (mapv #(keyword (str/join ["a" (str %)]))
                      (til (count (filter #{:hole} args))))
        passing (loop [x args y formals r []]
                  (cond (empty? x) r
                        (empty? y) (catv r x)
                        (= :hole (first x)) (recur (rest x)
                                                   (rest y)
                                                   (conj r [:id (name (first y))]))
                        :else (recur (next x) y (conj r [:raw (first x)]))))
        w {:formals formals
           :exprs [[:call [:target [:raw f]] (push :actuals passing)]]
           :pass-global-env true
           :rank [(count formals)]
           :snapshot-aware (if (snapshot-aware? f (count args))
                             [(count formals)]
                             [])
           :text (:text f)
           :env e}]
    [e (assoc w :f (feval "<synthesized partial>" w))]))
(defn fill-vector-holes [v a]
  (loop [x v y a r []]
    (cond (empty? x)          r
          (empty? y)          (catv r x)
          (= :hole (first x)) (recur (rest x) (rest y) (conj r (first y)))
          :else               (recur (next x) y (conj r (first x))))))
(declare make-stream)
(declare sub)
(defn invoke-with-streams [e f a]
  (let [b (map #(if (stream? %) :hole %) a)
        s (filterv stream? a)
        [e2 g] (if (= (count a) (count s))
                 [e f]
                 (invoke-partial e f b))]
    [e2 (make-func-stream e g s)]))
(defn index-from-streams [e c i]
  "Turn indexing with streams to invoking . with streams"
  (invoke-with-streams e
                       (:dot ops)
                       [c (if-not (some stream? i)
                            i
                            (make-func-stream e
                                              {:f (fn [& x] x)
                                               :rank [(count i)]}
                                              i))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn invoke-helper [e f a]
  "Apply f to a. If f is not a lambda, index f at depth using a"
  (if (not (lambda? f))
    (cond (and (vector? f) (some #{:hole} f)) ;; (;4)3 => (3;4)
          [e (fill-vector-holes f a)]
          (or (stream? f) (stream? a) (some stream? a))
          (index-from-streams e f a)
          :else [e (index-deep f a)])
    (if (and (not (some #{:hole} a))
             (some #{(count a)} (lambda-rank f)))
      (if (and (not (stream-aware? f (count a)))
               (some stream? a))
        (invoke-with-streams e f a)
        (if (and (not (snapshot-aware? f (count a)))
                 (some snapshot? a))
          ;; TODO We need to find the source!
          (err
           "nyi: invoking non-snapshot-aware function with snapshots via invoke")
              ;; [e (make-snapshot (apply (lambda-callable e f)
              ;;                          (mapv #(if (snapshot? %)
              ;;                                   (snapshot-value %)
              ;;                                   %)
              ;;                            a)))])
          [e (apply (lambda-callable e f) a)]))
      (invoke-partial e f a))))
(defn invoke [e f a]
  (let [[e r] (invoke-helper e f a)]
    (if (and (map? r) (:new-env r))
      [(:new-env r) nil]
      [e r])))
(defn is-callable "Can x be invoked" [x] (instance? clojure.lang.IFn x))
(defn can-only-be-monadic
  "Can x be invoked with 1 argument and no other number of arguments?"
  [x]
  (= [1] (lambda-rank x)))
(defn can-be-monadic "Can x be invoked with 1 argument?"
  [x] (some #{1} (lambda-rank x)))
(defn can-be-dyadic "Can x be invoked with 2 arguments?"
  [x] (some #{2} (lambda-rank x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adverbs and streams, hmm
;; first thought is that an adverb turns a stream it is iterating over
;; into a series of values, but leaves others alone.  That may mean
;; adverbs must all be stream aware.
(defn resolve-adverbed [tu e x]
  "Find the value of x, and adverbed expr"
  (let [m    (adverbs (keyword (:adverb x)))
        make (fn [o]
               (merge o {:f (m o)
                         :pass-global-env true
                         :stream-aware [1 2]
                         ;; :snapshot-aware [2] ;; inherit from o
                         :text (str (:text o) (:adverb x))}))]
    (if (contains? x :lhs) ; eval must be right to left!
      (if (contains? x :rhs)
        (let [[e2 r] (kresolve tu e  (:rhs x))
              [e3 o] (kresolve tu e2 (:verb x))
              [e4 l] (kresolve tu e3 (:lhs x))]
          (invoke e4 (make o) [l r]))
        (err "nyi: bind lhs of adverbed expr (partial)"))
      (if (contains? x :rhs)
        (let [[e2 r] (kresolve tu e  (:rhs x))
              [e3 o] (kresolve tu e2 (:verb x))]
          (invoke e3 (make o) [r]))
        (let [[e2 o] (kresolve tu e (:verb x))]
          [e2 (make o)])))))

(defn resolve-assign [tu e x]
  "Resolve the assignment specified by x"
  (let [[e2 r] (kresolve tu e (:rhs x))]
    [(merge e2 (kdestructure [(:lhs x)] [r])) r]))

(defn resolve-at [tu e x]
  "Resolve the @ expr specified by x"
  (if (:rhs x)
    (let [[e2 rhs] (kresolve tu e (:rhs x))]
      (if (:lhs x)
        (let [[e3 lhs] (kresolve tu e2 (:lhs x))]
          (invoke e3 lhs [rhs]))
        (at e2 rhs)))
    (if (:lhs x)
      (err "nyi: partially bound @ from lhs")
      [e (ops :at)])))

(defn resolve-dot [tu e x]
  "Resolve the . expr specified by x"
  (if (:rhs x)
    (let [[e2 rhs] (kresolve tu e (:rhs x))]
      (if (:lhs x)
        (let [[e3 lhs] (kresolve tu e2 (:lhs x))]
          (invoke e3 lhs rhs))
        [e2 (dot e2 rhs)]))
    (if (:lhs x)
      (err "nyi: partially bound . from lhs")
      [e (ops :dot)])))

(defn resolve-dotn [tu e x]
  (let [o (kresolve tu e (first x))]
    (null! "hmm")
    (if (null! (dict? x))
      (err "nyi: dot notation for dicts")
      (fn [& a]
        (null! "interop goes here")
        #?(:clj (let [g (eval
                          (read-string
                            (str "(fn [p] (." (second (second x)) " p))")))]
                  (g o)))))))

(defn resolve-call [tu e x]
  "Resolve the f[...] expr specified by x"
  (let [[e4 r] (reduce (fn [[e2 r] a]
                         (let [[e3 rr] (kresolve tu e2 a)]
                           [e3 (cons rr r)]))
                       [e []]
                       (reverse (:actuals x))) ;; right-to-left!
        [_ f] (kresolve tu e4 (:target x))]
      (invoke e4 f r)))

(defn resolve-dyop [tu e x]
  "Resolve the pOq expr specified by x"
  (let [o        (ops (keyword (:op x)))
        [e2 rhs] (kresolve tu e (:rhs x))
        [e3 lhs] (kresolve tu e2 (:lhs x))]
    (invoke e3 o [lhs rhs])))
(defn force-dyadic [x]
  "Create a dyadic-only function from x which may be variadic"
  ;; assert (can-be-dyadic x)
  (assoc x :rank [2]))
(defn resolve-juxt [tu e x]
  "Resolve two expressions next to each other without punctuation"
  (let [[e2 rhs] (kresolve tu e (:rhs x))
        [e3 lhs] (kresolve tu e2 (:lhs x))]
    (cond
      (:second rhs)       (invoke e3 rhs [lhs (:second rhs)])  ; 1 in 1 2 3
      (and (can-be-dyadic rhs)
           (not (can-be-dyadic lhs))) (invoke e3 (force-dyadic rhs) [lhs]) ; 1+
      (:second lhs)       (invoke e3 lhs [(:second lhs) rhs])  ; (in 1 2 3)1 2
      (can-be-dyadic lhs) [e3 (merge lhs {:second rhs})]       ; {x+y}3
      :else               (invoke e3 lhs [rhs]))))             ; {x}3

(defn resolve-lambda [tu text e x]
  "Create a lambda from an AST"
  (let [a (args x)
        w (merge x   ;; TODO: write lambda ctor
                 a
                 {:env e
                  :pass-global-env true
                  :snapshot-aware (:rank a)
                  :stream-aware (:rank a)
                  :text text})]
    [e (assoc w :f (feval tu w))]))
(defn vec-with-streams [e v]
  (let [f (fn [& x] ;; fill stream elements of v using x
            (loop [i v j x r []]
              (if (empty? i)
                r
                (if (stream? (first i))
                  (recur (next i) (next j) (conj r (first j)))
                  (recur (next i) j (conj r (first i)))))))]
    (make-func-stream e {:f f} (filterv stream? v))))
(defn resolve-vec [tu e x]
  "Resolve the (...;...) expr specified by x"
  (let [[e2 r] (reduce (fn [[e r] i]
                         (let [[ne nr] (kresolve tu e i)]
                           [ne (cons nr r)]))
                       [e ()]
                       (reverse x))]  ;; eval right-to-left!
    ;; Cannot turn vector with holes into lambda here, because .
    ;; uses vectors with holes for index elision
    ;; We do need to deal with streams, tho
    ;; We can handle streams and holes at the same time,
    ;; but I don't know if that makes any sense.
    [e2 (vec r)]))
    ;; (if (some stream? r)
    ;;   [e2 (vec-with-streams e2 r)]
    ;;   [e2 (vec r)])))

(defn resolve-monop [tu e x]
  "Resolve the Op specified by x"
  (let [o      (ops (keyword (:op x)))
        [e2 a] (kresolve tu e (:rhs x))]
    (invoke e2 o [a])))

(defn sub-table [t i]
  "Replace references to column names in t with an @ expr that refers
  to only those elements of t's columns specified by i"
  (if i
    {:id (fn [x] (if (some #{(keyword x)} (cols t))
                   [:at [:lhs [:id x]] [:rhs [:raw i]]]
                   [:id x]))}
    {}))

(defn apply-constraints [tu env t w]
  "The row numbers in t that satisfy w"
  (if (not w)
    [env (til-count t)]
    (reduce (fn [[e i] c]
              (let [[e2 j] (apply where
                                  (kresolve tu
                                            e
                                            (insta/transform (sub-table t i)
                                                             (first c))))]
                [e2 (index i j)]))
            (apply where (kresolve tu env (first w)))
            (next w))))

(defn guess-col [x]
  "When an aggregation is not assigned a name, use the name of the
  last variable used in the aggregation expression.  If no variable
  name appears in the aggregate expression, use x"
  (cond (empty? x)        :x
        (= :id (first x)) (keyword (second x))
        :else (last (map guess-col (next x)))))

(defn compute-aggs [tu e t i aggs]
  "Compute the aggregations defined by (expr trees) aggs for the rows
  of t indexed by i, all within environment e"
  (reduce (fn [[e r] a]
            (let [n      (if (= :assign (first a))
                           (keyword (second (second a)))
                           (guess-col a))
                  [e2 d] (kresolve tu e (insta/transform (sub-table t i) a))]
              [e2 (add-to-dict r n d)]))
          [e (make-dict)]
          aggs))

(defn resolve-delcols [tu e x]
  "Resolve the delete (columns) expr specified by x"
  (let [[_ t] (kresolve tu e (:from x))
        t2     (t-from-d (reduce remove-from-dict
                                 (d-from-t (unkey-table t))
                                 (map #(keyword (second %1)) (:dcols x))))]
    [e (if (and (keyed-table? t) (all? (in (keycols t) (cols t2))))
         (key-table-by-colnames (keycols t) t2)
         t2)]))
(defn resolve-delrows [tu e x]
  "Resolve the delete (rows) expr specified by x"
  (let [[e2 t] (kresolve tu e (:from x))
        ut     (unkey-table t)
        e3     (merge e2 (zipmap (dict-key ut) (dict-val ut)))
        [_ i] (apply-constraints tu e3 ut (:where x))
        u      (index-table ut (except (til-count ut) i))]
    [e (if (not (keyed-table? t)) u (key-table-by-colnames (keycols t) u))]))
(defn resolve-select [tu e x]
  "Resolve the select expr specified by x"
  (let [[e2 t] (kresolve tu e (:from x))
        ut     (unkey-table t)
        e3     (merge e2 (zipmap (dict-key ut) (dict-val ut)))
        [e4 i] (apply-constraints tu e3 ut (:where x))]
    (if-let [b (:by x)]
      (let [[e5 g] (compute-aggs tu e4 ut i b)
            j      (index i (group (flip (dict-val g))))
            k      (make-table (dict-key g) (flip (dict-key j)))]
        (if-let [a (:aggs x)]
          (let [u (mapv #(last (compute-aggs tu e5 ut % a)) (dict-val j))
                v (make-table (dict-key (first u))
                              (flip (mapv dict-val u)))]
            [e (make-keyed-table k v)])
          (let [c (except (cols t) (dict-key g)) ;; cols not in the by clause
                v (make-table c (mapv #(index % (dict-val j)) (index ut c)))]
            [e (make-keyed-table k v)])))
      (if-let [a (:aggs x)]
        (let [[_ r] (compute-aggs tu e4 ut i a)
              n      (apply max (mapv kcount (dict-val r)))
              v      (mapv #(if (= 1 (kcount %)) (ktake n %) %) (dict-val r))]
          [e (make-table (dict-key r) v)])
        [e (if (keyed-table? t)
              (key-table-by-colnames (keycols t) (index ut i))
              (index t i))]))))

(defn resolve-table-helper [tu e b]
  (let [c (map (comp map-from-tuples next)
               (if (= :col (first b)) [b] b))
        [e r] (reduce (fn [[e r] a]
                        (let [[ne rr] (kresolve tu e a)]
                          [ne (cons rr r)]))
                      [e ()]
                      (reverse (map :rhs c)))]
    (if (apply = (map count r))
      [e (make-table (mapv #(keyword (:id %)) c) (vec r))]
      (err "length"))))

(defn resolve-table [tu e x]
  "Resolve the ([]...) expr specified by x"
  (let [[e2 t] (resolve-table-helper tu e (:cols x))]
    (if-let [kc (:keycols x)]
      (let [[e3 k] (resolve-table-helper tu e2 kc)]
        [e3 (make-keyed-table k t)])
      [e2 t])))

;; TODO: just started
(defn resolve-update [tu e x]
  "Resolve the update expr specified by x"
  (let [[e2 t] (kresolve tu e (:from x))
        ut     (unkey-table t)
        [e3 i] (apply-constraints tu
                                  (merge e2 (zipmap (dict-key ut)
                                                    (dict-val ut)))
                                  ut
                                  (:where x))]
    (if-let [b (:by x)]
      (let [[e4 g] (compute-aggs tu e3 ut i b)
            j      (index i (group (flip (dict-val g))))
            k      (make-table (dict-key g) (flip (dict-key j)))]
        (if-let [a (:aggs x)]
          (let [u (mapv #(last (compute-aggs tu e4 ut % a)) (dict-val j))
                v (make-table (dict-key (first u))
                              (flip (mapv dict-val u)))]
            [e4 (make-keyed-table k v)])
          (let [c (except (cols t) (dict-key g)) ;; cols not in the by clause
                v (make-table c (mapv #(index % (dict-val j)) (index ut c)))]
            [e4 (make-keyed-table k v)])))
      (if-let [a (:aggs x)]
        (let [[e5 r] (compute-aggs tu e3 ut i a)
              n      (apply max (mapv kcount (dict-val r)))
              v      (mapv #(if (= 1 (kcount %)) (ktake n %) %) (dict-val r))]
          [e5 (make-table (dict-key r) v)])
        [e3 t]))))

(defn lj [x y]
  (cond (not (keyed-table? y)) (err "rhs of lj must be a keyed table")
        (not (every? #(some #{%} (cols x)) (keycols y))) (err "mismatch: lj" x y)
        ;; for every row in x, find the index of the first matching row in y
        :else
        (let [j (apply mapv
                       (partial index-of-colwise (dict-val (dict-key y)))
                       (index (dict-val x) (findv (cols x) (keycols y))))
              r (index-table (dict-val y) j)]
          (make-table (catv (dict-key x) (dict-key r))
                      (catv (dict-val x) (dict-val r))))))

(defn sort-table [x y o] ; cols table op
  (let [c (if (coll? x) x [x])
        t (unkey-table y)]
    (if (not (every? #(some #{%} (cols t)) c))
      (err "mismatch: xasc" c y)
      (let [r (index-table t (o (apply mapv vector (index-table t c))))]
        (if (keyed-table? y)
          (key-table (keycols y) r)
          r)))))
(defn xasc [x y] (sort-table x y less))
(defn xdesc [x y] (sort-table x y greater))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kresolve [tu e x] ; translation unit, env, parse tree
  "Resolve the expr specified by x in the context of environment e"
  (let [t (if (coll? x) (first x) x)
        v (if (coll? x)
            (cond (= :vec t)           (next x)
                  (= :expr t)          (second x)
                  (= :parexpr t)       (second x)
                  (= :raw t)           (second x)
                  (= :dotn t)          (next x)
                  (vector? (second x)) (map-from-tuples (next x))
                  (= 2 (count x))      (second x)
                  :else                (next x))
            nil)]
    (cond (= t :adverbed) (resolve-adverbed tu e v)
          (= t :arg)      (kresolve tu e v)
          (= t :assign  ) (resolve-assign tu e v)
          (= t :at      ) (resolve-at tu e v)
          (= t :bool    ) [e (= "1" v)]
          (= t :bools   ) [e (mapv #(= \1 %) v)]
          (= t :call    ) (resolve-call tu e v)
          (= t :char    ) [e (char (first v))]
          (= t :chars   ) [e (vec v)]
          (= t :delcols ) (resolve-delcols tu e v)
          (= t :delrows ) (resolve-delrows tu e v)
          (= t :dot     ) (resolve-dot tu e v)
          (= t :dotn    ) (resolve-dotn tu e v)
          (= t :dyop    ) (resolve-dyop tu e v)
          (= t :empty   ) [e []]
          (= t :expr    ) (resolve-full-expr tu e v)
          (= t :float   ) [e (parse-double v)]
          (= t :floats  ) [e (parse-double (str/split v #"[ \n\r\t]+"))]
          (= t :hole    ) [e :hole] ;; nil ?
          (= t :id      ) (let [u ((keyword v) e)]
                            (if-not (nil? u)
                              [e u]
                              (err v "not found in scope")))
          (= t :juxt    ) (resolve-juxt tu e v)
          (= t :lambda  ) (resolve-lambda tu
                                          (apply (partial subs tu) (instav/span x))
                                          e
                                          v)
          (= t :long    ) [e (parse-long v)]
          (= t :longs   ) [e (parse-long (str/split v #"[ \n\r\t]+"))]
          (= t :monop   ) (resolve-monop tu e v)
          (= t :op      ) [e (ops (keyword v))]
          (= t :parexpr ) (resolve-full-expr tu e v)
          (= t :raw     ) [e v]
          (= t :select  ) (resolve-select tu e v)
          (= t :symbol  ) [e (keyword v)]
          (= t :symbols ) [e (mapv keyword (next (str/split v #"`")))]
          (= t :table   ) (resolve-table tu e v)
          (= t :update  ) (resolve-update tu e v)
          (= t :vec     ) (resolve-vec tu e v)
          :else           [e x])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resolve-full-expr [tu e x]
  "Resolve x in the context of e.  If x resolves to an ambivalent
  expression and one argument is present, execute it"
  (let [[e2 r] (kresolve tu e x)]
    (if (and (= :juxt (first x)) (:second r) (can-be-monadic r))
      (invoke e2 r [(:second r)])
      [e2 r])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repl
(def viewport {:rows 25 :cols 80})

(defn substr "string begin end" [s b e] (subs s b (min e (count s))))

(declare stringify)
(defn stringify-boolean [x]
  (if x "1b" "0b"))
(defn stringify-dict [x]
  (let [ks (mapv stringify (dict-key x))
        kw (apply max (map count ks))
        f  (str "%" kw "s| %s")]
    (str/join "\n" (map #?(:clj #(format f %1 %2)
                           :cljs #(gstring/format f %1 %2))
                        ks
                        (mapv stringify (dict-val x))))))
(defn table-as-strings [x]
  (let [h (mapv name (cols x)) ; col header
        n (min (:rows viewport) (count (first (dict-val x)))) ; TODO: fix
        c (mapv #(vec (take n %)) (dict-val x)) ; col data
        s (mapv #(mapv stringify %) c)
        w (mapv #(apply max (cons (count %1) (map count %2))) h s)
        f (mapv #(str "%-" % "s") w)
        fmt-row (fn [& r] (str/join " " #?(:clj (map format f r)
                                           :cljs (map gstring/format f r))))] ; TODO: gstring format differs
    (vec (cons (apply fmt-row h)
               (cons (str/join (replicate (+ -1 (count w) (reduce + w)) "-"))
                     (apply (partial map fmt-row) s))))))
(defn stringify-keyed-table [x]
  (let [k (table-as-strings (dict-key x))
        v (table-as-strings (dict-val x))]
    (str/join "\n" (map (fn [& r] (str/join "| " r)) k v))))
(defn stringify-lambda [x] (lambda-text x))
(defn stringify-table [x]
  (str/join "\n" (table-as-strings x)))
(defn stringify-vector [x]
  (cond (= 0 (count x))   "()"
        (every? bool? x)  (str (str/join (mapv #(if % \1 \0) x)) \b)
        (kstring? x)      (str/join x)
        (= 1 (count x))   (str "," (stringify (first x)))
        (coll? (first x)) (str/join "\n" (mapv stringify x))
;;        (coll? (first x)) (str "(" (str/join ";" (mapv stringify x)) ")")
        :else               (str/join " " (mapv stringify x))))
(defn stringify [x]
  (cond (vector? x)      (stringify-vector x)
        (dict? x)        (stringify-dict x)
        (table? x)       (stringify-table x)
        (keyed-table? x) (stringify-keyed-table x)
        (lambda? x)      (stringify-lambda x)
        (snapshot? x)    (stringify (snapshot-value x))
        (bool? x)        (stringify-boolean x)
        :else            (str x)))
  
(defn show-dict [x]
  (let [n  (min (:rows viewport) (count (dict-key x)))
        w  (:cols viewport)
        ks (mapv stringify (take n (dict-key x)))
        kw (apply max (map count ks))
        f  (str "%" kw "s| %s")]
    (doseq [[k v] (mapv vector ks (take n (map stringify (dict-val x))))]
      (println (substr #?(:clj (format f k v)
                          :cljs (gstring/format f k v))  0 w)))))

(defn show-lambda [x]
  (println (lambda-text x)))

(defn show-table [x]
  (println (str/join "\n" (table-as-strings x))))

(defn show-keyed-table [x]
  (println (str/join "\n" (map (fn [& r] (str/join "| " r))
                               (table-as-strings (dict-key x))
                               (table-as-strings (dict-val x))))))

(defn show-vector [x] ;; TODO - distinguish uniform vs mixed (use meta?)
  (let [s (fn self [x]
            (if (< 0 (count x))
              (cond (char? (first x))   (str "\"" (str/join x) "\"")
                    (vector? (first x)) (mapv self x)
                    (= 1 (count x))     (str "," (str x))
                    :else               (str/join " " (mapv str x)))
              "[]"))]
    (println (s x))))

(defn show [x]
  (cond (vector? x)      (println (stringify-vector x))
        (dict? x)        (show-dict x)
        (table? x)       (show-table x)
        (keyed-table? x) (show-keyed-table x)
        (lambda? x)      (show-lambda x)
        (snapshot? x)    (show (snapshot-value x))
        (bool? x)        (println (stringify-boolean x))
        :else            (println x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser initialization
;; Used by the parse and parse functions to replace all limited
;; non-terminals with their regular (not limited) versions, as the
;; limits apply only for parsing and are not relevant to evaluation.
(def xform   {:ladverbed (fn [& x] (vec (cons :adverbed x)))
              :lassign   (fn [& x] (vec (cons :assign x)))
              :alhs      (fn [& x] (vec (cons :lhs x)))
              :dalhs     (fn [& x] (vec (cons :darg x)))
              :valhs     (fn [& x] (vec (cons :varg x)))
              :lat       (fn [& x] (vec (cons :at x)))
              :ldot      (fn [& x] (vec (cons :dot x)))
              :ldotn     (fn [& x] (vec (cons :dotn x)))
              :ldyop     (fn [& x] (vec (cons :dyop x)))
              :lexpr     (fn [& x] (vec (cons :expr x)))
              :lid       (fn [& x] (vec (cons :id x)))
              :ljuxt     (fn [& x] (vec (cons :juxt x)))
              :loneat    (fn [& x] (vec (cons :at x)))
              :lonedot   (fn [& x] (vec (cons :dot x)))
              :elhs      (fn [& x] (vec (cons :lhs x)))
              :lelhs     (fn [& x] (vec (cons :lhs x)))
              :llhs      (fn [& x] (vec (cons :lhs x)))
              :lmonop    (fn [& x] (vec (cons :monop x)))
              :lop       (fn [& x] (vec (cons :op x)))
              :lrhs      (fn [& x] (vec (cons :rhs x)))
              :lverb     (fn [& x] (vec (cons :verb x)))})

(defn load-grammar
  ([x] ;; path, url, whatever depending on platform
   #?(:clj  (clojure.java.io/resource x)
      :cljs (let [req (js/XMLHttpRequest.)]
              (.open req "GET" x false) ;; sync! we need that grammar
              (.send req)
              (if (= 200 (.-status req))
                (.-responseText req)
                (err "Could not load qiss grammar!" req)))))
  ([x f] ;; url, callback
   #?(:clj  (f (load-grammar x))
      :cljs (xhr/send x (fn [r] (f (response-text r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indentation thoughts
;; Consider
;; a)
;;     x+
;;     y
;; b)
;;     x
;;     +y
;; c)
;;     x
;;     +
;;     y
;; should all be the same as x+y?
;; a) looks like projection on line 1
;; b) looks like monadic on line 2
;; c) looks like function reference on line 2
;; We could require that multiline expressions must either
;;   1) be a special form [i.e., if, select, etc], or
;;   2) use parens
;; Since multiline expressions should be rare, this might be a good idea.
;; The downside is that most (all?) grammar components need two versions:
;; one that allows newlines in the whitespace, and one that doesn't.
;; Indentation is another option:
;; a)
;;    x+
;;     y
;; is one expr where as the previous example a) is two exprs.

;; Python does *both* by separating lexing from parsing. So, maybe the
;; right path is to do some preprocessing that handles indentation,
;; special forms, and parens before handing chunks of expressions off
;; to the parser.  This also means the preprocessor needs to be smart
;; about strings, too.

;; Perhaps we can do it the Python way by inserting special character
;; sequences to indicate indent and dedent.  Then we can let instaparse
;; do the hard work.

(defn expand [x]
  "Expand tabs in the string x (like unix expand)"
  (reduce (fn [s c]
            (if (not= \tab c)
              (str s c)
              (apply str s (repeat (- 8 (mod (count s) 8)) \space))))
          ""
          x))
(defn count-leading-spaces [x]
  "The number of leading spaces in the string x"
  (if-let [m (re-matches #"( +).*" x)]
    (count (second m))
    0))
(defn dedents [n]
  (apply str (repeat n ":::")))
(defn add-indent-tokens [pl]
  (first
   (reduce
    (fn [[r s] n] ;; result indent-stack linenum
      (let [ln (get pl n)
            i  (count-leading-spaces ln)
            t  (last (null! s))]
        (cond (= t i) [(conj r (str \; ln)) s]
              (< t i) [(conj r (str "!!!" ln)) (conj s i)]
              :else   (let [j (last-index-of s i)
                            k (- (count s) (+ 1 j))]
                        (if (< k 0)
                          (err "bad indent on line" (+ 1 n) ":" ln)
                          [(conj r (apply str \; (dedents k) ln))
                           (subvec s 0 k)])))))
    [[] [0]]
    (range (count pl)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare parse)
(defn kload [e x]
  ;; whatever we do, preserve original line numbers
  #?(:clj (let [f       (slurp x)
                pl      (mapv expand (str/split-lines (slurp x))) ;; physical lines
                indents (mapv count-leading-spaces pl)
                text    (mapv subs pl indents)] ;; prolly useless
            (reduce #(let [[ne r] (resolve-full-expr f %1 %2)] ne)
                    e
                    (rest (second (parse f :start :file)))))))
;;(defn kload [e x]
    ;; (reduce (fn [e [n i]]
    ;;           (let [t (get text n)]
    ;;             (if (or (= 0 (count t)) (= \/ (first t)))
    ;;               e
    ;;               (let [[ne r] (resolve-full-expr t e (parse t :start :exprs))]
    ;;                 ne))))
    ;;         e
    ;;         (mapv vector (til (count pl)) indents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Streams
;; Streams are implemented as observables; you subscribe and pass in
;; a map of functions: on-next and on-done (and someday on-error).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stream transforms
;; When a function is applied to a set of arguments (via invoke)
;; and any of those arguments is a stream, then we create a new
;; stream which will push the results of the function being re-applied
;; as its stream arguments change.  It's like a spreadsheet cell
;; containing a formula: the cell updates whenever any input to
;; the formula changes.
(defn make-monadic-stream [e f s]
  (let [g       (lambda-callable e f)
        h       (if (snapshot-aware? f 1) g #(g (snapshot-value %)))
        obs     (atom nil)
        on-next #(let [ss (derive-snapshot % (h %))]
                   (when-not (swallow? ss) ss))
        stream  (make-stream (:sources s) on-next)
        r       (assoc stream
                       :unsub
                       #(when (= [] ((:unsub stream) %))
                          ((:unsub s) @obs)))]
    ((:sub s) (reset! obs r))
    r))
(defn make-monadic-stream-dup [e f s n]
  (let [g       (lambda-callable e f)
        h       (if (snapshot-aware? f n)
                  (fn [x] (apply g (repeat n x)))
                  (fn [x] (apply g (repeat n (snapshot-value x)))))
        obs     (atom nil)
        on-next #(let [ss (derive-snapshot % (h %))]
                   (when-not (swallow? ss) ss))
        stream  (make-stream (:sources s) on-next)
        r       (assoc stream
                       :unsub
                       #(when (= [] ((:unsub stream) %))
                          ((:unsub s) @obs)))]
    ((:sub s) (reset! obs r))
    r))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-stream-distinct [e f s]
  (let [a       (atom (mapv (fn [x] nil) s)) ;; args
        d       (atom (mapv (fn [x] nil) s)) ;; done TODO
        obs     (atom nil)
        subs    (atom [])
        srcmap  (apply merge-with
                       into
                       (mapv #(let [t (:sources (s %))]
                                (zipmap t (repeat (count t) [%])))
                             (til-count s)))
        on-done (fn [i]
                  (fn []
                    (when (every? some? (swap! d assoc i true))
                      (doseq [s @subs] ((:on-done s)))
                      (reset! subs nil))))
        on-next (fn [i]
                  (fn [ssi] ;; snapshot in
                    (let [p (swap! a assoc i ssi)] ;; p is all current inputs
                      (when (every? some? p)
                        (let [q (mapv (fn [i] (:event (p i)))
                                      (srcmap (snapshot-source ssi)))]
                          (when (apply = q)
                            (let [sso (derive-snapshot ssi (f p))]
                              (when-not (swallow? sso)
                                (doseq [s @subs] ((:on-next s) sso))))))))))
        r     {:sources (vec (distinct (mapcat :sources s)))
               :stream  true
               :sub     #(swap! subs conj %)
               :unsub   #(when (= [] (swap! subs except %))
                           (doseq [x (mapv :unsub s)] (x @obs))
                           (reset! obs nil))}]
    (let [oo (mapv (fn [i] {:on-done (on-done i) :on-next (on-next i)})
                   (til-count s))]
      (doseq [[o in] (map vector oo s)] ((:sub in) o))
      (reset! obs oo))
    r))
;; This is too complicated :-/
;; f is a function of (count s) arguments; s is a vec of streams.
;; make-func-stream will create a stream that will call f every time
;; any element of s changes and push the results of f.
(defn make-func-stream [e f s] ;; env, function and stream args
  (if (= 1 (count s))
    (make-monadic-stream e f (first s))
    (let [g (group s)]
      (if (= 1 (kcount g))
        (make-monadic-stream-dup e f (first s) (count s))
        (let [h (if (snapshot-aware? f (count s))
                  #(apply (lambda-callable e f) %)
                  #(apply (lambda-callable e f) (mapv snapshot-value %)))]
          (if (= (count s) (kcount g))
            (make-stream-distinct e h s)
            ;; maybe this can be reworked around sources instead of streams
            ;; to make this case simpler
            (make-stream-distinct
             e
             (fn [& ss] ;; assert (= (count ss) (kcount g))
               (let [a (reduce (fn [v [n x]] (catv v (repeat n x)))
                               []
                               (map list (map count (dict-val g)) ss))]
                 (h a)))
             (dict-key g))))))))
(defn done [e f s] ;; add f (in env e) as an on-done handler to stream s
  ((:sub s) {:on-next (fn [e] nil) :on-done (lambda-callable e f)})
  s)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOM
(def last-event-source-id (atom 0))
#?(:cljs (defn ev [event ele]
           (let [source (swap! last-event-source-id inc)
                 subs   (atom [])
                 cb     (fn [e] (let [ss (make-snapshot source e)]
                                  (doseq [s @subs] ((:on-next s) ss))))]
             (dom/listen! (:element ele) event cb)
             (merge ele {:on-done (fn [] (dom/unlisten! (:element ele) event cb))
                         :stream  true
                         :event   event
                         :sources [source]
                         :sub     #(swap! subs conj %)
                         :unsub   #(when (= [] (swap! subs except %))
                                     (dom/unlisten! (:element ele) event cb))}))))
#?(:cljs (defn kdom [s]
           (cond (and (coll? s) (not (kstring? s))) (mapv kdom s)
                 (keyword? s) {:element (dom/sel1 (str "#" (name s)))}
                 (kstring? s) (mapv (fn [x] {:element x}) (dom/sel (apply str s)))
                 :else        (err "invalid argument to dom:" s))))
#?(:cljs (defn text
           ([e] (dom/text (:element e)))
           ([e t] (dom/set-text! (:element e) (apply str t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer
(defn set-timer [f t]
  #?(:clj  (future (while true (do (Thread/sleep t) (f))))
     :cljs (js/setInterval f t)))
(defn stop-timer [x]
  #?(:clj  (future-cancel x)
           :cljs (js/clearInterval x)))
(defn every [t] ;; milliseconds
  (let [source  (swap! last-event-source-id inc)
        subs    (atom [])
        on-done (fn [] (doseq [s @subs] ((:on-done s))))
        on-next (fn [e] (let [ss (make-snapshot source e)]
                          (doseq [s @subs] ((:on-next s) ss))))
        id      (set-timer (fn [] (on-next (now))) t)]
    {:on-done (fn [] (stop-timer id) (on-done))
     :sources [source]
     :stream  true
     :sub     #(swap! subs conj %)
     :unsub   #(when (= [] (swap! subs except %))
                 (stop-timer id))
     :timer-id id}))
(defn stop [x]
  (when-let [t (:timer-id x)] (stop-timer t))
  (when-let [d (:on-done x)]  (d)))
(defn throttle [t s] ;; milliseconds, stream
  "Sample s every t milliseconds"
  (let [subs     (atom [])
        latest   (atom nil)
        on-timer (fn [] (let [e @latest]
                          (when (some? e)
                            (doseq [s @subs] ((:on-next s) e)))))
        id       (set-timer on-timer t)
        on-done  (fn []
                   (stop-timer id)
                   (doseq [s @subs] ((:on-done s))))
        on-next  (fn [ss] (reset! latest ss))
        obs      {:on-done on-done :on-next on-next}]
    ((:sub s) obs)
    {:sources  (:sources s)
     :stream   true
     :sub      #(swap! subs conj %)
     :unsub    #(swap! subs except %)
     :timer-id id}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing event source
(def test-streams (atom []))
(defn into-stream [x] ;; x is a vector of events to push
  (let [v        (if (vector? x) x [x])
        subs     (atom [])
        i        (atom 0) ;; index of event in v to send next
        source   (swap! last-event-source-id inc)
        stream   (atom {})
        on-done  (fn []
                   (swap! test-streams except @stream)
                   (doseq [s @subs] ((:on-done s)))
                   (reset! subs nil))
        push     (fn []
                   (let [curr-subs @subs
                         j         (if (= 0 (count curr-subs))
                                     @i
                                     (swap! i #(+ 1 (min (count v) %))))]
                     (when (< 0 (count curr-subs))
                       (if (<= j (count v))
                         (do
                           (let [ss (make-snapshot source (v (dec j)))]
                             (doseq [s curr-subs] ((:on-next s) ss)))
                           (when (= j (count v))
                             (on-done)))
                         (when (= 0 (count v))
                           (on-done))))
                     (< j (count v)))) ;; more to send?
        r        {:push     push
                  :sources  [source]
                  :stream   true
                  :subs     subs ;; for debugging
                  :sub      #(swap! subs conj %)
                  :unsub    #(when (= [] (swap! subs except %))
                               (reset! i (count v)))}]
    (swap! test-streams conj r)
    (reset! stream r)))
(defn push-test-streams [streams]
  ;; push events from streams randomly
  ;; each stream must be pushable, which so far means only
  ;; streams created via into-stream
  (loop [s streams]
    (let [i (int (rand (count s)))]
      (if ((:push (s i)))
        (recur s)
        (if (< 1 (count s))
          (recur (removev s i)))))))
(defn push-test-sources [sources]
  ;; push events from the test streams identified by sources
  ;; remove those streams from the global atom test-streams
  (let [streams (atom [])]
    (swap! test-streams
           #(let [in-sources (mapv (fn [x]
                                     (if (some (set (:sources x)) sources)
                                       true
                                       false))
                                   %)]
              (reset! streams (index % (where in-sources)))
              (index % (where (mapv not in-sources)))))
    (push-test-streams @streams)))
(defn push-all-test-streams []
  ;; push events from all test streams randomly
  (push-test-streams @test-streams)
  (reset! test-streams []))
(defn wait [x]
  "Force all test streams (created via into-stream) that x depends on
   to push all their data, and wait for all callbacks to complete."
  (if-not (stream? x)
    x
    (let [extract (atom false)
          r       (atom [])
          on-done (fn [] nil)
          on-next #(let [v (snapshot-value %)]
                     (when (:extract %) (reset! extract true))
                     (swap! r conj v))
          obs     {:on-done on-done :on-next on-next}]
      ((:sub x) obs)
      (push-test-sources (:sources x))
      (if @extract (first @r) @r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare genv)
(defn set-genv [e] (reset! genv e) nil)
(defn keval
  ([x] (let [[e r] (resolve-full-expr x @genv (second (parse x)))]
         (set-genv e)
         (lose-env r)))
  ([e x] (lose-env (last (resolve-full-expr x e (second (parse x)))))))

(declare load-code)
#?(:clj (defn load-qiss-file [e f] (load-code e (slurp (string f)))))
#?(:clj (defn load-qiss-file-from-qiss [e f] {:new-env (load-qiss-file e f)}))

(def builtin-common {:cols     {:f cols :rank [1]}
                     :comp     {:f compose :rank [2]}
                     :div      {:f div :rank [2]}
                     :done     {:f done :pass-global-env true :rank [2]
                                :stream-aware[2]}
                     :eval     {:f #(keval (apply str %)) :rank [1]}
                     :every    {:f every :rank [1]}
                     :keys     {:f keycols :rank [1]}
                     :last     {:f klast :rank [1]}
                     :like     {:f like :rank [2]}
                     :lj       {:f lj :rank [2]}
                     :mod      {:f kmod :rank [2]}
                     :show     {:f show :rank [1] :snapshot-aware [1]}
                     :stop     {:f stop :rank [1] :stream-aware [1]}
                     :sv       {:f sv :rank [2]}
                     :throttle {:f throttle :rank [2] :stream-aware [2]}
                     :vs       {:f vs :rank [2]}
                     :xasc     {:f xasc :rank [2]}
                     :xdesc    {:f xdesc :rank [2]}})
(def builtin (merge builtin-common
                    #?(:clj  {:exit   {:f exit :rank [0 1]}
                              :load   {:f load-qiss-file-from-qiss
                                       :pass-global-env true
                                       :rank [1]}
                              :new    {:f k-new :rank [1 2]}
                              :rcsv   {:f rcsv :rank [2]}
                              :rcsvh  {:f rcsvh :rank [2]}
                              :read   {:f read-lines :rank [1]}
                              :wcsv   {:f wcsv :rank [2]}}
                       :cljs {:dom    {:f kdom :rank [1]}
                              :ev     {:f ev :rank [2]}
                              :text   {:f text :rank [2]}})))

(def genv (atom builtin))

(defn eval-no-env [x] (last (kresolve x {} (second (parse x)))))

(defn krun
  ([x] (show (keval x)))
  ([e x] (show (keval e x))))

;; Create an element sequence from an element collection
;; (i.e., a seq from an HTMLCollection)
#?(:cljs (defn eseq [x] (map #(aget x %) (range (count x)))))

;; qiss elements are script tags whose type attr is "text/qiss"
#?(:cljs (defn qiss-elements []
           (filter #(= "text/qiss" (.-type %))
                   (eseq (dom/sel "script")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-code
  #?(:cljs ([] (let [q (qiss-elements)
                     r (make-array (count q))]
                 (null! "found" (count q) "qisses to load...")
                 (doseq [[i e] (map list (range (count q)) q)]
                   (let [s (.-src e)]
                     (if (= "" s) ;; no src attr => code is in tag's text
                       (aset r i (.-text e))
                       (xhr/send s (fn [v] (aset r i (response-text v))
                                     (when (every? some? r)
                                       (set-genv (reduce load-code @genv r))
                                       (null! "qisses loaded"))))))))))
  ([env x]
   (reduce #(let [[ne r] (resolve-full-expr x %1 %2)]
              ne)
           env
           (rest (second (parse x :start :file))))))

(declare grammar)
(declare parser)
(declare parses)
#?(:clj (declare vis))
(defn initialize-qiss-common [after]
  (load-grammar "qiss/grammar"
                (fn [g]
                  (null! "loaded grammar OK")
                  (def grammar g)
                  (def parser  (insta/parser grammar))
                  (def parse   (comp (partial insta/transform xform) parser))
                  (def parses  #(mapv (partial insta/transform xform)
                                      (insta/parses parser %)))
                  #?(:clj (def vis (comp insta/visualize parse)))
                  (after))))
(defn initialize-qiss
  #?(:cljs ([] (initialize-qiss-common load-code)))
  #?(:clj  ([e] (initialize-qiss-common
                 (fn [] (load-code e (slurp "src/qiss/qiss.qiss")))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#?(:clj
   (defn repl
     ([] (repl @genv))
     ([e] ;; env cmdline
      (println "Welcome to qiss.  qiss is short and simple.")
      (loop [e e]
        (do ;; (print "e ") (println e)
          (print "qiss)") (flush))
        ;;               (print "\u00a7)") (flush))
        (if-let [line (read-line)]
          (if (or (empty? line) (= \/ (first line))) ; skip comments
            (recur e)
            (if (and (not= "\\\\" line) (not= "exit" line))
              (let [e2 (try
                         (let [x (second (parse line))
                               [ne r] (resolve-full-expr line e x)]
                           (if (and (not= :assign (first x))
                                    (not (nil? r)))
                             (show r))
                           ne)
                         (catch Exception ex
                           (println (.getMessage ex))
                           e))]
                (recur e2)))))))))

#?(:clj  (when-not *command-line-args* ;; i.e., we're in a clojure repl
           (set-genv (initialize-qiss builtin)))
   :cljs (initialize-qiss)) ;; DO NOT set-genv here; it's async
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main
  "qiss repl"
  [& args]
  ;; TODO support stdin and stdout in the usual way
  #?(:clj (let [a (vec args)
                i (where (like a ".*\\.qiss"))
                f (index a i)
                e (initialize-qiss builtin)]
            (repl (if (empty? f)
                    builtin
                    (load-qiss-file e (first f))))))
  0)
