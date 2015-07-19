(ns qiss.core
  (:require [instaparse.core :as insta])
  (:require [clojure-csv.core :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.stacktrace :as st])
  (:require [clojure.string :as str])
  (:require [midje.sweet :refer :all])
  (:gen-class))

;; Backlog
;;   indexing at depth
;;   dict+dict
;;   @ 3&4 args on keyed tables
;;   .
;;   builtins
;;   update, insert, delete
;;   conditional $
;;   nulls
;;   mixed-type lists with holes as factories
;;   k-ish console output
;;   java interop
;;   attributes
;;   lj
;;   enable UDFs to modify the global env
;;   dot notation for dictionaries
;;   time types
;;   \t and do (see clojure's time and dotimes functions)
;;   system
;;   something like functional query but easier to use

;; functions that will differ btwn JVM and JS
(def bool? (partial instance? java.lang.Boolean))
(def err (fn [& x] (throw (Exception. (str/join ["'" (str/join " " x)])))))
(defn exit
  ([] (exit 0))
  ([x] (System/exit x)))
(defn index-of
  "The first index in x where i appears, or (count x) if i does not
  exist in x"
  [x i] (let [j (.indexOf x i)] (if (< j 0) (count x) j)))
(defn string [x]
  (cond (string? x)  x
        (keyword? x) (name x)
        :else        (str/join x))) ;; vector of char
(defn read-lines [x]
  (with-open [r (io/reader (string x))] (vec (line-seq r))))

(def grammar (clojure.java.io/resource "qiss/grammar"))
;; Used by the parse and parse functions to replace all limited
;; non-terminals with their regular (not limited) versions, as the
;; limits apply only for parsing and are not relevant to evaluation.
(def xform   {:ladverbed (fn [& x] (vec (cons :adverbed x)))
              :lassign   (fn [& x] (vec (cons :assign x)))
              :lat       (fn [& x] (vec (cons :at x)))
              :ldot      (fn [& x] (vec (cons :dot x)))
              :ldyop     (fn [& x] (vec (cons :dyop x)))
              :lexpr     (fn [& x] (vec (cons :expr x)))
              :lid       (fn [& x] (vec (cons :id x)))
              :ljuxt     (fn [& x] (vec (cons :juxt x)))
              :llhs      (fn [& x] (vec (cons :lhs x)))
              :lmonop    (fn [& x] (vec (cons :monop x)))
              :lop       (fn [& x] (vec (cons :op x)))
              :lrhs      (fn [& x] (vec (cons :rhs x)))
              :lverb     (fn [& x] (vec (cons :verb x)))})
(def parser  (insta/parser grammar))
(def parse   (comp (partial insta/transform xform) parser))
(def parses  #(mapv (partial insta/transform xform) (insta/parses parser %)))
(def vis     (comp insta/visualize parse))

(defn to-long [x]
  (cond (coll? x) (if (= 0 (count x)) [] (mapv to-long x))
        (bool? x) (if x 1 0)
        :else x))

(defn all [x] (if (every? (fn [x] x) x) true false))
(defn any [x] (if (some (fn [x] x) x) true false))
(defn catv [x y] (vec (concat x y)))
(declare eq)
(defn except [x y]
  (let [p (if (coll? y) #(some #{%} y) #(= % y))]
    (vec (remove p x))))
(defn in [x y]
  (if (coll? x)
    (mapv #(in % y) x)
    (if (some #{x} y) true false)))
(defn inter
  "Intersection of 2 vectors.  Preserves the order per x"
  [x y] (vec (filter #(some #{%} y) x)))
(declare less)
(defn null!
  "Like 0N! but variadic, e.g., (null! msg thing) => thing"
  [& x] (do (apply println x) (last x)))
(defn raze [x] (vec (mapcat #(if (coll? %) % [%]) x)))
(defn removev [v i] (catv (subvec v 0 i) (subvec v (+ 1 i) (count v))))
(declare kcount)
(defn til-count [x] (vec (range (kcount x))))
(defn union
  "Union of 2 vectors.  Preserves the order of x and the items added
  from y retain their relative order as well"
  [x y] (vec (distinct (concat x y))))
(declare dict-key)
(defn where
  "All elements of x must be integers.  For each element of x (vector
  or dict), concat that many copies of that element's index"
  ([x] (vec (flatten (if (vector? x)
                       (map-indexed #(repeat %2 %1) (to-long x))
                       (map #(repeat %2 %1) (dict-key x) (:v x))))))
  ([e x] [e (where x)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-dict [k v] {:k k :v v})
(defn make-keyed-table [k v] {:k k :v v :kt true})
(defn make-table [c d] {:k c :v d :t true})
(defn dict? [x] (and (map? x) (:k x) (:v x) (not (:t x)) (not (:kt x))))
(defn dict-key [x] (:k x))
(defn dict-val [x] (:v x))
(defn keyed-table? [x] (and (map? x) (:k x) (:v x) (:kt x)))
(defn lambda? [x] (and (map? x) (:f x)))
(defn lambda-body [x] (:exprs x))
(defn lambda-code [x] (:f x))
(defn lambda-env [x] (:env x))
(defn lambda-formals [x] (:formals x))
(defn lambda-rank [x] (:rank x))
(defn lambda-text [x] (:text x))
(defn table? [x] (and (map? x) (:k x) (:v x) (:t x)))
(defn add-to-dict [d k v] (assoc d :k (conj (:k d) k) :v (conj (:v d) v)))
(defn cols [x]
  (cond (table? x)       (dict-key x)
        (keyed-table? x) (catv (cols (dict-key x)) (cols (dict-val x)))
        :else            (err "cols cannot be applied to" x)))
(defn keycols [x]
  (if (keyed-table? x)
    (cols (dict-key x))
    (err "keycols cannot be applied to " x)))
(defn d-from-t [x] (dissoc x :t))
(defn t-from-d [x] (assoc x :t true))

(declare index)
(defn klast [x]
  (cond (vector? x) (last x)
        (table? x)  (index x (- (count x) 1))
        (map? x)    (klast (dict-val x))
        :else       (err "can't apply last to" x)))

(declare apply-monadic)
(declare findv)
(declare index)
(declare invoke)
(defn at-xform-monadic [e x i f]
  ;; TODO: preserve env modifications by passing them back?
  ;; Is that necessary?  All of @'s args have been eval'd already.
  (let [h (fn self [r j]
            (if (coll? j)
              (reduce self r j)
              (let [[ne rr] (invoke e f [(r j)])]
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
              (let [[ne rr] (invoke e f [(index r j)])]
                (at-xform-update-table r j rr))))]
    (h x i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn at-xform-dyadic [e x i f y]
  (let [h (fn self [r [j b]]
            (if (coll? j)
              (reduce self r (if (coll? b)
                               (map vector j b)
                               (map #(vector % b) j)))
              (let [[ne rr] (invoke e f [(r j) b])]
                (assoc! r j rr))))]
    (persistent! (h (transient x) [i y]))))
(defn at-xform-dyadic-table [e x i f y]
  (let [h (fn self [r [j b]]
            (if (coll? j)
              (reduce self r (if (coll? b)
                               (map vector j b)
                               (map #(vector % b) j)))
              (let [[ne rr] (invoke e f [(index r j) b])]
                (at-xform-update-table r j rr))))]
    (h x [i y])))
(defn at-xform
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
  ([e x] (condp #(= (type %1) %2) x ; type
           java.lang.Boolean -1
           java.lang.Long -7
           java.lang.Double -9
           java.lang.Character -10
           java.lang.String 10
           clojure.lang.Keyword -11
           0))
  ([e x y] (last (apply-monadic e x y)))
  ([e x y z] (at-xform e x y z))
  ([e x y z a] (at-xform e x y z a)))

(defn dot
  ([e x] (cond (vector? x)      x
               (dict? x)        (dict-val x)
               (keyed-table? x) (dict-val x)
               (keyword? x)     (if-let [u (e x)] u (err x "not found"))
               :else            (err "nyi: . on" x)))
  ([e x y] (err "nyi: .")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                                  o (where (less p (count (dict-key x))))] ;; overlap
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

(defn amp
  ([x] (where x))
  ([x y] (promote-bools and min x y)))
(defn group [x]
  (cond (vector? x) (let [k (vec (distinct x))]
                      (make-dict k
                                  (reduce (fn [v [i g]]
                                            (let [j (index-of k g)]
                                              (assoc v j (conj (v j) i))))
                                          ;; couldn't make nested transients work
                                          (vec (replicate (count k) []))
                                          (map vector (iterate inc 0) x))))
        (dict? x)   (index (dict-key x) (group (dict-val x)))
        :else       (err "can't group " x)))
(defn eq
  ([x] (group x))
  ([x y] ((atomize =) x y)))

(defn neq
  ([x y] ((atomize not=) x y)))

(defn greater
  ([x] ;; idesc
   (cond (vector? x)      (vec (sort-by x (comp - compare) (til-count x)))
         (dict?   x)      (index (dict-key x) (greater (dict-val x)))
         (table?  x)      (greater (vec (apply (partial map vector)
                                               (dict-val x))))
         (keyed-table? x) (index (dict-key x) (greater (dict-val x)))
         :else            (err "can't >" x)))
  ([x y] ((atomize >) x y)))

(defn less
  ([x] ;; iasc
   (cond (vector? x)      (vec (sort-by x (til-count x)))
         (dict?   x)      (index (dict-key x) (less (dict-val x)))
         (table?  x)      (less (vec (apply (partial map vector) (dict-val x))))
         (keyed-table? x) (index (dict-key x) (less (dict-val x)))
         :else            (err "can't <" x)))
  ([x y] ((atomize <) x y)))

(defn ge [x y] ((atomize >=) x y))
(defn le [x y] ((atomize <=) x y))

(defn pipe
  ([x] (vec (reverse x)))
  ([x y] (promote-bools or max x y)))

(defn tilde
  ([x] ;; not
   (cond (vector? x)      (mapv tilde x)
         (dict? x)        (make-dict (dict-key x) (tilde (dict-val x)))
         (table? x)       (make-table (cols x) (tilde (dict-val x)))
         (keyed-table? x) (make-keyed-table (dict-key x) (tilde (dict-val x)))
         :else            (if (= 0 x) true (not x))))
  ([x y] (= x y))) ;; match

(defn unkey-table [x]
  (if (table? x)
    x
    (let [k (dict-key x) v (dict-val x)]
      (make-table (catv (dict-key k) (dict-key v))
                  (catv (dict-val k) (dict-val v))))))
(defn key-table-by-colname [x y]
  (let [i (index-of (dict-key y) x)]
    (if (= i (count (dict-key y)))
      (err "mismatch: key col" x y)
      (make-keyed-table (make-table [x] [((dict-val y) i)])
                        (make-table (removev (dict-key y) i)
                                    (removev (dict-val y) i))))))
(defn key-table-by-colnames [x y]
  (let [i (mapv #(index-of (dict-key y) %) x)]
    (if (any (mapv (partial = (count (dict-key y))) i))
      (err "mismatch: key cols" x y)
      (make-keyed-table
       (make-table x (index (dict-val y) i))
       (make-table (except (dict-key y) x)
                   (index (dict-val y)
                          (except (til-count (dict-key y)) i)))))))
(defn key-table-by-long [x y]
  (cond (= x 0) y
        (< x 0) (err "lhs of ! must be >=0 when keying a table")
        (<= (count (dict-key y)) x) (err "can't key" x "cols from" y)
        :else (make-keyed-table (make-table (vec (take x (dict-key y)))
                                            (vec (take x (dict-val y))))
                                (make-table (vec (drop x (dict-key y)))
                                            (vec (drop x (dict-val y)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn key-table [x y]
  (cond (table? x)   (if (= (kcount x) (kcount y))
                       (make-keyed-table x y)
                       (err "length" x y))
        (vector? x)  (key-table-by-colnames x y)
        (number? x)  (key-table-by-long x y)
        (keyword? x) (key-table-by-colname x y)
        :else        (err "nyi" x "!" y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bang
  ([x] (cond (number? x)      (vec (range x))
             (vector? x)      (til-count x)
             (dict? x)        (dict-key x)
             (keyed-table? x) (dict-key x)
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

(defn div [x y] ((atomize quot) x y))
(defn kmod [x y] ((atomize mod) x y))
(defn sv [x y]
  "string from vector"
  (vec (str/join (if (vector? x) (str/join x) x)
                 (mapv str/join y))))
(defn vs [x y]
  "vector from string"
  (mapv vec (str/split (str/join y)
                       (re-pattern (if (vector? x) (str/join x) (str x))))))

(defn fdiv
  ([x] (if (coll? x) (mapv #(double (/ %)) x)
           (double (/ x))))
  ([x y] ((atomize #(double (/ %1 %2))) x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn join
  ([x] [x])
  ([x y] (cond (vector? x)             (if (vector? y)
                                         (vec (concat x y))
                                         (conj x y))
               (vector? y)              (vec (cons x y))
               (dict? x)                (if (dict? y)
                                          ((atomize (fn [x y] y)) x y)
                                          (err "can't join" x y))
               (or (coll? x) (coll? y)) (err "can't join" x y)
               :else       [x y])))

(declare findv)
(declare ktake)
(defn take-from-vec [n x]
  (if (<= 0 n)
    (mapv #(x (mod % (count x))) (range n))
    (vec (take-last (- n) x)))) ; TODO neg overtake
(defn take-from-dict [n x]
  (cond (number? n) (make-dict (ktake n (dict-key x)) (ktake n (dict-val x)))
        (keyword? n) (take-from-dict [n] x)
        ;; TODO: introduce nulls like q does in this case?
        (not (every? #(some #{%} (dict-key x)) n)) (err "mismatch: #" n x)
        :else (make-dict n (index (dict-val x) (findv (dict-key x) n)))))
(defn take-from-table [n x]
  (cond (number? n) (make-table (cols x)
                                (mapv (partial ktake n) (dict-val x)))
        (keyword? n) (take-from-table [n] x)
        (not (every? #(some #{%} (cols x)) n)) (err "mismatch: #" n x)
        :else (make-table n (index (dict-val x) (findv (cols x) n)))))
(defn take-from-keyed-table [n x]
  (if (number? n)
    (make-keyed-table (ktake n (dict-key x)) (ktake n (dict-val x)))
    (take-from-table n (unkey-table x))))
(defn kcount [x]
  (cond (vector? x)      (count x)
        (dict? x)        (count (dict-val x))
        (table? x)       (count (first (dict-val x)))
        (keyed-table? x) (count (first (dict-val (dict-key x))))
        :else            1))
(defn ktake [x y]
  (if (and (coll? x) (not (empty? x)) (number? (first x)))
    (err "nyi: reshape") ;; TODO: take a box
    (let [n (if (bool? x) (if x 1 0) x)]
      (cond (not (coll? y))  (vec (repeat n y))
            (vector? y)      (take-from-vec n y)
            (dict? y)        (take-from-dict n y)
            (table? y)       (take-from-table n y)
            (keyed-table? y) (take-from-keyed-table n y)
            :else           (err "nyi: # on " y)))))
(defn pound
  ([x] (kcount x))
  ([x y] (ktake x y)))

(defn minus
  ([x] (- x))
  ([x y] ((atomize -) x y)))

(defn each [f] ; TODO: atomize when f is dyadic? e.g., 3+'1 2 3
  (fn [e & x]
    (let [c (lambda-code f)
          p (if (:pass-global-env f) (partial c e) c)]
      (apply (partial mapv (fn [& a] (apply p a))) x))))

(defn each-left [f]
  (fn [e x y]
    (let [c (lambda-code f)
          p (if (:pass-global-env f) (partial c e) c)]
      (mapv #(p % y) x))))

(defn each-prior [f]
  (fn [e & x]
    (let [c (lambda-code f)
          p (if (:pass-global-env f) (partial c e) c)]
      (if (= 1 (count x))
        (let [q (first x)]
          (vec (cons (first q) (map p (next q) (drop-last q)))))
        (let [q (second x)]
          (vec (map p q (cons (first x) (drop-last q)))))))))

(defn each-right [f]
  (fn [e x y]
    (let [c (lambda-code f)
          p (if (:pass-global-env f) (partial c e) c)]
      (mapv (partial p x) y))))

(defn over [f]
  (fn [e & x]
    (let [c (lambda-code f)
          p (if (:pass-global-env f) (partial c e) c)]
      (if (= 1 (count x))
        (reduce p (first x))
        (reduce p (first x) (second x))))))

(defn flip [x]
  (cond (vector? x) (apply mapv vector x)
        (table? x)  (d-from-t x)
        (dict? x)   (if (and (all (mapv keyword? (dict-key x)))
                             (all (mapv vector? (dict-val x)))
                             (apply = (mapv count (dict-val x))))
                      (t-from-d x)
                      (err "can only flip column dicts"))
        :else       (err "nyi: flip" x)))
(defn plus
  ([x] (flip x))
  ([x y] (promote-bools #(+ (map to-long [%1 %2])) + x y)))

(defn scan [f]
  (fn [e & x]
    (let [c (lambda-code f)
          p (if (:pass-global-env f) (partial c e) c)]
      (vec (drop 1 (if (= 1 (count x))
                     (reductions f (first x))
                     (reductions p (first x) (second x))))))))

(defn times
  ([x] (first x))
  ([x y] ((atomize *) x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn msome [p & c] ;; like some but p takes multiple args
  (when (not (any (map empty? c)))
    (or (apply p (map first c))
        (recur p (map next c)))))

(defn index-of-colwise [c & r] ;; c is vector of cols, r is row to find
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
  (if (and (or (dict? y) (table? y))
           (every? #(some #{%} (cols x)) (dict-key y)))
    (let [f (partial index-of-colwise (dict-val x))
          v (dict-val y)]
      (if (dict? y) (apply f v) (apply mapv f v)))
    (err "mismatch: ?" x y)))
(declare index)
(defn krand [x y]
  ;; TODO: x < 0 => take without replacement
  (if (vector? y)
    (index y (vec (repeatedly x #(rand-int (count y)))))
    (vec (if (float? y)
           (repeatedly x #(rand y))
           (repeatedly x #(rand-int y))))))
(defn ques
  ([x] (vec (distinct x)))
  ([x y] (cond (vector? x)      (findv x y)
               (dict? x)        (index (dict-key x) (ques (dict-val x) y))
               (table? x)       (find-table x y)
               (keyed-table? x) (index (dict-key x) (ques (dict-val x) y))
               :else            (krand x y)))
  ([x y z] nil)) ; TODO: vector cond

(defn deltas [x]
  (vec (cons (first x) (map - (next x) (drop-last x)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cut [x y] ;; y x+!'1_0-':x,#y
  (let [i (mapv range (next (deltas (conj x (kcount y)))))]
    (index y (mapv (fn [p q] (mapv #(+ p %) q)) x i))))
(defn kdrop [x y]
  (let [o (if (<= 0 x) (partial drop x) (partial drop-last (- x)))]
    (cond (vector? y)      (vec (o y))
          (dict? y)        (apply make-dict (map o [(dict-key y) (dict-val y)]))
          (table? y)       (make-table (dict-key y) (mapv o (dict-val y)))
          (keyed-table? y) (make-keyed-table (kdrop x (dict-key y))
                                             (kdrop x (dict-val y)))
          :else (err "nyi: _ (drop) on" x y))))
(defn kremove [x y]
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
  ([x] (long x)) ;; floor
  ([x y] (if (coll? y)
           (if (vector? x) (cut x y) (kdrop x y))
           (kremove x y))))

(defn parse-double [x]
  (if (coll? x) (mapv parse-double x) (Double/parseDouble x)))
(defn parse-long [x]
  (if (coll? x) (mapv parse-long x) (Long/parseLong x)))
(defn parse-symbol [x]
  (if (coll? x) (mapv keyword x) (keyword x)))
(defn parse-data [x y]
  (cond (= \J x) (parse-long y)
        (= \F x) (parse-double y)
        (= \S x) (parse-symbol y)
        (= \* x) y
        :else (err "nyi: parse type" (str x))))
(defn rcsv [c f] ;; col types and file name
  (let [d (csv/parse-csv (slurp (string f)))] ;; assumes header line
    (mapv parse-data (string c) (flip (vec d)))))
(defn rcsvh [c f] ;; col types and file name
  (let [[h & d] (csv/parse-csv (slurp (string f)))] ;; assumes header line
    (make-table (vec h) (mapv parse-data (string c) (flip (vec d))))))
(defn wcsv [f t] ;; output file and table
  (let [make-string (fn [x] (mapv #(if (keyword? %) (name %) (str %)) x))
        d (cons (mapv name (cols t)) (map make-string (flip (dict-val t))))]
    (with-open [w (io/writer (string f))]
      (.write w (csv/write-csv d)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ops {
          (keyword "~") {:op true :f tilde  :text "~" :rank [1 2]}
          :! {:op true :f bang :text "!" :rank [1 2]}
          :at {:f at :pass-global-env true :text "@" :rank [1 2 3 4]}
          :dot {:f dot :pass-global-env true :text "." :rank [1 2]} ;[1 2 3 4]}
          :+ {:op true :f plus :text "+" :rank [1 2]}
          :- {:op true :f minus :text "-" :rank [1 2]}
          :* {:op true :f times :text "*" :rank [1 2]}
          :% {:op true :f fdiv :text "%" :rank [1 2]}
          :# {:op true :f pound :text "#" :rank [1 2]}
          (keyword ",") {:f join :text "," :rank [1 2]}
          :& {:op true :f amp :text "&" :rank [1 2]}
          :| {:op true :f pipe :text "|" :rank [1 2]}
          :_ {:op true :f under :text "_" :rank [1 2]}
          := {:op true :f eq :text "=" :rank [1 2]}
          :<> {:op true :f neq :text "<>" :rank [2]}
          :< {:op true :f less :text "<" :rank [1 2]}
          :> {:op true :f greater :text ">" :rank [1 2]}
          :<= {:op true :f le :text "<=" :rank [2]} ; don't know what the monadic form does in k
          :>= {:op true :f ge :text ">=" :rank [2]}
          :? {:op true :f ques :text "?" :rank [1 2 3]}
          })
(def adverbs {:' each
              (keyword "\\:") each-left
              (keyword "':") each-prior
              (keyword "/:") each-right
              :/ over
              (keyword "\\") scan})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Like into but handles tuples not just pairs, because sometimes parse
;; gives you back tuples.
(defn map-from-tuples [x]
  (reduce #(let [p (first %2)
                 q (next %2)]
             (assoc %1 p (if (or (not= 1 (count q))
                                 (some #{(first %2)}
                                       [:actuals :aggs :by :exprs :formals :where]))
                           q
                           (first q))))
          {}
          x))
(defn strmax [x y]
  (cond (nil? x)            y
        (nil? y)            x
        (< 0 (compare x y)) x
        :else               y))
(defn implicit-args [x] ; exprs
  (let [a ["x" "y" "z"]
        mia (fn self [x] ; max implicit arg
              (cond (or (not (coll? x)) (empty? x) (= :lambda (first x))) nil
                    (= :id (first x)) (if (some #{(second x)} a)
                                        (second x)
                                        nil)
                    :else (reduce strmax (mapv self x))))
        n (mod (+ 1 (index-of a (mia x))) (+ 1 (count a)))]
    (vec (take n a))))
(defn args [f]
  (if-let [a (:formals f)]
    (mapv #(keyword (second %)) a)
    (mapv keyword (implicit-args (:exprs f)))))

(declare kresolve)
(declare resolve-full-expr)

;; TODO think through updating closed-over variables
;; vs locals vs globals ...
(defn feval [tu f]
  (fn [e & x]
    (if (not (some #{(count x)} (lambda-rank f)))
      (err "rank" (lambda-text f) (vec x))
      (let [e2 (merge e (merge (lambda-env f) (zipmap (lambda-formals f) x)))]
        (loop [e3 e2 p (lambda-body f) r nil]
          (if (empty? p)
            r
            (let [[e4 rr] (resolve-full-expr tu e3 (first p))]
              (recur e4 (next p) rr))))))))
(defn index-table [t i]
  (cond (or (and (vector? i) (keyword? (first i)))
            (keyword? i))
        (index (d-from-t t) i)
        (or (and (vector? i) (number? (first i)))
            (number? i))
        (let [d (d-from-t t)
              r (make-dict (dict-key d) (mapv #(index % i) (dict-val d)))]
          (if (vector? i)
            (t-from-d r)
            r))
        :else (mapv (partial index-table t) i)))
(declare apply-constraints)
(declare builtin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn index-keyed-table [t i]
  (cond (or (dict? i) (table? i)) (index-table (dict-val t)
                                               (find-table (dict-key t) i))
        ;; user supplied just the value of a dict we hope conforms
        (and (coll? i) (= (count i) (count (keycols t))))
        (index-keyed-table t (make-dict (keycols t) i))
        (and (not (coll? i)) (= 1 (count (keycols t))))
        (index-keyed-table t (make-dict (keycols t) [i]))
        :else (err "nyi: index keyed table" t i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: unify apply-monadic, index, invoke,
;; resolve-at, resolve-call, and resolve-juxt
(defn index [x i]
  (cond (table? x)       (index-table x i)
        (keyed-table? x) (index-keyed-table x i)
        (dict? i)        (make-dict (dict-key i) (index x (dict-val i)))
        (vector? i)      (mapv (partial index x) i)
        (coll? i)        (reduce index x i) ;; wrong
        (vector? x)      (x i)
        :else            ((dict-val x) (index-of (dict-key x) i))))

(defn invoke [e f a]
  (let [c (lambda-code f)
        p (if (:pass-global-env f) (partial c e) c)]
    (if (and (= 1 (count a))
             (= :hole (first a))
             (some #{0} (lambda-rank f)))
      [e (p)]
      [e (apply p a)])))
(defn is-callable [x] (instance? clojure.lang.IFn x))
(defn can-only-be-monadic [x] false)
(defn can-be-monadic [x] (some #{1} (lambda-rank x)))
(defn can-be-dyadic [x] (some #{2} (lambda-rank x)))
(defn apply-monadic [e f x]
  (if (not (lambda? f))
    [e (index f x)]
    (let [c (lambda-code f)]
      [e (if (:pass-global-env f) (c e x) (c x))])))



(defn resolve-adverbed [tu e x]
  (if (contains? x :lhs) ; eval must be right to left!
    (if (contains? x :rhs)
      (let [[e2 r] (kresolve tu e  (:rhs x))
            [e3 o] (kresolve tu e2 (:verb x))
            [e4 l] (kresolve tu e3 (:lhs x))
            m      (adverbs (keyword (:adverb x)))]
        [e4 ((m o) e l r)])
      (err "nyi: bind lhs of adverbed expr (partial)"))
    (if (contains? x :rhs)
      (let [[e2 r] (kresolve tu e  (:rhs x))
            [e3 o] (kresolve tu e2 (:verb x))
            m      (adverbs (keyword (:adverb x)))]
        [e3 ((m o) e r)])
      (let [[e2 o] (kresolve tu e (:verb x))
            m      (adverbs (keyword (:adverb x)))]
        [e2 (merge o {:f (m o) :pass-global-env true})]))))

(defn resolve-assign [tu e x]
  (let [[e2 r] (kresolve tu e (:rhs x))]
    [(assoc e2 (keyword (:id x)) r) r]))

(defn resolve-at [tu e x]
  (if (:rhs x)
    (let [[e2 rhs] (kresolve tu e (:rhs x))]
      (if (:lhs x)
        (let [[e3 lhs] (kresolve tu e2 (:lhs x))]
          (apply-monadic e3 lhs rhs))
        (at e2 rhs)))
    (if (:lhs x)
      (err "nyi: partially bound @ from lhs")
      [e (ops :at)])))

(defn resolve-dot [tu e x]
  (if (:rhs x)
    (let [[e2 rhs] (kresolve tu e (:rhs x))]
      (if (:lhs x)
        (let [[e3 lhs] (kresolve tu e2 (:lhs x))]
          (apply invoke e3 [lhs rhs])) ;; TODO: generalize
        [e2 (dot e2 rhs)]))
    (if (:lhs x)
      (err "nyi: partially bound . from lhs")
      [e (ops :dot)])))

(defn resolve-call [tu e x]
  (loop [e e a (reverse (:actuals x)) r ()]
    (if (empty? a) ;; TODO: partial
      (let [[ne f] (kresolve tu e (:target x))]
        (if (not (lambda? f))
          (if (= 1 (count r))
            [ne (index f (first r))]
            [ne (index f r)])
          (invoke ne f r)))
      (let [[ne rr] (kresolve tu e (first a))]
        (recur ne (next a) (cons rr r))))))

(defn resolve-dyop [tu e x]
  (let [o        (ops (keyword (:op x)))
        [e2 rhs] (kresolve tu e (:rhs x))
        [e3 lhs] (kresolve tu e2 (:lhs x))]
    (invoke e3 o [lhs rhs])))

(defn resolve-juxt [tu e x]
  (let [[e2 rhs] (kresolve tu e (:rhs x))
        [e3 o] (kresolve tu e2 (:lhs x))]
    (if (:second rhs)
      (invoke e3 rhs [o (:second rhs)])
      (if (can-be-dyadic o)
        [e3 (merge o {:second rhs})]
        (apply-monadic e3 o rhs)))))

(defn resolve-lambda [tu t e x]
  (let [a (args x)
        w (merge x {:formals a ;; TODO: write lambda ctor
                    :env e
                    :pass-global-env true
                    :rank [(count a)]
                    :text t})]
    [e (assoc w :f (feval tu w))]))

(defn resolve-list [tu e x]
  (let [p (reverse x)]
    (loop [e e i p r ()]
      (if (empty? i)
        [e (vec r)]
        (let [[ne nr] (kresolve tu e (first i))]
          (recur ne (next i) (cons nr r)))))))

(defn resolve-monop [tu e x]
  (let [o (ops (keyword (:op x)))
        [e2 a] (kresolve tu e (:rhs x))]
    (apply-monadic e2 o a)))

(defn sub-table [t i]
  (if i
    {:id (fn [x] (if (some #{(keyword x)} (cols t))
                   [:at [:lhs [:id x]] [:rhs [:raw i]]]
                   [:id x]))}
    {}))

(defn apply-constraints [tu env t w]
  (if w
    (loop [[e i] (apply where (kresolve tu env (first w)))
           c     (next w)]
      (if (empty? c)
        [e i]
        (let [[e2 j] (apply where
                            (kresolve tu e (insta/transform (sub-table t i)
                                                            (first c))))]
          (recur [e2 (index i j)] (next c)))))
    [env (vec (range (pound t)))]))

(defn guess-col [x]
  (cond (empty? x)        :x
        (= :id (first x)) (keyword (second x))
        :else (last (map guess-col (next x)))))

(defn compute-aggs [tu e t i a]
  (loop [e e a a r (make-dict [] [])]
    (if (empty? a)
      [e r]
      (let [p      (first a)
            n      (if (= :assign (first p))
                     (keyword (second (second p)))
                     (guess-col p))
            [e2 d] (kresolve tu e (insta/transform (sub-table t i)
                                                    p))]
        (recur e2 (next a) (add-to-dict r n d))))))

(defn resolve-select [tu e x]
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
        [e3 (if (keyed-table? t)
              (key-table-by-colnames (keycols t) (index ut i))
              (index t i))]))))

(defn resolve-table-helper [tu e b]
  (let [c (map (comp map-from-tuples next)
               (if (= :col (first b)) [b] b))]
    (loop [e e a (reverse (map :rhs c)) r ()]
      (if (empty? a)
        (if (apply = (map count r))
          [e (make-table (mapv #(keyword (:id %)) c) (vec r))]
          (err "length"))
        (let [[ne rr] (kresolve tu e (first a))]
          (recur ne (next a) (cons rr r)))))))

(defn resolve-table [tu e x]
  (let [[e2 t] (resolve-table-helper tu e (:cols x))]
    (if-let [kc (:keycols x)]
      (let [[e3 k] (resolve-table-helper tu e2 kc)]
        [e3 (make-keyed-table k t)])
      [e2 t])))

(defn resolve-update [tu e x]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kresolve [tu e x] ; translation unit, env, parse tree
  (let [t (if (coll? x) (first x) x)
        v (if (coll? x)
            (cond (= :list t)          (next x)
                  (= :expr t)          (second x)
                  (= :raw t)           (second x)
                  (vector? (second x)) (map-from-tuples (next x))
                  (= 2 (count x))      (second x)
                  :else                (next x))
            nil)]
    (cond (= t :adverbed) (resolve-adverbed tu e v)
          (= t :assign  ) (resolve-assign tu e v)
          (= t :at      ) (resolve-at tu e v)
          (= t :bool    ) [e (= "1" v)]
          (= t :bools   ) [e (mapv #(= \1 %) v)]
          (= t :call    ) (resolve-call tu e v)
          (= t :char    ) [e (char (first v))]
          (= t :chars   ) [e (vec v)]
          (= t :dot     ) (resolve-dot tu e v)
          (= t :dyop    ) (resolve-dyop tu e v)
          (= t :empty   ) [e []]
          (= t :expr    ) (resolve-full-expr tu e v)
          (= t :float   ) [e (parse-double v)]
          (= t :floats  ) [e (parse-double (str/split v #"[ \n\r\t]+"))]
          (= t :hole    ) [e :hole] ;; nil ?
          (= t :id      ) (if-let [u (e (keyword v))]
                            [e u]
                            (err v "not found in scope"))
          (= t :juxt    ) (resolve-juxt tu e v)
          (= t :lambda  ) (resolve-lambda tu
                                          (apply (partial subs tu) (insta/span x))
                                          e
                                          v)
          (= t :list    ) (resolve-list tu e v)
          (= t :long    ) [e (parse-long v)]
          (= t :longs   ) [e (parse-long (str/split v #"[ \n\r\t]+"))]
          (= t :monop   ) (resolve-monop tu e v)
          (= t :op      ) [e (ops (keyword v))]
          (= t :raw     ) [e v]
          (= t :select  ) (resolve-select tu e v)
          (= t :symbol  ) [e (keyword v)]
          (= t :symbols ) [e (mapv keyword (next (str/split v #"`")))]
          (= t :table   ) (resolve-table tu e v)
          (= t :update  ) (resolve-update tu e v)
          :else           [e x])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resolve-full-expr [tu e x]
  (let [[e2 r] (kresolve tu e x)]
    (if (and (= :juxt (first x)) (:second r))
      (apply-monadic e2 r (:second r))
      [e2 r])))

(def viewport {:rows 25 :cols 80})

(defn substr [s b e] (subs s b (min e (count s))))

(declare stringify)
(defn stringify-dict [x]
  (let [ks (mapv stringify (dict-key x))
        kw (apply max (map count ks))
        f  (str "%" kw "s| %s")]
    (str/join "\n" (map #(format f %1 %2) ks (mapv stringify (dict-val x))))))
(defn table-as-strings [x]
  (let [h (mapv name (cols x)) ; col header
        n (min (:rows viewport) (count (first (dict-val x)))) ; TODO: fix
        c (mapv #(vec (take n %)) (dict-val x)) ; col data
        s (mapv #(mapv stringify %) c)
        w (mapv #(apply max (cons (count %1) (map count %2))) h s)
        f (mapv #(str "%-" % "s") w)
        fmt-row (fn [& r] (str/join " " (map format f r)))]
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
  (if (and (< 0 (count x)) (char? (first x)))
    (str/join x)
    (str/join " " (mapv stringify x))))
;;    (str (mapv stringify x))))
(defn stringify [x]
  (cond (vector? x)      (stringify-vector x)
        (dict? x)        (stringify-dict x)
        (table? x)       (stringify-table x)
        (keyed-table? x) (stringify-keyed-table x)
        (lambda? x)      (stringify-lambda x)
        :else            (str x)))
  
(defn show-dict [x]
  (let [n  (min (:rows viewport) (count (dict-key x)))
        w  (:cols viewport)
        ks (mapv stringify (take n (dict-key x)))
        kw (apply max (map count ks))
        f  (str "%" kw "s| %s")]
    (doseq [[k v] (mapv vector ks (take n (map stringify (dict-val x))))]
      (println (substr (format f k v) 0 w)))))

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
                    :else               x)
              "[]"))]
    (println (s x))))

(defn show [x]
  (cond (vector? x)      (show-vector x)
        (dict? x)        (show-dict x)
        (table? x)       (show-table x)
        (keyed-table? x) (show-keyed-table x)
        (lambda? x)      (show-lambda x)
        :else       (println x)))

(defn eval-no-env [x] (last (kresolve x {} (second (parse x)))))

(defn kload [e x]
  (let [f (slurp x)]
    (reduce #(let [[ne r] (resolve-full-expr f %1 %2)] ne)
            e
            (rest (parse f :start :exprs)))))

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

(def builtin (kload {:cols   {:f cols :rank [1]}
                     :div    {:f div :rank [2]}
                     :exit   {:f exit :rank [0 1]}
                     :keys   {:f keycols :rank [1]}
                     :last   {:f klast :rank [1]}
                     :lj     {:f lj :rank [2]}
                     :mod    {:f kmod :rank [2]}
                     :rcsv   {:f rcsv :rank [2]}
                     :rcsvh  {:f rcsvh :rank [2]}
                     :read   {:f read-lines :rank [1]}
                     :sv     {:f sv :rank [2]}
                     :vs     {:f vs :rank [2]}
                     :wcsv   {:f wcsv :rank [2]}
                     :xasc   {:f xasc :rank [2]}
                     :xdesc  {:f xdesc :rank [2]}}
                    "src/qiss/qiss.qiss"))

(defn repl
  ([] (repl builtin))
  ([e] ;; env
   (println "Welcome to qiss.  qiss is short and simple.")
   (loop [e e]
     (do ;; (print "e ") (println e)
       ;;        (print "\u00b3)") (flush))
       (print "\u00a7)") (flush))
     (if-let [line (read-line)]
       (if (or (empty? line) (= \/ (first line))) ; skip comments
         (recur e)
         (if (and (not= "\\\\" line) (not= "exit" line))
           (let [e2 (try
                      (let [x (second (parse line))
                            [ne r] (resolve-full-expr line e x)]
                        (if (not= :assign (first x))
                          (show r))
                        ne)
                      (catch Exception ex
                        (println ex)
                        e))]
             (recur e2))))))))

(defn keval [x] (last (kresolve x builtin (second (parse x)))))
(defn krun [x]  (show (keval x)))

(defn -main
  "qiss repl"
  [& args]
  (repl builtin))

(facts "about bools"
       (fact "bools eval to themselves"
             (keval "1b") => true
             (keval "0b") => false))
(facts "about simple bool vector literals"
       (fact "compact"
             (keval "1001001b") => [true false false true false false true]))
(facts "about ~"
       (fact "atom"
             (keval "~5") => false
             (keval "~0b") => true)
       (fact "vector"
             (keval "~101b") => [false true false]
             (keval "~2 7 0") => [false false true])
       (fact "dict"
             (keval "~`a`b`c!101b") => (keval "`a`b`c!010b"))
       (fact "table"
             (keval "~([]a:1 0 4)") => (keval "([]a:010b)"))
       (fact "keyed tables"
             (keval "~([a:1 0 4]b:010b)") => (keval "([a:1 0 4]b:101b)")))
(facts "about chars"
       (facts "chars eval to themselves"
              (keval "\"a\"") => \a))
(facts "about simple char vector literals"
       (fact "char vector literals eval to vectors of char"
             (keval "\"abc\"") => [\a \b \c]))
(facts "about floats"
       (fact "floats eval to themselves"
             (keval "1.") => 1.0
             (keval "-1.") => -1.0))
(facts "about float vector literals"
       (fact "no punctuation"
             (keval "1.4 2.5 3.6") => [1.4 2.5 3.6])
       (fact "any float promotes the whole vector"
             (keval "1 2.0 3") => [1.0 2.0 3.0])
       (fact "fractional part not needed if it is zero"
             (keval "1 2. 3") => [1.0 2.0 3.0])
       (fact "spaces may be added for formatting"
             (keval "1     2.    3") => [1.0 2.0 3.0]))
(facts "about longs"
       (fact "longs eval to themselves"
             (keval "1") => 1
             (keval "-123") => -123)
       (fact "longs add" ;; TODO: + in clojure promotes to bignum
             (keval "1+10") => 11
             (keval "1+-10") => -9
             (keval "-1+10") => 9)
       (fact "longs subtract"
             (keval "1-10") => -9
             (keval "100-42") => 58))
(facts "about simple long vector literals"
       (fact "no puncuation"
             (keval "1 2 3") => [1 2 3])
       (fact "monadic - sticks to literals"
             (keval "1 -2 3") => [1 -2 3])
       (fact "spaces may be added for formatting"
             (keval "1    2     3") => [1 2 3]))
(facts "about mixed vectors"
       (fact "top-level indexing"
             (keval "(`a`b`c;1)0") => [:a :b :c]))
(facts "about !"
       (fact "monadic ! on a long is til"
             (keval "!3") => [0 1 2])
       (fact "monadic ! on a dict is key"
             (keval "!`a`b`c!1 2 3") => [:a :b :c]))
(facts "about ?"
       (fact "container?... => find"
             (keval "(!5)?0") => 0
             (keval "(!5)?0 2 4") => [0 2 4]
             (keval "(!5)?10 3 1") => [5 3 1]
             (keval "(`a`b`c!1 2 3)?2") => :b
             (keval "(`a`b`c!1 2 3)?3 1") => [:c :a]
             (keval "([]a:1 2 3)?([]a:1 2)") => [0 1]
             (keval "([a:`a`b`c]b:1 2 3)?([]b:1 2)") => (keval "([]a:`a`b)")))
(facts "about +"
       (fact "+ is atomic"
             (keval "1+10 20") => [11 21]
             (keval "1 2+10") => [11 12]
             (keval "1 2+10 20") => [11 22]))
(facts "about symbols"
       (fact "symbols eval to clojure keywords"
             (keval "`abc") => :abc))
(facts "about symbol vector literals"
       (fact "no spaces"
             (keval "`abc`def") => [:abc :def]))
;; TODO             (keval "`abc `def") => error
(facts "about +"
       (fact "+ doesn't care about spaces"
             (keval "1 +2") => 3
             (keval "1+ 2") => 3
             (keval "1 + 2") => 3))
(facts "about <"
       (fact "monadic < is iasc"
             (keval "<`d`d`e`c`c`b`d`e`e`b") => [5 9 3 4 0 1 6 2 7 8])
       (fact "< works on dicts"
             (keval "<`a`b`c`d`e`f`g`h`i`j!4 0 2 1 2 1 2 3 2 4") =>
             (keval "`b`d`f`c`e`g`i`h`a`j")))
(facts "about >"
       (fact "monadic > is idesc"
             (keval ">`d`d`e`c`c`b`d`e`e`b") => [2 7 8 0 1 6 3 4 5 9]))
(facts "about right-to-left"
       (fact "no operator precedence"
             (keval "10*2+3") => 50))
(facts "about _"
       (fact "n _ container => drop 1st n"
             (keval "2_!5") => [2 3 4]
             (keval "2_`a`b`c`d!1 2 3 4") => (keval "`c`d!3 4")
             (keval "2_([]a:`a`b`c`d;b:1 2 3 4)") =>
             (keval "([]a:`c`d;b:3 4)")
             (keval "2_([]a:`a`b`c;b:1 2 3)") => (keval "([]a:,`c;b:,3)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (fact "(-n) _ container => drop last n"
             (keval "-2_!5") => [0 1 2]
             (keval "-2_`a`b`c`d!1 2 3 4") => (keval "`a`b!1 2")
             (keval "-1_([]a:`a`b`c;b:1 2 3)") => (keval "([]a:`a`b;b:1 2)")
             (keval "2_([a:`a`b`c]b:1 2 3)") => (keval "([a:,`c]b:,3)"))
       (fact "[n0...] _ vector => cut"
             (keval "0 2 5_\"0123456\"") => (keval "(\"01\";\"234\";\"56\")"))
       (fact "container _ index => remove at"
             (keval "(!5)_2") => [0 1 3 4]
             (keval "(`a`b`c!1 2 3)_`c") => (keval "`a`b!1 2")
             (keval "(`a`b`c!1 2 3)_`b") => (keval "`a`c!1 3")
             (keval "([]a:`a`b`c;b:1 2 3)_`a") => (keval "([]b:1 2 3)")
             (keval "([]a:`a`b`c;b:1 2 3)_1") => (keval "([]a:`a`c;b:1 3)")))
(facts "about adverbs"
       (fact "they can be monadic"
             (keval "+/1 2 3") => 6)
       (fact "they can be dyadic"
             (keval "0+/1 2 3") => 6)
       (fact "/: does"
             (keval "1 2 3*/:1 2") => [[1 2 3] [2 4 6]])
       (fact "can be assigned"
             (keval "{a:+/;a[0;3 4 5]}[]") => 12))
;;       (fact "they can be compounded"  TODO: fix
;;             (keval ",//(1 2 3;(4 5 6;7 8 9))") => [1 2 3 4 5 6 7 8 9]))
(facts "about calling functions"
       (fact "supplying all arguments causing invocation"
             (keval "div[10;3]") => 3))
(facts "about indexing at depth"
       (fact "2-d vector"
             (keval "(`a`b`c;1 2 3)[0;1]") => :b
             (keval "(`a`b`c;1 2 3)[0;0 1]") => [:a :b]))
(facts "about lambdas"
       (fact "implicit args"
             (keval "{x}[3]") => 3
             (keval "{x+y}[3;4]") => 7
             (keval "{x+y+z}[3;4;5]") => 12))
(facts "about juxtaposition"
       (fact "lambdas juxtapose without whitespace"
             (keval "{x}3") => 3)
       (fact "symbol vector literals followed by longs"
             (keval "`a`b`c`d`e 1") => :b
             (keval "`a`b`c`d`e 1 2 3") => [:b :c :d])
       (fact "dyadic user-defined functions can be used infix"
             (keval "`a`b`c`d`e{x y}1 2 3") => [:b :c :d]))
(facts "about indexing"
       (fact "square brackets no semicolons"
             (keval "1 2 3 4[0 2]") => [1 3])
       (fact "with @"
             (keval "1 2 3 4@0 2") => [1 3])
       (fact "with juxt"
             (keval "{x 0 2}1 2 3 4") => [1 3])
       (fact "repeated"
             (keval "1 2 3 4@(0 2;1 3)") => [[1 3] [2 4]]))
(facts "about group (monadic =)"
       (fact "vector"
             (keval "=4 0 2 1 2 1 2 3 2 4") =>
             (keval "4 0 2 1 3!(0 9;,1;2 4 6 8;3 5;,7)"))
       (fact "dict"
             (keval "=`a`b`c`d`e`f`g`h`i`j!0 1 2 0 1 2 0 1 2 0") =>
             (keval "0 1 2!(`a`d`g`j;`b`e`h;`c`f`i)")))
(facts "about 3-arg @"
       (fact "vector indexed with long"
             (keval "@[!4;1;{x*2}]") => [0 2 2 3])
       (fact "vector indexed with vector"
             (keval "@[!4;1 3;{x+1}]") => [0 2 2 4])
       (fact "vector repeatedly indexed with vector"
             (keval "@[!4;(0 1;1 2);{x+1}]") => [1 3 3 3])
       (fact "dict indexed with atom"
             (keval "@[`a`b`c!1 2 3;`a;{x+1}]") => (keval "`a`b`c!2 2 3"))
       (fact "dict indexed with vector"
             (keval "@[`a`b`c!1 2 3;`a`c;{x+1}]") => (keval "`a`b`c!2 2 4"))
       (fact "dict indexed repeatedly with vector"
             (keval "@[`a`b`c!1 2 3;(`a`b;`a`c);{x+1}]") =>
             (keval "`a`b`c!3 3 4"))
       (fact "table indexed with long"
             (keval "@[([]a:1 2 3);0;{2*x}]") => (keval "([]a:2 2 3)"))
       (fact "table indexed with longs"
             (keval "@[([]a:1 2 3);0 2;{2*x}]") => (keval "([]a:2 2 6)"))
       (fact "table indexed with symbol"
             (keval "@[([]a:1 2 3;b:10 20 30);`a;{2*x}]") =>
             (keval "([]a:2 4 6;b:10 20 30)"))
       (fact "table indexed with symbols"
             (keval "@[([]a:1 2 3;b:10 20 30;c:100 200 300);`a`c;{2*x}]") =>
             (keval "([]a:2 4 6;b:10 20 30;c:200 400 600)"))
       (fact "table complex indexing"
             (keval "@[([]a:1 2 3;b:10 20 30;c:100 200 300);(`a;0);{2*x}]") =>
             (keval "([]a:4 4 6;b:20 20 30;c:200 200 300)")))
(facts "about 4-arg @"
       (fact "vector indexed with long"
             (keval "@[!4;1;*;2]") => [0 2 2 3])
       (fact "vector indexed with vector paired with atom"
             (keval "@[!4;1 3;*;2]") => [0 2 2 6])
       (fact "vector indexed with matrix paired with atom"
             (keval "@[!4;(1 3;2 0);*;2]") => [0 2 4 6])
       (fact "vector indexed with matrix paired with vector"
             (keval "@[!4;(1 3;2 0);*;2 4]") => [0 2 8 6])
       (fact "vector indexed with matrix paired with matrix"
             (keval "@[!4;(1 3;2 0);*;(2 4;6 8)]") => [0 2 12 12])
       (fact "dict indexed with ragged paired with atom"
             (keval "@[`a`b`c!1 2 3;(`a`c;`b);*;2]") => (keval "`a`b`c!2 4 6"))
       (fact "tables"
             (keval "@[([]a:1 2 3;b:10 20 30;c:100 200 300);(`a`b;0);*;(5 10;100)]") =>
             (keval "([]a:500 10 15;b:10000 200 300;c:10000 200 300)")))
(facts "about join"
       (fact "monadic envectors"
             (keval ",1") => [1]
             (keval ",1 2 3") => [[1 2 3]])
       (fact "dyadic joins"
             (keval "1 2,3 4") => [1 2 3 4])
       (fact "joining dicts is a merge where rhs wins"
             (keval "(`a`b`c`e!1 2 3 5),`b`c`d!10 20 30") =>
             (keval "`a`b`c`e`d!1 10 20 5 30")))
(facts "about select"
       (fact "no agg required"
             (keval "select from([]a:1 2 3)") => (keval "([]a:1 2 3)"))
       (fact "id agg guesses result column to match original column"
             (keval "select a from([]a:1 2 3)") => (keval "([]a:1 2 3)"))
       (fact "works on keyed tables"
             (keval "select from([a:`a`b]b:1 2)") => (keval "([a:`a`b]b:1 2)")))
(facts "about by"
       (fact "simple case"
             (keval "select +/b by a from([]a:6#`a`b`c;b:!6)") =>
             (keval "([a:`a`b`c]b:3 5 7)"))
       (fact "compound by clause"
             (keval "select by a,b from([]a:8#`a`a`b`b;b:8#`a`b;c:!8)") =>
             (keval "([a:`a`a`b`b;b:`a`b`a`b];c:(0 4;1 5;2 6;3 7))")))
(facts "about keying tables"
       (fact "can key by first n columns"
             (keval "1!([]a:`a`b`c;b:1 2 3)") => (keval "([a:`a`b`c]b:1 2 3)")
             (keval "2!([]a:`a`b;b:1 2;c:3 4") => (keval "([a:`a`b;b:1 2]c:3 4"))
       (fact "can key by col name(s)"
             (keval "`a!([]a:`a`b`c;b:1 2 3)") => (keval "([a:`a`b`c]b:1 2 3)")
             (keval "`a`c!([]a:`a`b;b:1 2;c:3 4") => (keval "([a:`a`b;c:3 4]b:1 2")))
(facts "about indexing keyed tables"
       (fact "can be done with a dict"
             (keval "([a:`a`b`c]b:1 2 3)`a!`a") => (keval "`b!1"))
       (fact "can be done with just the value of a dict"
             (keval "([a:`a`b`c]b:1 2 3)`c") => (keval "`b!3"))
       (fact "can be done with a table"
             (keval "([a:`a`b`c]b:1 2 3)([]a:`a`c)") => (keval "([]b:1 3)")))
(facts "about where"
       (fact "works with bools"
             (keval "&1001b") => [0 3])
       (fact "works with longs"
             (keval "&0 1 2 3") => [1 2 2 3 3 3]))
(facts "about lj"
       (fact "uses rhs key col"
             (keval "([]a:`a`b`c)lj([a:`a`b`c]b:1 2 3)") =>
             (keval "([]a:`a`b`c;b:1 2 3)"))
       (fact "two key cols"
             (keval "([]a:`a`b`c;b:1 2 3)lj([a:`a`b`c;b:1 2 3]c:10 20 30)") =>
             (keval "([]a:`a`b`c;b:1 2 3;c:10 20 30)")))
(facts "about xasc"
       (fact "sort on one col"
             (keval "`a xasc([]a:`c`b`a;b:1 2 3)") =>
             (keval "([]a:`a`b`c;b:3 2 1)"))
       (fact "sort on two cols"
             (keval "`a`b xasc([]a:`c`b`a`a`b`c;b:3 10 3 20 30 2)") =>
             (keval "([]a:`a`a`b`b`c`c;b:3 20 10 30 2 3)"))
       (fact "works on keyed tables"
             (keval "`a`b xasc([a:`c`b`a`a`b`c]b:3 10 3 20 30 2)") =>
             (keval "([a:`a`a`b`b`c`c];b:3 20 10 30 2 3)")))
(facts "about parsing non-ambiguity"
       (fact adverbed
             (count (parses "f/")) => 1
             (count (parses "f/1 2 3")) => 1
             (count (parses "0 f/1 2 3")) => 1
             (count (parses "0 f//1 2 3")) => 1
             (count (parses "f//1 2 3")) => 1)
       (fact "assign"
             (count (parses "a:3")) => 1
             (count (parses "a:`a`b`c`d`e 1 2 3")) => 1)
       (fact "bools"
             (count (parses "0b")) => 1
             (count (parses "1b")) => 1)
       (fact "bool vector literals"
             (count (parses "010b")) => 1
             (count (parses "1001b")) => 1)
       (fact "call"
             (count (parses "`a`b`c[0]")) => 1
             (count (parses "`a`b`c[0 1]")) => 1
             (count (parses "{x}[0]")) => 1)
       (fact "chars"
             (count (parses "\"a\"")) => 1
             (count (parses "\"\\\"\"")) => 1)
       (fact "char vector lterals"
             (count (parses "\"abc\"")) => 1
             (count (parses "\"abc\\\"\"def")) => 1)
       (fact "dyop"
             (count (parses "1+2")) => 1
             (count (parses "1 +2")) => 1
             (count (parses "1+ 2")) => 1
             (count (parses "1 + 2")) => 1)
       (fact "floats"
             (count (parses "1.")) => 1
             (count (parses "-1.")) => 1)
       (fact "float vector literals"
             (count (parses "1. 2 3")) => 1
             (count (parses "1 2. 3")) => 1
             (count (parses "-1 2. 3")) => 1
             (count (parses "1    2.    3")) => 1)
       (fact "ids"
             (count (parses "x")) => 1
             (count (parses "xyzzy")) => 1)
       (fact "juxt"
             (count (parses "`a`b`c`d`e 1")) => 1
             (count (parses "`a`b`c`d`e 1 2 3")) => 1
             (count (parses "x 1")) => 1
             (count (parses "x 1+2")) => 1
             (count (parses "{x}3")) => 1
             (count (parses "1{y}3")) => 1
             (count (parses "1{y}")) => 1) ; bind 1st arg
       (fact "lambdas"
             (count (parses "{}")) => 1
             (count (parses "{x}")) => 1
             (count (parses "{[a]a}")) => 1
             (count (parses "`a`b`c{x y}0 1 2")) => 1)
       (fact "longs"
             (count (parses "1")) => 1)
       (fact "long vector literals"
             (count (parses "1 2 3")) => 1
             (count (parses "1    2    3")) => 1)
       (fact "monop"
             (count (parses "*1 2 3")) => 1)
       (fact "symbols"
             (count (parses "`a")) => 1)
       (fact "symbol vector literals"
             (count (parses "`a`b`c`d`e")) => 1))
;; gave up on this one: couldn't fix the <exprx> rule
;; (fact "select"
;;       (count
;;        (parses
;;         "select +/a,+/b from([]a:,/3#/:1 2;b:6#10 20 30)where b<=20")) => 1))
