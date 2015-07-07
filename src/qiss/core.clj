(ns qiss.core
  (:require [instaparse.core :as insta])
  (:require [clojure.stacktrace :refer :all])
  (:require [clojure.string :as str])
  (:require [midje.sweet :refer :all])
  (:gen-class))

; Backlog
;   proper scoping (chained envs)
;   indexing at depth
;   dict+dict
;   @ 3&4 args
;   .
;   keyed tables
;   builtins
;   treat strings like vectors
;   special forms for queries
;   conditional $
;   nulls
;   mixed-type lists with holes as factories
;   k-ish console output
;   file I/O
;   java interop
;   attributes
;   lj
;   enable UDFs to modify the global env
;   dot notation for dictionaries
;   time types
;   system

; functions that will differ btwn JVM and JS
(def err #(throw (Exception. (str "'" %))))
(defn exit
  ([] (exit 0))
  ([x] (System/exit x)))
(defn index-of [x i] (let [j (.indexOf x i)] (if (< j 0) (count x) j)))

(def grammar (clojure.java.io/resource "qiss/grammar"))
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

(defn null! [x] (do (println x) x))
(defn all [x] (every? (fn [x] x) x))
(defn any [x] (some (fn [x] x) x))

(defn table? [x] (and (map? x) (:t x)))
(defn d-from-t [x] (dissoc x :t))
(defn t-from-d [x] (assoc x :t true))

(declare apply-monadic)
(declare index)
(defn at
  ([e x] (condp #(= (type %1) %2) x ; type
           java.lang.Boolean -1
           java.lang.Long -7
           java.lang.Double -9
           java.lang.Character -10
           java.lang.String 10
           clojure.lang.Keyword -11
           0))
   ([e x y] (last (apply-monadic e x y))))

(defn atomize [f]
  (fn self[x y]
    (cond (vector? x) (cond (vector? y)
                            (if (= (count x) (count y))
                              (mapv self x y)
                              (err "length"))
                            (map? y) (if (= (count x) (count (:k y)))
                                       {:k (:k y) :v (mapv self x (:v y))}
                                       (err "length"))
                            :else (mapv #(self % y) x))
          (map? x) (cond (vector? y)
                         (if (= (count (:k x)) (count y))
                           {:k (:k x) :v (mapv self (:v x) y)}
                           (err "length"))
                         (map? y) (err "nyi map op map")
                         :else {:k (:k x) :v (mapv #(self % y) (:v x))})
          ; x is atom cases
          (vector? y) (mapv #(self x %) y)
          (map? y) {:k (:k y) :v (mapv #(self x %) (:v y))}
          :else     (f x y))))

(defn to-long [x]
  (cond (coll? x) (if (= 0 (count x)) [] (mapv to-long x))
        (instance? java.lang.Boolean x) (if x 1 0)
        :else x))

(defn where
  ([x] (vec (flatten (map-indexed #(repeat %2 %1) (to-long x)))))
  ([e x] [e (where x)]))
(defn amp
  ([x] (where x))
  ([x y] ((atomize min) x y)))

(defn eq
  ([x] (if (vector? x)
         (let [k (vec (distinct x))]
           (reduce (fn [d [i g]]
                     (let [j (index-of (:k d) g)]
                       (assoc d :v (assoc (:v d) j (conj ((:v d) j) i)))))
                   {:k k :v (vec (replicate (count k) []))}
                   (map (fn [x y] [x y]) (iterate inc 0) x)))
         (err "can't group non-vector " x)))
  ([x y] ((atomize =) x y)))

(defn neq
  ([x y] ((atomize not=) x y)))

(defn greater
  ([x] (vec (sort-by x (comp - compare) (range (count x))))) ; idesc
  ([x y] ((atomize >) x y)))

(defn less
  ([x] (vec (sort-by x (range (count x))))) ; iasc
  ([x y] ((atomize <) x y)))

(defn ge [x y] ((atomize >=) x y))
(defn le [x y] ((atomize <=) x y))

(defn pipe
  ([x] (vec (reverse x)))
  ([x y] ((atomize max) x y)))

(defn mkdict [k v] {:k k :v v})

(defn tilde
  ([x] (if (vector? x) (mapv tilde x) (not x)))
  ([x y] (= x y)))

(defn bang
  ([x] (cond (= java.lang.Long (type x)) (vec (range x))
             (map? x) (:k x)
             :else (err (str "nyi: monadic ! on " x))))
  ([x y] (if (= (count x) (count y))
           (mkdict x y)
           (err "length"))))

(defn div [x y] ((atomize quot) x y))
(defn kmod [x y] ((atomize mod) x y))

(defn fdiv
  ([x] (if (coll? x) (mapv #(double (/ %)) x)
           (double (/ x))))
  ([x y] ((atomize #(double (/ %1 %2))) x y)))

(defn join
  ([x] [x])
  ([x y] (if (vector? x)
           (if (vector? y)
             (vec (concat x y))
             (conj x y))
           (if (vector? y)
             (vec (cons x y))
             [x y]))))

(defn ktake
  ([x] (count x))
  ;; take on a dictionary
  ([x y] (cond (coll? x) () ;; TODO: take a box
               (coll? y) (if (instance? java.lang.Boolean x)
                           (if x [(first y)] [])
                           (if (<= 0 x)
                             (mapv #(y (mod % (count y))) (range x))
                             (vec (take-last (- x) y)))) ; TODO neg overtake
               :else (if (instance? java.lang.Boolean x)
                       (if x [y] [])
                       (vec (repeat x y))))))

(defn minus
  ([x] (- x))
  ([x y] ((atomize -) x y)))

(defn each [f] ; TODO: atomize when f is dyadic? e.g., 3+'1 2 3
  (fn [e & x]
    (let [p (if (:pass-global-env f) (partial (:f f) e) (:f f))]
      (apply (partial mapv (fn [& a] (apply p a))) x))))
    
(defn each-left [f]
  (fn [e x y]
    (let [p (if (:pass-global-env f) (partial (:f f) e) (:f f))]
      (mapv #(p % y) x))))

(defn each-prior [f]
  (fn [e & x]
    (let [p (if (:pass-global-env f) (partial (:f f) e) (:f f))]
      (if (= 1 (count x))
        (let [q (first x)]
          (vec (cons (first q) (map p (next q) (drop-last q)))))
        (let [q (second x)]
          (vec (map p q (cons (first x) (drop-last q)))))))))

(defn each-right [f]
  (fn [e x y]
    (let [p (if (:pass-global-env f) (partial (:f f) e) (:f f))]
      (mapv (partial p x) y))))

(defn over [f]
  (fn [e & x]
    (let [p (if (:pass-global-env f) (partial (:f f) e) (:f f))]
      (if (= 1 (count x))
        (reduce p (first x))
        (reduce p (first x) (second x))))))

(defn plus
  ([x] (if (map? x)
         (if (table? x)
           (d-from-t x)
           (if (and (all (mapv keyword? (:k x)))
                    (all (mapv vector? (:v x)))
                    (apply = (mapv count (:v x))))
             (t-from-d x)
             (err "can only flip column dicts")))
         (err "nyi flipping stuff other than dicts")))
  ([x y] ((atomize +) x y)))

(defn scan [f]
  (fn [e & x]
    (let [p (if (:pass-global-env f) (partial (:f f) e) (:f f))]
      (vec (drop 1 (if (= 1 (count x))
                     (reductions f (first x))
                     (reductions p (first x) (second x))))))))

(defn times
  ([x] (first x))
  ([x y] ((atomize *) x y)))

(def builtin {:div  {:f div :rank [2]}
              :exit {:f exit :rank [0 1]}
              :mod  {:f kmod :rank [2]}})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare index)
(defn ques
  ([x] (vec (distinct x)))
  ([x y] (cond (vector? x) (if (vector? y)
                             (mapv #(index-of x %) y)
                             (index-of x y))
               (map? x)    (index (:k x) (ques (:v x) y))
               :else       (if (vector? y)
                             (index y (vec (repeatedly x #(rand-int (count y)))))
                             (vec (if (float? y)
                                    (repeatedly x #(rand y))
                                    (repeatedly x #(rand-int y)))))))
  ([x y z] nil)) ; TODO: vector cond

(defn deltas [x]
  (vec (cons (first x) (map - (next x) (drop-last x)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn under
  ([x] (long x))
  ([x y] (if (vector? x) ; y x+!'1_0-':x,#y
           (let [i (mapv range (next (deltas (conj x (count y)))))]
             (index y (mapv (fn [p q] (mapv #(+ p %) q)) x (null! i))))
           (vec (if (<= 0 x)
                  (drop x y)
                  (drop-last (- x) y))))))

(defn parse-long [x] (Long/parseLong x))
(defn parse-double [x] (Double/parseDouble x))

(def ops {
          (keyword "~") {:op true :f tilde  :rank [1 2]}
          :! {:op true :f bang :rank [1 2]}
          :at {:f at :pass-global-env true :rank [1 2]} ;[1 2 3 4]}
          :+ {:op true :f plus :rank [1 2]}
          :- {:op true :f minus :rank [1 2]}
          :* {:op true :f times :rank [1 2]}
          :% {:op true :f fdiv :rank [1 2]}
          :# {:op true :f ktake :rank [1 2]}
          (keyword ",") {:f join :rank [1 2]}
          :& {:op true :f amp :rank [1 2]}
          :| {:op true :f pipe :rank [1 2]}
          :_ {:op true :f under :rank [1 2]}
          := {:op true :f eq :rank [1 2]}
          :<> {:op true :f neq :rank [2]}
          :< {:op true :f less :rank [1 2]}
          :> {:op true :f greater :rank [1 2]}
          :<= {:op true :f le :rank [2]} ; don't know what the monadic form does in k
          :>= {:op true :f ge :rank [2]}
          :? {:op true :f ques :rank [1 2 3]}
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
                                       [:actuals :aggs :exprs :where]))
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

;; TODO inner functions, scope, closures
(defn feval [f]
  (fn [e & x]
    (if (not (some #{(count x)} (:rank f)))
      (do (println f) (println x) (err "rank")))
    (let [eb (merge e (zipmap (:formals f) x))]
      (loop [ec eb p (:exprs f) r nil]
        (if (empty? p)
          r
          (let [[ed rr] (kresolve ec (first p))]
            (recur ed (next p) rr)))))))
(defn index-table [t i]
  (if (or (and (vector? i) (keyword? (first i)))
          (keyword? i))
    (index (d-from-t t) i)
    (let [d (d-from-t t)
          r {:k (:k d) :v (mapv #(index % i) (:v d))}]
      (if (vector? i)
        (t-from-d r)
        r))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn index [x i]
  (cond (table? x)  (index-table x i)
        (vector? i) (mapv (partial index x) i)
        (map? i)    (mkdict (:k i) (index x (:v i)))
        (coll? i)   (reduce index x i)
        (vector? x) (x i)
        :else ((:v x) (index-of (:k x) i))))
(defn invoke [e f a]
  (let [p (if (:pass-global-env f) (partial (:f f) e) (:f f))]
    (if (and (= 1 (count a))
             (= :hole (first a))
             (some #{0} (:rank f)))
      [e (p)]
      [e (apply p a)])))
(defn is-callable [x] (instance? clojure.lang.IFn x))
(defn can-only-be-monadic [x] false)
(defn can-be-monadic [x] (some #{1} (:rank x)))
(defn can-be-dyadic [x] (some #{2} (:rank x)))
(defn apply-monadic [e f x]
  ; (println "apply-monadic" f "\t" x)
  (if (or (vector? f) (:k f) (table? f))
    [e (index f x)]
    [e (if (:pass-global-env f) ((:f f) e x) ((:f f) x))]))
(defn apply-dyadic [e f x y]
  [e (if (:pass-global-env f) ((:f f) e x y) ((:f f) x y))])



(defn resolve-adverbed [e x]
  (if (contains? x :lhs) ; eval must be right to left!
    (if (contains? x :rhs)
      (let [[e2 r] (kresolve e  (:rhs x))
            [e3 o] (kresolve e2 (:verb x))
            [e4 l] (kresolve e3 (:lhs x))
            m      (adverbs (keyword (:adverb x)))]
        [e4 ((m o) e l r)])
      (err "nyi: bind lhs of adverbed expr (partial)"))
    (if (contains? x :rhs)
      (let [[e2 r] (kresolve e  (:rhs x))
            [e3 o] (kresolve e2 (:verb x))
            m      (adverbs (keyword (:adverb x)))]
        [e3 ((m o) e r)])
      (let [[e2 o] (kresolve e (:verb x))
            m      (adverbs (keyword (:adverb x)))]
        [e2 (merge o {:f (m o)})]))))

(defn resolve-assign [e x]
  (let [[e2 r] (kresolve e (:rhs x))]
    [(assoc e2 (keyword (:id x)) r) r]))

(defn resolve-at [e x]
  (if (:rhs x)
    (let [[e2 rhs] (kresolve e (:rhs x))]
      (if (:lhs x)
        (let [[e3 lhs] (kresolve e2 (:lhs x))]
          (apply-monadic e3 lhs rhs))
        (at e2 rhs)))
    (if (:lhs x)
      (err "nyi: partially bound @ from lhs")
      [e (ops :at)])))

(defn resolve-call [e x]
  (loop [e e a (reverse (:actuals x)) r ()]
    (if (empty? a) ;; TODO: partial
      (let [[ne f] (kresolve e (:target x))]
        (if (or (vector? f) (:k f))
          (if (= 1 (count r))
            [ne (index f (first r))]
            [ne (index f r)])
          (invoke ne f r)))
      (let [[ne rr] (kresolve e (first a))]
        (recur ne (next a) (cons rr r))))))

(defn resolve-dyop [e x]
  (let [o        (ops (keyword (:op x)))
        [e2 rhs] (kresolve e (:rhs x))
        [e3 lhs] (kresolve e2 (:lhs x))]
    (apply-dyadic e3 o lhs rhs)))

(defn resolve-juxt [e x]
  (let [[e2 rhs] (kresolve e (:rhs x))
        [e3 o] (kresolve e2 (:lhs x))]
    (if (:second rhs)
      (invoke e3 rhs [o (:second rhs)])
      (if (can-be-dyadic o)
        [e3 (merge o {:second rhs})]
        (apply-monadic e3 o rhs)))))

(defn resolve-lambda [e x]
  (let [a (args x)
        w (merge x {:formals a
                    :pass-global-env true
                    :rank [(count a)]})
        f (feval w)]
    [e (assoc w :f f)]))

(defn resolve-list [e x]
  (let [p (reverse x)]
    (loop [e e i p r ()]
      (if (empty? i)
        [e (vec r)]
        (let [[ne nr] (kresolve e (first i))]
          (recur ne (next i) (cons nr r)))))))

(defn resolve-monop [e x]
  (let [o (ops (keyword (:op x)))
        [e2 a] (kresolve e (:rhs x))]
    (apply-monadic e2 o a)))

(defn sub-table [t i]
  {:id (fn [x] (if (some #{(keyword x)} (:k t))
                 [:at [:lhs [:id x]] [:rhs [:raw i]]]
                 [:id x]))})

(defn apply-constraints [env t w]
  (if w
    (loop [[e i] (apply where (kresolve env (first w)))
           c     (next w)]
      (if (empty? c)
        [e i]
        (let [[e2 j] (apply where
                            (kresolve e (insta/transform (sub-table t i)
                                                         (first c))))]
          (recur [e2 (index i j)] (next c)))))
    [e nil]))

(defn guess-col [x]
  (cond (empty? x)        :x
        (= :id (first x)) (keyword (second x))
        :else (last (map guess-col (next x)))))

(defn add-to-dict [d k v]
  (assoc d :k (conj (:k d) k) :v (conj (:v d) v)))

(defn resolve-select [e x]
  (let [[e2 t] (kresolve e (:from x))
        [e3 i] (apply-constraints (merge e2 (zipmap (:k t) (:v t)))
                                  t
                                  (:where x))]
    (if-let [s (:aggs x)]
      (loop [e e3 a s r {:t true :k [] :v []}]
        (if (empty? a)
          [e r]
          (let [p       (first a)
                cn      (if (= :assign (first p))
                          (keyword (second (second p)))
                          (guess-col p))
                [e4 rr] (kresolve e (insta/transform (sub-table t i) (first a)))
                cd      (if (coll? rr) rr [rr])]
            (recur e (next a) (add-to-dict r cn cd)))))
      [e3 (if i (index t i) t)])))

(defn resolve-table [e x]
  (let [b (:cols x)
        c (map (comp map-from-tuples next)
               (if (= :col (first b)) [b] b))]
    (loop [e e a (reverse (map :rhs c)) r ()]
      (if (empty? a)
        (if (apply = (map count r))
          [e (t-from-d (mkdict (mapv #(keyword (:id %)) c)
                               (vec r)))]
          (err "length"))
        (let [[ne rr] (kresolve e (first a))]
          (recur ne (next a) (cons rr r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kresolve [e x]
  (let [t (if (coll? x) (first x) x)
        v (if (coll? x)
            (cond (= :list t)          (next x)
                  (= :expr t)          (second x)
                  (= :raw t)           (second x)
                  (vector? (second x)) (map-from-tuples (next x))
                  (= 2 (count x))      (second x)
                  :else                (next x))
            nil)]
    (cond (= t :adverbed) (resolve-adverbed e v)
          (= t :assign  ) (resolve-assign e v)
          (= t :at    )   (resolve-at e v)
          (= t :bool    ) [e (= "1" v)]
          (= t :bools   ) [e (mapv #(= \1 %) v)]
          (= t :call    ) (resolve-call e v)
          (= t :char    ) [e (char (first v))]
          (= t :chars   ) [e v]
          (= t :dyop    ) (resolve-dyop e v)
          (= t :empty   ) [e []]
          (= t :expr    ) (kresolve e v)
          (= t :float   ) [e (parse-double v)]
          (= t :floats  ) [e (mapv parse-double (str/split v #"[ \n\r\t]+"))]
          (= t :hole    ) [e :hole] ;; nil ?
          (= t :id      ) [e (e (keyword v))]
          (= t :juxt    ) (resolve-juxt e v)
          (= t :lambda  ) (resolve-lambda e v)
          (= t :list    ) (resolve-list e v)
          (= t :long    ) [e (parse-long v)]
          (= t :longs   ) [e (mapv parse-long (str/split v #"[ \n\r\t]+"))]
          (= t :monop   ) (resolve-monop e v)
          (= t :op      ) [e (ops (keyword v))]
          (= t :raw     ) [e v]
          (= t :select  ) (resolve-select e v)
          (= t :symbol  ) [e (keyword v)]
          (= t :symbols ) [e (mapv keyword (next (str/split v #"`")))]
          (= t :table   ) (resolve-table e v)
          :else           [e x])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def viewport {:rows 25 :cols 80})

(defn substr [s b e] (subs s b (min e (count s))))

(defn show-dict [x]
  (let [n  (min (:rows viewport) (count (:k x)))
        w  (:cols viewport)
        kw (apply max (map #(count (str %)) (take n (:k x))))
        f  (str "%" kw "s| %s")]
    (doseq [[k v] (map list (take n (:k x)) (take n (:v x)))]
      (println (substr (format f k v) 0 w)))))

(defn show-table [x]
  (let [h (mapv name (:k x))
        n (min (:rows viewport) (count (first (:v x)))) ; TODO: fix
        c (mapv #(vec (take n %)) (:v x))
        s (mapv #(mapv str %) c)
        w (mapv #(apply max (cons (count %1) (map count %2))) h s)
        f (mapv #(str "%-" (+ 1 %) "s") w)]
    (doseq [[f s] (map list f h)] (printf f s))
    (println)
    (println (str/join (replicate (+ -1 (count w) (reduce + w)) "-")))
    (doseq [i (range n)]
      (doseq [[f s] (map list f s)] (printf f (s i)))
      (println))))

(defn show [x]
  (cond (vector? x) (println x) ; TODO - distinguish uniform vs mixed (use meta?)
        (map? x)    (if (:t x) (show-table x) (show-dict x))
        :else       (println x)))

(defn repl []
  (println "Welcome to qiss: short and simple.")
  (loop [e builtin] ; env
    (do ; (print "e ") (println e)
;        (print "\u00b3)") (flush))
        (print "\u00a7)") (flush))
    (if-let [line (read-line)]
      (if (or (empty? line) (= \/ (first line))) ; skip comments
        (recur e)
        (if (and (not= "\\\\" line) (not= "exit" line))
          (let [e2 (try
                     (let [x (second (parse line))
                           [ne r] (kresolve e x)]
                       (if (not= :assign (first x))
                         (show r))
                       ne)
                     (catch Exception ex
                       (println ex)
                       e))]
              (recur e2)))))))

(defn keval [x] (last (kresolve builtin (second (parse x)))))
(defn krun [x]  (show (keval x)))

(defn -main
  "qiss repl"
  [& args]
  (repl))

(facts "about bools"
       (fact "bools eval to themselves"
             (keval "1b") => true
             (keval "0b") => false))
(facts "about simple bool list literals"
       (fact "compact"
             (keval "1001001b") => [true false false true false false true]))
(facts "about chars"
       (facts "chars eval to themselves"
              (keval "\"a\"") => \a))
(facts "about simple char list literals"
       (fact "char list literals eval to clojure strings"
             (keval "\"abc\"") => "abc"))
(facts "about floats"
       (fact "floats eval to themselves"
             (keval "1.") => 1.0
             (keval "-1.") => -1.0))
(facts "about float list literals"
       (fact "no punctuation"
             (keval "1.4 2.5 3.6") => [1.4 2.5 3.6])
       (fact "any float promotes the whole list"
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
(facts "about simple long list literals"
       (fact "no puncuation"
             (keval "1 2 3") => [1 2 3])
       (fact "monadic - sticks to literals"
             (keval "1 -2 3") => [1 -2 3])
       (fact "spaces may be added for formatting"
             (keval "1    2     3") => [1 2 3]))
(facts "about mixed lists"
       (fact "top-level indexing"
             (keval "(`a`b`c;1)0") => [:a :b :c]))
(facts "about !"
       (fact "monadic ! on a long is til"
             (keval "!3") => [0 1 2])
       (fact "monadic ! on a dict is key"
             (keval "!`a`b`c!1 2 3") => [:a :b :c]))
(facts "about +"
       (fact "+ is atomic"
             (keval "1+10 20") => [11 21]
             (keval "1 2+10") => [11 12]
             (keval "1 2+10 20") => [11 22]))
(facts "about symbols"
       (fact "symbols eval to clojure keywords"
             (keval "`abc") => :abc))
(facts "about symbol list literals"
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
             (keval "<`d`d`e`c`c`b`d`e`e`b") => [5 9 3 4 0 1 6 2 7 8]))
(facts "about >"
       (fact "monadic > is idesc"
             (keval ">`d`d`e`c`c`b`d`e`e`b") => [2 7 8 0 1 6 3 4 5 9]))
(facts "about right-to-left"
       (fact "no operator precedence"
             (keval "10*2+3") => 50))
(facts "about adverbs"
       (fact "they can be monadic"
             (keval "+/1 2 3") => 6)
       (fact "they can be dyadic"
             (keval "0+/1 2 3") => 6)
       (fact "/: does"
             (keval "1 2 3*/:1 2") => [[1 2 3] [2 4 6]]))
;;       (fact "they can be compounded"  TODO: fix
;;             (keval ",//(1 2 3;(4 5 6;7 8 9))") => [1 2 3 4 5 6 7 8 9]))
(facts "about calling functions"
       (fact "supplying all arguments causing invocation"
             (keval "div[10;3]") => 3))
(facts "about indexing at depth"
       (fact "2-d list"
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
       (fact "symbol list literals followed by longs"
             (keval "`a`b`c`d`e 1") => :b
             (keval "`a`b`c`d`e 1 2 3") => [:b :c :d])
       (fact "dyadic user-defined functions can be used infix"
             (keval "`a`b`c`d`e{x y}1 2 3") => [:b :c :d]))
(facts "about indexing"
       (fact "square brackets no semicolons"
             (keval "1 2 3 4[0 2]") => [1 3]))
(facts "about join"
       (fact "monadic enlists"
             (keval ",1") => [1]
             (keval ",1 2 3") => [[1 2 3]])
       (fact "dyadic joins"
             (keval "1 2,3 4") => [1 2 3 4]))
(facts "about where"
       (fact "works with bools"
             (keval "&1001b") => [0 3])
       (fact "works with longs"
             (keval "&0 1 2 3") => [1 2 2 3 3 3]))

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
       (fact "bool list literals"
             (count (parses "010b")) => 1
             (count (parses "1001b")) => 1)
       (fact "call"
             (count (parses "`a`b`c[0]")) => 1
             (count (parses "`a`b`c[0 1]")) => 1
             (count (parses "{x}[0]")) => 1)
       (fact "chars"
             (count (parses "\"a\"")) => 1
             (count (parses "\"\\\"\"")) => 1)
       (fact "char list lterals"
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
       (fact "float list literals"
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
       (fact "long list literals"
             (count (parses "1 2 3")) => 1
             (count (parses "1    2    3")) => 1)
       (fact "monop"
             (count (parses "*1 2 3")) => 1)
       (fact "symbols"
             (count (parses "`a")) => 1)
       (fact "symbol list literals"
             (count (parses "`a`b`c`d`e")) => 1))
       ;; gave up on this one: couldn't fix the <exprx> rule
       ;; (fact "select"
       ;;       (count
       ;;        (parses
       ;;         "select +/a,+/b from([]a:,/3#/:1 2;b:6#10 20 30)where b<=20")) => 1))
