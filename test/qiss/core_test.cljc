(ns qiss.core-test
  #?(:clj
     (:require [clojure.test :refer :all]
               [qiss.core :refer :all]
;               [sparkling.api :as spark]
;               [sparkling.conf :as sparkconf]
               [flambo.conf :as sparkconf]
               [flambo.api :as spark]
               [flambo.tuple :as ft]
               [flambo.sql :as sparksql]))
  #?(:cljs
     (:require [cljs.test :refer-macros [deftest is run-tests testing]]
               [qiss.core :refer :all])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-db-tables
  []
  (keval "df:c sparksqlreadcsv \"iowa_liquor_sales_fixed_with_header10.csv\""))
(defn destroy-db-tables
  []
  (keval "df:()"))
(defn my-test-fixture [f]
  (load-db-tables)
  (f)
  (destroy-db-tables))

; Here we register my-test-fixture to be called once, wrapping ALL tests
; in the namespace
(use-fixtures :once my-test-fixture)

(deftest test-bool-literals
  (testing "bools eval to themselves"
    (is (= true (ktest "1b")))
    (is (= false (ktest "0b"))))
  (testing "simple bool vector literals are compact"
    (is (= [true false false true false false true] (ktest "1001001b")))))
(deftest test-tilde
  (testing "monadic ~ on an atom is not"
    (is (= false (ktest "~5")))
    (is (= true (ktest "~0b"))))
  (testing "monadic ~ on a vector nots the vector's elements"
    (is (= [false true false] (ktest "~101b")))
    (is (= [false false true] (ktest "~2 7 0"))))
  (testing "monadic ~ on a dict nots the dict's value"
    (is (= (ktest "`a`b`c!010b") (ktest "~`a`b`c!101b"))))
  (testing "monadic ~ on a table nots the table's elements"
    (is (= (ktest "([]a:010b)") (ktest "~([]a:1 0 4)"))))
  (testing "monadic ~ on a keyed table nots the keyed table's value"
    (is (= (ktest "([a:1 0 4]b:101b)") (ktest "~([a:1 0 4]b:010b)")))))
(deftest test-char-literals
  (testing "chars eval to themselves"
    (is (= \5 (ktest "\"5\"")))
    (is (= \a (ktest "\"a\""))))
  (testing "char vector literals eval to vectors of char"
    (is (= [\a \b \c] (ktest "\"abc\"")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-float-literals
  (testing "floats eval to themselves"
    (is (= 1.0 (ktest "1.")))
    (is (= -1.0 (ktest "-1."))))
  (testing "float vector literals eval to themselves"
    (is (= [1.4 2.5 3.6] (ktest "1.4 2.5 3.6")) "no punctuation")
    (is (= [1.0 2.0 3.0] (ktest "1 2.0 3")) "any float promotes the whole vector")
    (is (= [1.0 2.0 3.0] (ktest "1 2. 3")) "fractional part not needed if it is zero")
    (is (= [1.0 2.0 3.0] (ktest "1     2.    3")) "spaces may be added for formatting")))
(deftest test-long-literals
  (testing "longs eval to themselves"
    (is (= 1 (ktest "1")))
    (is (= -123 (ktest "-123"))))
  (testing "simple long vector literals eval to themselves"
    (is (= [1 2 3] (ktest "1 2 3")) "no puncuation")
    (is (= [1 -2 3] (ktest "1 -2 3")) "monadic - sticks to literals")
    (is (= [1 2 3] (ktest "1    2     3")) "spaces may be added for formatting")))
(deftest test-symbol-literals
  (testing "symbols eval to clojure keywords"
    (is (= :abc (ktest "`abc"))))
  (testing "symbol vector literals have no spaces"
    (is (= [:abc :def] (ktest "`abc`def")))))
;; ;; TODO             (ktest "`abc `def") => error
(deftest test-mixed-vector-literals
  (testing "(;) is a vector literal"
    (is (= [[:a :b :c] 1] (ktest "(`a`b`c;1)"))))
  (testing "top-level indexing applies to generic vector literals" ;; TODO put this where it belongs
    (is (= [:a :b :c] (ktest "(`a`b`c;1)0")))))
(deftest test-vector-literals-with-holes
  (testing "holes are maintained"
    (is (= [3 :hole] (ktest "(3;)"))))
  (testing "holes can be filled via []"
    (is (= [3 4] (ktest "(3;)[4]")))
    (is (= [4 3 5] (ktest "(;3;)[4;5]"))))
  (testing "holes can be filled via juxt"
    (is (= [3 4] (ktest "(3;)4"))))
  (testing "holes can be filled in stages"
    (is (= [4 3 :hole] (ktest "(;3;)4")))
    (is (= [4 3 5] (ktest "(;3;)[4]5")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-long-arithmetic
  (testing "longs add" ;; TODO: + in clojure promotes to bignum
    (is (= 11 (ktest "1+10")))
    (is (= -9 (ktest "1+-10")))
    (is (= 9  (ktest "-1+10"))))
  (testing "longs subtract"
    (is (= -9 (ktest "1-10")))
    (is (= 58 (ktest "100-42")))))
(deftest test-monadic-bang
  (testing "monadic ! on a long is til"
    (is (= [] (ktest "!0")))
    (is (= [0 1 2] (ktest "!3"))))
  (testing "monadic ! on a vector is til count"
    (is (= [0 1 2] (ktest "!`a`b`c"))))
  (testing "monadic ! on a dict is key"
    (is (= [:a :b :c] (ktest "!`a`b`c!1 2 3"))))
  (testing "monadic ! on a keyed table is key"
    (is (= (ktest "([]p:`a`b`c)") (ktest "!([p:`a`b`c]q:1 2 3)")))))
(deftest test-question-mark
  (testing "container?... => find"
    (is (= 0 (ktest "(!5)?0")))
    (is (= [0 2 4] (ktest "(!5)?0 2 4")))
    (is (= [5 3 1] (ktest "(!5)?10 3 1")))
    (is (= :b (ktest "(`a`b`c!1 2 3)?2")))
    (is (= [:c :a] (ktest "(`a`b`c!1 2 3)?3 1")))
    (is (= [0 1] (ktest "([]a:1 2 3)?([]a:1 2)")))
    (is (= (ktest "([]a:`a`b)") (ktest "([a:`a`b`c]b:1 2 3)?([]b:1 2)")))))
(deftest test-atomic-plus
  (testing "+ is atomic"
    (is (= [11 21] (ktest "1+10 20")))
    (is (= [11 12] (ktest "1 2+10")))
    (is (= [11 22] (ktest "1 2+10 20"))))
  (testing "dyadic operators don't care about spaces"
    (is (= 3 (ktest "1 +2")))
    (is (= 3 (ktest "1+ 2")))
    (is (= 3 (ktest "1 + 2")))))
(deftest test-monadic-less
  (testing "monadic < is iasc"
    (is (= [5 9 3 4 0 1 6 2 7 8] (ktest "<`d`d`e`c`c`b`d`e`e`b")))
    (is (= (ktest "`b`d`f`c`e`g`i`h`a`j")
           (ktest "<`a`b`c`d`e`f`g`h`i`j!4 0 2 1 2 1 2 3 2 4")))))
(deftest test-monadic-greater
  (testing "monadic > is idesc"
    (is (= [2 7 8 0 1 6 3 4 5 9] (ktest ">`d`d`e`c`c`b`d`e`e`b")))))
(deftest test-right-to-left
  (testing "no operator precedence"
    (is (= 50 (ktest "10*2+3")))))
(deftest test-dyadic-pound
  (testing "n#container => take 1st n"
    (is (= [0 1 2] (ktest "3#!5")))
    (is (= (ktest "`a`b!1 2") (ktest "2#`a`b`c!1 2 3")))
    (is (= (ktest "([]a:`a`b;b:1 2)") (ktest "2#([]a:`a`b`c`d;b:1 2 3 4)")))
    (is (= (ktest "([]a:,`a;b:,1)") (ktest "1#([]a:`a`b`c;b:1 2 3)"))))
  (testing "(-n)# container => take last n"
    (is (= [3 4] (ktest "-2#!5")))
    (is (= (ktest "`b`c!2 3") (ktest "-2#`a`b`c!1 2 3")))
    (is (= (ktest "([]a:`c`d;b:3 4)") (ktest "-2#([]a:`a`b`c`d;b:1 2 3 4)"))))
  (testing "n#x where n>#x => overtake"
    (is (= [0 1 2 0 1] (ktest "5#!3")))
    (is (= [1 2 0 1 2] (ktest "-5#!3"))))
  (testing "m n#x (where m and n >= 0) is 2d reshape"
    (is (= [] (ktest "2 5#!0")))
    (is (= [[0 1 2 3 4] [5 6 7 8 9]] (ktest "2 5#!10")))
    (is (= [[0 1] [2 3] [4 5] [6 7] [8 9]] (ktest "5 2#!10"))))
  (testing "m n#x (where m < 0) is reshape with fixed cols and flex rows"
    (is (= [[0 1 2] [3 4 5] [6 7]] (ktest "-1 3#!8")))
    (is (= [[0 1 2]] (ktest "-1 3#!3")))
    (is (= [[0 1]] (ktest "-1 3#!2")))
    (is (= [] (ktest "-1 3#!0")))))
(deftest test-underscore
  (testing "_n is floor"
    (is (= 3 (ktest "_3.14")))
    (is (= 3 (ktest "_3.99")))
    (is (= -4 (ktest "_-3.14"))))
  (testing "n _ container => drop 1st n"
    (is (= [2 3 4] (ktest "2_!5")))
    (is (= (ktest "`c`d!3 4") (ktest "2_`a`b`c`d!1 2 3 4")))
    (is (= (ktest "([]a:`c`d;b:3 4)")
           (ktest "2_([]a:`a`b`c`d;b:1 2 3 4)")))
    (is = ((ktest "([]a:,`c;b:,3)")
           (ktest "2_([]a:`a`b`c;b:1 2 3)"))))
  (testing "(-n) _ container => drop last n"
    (is (= [0 1 2] (ktest "-2_!5")))
    (is (= (ktest "`a`b!1 2") (ktest "-2_`a`b`c`d!1 2 3 4")))
    (is (= (ktest "([]a:`a`b;b:1 2)") (ktest "-1_([]a:`a`b`c;b:1 2 3)")))
    (is (= (ktest "([a:,`c]b:,3)") (ktest "2_([a:`a`b`c]b:1 2 3)"))))
  (testing "[n0...] _ vector => cut"
    (is (= (ktest "(\"01\";\"234\";\"56\")") (ktest "0 2 5_\"0123456\""))))
  (testing "container _ index => remove at"
    (is (= [0 1 3 4] (ktest "(!5)_2")))
    (is (= (ktest "`a`b!1 2") (ktest "(`a`b`c!1 2 3)_`c")))
    (is (= (ktest "`a`c!1 3") (ktest "(`a`b`c!1 2 3)_`b")))
    (is (= (ktest "([]b:1 2 3)") (ktest "([]a:`a`b`c;b:1 2 3)_`a")))
    (is (= (ktest "([]a:`a`c;b:1 3)") (ktest "([]a:`a`b`c;b:1 2 3)_1")))))
(deftest test-adverbs
  (testing "adverbs applied to dyadic functions can be used monadically"
    (is (= 6 (ktest "+/1 2 3"))))
  (testing "adverbs applied to dyadic functions can be used dyadically"
    (is (= 6 (ktest "0+/1 2 3"))))
  (testing "/: is each-right"
    (is (= [[1 2 3] [2 4 6]] (ktest "1 2 3*/:1 2"))))
  (testing "adverbed functions can be assigned"
    (is (= 12 (ktest "{a:+/;a[0;3 4 5]}[]"))))
  (testing "' can be each-both"
    (is (= [2 4 6] (ktest "1 2 3{x+y}'1 2 3"))))
  (testing "adverbs can be stacked"
    (is (= [1 2 3 4 5 6 7 8 9] (ktest ",//(1 2 3;(4 5 6;7 8 9))")))
    (is (= [[[1 3] [1 4]] [[2 3] [2 4]]] (ktest "1 2,/:\\:3 4")))
    (is (= [[3 3] [3 3]] (ktest "#''((1 2 3;3 4 5);(5 6 7;7 8 9))")))))
(deftest test-calling-functions
  (testing "supplying all a function's arguments causing invocation"
    (is (= 3 (ktest "div[10;3]"))))
  (testing "supplying some arguments via [] creates a partial"
    (is (= 5 (ktest "{z}[3][4][5]"))))
  (testing "partials can be created and invoked with [;]"
    (is (= 10 (ktest "{z;y}[;10;][3;4]")))
    (is (= 10 (ktest "{z;y}[;10][3;4]"))))
  (testing "partials can be invoked using juxt"
    (is (= 5 (ktest "{y}[3]5")))
    (is (= 4 (ktest "(1+)3")))
    (is (= [0 1 6 3 12 5 18 7 24 9] (ktest "@[!10;2*!5;3*]")))
    (is (= (ktest "110b") (ktest "(in 1 2 3)1 2")))))
(deftest test-indexing-at-depth
  (testing "indexing a 2-d vector using [i;j] does"
    (is (= :b (ktest "(`a`b`c;1 2 3)[0;1]")))
    (is (= [:a :b] (ktest "(`a`b`c;1 2 3)[0;0 1]"))))
  (testing "eliding the first dimension when indexing a 2-d vector yields a column"
    (is (= [0 3 6] (ktest "(0 1 2;3 4 5;6 7 8)[;0]")))))
(deftest test-lambdas-implicit-args
  (testing "x,y and z are implicit args"
    (is (= 3 (ktest "{x}[3]")))
    (is (= 7 (ktest "{x+y}[3;4]")))
    (is (= 12 (ktest "{x+y+z}[3;4;5]")))))
(deftest test-juxtaposition
  (testing "lambdas juxtapose without whitespace"
    (is (= 3 (ktest "{x}3"))))
  (testing "lambdas juxtapose with whitespace"
    (is (= 3 (ktest "{x} 3"))))
  (testing "a symbol vector literal followed by longs indexes the symbols"
    (is (= :b (ktest "`a`b`c`d`e 1")))
    (is (= [:b :c :d] (ktest "`a`b`c`d`e 1 2 3"))))
  (testing "dyadic user-defined functions can be used infix"
    (is (= [:b :c :d] (ktest "`a`b`c`d`e{x y}1 2 3")))
    (is (= (ktest "110b") (ktest "1 2 4 in 1 2 3"))))
  (testing "float vector literals are valid lhs"
    (is (= (ktest "4 6.") (ktest "1 2.{x+y}3 4")))
    (is (= (ktest "01b") (ktest "1 2. in 2 4. 6")))))
(deftest test-top-level-indexing
  (testing "square brackets without semicolons means top-level indexing"
    (is (= [1 3] (ktest "1 2 3 4[0 2]"))))
  (testing "@ can do top-level indexing"
    (is (= [1 3] (ktest "1 2 3 4@0 2"))))
  (testing "juxt does top-level indexing"
    (is (= [1 3] (ktest "{x 0 2}1 2 3 4"))))
  (testing "@ can do repeated top-level indexing"
    (is (= [[1 3] [2 4]] (ktest "1 2 3 4@(0 2;1 3)")))))
(deftest test-monadic-eq-ie-group
  (testing "monadic = on a vector is group"
    (is (= (ktest "4 0 2 1 3!(0 9;,1;2 4 6 8;3 5;,7)")
           (ktest "=4 0 2 1 2 1 2 3 2 4"))))
  (testing "monadic = on a dict groups the dict's value"
    (is (= (ktest "0 1 2!(`a`d`g`j;`b`e`h;`c`f`i)")
           (ktest "=`a`b`c`d`e`f`g`h`i`j!0 1 2 0 1 2 0 1 2 0")))))
(deftest test-2-arg-at
  (testing "@ can be called like a 2-arg function using []"
    (is (= [1 3] (ktest "@[1 2 3 4;0 2]")))
    (is (= 8 (ktest "@[{x+x};4]"))))
  (testing "2-arg @ can apply monadic functions"
    (is (= 4 (ktest "{x*x}@2")))
    (is (= [0 1 2 3 4] (ktest ",/@(0 1;2 3;4)")))))
(deftest test-3-arg-at
  (testing "3-arg @ on a vector indexed with a long is selective xform"
    (is (= [0 2 2 3] (ktest "@[!4;1;{x*2}]"))))
  (testing "3-arg @ on a vector indexed with longs is selective xform"
    (is (= [0 2 2 4] (ktest "@[!4;1 3;{x+1}]"))))
  (testing "3-arg @ on a vector indexed with a matrix of longs is repeated selective xform"
    (is (= [1 3 3 3] (ktest "@[!4;(0 1;1 2);{x+1}]"))))
  (testing "3-arg @ on a dict indexed with an atom is selective xform"
    (is (= (ktest "`a`b`c!2 2 3") (ktest "@[`a`b`c!1 2 3;`a;{x+1}]"))))
  (testing "3-arg @ on a dict indexed with a vector is selective xform"
    (is (= (ktest "`a`b`c!2 2 4") (ktest "@[`a`b`c!1 2 3;`a`c;{x+1}]"))))
  (testing "3-arg @ on a dict indexed with a matrix is repeated selective xform"
    (is (= (ktest "`a`b`c!3 3 4")
           (ktest "@[`a`b`c!1 2 3;(`a`b;`a`c);{x+1}]"))))
  (testing "3-arg @ on a table indexed with long is selective xform on row"
    (is (= (ktest "([]a:2 2 3)") (ktest "@[([]a:1 2 3);0;{2*x}]"))))
  (testing "3-arg @ on a table indexed with longs is selective xform on rows"
    (is (= (ktest "([]a:2 2 6)") (ktest "@[([]a:1 2 3);0 2;{2*x}]"))))
  (testing "3-arg @ on a table indexed with symbol is selective xform on col"
    (is (= (ktest "([]a:2 4 6;b:10 20 30)")
           (ktest "@[([]a:1 2 3;b:10 20 30);`a;{2*x}]"))))
  (testing "3-arg @ on a table indexed with symbols is selective xform on cols"
    (is (= (ktest "([]a:2 4 6;b:10 20 30;c:200 400 600)")
           (ktest "@[([]a:1 2 3;b:10 20 30;c:100 200 300);`a`c;{2*x}]"))))
  (testing "3-arg @ on a table indexed with a mixed list is repeated selective xform"
    (is (= (ktest "([]a:4 4 6;b:20 20 30;c:200 200 300)")
           (ktest "@[([]a:1 2 3;b:10 20 30;c:100 200 300);(`a;0);{2*x}]")))))
(deftest test-4-arg-at
  (testing "4-arg @ on a vector indexed with a long is selective xform with rhs"
    (is (= [0 2 2 3] (ktest "@[!4;1;*;2]"))))
  (testing "4-arg @ on a vector indexed with longs is selective xform with rhs"
    (is (= [0 2 2 6] (ktest "@[!4;1 3;*;2]"))))
  (testing "4-arg @ on a vector indexed with a matrix is selective xform with rhs"
    (is (= [0 2 4 6] (ktest "@[!4;(1 3;2 0);*;2]")))
    (is (= [0 2 8 6] (ktest "@[!4;(1 3;2 0);*;2 4]")))
    (is (= [0 2 12 12] (ktest "@[!4;(1 3;2 0);*;(2 4;6 8)]"))))
  (testing "4-arg @ on a dict indexed with a ragged 2-d vector is selective xform with rhs"
    (is (= (ktest "`a`b`c!2 4 6")
           (ktest "@[`a`b`c!1 2 3;(`a`c;`b);*;2]"))))
  (testing "4-arg @ works on tables"
    (is (= (ktest "([]a:500 10 15;b:10000 200 300;c:10000 200 300)")
           (ktest "@[([]a:1 2 3;b:10 20 30;c:100 200 300);(`a`b;0);*;(5 10;100)]")))))
(deftest test-2-arg-dot
  (testing "2-arg . on a vector indexed with a vector does"
    (is (= 2 (ktest "1 2 3 4 .,1")))
    (is (= [2 3] (ktest "1 2 3 4 .,1 2")))
    (is (= [[0 1 2] [6 7 8]] (ktest "(0 1 2;3 4 5;6 7 8).,0 2")))
    (is (= 4 (ktest "(0 1 2;3 4 5;6 7 8). 1 1")))
    (is (= [1 4] (ktest "(0 1 2;3 4 5;6 7 8).(0 1;1)")))
    (is (= [[0 2] [6 8]] (ktest "(0 1 2;3 4 5;6 7 8).(0 2;0 2)"))))
  (testing "2-arg . on a vector eliding the first index yields columns"
    (is (= [[0 2] [3 5] [6 8]] (ktest "(0 1 2;3 4 5;6 7 8).(;0 2)"))))
  (testing "2-arg . indexing dicts of vectors does"
    (is (= 4 (ktest "(`a`b`c!(0 1 2;3 4 5;6 7 8)).(`b;1)")))
    (is (= [1 4] (ktest "(`a`b`c!(0 1 2;3 4 5;6 7 8)).(`a`b;1)")))
    (is (= [3 5] (ktest "(`a`b`c!(0 1 2;3 4 5;6 7 8)).(`b;0 2)"))))
  (testing "2-arg . indexing tables does"
    (is (= 5 (ktest "([]a:1 2 3;b:4 5 6).(`b;1)")))
    (is (= 5 (ktest "([]a:1 2 3;b:4 5 6).(1;`b)")))
    (is (= [2 5] (ktest "([]a:1 2 3;b:4 5 6).(`a`b;1)")))
    (is (= [[1 3] [4 6]] (ktest "([]a:1 2 3;b:4 5 6).(`a`b;0 2)"))))
  (testing "2-arg . can apply functions"
    (is (= 2 (ktest "{2}.()")))
    (is (= 2 (ktest "{x}.,2")))
    (is (= 3 (ktest "{x+y}. 1 2")))
    (is (= 3 (ktest "+. 1 2")))
    (is (= 4 (ktest "{x+z}. 1 2 3")))))
(deftest test-3-arg-dot
  (testing "3-arg . is selective xform at depth"
    (is (= (ktest "(100 101 2;103 104 5;6 7 8)")
           (ktest ".[(0 1 2;3 4 5;6 7 8);(0 1;0 1);{x+100}]")))
    (is (= (ktest "(0 1 2;103 104 105;6 7 8)")
           (ktest ".[(0 1 2;3 4 5;6 7 8);(1;0 1 2);{x+100}]")))
    (is (= (ktest "((0 1;2 3);(4 5;6 49);(8 9;10 11))")
           (ktest ".[((0 1;2 3);(4 5;6 7);(8 9;10 11));1 1 1;{x*x}]")))))
(deftest test-4-arg-dot
  (testing "4-arg . is selective xform at depth with rhs"
    (is (= (ktest "`a`b`c!(0 1 2;30 4 500;6 7 8)")
           (ktest ".[`a`b`c!(0 1 2;3 4 5;6 7 8);(`b;0 2);*;10 100]")))))
(deftest test-like
  (testing "like works on clojure strings"
    (is (like "foo.qiss" ".*\\.qiss")))
  (testing "like works on qiss strings"
    (is (like (ktest "\"foo.qiss\"") (ktest "\".*\\.qiss\""))))
  (testing "like works on qiss symbols"
    (is (like (ktest "`foo.qiss") (ktest "`.*\\.qiss")))))
(deftest test-join
  (testing "monadic , envectors"
    (is (= [1] (ktest ",1")))
    (is (= [[1 2 3]] (ktest ",1 2 3"))))
  (testing "dyadic , joins"
    (is (= [1 2 3 4] (ktest "1 2,3 4"))))
  (testing "joining dicts is a merge where rhs wins"
    (is (= (ktest "`a`b`c`e`d!1 10 20 5 30")
           (ktest "(`a`b`c`e!1 2 3 5),`b`c`d!10 20 30")))))
(deftest test-deleting-columns
  (testing "delete can delete a column"
    (is (= (ktest "([]a:1 2 3)")
           (ktest "delete b from ([]a:1 2 3;b:1 2 3)"))))
  (testing "delete can delete multiple columns"
    (is (= (ktest "([]b:1 2 3)")
           (ktest "delete a,c from ([]a:1 2 3;b:1 2 3;c:1 2 3)"))))
  (testing "delete can delete a key column"
    (is (= (ktest "([]b:1 2 3;c:1 2 3)")
           (ktest "delete a from ([a:1 2 3]b:1 2 3;c:1 2 3)"))))
  (testing "deleting only non-keycols from a keyed table preserves keycols"
    (is (= (ktest "([a:1 2 3]c:1 2 3)")
           (ktest "delete b from ([a:1 2 3]b:1 2 3;c:1 2 3)")))
    (is (= (ktest "([]a:1 2 3;c:1 2 3)")
           (ktest "delete b from ([a:1 2 3;b:1 2 3]c:1 2 3)")))))
(deftest test-deleting-rows
  (testing "delete can delete all rows"
    (is (= (ktest "([]a:())") (ktest "delete from([]a:1 2 3)"))))
  (testing "delete can delete rows specified by where"
    (is (= (ktest "([]a:1 3)") (ktest "delete from([]a:1 2 3)where a=2"))))
  (testing "delete can delete rows from keyed tables"
    (is (= (ktest "([a:1 3]b:1 3)")
           (ktest "delete from ([a:1 2 3]b:1 2 3)where a=2")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(deftest test-prompt-eating
;  (testing "eat the prompt on cut and pasted qiss code"
;    (is (= "2" (ktest "qiss)1+1")))))
(deftest test-select
  (testing "select does not require any agg exprs"
    (is (= (ktest "([]a:1 2 3)") (ktest "select from([]a:1 2 3)"))))
  (testing "id agg guesses result column to match original column"
    (is (= (ktest "([]a:1 2 3)") (ktest "select a from([]a:1 2 3)"))))
  (testing "col op lit correctly assigns matching column name"
    (is (= (ktest "([]a:2 3 4)") (keval "select a+1 from([]a:1 2 3)"))))
  (testing "lit op col correctly assigns matching column name"
    (is (= (ktest "([]a:2 3 4)") (keval "select 1+a from([]a:1 2 3)"))))
  (testing "select works on keyed tables"
    (is (= (ktest "([a:`a`b]b:1 2)") (ktest "select from([a:`a`b]b:1 2)")))))
(deftest test-by
  (testing "by clause does"
    (is (= (ktest "([a:`a`b`c]b:3 5 7)")
           (ktest "select +/b by a from([]a:6#`a`b`c;b:!6)"))))
  (testing "compound by clause does"
    (is (= (ktest "([a:`a`a`b`b;b:`a`b`a`b];c:(0 4;1 5;2 6;3 7))")
           (ktest "select by a,b from([]a:8#`a`a`b`b;b:8#`a`b;c:!8)")))))
(deftest test-keying-tables
  (testing "! can key a table by first n columns"
    (is (= (ktest "([a:`a`b`c]b:1 2 3)") (ktest "1!([]a:`a`b`c;b:1 2 3)")))
    (is (= (ktest "([a:`a`b;b:1 2]c:3 4")
           (ktest "2!([]a:`a`b;b:1 2;c:3 4"))))
  (testing "! can key a table by col name(s)"
    (is (= (ktest "([a:`a`b`c]b:1 2 3)") (ktest "`a!([]a:`a`b`c;b:1 2 3)")))
    (is (= (ktest "([a:`a`b;c:3 4]b:1 2") (ktest "`a`c!([]a:`a`b;b:1 2;c:3 4")))))
(deftest test-indexing-keyed-tables
  (testing "can index a keyed table with a dict"
    (is (= (ktest "`b!1") (ktest "([a:`a`b`c]b:1 2 3)`a!`a"))))
  (testing "can index a keyed table with just the value of a dict"
    (is (= (ktest "`b!3") (ktest "([a:`a`b`c]b:1 2 3)`c"))))
  (testing "can be done with a table"
    (is (= (ktest "([]b:1 3)") (ktest "([a:`a`b`c]b:1 2 3)([]a:`a`c)")))))
(deftest test-where
  (testing "monadic & turns bools into indexes that were true"
    (is (= [0 3] (ktest "&1001b"))))
  (testing "monadic & turns longs into copies of indexes"
    (is (= [1 2 2 3 3 3] (ktest "&0 1 2 3")))))
(deftest test-lj
  (testing "lj uses rhs key col"
    (is (= (ktest "([]a:`a`b`c;b:1 2 3)")
           (ktest "([]a:`a`b`c)lj([a:`a`b`c]b:1 2 3)"))))
  (testing "lj works with multiple key cols on the rhs"
    (is (= (ktest "([]a:`a`b`c;b:1 2 3;c:10 20 30)")
           (ktest "([]a:`a`b`c;b:1 2 3)lj([a:`a`b`c;b:1 2 3]c:10 20 30)")))))
(deftest test-xasc
  (testing "xasc can sort on one col"
    (is (= (ktest "([]a:`a`b`c;b:3 2 1)")
           (ktest "`a xasc([]a:`c`b`a;b:1 2 3)"))))
  (testing "xasc can sort on two cols"
    (is (= (ktest "([]a:`a`a`b`b`c`c;b:3 20 10 30 2 3)")
           (ktest "`a`b xasc([]a:`c`b`a`a`b`c;b:3 10 3 20 30 2)"))))
  (testing "xasc works on keyed tables"
    (is (= (ktest "([a:`a`a`b`b`c`c];b:3 20 10 30 2 3)")
           (ktest "`a`b xasc([a:`c`b`a`a`b`c]b:3 10 3 20 30 2)")))))
(deftest test-destructuring-function-arguments
  (testing "vector destructuring can use vector literal syntax"
    (is (= 2 (ktest "{[(x;y)]y}1 2")))
    (is (= 3 (ktest "{[(x;y);z]z}[1 2;3]")))
    (is (= 3 (ktest "{[x;(y;z)]z}[1;2 3]"))))
  (testing "vector destructuring can use whitespace"
    (is (= 2 (ktest "{[x y]y}1 2")))
    (is (= 3 (ktest "{[x y;z]z}[1 2;3]")))
    (is (= 3 (ktest "{[x;y z]z}[1;2 3]")))
    (is (= [1 1 2 3 5 8 13 21 34 55] (ktest "*'0 1{[x y;z]y,x+y}\\!10"))))
  (testing "dictionary destructuring uses !"
    (is (= [3 4] (ktest "{[x!y]y}`a`b!3 4")))
    (is (= [3 4] (ktest "{[x!y;z]y}[`a`b!3 4;5]")))
    (is (= [:a :b] (ktest "{[x;y!z]y}[6;`a`b!3 4]"))))
  (testing "dictionary destructuring works on keyed tables"
    (is (= (ktest "([]q:1 2 3)") (ktest "{[k!v]v}([p:`a`b`c]q:1 2 3)"))))
  (testing "table destructuring can use table literal syntax"
    (is (= [1 2 3] (ktest "{[([]a;b)]b}([]p:`a`b`c;q:1 2 3)"))))
  (testing "table destructuring can use spaces instead of ;"
    (is (= [1 2 3] (ktest "{[([]a b)]b}([]p:`a`b`c;q:1 2 3)"))))
  (testing "keyed tables can be destructured in function arguments"
    (is (= [1 2 3] (ktest "{[([a]b c)]c}([p:`a`b`c]q:`d`e`f;r:1 2 3)"))))
  (testing "destructuring can use _ for values you don't need"
    (is (= 2 (ktest "{[_ _ a _ _]a}@!5"))))
  (testing "destructuring can be incomplete"
    (is (= 3 (ktest "{[x y z]z}1 2 3 4")))
    (is (= 0 (ktest "{[a b!c d]c}`a`b`c`d!!4")))
    (is (= 2 (ktest "{[_ _ a]a}@!5"))))
  (testing "destructuring is recursive"
    (is (= 3 (ktest "{[(a b;c d)]c}(1 2;3 4)")))
    (is (= 1 (ktest "{[a b!c d]c}`a`b!1 2")))
    (is (= 2 (ktest "{[_!_ a b]a*b}`a`b`c`d`e!!5")))
    (is (= [1 2 3] (ktest "{[([]a)!([]b)]b}([p:`a`b`c]q:1 2 3)")))))
(deftest test-destructuring-assignments
  (testing "vector destructuring can use vector literal syntax"
    (is (= 2 (ktest "{(a;b):1 2;b}[]"))))
  (testing "vector destructuring can use whitespace but parens are required"
    (is (= 2 (ktest "{(a b):1 2;b}[]"))))
  (testing "dictionary destructuring requires parens"
    (is (= [:a :b] (ktest "{(a!b):`a`b!1 2;a}[]"))))
  (testing "table destructuring can use table literal syntax"
    (is (= [1 2 3] (ktest "{[]([]a;b):([]p:`a`b`c;q:1 2 3);b}[]"))))
  (testing "table destructuring can use spaces instead of ;"
    (is (= [1 2 3] (ktest "{[]([]a b):([]p:`a`b`c;q:1 2 3);b}[]"))))
  (testing "keyed tables can be destructured in assignments"
    (is (= [1 2 3] (ktest "{[]([a]b):([p:`a`b`c]q:1 2 3);b}[]")))))
(deftest test-parsing-non-ambiguity
  (testing "adverbed"
    (is (= 1 (count (parses "f/"))))
    (is (= 1 (count (parses "f/1 2 3"))))
    (is (= 1 (count (parses "0 f/1 2 3"))))
    (is (= 1 (count (parses "0 f//1 2 3"))))
    (is (= 1 (count (parses "f//1 2 3")))))
  (testing "assign"
    (is (= 1 (count (parses "a:3"))))
    (is (= 1 (count (parses "a:`a`b`c`d`e 1 2 3")))))
  (testing "bools"
    (is (= 1 (count (parses "0b"))))
    (is (= 1 (count (parses "1b")))))
  (testing "bool vector literals"
    (is (= 1 (count (parses "010b"))))
    (is (= 1 (count (parses "1001b")))))
  (testing "call"
    (is (= 1 (count (parses "`a`b`c[0]"))))
    (is (= 1 (count (parses "`a`b`c[0 1]"))))
    (is (= 1 (count (parses "{x}[0]")))))
  (testing "chars"
    (is (= 1 (count (parses "\"a\""))))
    (is (= 1 (count (parses "\"\\\"\"")))))
  (testing "char vector lterals"
    (is (= 1 (count (parses "\"abc\""))))
    (is (= 1 (count (parses "\"abc\\\"\"def")))))
  (testing "dyop"
    (is (= 1 (count (parses "1+2"))))
    (is (= 1 (count (parses "1 +2"))))
    (is (= 1 (count (parses "1+ 2"))))
    (is (= 1 (count (parses "1 + 2")))))
  (testing "floats"
    (is (= 1 (count (parses "1."))))
    (is (= 1 (count (parses "-1.")))))
  (testing "float vector literals"
    (is (= 1 (count (parses "1. 2 3"))))
    (is (= 1 (count (parses "1 2. 3"))))
    (is (= 1 (count (parses "-1 2. 3"))))
    (is (= 1 (count (parses "1    2.    3")))))
  (testing "ids"
    (is (= 1 (count (parses "x"))))
    (is (= 1 (count (parses "xyzzy")))))
  (testing "juxt"
    (is (= 1 (count (parses "`a`b`c`d`e 1"))))
    (is (= 1 (count (parses "`a`b`c`d`e 1 2 3"))))
    (is (= 1 (count (parses "x 1"))))
    (is (= 1 (count (parses "x 1+2"))))
    (is (= 1 (count (parses "{x}3"))))
    (is (= 1 (count (parses "1{y}3"))))
    (is (= 1 (count (parses "1{y}"))))) ; bind 1st arg
  (testing "lambdas"
    (is (= 1 (count (parses "{}"))))
    (is (= 1 (count (parses "{x}"))))
    (is (= 1 (count (parses "{[a]a}"))))
    (is (= 1 (count (parses "`a`b`c{x y}0 1 2")))))
  (testing "longs"
    (is (= 1 (count (parses "1")))))
  (testing "long vector literals"
    (is (= 1 (count (parses "1 2 3"))))
    (is (= 1 (count (parses "1    2    3")))))
  (testing "monop"
    (is (= 1 (count (parses "*1 2 3")))))
  (testing "symbols"
    (is (= 1 (count (parses "`a")))))
  (testing "symbol vector literals"
    (is (= 1 (count (parses "`a`b`c`d`e"))))))
(deftest streams-test
  (testing "atomic ops work on streams"
    (is (= [0 2 4] (ktest "<=2*>=!3")))
    (is (= [[0 0 0] [1 2 3] [2 4 6]] (ktest "<=1 2 3*>=!3"))))
  (testing "user-defined functions work on streams"
    (is (= [0 1 4] (ktest "<={x*x}@>=!3")))
    (is (= [0 2 6] (ktest "<={x+x*x}@>=!3"))))
  (testing "first works on streams"
    (is (= 0 (ktest "<=*>=!3")))
    (is (= 0 (ktest "<={*x}@>=!3"))))
  (testing "take on a stream creates a stream that stops after n events"
    (is (= [] (ktest "<=0#>=!3")))
    (is (= [0 1] (ktest "<=2#>=!3")))
    (is (= [0 1] (ktest "<={2#x}@>=!3"))))
  (testing "take from the back on a stream works via a new stream"
    (is (= [2] (ktest "<=-1#>=!3")))
    (is (= [0 1 2] (ktest "<=-3#>=!3"))))
  (testing "overtake from a stream works"
    (is (= [0 1 2 0 1] (ktest "<=5#>=!3")))
    (is (= [1 2 0 1 2] (ktest "<=-5#>=!3"))))
  (testing "taking a box (reshaping) from a stream works"
    (is (= [[0 1 2] [3 4 5] [6 7 8]] (ktest "<=3 3#>=!10")))
    (is (= [[0 1 2] [3 4 5] [6 7 8] [9]] (ktest "<=-1 3#>=!10"))))
  (testing "drop from a stream does"
    (is (= [1 2] (ktest "<=1_>=!3")))
    (is (= [1 2] (ktest "<={1_x}@>=!3")))
    (is (= [] (ktest "<=3_>=!3")))
    (is (= [] (ktest "<=4_>=!3"))))
  (testing "drop from the back"
    (is (= [0 1] (ktest "<=-1_>=!3")))
    (is (= [] (ktest "<=-3_>=!3")))
    (is (= [] (ktest "<=-4_>=!3"))))
  (testing "atom,stream works"
    (is (= [4] (ktest "<=4,>=!0")))
    (is (= [4 0 1 2] (ktest "<=4,>=!3"))))
  (testing "stream,atom works"
    (is (= [4] (ktest "<=(>=()),4")))
    (is (= [0 1 2 4] (ktest "<=(>=!3),4"))))
  (testing "vector,stream works"
    (is (= [] (ktest "<=(),>=()")))
    (is (= [0 1] (ktest "<=0 1,>=()")))
    (is (= [2 3] (ktest "<=(),>=2 3")))
    (is (= [0 1 2 3] (ktest "<=0 1,>=2 3"))))
  (testing "stream,vector works"
    (is (= [] (ktest "<=(>=!0),!0")))
    (is (= [0 1] (ktest "<=(>=0 1),()")))
    (is (= [2 3] (ktest "<=(>=!0),2 3")))
    (is (= [0 1 2 3] (ktest "<=(>=!2),2 3"))))
  (testing "stream,stream is concatAll"
    (is (= [] (ktest "<=(>=!0),>=!0")))
    (is (= [0 1] (ktest "<=(>=!2),>=!0")))
    (is (= [2 3] (ktest "<=(>=!0),>=2 3")))
    (is (= [0 1 2 3] (ktest "<=(>=!2),>=2 3")))
    (is (= [0 1 10 20 100 200] (ktest "<=(>=!2),(>=10 20),>=100 200"))))
  (testing "vector literals with stream components act like nested containers"
    (is (= [1 [0 1 2]] (ktest "<='(1;>=!3)"))))
  (testing "indexing with @ with a stream on the rhs"
    (is (= [0 2 4] (ktest "<=(!5)@>=0 2 4"))))
  (testing "indexing via @ with a stream on the lhs"
    (is (= [2 3] (ktest "<=(>=(0 2 4;1 3 5))@1"))))
  (testing "indexing via juxt with a stream on the lhs"
    (is (= [2 3] (ktest "<=(>=(0 2 4;1 3 5))1"))))
  (testing "indexing with . with a stream on the rhs"
    (is (= [1 3] (ktest "<=(0 1;2 3).>=(0 1;1 1)"))))
  (testing "indexing with . with a stream on the lhs"
    (is (= [1 5] (ktest "<=(>=((0 1;2 3);(4 5;6 7))). 0 1"))))
  (testing "@& can filter on a stream"
    (is (= [0 2] (ktest "<={x@&0=x mod 2}@>=!3"))))
  (testing "multiple streams in the same computation works"
    (is (some #{(ktest "<=(>=!3)*>=!3")}
              [[0 2 4] [0 1 2 4] [0 0 2 4] [0 0 1 2 4] [0 0 0 2 4]])))
  (testing "/: over a stream works"
    (is (= [[0 0] [1 1]] (ktest "<=2#/:>=!2"))))
  (testing "\\: over a stream works"
    (is (= [[] [3] [3 2] [3 2 1]] (ktest "<=(>=!4)#\\:3 2 1"))))
  (testing "/ over a stream works"
    (is (= 6 (ktest "<=+/>=!4")))
    (is (= 9 (ktest "<=3+/>=!4")))
    (is (= [0 1 2 3 4] (ktest "<=,/>='(0 1;2 3;4)"))))
  (testing "\\ over a stream works"
    (is (= [0 1 3 6] (ktest "<=+\\>=!4")))
    (is (= [3 4 6 9] (ktest "<=3+\\>=!4")))
    (is (= [[0] [0 1] [0 1 2]] (ktest "<=,\\!3"))))
    ;; ,\ over multiple streams doesn't do what you want.  maybe.
  (testing "': over a stream works"
    (is (= [3 7 4 6] (ktest "<=-':>=3 10 14 20")))
    (is (= [2 7 4 6] (ktest "<=1-':>=3 10 14 20")))))
;; ;; gave up on this one: couldn't fix the <exprx> rule
;; ;; (testing "select"
;; ;;       (is (= 1 (count
;; ;;        (parses
;; ;;         "select +/a,+/b from([]a:,/3#/:1 2;b:6#10 20 30)where b<=20"))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(deftest spark-configuration-and-context
;(let [conf (-> (sparkconf/spark-conf)
;               (sparkconf/set-sparkling-registrator)
;               (sparkconf/set "spark.kryo.registrationRequired" "true")
;               (sparkconf/master "local[*]")
;               (sparkconf/app-name "qiss-spark-test"))]
;  (spark/with-context sc conf
;                      (testing
;                        "Spark configuration object instantiation"
;                        (is (= org.apache.spark.SparkConf
;                               (class (keval "sparkconf[\"local[*]\";\"qiss-spark-test-2\"]")))))
;                      (testing
;                        "Spark context start, stop"
;                        (is (= nil
;                               (keval "sparkstop sparkcontext sparkconf[\"local[*]\";\"qiss-spark-test-start-stop\"]")))))
;  ))


;; spark tests
;(deftest spark-configuration-and-context
;  (testing
;    "Spark configuration object instantiation"
;    (is (= org.apache.spark.SparkConf
;           (class (keval "sparkconf[\"local[*]\";\"qiss-spark-test-2\"]"))))))
;(testing
;  "Spark context start, stop"
;  (is (= nil
;         (keval "sparkstop sparkcontext sparkconf[\"local[*]\";\"qiss-spark-test-start-stop\"]")))) )
;



;(deftest spark-test
;  (testing
;    ""
;    (is (= 3
;     (keval "sparkcount sparkcollect sparkfilter[  sc sparkparallelize (0;1;2;3;4;5) ; {0=x mod 2} ]\n")))))

(deftest qiss-sparkql-select-function-test
  (testing "select all cols from dataframe"
    (is (= (keval "df") (keval "select from df"))))
;;  (testing "select a single column from dataframe"  ; TODO - make sparksqlselect() single column select is broken, multiple works below
;;    (is (keval "sparksqlselect[df;\"ZIPCODE\"] ~ select ZIPCODE from df")))
  (testing "select a single column from dataframe via selectExpr"
    (is (keval "sparksqlselectexpr[df;\"ZIPCODE\"] ~ select ZIPCODE from df")))
  (testing "group a single column by ZIPCODE from dataframe, apply average"
    (is (keval "sparksqlgroupeddataagg[ sparksqlgroupby[df; \"ZIPCODE\"]; \"ITEM\"; \"avg\" ] ~ (select avg(ITEM) by ZIPCODE from df)")))
  (testing "group a single column by ZIPCODE from dataframe, apply max"
    (is (keval "sparksqlgroupeddataagg[ sparksqlgroupby[df; \"ZIPCODE\"]; \"ITEM\"; \"max\" ] ~ (select max(ITEM) by ZIPCODE from df)")))
  ;;  (testing "select multiple columns from dataframe"
  ;;    (is (keval "sparksqlselect[df;\"ZIPCODE\"; \"NAME\"] ~ select ZIPCODE, NAME from df")))
  ;;  (testing "select multiple column from dataframe via selectExpr"
  ;;    (is (keval "sparksqlselectexpr[df;\"ZIPCODE\"; \"NAME\"] ~ select ZIPCODE, NAME from df")))
  )