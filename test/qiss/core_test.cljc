(ns qiss.core-test
  #?(:clj  (:require [clojure.test :refer :all]
                     [qiss.core :refer :all]))
  #?(:cljs (:require [cljs.test :refer-macros [deftest is run-tests testing]]
                     [qiss.core :refer :all])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-bool-literals
  (testing "bools eval to themselves"
    (is (= true (keval "1b")))
    (is (= false (keval "0b"))))
  (testing "simple bool vector literals are compact"
    (is (= [true false false true false false true] (keval "1001001b")))))
(deftest test-tilde
  (testing "monadic ~ on an atom is not"
    (is (= false (keval "~5")))
    (is (= true (keval "~0b"))))
  (testing "monadic ~ on a vector nots the vector's elements"
    (is (= [false true false] (keval "~101b")))
    (is (= [false false true] (keval "~2 7 0"))))
  (testing "monadic ~ on a dict nots the dict's value"
    (is (= (keval "`a`b`c!010b") (keval "~`a`b`c!101b"))))
  (testing "monadic ~ on a table nots the table's elements"
    (is (= (keval "([]a:010b)") (keval "~([]a:1 0 4)"))))
  (testing "monadic ~ on a keyed table nots the keyed table's value"
    (is (= (keval "([a:1 0 4]b:101b)") (keval "~([a:1 0 4]b:010b)")))))
(deftest test-char-literals
  (testing "chars eval to themselves"
    (is (= \5 (keval "\"5\"")))
    (is (= \a (keval "\"a\""))))
  (testing "char vector literals eval to vectors of char"
    (is (= [\a \b \c] (keval "\"abc\"")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-float-literals
  (testing "floats eval to themselves"
    (is (= 1.0 (keval "1.")))
    (is (= -1.0 (keval "-1."))))
  (testing "float vector literals eval to themselves"
    (is (= [1.4 2.5 3.6] (keval "1.4 2.5 3.6")) "no punctuation")
    (is (= [1.0 2.0 3.0] (keval "1 2.0 3")) "any float promotes the whole vector")
    (is (= [1.0 2.0 3.0] (keval "1 2. 3")) "fractional part not needed if it is zero")
    (is (= [1.0 2.0 3.0] (keval "1     2.    3")) "spaces may be added for formatting")))
(deftest test-long-literals
  (testing "longs eval to themselves"
    (is (= 1 (keval "1")))
    (is (= -123 (keval "-123"))))
  (testing "simple long vector literals eval to themselves"
    (is (= [1 2 3] (keval "1 2 3")) "no puncuation")
    (is (= [1 -2 3] (keval "1 -2 3")) "monadic - sticks to literals")
    (is (= [1 2 3] (keval "1    2     3")) "spaces may be added for formatting")))
(deftest test-symbol-literals
  (testing "symbols eval to clojure keywords"
    (is (= :abc (keval "`abc"))))
  (testing "symbol vector literals have no spaces"
    (is (= [:abc :def] (keval "`abc`def")))))
;; ;; TODO             (keval "`abc `def") => error
(deftest test-mixed-vector-literals
  (testing "(;) is a vector literal"
    (is (= [[:a :b :c] 1] (keval "(`a`b`c;1)"))))
  (testing "top-level indexing applies to generic vector literals" ;; TODO put this where it belongs
    (is (= [:a :b :c] (keval "(`a`b`c;1)0")))))
(deftest test-vector-literals-with-holes
  (testing "holes are maintained"
    (is (= [3 :hole] (keval "(3;)"))))
  (testing "holes can be filled via []"
    (is (= [3 4] (keval "(3;)[4]")))
    (is (= [4 3 5] (keval "(;3;)[4;5]"))))
  (testing "holes can be filled via juxt"
    (is (= [3 4] (keval "(3;)4"))))
  (testing "holes can be filled in stages"
    (is (= [4 3 :hole] (keval "(;3;)4")))
    (is (= [4 3 5] (keval "(;3;)[4]5")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-long-arithmetic
  (testing "longs add" ;; TODO: + in clojure promotes to bignum
    (is (= 11 (keval "1+10")))
    (is (= -9 (keval "1+-10")))
    (is (= 9  (keval "-1+10"))))
  (testing "longs subtract"
    (is (= -9 (keval "1-10")))
    (is (= 58 (keval "100-42")))))
(deftest test-monadic-bang
  (testing "monadic ! on a long is til"
    (is (= [] (keval "!0")))
    (is (= [0 1 2] (keval "!3"))))
  (testing "monadic ! on a vector is til count"
    (is (= [0 1 2] (keval "!`a`b`c"))))
  (testing "monadic ! on a dict is key"
    (is (= [:a :b :c] (keval "!`a`b`c!1 2 3"))))
  (testing "monadic ! on a keyed table is key"
    (is (= (keval "([]p:`a`b`c)") (keval "!([p:`a`b`c]q:1 2 3)")))))
(deftest test-question-mark
  (testing "container?... => find"
    (is (= 0 (keval "(!5)?0")))
    (is (= [0 2 4] (keval "(!5)?0 2 4")))
    (is (= [5 3 1] (keval "(!5)?10 3 1")))
    (is (= :b (keval "(`a`b`c!1 2 3)?2")))
    (is (= [:c :a] (keval "(`a`b`c!1 2 3)?3 1")))
    (is (= [0 1] (keval "([]a:1 2 3)?([]a:1 2)")))
    (is (= (keval "([]a:`a`b)") (keval "([a:`a`b`c]b:1 2 3)?([]b:1 2)")))))
(deftest test-atomic-plus
  (testing "+ is atomic"
    (is (= [11 21] (keval "1+10 20")))
    (is (= [11 12] (keval "1 2+10")))
    (is (= [11 22] (keval "1 2+10 20"))))
  (testing "dyadic operators don't care about spaces"
    (is (= 3 (keval "1 +2")))
    (is (= 3 (keval "1+ 2")))
    (is (= 3 (keval "1 + 2")))))
(deftest test-monadic-less
  (testing "monadic < is iasc"
    (is (= [5 9 3 4 0 1 6 2 7 8] (keval "<`d`d`e`c`c`b`d`e`e`b")))
    (is (= (keval "`b`d`f`c`e`g`i`h`a`j")
           (keval "<`a`b`c`d`e`f`g`h`i`j!4 0 2 1 2 1 2 3 2 4")))))
(deftest test-monadic-greater
  (testing "monadic > is idesc"
    (is (= [2 7 8 0 1 6 3 4 5 9] (keval ">`d`d`e`c`c`b`d`e`e`b")))))
(deftest test-right-to-left
  (testing "no operator precedence"
    (is (= 50 (keval "10*2+3")))))
(deftest test-dyadic-pound
  (testing "n#container => take 1st n"
    (is (= [0 1 2] (keval "3#!5")))
    (is (= (keval "`a`b!1 2") (keval "2#`a`b`c!1 2 3")))
    (is (= (keval "([]a:`a`b;b:1 2)") (keval "2#([]a:`a`b`c`d;b:1 2 3 4)")))
    (is (= (keval "([]a:,`a;b:,1)") (keval "1#([]a:`a`b`c;b:1 2 3)"))))
  (testing "(-n)# container => take last n"
    (is (= [3 4] (keval "-2#!5")))
    (is (= (keval "`b`c!2 3") (keval "-2#`a`b`c!1 2 3")))
    (is (= (keval "([]a:`c`d;b:3 4)") (keval "-2#([]a:`a`b`c`d;b:1 2 3 4)"))))
  (testing "n#x where n>#x => overtake"
    (is (= [0 1 2 0 1] (keval "5#!3")))
    (is (= [1 2 0 1 2] (keval "-5#!3")))))
(deftest test-underscore
  (testing "n _ container => drop 1st n"
    (is (= [2 3 4] (keval "2_!5")))
    (is (= (keval "`c`d!3 4") (keval "2_`a`b`c`d!1 2 3 4")))
    (is (= (keval "([]a:`c`d;b:3 4)")
           (keval "2_([]a:`a`b`c`d;b:1 2 3 4)")))
    (is = ((keval "([]a:,`c;b:,3)")
           (keval "2_([]a:`a`b`c;b:1 2 3)"))))
  (testing "(-n) _ container => drop last n"
    (is (= [0 1 2] (keval "-2_!5")))
    (is (= (keval "`a`b!1 2") (keval "-2_`a`b`c`d!1 2 3 4")))
    (is (= (keval "([]a:`a`b;b:1 2)") (keval "-1_([]a:`a`b`c;b:1 2 3)")))
    (is (= (keval "([a:,`c]b:,3)") (keval "2_([a:`a`b`c]b:1 2 3)"))))
  (testing "[n0...] _ vector => cut"
    (is (= (keval "(\"01\";\"234\";\"56\")") (keval "0 2 5_\"0123456\""))))
  (testing "container _ index => remove at"
    (is (= [0 1 3 4] (keval "(!5)_2")))
    (is (= (keval "`a`b!1 2") (keval "(`a`b`c!1 2 3)_`c")))
    (is (= (keval "`a`c!1 3") (keval "(`a`b`c!1 2 3)_`b")))
    (is (= (keval "([]b:1 2 3)") (keval "([]a:`a`b`c;b:1 2 3)_`a")))
    (is (= (keval "([]a:`a`c;b:1 3)") (keval "([]a:`a`b`c;b:1 2 3)_1")))))
(deftest test-adverbs
  (testing "adverbs applied to dyadic functions can be used monadically"
    (is (= 6 (keval "+/1 2 3"))))
  (testing "adverbs applied to dyadic functions can be used dyadically"
    (is (= 6 (keval "0+/1 2 3"))))
  (testing "/: is each-right"
    (is (= [[1 2 3] [2 4 6]] (keval "1 2 3*/:1 2"))))
  (testing "adverbed functions can be assigned"
    (is (= 12 (keval "{a:+/;a[0;3 4 5]}[]"))))
  (testing "adverbs can be stacked"
    (is (= [1 2 3 4 5 6 7 8 9] (keval ",//(1 2 3;(4 5 6;7 8 9))")))
    (is (= [[[1 3] [1 4]] [[2 3] [2 4]]] (keval "1 2,/:\\:3 4")))
    (is (= [[3 3] [3 3]] (keval "#''((1 2 3;3 4 5);(5 6 7;7 8 9))")))))
(deftest test-calling-functions
  (testing "supplying all a function's arguments causing invocation"
    (is (= 3 (keval "div[10;3]"))))
  (testing "supplying some arguments via [] creates a partial"
    (is (= 5 (keval "{z}[3][4][5]"))))
  (testing "partials can be created and invoked with [;]"
    (is (= 10 (keval "{z;y}[;10;][3;4]")))
    (is (= 10 (keval "{z;y}[;10][3;4]"))))
  (testing "partials can be invoked using juxt"
    (is (= 5 (keval "{y}[3]5")))
    (is (= 4 (keval "(1+)3")))
    (is (= [0 1 6 3 12 5 18 7 24 9] (keval "@[!10;2*!5;3*]")))
    (is (= (keval "110b") (keval "(in 1 2 3)1 2")))))
(deftest test-indexing-at-depth
  (testing "indexing a 2-d vector using [i;j] does"
    (is (= :b (keval "(`a`b`c;1 2 3)[0;1]")))
    (is (= [:a :b] (keval "(`a`b`c;1 2 3)[0;0 1]"))))
  (testing "eliding the first dimension when indexing a 2-d vector yields a column"
    (is (= [0 3 6] (keval "(0 1 2;3 4 5;6 7 8)[;0]")))))
(deftest test-lambdas-implicit-args
  (testing "x,y and z are implicit args"
    (is (= 3 (keval "{x}[3]")))
    (is (= 7 (keval "{x+y}[3;4]")))
    (is (= 12 (keval "{x+y+z}[3;4;5]")))))
(deftest test-juxtaposition
  (testing "lambdas juxtapose without whitespace"
    (is (= 3 (keval "{x}3"))))
  (testing "lambdas juxtapose with whitespace"
    (is (= 3 (keval "{x} 3"))))
  (testing "a symbol vector literal followed by longs indexes the symbols"
    (is (= :b (keval "`a`b`c`d`e 1")))
    (is (= [:b :c :d] (keval "`a`b`c`d`e 1 2 3"))))
  (testing "dyadic user-defined functions can be used infix"
    (is (= [:b :c :d] (keval "`a`b`c`d`e{x y}1 2 3")))
    (is (= (keval "110b") (keval "1 2 4 in 1 2 3"))))
  (testing "float vector literals are valid lhs"
    (is (= (keval "4 6.") (keval "1 2.{x+y}3 4")))
    (is (= (keval "01b") (keval "1 2. in 2 4. 6")))))
(deftest test-top-level-indexing
  (testing "square brackets without semicolons means top-level indexing"
    (is (= [1 3] (keval "1 2 3 4[0 2]"))))
  (testing "@ can do top-level indexing"
    (is (= [1 3] (keval "1 2 3 4@0 2"))))
  (testing "juxt does top-level indexing"
    (is (= [1 3] (keval "{x 0 2}1 2 3 4"))))
  (testing "@ can do repeated top-level indexing"
    (is (= [[1 3] [2 4]] (keval "1 2 3 4@(0 2;1 3)")))))
(deftest test-monadic-eq-ie-group
  (testing "monadic = on a vector is group"
    (is (= (keval "4 0 2 1 3!(0 9;,1;2 4 6 8;3 5;,7)")
           (keval "=4 0 2 1 2 1 2 3 2 4"))))
  (testing "monadic = on a dict groups the dict's value"
    (is (= (keval "0 1 2!(`a`d`g`j;`b`e`h;`c`f`i)")
           (keval "=`a`b`c`d`e`f`g`h`i`j!0 1 2 0 1 2 0 1 2 0")))))
(deftest test-2-arg-at
  (testing "@ can be called like a 2-arg function using []"
    (is (= [1 3] (keval "@[1 2 3 4;0 2]")))
    (is (= 8 (keval "@[{x+x};4]")))))
(deftest test-3-arg-at
  (testing "3-arg @ on a vector indexed with a long is selective xform"
    (is (= [0 2 2 3] (keval "@[!4;1;{x*2}]"))))
  (testing "3-arg @ on a vector indexed with longs is selective xform"
    (is (= [0 2 2 4] (keval "@[!4;1 3;{x+1}]"))))
  (testing "3-arg @ on a vector indexed with a matrix of longs is repeated selective xform"
    (is (= [1 3 3 3] (keval "@[!4;(0 1;1 2);{x+1}]"))))
  (testing "3-arg @ on a dict indexed with an atom is selective xform"
    (is (= (keval "`a`b`c!2 2 3") (keval "@[`a`b`c!1 2 3;`a;{x+1}]"))))
  (testing "3-arg @ on a dict indexed with a vector is selective xform"
    (is (= (keval "`a`b`c!2 2 4") (keval "@[`a`b`c!1 2 3;`a`c;{x+1}]"))))
  (testing "3-arg @ on a dict indexed with a matrix is repeated selective xform"
    (is (= (keval "`a`b`c!3 3 4")
           (keval "@[`a`b`c!1 2 3;(`a`b;`a`c);{x+1}]"))))
  (testing "3-arg @ on a table indexed with long is selective xform on row"
    (is (= (keval "([]a:2 2 3)") (keval "@[([]a:1 2 3);0;{2*x}]"))))
  (testing "3-arg @ on a table indexed with longs is selective xform on rows"
    (is (= (keval "([]a:2 2 6)") (keval "@[([]a:1 2 3);0 2;{2*x}]"))))
  (testing "3-arg @ on a table indexed with symbol is selective xform on col"
    (is (= (keval "([]a:2 4 6;b:10 20 30)")
           (keval "@[([]a:1 2 3;b:10 20 30);`a;{2*x}]"))))
  (testing "3-arg @ on a table indexed with symbols is selective xform on cols"
    (is (= (keval "([]a:2 4 6;b:10 20 30;c:200 400 600)")
           (keval "@[([]a:1 2 3;b:10 20 30;c:100 200 300);`a`c;{2*x}]"))))
  (testing "3-arg @ on a table indexed with a mixed list is repeated selective xform"
    (is (= (keval "([]a:4 4 6;b:20 20 30;c:200 200 300)")
           (keval "@[([]a:1 2 3;b:10 20 30;c:100 200 300);(`a;0);{2*x}]")))))
(deftest test-4-arg-at
  (testing "4-arg @ on a vector indexed with a long is selective xform with rhs"
    (is (= [0 2 2 3] (keval "@[!4;1;*;2]"))))
  (testing "4-arg @ on a vector indexed with longs is selective xform with rhs"
    (is (= [0 2 2 6] (keval "@[!4;1 3;*;2]"))))
  (testing "4-arg @ on a vector indexed with a matrix is selective xform with rhs"
    (is (= [0 2 4 6] (keval "@[!4;(1 3;2 0);*;2]")))
    (is (= [0 2 8 6] (keval "@[!4;(1 3;2 0);*;2 4]")))
    (is (= [0 2 12 12] (keval "@[!4;(1 3;2 0);*;(2 4;6 8)]"))))
  (testing "4-arg @ on a dict indexed with a ragged 2-d vector is selective xform with rhs"
    (is (= (keval "`a`b`c!2 4 6")
           (keval "@[`a`b`c!1 2 3;(`a`c;`b);*;2]"))))
  (testing "4-arg @ works on tables"
    (is (= (keval "([]a:500 10 15;b:10000 200 300;c:10000 200 300)")
           (keval "@[([]a:1 2 3;b:10 20 30;c:100 200 300);(`a`b;0);*;(5 10;100)]")))))
(deftest test-2-arg-dot
  (testing "2-arg . on a vector indexed with a vector does"
    (is (= 2 (keval "1 2 3 4 .,1")))
    (is (= [2 3] (keval "1 2 3 4 .,1 2")))
    (is (= [[0 1 2] [6 7 8]] (keval "(0 1 2;3 4 5;6 7 8).,0 2")))
    (is (= 4 (keval "(0 1 2;3 4 5;6 7 8). 1 1")))
    (is (= [1 4] (keval "(0 1 2;3 4 5;6 7 8).(0 1;1)")))
    (is (= [[0 2] [6 8]] (keval "(0 1 2;3 4 5;6 7 8).(0 2;0 2)"))))
  (testing "2-arg . on a vector eliding the first index yields columns"
    (is (= [[0 2] [3 5] [6 8]] (keval "(0 1 2;3 4 5;6 7 8).(;0 2)"))))
  (testing "2-arg . indexing dicts of vectors does"
    (is (= 4 (keval "(`a`b`c!(0 1 2;3 4 5;6 7 8)).(`b;1)")))
    (is (= [1 4] (keval "(`a`b`c!(0 1 2;3 4 5;6 7 8)).(`a`b;1)")))
    (is (= [3 5] (keval "(`a`b`c!(0 1 2;3 4 5;6 7 8)).(`b;0 2)"))))
  (testing "2-arg . indexing tables does"
    (is (= 5 (keval "([]a:1 2 3;b:4 5 6).(`b;1)")))
    (is (= 5 (keval "([]a:1 2 3;b:4 5 6).(1;`b)")))
    (is (= [2 5] (keval "([]a:1 2 3;b:4 5 6).(`a`b;1)")))
    (is (= [[1 3] [4 6]] (keval "([]a:1 2 3;b:4 5 6).(`a`b;0 2)"))))
  (testing "2-arg . can apply functions"
    (is (= 2 (keval "{2}.()")))
    (is (= 2 (keval "{x}.,2")))
    (is (= 3 (keval "{x+y}. 1 2")))
    (is (= 3 (keval "+. 1 2")))
    (is (= 4 (keval "{x+z}. 1 2 3")))))
(deftest test-3-arg-dot
  (testing "3-arg . is selective xform at depth"
    (is (= (keval "(100 101 2;103 104 5;6 7 8)")
           (keval ".[(0 1 2;3 4 5;6 7 8);(0 1;0 1);{x+100}]")))
    (is (= (keval "(0 1 2;103 104 105;6 7 8)")
           (keval ".[(0 1 2;3 4 5;6 7 8);(1;0 1 2);{x+100}]")))
    (is (= (keval "((0 1;2 3);(4 5;6 49);(8 9;10 11))")
           (keval ".[((0 1;2 3);(4 5;6 7);(8 9;10 11));1 1 1;{x*x}]")))))
(deftest test-4-arg-dot
  (testing "4-arg . is selective xform at depth with rhs"
    (is (= (keval "`a`b`c!(0 1 2;30 4 500;6 7 8)")
           (keval ".[`a`b`c!(0 1 2;3 4 5;6 7 8);(`b;0 2);*;10 100]")))))
(deftest test-join
  (testing "monadic , envectors"
    (is (= [1] (keval ",1")))
    (is (= [[1 2 3]] (keval ",1 2 3"))))
  (testing "dyadic , joins"
    (is (= [1 2 3 4] (keval "1 2,3 4"))))
  (testing "joining dicts is a merge where rhs wins"
    (is (= (keval "`a`b`c`e`d!1 10 20 5 30")
           (keval "(`a`b`c`e!1 2 3 5),`b`c`d!10 20 30")))))
(deftest test-deleting-columns
  (testing "delete can delete a column"
    (is (= (keval "([]a:1 2 3)")
           (keval "delete b from ([]a:1 2 3;b:1 2 3)"))))
  (testing "delete can delete multiple columns"
    (is (= (keval "([]b:1 2 3)")
           (keval "delete a,c from ([]a:1 2 3;b:1 2 3;c:1 2 3)"))))
  (testing "delete can delete a key column"
    (is (= (keval "([]b:1 2 3;c:1 2 3)")
           (keval "delete a from ([a:1 2 3]b:1 2 3;c:1 2 3)"))))
  (testing "deleting only non-keycols from a keyed table preserves keycols"
    (is (= (keval "([a:1 2 3]c:1 2 3)")
           (keval "delete b from ([a:1 2 3]b:1 2 3;c:1 2 3)")))
    (is (= (keval "([]a:1 2 3;c:1 2 3)")
           (keval "delete b from ([a:1 2 3;b:1 2 3]c:1 2 3)")))))
(deftest test-deleting-rows
  (testing "delete can delete all rows"
    (is (= (keval "([]a:())") (keval "delete from([]a:1 2 3)"))))
  (testing "delete can delete rows specified by where"
    (is (= (keval "([]a:1 3)") (keval "delete from([]a:1 2 3)where a=2"))))
  (testing "delete can delete rows from keyed tables"
    (is (= (keval "([a:1 3]b:1 3)")
           (keval "delete from ([a:1 2 3]b:1 2 3)where a=2")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-select
  (testing "select does not require any agg exprs"
    (is (= (keval "([]a:1 2 3)") (keval "select from([]a:1 2 3)"))))
  (testing "id agg guesses result column to match original column"
    (is (= (keval "([]a:1 2 3)") (keval "select a from([]a:1 2 3)"))))
  (testing "select works on keyed tables"
    (is (= (keval "([a:`a`b]b:1 2)") (keval "select from([a:`a`b]b:1 2)")))))
(deftest test-by
  (testing "by clause does"
    (is (= (keval "([a:`a`b`c]b:3 5 7)")
           (keval "select +/b by a from([]a:6#`a`b`c;b:!6)"))))
  (testing "compound by clause does"
    (is (= (keval "([a:`a`a`b`b;b:`a`b`a`b];c:(0 4;1 5;2 6;3 7))")
           (keval "select by a,b from([]a:8#`a`a`b`b;b:8#`a`b;c:!8)")))))
(deftest test-keying-tables
  (testing "! can key a table by first n columns"
    (is (= (keval "([a:`a`b`c]b:1 2 3)") (keval "1!([]a:`a`b`c;b:1 2 3)")))
    (is (= (keval "([a:`a`b;b:1 2]c:3 4")
           (keval "2!([]a:`a`b;b:1 2;c:3 4"))))
  (testing "! can key a table by col name(s)"
    (is (= (keval "([a:`a`b`c]b:1 2 3)") (keval "`a!([]a:`a`b`c;b:1 2 3)")))
    (is (= (keval "([a:`a`b;c:3 4]b:1 2") (keval "`a`c!([]a:`a`b;b:1 2;c:3 4")))))
(deftest test-indexing-keyed-tables
  (testing "can index a keyed table with a dict"
    (is (= (keval "`b!1") (keval "([a:`a`b`c]b:1 2 3)`a!`a"))))
  (testing "can index a keyed table with just the value of a dict"
    (is (= (keval "`b!3") (keval "([a:`a`b`c]b:1 2 3)`c"))))
  (testing "can be done with a table"
    (is (= (keval "([]b:1 3)") (keval "([a:`a`b`c]b:1 2 3)([]a:`a`c)")))))
(deftest test-where
  (testing "monadic & turns bools into indexes that were true"
    (is (= [0 3] (keval "&1001b"))))
  (testing "monadic & turns longs into copies of indexes"
    (is (= [1 2 2 3 3 3] (keval "&0 1 2 3")))))
(deftest test-lj
  (testing "lj uses rhs key col"
    (is (= (keval "([]a:`a`b`c;b:1 2 3)")
           (keval "([]a:`a`b`c)lj([a:`a`b`c]b:1 2 3)"))))
  (testing "lj works with multiple key cols on the rhs"
    (is (= (keval "([]a:`a`b`c;b:1 2 3;c:10 20 30)")
           (keval "([]a:`a`b`c;b:1 2 3)lj([a:`a`b`c;b:1 2 3]c:10 20 30)")))))
(deftest test-xasc
  (testing "xasc can sort on one col"
    (is (= (keval "([]a:`a`b`c;b:3 2 1)")
           (keval "`a xasc([]a:`c`b`a;b:1 2 3)"))))
  (testing "xasc can sort on two cols"
    (is (= (keval "([]a:`a`a`b`b`c`c;b:3 20 10 30 2 3)")
           (keval "`a`b xasc([]a:`c`b`a`a`b`c;b:3 10 3 20 30 2)"))))
  (testing "xasc works on keyed tables"
    (is (= (keval "([a:`a`a`b`b`c`c];b:3 20 10 30 2 3)")
           (keval "`a`b xasc([a:`c`b`a`a`b`c]b:3 10 3 20 30 2)")))))
(deftest test-destructuring-function-arguments
  (testing "vector destructuring can use vector literal syntax"
    (is (= 2 (keval "{[(x;y)]y}1 2")))
    (is (= 3 (keval "{[(x;y);z]z}[1 2;3]")))
    (is (= 3 (keval "{[x;(y;z)]z}[1;2 3]"))))
  (testing "vector destructuring can use whitespace"
    (is (= 2 (keval "{[x y]y}1 2")))
    (is (= 3 (keval "{[x y;z]z}[1 2;3]")))
    (is (= 3 (keval "{[x;y z]z}[1;2 3]")))
    (is (= [1 1 2 3 5 8 13 21 34 55] (keval "*'0 1{[x y;z]y,x+y}\\!10"))))
  (testing "dictionary destructuring uses !"
    (is (= [3 4] (keval "{[x!y]y}`a`b!3 4")))
    (is (= [3 4] (keval "{[x!y;z]y}[`a`b!3 4;5]")))
    (is (= [:a :b] (keval "{[x;y!z]y}[6;`a`b!3 4]"))))
  (testing "dictionary destructuring works on keyed tables"
    (is (= (keval "([]q:1 2 3)") (keval "{[k!v]v}([p:`a`b`c]q:1 2 3)"))))
  (testing "table destructuring can use table literal syntax"
    (is (= [1 2 3] (keval "{[([]a;b)]b}([]p:`a`b`c;q:1 2 3)"))))
  (testing "table destructuring can use spaces instead of ;"
    (is (= [1 2 3] (keval "{[([]a b)]b}([]p:`a`b`c;q:1 2 3)"))))
  (testing "keyed tables can be destructured in function arguments"
    (is (= [1 2 3] (keval "{[([a]b c)]c}([p:`a`b`c]q:`d`e`f;r:1 2 3)"))))
  (testing "destructuring can use _ for values you don't need"
    (is (= 2 (keval "{[_ _ a _ _]a}@!5"))))
  (testing "destructuring can be incomplete"
    (is (= 3 (keval "{[x y z]z}1 2 3 4")))
    (is (= 0 (keval "{[a b!c d]c}`a`b`c`d!!4")))
    (is (= 2 (keval "{[_ _ a]a}@!5"))))
  (testing "destructuring is recursive"
    (is (= 3 (keval "{[(a b;c d)]c}(1 2;3 4)")))
    (is (= 1 (keval "{[a b!c d]c}`a`b!1 2")))
    (is (= 2 (keval "{[_!_ a b]a*b}`a`b`c`d`e!!5")))
    (is (= [1 2 3] (keval "{[([]a)!([]b)]b}([p:`a`b`c]q:1 2 3)")))))
(deftest test-destructuring-assignments
  (testing "vector destructuring can use vector literal syntax"
    (is (= 2 (keval "{(a;b):1 2;b}[]"))))
  (testing "vector destructuring can use whitespace but parens are required"
    (is (= 2 (keval "{(a b):1 2;b}[]"))))
  (testing "dictionary destructuring requires parens"
    (is (= [:a :b] (keval "{(a!b):`a`b!1 2;a}[]"))))
  (testing "table destructuring can use table literal syntax"
    (is (= [1 2 3] (keval "{[]([]a;b):([]p:`a`b`c;q:1 2 3);b}[]"))))
  (testing "table destructuring can use spaces instead of ;"
    (is (= [1 2 3] (keval "{[]([]a b):([]p:`a`b`c;q:1 2 3);b}[]"))))
  (testing "keyed tables can be destructured in assignments"
    (is (= [1 2 3] (keval "{[]([a]b):([p:`a`b`c]q:1 2 3);b}[]")))))
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
    (is (= [0 2 4] (keval "<=2*>=!3")))
    (is (= [[0 0 0] [1 2 3] [2 4 6]] (keval "<=1 2 3*>=!3"))))
  (testing "user-defined functions work on streams"
    (is (= [0 1 4] (keval "<={x*x}@>=!3")))
    (is (= [0 2 6] (keval "<={x+x*x}@>=!3"))))
  (testing "first works on streams"
    (is (= 0 (keval "<=*>=!3")))
    (is (= 0 (keval "<={*x}@>=!3"))))
  (testing "take on a stream creates a stream that stops after n events"
    (is (= [] (keval "<=0#>=!3")))
    (is (= [0 1] (keval "<=2#>=!3")))
    (is (= [0 1] (keval "<={2#x}@>=!3"))))
  (testing "take from the back on a stream works via a new stream"
    (is (= [2] (keval "<=-1#>=!3")))
    (is (= [0 1 2] (keval "<=-3#>=!3"))))
  (testing "overtake from a stream works"
    (is (= [0 1 2 0 1] (keval "<=5#>=!3")))
    (is (= [1 2 0 1 2] (keval "<=-5#>=!3"))))
  (testing "drop from a stream does"
    (is (= [1 2] (keval "<=1_>=!3")))
    (is (= [1 2] (keval "<={1_x}@>=!3")))
    (is (= [] (keval "<=3_>=!3")))
    (is (= [] (keval "<=4_>=!3"))))
  (testing "drop from the back"
    (is (= [0 1] (keval "<=-1_>=!3")))
    (is (= [] (keval "<=-3_>=!3")))
    (is (= [] (keval "<=-4_>=!3"))))
  (testing "vector literals with stream components"
    (is (= [[1 0] [1 1] [1 2]] (keval "<=(1;>=!3)"))))
  (testing "indexing with @ with a stream on the rhs"
    (is (= [0 2 4] (keval "<=(!5)@>=0 2 4"))))
  (testing "indexing via @ with a stream on the lhs"
    (is (= [2 3] (keval "<=(>=(0 2 4;1 3 5))@1"))))
  (testing "indexing via juxt with a stream on the lhs"
    (is (= [2 3] (keval "<=(>=(0 2 4;1 3 5))1"))))
  (testing "indexing with . with a stream on the rhs"
    (is (= [1 3] (keval "<=(0 1;2 3).>=(0 1;1 1)"))))
  (testing "indexing with . with a stream on the lhs"
    (is (= [1 5] (keval "<=(>=((0 1;2 3);(4 5;6 7))). 0 1"))))
  (testing "@& can filter on a stream"
    (is (= [0 2] (keval "<={x@&0=x mod 2}@>=!3"))))
  (testing "multiple streams in the same computation works"
    (is (some #{(keval "<=(>=!3)*>=!3")}
              [[0 2 4] [0 1 2 4] [0 0 2 4] [0 0 1 2 4] [0 0 0 2 4]])))
  (testing "/: over a stream works"
    (is (= [[0 0] [1 1]] (keval "<=2#/:>=!2"))))
  (testing "\\: over a stream works"
    (is (= [[] [3] [3 2] [3 2 1]] (keval "<=(>=!4)#\\:3 2 1"))))
  (testing "/ over a stream works"
    (is (= 6 (keval "<=+/>=!4")))
    (is (= 9 (keval "<=3+/>=!4"))))
  (testing "\\ over a stream works"
    (is (= [0 1 3 6] (keval "<=+\\>=!4")))
    (is (= [3 4 6 9] (keval "<=3+\\>=!4"))))
  (testing "': over a stream works"
    (is (= [3 7 4 6] (keval "<=-':>=3 10 14 20")))
    (is (= [2 7 4 6] (keval "<=1-':>=3 10 14 20")))))
;; ;; gave up on this one: couldn't fix the <exprx> rule
;; ;; (testing "select"
;; ;;       (is (= 1 (count
;; ;;        (parses
;; ;;         "select +/a,+/b from([]a:,/3#/:1 2;b:6#10 20 30)where b<=20"))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
