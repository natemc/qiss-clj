# qiss

qiss is short and simple: a q-like programming language for the
JVM.  Like all APL derivatives, qiss derives its expressiveness from
the following features:

* Automatic vectorization
* Precedence-less right-to-left evaluation of expressions
* Syntax that requires minimal punctuation

The interpreter is written in clojure, and the plan is to port it to
ClojureScript so we can write in qisses (qiss on EcmaScript), too.

## Installation

### Make sure you have the following installed:
  * a recent JDK from [Oracle](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
  * git
  * [Leiningen](http://leiningen.org/)
  * rlwrap

### Get the code

```
$ git clone https://github.com/natemc/qiss.git
$ cd qiss 
```
	
## Usage

Like most clojure applications, qiss is invoked using leiningen.  The
first time you run it, leiningen will automatically download the
clojure libraries needed by qiss, so it may take a minute.

```
qiss$ rlwrap lein run 
```

## Options

None.

## Examples

```
qiss$ rlwrap lein run 
§)/ / with whitespace (including newline) preceding is a comment
§) 
§)/ operators are ambivalent
§)/ i.e., they are monadic or dyadic depending on context
§)%5 / monadic (i.e., unary) % is reciprocal
0.2
§)10%5 / dyadic (i.e., binary) % is floating-point division
2.0
§)
§)/ all expressions are evaluated right-to-left
§)/ there is no precedence
§)3*2+2
12
§)
§)/ uniform vector literals require minimal punctuation
§)1 2 3 / integer (64-bit; clojure long) vector literal
[1 2 3]
§)1 2. 3 / float (64-bit; clojure double) vector literal
[1.0 2.0 3.0]
§)1001b / boolean vector literal.  no spaces allowed!
[true false false true]
§)`a`b`xyzyy / symbol vector literal.  no spaces allowed!
[:a :b :xyzzy]
§)
§)/ most dyadic operators are atomic: they automatically vectorize
§)1+!5 / ! is the key operator; !5 means "til 5" i.e. 0 1 2 3 4
[1 2 3 4 5]
§)(!5)*2
[0 2 4 6 8]
§)/ relational operators are atomic, too
§)(!5)<=|!5 / monadic | is reverse
[true true true false false]
§) 
§)/ indexing is also vectorized
§)(2*!5)[3]
6
§)(1+!5)[0 2 4]
[1 3 5]
§)/ the @ operator is an alternative way to express indexing
§)/ it is handy for forcing an ambivalent operator to be monadic
§)/ and also for applying an adverb (see below) to the indexing operation
§)`a`b`c`d`e@1 3
[:b :d]
§) 
§)/ : is for assignment
§)p:42
§)p
42
§)/ assignment is an expression
§)/ like *all* expressions, assignment is processed right-to-left
§)a*3+a:6
54
§) 
§)/ lambdas are in curly braces
§)K:{[a;b]a} / the K combinator takes 2 args and returns its 1st
§)K[3;4]
3
§)/ x, y, and z are implicit args
§)add:{x+y}
§)add[3;4]
7
§) 
§)/ lambdas are closures
§)f:{[a]{a+x}}3 / a is captured
§)f@!10 / @ for monadic function application; just like indexing
[3 4 5 6 7 8 9 10 11 12]
§) 
§)/ BEWARE! () do not invoke a function, [] do
§) 
§)/ dyadic functions, including user-defined ones, can be used infix
§)(!10)div 3 / div is builtin, atomic and performs integer division
[0 0 0 1 1 1 2 2 2 3]
§)(!10)mod 3 / mod is builtin and atomic
[0 1 2 0 1 2 0 1 2 0]
§)1 3 5 add 6 / since add's body is atomic, so is add
7 9 11
§) 
§)/ juxtaposition
§)/ indexing does not require square brackets
§)`a`b`c`d`e 0 2 4
[:a :c :e]
§)/ monadic functions can be applied without square brackets
§)count:{#x}
§)count[1 2 3]
3
§)count 1 2 3
3
§) 
§)/ adverbs
§)/ loops are expressed via higher-order functions called adverbs
§)/ adverbs are suffixed to the operator or function they modify
§)1 2+/:3 4 5 / /: is each-right
[[4 5] [5 6] [6 7]]
§)1 2+\:3 4 5 / \: is each-left
[[4 5 6] [5 6 7]]
§)/ / is over which is like reduce in clojure
§)0+/!5 / optional left-hand side with over is the initial value
10
§)5+\!5 / \ is scan (like reductions in clojure)
[5 6 8 11 15]
§)/ ' is each, and it takes the valence/arity of the function it modifies
§)#'(1 2 3;(4 5 6;7 8 9)) / (;) is a generic vector literal
[3 2]
§)1 2 3+'10 20 30 / ' is redundant here since + is atomic
[11 22 33]
§){x*y+z}'[1 2 3;4 5 6;7 8 9] / triadic (aka ternary) each
[11 26 45]
§)/ ': is each-prior
§)0-':2 6 6 10 14 18 21 23 26 28
[2 4 0 4 4 4 3 2 3 2]
§) 
§)/ dicts are formed using the ! operator on two vectors
§)`a`b`c!1 2 3
:a| 1
:b| 2
:c| 3
§)/ BEWARE the interpreter will die if the vectors' lengths don't match
§)/ as a convenience, two atoms can be made into a dict
§)/ as if they were two vectors, each of length one
§)(,`a)!,1
:a| 1
§)`a!1
:a| 1
§)/ dict indexing is analogous to vector indexing 
§)(`a`b`c!1 2 3)`a
1
§)(`a`b`c!1 2 3)`a`c
[1 3]
§)/ monadic = is the group function
§)/ it creates a dict mapping distinct values to indexes
§)n:`c`a`d`b`a`b`c`c`d`e`a`a`b`e`c`b`c`b`e`e
§)=n
:c| [0 6 7 14 16]
:a| [1 4 10 11]
:d| [2 8]
:b| [3 5 12 15 17]
:e| [9 13 18 19]
§)/ most operations work on dicts analogously to vectors
§)10+`a`b`c!1 2 3
:a| 11
:b| 12
:c| 13
§)/ similarly, a dict can be used as an index
§)v:82 63 80 99 83 66 51 34 83 69 16 81 16 90 21 82 71 32 55 19
§)v@=n
:c| [82 51 34 21 71]
:a| [63 83 16 81]
:d| [80 83]
:b| [99 66 16 82 32]
:e| [69 90 55 19]
§) 
§)/ tables
§)t:([]a:,/3#/:1 2 3;b:9#10 20 30)
§)t
a b
----
1 10 
1 20 
1 30 
2 10 
2 20 
2 30 
3 10 
3 20 
3 30
§)/ tables can be indexed using column names
§)t`a
[1 1 1 2 2 2 3 3 3]
§)&25>t`b / monadic & is where: it returns the true indexes
[0 1 3 4 6 7]
§)/ tables can also be indexed using row numbers (starting from zero)
§)t@!3 / without the @, ! would be dyadic and fail
a b 
----
1 10 
1 20 
1 30
§)t 0 / a single row is a dict
:a| 1
:b| 10
§)/ IMPORTANT: therefore, a table can be treated as a vector of dicts
§)t@&25>t`b
a b
----
1 10 
1 20 
2 10 
2 20 
3 10 
3 20 
§) 
§)/ special forms enable sql-like syntax
§)select +/a,+/b from t where b<=20,a<3
a b
----
6 60
§)select +/b by a from t
a| b 
-| --
1| 30
2| 30
3| 30
§)/ note the vertical bar in the above select result
§)/ this denotes a keyed table, which is a dict made from two tables
§)/ a keyed table is indexed by its key 
§)/ since a keyed table's key is a table, a single index is a dict
§) (select +/b by a from t)`a!1
:b| 60 
§)/ multiple indexes are a table
§)(select +/b by a from t)([]a:1 3)
b 
--
60
60
§)/ as a convenience, a single row can be retrievd from a
§)/ keyed table by supplying only the value of the implied dict
§) (select +/b by a from t)1 / same as indexing with `a!1
:b| 60 
§)/ the ! operator can be used to manipulate a table's keys
§)1!t / make t's first column a key  
a| b 
-| --
1| 10
1| 20
1| 30
2| 10
2| 20
2| 30
3| 10
3| 20
3| 30
§)`b!t / create key column(s) by name
§)`b!t
b | a
--| -
10| 1
20| 1
30| 1
10| 2
20| 2
30| 2
10| 3
20| 3
30| 3
§)0!select +/b by a from t / remove keys from a keyed table
a b 
----
1 60
2 60
3 60
§)/ to create a keyed table as a literal, place the key columns in the []
§)([a:`a`b`c`d`e]b:10*1+!5)
a | b 
--| --
:a| 10
:b| 20
:c| 30
:d| 40
:e| 50
§) 
§)/ a query on a keyed table preserve its keys if the query 
§)/ has no aggregations and no by clause
§)select from 1!t where a in 1 2
a| b 
-| --
1| 10
1| 20
1| 30
2| 10
2| 20
2| 30
§)select +/b from 1!t where a in 1 2
b  
---
120
§) 
§)/ there is no string type in qiss; strings are vectors of chars
§)"x" / char
\x
§)"xyzzy" / vector of char
xyzzy
§)/ a vector of char behaves like any vector wrt atomic ops
§)"foo","bar" 
foobar
§)(,"foo"),,"bar" / to make a pair of strings, enlist them first
["foo" "bar"]
§)"foo"="foo" / atomic =
["true" "true" "true"]
§)"foo"~"foo" / use ~ (match) to compare strings as a unit
true
§)/ exit
§)\\
qiss$
```

### Bugs

Unbounded!  There is much work to do.  These are some of the most
pressing issues:

* fix grammar: infix with vector literal on lhs (e.g., 1 2 in 1 2 3)
* fix grammar so loading/running scripts isn't so fragile
* test cases
* nulls: will nil do?
* update
* port to ClojureScript
* indexing at depth: (`a`b`c;`d`e`f)[0 1;0 1] => [[:a :b] [:d :e]]
* . (value and 2-arg version)
* destructuring for function arguments (and assignments?)
(a;b):0 1 / same as a:0;b:1
(){[a;(b;c)]a+b*c]/(0 1;2 3;4 5)
* dict op dict
* insert
* delete
* reshape (take a box) form of the # operator
* partial function application
* 3- and 4-arg versions of @ and .
Allow : as the 3rd arg?  {y} works just as well and doesn't complicate
things.
* generic list literal with holes is a list constructor
* get rid of :pass-global-env 
* if special form 
* casting and parsing with $ 
* stacked adverbs e.g. ,//(1 2 3;(4 5 6;7 8 9)) => [1 2 3 4 5 6 7 8 9]
* better console output
* vector conditional form of ?
* more ascii file I/O options

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
