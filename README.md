# qiss

qiss is short and simple: a k/q-like programming language for the JVM and JavaScript engines.  Like all APL derivatives, qiss derives its expressiveness from the following features:

* Automatic vectorization
* Precedence-less right-to-left evaluation of expressions
* Syntax that requires minimal punctuation

qiss is a toy interpreter whose primary purpose is for us to explore ideas (e.g., what would FRP look like in a single-page browser app if k had built-in FRP support?); no attempt has been made to make it robust or efficient.  The interpreter is written in clojure, and it works (modulo platform limitations) in ClojureScript so we can write in qisses (qiss on EcmaScript), too.

## Installation

### Make sure you have the following installed (or see Docker section below):
  * a recent (>= SE 8) [JDK from Oracle](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
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

## Docker

If you are new to leiningren and wish to jump right in to the qiss REPL in
a single command, the provided Dockerfile along with the Makefile will 
construct a container with the complete stack on your behalf.

```
qiss$ make docker
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
§)/ assignment supports clojure-esque destructuring
§)(a;b):1 2 / (;) is general vector literal syntax
§)a
1
§)b
2 
§)(a;b):5 6 7 8 / when destructuring, extra content is ignored
§)b
6
§)(;b):9 10 / holes are skipped
§)a
5
§)b
10
§)(a b):3 4 / ; can be replaced with space when destructuring vectors
§)a
3
§)(_ b):`a`b / when using spaces, use _ to skip
§)a
3
§)b
:b
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
§)/ partial function application aka projection
§)inc:1+ / when args are missing, binds the ones present
§)inc 5
6
§)dec:-[;1] / elision of first argument
§)dec 5
4
§)/ formal arguments support destructuring like assignment
§)second:{[(a;b)]b} / destructured formal arg resembles corresponding literal
§)second 1 2
2
§)second:{[a b]b}   / simplified syntax: no (;) needed for a vector
§)second 1 2 3 4   / extra content is ignored
2
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
§)0-':2 6 6 10 14 18 21 23 26 28 / deltas
[2 4 0 4 4 4 3 2 3 2]
§)/ destructuring can make the use of adverbs simpler
§)*'0 1{[x y;z]y,x+y}\!10 / monadic * is first
[1 1 2 3 5 8 13 21 34 55]
§) 
§)/ dicts are formed using the ! operator on two vectors
§)`a`b`c!1 2 3
:a| 1
:b| 2
:c| 3
§)/ as a convenience, two atoms can be made into a dict
§)/ as if they were two vectors, each of length one
§)(,`a)!,1
:a| 1
§)`a!1
:a| 1
§)(k!v):`a`b`c!1 2 3 / destructuring a dict
§)k
[:a :b :c]
§)v
[1 2 3]
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
§)/ a vector of char behaves like any vector
§)"foo","bar" 
foobar
§)(,"foo"),,"bar" / to make a pair of strings, enlist them first
["foo" "bar"]
§)"foo"="foo" / atomic =
[true true true]
§)"foo"~"foo" / use ~ (match) to compare strings as a unit
true
§)/ exit
§)\\
qiss$
```

### qisses in the browser

To get things started, run the fig script in the qiss directory.  Next, go to http://localhost:3449 in your browser.  Lastly, view the web page source to see how qiss has been embedded in a script tag in the page.

Event streams in qisses are like time-varying values (see http://conal.net/fran/tutorial.htm).  Any expression involving a stream (time-varying value) produces another stream (time-varying value).  That way, chaining a sequence of callbacks looks just like any other qisses expression.  Not only do we make async code look sync, we dispense with a special set of functions and syntax for streams.

In the following example, every is a monadic function that produces a stream of times.  Whenever the value of that stream changes, it forces re-evaluation of all the expressions that depend on it so that they are consistent (hence, the functional part of FRP).

```
dom[`result]text str every 2000;
```

## qiss in the browser via Docker

Our Docker container runs the very same figwheel setup above.  Note that if your container is running on a ip address different from the host, 
don't forget to edit the project.clj file to reflect this and point your browser to the container's ip address (say http://192.168.99.100:3449) 
to invoke the figwheel prompt.

```
qiss$ make dockerfigwheel
```


### Bugs

Unbounded!  There is much work to do.  These are some of the most pressing issues:

* ,' on tables
* fix grammar so loading/running scripts isn't so fragile
* test cases
* nulls: will nil do?
* update
* good error messages
* virtual column i
* JS oddities: no int/float distinction, only numbers; no char/string distinction
* JS interop (w/react? DOM? node?)
* insert
* upsert
* exec
* reshape (take a box) form of the # operator
* more variations on 3- and 4-arg versions of @ and .
* Allow : as the 3rd arg (maybe.  {y} works just as well and doesn't complicate
things.)
* get rid of :pass-global-env 
* if special form
* date and time types
* casting and parsing with $ 
* better console output
* vector conditional form of ? (or maybe the if form automatically vectorizes)
* more ascii file I/O options
* be smart about indentation to reduce the need for ; (or use \ after whitespace as line-continuation)
* learn APL and J to steal ideas
* promote longs to doubles for =
* fuzzy comparison of doubles
* over and scan on state transition matrices
* experiment with make-array etc instead of using clojure vectors
* threads just to see what's possible - expose clojure's stm
