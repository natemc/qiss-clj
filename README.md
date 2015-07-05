# qiss

qiss is simple and succinct: a q-like programming language for the
JVM.  Like all APL derivatives, qiss derives its expressiveness from
the following features:

Automatic vectorization
Precedence-less right-to-left evaluation of expressions
Syntax that requires minimal punctuation

The interpreter is written in clojure, and the plan is to port it to
ClojureScript so we can write in qisses (qiss on EcmaScript), too.

## Installation

git clone https://github.com/natemc/qiss.git

## Usage

qiss$ lein run

## Options

None.

## Examples

```
  / / with whitespace (including newline) preceding is a comment

  / operators are ambivalent
  / i.e., they are monadic or dyadic depending on context
  %5 / monadic (i.e., unary) % is reciprocal
0.2
  10%5 / dyadic (i.e., binary) % is floating-point division
2.0

  / all expressions are evaluated right-to-left
  / there is no precedence
  3*2+2
12

  / uniform vector literals require minimal punctuation
  1 2 3 / integer (64-bit; clojure long) vector literal
[1 2 3]
  1 2. 3 / float (64-bit; clojure double) vector literal
[1.0 2.0 3.0]
  1001b / boolean vector literal. no spaces allowed!
[true false false true]
  `a`b`xyzyy / symbol vector literal.  no spaces allowed!
[:a :b :xyzzy]
  
  / most dyadic operators are atomic: they automatically vectorize
  1+!5 / !5 means "til 5" i.e. 0 1 2 3 4
[1 2 3 4 5]
  (!5)*2
[0 2 4 6 8]
  / relational operators are atomic, too
  (!5)<=|!5 / monadic | is reverse
[true true true false false]

  / indexing is also vectorized
  (2*!5)[3]
6
  (1+!5)[0 2 4]
[1 3 5]
  / the @ operator is an alternative way to express indexing
  / it is handy for forcing an ambivalent operator to be monadic
  / and also for applying an adverb (see below) to the indexing operation
  `a`b`c`d`e@1 3
[:b :d]

  / : is for assignment
  p:42
  p
42
  / assignment is an expression
  / like *all* expressions, assignment is processed right-to-left
  a*3+a:6
54

  / lambdas are in curly braces
  K:{[a;b]a} / the K combinator takes 2 args and returns its 1st
  K[3;4]
3
  / x, y, and z are implicit args
  add:{x+y}
  add[3;4]
7
  / BEWARE! () do not invoke a function, [] do

  / dyadic functions, including user-defined ones, can be used infix
  (!10)div 3 / div is builtin, atomic and performs integer division
[0 0 0 1 1 1 2 2 2 3]
  (!10)mod 3 / mod is builtin and atomic
[0 1 2 0 1 2 0 1 2 0]
  1 3 5 add 6 / since add's body is atomic, so is add
7 9 11

  / juxtaposition
  / indexing does not require square brackets
  `a`b`c`d`e 0 2 4
[:a :c :e]
  / monadic functions can be applied without square brackets
  count:{#x}
  count[1 2 3]
3
  count 1 2 3
3

  / adverbs
  / loops are expressed via higher-order functions called adverbs
  / adverbs are suffixed to the operator or function they modify
  1 2+/:3 4 5 /  /: is each-right
[[4 5] [5 6] [6 7]]
  1 2+\:3 4 5 / \: is each-left
[[4 5 6] [5 6 7]]
  / / is over which is like reduce in clojure
  0+/!5 / optional left-hand side with over is the initial value
10
  5+\!5 / \ is scan (like reductions in clojure)
[5 6 8 11 15]
  / ' is each, and it takes the valence/arity of the function it modifies
  #'(1 2 3;(4 5 6;7 8 9)) / (;) is a generic vector literal
[3 2]
  1 2 3+'10 20 30 / ' is redundant here since + is atomic
[11 22 33]
  {x*y+z}'[1 2 3;4 5 6;7 8 9] / triadic (aka ternary) each
[11 26 45]
  / ': is each-prior
  0-':2 6 6 10 14 18 21 23 26 28
[2 4 0 4 4 4 3 2 3 2]

  / dicts are formed using the ! operator on two vectors
  / BEWARE the interpreter will die if the vectors' lengths don't match 
  `a`b`c!1 2 3
:a| 1
:b| 2
:c| 3
  / dict indexing is analogous to vector indexing
  (`a`b`c!1 2 3)`a
1
  (`a`b`c!1 2 3)`a`c
[1 3]

  / tables
  t:([]a:,/3#/:1 2 3;b:9#10 20 30)
  t
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
  / tables can be indexed using column names
  t`a
[1 1 1 2 2 2 3 3 3]
  &25>t`b / monadic & is where: it returns the true indexes
[0 1 3 4 6 7]
  / tables can also be indexed using row numbers (starting from zero)
  t@!3 / without the @, ! would be dyadic and fail
a b 
----
1 10 
1 20 
1 30
  t 0 / a single row is a dict
:a| 1
:b| 10
  t@&25>t`b
a b
----
1 10 
1 20 
2 10 
2 20 
3 10 
3 20 

  / special forms enable sql-like syntax
  select +/a,+/b from([]a:,/3#/:1 2 3;b:9#10 20 30)where b<=20,a<3
a b
----
6 60
```

### Bugs

Unbounded!  There is much work to do.  These are some of the most
pressing issues:

scope
compound indexing, i.e., `a`b`c`d`e(1 2 3;1 2 3) =>[[:a :b :c][:a :b :c]]
indexing at depth: (`a`b`c;`d`e`f)[1;1] => :e
dict op dict
keyed tables
by clause in select
treat strings like vectors
nulls
cut and drop (i.e., the _ operator)
partial function application
ascii I/O
stacked adverbs e.g. ,//(1 2 3;(4 5 6;7 8 9)) => [1 2 3 4 5 6 7 8 9]

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
