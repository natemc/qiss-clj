# Philosophy

In [The Design of APL](http://www.jsoftware.com/papers/APLDesign.htm), Falkoff and Iverson explain that simplicity was a driving force in APL's design, and that simplicity meant the following four ideas:

* Uniformity
* Generality
* Familiarity
* Brevity

(As an aside, it seems that in popular programming languages today familiarity is valued over all else - familiarity meaning, in this case, looking like C.  The sort of familiarity Iverson had in mind - familiar to mathematicians - is profoundly unfamiliar to most of us writing software today.  Meanwhile, the other attributes above are hardly present in our most-used programming languages, including the handful of OO scripting languages that have gained a foothold without matching C's syntax.)

As descendants of APL, k dialects display an unusual degree of uniformity in the treatment of collections and of functions.  For example, in most languages, indexing a vector is analogous to indexing a dictionary:

```
qiss)v:0 1 4 9 16 25 36 49 64 81
qiss)v[8]
64
qiss)d:(!10)!v
qiss)d[8]
64
```

In k dialects, this uniformity extends to atomic operations (i.e., operations that automatically vectorize):

```
qiss)v+1
1 2 5 10 17 26 37 50 65 82
qiss)d+1
0| 1
1| 2
2| 5
3| 10
4| 17
5| 26
6| 37
7| 50
8| 65
9| 82
```

Moreover, function application utilizes the same syntax as indexing:

```
qiss)square:{x*x}
qiss)square[8]
64
```

The theme is uniform treatment of maps, i.e., mechanisms for associating a member of one set with another.  A vector is an enumerated map from the non-negative integers to its value, a dictionary is an enumerated map from its key to its value, and a function is a rule-based map from its input to its output.  However, they are all maps, and the syntax treats them so.

In qiss, we would like to extend this uniformity to event streams.  We want

* asynchronous code to look like synchronous code,
* functions written without consideration for streams to be applicable to streams, and
* to obviate any need for a special set of functions for mapping/filtering/etc over streams.

For example, we should be able to write code like the following (supposing we are running in a browser repl with suitable bindings):

```
qiss)(dom`result)text{$x@&0=x mod 2}`clientX of`mousemove event dom`box;
```

Contrast this to a (hypothetical) transliteration of most FRP libraries today:

```
qiss){dom[`result]text$@x}mapstream{0=x mod 2}filterstream(`clientX of)mapstream`mousemove event dom`box
```

## Event Stream as Maps?

If we want event streams that are maps, the first idea that comes to mind is that event stream are maps from time to value...
