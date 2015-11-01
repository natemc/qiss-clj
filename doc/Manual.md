qiss Manual
=========


qiss for JavaScript Programmers
--------------


<table border=1>
<tr>
	<th>JavaScript</th>
	<th>qiss</th>
</tr>
<tr>
	<td>
		<a name="forEach"/>
		<tt>forEach</tt> <b></b>
<pre><code>> [1, 2, 3].forEach(x => console.log(x))
> 1
> 2
> 3
>
</code></pre>
	</td>
	<td>
		<tt>'</tt> is <b>each</b>
<pre><code>qiss)show'1 2 3
1
2
3
[nil nil nil]
qiss)
</pre></code>
	</td>
</tr>
<tr>
	<td valign="top">
		<a name="map"/>
		<tt>map</tt> <b></b>
<pre><code>> [1, 2, 3].map(x => x + 1)
> [2, 3, 4]
>
</code></pre>
	</td>
	<td>
		<tt>'</tt> <b>each</b> is equivalent to <b>map</b>
<pre><code>qiss){x+1}'1 2 3
[2 3 4]
qiss)
</code></pre>
		Many built-in operations are <i>atomic</i>, meaning they automatically vectorize:
<pre><code>qiss)1+1 2 3
[2 3 4]
qiss)
</code></pre>
	</td>
</tr>
<tr>
	<td>
		<a name="filter"/>
		<tt></tt> <b>filter</b>
<pre><code>> [1, 2, 3].filter(x => x > 1)
> [2, 3]
>
	</td>
	<td>
		<tt>@&</tt> <b>at where</b> is equivalent to <b>filter</b>
<pre><code>qiss){x@&x>1}1 2 3
[2 3]
qiss)</code></pre>
	</td>
</tr>
<tr>
	<td>
		<a name="concatAll"/>
		<tt>concatAll</tt>  <b></b>
<pre><code>> [[1], [2, 3], [], [4]].concatAll()
> [1, 2, 3, 4]
>
</code></pre>
	</td>
	<td>
		<tt>,/</tt> <b>join over</b> is equivalent to <b>concatAll</b>
<pre><code>qiss),/(1;2 3;();4)
[1 2 3 4]
qiss)</code></pre>
	</td>
</tr>

<tr>
	<td>
		<a name="iterator"/>
		<tt>iterator</tt>  <b></b>
<pre><code>> var iterator = [1, 2, 3].iterator();
> console.log(iterator.next());
> { value: 1, done: false }
> console.log(iterator.next());
> { value: 2, done: false }
> console.log(iterator.next());
> { value: 3, done: false }
> console.log(iterator.next());
> { done: true }
> 
</code></pre>
	</td>
	<td>
		<tt></tt> <b></b> is equivalent to <b>iterator</b>  TODO
<pre><code>qiss)show'1 2 3
1
qiss)</code></pre>
	</td>
</tr>


<tr>
	<td>
		<a name="Observable"/>
		<tt>Observable</tt>  <b></b>
<pre><code>
> var mouseMoves = Observable.fromEvent(element, "mousemove");
> var subcription = mouseMoves.forEach(console.log);
> 
</code></pre>
	</td>
	<td>
		<tt>event</tt> <b></b> is equivalent to <b>fromEvent</b>  
<pre><code>qiss){show x}`mouseMoves event dom[`element] 
qiss)</code></pre>
	</td>
</tr>


<tr>
	<td>
		<a name="Observable"/>
		<tt>Expanded Observable.forEach</tt>  <b></b>
<pre><code>
> var mouseMoves = Observable.fromEvent(element, "mousemove");
> var subcription = mouseMoves.forEach(console.log
                 // next data
                 event => console.log(event),
                 // error
                 error => console.error(error),
                 // completed
                 () => console.log("done"));
> 
</code></pre>
	</td>
	<td>
		<tt>event</tt> <b></b> is equivalent to <b>fromEvent</b> TODO
<pre><code>qiss)event[`mousemove; dom[`element]; {show x}; {[e] show e}; {show "done"}] 
qiss)</code></pre>
	</td>
</tr>


</table>

JavaScript example credit:
Jafar Husain
https://frontendmasters.com/courses/asynchronous-javascript