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
<pre><code> > [[1], [2, 3], [], [4]].concatAll()
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
</table>

