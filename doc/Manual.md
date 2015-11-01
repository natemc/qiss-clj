qiss Manual
=========


Events Reference
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
<pre><code>> [1, 2, 3].forEach( x => console.log(x))
> 1
> 2
> 3
>
</code></pre>
	</td>
	<td>
		<tt>'</tt> is <b>each</b>
<pre><code>qiss){ show x }'1 2 3
1
2
3
qiss)
</pre></code>
	</td>
</tr>
<tr>
	<td>
		<a name="map"/>
		<tt>map</tt> <b></b>
<pre><code>> [1, 2, 3].map( x => x + 1 )
> [2, 3, 4]
>
</code></pre>
	</td>
	<td>
		<tt>'</tt> <b>each</b> is equivalent to <b>map</b>
<pre><code>qiss){x + 1}'1 2 3
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
		<tt>fby</tt> is <b>filter-by</b>  TODO
<pre><code>qiss)( ; ) fby  1 2 3
[2 3]
qiss)</code></pre>
	</td>
</tr>
<tr>
	<td>
		<a name="concatAll"/>
		<tt>concatAll</tt>  <b></b>
<pre><code> > [ [1], [2, 3], [], [4] ].concatAll()
> [ 1, 2, 3, 4]
>
</code></pre>
	</td>
	<td>
		<tt>raze</tt> is <b> flattens</b>
<pre><code>qiss) raze (1 ; 2 3 ; ; 4)  TODO
[ 1 2 3 4 ]
qiss)</code></pre>
	</td>
</tr>
</table>

