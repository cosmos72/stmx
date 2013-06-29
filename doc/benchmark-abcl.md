STMX Performance
----------------

This document is an addition to benchmark.md. Please read it first.

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 29 June 2013

Hardware: Intel Core-i7 4770 @3.5 GHz (quad-core w/ hyper-threading), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), OpenJDK 6b27-1.12.5-2 (x86_64),
          ABCL 1.1.1, STMX 1.3.3

<table>
 
 <tr><th colspan="6">
       Concurrent benchmarks on a 4-core CPU. They already iterate
       ten million times, do not wrap them in <code>(1m ...)</code>.
     </th></tr>

 <tr><th colspan="6">
       Dining philosophers, load with<br>
       <code>(load "stmx/example/dining-philosophers-stmx.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers-hw-tx.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers-lock.lisp")</code><br>
       <code>(in-package :stmx.example{1|2|3})</code>
     </th></tr>

 <tr><th rowspan="2"><b>number of threads</b></th>
     <th rowspan="2"><b>executed code</b></th>
     <th><b>STMX (sw transactions)</b></th>
     <th><b>HW-TX (hw transactions)</b></th>
     <th><b>LOCK (atomic compare-and-swap)</b></th>
     <th><b>LOCK (bordeaux-threads mutex)</b></th></tr>

 <tr><th colspan="4"><b>millions transactions per second</b></th></tr>

 <tr><td>1 thread</td>
     <td><code>(dining-philosophers 1)</code></td>
     <td>0.071</td><td>     </td><td>     </td><td>0.575</td></tr>

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2)</code></td>
     <td>0.143</td><td>     </td><td>     </td><td>0.558</td></tr>

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3)</code></td>
     <td>0.206</td><td>     </td><td>     </td><td>0.561</td></tr>

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4)</code></td>
     <td>0.241</td><td>     </td><td>     </td><td>0.698</td></tr>

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5)</code></td>
     <td>0.246</td><td>     </td><td>     </td><td>0.803</td></tr>

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6)</code></td>
     <td>0.241</td><td>     </td><td>     </td><td>0.954</td></tr>

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7)</code></td>
     <td>0.269</td><td>     </td><td>     </td><td>1.096</td></tr>

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8)</code></td>
     <td>0.276</td><td>     </td><td>     </td><td>1.209</td></tr>

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10)</code></td>
     <td>0.124</td><td>     </td><td>     </td><td>1.424</td></tr>

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15)</code></td>
     <td>0.122</td><td>     </td><td>     </td><td>1.845</td></tr>

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20)</code></td>
     <td>0.129</td><td>     </td><td>     </td><td>2.048</td></tr>

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30)</code></td>
     <td>0.139</td><td>     </td><td>     </td><td>2.130</td></tr>

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40)</code></td>
     <td>0.153</td><td>     </td><td>     </td><td>2.154</td></tr>

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50)</code></td>
     <td>0.163</td><td>     </td><td>     </td><td>2.160</td></tr>

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100)</code></td>
     <td>0.162</td><td>     </td><td>     </td><td>2.156<td></tr>

 <tr><td>200 threads</td>
     <td><code>(dining-philosophers 200)</code></td>
     <td>0.160</td><td>     </td><td>     </td><td>2.164</td></tr>

</table>
