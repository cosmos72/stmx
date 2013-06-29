STMX Performance
----------------

This document is an addition to benchmark.md. Please read it first.

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 29 June 2013

Hardware: Intel Core-i7 4770 @3.5 GHz (quad-core w/ hyper-threading), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), CCL 1.9-r15769 (x86_64), STMX 1.3.3

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
     <td>0.639</td><td>     </td><td>     </td><td> 8.39</td></tr>

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2)</code></td>
     <td>1.115</td><td>     </td><td>     </td><td> 4.60</td></tr>

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3)</code></td>
     <td>0.978</td><td>     </td><td>     </td><td> 4.96</td></tr>

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4)</code></td>
     <td>0.927</td><td>     </td><td>     </td><td> 6.05</td></tr>

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5)</code></td>
     <td>0.937</td><td>     </td><td>     </td><td> 7.56</td></tr>

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6)</code></td>
     <td>0.892</td><td>     </td><td>     </td><td> 7.54</td></tr>

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7)</code></td>
     <td>0.858</td><td>     </td><td>     </td><td> 8.34</td></tr>

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8)</code></td>
     <td>0.864</td><td>     </td><td>     </td><td> 7.11</td></tr>

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10)</code></td>
     <td>0.797</td><td>     </td><td>     </td><td>11.63</td></tr>

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15)</code></td>
     <td>0.657</td><td>     </td><td>     </td><td>14.96</td></tr>

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20)</code></td>
     <td>0.066</td><td>     </td><td>     </td><td>19.33</td></tr>

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30)</code></td>
     <td>0.061</td><td>     </td><td>     </td><td>20.42</td></tr>

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40)</code></td>
     <td>0.095</td><td>     </td><td>     </td><td>19.76</td></tr>

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50)</code></td>
     <td>0.125</td><td>     </td><td>     </td><td>19.25</td></tr>

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100)</code></td>
     <td>0.092</td><td>     </td><td>     </td><td>18.11</td></tr>

 <tr><td>200 threads</td>
     <td><code>(dining-philosophers 200)</code></td>
     <td>0.053</td><td>     </td><td>     </td><td>17.70</td></tr>

</table>
