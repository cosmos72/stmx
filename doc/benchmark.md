STMX Performance
----------------

As for any software, the topic of performance is often sensitive and
plagued with heated discussions. It is objectively difficult to come up with
scientifically accurate figures as they depend on many factors, including at least
hardware, operating system, common lisp implementation, optimization flags and usage pattern.

What follows is a list of micro-benchmarks, suitable to have an initial idea
about STMX performance for short, non-conflicting transactions.

Benchmark procedure
-------------------

Setup and optimization flags:

1. Before starting the REPL, it is recommended to remove any cached FASL file
   by deleting the folder ~/.cache/common-lisp/<your-CL-implementation>

2. Start the REPL and execute what follows:

        (declaim (optimize (compilation-speed 0) (space 0) (debug 0) (safety 0) (speed 3)))
        (ql:quickload "stmx")
        (in-package :stmx.util)
        (defvar *v* (tvar 0))
        (defvar *m*  (new 'rbmap :pred 'fixnum<)) 
        (defvar *tm* (new 'tmap  :pred 'fixnum<)) 
        (defvar *h*  (new 'ghash-table :test 'fixnum= :hash 'identity)) 
        (defvar *th* (new 'thash-table :test 'fixnum= :hash 'identity)) 
        ;; some initial values
        (setf (get-gmap *m* 1) 0)
        (setf (get-gmap *tm* 1) 0)
        (setf (get-ghash *h* 1) 0)
        (setf (get-ghash *th* 1) 0)
        (defmacro x3 (&rest body)
          `(let ((v *v*)
                 (m *m*)
                 (tm *tm*)
                 (h *h*)
                 (th *th*))
             (declare (ignorable v m tm h th))
             (dotimes (,(gensym) 3)
               ,@body)))
        (defmacro 1m (&rest body)
          `(time (dotimes (i 1000000)
             ,@body)))

3. to warm-up STMX and the common-lisp process before starting the benchmarks,
   it is also recommended to run first the test suite with:

        (ql:quickload "stmx.test")
        (fiveam:run! 'stmx.test:suite)

4. Run each benchmark inside an `(atomic ...)` block one million times
   (see `1m` macro above) in a single thread. Repeat each run three times
   (see `3x` macro above) and take the lowest of the three reported elapsed times.
   Divide by one million to get the average elapsed real time per iteration.

   This means for example that to run the benchmark `($ v)` one has to type
   `(x3 (1m (atomic ($ v))))`

All timings reported in the next section are the output on the author's system
of the procedure just described, and thus for each benchmark they contain
the average elapsed real time per iteration,
i.e. the total elapsed time divided by the number of iterations (one million).


Benchmark results
-----------------

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 27 July 2013

Hardware: Intel Core-i7 4770 @3.4 GHz (quad-core w/ hyper-threading), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.15 (x86_64), STMX 1.3.3


<table>

 <tr><th colspan="5">
       Single-thread benchmarks, executed one million times
       with <code>(x3 (1m (atomic ...)))</code>
     </th></tr>

 <tr><th rowspan="2"><b>name</b>      </th>
     <th rowspan="2"><b>executed code</b></th>
     <th>STMX sw-only transactions</th>
     <th>STMX hybrid hw+sw (requires Intel TSX and 64-bit SBCL)</th>
     <th>HAND-OPTIMIZED hw transactions - see doc/benchmark.lisp</th></tr>

 <tr><th colspan="2"><b>average time in microseconds</b></th></tr>


 <tr><td>nil       </td><td><code>nil</code></td>
     <td>0.070</td><td>0.022</td><td>0.012</td></tr>
     <!-- laptop 0.153; gv6 0.159 -->

 <tr><td>read-1    </td><td><code>($ v)</code></td>
     <td>0.087</td><td>0.022</td>0.021<td></td></tr>
     <!-- laptop 0.187; gv6 0.195 -->

 <tr><td>write-1   </td><td><code>(setf ($ v) 1)</code></td>
     <td>0.108</td><td>0.027</td><td>0.022</td></tr>
     <!-- laptop 0.277; gv6 0.257 -->

 <tr><td>read-write-1</td><td><code>(incf (the fixnum ($ v)))</code></td>
     <td>0.148</td><td>0.027</td><td>0.022</td></tr>
     <!-- laptop 0.339; gv6 0.596 -->

 <tr><td>read-write-10</td>
     <td><code>(dotimes (j 10) (incf (the fixnum ($ v))))</code></td>
     <td>0.272</td><td>0.059</td><td>0.036</td></tr>
     <!-- laptop 0.686; gv6 0.971 -->

 <tr><td>read-write-100</td>
     <td><code>(dotimes (j 100) (incf (the fixnum ($ v))))</code></td>
     <td>1.399</td><td>0.409</td><td>0.196</td></tr>
     <!-- laptop 3.703; gv6 4.160 -->

 <tr><td>read-write-1000</td>
     <td><code>(dotimes (j 1000) (incf (the fixnum ($ v))))</code></td>
     <td>9.922</td><td>3.852</td><td>1.656</td></tr>
     <!-- laptop 33.070; gv6 34.607 -->

 <tr><td>read-write-N</td><td>best fit of the 3 runs above</td>
     <td>(0.142+N*0.0098)</td><td>(0.0226+N*0.0036)</td><td>(0.0260+N*0.0016)</td></tr>

 <tr><td>orelse empty     </td><td><code>(orelse)</code></td>
     <td>0.043</td><td>0.026</td><td>0.021</td></tr>

 <tr><td>orelse unary     </td><td><code>(orelse ($ v))</code></td>
     <td>0.234</td><td>0.266</td></tr>

 <tr><td>orelse retry-1   </td><td><code>(orelse (retry) ($ v))</code></td>
     <td>0.429</td><td>0.488</td></tr>

 <tr><td>orelse retry-2   </td><td><code>(orelse (retry) (retry) ($ v))</code></td>
     <td>0.601</td><td>0.674</td></tr>

 <tr><td>orelse retry-4   </td><td><code>(orelse (retry) (retry)<br/>
                                         (retry) (retry) ($ v))</code></td>
     <td>0.963</td><td>1.035</td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td>
     <td>(0.248+N*0.178)</td><td>(0.308+N*0.182)</td></tr>

 <tr><td>tmap read-1</td>
     <td><code>(get-gmap tm 1)</code></td>
     <td>0.236</td><td>0.175</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(incf (get-gmap tm 1))</code></td>
     <td>0.531</td><td>0.419</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(when (zerop (mod i   10)) (clear-gmap tm))<br>
               (set-gmap tm i t)</code></td>
     <td>3.882</td><td>4.035</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(when (zerop (mod i  100)) (clear-gmap tm))<br>
               (set-gmap tm i t)</code></td>
     <td>5.392</td><td>5.641</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(when (zerop (mod i 1000)) (clear-gmap tm))<br>
              (set-gmap tm i t)</code></td>
     <td>6.443</td><td>6.775</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(incf (get-ghash th 1))</code></td>
     <td>0.674</td><td>0.525</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(when (zerop (mod i   10)) (clear-ghash th))<br>
              (set-ghash th i t)</code></td>
     <td>2.024</td><td>2.381</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(when (zerop (mod i  100)) (clear-ghash th))<br>
              (set-ghash th i t)</code></td>
     <td>1.913</td><td>2.176</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(when (zerop (mod i  1000)) (clear-ghash th))<br>
              (set-ghash th i t)</code></td>
     <td>1.933</td><td>2.183</td></tr>

 </table>


 <table>
 
 <tr><th colspan="8">
       Concurrent benchmarks on a 4-core CPU. They already iterate
       ten million times, do not wrap them in <code>(1m ...)</code>.
     </th></tr>

 <tr><th colspan="8">
       Dining philosophers, load with<br>
       <code>(load "stmx/example/dining-philosophers.stmx.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers.stmx-hw.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers.hw-only.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers.lock.lisp")</code><br>
       <code>(in-package :stmx.example.dining-philosophers.[...])</code>
     </th></tr>

 <tr><th rowspan="2"><b>number of threads</b></th>
     <th rowspan="2"><b>executed code</b></th>
     <th><b>STMX sw-only transactions</b></th>
     <th><b>STMX hybrid hw+sw</b></th>
     <th><b>STMX hybrid hw+sw, HAND OPTIMIZED</b></th>
     <th><b>hw-only, HAND-OPTIMIZED</b></th>
     <th><b>LOCK (atomic compare-and-swap)</b></th>
     <th><b>LOCK (bordeaux-threads mutex)</b></th></tr>

 <tr><th colspan="5"><b>millions transactions per second</b></th></tr>

 <tr><td>1 thread</td>
     <td><code>(dining-philosophers 1)</code></td>
     <td>4.27</td><td>24.45</td><td>34.97</td>
     <td>50.00</td><td>68.97</td><td>14.64</td></tr>

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2)</code></td>
     <td>7.77</td><td>9.46</td><td>11.48</td>
     <td>26.44</td><td>56.92</td><td>11.43</td></tr>

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3)</code></td>
     <td>11.37</td><td>9.67</td><td>10.40</td>
     <td>30.33</td><td>51.62</td><td>9.48</td></tr>

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4)</code></td>
     <td>14.87</td><td>11.83</td><td>13.84</td>
     <td>32.05</td><td>44.98</td><td>14.24</td></tr>

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5)</code></td>
     <td>14.64</td><td>13.05</td><td>15.16</td>
     <td>32.38</td><td>63.13</td><td>18.59</td></tr>

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6)</code></td>
     <td>14.79</td><td>13.94</td><td>14.86</td>
     <td>37.48</td><td>72.46</td><td>19.14</td></tr>

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7)</code></td>
     <td>15.00</td><td>14.39</td><td>13.25</td>
     <td>43.48</td><td>86.63</td><td>20.92</td></tr>

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8)</code></td>
     <td>15.43</td><td>13.59</td><td>14.15</td>
     <td>47.90</td><td>102.11</td><td>23.55</td></tr>

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10)</code></td>
     <td>15.24</td><td>13.96</td><td>16.59</td>
     <td>56.18</td><td>117.10</td><td>30.24</td></tr>

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15)</code></td>
     <td>15.43</td><td>16.28</td><td>21.54</td>
     <td>88.94</td><td>165.20</td><td>49.68</td></tr>

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20)</code></td>
     <td>15.55</td><td>18.59</td><td>21.12</td>
     <td>142.20</td><td>203.77</td><td>53.89</td></tr>

 <tr><td>25 threads</td>
     <td><code>(dining-philosophers 25)</code></td>
     <td></td><td></td><td></td>
     <td>188.54</td><td></td><td></td></tr>

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30)</code></td>
     <td>15.51</td><td>15.84</td><td>16.01</td>
     <td>211.86</td><td>235.94</td><td>57.64</td></tr>

 <tr><td>35 threads</td>
     <td><code>(dining-philosophers 35)</code></td>
     <td></td><td></td><td></td>
     <td>260.61</td><td></td><td></td></tr>

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40)</code></td>
     <td>15.50</td><td>20.16</td><td>15.20</td>
     <td>278.75</td><td>254.62</td><td>58.34</td></tr>

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50)</code></td>
     <td>15.42</td><td>16.34</td><td>19.27</td>
     <td>272.33</td><td>262.67</td><td>58.98</td></tr>

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100)</code></td>
     <td>15.51</td><td></td><td></td>
     <td>275.22</td><td>274.80</td><td></td></tr>

 <tr><td>200 threads</td>
     <td><code>(dining-philosophers 200)</code></td>
     <td>15.53</td><td></td><td></td>
     <td>284.21</td><td>277.47</td><td></td></tr>

</table>
