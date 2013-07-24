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
        (defmacro x3 (&rest body)
          `(dotimes (,(gensym) 3)
             ,@body))
        (defmacro 1m (&rest body)
          `(time (dotimes (i 1000000)
             ,@body)))
        (defvar v (tvar 0))
        (defvar m  (new 'rbmap :pred #'fixnum<)) 
        (defvar tm (new 'tmap  :pred #'fixnum<)) 
        (defvar h  (new 'ghash-table :test #'fixnum= :hash #'identity)) 
        (defvar th (new 'thash-table :test #'fixnum= :hash #'identity)) 
        ;; some initial values
        (setf (get-gmap m 1) 0)
        (setf (get-gmap tm 1) 0)
        (setf (get-ghash h 1) 0)
        (setf (get-ghash th 1) 0)

3. to warm-up STMX and the common-lisp process before starting the benchmarks,
   it is also recommended to run first the test suite with:

        (ql:quickload "stmx.test")
        (fiveam:run! 'stmx.test:suite)

4. Run each benchmark one million times (see `1m` macro above) in a single
   thread. Repeat each run three times (see `3x` macro above) and take the lowest
   of the three reported elapsed times. Divide by one million to get the average
   elapsed real time per iteration.

   This means for example that to run the benchmark `(atomic ($-tx v))` one has to type
   `(x3 (1m (atomic ($-tx v))))`

All timings reported in the next section are the output on the author's system
of the procedure just described, and thus for each benchmark they contain
the average elapsed real time per iteration,
i.e. the total elapsed time divided by the number of iterations (one million).


Benchmark results
-----------------

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 3 July 2013

Hardware: Intel Core-i7 4770 @3.5 GHz (quad-core w/ hyper-threading), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.8 (x86_64), STMX 1.3.3


<table>

 <tr><th colspan="5">
       Single-thread benchmarks, executed one million times
       with <code>(x3 (1m (atomic ...)))</code>
     </th></tr>

 <tr><th rowspan="2"><b>name</b>      </th>
     <th rowspan="2"><b>executed code</b></th>
     <th>STMX STABLE (sw-only transactions)</th>
     <th>STMX EXPERIMENTAL (hybrid hw+sw transactions)</th>
     <th>HAND-OPTIMIZED hw transactions - see doc/benchmark.lisp</th></tr>

 <tr><th colspan="2"><b>average time in microseconds</b></th></tr>


 <tr><td>atomic nil       </td><td><code>(atomic nil)</code></td>
     <td>0.069</td><td>0.021</td><td>0.012</td></tr>
     <!-- laptop 0.153; gv6 0.159 -->

 <tr><td>atomic read-1    </td><td><code>(atomic ($ v))</code></td>
     <td>0.082</td><td>0.021</td>0.021<td></td></tr>
     <!-- laptop 0.187; gv6 0.195 -->

 <tr><td>atomic write-1   </td><td><code>(atomic (setf ($ v) 1))</code></td>
     <td>0.108</td><td>0.025</td><td>0.022</td></tr>
     <!-- laptop 0.277; gv6 0.257 -->

 <tr><td>atomic read-write-1</td><td><code>(atomic (incf (the fixnum ($ v))))</code></td>
     <td>0.135</td><td>0.026</td><td>0.022</td></tr>
     <!-- laptop 0.339; gv6 0.596 -->

 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf (the fixnum ($ v)))))</code></td>
     <td>0.239</td><td>0.054</td><td>0.034</td></tr>
     <!-- laptop 0.686; gv6 0.971 -->

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf (the fixnum ($ v)))))</code></td>
     <td>1.118</td><td>0.382</td><td>0.185</td></tr>
     <!-- laptop 3.703; gv6 4.160 -->

 <tr><td>atomic read-write-1000</td>
     <td><code>(atomic (dotimes (j 1000) (incf (the fixnum ($ v)))))</code></td>
     <td>9.922</td><td>3.617</td><td>1.686</td></tr>
     <!-- laptop 33.070; gv6 34.607 -->

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td>
     <td>(0.142+N*0.0098)</td><td>(0.0201+N*0.0036)</td><td>(0.0177+N*0.00167)</td></tr>

 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code></td>
     <td>0.043</td><td>0.024</td><td>0.021</td></tr>

 <tr><td>orelse unary     </td><td><code>(atomic (orelse ($-tx v)))</code></td>
     <td>0.234</td><td></td></tr>

 <tr><td>orelse retry-1   </td><td><code>(atomic (orelse (retry) ($-tx v)))</code></td>
     <td>0.429</td><td></td></tr>

 <tr><td>orelse retry-2   </td><td><code>(atomic (orelse (retry) (retry) ($-tx v)))</code></td>
     <td>0.601</td><td></td></tr>

 <tr><td>orelse retry-4   </td><td><code>(atomic (orelse (retry)<br/>
                                         (retry) (retry) (retry) ($-tx v)))</code></td>
     <td>0.963</td><td></td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td>
     <td>(0.248+N*0.178)</td><td></td></tr>

 <tr><td>tmap read-1</td>
     <td><code>(atomic (get-gmap tm 1))</code></td>
     <td></td><td>0.172</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-gmap tm 1)))</code></td>
     <td>0.531</td><td>0.364</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-gmap tm))<br>
               (set-gmap tm i t))</code></td>
     <td>3.882</td><td></td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>5.392</td><td></td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>6.443</td><td></td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-ghash th 1)))</code></td>
     <td>0.674</td><td></td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>2.024</td><td></td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>1.913</td><td></td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i  1000)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>1.933</td><td></td></tr>

 </table>


 <table>
 
 <tr><th colspan="7">
       Concurrent benchmarks on a 4-core CPU. They already iterate
       ten million times, do not wrap them in <code>(1m ...)</code>.
     </th></tr>

 <tr><th colspan="7">
       Dining philosophers, load with<br>
       <code>(load "stmx/example/dining-philosophers-stmx.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers-hw-tx.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers-lock.lisp")</code><br>
       <code>(in-package :stmx.example{1|2|3})</code>
     </th></tr>

 <tr><th rowspan="2"><b>number of threads</b></th>
     <th rowspan="2"><b>executed code</b></th>
     <th><b>STMX (sw transactions)</b></th>
     <th><b>STMX EXPERIMENTAL (hw transactions + sw fallback)</b></th>
     <th><b>SB-TRANSACTION (hw transactions)</b></th>
     <th><b>LOCK (atomic compare-and-swap)</b></th>
     <th><b>LOCK (bordeaux-threads mutex)</b></th></tr>

 <tr><th colspan="5"><b>millions transactions per second</b></th></tr>

 <tr><td>1 thread</td>
     <td><code>(dining-philosophers 1)</code></td>
     <td>4.78</td><td></td><td>50.00</td><td>71.43</td><td>15.67</td></tr>

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2)</code></td>
     <td>8.39</td><td></td><td>39.18</td><td>60.42</td><td>11.80</td></tr>

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3)</code></td>
     <td>12.10</td><td></td><td>31.51</td><td>49.02</td><td>10.25</td></tr>

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4)</code></td>
     <td>16.15</td><td></td><td>32.73</td><td>48.60</td><td>15.17</td></tr>

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5)</code></td>
     <td>14.55</td><td></td><td>39.06</td><td>61.35</td><td>18.01</td></tr>

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6)</code></td>
     <td>16.43</td><td></td><td>45.91</td><td>75.66</td><td>21.03</td></tr>

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7)</code></td>
     <td>16.85</td><td></td><td>55.56</td><td>90.09</td><td>24.30</td></tr>

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8)</code></td>
     <td>17.79</td><td></td><td>72.86</td><td>102.70</td><td>25.56</td></tr>

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10)</code></td>
     <td>17.26</td><td></td><td>76.75</td><td>121.51</td><td>32.39</td></tr>

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15)</code></td>
     <td>17.28</td><td></td><td>135.75</td><td>164.84</td><td>51.62</td></tr>

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20)</code></td>
     <td>17.69</td><td></td><td>205.55</td><td>205.55</td><td>57.95</td></tr>

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30)</code></td>
     <td>17.57</td><td></td><td>249.58</td><td>240.38</td><td>59.48</td></tr>

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40)</code></td>
     <td>17.63</td><td></td><td>242.72</td><td>250.78</td><td>57.97</td></tr>

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50)</code></td>
     <td>17.59</td><td></td><td>262.33</td><td>244.38</td><td>55.43</td></tr>

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100)</code></td>
     <td>17.65</td><td></td><td>269.91</td><td>234.25</td><td>50.12</td></tr>

 <tr><td>200 threads</td>
     <td><code>(dining-philosophers 200)</code></td>
     <td>17.67</td><td></td><td>278.20</td><td>254.58</td><td>51.68</td></tr>

</table>
