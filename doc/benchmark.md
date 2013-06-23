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

Date: 18 June 2013

Hardware: Intel Core-i7 4770 @3.5 GHz (quad-core w/ hyper-threading), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.8 (x86_64), STMX 1.3.3


<table>

 <tr><th colspan="3">
       Single-thread benchmarks, executed one million times
       with <code>(x3 (1m (atomic ...)))</code>
     </th></tr>

 <tr><th><b>name</b>      </th>
     <th><b>executed code</b></th>
     <th><b>average time</b></th></tr>

 <tr><td>atomic nil       </td><td><code>(atomic nil)</code>              </td><td>0.069&nbsp;microseconds</td></tr>
 <tr><td>atomic read-1    </td><td><code>(atomic ($-tx v))</code>         </td><td>0.081&nbsp;microseconds</td></tr>
 <tr><td>atomic write-1   </td><td><code>(atomic (setf ($-tx v) 1))</code></td><td>0.103&nbsp;microseconds</td></tr>
 <tr><td>atomic read-write-1</td><td><code>(atomic (incf ($-tx v)))</code></td><td>0.127&nbsp;microseconds</td></tr>

<!-- Intel Core-i5 750 @4GHz (quad-core), 16GB RAM -->

<!-- sbcl64, speed 3, atomic-ops (default)
    nil               0.083
    ($-tx v)          0.102
    (setf ($-tx v) 1) 0.125
    (incf ($-tx v))   0.156     -->

<!-- sbcl64, speed 3
    nil               0.126
    ($-tx v)          0.146
    (setf ($-tx v) 1) 0.239
    (incf ($-tx v))   0.276     -->

<!-- ccl64, speed 3
    nil               0.271
    ($-tx v)          0.341
    (setf ($-tx v) 1) 0.688
    (incf ($-tx v))   0.802     -->


 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf ($-tx v))))</code></td>
     <td>0.234&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf ($-tx v))))</code></td>
     <td>1.100&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-1000</td>
     <td><code>(atomic (dotimes (j 1000) (incf ($-tx v))))</code></td>
     <td>9.807&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td><td>(0.135+N*0.0097)&nbsp;microseconds</td></tr>

 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code>           </td><td>0.045&nbsp;microseconds</td></tr>
 <tr><td>orelse unary     </td><td><code>(atomic (orelse ($-tx v)))</code>     </td><td>0.230&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-1   </td><td><code>(atomic (orelse (retry) ($-tx v)))</code> </td><td>0.428&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-2   </td><td><code>(atomic (orelse (retry) (retry) ($-tx v)))</code> </td><td>0.596&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-4   </td><td><code>(atomic (orelse (retry) (retry) (retry) (retry) ($-tx v)))</code></td><td>0.951&nbsp;microseconds</td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td><td>(0.251+N*0.175)&nbsp;microseconds</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-gmap tm 1)))</code></td>
     <td>0.518&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>3.775&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>5.257&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>6.311&nbsp;microseconds</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-ghash th 1)))</code></td>
     <td>0.796&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>2.026&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>1.896&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i  1000)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>1.881&nbsp;microseconds</td></tr>

 </table>


 <table>
 
 <tr><th colspan="5">
       Concurrent benchmarks on a 4-core CPU. They already iterate
       one million times, do not wrap them in <code>(1m ...)</code>.
     </th></tr>

 <tr><th colspan="5">
       Dining philosophers, load with<br>
       <code>(load "stmx/example/dining-philosophers-stmx.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers-lock.lisp")</code><br>
       <code>(load "stmx/example/dining-philosophers-rtm.lisp")</code><br>
       <code>(in-package :stmx.example{1|2|3})</code>
     </th></tr>

 <tr><th rowspan="2"><b>number of threads</b></th>
     <th rowspan="2"><b>executed code</b></th>
     <th><b>STMX (sw transactions)</b></th>
     <th><b>LOCK based</b></th>
     <th><b>RTM (hw transactions)</b></th></tr>

 <tr><th colspan="3"><b>millions transactions per second</b></th></tr>

 <tr><td>1 thread</td>
     <td><code>(dining-philosophers 1)</code></td>
     <td>4.72</td><td>69.06</td><td>51.55</td></tr>

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2)</code></td>
     <td>8.44</td><td>55.71</td><td>38.10</td></tr>

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3)</code></td>
     <td>12.35</td><td>49.02</td><td>30.90</td></tr>

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4)</code></td>
     <td>16.39</td><td>47.11</td><td>32.50</td></tr>

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5)</code></td>
     <td>13.70</td><td>61.35</td><td>40.02</td></tr>

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6)</code></td>
     <td>15.71</td><td>75.66</td><td>42.85</td></tr>

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7)</code></td>
     <td>16.17</td><td>90.09</td><td>49.66</td></tr>

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8)</code></td>
     <td>17.24</td><td>102.70</td><td>65.40</td></tr>

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10)</code></td>
     <td>16.67</td><td>121.51</td><td>72.67</td></tr>

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15)</code></td>
     <td>16.63</td><td>156.09</td><td>128.47</td></tr>

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20)</code></td>
     <td>17.20</td><td>202.84</td><td>176.65</td></tr>

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30)</code></td>
     <td>17.06</td><td>231.84</td><td>227.74</td></tr>

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40)</code></td>
     <td>17.23</td><td>242.72</td><td>251.51</td></tr>

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50)</code></td>
     <td>17.21</td><td>232.99</td><td>257.19</td></tr>

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100)</code></td>
     <td>17.40</td><td>232.99</td><td>272.31</td></tr>

 <tr><td>200 threads</td>
     <td><code>(dining-philosophers 200)</code></td>
     <td>17.38</td><td>248.69</td><td>278.75</td></tr>

</table>
