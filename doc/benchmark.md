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

All timings reported in the next secion are the output on the author's system
of the procedure just described, and thus for each benchmark they contain
the average elapsed real time per iteration,
i.e. the total elapsed time divided by the number of iterations (one million).


Benchmark results
-----------------

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 15 May 2013

Hardware: Intel Core-i5 750 @4.0 GHz (quad-core), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.6 (x86_64), STMX 1.3.2


<table>

 <tr><th colspan="3">
       Single-thread benchmarks, executed one million times
       with <code>(x3 (1m (atomic ...)))</code>
     </th></tr>

 <tr><th><b>name</b>      </th>
     <th><b>executed code</b></th>
     <th><b>average time</b></th></tr>

 <tr><td>atomic nil       </td><td><code>(atomic nil)</code>              </td><td>0.083&nbsp;microseconds</td></tr>
 <tr><td>atomic read-1    </td><td><code>(atomic ($-tx v))</code>         </td><td>0.102&nbsp;microseconds</td></tr>
 <tr><td>atomic write-1   </td><td><code>(atomic (setf ($-tx v) 1))</code></td><td>0.125&nbsp;microseconds</td></tr>
 <tr><td>atomic read-write-1</td><td><code>(atomic (incf ($-tx v)))</code></td><td>0.156&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf ($-tx v))))</code></td>
     <td>0.300&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf ($-tx v))))</code></td>
     <td>1.702&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-1000</td>
     <td><code>(atomic (dotimes (j 1000) (incf ($-tx v))))</code></td>
     <td>15.675&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td><td>(0.147+N*0.016)&nbsp;microseconds</td></tr>

 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code>           </td><td>0.060&nbsp;microseconds</td></tr>
 <tr><td>orelse unary     </td><td><code>(atomic (orelse ($-tx v)))</code>     </td><td>0.262&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-1   </td><td><code>(atomic (orelse (retry) ($-tx v)))</code> </td><td>0.510&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-2   </td><td><code>(atomic (orelse (retry) (retry) ($-tx v)))</code> </td><td>0.732&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-4   </td><td><code>(atomic (orelse (retry) (retry) (retry) (retry) ($-tx v)))</code></td><td>1.170&nbsp;microseconds</td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td><td>(0.296+N*0.220)&nbsp;microseconds</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-gmap tm 1)))</code></td>
     <td>0.686&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>5.373&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>7.376&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-gmap tm))<br>
              (set-gmap tm i t))</code></td>
     <td>8.707&nbsp;microseconds</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-ghash th 1)))</code></td>
     <td>0.977&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>2.891&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>2.734&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i  1000)) (clear-ghash th))<br>
              (set-ghash th i t))</code></td>
     <td>2.732&nbsp;microseconds</td></tr>

 <tr><th colspan="3">
       Concurrent benchmarks on a 4-core CPU. They already iterate
       one million times, do not wrap them in <code>(1m ...)</code>.
     </th></tr>

 <tr><th colspan="3">
       Dining philosophers, load with<br>
       <code>(load "stmx/examples/dining-philosophers.lisp")</code><br>
       <code>(in-package :stmx.example1)</code>
     </th></tr>

 <tr><th><b>number of threads</b></th>
     <th><b>executed code</b></th>
     <th><b>total transactions per second</b></th></tr>

 <tr><td>1 thread</td>
     <td><code>(dining-philosophers 1 1000000)</code></td>
     <td>3.70&nbsp;millions</td></tr> <!-- lock: 76.92 -->

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2 1000000)</code></td>
     <td>6.78&nbsp;millions</td></tr> <!-- lock: 66.67 -->

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3 1000000)</code></td>
     <td>9.87&nbsp;millions</td></tr> <!-- lock: 54.55 -->

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4 1000000)</code></td>
     <td>12.99&nbsp;millions</td></tr> <!-- lock: 59.70 -->

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5 1000000)</code></td>
     <td>11.39&nbsp;millions</td></tr> <!-- lock: 89.29 -->

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6 1000000)</code></td>
     <td>12.79&nbsp;millions</td></tr> <!-- lock: 115.38 -->

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7 1000000)</code></td>
     <td>12.59&nbsp;millions</td></tr> <!-- lock: 118.64 -->

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8 1000000)</code></td>
     <td>13.14&nbsp;millions</td></tr> <!-- lock: 131.15 -->

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10 1000000)</code></td>
     <td>12.84&nbsp;millions</td></tr> <!-- lock: 153.85 -->

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15 1000000)</code></td>
     <td>13.00&nbsp;millions</td></tr> <!-- lock: 148.51 -->

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20 1000000)</code></td>
     <td>13.40&nbsp;millions</td></tr> <!-- lock: 148.15 -->

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30 1000000)</code></td>
     <td>13.00&nbsp;millions</td></tr> <!-- lock: 144.93 -->

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40 1000000)</code></td>
     <td>13.39&nbsp;millions</td></tr> <!-- lock: 138.41 -->

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50 1000000)</code></td>
     <td>12.93&nbsp;millions</td></tr> <!-- lock: 142.86 -->

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100 1000000)</code></td>
     <td>13.37&nbsp;millions</td></tr> <!-- lock: 142.05 -->

 <tr><td>200 threads</td>
     <td><code>(dining-philosophers 200 1000000)</code></td>
     <td>13.34&nbsp;millions</td></tr> <!-- lock: 144.72 -->


</table>


