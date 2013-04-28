STMX Performance
----------------

As for any software, the topic of performance is often sensitive and
plagued with heated discussions. It is objectively difficult to come up with
scientifically accurate figures as they depend on many factors, including at least
hardware, operating system, common lisp implementation, optimization flags and usage pattern.


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
    (defvar h  (make-hash-table))  
    (defvar th (new 'thash-table)) 
    ;; some initial values
    (set-bmap m 1 0)
    (set-bmap tm 1 0)
    (setf (gethash   'x h)  0)
    (setf (get-thash th 'x) 0)

3. to warm-up STMX and the common-lisp process before starting the benchmarks,
   it is also recommended to run first the test suite with:

    (ql:quickload "stmx.test")
    (fiveam:run! 'stmx.test:suite)

4. Run each benchmark one million times (see `1m` macro above) in a single
   thread. Repeat each run three times (see `3x` macro above) and take the lowest
   of the three reported elapsed times. Divide by one million to get the average
   elapsed real time per iteration.

   This means for example that to run the benchmark `(atomic ($ v))` one has to type
   `(x3 (1m (atomic ($ v))))`

All timings reported in the next secion are the output on the author's system
of the procedure just described, and thus for each benchmark they contain
the average elapsed real time per iteration,
i.e. the total elapsed time divided by the number of iterations (one million).


Benchmark results
-----------------

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 27 April 2013

Hardware: Intel Core-i5 750 @4.0 GHz (quad-core), 16GB RAM

Software: Debian GNU/Linux 7 (wheezy) x86_64, SBCL 1.1.6 x86_64, STMX 1.2.1


<table>

 <tr><th colspan="3">
       Single-thread benchmarks, executed one million times
       with <code>(x3 (1m (atomic ...)))</code>
     </th></tr>

 <tr><th><b>name</b>      </th>
     <th><b>executed code</b></th>
     <th><b>average time</b></th></tr>

 <tr><td>atomic nil       </td><td><code>(atomic nil)</code>                </td><td>0.144&nbsp;microseconds</td></tr>
 <tr><td>atomic read-1    </td><td><code>(atomic ($ v))</code>              </td><td>0.267&nbsp;microseconds</td></tr>
 <tr><td>atomic write-1   </td><td><code>(atomic (setf ($ v) i))</code>     </td><td>0.326&nbsp;microseconds</td></tr>
 <tr><td>atomic read-write-1</td><td><code>(atomic (incf ($ v)))</code>     </td><td>0.468&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf ($ v))))</code></td>
     <td>0.746&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf ($ v))))</code></td>
     <td>3.446&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-1000</td>
     <td><code>(atomic (dotimes (j 1000) (incf ($ v))))</code></td>
     <td>31.757&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td><td>(0.373+N*0.031)&nbsp;microseconds</td></tr>

 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code>           </td><td>0.119&nbsp;microseconds</td></tr>
 <tr><td>orelse unary     </td><td><code>(atomic (orelse ($ v)))</code>     </td><td>0.587&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-1   </td><td><code>(atomic (orelse (retry) ($ v)))</code> </td><td>1.082&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-2   </td><td><code>(atomic (orelse (retry) (retry) ($ v)))</code> </td><td>1.439&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-4   </td><td><code>(atomic (orelse (retry) (retry) (retry) (retry) ($ v)))</code></td><td>2.192&nbsp;microseconds</td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td><td>(0.705+N*0.371)&nbsp;microseconds</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-bmap tm 1)))</code></td>
     <td>1.362&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>6.646&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>9.211&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>11.094&nbsp;microseconds</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-thash th 'x)))</code></td>
     <td>2.539&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>3.167&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>8.497&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i  1000)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>60.090&nbsp;microseconds</td></tr>


 <tr><th colspan="3">
       Concurrent benchmarks on a 4-core CPU. They already iterate
       one million times, do not wrap them in <code>(1m ...)</code>
     </th></tr>

 <tr><th colspan="3">
       Dining philosophers, load with<br>
       <code>(load "stmx/examples/dining-philosophers.lisp")</code><br>
       <code>(in-package :stmx.example1)</code>
     </th></tr>

 <tr><th><b>number of threads</b></th>
     <th><b>executed code</b></th>
     <th><b>average transactions per second</b></th></tr>

 <tr><td>1 thread</td>
     <td><code>(dining-philosophers 1 1000000)</code></td>
     <td>1.10&nbsp;millions</td></tr>

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2 1000000)</code></td>
     <td>1.77&nbsp;millions</td></tr>

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3 1000000)</code></td>
     <td>2.49&nbsp;millions</td></tr>

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4 1000000)</code></td>
     <td>3.05&nbsp;millions</td></tr>

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5 1000000)</code></td>
     <td>2.97&nbsp;millions</td></tr>

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6 1000000)</code></td>
     <td>3.16&nbsp;millions</td></tr>

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7 1000000)</code></td>
     <td>3.07&nbsp;millions</td></tr>

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8 1000000)</code></td>
     <td>2.96&nbsp;millions</td></tr>

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10 1000000)</code></td>
     <td>3.01&nbsp;millions</td></tr>

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15 1000000)</code></td>
     <td>2.83&nbsp;millions</td></tr>

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20 1000000)</code></td>
     <td>3.15&nbsp;millions</td></tr>

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30 1000000)</code></td>
     <td>2.97&nbsp;millions</td></tr>

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40 1000000)</code></td>
     <td>2.97&nbsp;millions</td></tr>

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50 1000000)</code></td>
     <td>2.98&nbsp;millions</td></tr>

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100 1000000)</code></td>
     <td>2.91&nbsp;millions</td></tr>


</table>


