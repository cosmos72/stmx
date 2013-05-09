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

Date: 05 May 2013

Hardware: Intel Core-i5 750 @4.0 GHz (quad-core), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.7 (x86_64), STMX 1.3.1


<table>

 <tr><th colspan="3">
       Single-thread benchmarks, executed one million times
       with <code>(x3 (1m (atomic ...)))</code>
     </th></tr>

 <tr><th><b>name</b>      </th>
     <th><b>executed code</b></th>
     <th><b>average time</b></th></tr>

 <tr><td>atomic nil       </td><td><code>(atomic nil)</code>                </td><td>0.086&nbsp;microseconds</td></tr>
 <tr><td>atomic read-1    </td><td><code>(atomic (fast-$ v))</code>              </td><td>0.107&nbsp;microseconds</td></tr>
 <tr><td>atomic write-1   </td><td><code>(atomic (setf (fast-$ v) i))</code>     </td><td>0.130&nbsp;microseconds</td></tr>
 <tr><td>atomic read-write-1</td><td><code>(atomic (incf (fast-$ v)))</code>     </td><td>0.165&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf (fast-$ v))))</code></td>
     <td>0.309&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf (fast-$ v))))</code></td>
     <td>1.718&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-1000</td>
     <td><code>(atomic (dotimes (j 1000) (incf (fast-$ v))))</code></td>
     <td>15.693&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td><td>(0.159+N*0.016)&nbsp;microseconds</td></tr>

 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code>           </td><td>0.062&nbsp;microseconds</td></tr>
 <tr><td>orelse unary     </td><td><code>(atomic (orelse (fast-$ v)))</code>     </td><td>0.269&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-1   </td><td><code>(atomic (orelse (retry) (fast-$ v)))</code> </td><td>0.524&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-2   </td><td><code>(atomic (orelse (retry) (retry) (fast-$ v)))</code> </td><td>0.749&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-4   </td><td><code>(atomic (orelse (retry) (retry) (retry) (retry) (fast-$ v)))</code></td><td>1.213&nbsp;microseconds</td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td><td>(0.291+N*0.230)&nbsp;microseconds</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-bmap tm 1)))</code></td>
     <td>0.778&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>5.585&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>7.782&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>9.324&nbsp;microseconds</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-thash th 'x)))</code></td>
     <td>1.779&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>2.300&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>7.489&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i  1000)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>58.723&nbsp;microseconds</td></tr>

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
     <td>2.67&nbsp;millions</td></tr>

 <tr><td>2 threads</td>
     <td><code>(dining-philosophers 2 1000000)</code></td>
     <td>3.94&nbsp;millions</td></tr>

 <tr><td>3 threads</td>
     <td><code>(dining-philosophers 3 1000000)</code></td>
     <td>5.15&nbsp;millions</td></tr>

 <tr><td>4 threads</td>
     <td><code>(dining-philosophers 4 1000000)</code></td>
     <td>5.98&nbsp;millions</td></tr>

 <tr><td>5 threads</td>
     <td><code>(dining-philosophers 5 1000000)</code></td>
     <td>5.98&nbsp;millions</td></tr>

 <tr><td>6 threads</td>
     <td><code>(dining-philosophers 6 1000000)</code></td>
     <td>6.32&nbsp;millions</td></tr>

 <tr><td>7 threads</td>
     <td><code>(dining-philosophers 7 1000000)</code></td>
     <td>6.69&nbsp;millions</td></tr>

 <tr><td>8 threads</td>
     <td><code>(dining-philosophers 8 1000000)</code></td>
     <td>6.98&nbsp;millions</td></tr>

 <tr><td>10 threads</td>
     <td><code>(dining-philosophers 10 1000000)</code></td>
     <td>7.01&nbsp;millions</td></tr>

 <tr><td>15 threads</td>
     <td><code>(dining-philosophers 15 1000000)</code></td>
     <td>7.37&nbsp;millions</td></tr>

 <tr><td>20 threads</td>
     <td><code>(dining-philosophers 20 1000000)</code></td>
     <td>7.29&nbsp;millions</td></tr>

 <tr><td>30 threads</td>
     <td><code>(dining-philosophers 30 1000000)</code></td>
     <td>7.38&nbsp;millions</td></tr>

 <tr><td>40 threads</td>
     <td><code>(dining-philosophers 40 1000000)</code></td>
     <td>7.53&nbsp;millions</td></tr>

 <tr><td>50 threads</td>
     <td><code>(dining-philosophers 50 1000000)</code></td>
     <td>7.48&nbsp;millions</td></tr>

 <tr><td>100 threads</td>
     <td><code>(dining-philosophers 100 1000000)</code></td>
     <td>7.40&nbsp;millions</td></tr>


</table>


