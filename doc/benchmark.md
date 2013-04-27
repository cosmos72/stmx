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

Hardware: Intel Core-i5 750 @4.0 GHz, 16GB RAM

Software: Debian GNU/Linux 7 (wheezy) x86_64, SBCL 1.1.6 x86_64, STMX 1.2.0


<table>
 <tr><th><b>name</b>      </th>
     <th><b>executed code</b></th>
     <th><b>average time</b></th></tr>

 <tr><td>atomic nil       </td><td><code>(atomic nil)</code>                </td><td>0.148&nbsp;microseconds</td></tr>
 <tr><td>atomic read-1    </td><td><code>(atomic ($ v))</code>              </td><td>0.270&nbsp;microseconds</td></tr>
 <tr><td>atomic write-1   </td><td><code>(atomic (setf ($ v) i))</code>     </td><td>0.381&nbsp;microseconds</td></tr>
 <tr><td>atomic read-write-1</td><td><code>(atomic (incf ($ v)))</code>     </td><td>0.547&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf ($ v))))</code></td>
     <td>0.821&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf ($ v))))</code></td>
     <td>3.534&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td><td>(0.518+N*0.030)&nbsp;microseconds</td></tr>

 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code>           </td><td>0.122&nbsp;microseconds</td></tr>
 <tr><td>orelse unary     </td><td><code>(atomic (orelse ($ v)))</code>     </td><td>0.578&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-1   </td><td><code>(atomic (orelse (retry) ($ v)))</code> </td><td>1.101&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-2   </td><td><code>(atomic (orelse (retry) (retry) ($ v)))</code> </td><td>1.543&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-4   </td><td><code>(atomic (orelse (retry) (retry) (retry) (retry) ($ v)))</code></td><td>2.309&nbsp;microseconds</td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td><td>(0.718+N*0.400)&nbsp;microseconds</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-bmap tm 1)))</code></td>
     <td>1.667&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>7.593&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>11.204&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>14.007&nbsp;microseconds</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-thash th 'x)))</code></td>
     <td>2.983&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>3.660&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>9.017&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i  1000)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>60.299&nbsp;microseconds</td></tr>

</table>
