STMX Performance
----------------

As for any software, the topic of performance is often sensitive and
plagued with heated discussions. It is objectively difficult to come up with
scientifically accurate figures as they depend on many factors, including at least
hardware, operating system, common lisp implementation, optimization flags and usage pattern.

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 9 April 2013

Hardware: Intel Core-i5 750 @4.0 GHz, 16GB RAM

Software: Debian GNU/Linux 7 (wheezy) x86_64, SBCL 1.0.57.0.debian x86_64, STMX 0.9.3

Optimization flags and setup:

    (declaim (optimize (compilation-speed 0) (space 0) (debug 0) (safety 0) (speed 3)))
    (ql:quickload "stmx")
    (in-package :stmx.util)
    (defmacro one-million (&rest body)
      `(time (dotimes (i 1000000)
              ,@body)))
    (defvar v (new 'tvar :value 0))
    (defvar m  (new 'rbmap :pred #'fixnum<)) 
    (defvar tm (new 'tmap  :pred #'fixnum<)) 
    (defvar h  (make-hash-table))  
    (defvar th (new 'thash-table)) 
    ;; some initial values
    (set-bmap m 1 0)
    (set-bmap tm 1 0)
    (setf (gethash   'x h)  0)
    (setf (get-thash th 'x) 0)

For each benchmark, a loop runs the code shown one million times (see `one-million` macro above)
in a single thread and the best of three loops is used.
All reported times are the average elapsed real time per iteration, i.e. the total elapsed time
divided by the number of iterations (one million).

<table>
 <tr><th><b>name</b>      </th>
     <th><b>executed code</b></th>
     <th><b>average time</b></th></tr>

 <tr><td>atomic           </td><td><code>(atomic)</code>                    </td><td>0.001&nbsp;microseconds</td></tr>
 <tr><td>atomic nil       </td><td><code>(atomic nil)</code>                </td><td>0.234&nbsp;microseconds</td></tr>
 <tr><td>atomic read-1    </td><td><code>(atomic ($ v))</code>              </td><td>0.404&nbsp;microseconds</td></tr>
 <tr><td>atomic write-1   </td><td><code>(atomic (setf ($ v) i))</code>     </td><td>0.696&nbsp;microseconds</td></tr>
 <tr><td>atomic read-write-1</td><td><code>(atomic (incf ($ v)))</code>     </td><td>0.950&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf ($ v))))</code></td>
     <td>1.277&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf ($ v))))</code></td>
     <td>4.768&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td><td>(0.890+N*0.039)&nbsp;microseconds</td></tr>

 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code>           </td><td>0.223&nbsp;microseconds</td></tr>
 <tr><td>orelse unary     </td><td><code>(atomic (orelse ($ v)))</code>     </td><td>0.930&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-1   </td><td><code>(atomic (orelse (retry) ($ v)))</code> </td><td>1.677&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-2   </td><td><code>(atomic (orelse (retry) (retry) ($ v)))</code> </td><td>2.423&nbsp;microseconds</td></tr>
 <tr><td>orelse retry-4   </td><td><code>(atomic (orelse (retry) (retry) (retry) (retry) ($ v)))</code></td><td>3.575&nbsp;microseconds</td></tr>

 <tr><td>orelse retry-N   </td><td>best fit of the 3 runs above</td><td>(1.001+N*0.624)&nbsp;microseconds</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-bmap tm 1)))</code></td>
     <td>2.539&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>11.197&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>17.284&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-bmap tm))<br>
              (set-bmap tm i t))</code></td>
     <td>22.407&nbsp;microseconds</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-thash th 'x)))</code></td>
     <td>4.693&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>5.057&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>10.613&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i  1000)) (clear-thash th))<br>
              (set-thash th i t))</code></td>
     <td>61.901&nbsp;microseconds</td></tr>

</table>
