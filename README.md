STMX
======

Summary
-------

STMX is a high-performance implementation of composable Software Transactional
Memory (STM) for Common Lisp. STM is a concurrency control mechanism aimed
at making concurrent programming easier to write and understand. Instead of
traditional lock-based programming, one programs with atomic transactions.
Atomic transactions can be composed together to make larger atomic transactions.

A transaction gets committed if it returns normally, while it gets rolled
back if it signals an error (and the error is propagated to the caller).

Finally, transactions can safely run in parallel in different threads,
are re-executed from the beginning in case of conflicts, and effects
of a transaction are not visible from other threads until it commits.

STM gives freedom from deadlocks, automatic roll-back on failure,
and it aims at resolving the tension between granularity and concurrency.

Installation and loading
------------------------

STMX is packaged with asdf. The simplest way to install it is to first
install [Quicklisp](http://www.quicklisp.org), as it can automatically
resolve and download STMX dependencies.

Once quicklisp is installed, download STMX from GitHub and save it
into your Quicklisp local-projects folder with the commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/stmx.git

then load a REPL and run:

    CL-USER> (ql:quickload "stmx")
    ;; lots of output...
    CL-USER> (use-package :stmx)
     
If all goes well, it will automatically download and install STMX dependencies:

- `arnesi`
- `closer-mop`
- `bordeaux-threads`

then it will load STMX and be ready to use.

In case you get errors:

- check that quicklisp is installed correctly, for example by executing at REPL

        CL-USER> (ql:quickload "arnesi")

- check that you downloaded STMX creating an `stmx/` folder inside
  your Quicklisp local-projects folder, usually `~/quicklisp/local-projects`

After loading STMX, the test suite can be executed with:

    CL-USER> (ql:quickload "stmx.test")
    ;; lots of output...
    CL-USER> (fiveam:run! 'stmx.test:suite)

Note: `(ql:quickload "stmx.test")` intentionally works only *after*
`(ql:quickload "stmx")` has completed successfuly.

You are welcome to report any failure you get while running the test suite,
please include in the report:
- operating system name and version (example: Debian GNU/Linux x86_64 version 7.0)
- Common Lisp implementation and version (example: SBCL 1.0.57.0.debian, 64bit)
- EXACT output produced by the test suite
- any other relevant information

See "Contacts, help, discussion" below for the preferred method to send the report.


Documentation
-------------

[Composable Memory Transactions](http://research.microsoft.com/~simonpj/papers/stm/stm.pdf)
is a very good - though a bit technical - explanation of transactions and
how they are used and combined. For the interested reader, it also goes in
deep detail on how to actually implement memory transactions.

Basic usage
-----------

STMX offers the following Lisp macros and functions, also heavily documented
in the sources - remember `(describe 'some-symbol)` at REPL.

- `TRANSACTIONAL` declares that a class is transactional, i.e. that its
  slots contain transactional data. Use it to wrap a class definition:
  
        (transactional
          (defclass foo ()
            ((value1 :type integer :initarg value1 :accessor :value1-of)
             (value2 :type string  :initarg value2 :accessor :value2-of)
             ;; ...
            )))

  Note: on some Common Lisp implementations, `slot-value` and `(setf slot-value)`
  are known to ignore the transactional machinery (implemented with MOP
  slot-value-using-class, if you wonder) causing all kinds of bugs
  on transactional classes.
  To avoid hard-to-debug problems, always use accessors to read and write
  the slots of transactional classes.


- `TRANSACTION` declares that a method or function is an atomic
  transaction, and is actually just a convenience macro for `ATOMIC`.
  Use it to wrap a function definition:
  
        (transaction
          (defun show-foo (obj)
            (declare (type foo obj))
            (format t "atomic function show-foo: foo is ~A ~A~%"
              (value1-of obj) (value2-of obj))
            ;; ...
          ))
      
  or a method definition:
  
        (transaction
          (defmethod set-foo ((obj foo) value1 value2)
            (declare (type integer value1)
                     (type string value2))
            (setf (value1-of obj) value1)
            (setf (value2-of obj) value2)
            (format t "atomic method set-foo: foo is now ~A ~A~%" value1 value2)
            ;; ...
          ))

- `ATOMIC` is the main macro: it wraps Lisp forms into a transaction then executes them.
  The above functions and methods could also be written as:
  
        (defun show-foo (obj)
          (declare (type foo obj))
          (atomic
            (format t "atomic function show-foo: foo is ~A ~A~%"
              (value1-of obj) (value2-of obj))
            ;; ...
          ))
      
        (defmethod set-foo ((obj foo) value1 value2)
          (declare (type integer value1)
                   (type string value2))
          (atomic
            (setf (value1-of obj) value1)
            (setf (value2-of obj) value2)
            (format t "atomic method set-foo: foo is now ~A ~A~%" value1 value2)
            ;; ...
          ))
      
  with the difference that `atomic` is not limited to functions and
  methods: it can be used to wrap *any* list of forms (it contains an
  implicit `progn`).

- `RETRY` is a function. It is more tricky to understand, but really powerful.
  As described in the summary, transactions will commit if they return normally,
  while they will rollback if they signal an error or condition.

  The `(retry)` function call offers a third option: if invoked inside
  a transaction, it tells STMX that the transaction cannot complete
  immediately, for example because some necessary data is not currently
  available, and instructs STMX to wait until the data has changed,
  then re-execute the transaction from scratch.

  How does `(retry)` know which data it should monitor for changes?
  Simple: it will monitor *all* transactional data (including slots of
  transactional objects) that was read since the beginning of the
  transaction and until `(retry)` was invoked. 

  With `RETRY`, reliable communication among threads is (hopefully)
  extremely simple to implement: a thread can read one (or more)
  transactional data, checking for values that some other thread
  will write there, and just `(retry)` if no appropriate values are
  there yet.

- `ORELSE` is a macro to execute two or more Lisp forms as alternatives
  in separate, nested transactions: if the first retries or is invalid,
  the second will be executed and so on, until one transaction either
  commits (returns normally) or rollbacks (signals an error or condition).
  It can only be used inside a transaction.

- `NONBLOCKING` is an utility macro based on `ORELSE` to convert a blocking
  transaction into another that returns NIL instead of waiting
  (and otherwise returns T followed by the values or the original transaction)
  
        (nonblocking (x) (y) (z))
        
  basically expands to
  
        (orelse (values t (progn (x) (y) (z))) nil)
        
  with the difference that `(nonblocking ...)` actually captures all the values
  returned by the transaction, not just the first as in the example above.


Advanced usage
--------------

For those cases where the basic features are not sufficient, or where more
control is desired during the execution of transactional code, some advanced
features are available:

- `RUN-ATOMIC` is the function version of `ATOMIC`: takes a single function
  argument and executes it in a transaction. This means the following two code
  snippets are equivalent:

        (defvar a (make-instance 'foo))
        (defvar b (make-instance 'foo))
        (atomic
          (set-foo a 1 "abc")
          (set-foo b 2 "def"))

  and

        (defvar a (make-instance 'foo))
        (defvar b (make-instance 'foo))

        (defun init-foo-a-and-b ()
          (set-foo a 1 "abc")
          (set-foo b 2 "def"))

        (run-atomic #'init-foo-a-and-b)

- `RUN-ORELSE` is the function version of `ORELSE`: it accepts any number
  of functions and executes them as alternatives in separate nested transactions:
  if the first retries or is invalid, the second will be executed and so on,
  until one function either commits (returns normally) or rollbacks
  (signals an error or condition).

  If X, Y and Z are no-argument functions, the following two lines are equivalent:
  
        (orelse (x) (y) (z))
        (run-orelse #'x #'y #'z)

- `BEFORE-COMMIT` is a macro that registers Lisp forms to be executed later, just before
  the transaction tries to commit.
  It can be useful to normalize or simplify some transactional data, or perform any kind
  of bookkeeping activity. 

  Be aware that the transaction is not yet committed when the forms registered with
  BEFORE-COMMIT run. This means in particular:

  - If the forms signal an error when executed, the error is propagated to the caller,
    forms registered later with BEFORE-COMMIT are not executed, and the transaction rollbacks.

  - The forms can read and write normally to transactional memory,
    and in case of conflicts the whole transaction, _including_ all forms
    registered with BEFORE-COMMIT, is re-executed from the beginning.

  - The forms cannot (retry) - attempts to do so will signal an error.
    Starting a nested transaction and retrying inside that is acceptable,
    as long as the (retry) does not propagate outside the forms themselves.

- `AFTER-COMMIT` is another macro that registers Lisp forms to be executed later,
  but in this case they are executed immediately after the transaction has been
  successfully committed.
  It can be useful to notify some subsystem that for any reason cannot call (retry)
  to be informed of changes in transactional memory - for example because
  it is some existing code that one does not wish to modify.

  In this case, the transaction is already committed when the forms registered with
  AFTER-COMMIT run, and the forms are not allowed to change it further.
  This means there are stricter limitations on what the forms can do:

  - If the forms signal an error when executed, the error is propagated to the caller,
    forms registered later with AFTER-COMMIT are not executed, but the transaction
    stays committed.

  - The forms must not write to _any_ transactional memory: the consequences
    are undefined.

  - The forms can _only_ read from transactional memory previously read or written
    during the same transaction. Reading from other transactional memory
    has undefined consequences.

  - The forms cannot (retry) - attempts to do so will signal an error.
    Starting a nested transaction and retrying inside that is acceptable
    as long as the (retry) does not propagate outside the forms themselves
    and the nested transaction respects the two previous limitations.

- `CALL-BEFORE-COMMIT` is the function version of `BEFORE-COMMIT`: it accepts a single
   function and registers it to be executed before the transaction tries to commit.

- `CALL-AFTER-COMMIT` is the function version of `AFTER-COMMIT`: it accepts a single
   function and registers it to be executed after the transaction has been
   successfully committed.

- `TVAR` is the class implementing transactional memory behind the scenes.
   It is used internally by slots of transactional classes, but can also be used
   directly. All its functions and methods work both inside and outside transactions:
   - `(make-instance 'tvar &key value)` Create a new TVAR, optionally bound to a value.
   - `($ var)` Get the value of VAR. Signals an error if VAR is not bound to any value.
   - `(setf ($ var) value)` Store VALUE into VAR.
   - `(bound-$? var)` Return true if VAR is bound to some value.
   - `(unbind-$ var)` Unbind VAR from its value.
   - `(value-of var)` getter method, equivalent to `($ var)`
   - `(setf (value-of var) value)` setter method, equivalent to `(setf ($ var) value)`

Utilities and examples
---------------------

See the [util](util) folder, which contains several examples and utilities
built with STMX and should be relatively straightforward to understand.
The folder contains the following classes with related methods and functions,
all in the STMX.UTIL package - for more details, use `(describe 'some-symbol)` at REPL:

- `CELL` is the simplest transactional class. It is created with
  `(make-instance 'cell &key value)` and it can be empty or hold a single value.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `PUT` `TRY-TAKE` `TRY-PUT`.

  When empty, taking a value will (retry) and wait until some other thread
  puts a value.
  
  When full, putting a value will (retry) and wait until some other thread
  removes the previous value.

  Note: raw TVARs support exactly the same methods.

- `STACK` is a transactional first-in-last-out buffer. It is created with
  `(make-instance 'stack)` and it can be empty or hold unlimited values.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `PUT` `TRY-TAKE` `TRY-PUT`.

  All methods append or remove values from the end.
  For the rest, the methods behave as described for the `CELL` class.

- `FIFO` is a transactional first-in-first-out buffer. It is created with
  `(make-instance 'fifo)` and it can be empty or hold unlimited values.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `PUT` `TRY-TAKE` `TRY-PUT`.

  `PUT` and `TRY-PUT` append values at the end, `PEEK` `TAKE` and `TRY-TAKE` get or remove them from the beginning, shifting the remaining values.
  For the rest, the methods behave as described for the `CELL` class.

- `CHANNEL` is a transactional multicast channel. It is created with
  `(make-instance 'channel)`, can contain unlimited values and it is write-only.
  To read from it, create a `PORT` as described below.

  Methods: `FULL?` `EMPTY?` `PUT` `TRY-PUT`.

  `PUT` and `TRY-PUT` append values at the end, making them available to connected ports.
  `FULL?` always returns nil, since a channel can contain unlimited values.
  `EMPTY?' always returns t, since it is not possible to get values from a channel.

  It is possible to write into the same channel from multiple threads: added elements
  will be interleaved and made available to all connected ports.

- `PORT` is a transactional reader for `CHANNEL`. It is created with
  `(make-instance 'port :channel some-chanel)`.
  Ports do not support putting values, they are used to retrieve values from the channel
  they are connected to.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `TRY-TAKE`.

  `PEEK` `TAKE` and `TRY-TAKE` get or consume values previously added to the connected channel.
  All ports connected to the same channel receive all the values in the same order,
  and they consume values independently: taking a value from a port does not consume it
  in the other ports.

  `FULL?` always returns t, since it is not possible to put values in a port.
  `EMPTY?' returns t if some values are available to read or consume.
  `EMPTY!' consumes all values currently available.

  It is also possible to use the same port from multiple threads: elements consumed
  by one thread will not be available to other threads using the same port.

- `THASH-TABLE` is a transactional hash table.
  It is created with `(make-instance 'thash-table &key (test 'eql) other-options)`.
  An interesting feature: it accepts exactly the same options as MAKE-HASH-TABLE,
  including any non-standard option supported by the underlying MAKE-HASH-TABLE implementation.

  Methods: `THASH-COUNT` `THASH-EMPTY?` `CLEAR-THASH`
           `GET-THASH` `(SETF GET-THASH)` `REM-THASH` 
           `MAP-THASH` `DO-THASH`.

- `TMAP` is a transactional sorted map, backed by a red-black tree.
  It is created with `(make-instance 'tmap :pred compare-function)`
  where COMPARE-FUNCTION must be a function accepting two arguments, KEY1 and KEY2,
  and returning t if KEY1 is smaller that KEY2. Typical values for COMPARE-FUNCTION
  are `#'<` and the faster `#'fixnum<` or, for string keys, `#'string<`

  Methods: `BMAP-PRED` `BMAP-COUNT` `BMAP-EMPTY?` `CLEAR-BMAP`
           `GET-BMAP` `SET-BMAP` `(SETF GET-BMAP)` `REM-BMAP` 
           `MIN-BMAP` `MAX-BMAP` `MAP-BMAP` `DO-BMAP`
           `BMAP-KEYS` `BMAP-VALUES` `BMAP-PAIRS`.

- `RBMAP` is the non-transactional version of `TMAP`. Not so interesting by itself,
  as many other red-black trees implementations exist already on the net.
  It supports exactly the same methods as `TMAP`.

Performance
-----------

As for any software, the topic of performance is often sensitive and
plagued with heated discussions. It is objectively difficult to come up with
scientifically accurate figures as they depend on many factors, including at least
hardware, operating system, common lisp implementation, optimization flags and usage pattern.

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 6 April 2013

Hardware: Intel Core-i5 750 @4.0 GHz, 16GB RAM

Software: Debian GNU/Linux 7 (wheezy) x86_64, SBCL 1.0.57.0.debian x86_64, STMX 0.9.3

Setup and optimization flags:

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
    (setf (get-thash 'x th) 0)

For each benchmark, a loop runs the code shown one million times (see `one-million` macro above)
in a single thread and the best of three loops is used.
All reported times are the average elapsed real time per iteration, i.e. the total elapsed time
divided by the number of iterations (one million).

<table>
 <tr><th><b>name</b>      </th>
     <th><b>executed code</b></th>
     <th><b>average time</b></th></tr>

 <tr><td>atomic empty     </td><td><code>(atomic)</code>                    </td><td>0.264&nbsp;microseconds</td></tr>
 <tr><td>atomic dummy     </td><td><code>(atomic 1)</code>                  </td><td>0.264&nbsp;microseconds</td></tr>
 <tr><td>atomic read-1    </td><td><code>(atomic ($ v))</code>              </td><td>0.696&nbsp;microseconds</td></tr>
 <tr><td>atomic write-1   </td><td><code>(atomic (setf ($ v) i))</code>     </td><td>1.495&nbsp;microseconds</td></tr>
 <tr><td>atomic read-write-1</td><td><code>(atomic (incf ($ v)))</code>     </td><td>1.969&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-10</td>
     <td><code>(atomic (dotimes (j 10) (incf ($ v))))</code></td>
     <td>2.851&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-100</td>
     <td><code>(atomic (dotimes (j 100) (incf ($ v))))</code></td>
     <td>11.154&nbsp;microseconds</td></tr>

 <tr><td>atomic read-write-N</td><td>best fit of the 3 runs above</td><td>(1.900+N*0.093)&nbsp;microseconds</td></tr>
 <tr><td>orelse empty     </td><td><code>(atomic (orelse))</code>           </td><td>0.243&nbsp;microseconds</td></tr>
 <tr><td>orelse unary     </td><td><code>(atomic (orelse 1))</code>         </td><td>0.753&nbsp;microseconds</td></tr>
 <tr><td>orelse binary    </td><td><code>(atomic (orelse (retry) 1))</code> </td><td>1.433&nbsp;microseconds</td></tr>
 <tr><td>orelse ternary   </td><td><code>(atomic (orelse (retry) (retry) 1))</code> </td><td>2.446&nbsp;microseconds</td></tr>
 <tr><td>orelse 5-ary     </td><td><code>(atomic (orelse (retry) (retry) (retry) (retry) 1))</code></td><td>3.717&nbsp;microseconds</td></tr>

 <tr><td>orelse N-ary     </td><td>best fit of the 3 runs above</td><td>(0.008+N*0.749)&nbsp;microseconds</td></tr>

 <tr><td>tmap read-write-1</td>
     <td><code>(atomic (incf (get-bmap tm 1)))</code></td>
     <td>5.247&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-bmap tm))<br>
              (set-bmap tm i t)))</code></td>
     <td>18.885&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-bmap tm))<br>
              (set-bmap tm i t)))</code></td>
     <td>35.093&nbsp;microseconds</td></tr>

 <tr><td>grow tmap from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-bmap tm))<br>
              (set-bmap tm i t)))</code></td>
     <td>49.399&nbsp;microseconds</td></tr>

 <tr><td>thash read-write-1</td>
     <td><code>(atomic (incf (get-thash th 'x)))</code></td>
     <td>11.207&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 10)</td>
     <td><code>(atomic (when (zerop (mod i   10)) (clear-thash tm))<br>
              (set-thash i tm t))</code></td>
     <td>10.912&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 100)</td>
     <td><code>(atomic (when (zerop (mod i  100)) (clear-thash tm))<br>
              (set-thash tm i t)))</code></td>
     <td>16.620&nbsp;microseconds</td></tr>

 <tr><td>grow thash from N to N+1 entries (up to 1000)</td>
     <td><code>(atomic (when (zerop (mod i 1000)) (clear-thash tm))<br>
              (set-thash tm i t)))</code></td>
     <td>68.615&nbsp;microseconds</td></tr>

</table>

Contacts, help, discussion
--------------------------

As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/stmx/issues)
can be used to report test suite failures, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.

Status
------

As of April 2013, STMX is being written by Massimiliano Ghilardi
and is considered by the author to be in BETA status.

STMX is a full rewrite of CL-STM, which has been developed by Hoan Ton-That
for the Google Summer of Code 2006.

Legal
-----

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
