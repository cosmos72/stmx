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
are re-executed from the beginning in case of conflicts or if consistent reads
cannot be guaranteed, and effects of a transaction are not visible from other
threads until it commits.

Transactional memory gives freedom from deadlocks, automatic roll-back on failure,
and it aims at resolving the tension between granularity and concurrency.

### Latest news, 1st July 2013

Support for hardware transactions is being added to STMX. It used Restricted
Memory Transactions (RTM) available on the following Intel x86-64 processors
released in June 2013:
* Intel Core i5 4570
* Intel Core i5 4670
* Intel Core i7 4770

The current implementation is still experimental, unoptimized, and disabled by default.
It can be enabled by uncommenting the relevant flag in `stmx/lang/features-detect.lisp`.

To have a taste of hardware transactions, STMX includes [SB-TRANSACTION](sb-transaction),
a standalone library that does **not** depend on STMX and provides hardware-only
memory transactions on CPUs from the list above.
Its performance is **much** higher than STMX and even higher than
hand-optimized compare-and-swap fine grained locking code (see benchmark
results in [doc/benchmark.md](doc/benchmark.md)). The reason is that it avoids
the overhead and the software compatibility requirements of STMX, providing
only the raw features - and the limitations - of the underlying CPU.
At the moment, SB-TRANSACTION only works on SBCL running in native 64-bit mode
on a CPU with hardware transaction support (see the list above).

The objective is to integrate SB-TRANSACTION with STMX in order
to take advantage of hardware transactions when available,
while falling back on software transactions in the following cases:
- CPU not supporting hardware transactions
- transactions exceed hardware limits
- transactions use features not supported by the hardware (ORELSE, RETRY, allocating memory,
  input/output, system calls...)

General documentation
---------------------
An [introduction](doc/introduction.md) is available to explain more in detail
what STMX is, what it is not, and how it is implemented.

For background information,
[Composable Memory Transactions](http://research.microsoft.com/~simonpj/papers/stm/stm.pdf)
is a very good - though a bit technical - explanation of memory transactions and
how they are used and combined. For the interested reader, it also goes in
deep detail on how to actually implement them.



Supported systems
-----------------
STMX is currently tested on the following Common Lisp implementations:

* SBCL  version 1.1.8        (x86_64) on Debian GNU/Linux 7.0  (x86_64)
* SBCL  version 1.0.55.0     (x86)    on Ubuntu Linux 12.04LTS (x86)
* ABCL  version 1.1.1 with OpenJDK 6b27-1.12.5-2 (x86_64) on Debian GNU/Linux 7.0 (x86_64)
* CCL   version 1.9-r15769   (x86_64) on Debian GNU/Linux 7.0  (x86_64)
* CCL   version 1.9-r15769M  (x86)    on Debian GNU/Linux 7.0  (x86_64)
* CCL   version 1.9-dev-r15475M-trunk (LinuxARM32) on Raspbian GNU/Linux (armhf) Raspberry Pi
* CMUCL version 20d Unicode  (x86)    on Debian GNU/Linux 7.0  (x86_64)
* ECL   version 13.5.1       (x86_64) on Debian GNU/Linux 7.0  (x86_64)

It will probably work on several other Common Lisp implementations as long as they support
log4cl, closer-mop, bordeaux-threads and trivial-garbage, but the author gives no guarantees.


Installation and loading
------------------------

### Stable version - from [Quicklisp](http://www.quicklisp.org)

STMX is now available from Quicklisp. The simplest way to obtain it is to first
install [Quicklisp](http://www.quicklisp.org) then run these commands from REPL:

    CL-USER> (ql:quickload "stmx")
    ;; lots of output...
    CL-USER> (use-package :stmx)
     
If all goes well, this will automatically download and install the
**stable** branch of STMX and its dependencies:

- `log4cl`
- `closer-mop`
- `bordeaux-threads`
- `trivial-garbage`

Since STMX was added to QuickLisp quite recently (15 June 2013), it
may happen that your Quicklisp installation can't find it. In such
case, you need to first update your QuickLisp installation as
described [here](http://www.quicklisp.org/beta) - search for "To get updated
software" in the page.
  

### Latest version - from GIT

In case you want to use the "latest and greatest" version directly
from the author, which may contain new features, improvements, bug
fixes (and occasionally new bugs), you need to download it
into your Quicklisp local-projects folder. Open a shell and run the
commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/stmx.git

then proceed as before - load a REPL and run:

    CL-USER> (ql:quickload "stmx")
    ;; lots of output...
    CL-USER> (use-package :stmx)
     
If all goes well, it will automatically load STMX and its dependencies.


### Troubleshooting

In case you get errors:

- check that Quicklisp is installed correctly, for example by
  executing at REPL:

        CL-USER> (ql:quickload "closer-mop")

- if you tried to download the stable version from Quicklisp,
  check that your quicklisp is updated and knows about STMX:
  
        CL-USER> (ql:system-apropos "stmx")
        
  should print something like
  
        #<SYSTEM stmx / stmx-stable-7e68763b-git / quicklisp 2013-06-15>
        #<SYSTEM stmx.test / stmx-stable-7e68763b-git / quicklisp 2013-06-15>
     
  If it doesn't, you need to update Quicklisp as described
  [here](http://www.quicklisp.org/beta) - search for "To get updated
  software" in the page.
  
- if you tried to download the latest version from GIT,
  check that you downloaded STMX creating an `stmx/` folder inside
  your Quicklisp local-projects folder, usually `~/quicklisp/local-projects`


### Testing that it works

After loading STMX for the first time, it is recommended to run the
test suite to check that everything works as expected. From the REPL,
run:

    CL-USER> (ql:quickload "stmx.test")
    ;; lots of output...
    CL-USER> (fiveam:run! 'stmx.test:suite)
    ;; even more output...
     Did 4585 checks.    
        Pass: 4585 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)
        
Note: `(ql:quickload "stmx.test")` intentionally works only **after**
`(ql:quickload "stmx")` has completed successfuly.

The test suite should report zero Skip and zero Fail; the number of Pass may vary.
You are welcome to report any failure you get while running the test suite,
please include in the report:
- operating system name and version (example: Debian GNU/Linux x86_64 version 7.0)
- Common Lisp implementation and version (example: SBCL 1.0.57.0.debian, x86_64)
- EXACT output produced by the test suite
- any other relevant information

See "Contacts, help, discussion" below for the preferred method to send the report.


Basic usage
-----------

STMX offers the following Lisp macros and functions, also heavily documented
in the sources - remember `(describe 'some-symbol)` at REPL.

- `TRANSACTIONAL` declares that a class is transactional, i.e. that its
  slots contain transactional data. Use it to wrap a class definition:
  
        (transactional
          (defclass foo ()
            ((value1 :type integer :initarg :value1 :initform 0)
             (value2 :type string  :initarg :value2 :initform ""))))

  Note: on some Common Lisp implementations (ABCL and possibly others)
  slot accessors are known to ignore by default the transactional machinery
  (implemented with MOP slot-value-using-class, if you wonder) causing all
  kind of errors on transactional classes.
  Even though usually this problem can be usually at least *partially* fixed
  with implementation-specific options, it is recommended to use `slot-value`
  instead of slot accessors to read and write the slots of transactional
  classes or, even better, a macro that can be defined to use either
  `slot-value` or slot accessors.

- `ATOMIC` is the main macro: it wraps Lisp forms into an atomic transaction
  then executes them. For example, defining

        (defun show-foo (obj)
          (declare (type foo obj))
          (multiple-value-bind (value1 value2)
              (atomic
                (values (slot-value obj 'value1)
                        (slot-value obj 'value2)))
            (format t "atomic function show-foo: foo contains ~S, ~S~%"
                    value1 value2)))
      
        (defmethod set-foo ((obj foo) value1 value2)
          (declare (type integer value1)
                   (type string value2))
          (atomic
            (setf (slot-value obj 'value1) value1)
            (setf (slot-value obj 'value2) value2))
          (format t "atomic method set-foo: foo now contains ~S, S~%"
                  value1 value2))
      
  SHOW-FOO will atomically read the slots VALUE1 and VALUE2 of a FOO
  instance, then print both. Note that `(format t ...)` is **outside** the atomic
  block - more on this later.

  SET-FOO will atomically set the slots VALUE1 and VALUE2 of a FOO
  instance.

  Using these two functions, STMX guarantees that multiple threads accessing
  the same FOO instance will always see consistent values for both slots,
  i.e. SHOW-FOO will **never** see intermediate states of a transaction,
  where for example one slot has been updated by SET-FOO, but the other slot
  has not been updated yet.

  This is the main feature of STMX: if an atomic block completes normally,
  it is assumed to be successful and it gets committed: all its writes
  to transactional memory become visible simultaneously to other threads.
  If instead an atomic block exits with a non-local control transfer
  (signals an error, throws, or invokes a `(go some-label)`), it is assumed
  to be failed and it gets rolled back: all its writes to transactional memory
  are discarded.

  Warning: in order to avoid deadlocks and conflicts while still reaching good
  performance, STMX may execute more than once the contents of an atomic block.
  Also, some instructions as `(retry)` described below, explicitly cause an
  atomic block to be re-executed from the beginning. For this reasons,
  atomic blocks should **not** contain irreversible operations such as
  input/output. More details in the paragraph INPUT/OUTPUT DURING TRANSACTIONS
  below.

  Note: STMX allows using transactional memory both inside and outside atomic
  blocks, but be aware that accessing transactional memory from outside
  atomic transactions is only intended for **debugging** purposes at the REPL:
  in a program it can cause a lot of problems, due to inconsistencies and due to
  other threads not being notified when a transactional memory location is
  updated.
  Future versions may remove this convenience hack and replace it with a cleaner,
  stricter mechanism.
  In a program, **always** make sure that all code that uses transactional memory
  is directly or indirectly executed inside an `(atomic ...)` block.

- `TRANSACTION` declares that a method or function is an atomic
  transaction, and is actually just a macro that wraps the body of a function
  or method in an `(atomic ...)` block.
  In the past, it was suggested as a more convenient alternative to `ATOMIC`,
  but for various stylistic reasons the current recommendation is to avoid it.
  The main reason is that it encourages performing too many operations inside
  an atomic block, including irreversible ones as input/output, which has
  unpredictable behaviour and should be **really** avoided. Examples:

        (transaction
          (defun get-foo-values (obj)
            (declare (type foo obj))
            (values
              (value1-of obj) (value2-of obj))))
      
        (transaction
          (defmethod set-foo-values ((obj foo) value1 value2)
            (declare (type integer value1)
                     (type string value2))
            (setf (value1-of obj) value1)
            (setf (value2-of obj) value2)
            obj))

- Composing transactions

  A key feature of `ATOMIC` is its composability:
  smaller transactions can be composed to create larger transactions.
  For example, the following three program fragments are perfectly equivalent:

  1) use `(atomic ...)` to wrap into a single transaction many smaller `(atomic ...)` blocks

        (defmethod swap-value1-of ((x foo) (y foo))
          (format t "swapping value1 of ~S and ~S~%" x y)
          (atomic
            (rotatef (slot-value x 'value1) (slot-value y 'value1))))

        (defmethod swap-value2-of ((x foo) (y foo))
          (format t "swapping value2 of ~S and ~S~%" x y)
          (atomic
            (rotatef (slot-value x 'value2) (slot-value y 'value2))))

        (defmethod swap-contents ((x foo) (y foo))
          (atomic
            (swap-value1-of x y)
            (swap-value2-of x y)))

  2) write redundant `(atomic ...)` blocks

        (defmethod swap-contents ((x foo) (y foo))
          (format t "swapping value1 and value2 of ~S and ~S~%" x y)
          (atomic
            (atomic
              (rotatef (slot-value x 'value1) (slot-value y 'value1)))
            (atomic
              (rotatef (slot-value x 'value2) (slot-value y 'value2)))))

  3) write a single `(atomic ...)` block

        (defmethod swap-contents ((x foo) (y foo))
          (format t "swapping value1 and value2 of ~S and ~S~%" x y)
          (atomic
            (rotatef (slot-value x 'value1) (slot-value y 'value1))
            (rotatef (slot-value x 'value2) (slot-value y 'value2))))

  This composability property has an important consequence: transactional code,
  possibly written by different people for unrelated purposes, can be combined
  into larger transactions without modifying it - actually, without looking at
  the source code at all - as long as it all uses the same transactional library.

  The STMX machinery will guarantee that transactions intermediate status, where
  an atomic block is half-way through its job, will **never** be visible to other
  transactions.

  For example, it becomes trivial to write some code that atomically removes
  an object from a transactional container and adds it to another one:
  just write something like

        (defmethod move-obj-from-a-to-b ((a some-container) (b another-container))
          (atomic
            (let ((obj (take-obj-from-some-container a)))
               (put-obj-into-another-container obj b))))

  and it will work as long as both container are transactional and use the same
  transaction library (in this case, STMX).

  A lot of facts that in other concurrent programming paradigms can be
  great obstacles to such a solution become completely irrelevant
  when using transactions:
  it is irrelevant that the two containers may be unrelated classes,
  that the respective authors may not have anticipated such need in the APIs,
  that the internal details of the two implementations may be unknown to the
  author of code that combines them atomically (the `move-obj-from-a-to-b`
  in the example),
  that other existing code in the program uses the same containers `a` and `b`
  but does not cooperate with `move-obj-from-a-to-b`.

  Style suggestion: in order to guarantee that all transactional memory accesses
  are performed inside an atomic block, it may be tempting to wrap each function
  or method body inside `(atomic ...)`. While safe and correct, this approach
  has a small performance penalty that performance-critical code may want to
  avoid by minimizing the number of `(atomic ...)` blocks: it is enough to have
  a top-level atomic block that corresponds to the largest transaction that one
  wants to execute, and omit inner atomic blocks in the same or other functions
  called directly or indirectly from the top-level atomic block. In such case,
  it is strongly recommended to insert in the documentation of the functions
  accessing transactional memory without a direct atomic block a sentence like
  "This function should be always invoked from inside an STMX atomic block."

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
  in separate, nested transactions: if the first retries or detects an
  inconsistent read, the second will be executed and so on, until one
  transaction either commits (returns normally) or rollbacks (signals
  an error or condition). It can only be used inside a transaction.

- `NONBLOCKING` is an utility macro based on `ORELSE` to convert a blocking
  transaction into another that returns NIL instead of waiting
  (and otherwise returns T followed by the values or the original transaction)
  
        (nonblocking (x) (y) (z))
        
  basically expands to
  
        (orelse (values t (progn (x) (y) (z))) nil)
        
  with the difference that `(nonblocking ...)` actually captures all the values
  returned by the transaction, not just the first as in the example above.


Input/Output during transactions
--------------------------------
**WARNING:** since transactions will be re-executed in case of conflicts with
others and can also rollback or retry, all non-transactional code inside an
atomic block may be executed more times than expected, or may be executed when
**not** expected.

Some transactional memory implementations, especially for statically-typed
languages, forbid performing input/output during a transaction on the ground
that I/O is not transactional: if a transaction sends an irreversible command
to the outside world, there is no way to undo it in case the transaction
rolls back, retries or conflicts.

STMX does not implement such restrictions, i.e. I/O and any other irreversible
action can also be performed inside an atomic block.
This means you are free to launch missiles during a transaction, and destroy
the world when you shouldn't have. **You have been warned.**

Despite the risk, there are at least two reasons for such a design choice:
* Forbidding I/O operations inside transactions, if done at all, should be done
  while compiling a program rather than while running it.
  In Common Lisp, neither of the two seems easy to implement.
* Common Lisp programs are often much more dynamic and flexible than programs
  in other languages, and programmers are trusted to know what they are doing.
  Such a prohibition does not seem to fit well with this spirit.

The typical solution for the above risk is: during a transaction, perform I/O
**only** for debugging purposes, for example using a logging library as log4cl
(or whatever is appropriate for your program), and queue any I/O operation
in a transactional buffer. Then, invoke a separate function that first runs
a transaction to atomically consume the buffer and only later,
**outside** any transaction, performs the actual I/O operation.

An alternative solution is: during a transaction, instead of performing I/O
pass to `AFTER-COMMIT` a function that will perform I/O when executed.
Note: `AFTER-COMMIT` is described in Advanced usage below, read it carefully
because functions executed by `AFTER-COMMIT` have several restrictions on what
they are allowed to do.

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
  of functions and executes them as alternatives in separate, nested
  transactions:
  if the first retries or is invalid, the second will be executed and so on,
  until one function either commits (returns normally) or rollbacks
  (signals an error or condition).

  If X, Y and Z are no-argument functions, the following two lines are
  equivalent:
  
        (orelse (x) (y) (z))
        (run-orelse #'x #'y #'z)

- `BEFORE-COMMIT` is a macro that registers Lisp forms to be executed later,
  just before the transaction tries to commit.
  It can be useful to normalize or simplify some transactional data, or perform
  any kind of bookkeeping activity. 

  Be aware that the transaction is not yet committed when the forms registered
  with BEFORE-COMMIT run. This means in particular:

  - If the forms signal an error when executed, the error is propagated to the
    caller, forms registered later with BEFORE-COMMIT are not executed, and the
    transaction rolls back.

  - The forms can read and write normally to transactional memory,
    and in case of conflicts the whole transaction, _including_ all forms
    registered with BEFORE-COMMIT, is re-executed from the beginning.

  - The forms cannot (retry) - attempts to do so will signal an error.
    Starting a nested transaction and retrying inside that is acceptable,
    as long as the (retry) does not propagate outside the forms themselves.

- `AFTER-COMMIT` is another macro that registers Lisp forms to be executed
  later,
  but in this case they are executed immediately after the transaction has been
  successfully committed.
  It can be useful to notify some subsystem that for any reason cannot call
  `(retry)` to be informed of changes in transactional memory - for example
  because it is some existing code that one does not wish to modify.

  In this case, the transaction is already committed when the forms registered
  with AFTER-COMMIT run, and (since STMX 1.3.2) the forms are executed *outside*
  any transaction. There are some limitations on what the forms can do:

  - If the forms signal an error when executed, the error is propagated to the
    caller, forms registered later with AFTER-COMMIT are not executed, but the
    transaction remains committed.

  - The forms are not executed inside a transaction: while it is certainly
    possible to explicitly run an `(atomic)` block from them, doing so would
    probably defeat the purpose of AFTER-COMMIT and it may also cause
    a significant performance penalty.

- `CALL-BEFORE-COMMIT` is the function version of `BEFORE-COMMIT`: it accepts a
   single function and registers it to be executed before the transaction tries
   to commit.

- `CALL-AFTER-COMMIT` is the function version of `AFTER-COMMIT`: it accepts a
   single function and registers it to be executed after the transaction
   has been successfully committed.

- `TVAR` is the class implementing transactional memory behind the scenes.
   It is used internally by slots of transactional classes, but can also be used
   directly. Except if specified, all its functions and methods work both inside
   and outside transactions (remember that using transactional memory outside
   transactions is only intended for **debugging** purposes). Functions and
   methods:
   - `(tvar [initial-value])` Create a new TVAR, optionally bound to a value.
   - `($ var)` Get the value of VAR. Signals an error if VAR is not bound to any
      value.
   - `(setf ($ var) value)` Store VALUE into VAR.
   - `(bound-$? var)` Return true if VAR is bound to some value.
   - `(unbind-$ var)` Unbind VAR from its value.
   - `(value-of var)` getter method, equivalent to `($ var)`
   - `(setf (value-of var) value)` setter method, equivalent to
      `(setf ($ var) value)`

   For programmers that want to squeeze the last CPU cycle out of STMX, there
   are two more functions that work **only inside** transactions:
   - `($-tx var)` Get the value of VAR. Return `+unbound-tvar+` if VAR is not
     bound to any value.
     Quite obviously, explicitly checking the value returned by `$-tx` against
     `+unbound-tvar+` would negate the speed advantage: `$-tx` is intended
     for those cases where the TVAR is known to be bound to a value.
   - `(setf ($-tx var) value)` Store VALUE into VAR.
   
   And two that work **only outside** transactions:
   - `($-notx var)` Analogous to `$-tx`: get the value of VAR, return
     `+unbound-tvar+` if VAR is not bound to any value.
   - `(setf ($-notx var) value)` Analogous to `(setf $-tx)`: store VALUE into
      VAR.

Utilities and examples
---------------------

See the [example](example) and [util](util) folder, which contains several
examples and utilities built with STMX and should be relatively straightforward
to understand. The folder [util](util) contains the following classes with
related methods and functions, all in the STMX.UTIL package - for more details,
use `(describe 'some-symbol)` at REPL:

- `TCELL` is the simplest transactional class. It is created with
  `(make-instance 'tcell [:value initial-value])` and it can be empty or hold a
  single value.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `PUT` `TRY-TAKE` `TRY-PUT`.

  When empty, taking a value will (retry) and wait until some other thread
  puts a value.
  
  When full, putting a value will (retry) and wait until some other thread
  removes the current value.

  Note: raw TVARs support exactly the same methods.

- `TCONS` is a transactional cons cell. It is created with
  `(tcons first-value second-value)`.

  Methods: `TFIRST` `(SETF TFIRST)` `TREST` `(SETF TREST)`.

  Seldom used directly.

- `TLIST` is a transactional list. It is created with
  `(tlist [values ...])`.

  Methods: `TFIRST` `(SETF TFIRST)` `TREST` `(SETF TREST)` `TPUSH` `TPOP`.

  Normal lists are perfectly suitable for transactional use as long as
  they are not destructively modified, so TLIST is often unnecessary:
  it becomes needed only to support transactional destructive modifications.

- `TSTACK` is a transactional first-in-last-out buffer. It is created with
  `(make-instance 'tstack)` and it can be empty or hold unlimited values.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `PUT` `TRY-TAKE` `TRY-PUT`.

  All methods append or remove values from the end, and putting a value
  always succeeds, even when other values are already present: the new
  value is simple appended at the end.
  For the rest, the methods behave as described for the `TCELL` class.

- `TFIFO` is a transactional first-in-first-out buffer. It is created with
  `(make-instance 'tfifo)` and it can be empty or hold unlimited values.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `PUT` `TRY-TAKE` `TRY-PUT`.

  `PUT` and `TRY-PUT` append values at the end, `PEEK` `TAKE` and `TRY-TAKE`
  get or remove them from the beginning, shifting the remaining values.
  For the rest, the methods behave as described for the `TCELL` and `TSTACK`
  classes.

- `TCHANNEL` is a transactional multicast channel. It is created with
  `(make-instance 'tchannel)`, can contain unlimited values and it is write-only.
  To read from it, create a `TPORT` as described below.

  Methods: `FULL?` `EMPTY?` `PUT` `TRY-PUT`.

  `PUT` and `TRY-PUT` append values at the end, making them available to
  connected ports.
  `FULL?` always returns nil, since a channel can contain unlimited values.
  `EMPTY?` always returns t, since it is not possible to get values from a
  channel.

  It is possible to write into the same channel from multiple threads: added
  elements will be interleaved and made available to all connected ports.

- `TPORT` is a transactional reader for `TCHANNEL`. It is created with
  `(make-instance 'tport :channel some-channel)`.
  Ports do not support putting values, they are used to retrieve values from the
  channel they are connected to.

  Methods: `FULL?` `EMPTY?` `EMPTY!` `PEEK` `TAKE` `TRY-TAKE`.

  `PEEK` `TAKE` and `TRY-TAKE` get or consume values previously added to the
  connected channel. All ports connected to the same channel receive all the
  values in the same order, and they consume values independently: taking a
  value from a port does not consume it from the other ports.

  `FULL?` always returns t, since it is not possible to put values in a port.
  `EMPTY?` returns t if some values are available to read or consume.
  `EMPTY!` consumes all values currently available.

  It is also possible to use the same port from multiple threads: elements
  consumed by one thread will not be available to other threads using the same
  port.

- `THASH-TABLE` is a transactional hash table.
  It is created with
  `(make-instance 'thash-table [:test #'some-test-function] [:hash #'some-hash-function])`.

  Two differences from standard Common Lisp HASH-TABLE:
  - `:test` argument must be an actual function, not a symbol. The default is `#'eql`.
  - a hash function can be specified explicitly with `:hash #'some-hash-function`
  For the usual test functions, i.e. `#'eq` `#'eql` and `#'equal` the hash function
  can be omitted and a safe default (usually `#'sxhash`) will be used.
  For other test functions, the hash function becomes mandatory.

  Methods: `GHASH-TABLE-COUNT` `GHASH-TABLE-EMPTY?` `CLEAR-GHASH`
           `GET-GHASH` `(SETF GET-GHASH)` `SET-GHASH` `REM-GHASH` 
           `MAP-GHASH` `DO-GHASH` `COPY-GHASH`
           `GHASH-KEYS` `GHASH-VALUES` `GHASH-PAIRS`.

  Note: THASH-TABLE has been completely rewritten in STMX 1.3.3 and
  has now much better performance. Previously its methods contained
  `THASH` instead of `GHASH` in their names.

- `TMAP` is a transactional sorted map, backed by a red-black tree.
  It is created with `(make-instance 'tmap :pred compare-function)`
  where COMPARE-FUNCTION must be a function accepting two arguments, KEY1 and
  KEY2, and returning t if KEY1 is smaller that KEY2. For numeric keys, typical
  COMPARE-FUNCTIONs are `#'<` or `#'>` and the faster `#'fixnum<` or `#'fixnum>`.
  For string keys, typical COMPARE-FUNCTIONs are `#'string<` and `#'string>`.

  Methods: `GMAP-PRED` `GMAP-COUNT` `GMAP-EMPTY?` `CLEAR-GMAP`
           `GET-GMAP` `(SETF GET-GMAP)` `SET-GMAP` `REM-GMAP` 
           `MIN-GMAP` `MAX-GMAP` `MAP-GMAP` `DO-GMAP`
           `GMAP-KEYS` `GMAP-VALUES` `GMAP-PAIRS`.

  Note: TMAP methods were renamed in STMX 1.3.3, they previously contained
  `BMAP` instead of `GMAP` in their names.

- `GHASH-TABLE` is the non-transactional version of `THASH-TABLE`. Not so
  interesting by itself, as Common Lisp offers a standard (and usually faster)
  HASH-TABLE implementation. It supports exactly the same methods as `THASH-TABLE`.

- `RBMAP` is the non-transactional version of `TMAP`. Not so interesting by
  itself, as many other red-black trees implementations exist already on the
  net. It supports exactly the same methods as `TMAP`.


Performance
-----------

In order for STMX to reach its peak performance, four requirements need to be satisfied
by the Lisp compiler being used:
- it must have good multi-threading support
- it must produce fast, highly optimized code
- it must expose atomic compare-and-swap operations or, as slower alternative,
  a function to retrieve the thread owning a lock
- on unordered CPUs (i.e. on most CPUs except x86 and x86-64) it must expose
  memory barrier operations

Among the non-commercial Lisp compilers, SBCL is the only one known to STMX author
that satisfies all the four requirements. Actually, all the other tested free Lisp compilers
(ABCL, CCL, CMUCL, ECL) are quite lacking in the second area, and none of them offers
atomic compare-and-swap or memory barrier operations at all.
One - CMUCL - produces relatively fast code, but does not support native threads.
STMX is not tested on any commercial Lisp compiler, so performance on them is simply unknown.

For these reasons, STMX will reach the highest known performance on SBCL by a large
margin - possibly by a factor from 10 to 100 with respect to other tested systems.

For performance considerations and a lot of raw numbers produced by running micro-benchmarks,
see the included files [doc/benchmark.md](doc/benchmark.md), [doc/benchmark-abcl.md](doc/benchmark-abcl.md),
[doc/benchmark-ccl64.md](doc/benchmark-ccl64.md) and [doc/benchmark-cmucl.md](doc/benchmark-cmucl.md).

The short version is: as of June 2013, on a fast consumer PC (Core i7 4770 @ 3.5GHz or
better) with SBCL 1.1.8 or better, STMX can execute more than 7 millions transactions
per second per CPU core. The second platform in terms of performance is CCL (x64_64),
that reaches 1.1 millions transactions per second per CPU core using two threads,
but STMX performance quickly decreases with more threads.

A small example with very short transactions is the [dining philosophers](example/dining-philosophers-stmx.lisp),
with 5 reads and 5 writes to transactional memory per atomic block,
where each CPU core runs approximately 4.4 millions transactions per second -
hyperthreading has very limited effects.

Obviously, performance in other usage scenarios will depend on the complexity of the code
inside transactions, on the number of reads and writes to transactional memory,
and the rate of conflicts and retries.

### Note
These result are **not** absolute performance considerations of the tested Lisp systems.
They are simply the outcome of running micro-benchmarks of a particular library optimized for SBCL
(see the atomic compare-and-swap point) on some other Lisp systems.
Do **not** try to construct these results as STMX author's opinions on the mentioned Lisp systems.

### Lee-STMX
For a less artificial and hopefully more realistic benchmark, the author has ported
[Lee-TM](http://apt.cs.man.ac.uk/projects/TM/LeeBenchmark/),
a non-trivial benchmark suite for transactional memory developed in 2007
by the University of Manchester (UK). The result is
[Lee-STMX](https://github.com/cosmos72/lee-stmx) - as of July 2013, its
status is BETA.

Contacts, help, discussion
--------------------------
As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/stmx/issues)
can be used to report test suite failures, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.

Status
------

As of July 2013, STMX is being written by Massimiliano Ghilardi
and is considered by the author to be stable.

STMX is a full rewrite of CL-STM, which has been developed by Hoan Ton-That
for the Google Summer of Code 2006.

Legal
-----

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
