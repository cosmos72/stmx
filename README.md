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

Once quicklisp is installed, download STMX into your Quicklisp
local-projects folder with the commands:

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

- check that quicklisp is installed correctly, for example by executing at
  REPL:

       CL-USER> (ql:quickload "arnesi")

- check that you downloaded STMX creating an `stmx/` folder inside
  your Quicklisp local-projects folder, usually `~/quicklisp/local-projects`


Documentation
-------------

[Composable Memory Transactions](http://research.microsoft.com/~simonpj/papers/stm/stm.pdf)
is a very good - though a bit technical - explanation of transactions and
how they are used and combined. For the interested reader, it also goes in
deep detail on how to actually implement memory transactions.

Basic usage
-----------

For the impatient, see the [util](util) folder, which contains several
examples and utilities build with STMX, and should be quite easy to
understand.

For the *very* impatient, STMX offers the following Lisp macros and functions:

- `TRANSACTIONAL` declares that a class is transactional, i.e. that its
  slots contain transactional data. Use it to wrap a class definition:
  
        (transactional
          (defclass foo ()
            ((value1 :type integer :accessor :value1-of)
             (value2 :type string  :accessor :value2-of)
             ;; ...
            )))

- `TRANSACTION` declares that a method or function is an atomic
  transaction, and is actually just a convenience macro for `ATOMIC`.
  Use it to wrap a function definition:
  
        (transaction
          (defun bar (obj)
            (declare (type foo obj))
            (format t "atomic function bar: foo is ~A ~A~%"
              (value1-of obj) (value2-of obj))
            ;; ...
          ))
      
  or a method definition:
  
        (transaction
          (defmethod baz ((obj foo) value1 value2)
            (declare (type integer value1)
                     (type string value2))
            (setf (value1-of obj) value1)
            (setf (value2-of obj) value2)
            (format t "atomic method baz: foo is now ~A ~A~%" value1 value2)
            ;; ...
          ))

- `ATOMIC` is the main macro: it wraps Lisp forms into a transaction.
  The above functions and methods could also be written as:
  
        (defun bar (obj)
          (declare (type foo obj))
          (atomic
            (format t "atomic function bar: foo is ~A ~A~%"
              (value1-of obj) (value2-of obj))
            ;; ...
          ))
      
        (defmethod baz ((obj foo) value1 value2)
          (declare (type integer value1)
                   (type string value2))
          (atomic
            (setf (value1-of obj) value1)
            (setf (value2-of obj) value2)
            (format t "atomic method baz: foo is now ~A ~A~%" value1 value2)
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
  immediately, for example because some necessary data is not
  currently available, and instructs STMX to re-execute the
  transaction from scratch after the data has changed.

  How does `(retry)` know which data it should monitor for changes?
  Simple: it will monitor *all* transactional data (including slots of
  transactional objects) that was read since the beginning of the
  transaction and until `(retry)` was invoked. 

  With RETRY, reliable communication among threads is (hopefully)
  extremely simple to implement: a thread can read one (or more)
  transactional data, checking for values that some other thread
  will write there, and just `(retry)` if no appropriate values are
  there yet.

- `ORELSE` is a macro to combine two transactions as alternatives:
  if the first retries, the second will be executed and so on, until one
  transaction either commits (returns normally) or rollbacks (signals an error
  or condition).

- `TRY` is the n-ary version of `ORELSE`: it combines two *or more*
  transactions as alternatives

- `NONBLOCKING` is an utility macro based on `ORELSE` to convert a blocking
  transaction into another that returns NIL instead of blocking
  (and otherwise returns T followed by the values or the original transaction)
  
        (nonblocking (x) (y) (z))
        
  basically expands to
  
        (orelse (values t (progn (x) (y) (z))) nil)
        
  with the difference that (nonblocking ...) actually captures all the values
  returned by the transaction, not just the first as in the example above.


Mailing List
------------

The old [CL-STM mailing list](http://common-lisp.net/cgi-bin/mailman/listinfo/cl-stm-devel)
seems to be abandoned since 2006. A new one will be created as soon as possible.

Status
------

As of March 2013, STMX is being written by Massimiliano Ghilardi
and is considered by the author to be in BETA status.

STMX is a full rewrite of CL-STM, which has been developed by Hoan Ton-That
for the Google Summer of Code 2006.

Legal
-----

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
