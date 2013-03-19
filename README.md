STMX
======

Summary
-------

STMX is an extension of Common Lisp to support composable Software
Transactional Memory. Transactional Memory makes concurrent programming
easier to write and understand, and possibly qualitatively better.
Instead of traditional lock-based programming, you program with atomic
transactions. Atomic transactions can be composed together to make larger
atomic transactions.

A transaction gets committed if it returns normally it, while it gets
rolled back if it signals an error (and the error is propagated to the caller).

Finally, it just so happens that transactions run in parallel,
and are re-executed from the beginning if there are conflicts.

STM gives freedom from deadlocks, automatic roll-back on failure,
and it aims at resolving the tension between granularity and concurrency.


Installation and loading
------------------------

STMX is packaged with asdf. The simplest way to install STMX is to
first install [Quicklisp](http://www.quicklisp.org), as it can automatically
resolve and download STMX dependencies.

Once quicklisp is installed, download STMX into your Quicklisp
local-projects folder with the commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/stmx.git

then load a REPL and run:

     CL-USER> (ql:quickload "stmx")
     
If all goes well, it will automatically download and install STMX dependencies:

- `bordeaux-threads`
- `closer-mop`
- `arnesi`

then it will load STMX and be ready to use.

In case you get errors:

- check that quicklisp is installed correctly, for example by executing at REPL:

     CL-USER> (ql:quickload "arnesi")

- check that you downloaded STMX and uncompressed it, creating an
  stmx/ folder, inside your Quicklisp local-projects folder, usually
  `~/quicklisp/local-projects`


Documentation
-------------

[Composable Memory Transactions](http://research.microsoft.com/~simonpj/papers/stm/stm.pdf)
gives a very good - though a bit technical - explanation of transactions and how they are used and combined.

Basic usage
-----------

For the impatient, see the [util](util) folder, which contains several
examples and utilities build with STMX, and should be quite easy to
understand.

For the *very* impatient, STMX offers four Lisp special forms (macros)
and a function:

- TRANSACTIONAL declares that a class is transactional, i.e. that it
  contains transactional slots. Use it to wrap a class definition:
  
    (transactional
      (defclass foo ()
        ((value1 :type integer)
         (value2 :type string)
         ;; ...
        )))

- TRANSACTION declares that a method or function is an atomic
  transaction, and is actually just a convenience macro for ATOMIC.
  Use it to wrap a function definition
  
    (transaction
      (defun bar (x y)
        (format t "hello from atomic function bar ~A ~A~%" x y)
        ;; ...
      ))
      
  or a method definition
  
    (transaction
      (defmethod baz (x y)
        (format t "hello from atomic method baz ~A ~A~%" x y)
        ;; ...
      ))

- ATOMIC is the main macro: it wraps Lisp forms into a transaction.
  The above functions and methods could also be written as:
  
    (defun bar (x y)
      (atomic
        (format t "hello from atomic function bar ~A ~A~%" x y)
        ;; ...
      ))
      
    (defmethod baz (x y)
      (atomic
        (format t "hello from atomic method baz ~A ~A~%" x y)
        ;; ...
      ))
      
  with the difference that `atomic` is not limited to functions and
  methods: it can be used to wrap *any* list of forms (it contains an
  implicit `progn`).

- RETRY is a function. It is more tricky to understand, but really powerful.
  As described in the summary, transactions will commit if they return normally,
  while they will rollback if they signal an error or condition.

  RETRY offers a third option: if invoked inside a transaction, it tells
  STMX that the transaction cannot complete immediately, for example
  because some data is not available, and instructs STMX to re-execute
  the transaction from scratch after the data has changed.

  How does RETRY know which data it should monitor for changes?
  Simple: it will monitor *all* transactional slots (i.e. slots of
  transactional objects) that were read since the beginning of the
  transaction and until (retry) was invoked. 

  With RETRY, reliable communication among threads is (hopefully)
  extremely simple to implement: one thread can read one (or more)
  transactional objects, checking for values that some other thread
  will write there, and just RETRY if no values are there yet.

- ORELSE is a macro to combine two or more transactions as alternatives:
  if the first retries, the second will be executed and so on, until one
  transaction either commits (returns normally) or rollbacks (signals an error
  or condition).

  NOTE: ORELSE IS NOT YET IMPLEMENTED.

Mailing List
------------

The old [CL-STM mailing list](http://common-lisp.net/cgi-bin/mailman/listinfo/cl-stm-devel)
seems to be abandoned since 2006. A new one will be created as soon as possible.

Status
------

STMX is being written by Massimiliano Ghilardi.

STMX is a rewrite of CL-STM, which has been developed by Hoan Ton-That
for the Google Summer of Code 2006.
