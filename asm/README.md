STMX.ASM
========

Summary
-------
STMX.ASM is a library to use hardware memory transactions on x86 and x86-64 SBCL.

It defines regular, easy to use Common Lisp functions to start, commit, abort
and test hardware memory transactions, exploiting a set of new CPU instructions
introduced in 2012 by Intel and known as [Intel TSX hardware transactional memory]
(http://en.wikipedia.org/wiki/Transactional_Synchronization_Extensions)

Note that Intel TSX comprises two different CPU instructions sets
to use hardware memory transactions:
* Restricted Transactional Memory (RTM) - a new set of instructions providing
  direct control to hardware memory transactions: XBEGIN, XEND (i.e commit), XABORT and XTEST.
* Hardware Lock Elision (HLE) - designed for backward compatibility
  with locking code that already uses atomic compare-and-swap
  instructions LOCK CMPXCHG and LOCK CMPXCHG8B.

The current implementation of STMX.ASM only knowns about RTM.


Supported systems
-----------------
Due to its nature, STMX.ASM is highly dependent on specific hardware and software:

- It can be **compiled** only with SBCL on a x86 or x86-64 CPU.

- It can be **used** only with SBCL on a x86 or x86-64 CPU that actually supports Intel TSX.
  As of February 2014, the CPUs supporting Intel TSX are:
    * Intel Core i7 4771
    * Intel Core i7 4770, 4770S, 4770T, 4770TE
    * Intel Core i7 4765T
    * Intel Core i5 4670, 4670S, 4670T 
    * Intel Core i5 4570, 4570S, 4570T, 4570TE
    
  Note: all the current Intel Core K and R models, as for example Core i7 4770K and Core i5 4670K,
  do **not** support Intel TSX.
  
Tested systems:
* SBCL  version 1.1.14           (x86-64) on Debian GNU/Linux 7.0 (x86-64) on Intel Core i7 4770
* SBCL  version 1.1.8            (x86-64) on Debian GNU/Linux 7.0 (x86-64) on Intel Core i7 4770
* SBCL  version 1.1.8.60-77641d6 (x86-64) on Debian GNU/Linux 7.0 (x86-64) on Intel Core i7 4770
* SBCL  version 1.0.57.0.debian  (x86-64) on Debian GNU/Linux 7.0 (x86-64) on Intel Core i7 4770

The x86 version of STMX.ASM targets the same (x86-64) CPUs running in 32-bit legacy mode.



Installation and loading
------------------------

STMX.ASM is currently packaged together with [STMX](https://github.com/cosmos72/stmx.git),
a high-performance Common Lisp library that implements transactional memory in software.

It can be used by itself, yet the author's objective is to provide
STMX.ASM as a foundation for STMX and possible third-party libraries
to exploit hardware support and improve the performance of transactional memory
libraries beyond the possibilities of software-only solutions.

STMX.ASM is available from GitHub. The simplest way to install it
is to first install [Quicklisp](http://www.quicklisp.org), then download it
into your Quicklisp local-projects folder. Open a shell and run the commands:

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/cosmos72/stmx.git

then load a REPL and run:

    CL-USER> (ql:quickload "stmx")
    ;; lots of output...
    CL-USER> (use-package :stmx)
     
If all goes well, it will automatically load both STMX.ASM and STMX,
as well as STMX dependencies (STMX.ASM has no dependencies).

Note: a stable version of STMX can be downloaded directly from Quicklisp with
`(ql:quickload "stmx")` without going through GitHub. Such version does **NOT**
contain STMX.ASM yet. 


### Troubleshooting

In case you get errors:

- check that Quicklisp is installed correctly, for example by
  executing at REPL:

        CL-USER> (ql:quickload "closer-mop")

- check that you downloaded the correct STMX version, creating an `stmx/` folder
  inside your Quicklisp local-projects folder, usually `~/quicklisp/local-projects/`
  and that it contains the folder `~/quicklisp/local-projects/stmx/sb-transaction/`


### Testing that it works

A simple test is the following. From the REPL, run:

    CL-USER> (use-package :stmx.asm)
             (defun dummy-tx () (progn
                  (when (= (transaction-begin) +transaction-started+)
                    (transaction-end)
                    t)))
             (disassemble 'dummy-tx)

It should produce output similar to the following - note the XBEGIN and XEND assembler
instructions in the output:

    ; disassembly for DUMMY-TX
    ; Size: 49 bytes
    ; 0D1759A4:       B803000000       MOV EAX, 3                 ; no-arg-parsing entry point
    ;       A9:       C7F800000000     XBEGIN L0
    ;       AF: L0:   488BC8           MOV RCX, RAX
    ;       B2:       48D1E1           SHL RCX, 1
    ;       B5:       4883F906         CMP RCX, 6
    ;       B9:       750E             JNE L2
    ;       BB:       0F01D5           XEND
    ;       BE:       BA4F001020       MOV EDX, 537919567
    ;       C3: L1:   488BE5           MOV RSP, RBP
    ;       C6:       F8               CLC
    ;       C7:       5D               POP RBP
    ;       C8:       C3               RET
    ;       C9: L2:   BA17001020       MOV EDX, 537919511
    ;       CE:       EBF3             JMP L1
    ;       D0:       CC0A             BREAK 10                   ; error trap
    ;       D2:       02               BYTE #X02
    ;       D3:       19               BYTE #X19                  ; INVALID-ARG-COUNT-ERROR
    ;       D4:       9A               BYTE #X9A                  ; RCX


As stated, in order to actually run RTM-based code you need a CPU with RTM support.
You can test if your CPU supports RTM with the function:

    CL-USER> (transaction-supported-p)
    T

It will return T if RTM instructions are supported by the CPU, otherwise it will return NIL.
On such machines, hardware transaction-based code is just regular Lisp code:

    CL-USER> (dummy-tx)
    T



Basic usage
-----------

STMX.ASM offers the following Lisp functions, also documented
in the sources - remember `(describe 'some-symbol)` at REPL.

- `TRANSACTION-SUPPORTED-P` is a function returning T if the CPU supports 
  hardware memory transactions (RTM), and NIL if it does not.
  If `(transaction-supported-p)` returns NIL, invoking any of the functions below
  has undefined consequences - usually *bad* consequences like SIGILL signals
  or unhandled memory faults.

- `TRANSACTION-BEGIN` starts a new transaction. It returns a fixnum
  equal to `+transaction-started+` if the transaction started successfully,
  otherwise returns the code of the error that caused the transaction to abort.

  Invoking `(transaction-begin)` while there is already a running transaction
  has implementation-dependent effects.

- `TRANSACTION-END` tries to commit a transaction.
  It returns normally (with an implementation-dependent value) if commit is successful,
  otherwise aborts the transaction.

  In case the transaction is aborted for any reason, **all** effects of code
  between TRANSACTION-BEGIN and TRANSACTION-END are rolled back (undone):
  execution resumes at the instruction immediately after TRANSACTION-BEGIN,
  in such a way that TRANSACTION-BEGIN will appear to have returned
  an error code (that describes the abort reason).

  Invoking `(transaction-end)` without a running hardware memory transaction
  has undefined consequences - usually *bad* consequences like unhandled memory faults.

- `TRANSACTION-ABORT` immediately aborts a transaction with an error-code equal to
  `+transaction-user-abort+`. Note that transactions may abort spontaneouly
  for a number of reasons, usually including but not limited to:
  * another CPU core writing to the same memory locations
  * hardware limitations - there are a lot of them: for example, transactions usually
    exploit CPU L1 cache as transactional read/write buffer; overflowing the L1 cache
    is a typical cause of abort
  * executing certain assembler instructions as CPUID, PAUSE and XABORT
  * invoking an operating system call
  * executing a context switch on the CPU core running the transaction,
    i.e. multitasking

  If a transaction is aborted, it is the programmer's responsibility to implement
  alternative solutions: depending on the error code, re-running it may be useful or not -
  see the [official RTM and HLE reference manual from Intel](http://software.intel.com/sites/default/files/m/9/2/3/41604)
  for the possibile error codes and their meaning.

- `TRANSACTION-RUNNING-P` returns T if a hardware memory transaction
  is currently in progress, otherwise returns NIL.

Examples
--------

Since STMX.ASM is a low-level library, short examples are necessarily limited
to very simple tasks.

For example, a macro that tries to transactionally swap two memory locations (places) could
be written as:

    (defmacro try-swap-transaction (place1 place2)
       `(when (= (transaction-begin) +transaction-started+)
          (rotatef ,place1 ,place2)
          (transaction-end)
          t))

Please note that this example is intentionally simplified: a robust implementation
should also handle errors and, depending on the situation, probably invoke either
(transaction-abort) or (transaction-end) before propagating the signal,
and it should surely provide an alternative solution to perform the swap
if the transaction aborts.


Performance
-----------
Being a low-level library that exposes CPU instructions, performance is very high.

As of June 2013, with SBCL 1.1.8 running on Debian GNU/Linux 7 on a Core i7 4770 @ 3.5GHz,
the following example code

    (declaim (optimize (compilation-speed 0) (space 0) (debug 0) (safety 0) (speed 3)))
    (let ((a 1) (b 2) (count 0))
      (time (dotimes (i 1000000000)
              (when (try-swap-transaction a b)
                (incf (the fixnum count)))))
      (values a b count))

runs one billion transactions in approximately 18.1 seconds (timing depends also
on system activity), which means 18.1 nanoseconds per transaction.

The transaction abort rate can be deduced from the 'count' variable,
and typically fluctuates from 3000 to 7000 per billion.
In other words, the success rate is between 99.9992% and 99.9997%.

Obviously, this is a very short single-threaded micro benchmark, and performance
in other cases will depend on the complexity of the code inside transactions,
on the number of reads and writes to transactional memory,
and the rate of conflicts and retries.


Contacts, help, discussion
--------------------------
As long as the traffic is low enough, [GitHub Issues](https://github.com/cosmos72/stmx/issues)
can be used to report problems, bugs, suggestions, general discussion etc.

If the traffic becomes high, more appropriate discussion channels will be set-up.

The author will also try to answer support requests, but gives no guarantees.

Status
------

As of June 2013, STMX.ASM is being written by Massimiliano Ghilardi
and is considered by the author to be stable.

Legal
-----

STMX.ASM is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
