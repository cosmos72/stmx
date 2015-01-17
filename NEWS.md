### Latest news, 16th January 2015

Version 2.0.1 released.
It adds support for transactional structs in addition to transactional CLOS objects,
and a faster, struct-based implementation of transactional CONS cells and lists,
including several list-manipulating functions - see [util/tcons.lisp](util/tcons.lisp)
and [util/tlist.lisp](util/tlist.lisp)

Unluckily, the hardware bug that prompted Intel to disable hardware transactional memory (TSX)
in August 2014 is still there, and *very* few new models are available without the bug.
So for the moment STMX will be software-only on many CPUs.

### News, 20th May 2014

STMX was presented at
[7th European Lisp Symposium (ELS 2014)](http://www.european-lisp-symposium.org/)
in a technical paper titled
[High performance concurrency in Common Lisp - hybrid transactional memory with STMX](doc/stmx-ELS-2014.pdf).

[Slides](http://www.european-lisp-symposium.org/ghilardi.pdf)
and [video](http://medias.ircam.fr/xcc8494) 
of STMX presentation are available from
[ELS 2014 website](http://www.european-lisp-symposium.org/content-programme-full.html).

Thanks everybody who joined this great event in Paris!


### News, 31st August 2013

Since version 1.9.0, STMX supports hardware memory transactions in addition to
classic software ones. It uses Transactional Synchronization
Extensions (Intel TSX) available on the following Intel x86_64 processors:
- Intel Core i7 4771
- Intel Core i7 4770, 4770S, 4770T, 4770TE
- Intel Core i7 4765T
- Intel Core i5 4670, 4670S, 4670T 
- Intel Core i5 4570, 4570S, 4570T, 4570TE

To actually use hardware memory transactions with STMX, you will need:

- a CPU supporting Intel TSX, for example one from the above list
- a 64-bit OS (currently tested on Debian GNU/Linux x86_64)
- a 64-bit installation of Steel Bank Common Lisp (SBCL) version 1.0.55 or later
  Note: x86_64 is often named AMD64 - they are the same thing
- the latest STMX version - download it from [GitHub](https://github.com/cosmos72/stmx)
  as described in **Installation and loading** below

The current hardware memory transactions implementation is very fast, yet it
still has room for optimizations. Currently, it can accelerate short
transactions up to 4-5 times while seamlessly falling back on software
transactions when the hardware limits are exceeded. Experiments with
hand-optimized code (not yet included in STMX) show that the maximum possible
performance increase is 7-8 times.

### News, 27th July 2013

Since version 1.3.3, STMX also includes [SB-TRANSACTION](sb-transaction), a
standalone library that does **not** depend on STMX and provides hardware-only
memory transactions on CPUs that support Intel TSX instructions.
It is a low-level library providing raw access to the new CPU instructions
for hardware transactions.
Its performance reaches the theoretical peak supported by the underlying CPU,
and it is obviously faster than STMX - it is usually even faster than
hand-optimized compare-and-swap fine grained locking code (see benchmark
results in [doc/benchmark.md](doc/benchmark.md)). The reason is that it avoids
the overhead and the software compatibility requirements of STMX, providing
only the raw features - and the limitations - of the underlying CPU.
At the moment, SB-TRANSACTION only works on SBCL running in native 64-bit mode
on a CPU with hardware transaction support (see the list above).

