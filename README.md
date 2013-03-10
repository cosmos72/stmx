CL-STM
======

Summary
-------

CL-STM2 is an extension of Common Lisp to support composable Software
Transactional Memory. STM makes concurrent programming qualitatively better.
Instead of traditional lock-based programming, you program with atomic
transactions. Atomic transactions can be composed together to make larger
atomic transactions. Finally, it just so happens that transactions run
in parallel, and are rolled back if there are conflicts.

STM gives us freedom from deadlock, automatic roll-back on failure,
and it resolves the tension between granularity and concurrency.

Documentation
-------------

Have a look at the [tutorial](doc/tutorial.html) for
a beginners' guide.  For more information see the [API](doc/index.html),
which is generated from the source code.

[Composable Memory Transactions](http://research.microsoft.com/~simonpj/papers/stm/stm.pdf)
gives you a deeper understanding of concept of transactions, and how they are
combined.

Mailing List
------------

[Subscribe](http://common-lisp.net/cgi-bin/mailman/listinfo/cl-stm-devel)
to the mailing list to keep up to date with all the latest
developments.  If you don't want your inbox clogged then browse the
[archives](http://common-lisp.net/pipermail/cl-stm-devel/) on the web.

Status
------

CL-STM has been developed by Hoan Ton-That for the Google Summer of Code 2006.
Feel free to mail any questions and comments at
[hoan@ton-that.org](mailto:hoan@ton-that.org)
