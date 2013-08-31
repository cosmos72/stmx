This file contains an extract from
https://github.com/daveboutcher/tl2-x86-mp/blob/master/RELEASENOTES
for documentation and reference purposes.

It is covered by BSD license as stated at the bottom.


------------------------------------------------------------------------

* In our very first TL2 implementation we incremented the global version number
at commit-time for each update transaction. While correct, this incurs
CAS contention (on SPARC we implement the fetch-and-add of the global version
number with CAS in a loop) as well as considerable cache coherency traffic.
Recall that all transactions must fetch the global version number variable.
We refer to this form of clock management as "GV1".

We later developed more refined clock management schemes called GV4, GV5
and GV6. The source code contains conditional compilation directives that
allow the developer to switch between the various schemes at compile-time.
See the

GV4, GV5 and GV6:

In TL2 transactions must initially read the shared global version
number variable. The read value is subsequently used during the transaction to
validate that the read-set (the observed data values) is consistent. All
transactions that update shared variables must increment that global version
number at commit-time. The highly read-write nature of the global version number
variable results in considerable SMP coherency traffic. Relatedly, we can suffer
contention in the loop that increments the global version number in the form of
failed compare-and-swap operations. These effects can limit the scalability of TL2.

Reduced coherency traffic and compare-and-swap contention resulting in improved
transactional throughput

We describe 3 related mechanisms that address the problems described above.

1. "GV4"

In the original TL2 algorithm the transactional commit operation would (a)
acquire locks covering the transaction's write-set, (b) atomically increment the global
version number yielding a WV (Write Version) value, (c) validate the transaction's
read-set set, and, contingent upon (c), write-back the values from the write-set to
their ultimate shared locations and then release and update the locks covering the
write-set by storing WV into the lock-words. The increment of the global version
number was accomplished with a loop using an atomic compare-and-swap (CAS) instruction.

We observed, however, that we can safely replace the loop with a single
CAS attempt. Lets say we have two nearly simultaneous writers trying to atomically
increment the global version number. The CAS performed by one thread succeeds but the
CAS performed by the 2nd thread fails, returning the value just installed by the
1st thread. We have determined that we can safely allow both threads to use the
same WV. The thread whose CAS fails "borrows" the newly incremented value returned
by the failing CAS instruction and uses that value as its WV. Note that we still
incur CAS latency on every attempt to increment the global clock and we still
generate cache-coherent read-write traffic on the clock but we have avoided CAS
contention and the retries inherent in the original loop.

Allowing 2 writers to use the same WV is safe. If the CAS used to atomically increment
the global version number fails then we have 2 writers racing; one atomic increment
attempt succeeded and one failed. Because the 2 threads hold locks on their respective
write-sets at the time they try to increment, we know that their write-sets do not
intersect. If the write-set of one thread intersects the read-set of the other then we
know that one transaction will subsequently fail validation (either because the
lock associated with the read-set entry is held by the other thread, or because the
other thread already committed and released the lock covering the variable, installing
the new WV). As such we can safely allow both threads to use the same (duplicate) WV.
This relaxation provides a significant performance benefit on high-order SMP systems.
Without loss of generality, we can extend this reasoning to more than 2 threads.

The critical safety invariant is that any computed WV must be > any previously
read (observed) RV.

2. "GV5"

Instead of attempting to increment the global version number, we simply compute
WV = GlobalVersionNumber + 1. This greatly reduces coherency traffic (write-rate)
on the GlobalVersionNumber at the cost of an increased false-positive abort rate.
In GV5 we only increment GlobalVersionNumber at abort-time.


3. "GV6"

GV6 is an adaptive hybrid of GV4 and GV5. We employ a random number generator to select
between GV4 and GV5. Randomly, 1 out N commit operations, we use GV4, the other N-1 times
we use GV5. In one variation on GV6 We vary N based on the recent successful commit rate
(transactional throughput). That is, GV6 programmatically varies N using feedback to try
to maximize transactional throughput. (Alternately, we can attempt to minimize the
recent abort rate). 








------------------------------------------------------------------------

Unless otherwise noted, the following license applies to global-clock.md file:

Copyright (c) 2006, Sun Microsystems, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.
    * Neither the name of the Sun Microsystems, Inc. nor the names of
      its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.