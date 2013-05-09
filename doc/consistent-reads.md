STMX
======

Consistent Reads implementation
-------------------------------

All STM implementations must confront with the following theoretical problem:
how to guarantee that transactions will see a consistent, immutable view
of transactional memory while they run?
In database terminology, how to guarantee transactions coerency and isolation, i.e.
the C and I in [ACID](http://en.wikipedia.org/wiki/ACID) ?

Doing an actual copy of all transactional memory when a transaction starts
is prohibitively expensive, and obviously not considered.

A good solution is to use a global version clock as described in [Transactional
Locking II](http://home.comcast.net/~pjbishop/Dave/GVTL-TL2-Disc06-060711-Camera.pdf)

- when a transaction commits, it first atomically increases the global
  version clock, saving the new value in a thread-local variable (W),
  then updates all transactional memory it changed, writing both the
  global version (W) and the new values.

- when a transaction starts it reads the global version clock, saving
  the current value in a thread-local variable (R).
  Then, each time it reads a transactional memory location, checks that 
  its version is <= (R). If the check fails, the transaction rolls back
  and automatically re-executes from the beginning.

Which locks exactly must be acquired by a transaction while committing in order
to guarantee atomicity, consistency and isolation (the A, C and I in ACID) ?


      Tx1        Tx2     The possible dependencies between two transactions are:
     reads__RR__reads    RR - they read some common memory
       | RW\  /  |       RW - Tx1 reads some memory written by Tx2
       |    \/   |       WR - Tx1 writes some memory read by Tx1
       |    /\   |       WW - they write some common memory
       v WR/  \  v       and of course all the combinations of these
    writes/____\writes   for a total of 2^4 = 16 possibilities
            WW

1) no dependencies: no constraints on the implementation

2) RR dependency: no constraints on the implementation

3) RW dependency:
   Tx1 appears to commit before Tx2 *if-and-only-if* Tx1 does not see Tx2 writes.
   Tx1 must either see *all* Tx1 writes that it also reads, or none of them.

   To reach this goal, writes can be protected by locks: before writing,
   a Tx tries to acquire the exclusive lock for each location to be written,
   then, after *all* the locks are acquired successfully, increases the
   global version clock, actually updates the locations, and finally
   releases the locks.

   What about the locations being read? A Tx must either see them fully
   updated by a concurrent transaction, or do not see any of those updates.
   This means a Tx must be prevented from reading the locations that another
   transaction has locked for writing.

   Is it enough to check that "read locations are not locked" *before*
   a Tx starts to write, or shared locks must be acquired and maintained
   for the whole duration of the commit? (ignore Tx3 for now in the diagram)

         Tx1                            Tx2                         Tx3

    reads A, B                  reads E, F                  reads A, B, C, D
    writes C, D                 writes A, B                 writes G
    commit means:               commit means:               commit means:
    --------------------------  --------------------------  --------------------------
    excl-lock C                                             excl-lock G
    excl-lock D                                             (W3) = incf global-clock
    (W1) = incf global-clock
    check A, B (success)
                                excl-lock A
                                excl-lock B
                                (W2) = incf global-clock
                                check E, F (success)
                                commit A: set value, (W2)
                                commit B: set value, (W2)
                                excl-unlock A
                                excl-unlock B
                                committed
                                                            check A, B (success)
                                                            check C, D (must fail)
    commit C: set value, (W1)                               fail
    commit D: set value, (W1)                               excl-unlock G
    excl-unlock C                                           failed, must re-run
    excl-unlock D
    committed                   
    --------------------------  --------------------------
    
   No matter what checks are performed, in the above sequence both Tx1 and Tx2
   will commit. This means that Tx1 will appear to commit before Tx2 (it used
   the values of A and B before Tx2 changed them) even though it may actually
   commit after.
   Is it a problem? I.e. is it possible for a third transaction Tx3 to observe
   Tx2 as already committed but Tx1 as not yet committed?
   In order to do so, Tx3 needs to read C, D before Tx1 commits them
   (to tell that Tx1 is not committed) and A, B after Tx2 commits them
   (to tell that Tx2 is committed) and then must still be able to commit:
   figure above depicts such case too, and tells that in order to prevent Tx3
   from seeing such a non-causal situation, it must fail when it checks C, D
   on the basis that they are still locked by Tx1.

   Consequence 1: checking the reads must fail if some other Tx have exclusively
   locked those memory locations for writing.

   All possible sequences for the same Tx1 and Tx2 above must always guarantee
   that Tx1 will fail to commit if Tx1 checks A, B while Tx2 is in the process
   of committing them. This results again in the same rule just stated:

   Consequence 1 (again): checking the reads must fail if some other Tx have
   exclusively locked those memory locations for writing.

   Also, Tx1 must fail to commit if the A, B it read during the transaction
   have been modified by Tx2 before Tx1 can commit:

         Tx1                            Tx2              

    reads A, B                  reads E, F                
    writes C, D                 writes A, B               
    commit means:               commit means:             
    --------------------------  --------------------------
    excl-lock C                                           
    excl-lock D                                           
    (W1) = incf global-clock
                                excl-lock A
                                excl-lock B
                                (W2) = incf global-clock
                                check E, F (success)
                                commit A: set value, (W2)
                                commit B: set value, (W2)
                                excl-unlock A
                                excl-unlock B
                                committed
    check A, B (must fail)
    fail
    excl-unlock C                                         
    excl-unlock D
    failed, must re-run                   
    --------------------------  --------------------------

   Consequence 2: checking the reads must fail if some other Tx has modified
   them, i.e. if the values found during checks are different from the values
   stored in the transaction read log.

4) WR dependency: same as 3) - just swap Tx1 and Tx2

5) WW dependency: exclusively locking locations before committing is enough,
   as it guarantees mutual exclusion.

6) multiple dependencies: RW+WR, RW+WW (same as WR+WW) or RW+WR+WW -
   just apply simultaneously the constraints coming from each single dependency
