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



Real memory barriers with atomic compare-and-swap
---------------------------------------------------

Suppose an application gives the transactional guarantee than A = B, i.e. all
transactions either don't modify A and B or modify them so that at commit it
will be A = B. Can we guarantee that all transactions will **see** A = B while
running, provided they didn't (yet) modify them?

Suppose we have real memory barriers and also atomic compare-and-swap.
How can we guarantee such consistent reads?

First attempt: do not wrap TVARs value and version in a CONS.
Embed TVARs lock as a bit inside their version, and set it with compare-and-swap.
When reading a TVARs get version + lock bit, then value, finally version + lock bit again;
rerun if the two version + lock bit are different, or if (they are equal but) the second is locked.
When committing, set lock first (with atomic compare-and-swap), then set value,
finally set version with lock bit cleared, which implies an unlock.

         Tx1                            Tx2                            Tx3

    start transaction              start transaction              start transaction
    (R1) = global-clock                                           
    write new A to Tx log                                         (R3) = global-clock < (W1)
    write new B to Tx log                                         
                                                                  read A: old value, old version,
    commit                                                                unlocked
      commit means:                                               
    --------------------------                                    read B: get version
    excl-lock A                                                     -> either old + unlocked (ok)
    excl-lock B                                                     -> or old + lock (DANGER)
    (W1) = incf global-clock
                                  (R2) = global-clock = (W1)
    set value first,            
      then version:               
    commit A: set value           
              set (W1, unlocked)                                          
                                  read B: get version             
    commit B: set value             -> either old + lock          read B: get value
                                       (DANGER)                     -> either old (ok)
              set (W1, unlocked)    -> or new + unlocked,           -> or new (DANGER)
                                       then guarantees new value
                                                                  read B: get version again
                                  read B: get value                 -> either old + unlocked,
                                    -> old or new if old version       ok, value must be old
                                    -> surely new if new version    -> either old + lock (rerun)
                                                                    -> or new (rerun)
    committed                     read B: get version again     
                                    -> either old + lock
                                    -> or new + unlocked

                                  if the two B versions+lock,
                                  are different or the second
                                  is locked, rerun. Ensures
                                  that old value is avoided.

                                  read A: new value & version
                                          & unlocked
                                  ok, got A = B (new values)      ok, got A = B (old values)
                                      or rerun                        or rerun
    ----------------------------  -----------------------------   ----------------------------  

It works.


Real memory barriers but no atomic compare-and-swap
---------------------------------------------------

Same problem as previous paragraph. We have real memory barriers
but no atomic compare-and-swap, so without a lock bit embedded in the version.

How can we still guarantee consistent reads?

First of all, notice that the lock bit in the version read the first time is not
actually used, such "early" version read is only to detect version number
changes. Only the lock bit in the second read of the version is actually used.

From this, we deduce that we can split the lock bit and the version and omit
reading the lock bit the first time, provided that we use a pessimistic approach
by guaranteeing that the lock will be found locked at least as often as previously.

To get such behaviour, we test the lock (now a standard bordeaux-threads lock)
before reading the version for the second time.

Let's see if it works: do not wrap TVARs value and version in a CONS, and
read TVARs in this order: version (1st time), value, lock, version (2nd time).

         Tx1                            Tx2                            Tx3

    start transaction              start transaction              start transaction
    (R1) = global-clock                                           
    write new A to Tx log                                        
    write new B to Tx log                                         (R3) = global-clock < (W1)     
    commit                                                       
      commit means:                                               read A: old value, old version,
    --------------------------                                            unlocked
    excl-lock A                                                   
    excl-lock B 
    (W1) = incf global-clock                                      read B: get version
                                                                    -> old version
    set value first,            
      then version:               
    commit A: set value           
              set (W1, unlocked)
                                  (R2) = global-clock = (W1)
                                  read B: get version             
    commit B: set value             -> either old (DANGER),       read B: get value
                                       allows to get old value      -> either old (ok)
              set (W1, unlocked)    -> or new,                      -> or new (DANGER), guarantees
                                       guarantees new value            2nd version will be new
                                                                  
                                  read B: get value               
                                    -> old or new if old version  
                                    -> surely new if new version  
    unlock A                                                      
    unlock B                      check-lock B                    check-lock B
                                    -> if unlocked, guarantees      -> result does not matter
                                       2nd version will be new
                                    -> if locked, no guarantees
                                  
    committed                     read B: get version again       read B: get version again
                                    -> either old (lock detected)  -> either old, value must be old
                                    -> or new (lock may be         -> or new, value may be new
                                               undetected)            but too high version, (rerun)

                                  if the two B versions,
                                  are different or the lock
                                  is locked, rerun. Ensures
                                  that old value is avoided.

                                  read A: new value & version
                                          & unlocked
                                  ok, got A = B (new values)      ok, either A = B (old values)
                                      or rerun                        or rerun
    ----------------------------  -----------------------------   ----------------------------  

It works.


Trivial memory barriers and no atomic compare-and-swap
------------------------------------------------------

Same example as previous section, but without real memory barriers.

On a CPU that guarantees ordered reads and writes - in other words, assembler
instructions for read and write barriers are NO-OP - but without compiler support
for them, i.e. no way to stop the compiler from reordering the assembler instructions
it produces, can we still guarantee consistent Reads in such case? And how / at what price?

This is a very common case: just take an ordinary Lisp (or any other compiler)
on x86 or x86-64, without special compiler extensions.

Critical to the reasoning is that locking primitives are available,
and that "obtaining the owner of a lock" is NOT an inline function,
so that the compiler cannot optimize/reorder it.

First attempt: same technique as real memory barriers.
We just need to re-analize the last diagram above,
which reveals a critical step no longer guaranteed:
Tx2 may read B in either order: version 1, value, lock, version 2
or value, version 1, lock, version 2. Reading value as first
is obviously wrong: it may be the old value, and no way to detect it.

Second attempt: still do not wrap TVARs value and version in a CONS,
but read TVARs value & version twice: once _before_ and once _after_ checking their lock.

         Tx1                               Tx2                               Tx3

    start transaction              start transaction              start transaction
    (R1) = global-clock                                           (R3) = global-clock  < (W1)
    write new A to Tx log                                        
    write new B to Tx log       
    commit                                                        read B means:
      commit means:                                               get B value & version
    --------------------------                                    got old value & version
    excl-lock A                                                   check-lock B: unlocked, ok
    excl-lock B                                                   
    (W1) = incf global-clock
                                  (R2) = global-clock = (W1)
                                 
    no compiler memory barriers,
    cannot tell if value or
    version will be set first!
                                  
    commit A: set value, (W2)     
                                  read B means:
    commit B: set value, (W2)     get B value & version           get B value & version again.
                                                                  if we get old value & version,
                                  no compiler memory barriers:    => success. otherwise we detect
                                  may get new value & old version the mismatch and try again or rerun
                                  or vice-versa, or both new,
                                  or both old. danger...
    excl-unlock A                                         
    excl-unlock B
    committed                     check-lock B: unlocked, ok.
                                  get B value & version again.
                                  got new value & version
                                  
                                  compare with previous ones:
                                  if new version is too high, rerun.
                                  otherwise if values/versions don't
                                  match, try again or rerun.
    ----------------------------  ----------------------------  ----------------------------

It works.




Consistent Reads summary
------------------------


<table>

 <tr><th colspan="2" rowspan="2">&nbsp;</th>
     <th colspan="2">atomic compare-and-swap</th></tr>

 <tr><th>no</th>
     <th>yes</th></tr>

 <tr><th rowspan="3">memory read/write barriers</th>
     <th>no</th>
     <td>Slow. We must lock TVARs to read them, use bordeaux-threads locks.
         We can unwrap TVARs.</td>
     <td>Same as case on the left, replace locks with fast atomic compare-and-swap.</td></tr>

 <tr><th>trivial</th>
     <td>Unwrap TVARs, use bordeaux-threads locks.<br/>
         Read TVARs in this order: value and version (no way to force the order),
         then check lock, then value and version again;<br/>
         rerun if locked, of if values or version differ.<br/>
         When committing: lock, then write value and version (no way to force the order),
         finally unlock.
     <td>Same as case on the left, replace locks with fast atomic compare-and-swap.</td></tr>

 <tr><th>yes</th>
     <td>Use a standard bordeaux-threads lock, unwrap TVARs.<br/>
         When reading a TVAR: get version first, then value, then check the lock,
         then get the version again.<br/>
         If locked, or versions mismatch, or the new version is too high, rerun.<br/>
         When committing: lock, then set value first then version, finally unlock.</td>
     <td>Unwrap TVARs, embed lock bit in version.<br/>
         When reading a TVAR get version first, then value finally version again;<br/>
         rerun if the two versions are different, or are equal but locked,
         or are equal but too high.<br/>
         When committing: to lock use compare-and-swap,
         to actually write set value first then version (which also unlocks).</td></tr>

</table>


