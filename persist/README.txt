STMX-PERSIST is a high-performance Common Lisp persistence library.

It is based on transactional memory (provided by STMX library)
and it implements persistence through a memory-mapped Lisp object store.

Design:

1. having a different mmap area for each (primitive) type simplifies tremendously
   type management (i.e. implementing of "values know their type")
   but it is a performance killer: objects in store can only contain pointers to values,
   not directly the values.
   Optimizing at least FIXNUMs, CHARs, T and NIL to be stored in-place would greatly help.

2. ABI. assign a small non-negative number to each persistent type.
   Reserve type 0 for "unallocated mmapped bytes", used to keep chained lists
   of unallocated mmapped areas.

3. ABI. primitive types fitting a CPU register (= CPU word)
   are stored in mmapped area in the following format:

   ----------------------------------------------------------------------------------
   64bit ABI uses 63 bits for both pointers and fixnums.
   pointer offset occupies lowest 48 bits, allowing 256T elements per type
   pointer tag occupies upper 15 bits, allowing 65534 types (tags 0 and 1 are reserved)

   unallocated:   0  ; to exploit sparse files; uses tag 0
   unbound:       1  ; means that the object's slot is unbound; uses tag 0
   nil:           2  ; uses tag 0
   t:             3  ; uses tag 0

   characters:    n | #x0100000000000000 ; uses type 1; only 21 bits actually used, as needed by unicode. 

   single-floats: #x02000000 in the most significant 32 bits; uses tag 2
                  then IEEE float value in the least significant 32 bits

   double-floats: not usable by this ABI, must be stored as boxed values instead

   pointers:      n ; #b0.... ; top bit: always = 0
                              ; next 15 bits: tag, explained in paragraph 4. "pointer tags" 
                              ; lowest 48 bits: offset in units of a CPU word

   fixnums:       n | #x8000000000000000 ; top bit: always = 1 to distinguish from other types
                                         ; next 1 bit: sign. 0 if positive or zero, 1 if negative
                                         ; lowest 62 bits: value in two's complement representation

   ----------------------------------------------------------------------------------
   32bit ABI uses 31 bits for both pointers and fixnums.
   pointer offset occupies lowest 24 bits, allowing 16M elements per type
   pointer type occupies upper 7 bits, allowing 124 user-defined types (types 0..3 are reserved)

   values using reserved pointer type 0:
   unallocated: #x00000000     ; to exploit sparse files; uses tag 0 
   unbound:     #x00000001     ; means that the object's slot is unbound; uses tag 0
   nil:         #x00000002     ; uses tag 0
   t:           #x00000003     ; uses tag 0

   characters:  n | #x01000000 ; #b00000001000xxxxxxxxxxxxxxxxxxxxx
                               ; top bit: always = 0
                               ; next 7 bits: tag, always = 1
                               ; next 3 bits: reserved, must be zero
                               ; lowest 21 bits (x): Unicode character code 

   single-floats: not usable by this ABI, must be stored as boxed values instead
   double-floats: not usable by this ABI, must be stored as boxed values instead

   pointers:    n              ; #b0xxxxxxxyyyyyyyyyyyyyyyyyyyyyyyy
                               ; top bit: always = 0
                               ; next 7 bits (x): tag, explained in paragraph 4. "pointer tags" 
                               ; lowest 24 bits (y): offset in units of a CPU word

   fixnums:     n | #x80000000 ; #b1sxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
                               ; top bit: always = 1 to distinguish from other types
                               ; next 1 bit (s): sign. 0 if positive or zero, 1 if negative
                               ; lowest 20 bits (x): value in two's complement representation

4. pointer tags

   0 = reserved (one of: unallocated, unbound, boolean, keyword or symbol)

   1 = character

   2 = inline float (cannot be used by 32bit ABI,
       it reuses pointer offset to store the float value)

   3 = inline double (cannot be used by 32bit and 64bit ABIs,
       it reuses pointer offset to store the double value)

   4 = boxed value not containing pointers
       (bignum, ratio, float, complex, cons/list, pathname, array or hash-table)

   5 = boxed value containing pointers
       (ratio, complex, cons/list, pathname, array or hash-table)

   6.. pointer to persistent object with user-defined type

   tag >= 5 mean that object pointed-to may contain itself pointers.


5. unallocated areas ABI

   Applies to both unallocated boxed values and to unallocated user-defined persistent types.

   word 0...N-3 : not used, can have any value

   word N-2: tag = always 0, means unallocated area
           value = pointer to next unallocated area
           
   word N-1: tag = not used, can have any value
           value = number of allocated words /4. also counts the footer (i.e. words N-2 and N-1)

   
6. boxed values

;; TODO: remove unnecessary owner from BOXes
;; reduces overhead to 1 word: fulltag = type, value = (/ allocated-words +mem-box/min-words+)
;; TODO: turn BOXes into CONS cells

   word 0: tag = type. it uses a different coding than pointer tags (see table below)
           value = pointer to owner.

   word 1: tag = available for value-specific data, for example sign bits
           value = number of allocated words /4. also counts the header (i.e. words 0 and 1)
   
   word 2... : payload. depends on type

   the lowest four bits of the type are coded as follows:

   0 = unallocated (see unallocated areas ABI)
   1 = bignum
   2 = ratio
   3 = single-float
   4 = double-float
   5 = complex of single-float
   6 = complex of double-float
   7 = complex of rationals (fixnums, bignums or ratios)
   8 = cons or list
   9 = pathname
   10 = 1-dimensional array, vector or string
   11 = multi-dimensional array
   12 = hash-table
   13...15 reserved for future use

   the remaining bits of the type are used as flags:

   #x10...#x1000 i.e. bits from 4 to 12 are reserved for future use
   #x2000 i.e. bit 13 is set to 1 if boxed value may contain pointers (helps the GC)
   #x4000 i.e. bit 14 / is set to 0 if owner is a user-defined persistent object,
                      \ is set to 1 if owner is another boxed value.

6.1. boxed bignum

   word 0: as boxed values
   word 1: tag is used as sign bit. 0 means positive, 1 for means negative
   word 2: value is number of words in the bignum (may be less than allocated words)
   word 3... : little-endian array of words, in two's complement representation,
               of bignum value

6.2. boxed ratio

   word 0: as boxed values
   word 1: as boxed values. allocated words >= 4.
   word 2: numerator, in general representation. must be either a fixnum value or a pointer
   word 3: denominator, in general representation. must be either a fixnum value or a pointer
   
6.3. boxed single-float (only needed by 32bit ABI)

   word 0: as boxed values
   word 1: as boxed values. allocated words >= 3
   word 2: single-float value in IEEE format.

6.4. boxed double-float

   word 0: as boxed values
   word 1: as boxed values. allocated words >= 3 (depends on ABI)
   word 2: double-float value in IEEE format.
   word 3: if needed by ABI, continuation of word 2.

6.5. boxed complex of single-float

   word 0: as boxed values
   word 1: as boxed values. allocated words >= 3 (depends on ABI)

   32bit ABI:
     word 2: real part. single-float value in IEEE format.
     word 3: imag part. single-float value in IEEE format.

   64bit ABI or larger:
     word 2: / lowest  32 bits: real part. single-float value in IEEE format.
             \ next    32 bits: imag part. single-float value in IEEE format.

6.6. boxed complex of double-float

   word 0: as boxed values
   word 1: as boxed values. allocated words >= 4 (depends on ABI)

   32bit ABI:
     word 2: real part. double-float value in IEEE format.
     word 3:            continuation of word 2
     word 4: imag part. double-float value in IEEE format.
     word 5:            continuation of word 4

   64bit ABI or larger:
     word 2: real part. double-float value in IEEE format.
     word 3: imag part. double-float value in IEEE format.


6.7. boxed complex of rationals (fixnums, bignums or ratios)

   word 0: as boxed values
   word 1: as boxed values. allocated words >= 4

   word 2: real part. uses general format, i.e. either an inline fixnum or a pointer
   word 3: imag part. uses general format, i.e. either an inline fixnum or a pointer

6.8. boxed cons or list

   word 0: as boxed values
   word 1: as boxed values
           tag = 0 means proper list
           tag = 1 means improper list, i.e. last cons is a dotted pair

   word 2: list length. may be smaller than allocated length
   word 3... : elements of the list, stored in general format

6.9. boxed 1-dimensional array, vector or string

   word 0: as boxed values
   word 1: as boxed values
           tag is used as element type (see paragraph 7 "array element type")
  
   word 2: array length. may be smaller than allocated length
   word 3... : elements of the array.
               stored in general format if element type = 0,
               otherwise may be stored in compact forms specific to the element type

6.10. boxed N-dimensional array

   word 0: as boxed values
   word 1: as boxed values
           tag is used as element type (see paragraph 7 "array element type")
  
   word 2: array rank (R) i.e. dimensionality
   word 3...R+2 : dimensions
   word R+3... : elements of the array.
                 stored in general format if element type = 0,
                 otherwise may be stored in compact forms specific to the element type

6.11. boxed hash-table

   word 0: as boxed values
   word 1: as boxed values
           tag is used as hash-key comparator: 0 = eq, 1 = eql, 2 = equal, 3 = equalp
  
   word 2: size of hash-table (N). may be smaller than half allocated length
   word 3...2+N*2 : sequence of entries: key1, value1, key2, value2 ... keyN, valueN
                    stored in general format
