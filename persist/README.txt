STMX-PERSIST is a high-performance Common Lisp persistence library.

It is based on transactional memory (provided by STMX library)
and it implements persistence through a memory-mapped Lisp object store.

Notes:

1) having a different mmap area for each (primitive) type simplifies tremendously
   type management (i.e. implementing of "values know their type")
   but it is a performance killer: objects in store can only contain pointers to values,
   not directly the values.
   Optimizing at least FIXNUMs, CHARs, T and NIL to be stored in-place would greatly help.

2) ABI. assign a small non-negative number to each persistent type.
   Reserve type 0 for "unallocated mmapped bytes", used to keep chained lists
   of unallocated mmapped areas.

3) ABI. primitive types fitting a CPU register (= CPU word)
   are stored in mmapped area in the following format:

   ----------------------------------------------------------------------------------
   64bit ABI uses 63 bits for both pointers and fixnums.
   pointer offset occupies lowest 48 bits, allowing 256T elements per type
   pointer type occupies upper 15 bits, allowing 65534 types (types 0 and 1 are reserved)

   unallocated: 0  ; to exploit sparse files. uses type 0
   nil:         1  ; uses type 0
   t:           2  ; uses type 0
   characters:  #x0100000000000000 | n ; uses type 1; only 21 bits actually used, as needed by unicode. 
   pointers:    n ; #b0.... ; lowest 48 bits are the offset, in units of a CPU word
                            ; highest 15 bits are the type
   fixnums:     #x8000000000000000 | n ; 62 bits + sign

   ----------------------------------------------------------------------------------
   32bit ABI uses 31 bits for both pointers and fixnums.
   pointer offset occupies lowest 24 bits, allowing 16M elements per type
   pointer type occupies upper 7 bits, allowing 126 types (types 0 and 1 are reserved)

   values using reserved pointer type 0:
   unallocated: #x00000000     ; to exploit sparse files. named +unallocated+ in Lisp
   nil:         #x00000001
   t:           #x00000002
   characters:  #x01000000 | n ; #b00000001000xxxxxxxxxxxxxxxxxxxxx ; 21 bits, as needed by unicode
   pointers:    n              ; #b0xxxxxxxyyyyyyyyyyyyyyyyyyyyyyyy
                               ; x = type (7 bits)
                               ; y = offset (24 bits) in units of a CPU word
   fixnums:     #x80000000 | n ; #b1sxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
                               ; s = sign (1 bit)
                               ; x = value (30 bits)

4) tags

   0 = reserved (unallocated, keyword or symbol)
   1 = character
   
   The rest are pointers:
   2 = pointer to bignum   
   
   4 = pointer to single-float
   5 = pointer to double-float
   6 = pointer to complex of single-float
   7 = pointer to complex of double-float
   
   8 = pointer to ratio (which contains no pointers, i.e. both numerator and denominator are mem-int)
   9 = pointer to complex of fixnum (which contains no pointers, i.e. both real and imag parts are mem-int)
   10 = pointer to cons or list (which contains no pointers)
   11 = pointer to array (which contains no pointers)
   
   12 = pointer to ratio (which contains pointers, i.e. numerator or denominator are bignums)
   13 = pointer to complex of pointer (which contains pointers, i.e. real or imag parts are bignums or ratios)
   14 = pointer to cons or list (which contains pointers)
   15 = pointer to array (which contains pointers)
   
   HASH-TABLE ??? must be added...
   
   16... pointer to user-defined persistent type

   tag >= 12 indicate that object pointed-to may contain itself pointers.

   
5) allocated  areas ABI

   word 0: tag = type of this area. tag = 0 means unallocated area (see paragraph 6)
           value = number of allocated words. also counts the header (i.e. words 0 and 1)
           
   word 1: tag = type of owner      \ the object that points to this one.
           value = pointer to owner / used to simplify GC. tag = 0 means eligible for GC.
           
   word 2... : payload. depends on type
   
   
6) unallocated areas ABI.

   word 0: tag = always 0, means unallocated area
           value = number of words in this area. also counts the header (i.e. words 0 and 1)
           
   word 1: tag = always 0, means unallocated area
           value = pointer to next unallocated area (it is a circular list)

   word 2... : not used
 

   
7) arrays ABI.

   they start with allocated areas ABI, then:

   CONS: easy, just store the CAR and CDR

   COMPLEX-SINGLE-FLOAT: easy, just store the REALPART and IMAGPART floating points in place (not as pointers)
   COMPLEX-DOUBLE-FLOAT: easy, just store the REALPART and IMAGPART floating points in place (not as pointers)
   BUILTIN-COMPLEX: store the REALPART and IMAGPART using the general purpose format

   vectors and one-dimensional arrays:
           store the element type (it's a small non-negative number,
                                   implies the element size to allow bit arrays),
                 with the 1-dimensional, adjustable and fill-pointer flags (merge them with element type),
           then the length (= number of elements),
           then the elements

   multi-dimensional arrays:
           store the element type (it's a small non-negative number,
                                   implies the element size to allow bit arrays),
                 with the N-dimensional, adjustable and fill-pointer flags (merge them with element type),
           then the number of dimensions,
           then the actual dimensions,
           then the elements

