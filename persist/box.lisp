;; -*- lisp -*-

;; this file is part of stmx-persist.
;; copyright (c) 2013 massimiliano ghilardi
;;
;; this library is free software: you can redistribute it and/or
;; modify it under the terms of the lisp lesser general public license
;; (http://opensource.franz.com/preamble.html), known as the llgpl.
;;
;; this library is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.
;; see the lisp lesser general public license for more details.


(in-package :stmx-persist)


;;;; boxed values, i.e. mem-box, are variable-length mmap areas
;;;; used to store all kind of CL built-in types that do not fit a single CPU word:
;;;; bignums, ratios, single-floats and double-floats, complexes,
;;;; pathnames, cons cells and lists, vectors, arrays, strings and hash-tables.
;;;;
;;;; mem-boxes are allocates in multiples of 4 (actually +mem-box/min-words+) CPU words,
;;;; and they contain a 2 CPU-word header followed by type-specific payload:
;;;;
;;;;   word 0: tag = type. it uses a different coding than pointer tags (see +mem-box-*+ constants)
;;;;           value = pointer to owner. used by GC.
;;;;
;;;;   word 1: tag = available for type-specific data, for example sign bits
;;;;           value = number of allocated words / 4. also counts the header (i.e. words 0 and 1)
;;;;
;;;;   word 2... : payload. depends on type




(defmacro mwrite-box-0 (ptr index boxed-type owner)
  "Write to memory the 0-th word of a boxed value"
  `(mset-fulltag-and-value ,ptr ,index ,boxed-type ,owner))

(defmacro mwrite-box-1 (ptr index payload-specific-tag allocated-words/4)
  "Write to memory the 1-st word of a boxed value"
  `(mset-fulltag-and-value ,ptr ,index ,payload-specific-tag ,allocated-words/4))

(defmacro mread-box-0 (ptr index)
  `(mget-fulltag-and-value ,ptr ,index))

(defmacro mread-box-1 (ptr index)
  `(mget-fulltag-and-value ,ptr ,index))


(declaim (inline box-pointer->size size->box-pointer))

(defun box-pointer->size (value)
  (declare (type mem-pointer value))
  (the mem-size (* value +mem-box/min-words+)))

(defun size->box-pointer (index)
  (declare (type mem-size index))
  (the mem-pointer (nth-value 0 (truncate index +mem-box/min-words+))))


(declaim (inline %make-mfree-cell mfree-cell-index mfree-cell-n-words mfree-cell-next))

(defstruct (mfree-cell (:constructor %make-mfree-cell))
  (index   +mem-unallocated+ :type mem-size)
  (n-words 0                 :type mem-size)
  (next    nil               :type (or null mfree-cell)))


(defun new-mfree-cell (index n-words &optional next)
  (%make-mfree-cell :index index :n-words n-words :next next))


(defun mfree-cell-null? (cell)
  "Return T if cell is full of zeroes, for example when loaded from a newly created file."
  (declare (type mfree-cell cell))
  (and (eql   (mfree-cell-index cell) +mem-unallocated+)
       (zerop (mfree-cell-n-words cell))
       (null  (mfree-cell-next    cell))))


(declaim (inline mwrite-free-cell-next mwrite-box-n-words mwrite-free-cell-n-words))

(defun mwrite-free-cell-next (ptr cell)
  "Write the NEXT slot of a mfree-cell into mmap memory starting
at (+ PTR (mfree-cell-index CELL))"

  (declare (type maddress ptr)
           (type mfree-cell cell))

  (let* ((index (mfree-cell-index cell))
         (next  (mfree-cell-next cell))
         (next-index (if next (mfree-cell-index next) 0)))

    (mwrite-box-0 ptr index +mem-unallocated+ (size->box-pointer next-index))))


(defun mwrite-box-n-words (ptr index n-words)
  "Write the N-WORDS slot of a box or free-cell into mmap memory starting
at (+ PTR INDEX)"

  (declare (type maddress ptr)
           (type mem-size index n-words))

  (mwrite-box-1 ptr (mem-size+1 index) +mem-unallocated+ (size->box-pointer n-words)))


(defun mwrite-free-cell-n-words (ptr cell &optional (n-words (mfree-cell-n-words cell)))
  "Write the N-WORDS slot of a mfree-cell into mmap memory starting
at (+ PTR (mfree-cell-index CELL))"

  (let ((index (mfree-cell-index cell)))

    (mwrite-box-n-words ptr index n-words)))



(defun mwrite-free-cell (ptr cell)
  "Write a mfree-cell into mmap memory starting at (+ PTR (mfree-cell-index CELL))"

  (declare (type maddress ptr)
           (type mfree-cell cell))

  (mwrite-free-cell-next    ptr cell)
  (mwrite-free-cell-n-words ptr cell))







(declaim (inline mread-box-n-words))

(defun mread-box-n-words (ptr index)
  "Read N-WORDS from box or free cell in mmap memory starting at (PTR+INDEX) and return it."

  (declare (type maddress ptr)
           (type mem-size index))

  (box-pointer->size (mget-value ptr (mem-size+1 index))))


(defun mread-free-cell (ptr index)
  "Read a mfree-cell from mmap memory starting at (PTR+INDEX) and return it.
Note: NEXT slot of returned object always contains NIL,
      instead NEXT value stored in mmap is returned as multiple values."

  (declare (type maddress ptr)
           (type mem-size index))

  (let ((next    (box-pointer->size (mget-value ptr index)))
        (n-words (mread-box-n-words ptr index)))
    (values
     (new-mfree-cell index n-words)
     (the mem-size next))))



(defun mread-free-list (ptr index)
  "Read a list of MFREE-CELL from memory starting at (PTR+INDEX) and return it.
FIXME: it currently loads the whole free-list in RAM (bad!)"
  (declare (type mem-size index))

  (let ((head)
        (prev))
    (loop
       (multiple-value-bind (this next-index) (mread-free-cell ptr index)
         (declare (type mem-size next-index))

         (if prev
           (setf (mfree-cell-next prev) this)
           (setf head this
                 ;; head is just a pointer to next cell, it must have zero free words
                 (mfree-cell-n-words this) 0))

         (when (zerop next-index)
           (return head))

         (setf prev this
               index next-index)))))



(defvar *mfree* nil "thread-local list of unallocated mmap memory")

(defun mem-free (ptr index &optional (n-words
                                      (mread-box-n-words ptr index)))
  "A very naive deallocator. Useful only for debugging and development."
  (declare (type maddress ptr)
           (type mem-size index))

  (let ((head *mfree*))
    
    (unless (zerop n-words)
      ;; mfree-cells are written at the end of the free mmap area they represent!
      (incf-mem-size index (mem-size- n-words +mem-box/min-words+))

      (let* ((next (mfree-cell-next head))
             (this (new-mfree-cell index n-words next)))
    
        (setf (mfree-cell-next head) this)
        (mwrite-free-cell      ptr this)
        (mwrite-free-cell-next ptr head)))
    
    head))




(defun mem-alloc (ptr n-words)
  "A very naive first-fit allocator for mmap areas. Useful only for debugging and development."
  (declare (type maddress ptr)
           (type mem-size n-words))

  ;; trying to allocate zero words? then return invalid pointer
  (when (zerop n-words)
    (return-from mem-alloc +mem-unallocated+))

  (when (> n-words +mem-box/max-words+)
    (error "cannot allocate ~S consecutive words from mmap area. Maximum supported is ~S words"
            n-words +mem-box/max-words+))

  ;; round up n-words to a multiple of +mem-box/min-words+
  (let ((remainder (logand n-words (1- +mem-box/min-words+))))
    (unless (zerop remainder)
      (incf-mem-size n-words (- +mem-box/min-words+ remainder))))

  (loop for prev = *mfree* then this
     for this = (mfree-cell-next prev) then next
     while this
     for this-len = (mfree-cell-n-words this)
     for next = (mfree-cell-next this)
     do
       (when (>= this-len n-words)
         ;; mfree-cells are written at the end of the free mmap area they represent!
         (let ((result (mem-size- (mfree-cell-index this) (mem-size- this-len +mem-box/min-words+))))
           ;; update this length
           (decf this-len n-words)

           (if (zerop this-len)
               ;; exact match? then remove THIS from free list (it cannot be the head)
               (let ((next (mfree-cell-next this)))
                 (setf (mfree-cell-next prev) next)
                 ;; write back the new link PREV->NEXT that bypasses THIS
                 (mwrite-free-cell-next ptr prev))

               ;; otherwise update THIS n-words
               (progn
                 (setf (mfree-cell-n-words this) this-len)
                 ;; also write back THIS n-words in mmap area
                 (mwrite-free-cell-n-words ptr this this-len)))

           ;; we have an allocated n-words block starting at RESULT
           ;; write to mmap its number of words
           (mwrite-box-n-words ptr result n-words)

           (return-from mem-alloc result))))

  (error "out of memory! failed to allocate ~S words from mmap area ~S" n-words ptr))

             
                 

(defun mem-alloc-rounded (ptr n-words)
  "Round up N-WORDS somewhat (typically 25%) then allocate that many words from mmap area."
  (declare (type maddress ptr)
           (type mem-size n-words))

  (let ((delta
         (if (<= n-words #.(truncate +mem-box/max-words+ 2))
             n-words
             (mem-size- +mem-box/max-words+ n-words))))

    (the (values mem-size &optional)
      (mem-alloc ptr (mem-size+ n-words (ash delta 2))))))

