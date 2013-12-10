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


(declaim (inline box-index   (setf box-index)
                 box-n-words (setf box-n-words)
                 box-value   (setf box-value)
                 box-next    (setf box-next)))


#|
;; wrapper for values that cannot be stored as unboxed
(deftype box () 'cons)

(defun make-box (value index n-words)
  "Create a new box to wrap VALUE. Assumes VALUE will be stored at INDEX in memory store."
  (declare (type mem-size index n-words))
  `(,value ,index . ,n-words)))

(defun box-value (box)
  (declare (type box box))
  (first box))

(defun (setf box-value) (value box)
  (declare (type box box))
  (setf (first box) value))

(defun box-index (box)
  (declare (type box box))
  (the mem-size (second box)))

(defun (setf box-index) (index box)
  (declare (type box box)
           (type mem-size index))
  (setf (second box) index))

(defun box-n-words (box)
  (declare (type box box))
  (the mem-size (rest (rest box))))

(defun (setf box-n-words) (n-words box)
  (declare (type box box)
           (type mem-size n-words))
  (setf (rest (rest box)) n-words))
|#


(declaim (inline %make-box))
(defstruct (box (:constructor %make-box))
  (index   0 :type mem-size)
  (n-words 0 :type mem-size)
  (value   nil))


(defun make-box (index n-words &optional value)
  "Create a new box to wrap VALUE. Assumes VALUE will be stored at INDEX in memory store."
  (declare (type mem-size index n-words))
  (%make-box :index index :n-words n-words :value value))

(defun box-next (box)
  (declare (type box box))
  (box-value box))

(defun (setf box-next) (value box)
  (declare (type box box))
  (setf (box-value box) value))

           

;; TODO: remove unnecessary owner from BOXes
;; reduces overhead to 1 word: fulltag = type, value = (/ allocated-words +mem-box/min-words+)
;; TODO: turn BOXes into CONS cells

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


(defun box-null? (box)
  "Return T if box is full of zeroes, for example when loaded from a newly created file."
  (declare (type box box))
  (and (<     (box-index   box) +mem-box/min-words+)
       (zerop (box-n-words box))
       (null  (box-value   box))))


(declaim (inline mwrite-box-next mwrite-box-n-words mwrite-box-n-words))

(defun mwrite-box-next/free (ptr box)
  "Write the NEXT slot of a free box into mmap memory starting
at (+ PTR (box-index BOX))"

  (declare (type maddress ptr)
           (type box box))

  (let* ((index (box-index box))
         (next  (box-next box))
         (next-index (if next (box-index next) 0)))

    (mwrite-box-0 ptr index +mem-unallocated+ (size->box-pointer next-index))))


(defun %mwrite-box-n-words (ptr index n-words)
  "Write the N-WORDS slot of a box or box into mmap memory starting
at (+ PTR INDEX)"

  (declare (type maddress ptr)
           (type mem-size index n-words))

  (mwrite-box-1 ptr (mem-size+1 index) +mem-unallocated+ (size->box-pointer n-words)))


(defun mwrite-box-n-words (ptr box &optional (n-words (box-n-words box)))
  "Write the N-WORDS slot of a box into mmap memory starting
at (+ PTR (box-index BOX))"

  (%mwrite-box-n-words ptr (box-index box) n-words))



(defun mwrite-box/free (ptr box)
  "Write a free box into mmap memory starting at (+ PTR (box-index BOX))"
  (declare (type maddress ptr)
           (type box box))

  (mwrite-box-next/free ptr box)
  (mwrite-box-n-words   ptr box))







(declaim (inline mread-box-n-words))

(defun mread-box-n-words (ptr index)
  "Read N-WORDS from box in mmap memory starting at (PTR+INDEX) and return it."

  (declare (type maddress ptr)
           (type mem-size index))

  (box-pointer->size (mget-value ptr (mem-size+1 index))))


(defun mread-box/free (ptr index)
  "Read a free box from mmap memory starting at (PTR+INDEX) and return it.
Note: NEXT slot of returned object always contains NIL,
      instead NEXT value stored in mmap is returned as multiple values."

  (declare (type maddress ptr)
           (type mem-size index))

  (let* ((next-index (mem-size+ +mem-box/payload-words+
                                (box-pointer->size (mget-value ptr index))))
         (n-words    (mread-box-n-words ptr index)))
    (values
     (make-box index n-words)
     (the mem-size next-index))))



(defvar *mfree* nil "thread-local list of unallocated mmap memory")


(defun init-free-list (ptr total-n-words)
  "Create and return a new free list containing ALL the words up to TOTAL-N-WORDS."
  (declare (type maddress ptr)
           (ignore ptr)
           (type mem-size total-n-words))
  (let ((lo +mem-box/payload-words+)
        (hi (mem-size- total-n-words +mem-box/header-words+)))

    (setf *mfree*
          (make-box +mem-box/payload-words+ 0
                    (make-box hi (mem-size- hi lo))))))


(defun mread-free-list (ptr)
  "Read a list of BOX from memory starting at (PTR + +MEM-BOX/PAYLOAD-WORDS+) and return it.
FIXME: it currently loads the whole free-list in RAM (bad!)"
  (declare (type maddress ptr))

  (let ((index +mem-box/payload-words+)
        (head)
        (prev))
    (loop
       (multiple-value-bind (this next-index) (mread-box/free ptr index)
         (declare (type mem-size next-index))

         (if prev
           (setf (box-next prev) this)
           (setf head this
                 ;; head is just a pointer to next box, it must have zero free words
                 (box-n-words this) 0))

         (when (< next-index +mem-box/min-words+)
           (return (setf *mfree* head)))

         (setf prev this
               index next-index)))))


(defun minsert-box/free (ptr prev box lo hi)
  (declare (type maddress ptr)
           (type box prev box)
           (type mem-size lo hi))

  (let ((curr (box-next prev)))
    ;; boxes are written at the end of the free mmap area they represent!
    (setf (box-index   box) hi
          (box-n-words box) (mem-size- hi lo)
          (box-next    box) curr
          (box-next    prev) box)

    (mwrite-box/free ptr box)
    (mwrite-box-next ptr prev)
    prev))


(defun %box-free (ptr head box lo hi)
  (declare (type maddress ptr)
           (type box head box)
           (type mem-size lo hi))

  (loop
     for prev = head then curr
     for curr = (box-next prev)
     while curr
     for curr-hi = (box-index curr)
     for curr-n  = (box-n-words curr)
     for curr-lo = (mem-size- curr-hi curr-n)
     do
       (cond
         ((> lo curr-hi)
          nil)
         ((= lo curr-hi)
          (setf lo curr-lo)
          (setf (box-next prev) (box-next curr))
          (mwrite-box-next ptr prev)
          (setf curr prev))
         ((= hi curr-lo)
          (setf hi curr-hi)
          (setf (box-next prev) (box-next curr))
          (mwrite-box-next ptr prev)
          (setf curr prev))
         ((< hi curr-lo)
          (loop-finish)))
     finally
       (minsert-box/free ptr prev box lo hi)))


(defun box-free (ptr box)
  "A very naive deallocator. Useful only for debugging and development."
  (declare (type maddress ptr)
           (type box box))

  (let ((head *mfree*)
        (n-words (box-n-words box)))
    
    (unless (zerop n-words)
      (let* ((lo (mem-size- (box-index box) +mem-box/header-words+))
             (hi (mem-size+ lo n-words)))
        (%box-free ptr head box lo hi)))

    head))






(defun box-alloc (ptr n-words)
  "A very naive first-fit allocator for mmap areas. Useful only for debugging and development."
  (declare (type maddress ptr)
           (type mem-size n-words))

  ;; trying to allocate zero words? then return invalid pointer
  (when (zerop n-words)
    (return-from box-alloc nil))

  (when (> n-words +mem-box/max-words+)
    (error "cannot allocate ~S consecutive words from mmap area. Maximum supported is ~S words"
            n-words +mem-box/max-words+))

  ;; round up n-words to a multiple of +mem-box/min-words+
  (let ((remainder (logand n-words (1- +mem-box/min-words+))))
    (unless (zerop remainder)
      (incf-mem-size n-words (- +mem-box/min-words+ remainder))))

  (loop for prev = *mfree* then this
     for this = (box-next prev) then next
     while this
     for this-len = (box-n-words this)
     for next = (box-next this)
     do
       (when (>= this-len n-words)
         ;; boxes are written at the end of the free mmap area they represent!
         (let ((result (mem-size- (box-index this) (mem-size- this-len +mem-box/header-words+)))
               (box nil))
           ;; update this length
           (decf this-len n-words)

           (if (zerop this-len)
               ;; exact match? then remove THIS from free list (it cannot be the head)
               (let ((next (box-next this)))
                 (setf (box-next prev) next)
                 ;; write back the new link PREV->NEXT that bypasses THIS
                 (mwrite-box-next ptr prev)
                 (setf box this
                       (box-index box) result))

               ;; otherwise update THIS n-words
               (progn
                 (setf (box-n-words this) this-len
                       ;; create and return a new box
                       box (make-box result n-words))
                 (mwrite-box-n-words ptr this)))

           (return-from box-alloc box))))

  (error "out of memory! failed to allocate ~S words from mmap area ~S" n-words ptr))

             
                 

(defun box-alloc-rounded (ptr n-words)
  "Round up N-WORDS somewhat (typically 25%) then allocate that many words from mmap area."
  (declare (type maddress ptr)
           (type mem-size n-words))

  (let ((delta
         (if (<= n-words #.(truncate +mem-box/max-words+ 2))
             n-words
             (mem-size- +mem-box/max-words+ n-words))))

    (the (values mem-size &optional)
      (box-alloc ptr (mem-size+ n-words (ash delta 2))))))






