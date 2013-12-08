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

;; wrapper for values that cannot be stored as unboxed
(deftype box () 'cons)

(defun make-box (value index n-words)
  "Create a new box to wrap VALUE. Assumes VALUE will be stored at INDEX in memory store."
  (declare (type mem-size index n-words))
  (cons value (cons index n-words)))

(declaim (inline box-value box-index box-n-words))

(defun box-value (box)
  (declare (type box box))
  (first box))

(defun box-index (box)
  (declare (type box box))
  (the mem-size (second box)))

(defun box-n-words (box)
  (declare (type box box))
  (the mem-size (rest (rest box))))




(declaim (inline mwrite-box/header))

(defun mwrite-box/header (ptr owner box boxed-type value-specific-fulltag)
  "Write to mmap area the header common to all boxed values.
Return INDEX pointing to box payload"
  (declare (type maddress ptr)
           (type box box)
           (type mem-fulltag boxed-type value-specific-fulltag))

  (let ((index (box-index box)))
    (mwrite-box-0 ptr index boxed-type owner)
    (incf (the mem-size index))
    (mwrite-box-1 ptr index value-specific-fulltag (size->box-pointer (box-n-words box)))
    (incf (the mem-size index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    BIGNUMs                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun bignum-words (n)
  "Return the number of words needed to store bignum N in memory."
  (declare (type integer n))

  (let* ((len (integer-length n))
         (words (truncate (+ len +mem-word/bits+ -1) ;; round up
                          +mem-word/bits+)))
    (unless (<= words +mem-bignum/max-words+)
      (error "STMX-PERSIST: bignum too large for object store,
    it requires ~S words, maximum supported is ~S words"
             words +most-positive-size+))

    (the (integer 0 #.+mem-bignum/max-words+) words)))



(defun %mwrite-bignum-loop (ptr index n-words n)
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type integer n))

  (let ((shift (- +mem-word/bits+))
        (mask +mem-word/mask+))

    (loop for i-word from n-words downto 1 do
         (mset-word ptr (incf (the mem-size index)) (logand n mask))
         (setf n (ash n shift)))))


(defun %mwrite-bignum-recurse (ptr index n-words n)
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type integer n))

  (log.trace "index: ~S n-words: ~S n: #x~X" index n-words n)

  (if (<= n-words 16)
      (%mwrite-bignum-loop ptr index n-words n)

      (let* ((n-words-high (truncate n-words 2))
             (n-words-low  (- n-words n-words-high))
             (high-shift (the fixnum (* n-words-low +mem-word/bits+)))
             (low-mask (1- (ash 1 high-shift))))

        (%mwrite-bignum-recurse ptr index n-words-low (logand n low-mask))
        (%mwrite-bignum-recurse ptr (the mem-size (+ index n-words-low))
                                n-words-high (ash n (- high-shift))))))


(defun mwrite-box/bignum (ptr owner box)
  "Reuse the memory block starting at (PTR+INDEX) and write bignum N into it.

ABI: bignum is stored as box prefix (with N's sign as value-specific-tag),
followed by mem-int (bignum-words N), followed by an array of words."
  (declare (type maddress ptr)
           (type mem-pointer owner)
           (type box box))

  (let* ((index (box-index box))
         (n (the integer (box-value box)))
         (n-words (bignum-words n)))

    (setf index
          (mwrite-box/header ptr owner box +mem-box-bignum+ (if (< n 0) 1 0)))

    (mset-int ptr index n-words)
    (%mwrite-bignum-recurse ptr index n-words n)))
      

(defun %mread-pos-bignum-loop (ptr index n-words)
  "Read an unsigned bignum"
  (declare (type maddress ptr)
           (type mem-size index n-words))

  (let* ((bits +mem-word/bits+)
         (limit (the fixnum (* bits n-words)))
         (n 0))
    (declare (type integer n))

    (loop for shift from 0 below limit by bits
       for word = (mget-word ptr (incf (the mem-size index))) do
         (setf n (logior n (ash word shift))))

    (the integer n)))


(defun %mread-neg-bignum-loop (ptr index n-words)
  "Read a negative bignum"
  (declare (type maddress ptr)
           (type mem-size index n-words))

  (decf (the mem-size n-words))

  (let* ((n (%mread-pos-bignum-loop ptr index n-words))
         ;; read last word as negative
         (bits  +mem-word/bits+)
         (limit (the fixnum (* bits n-words)))
         (word  (mget-word ptr (the mem-size (+ n-words (incf (the mem-size index)))))))

    (the integer (logior n (ash (logior word #.(- -1 +mem-word/mask+)) limit)))))




(defun %mread-bignum-recurse (ptr index n-words sign)
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type mem-fulltag sign))

  (if (<= n-words 16)
      (if (zerop sign)
          (%mread-pos-bignum-loop ptr index n-words)
          (%mread-neg-bignum-loop ptr index n-words))

      (let* ((n-words-high (truncate n-words 2))
             (n-words-low  (- n-words n-words-high))

             (n-low  (%mread-bignum-recurse ptr index n-words-low 0))
             (n-high (%mread-bignum-recurse ptr (the mem-size (+ index n-words-low))
                                            n-words-high sign))

             (high-shift (the fixnum (* n-words-low +mem-word/bits+))))

        (log.trace "n-low: #x~X n-high: #x~X" n-low n-high)

        (logior n-low (ash n-high high-shift)))))


(defun mread-box/bignum (ptr index)
  "Read a bignum from the boxed memory starting at (PTR+INDEX).
Return a new BOX wrapping the bignum"
  (declare (type maddress ptr)
           (type mem-size index))
  
  ;; read sign at INDEX+1 and bignum-words at INDEX+2
  (multiple-value-bind (sign allocated-words/4)
      (mget-fulltag-and-value ptr (mem-size+1 index))

    (let* ((n-words (mget-int              ptr (mem-size+ index +mem-box/header-words+)))
           (n       (%mread-bignum-recurse ptr index n-words sign)))

      (make-box n index (box-pointer->size allocated-words/4)))))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    STRINGs                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mwrite-string (ptr index string &optional (start 0) (end (length string)))
  "Write (END-START) characters from STRING to the memory starting at (PTR+INDEX).
Characters will be stored using the general-purpose representation."
  (declare (type maddress ptr)
           (type mem-size index)
           (type simple-array string)
           (type ufixnum start end))

  (loop for i from start below end do
       (mset-character ptr (the mem-size (+ index i))
                       (svref string i))))


(defun mread-string (ptr index result-string &optional (start 0) (end (length result-string)))
  "Read (END-START) characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING. Return RESULT-STRING.
Characters will be read using the general-purpose representation."
  (declare (type maddress ptr)
           (type mem-size index)
           (type simple-array result-string)
           (type ufixnum start end))

  (loop for i from start below end do
       (setf (svref result-string i)
             (mget-character ptr (the mem-size (+ index i)))))
  result-string)


(defun mwrite-base-string (ptr index string &optional (start 0) (end (length string)))
  "Write (END-START) single-byte characters from STRING into the memory starting at (PTR+INDEX).
Characters are written using the compact, single-byte representation.
For this reason the codes of all characters to be stored must be in the range
0 ... +most-positive-byte+ (typically 0 ... 255)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type simple-array string)
           (type ufixnum start end))

  (loop for i from start below end do
       (%mset-t (the (unsigned-byte #.+mem-byte/bits+)
                  (char-code
                   (svref string i))) :byte
                ptr (the mem-size (+ index i)))))



(defun mread-base-string (ptr index result-string &optional (start 0) (end (length result-string)))
  "Read (END-START) single-byte characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING.  Return RESULT-STRING.
Characters are read from memory using the compact, single-byte representation.
For this reason only codes in the range 0 ... +most-positive-byte+ can be read
\(typically 0 ... 255)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type simple-array result-string)
           (type ufixnum start end))

  (loop for i from start below end do
       (setf (svref result-string i)
             (code-char
              (the (unsigned-byte #.+mem-byte/bits+)
                (%mget-t :byte ptr (the mem-size (+ index i)))))))
  result-string)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    generic    BOXes                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mwrite-box (ptr owner index value)

  #+never
  (when (typep value 'integer)
    (when (> n-words +mem-bignum/max-words+)
      (error "bignum too large, cannot store in mmap: this bignum requires ~S words,
    maximum supported words in a bignum is ~S" n-words +mem-bignum/max-words+))


    (let (;; overhead is: bignum-words prefix (1 word) plus box header (2 words)
          (min-allocated-words (the mem-size (+ 1 n-words +mem-box/header-words+)))
          (allocated-words (if (mem-invalid-index? ptr index) 0 (mread-box-n-words *p* index))))
      
      ;; if current memory is not large enough, free it and allocate a new block
      (when (< allocated-words min-allocated-words)
        (mem-free ptr index allocated-words)
        (setf index (mem-alloc-rounded ptr min-allocated-words))))))

