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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (< +most-positive-pointer+ +most-positive-character+)
    
    (error "cannot compile STMX-PERSIST: assuming ~S-bit characters (i.e. Unicode),
    cannot fit them in the ~S bits reserved by ABI." +character-bits+ +mem-pointer/bits+)))



(deftype mem-word    () '(unsigned-byte #.+mem-word/bits+))
(deftype mem-pointer () '(unsigned-byte #.+mem-pointer/bits+))
(deftype mem-size    () 'mem-pointer)
(deftype mem-tag     () '(unsigned-byte #.+mem-tag/bits+))
(deftype mem-fulltag () '(unsigned-byte #.+mem-fulltag/bits+))
(deftype mem-int     () '(integer #.+most-negative-int+ #.+most-positive-int+))


(defun get-abi ()
  '((:stmx-persist-magic . (#.(code-char 4) #\s #\t #\m #\x
                            #.(code-char 7) #\p #\e #\r #\s #\i #\s #\t
                            #.(code-char 2) #.(code-char 13) #.(code-char 10) #.(code-char 0)))
    (:file-version   . 1)
    (:bits-per-byte  . #.+mem-byte/bits+)
    (:bits-per-tag   . #.+mem-tag/bits+)
    (:bits-per-pointer . #.+mem-pointer/bits+)
    (:bits-per-word  . #.+mem-word/bits+)
    (:sizeof-byte    . #.+msizeof-byte+)
    (:sizeof-word    . #.+msizeof-word+)
    (:sizeof-single-float . #.+msizeof-sfloat+)
    (:sizeof-double-float . #.+msizeof-dfloat+)
    (:word-endianity . #.(let ((fmt (format nil "~A~D~A" "#x~" (* 2 +msizeof-word+) ",'0X")))
                           (format nil fmt +mem-word/endianity+)))))
    
  
  
(defmacro %to-value (value)
  `(logand +mem-size/mask+ (ash ,value ,(- +mem-size/shift+))))

(defmacro %to-fulltag (value)
  `(logand +mem-fulltag/mask+ (ash ,value ,(- +mem-fulltag/shift+))))

(defmacro %to-fulltag-and-value (val)
  (let ((value     (gensym "VALUE-")))
    `(let ((,value ,val))
       (values
        (%to-fulltag ,value)
        (%to-value ,value)))))
       

(declaim (inline mget-fulltag mget-value mget-fulltag-and-value))

(defun mget-fulltag (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-fulltag (mget-word ptr index)))

(defun mget-value (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-value (mget-word ptr index)))

(defun mget-fulltag-and-value (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-fulltag-and-value (mget-word ptr index)))



(declaim (inline mset-fulltag-and-value))
(defun mset-fulltag-and-value (ptr index fulltag value)
  (declare (type maddress ptr)
           (type mem-size index value)
           (type mem-fulltag fulltag))

  (setf (mget-word ptr index)
        (logior
         (ash fulltag +mem-fulltag/shift+)
         (ash value   +mem-size/shift+)))
  t)


(declaim (inline mset-int))
(defun mset-int (ptr index value)
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-int value))

  (setf (mget-word ptr index)
        (logand +mem-word/mask+
                (logior +mem-int/flag+ value)))
  t)


(defmacro %to-int (val)
  (let ((value     (gensym "VALUE-"))
        (int-value (gensym "INT-VALUE-"))
        (sign      (gensym "SIGN-")))
    `(let* ((,value ,val)
            (,int-value (logand +mem-int/value-mask+ ,value))
            (,sign (logand +mem-int/sign-mask+ ,value)))
       (the mem-int (- ,int-value ,sign)))))
  
(declaim (inline mget-int))
(defun mget-int (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))

  (%to-int (mget-word ptr index)))



(declaim (inline mset-character mget-character))

(defun mset-character (ptr index value)
  (declare (type maddress ptr)
           (type mem-size index)
           (type character value))

  (mset-fulltag-and-value ptr index +mem-tag-character+ (char-code value)))


(defun mget-character (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))

  (code-char (%to-value (mget-word ptr index))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro mget-float-0 (type ptr index)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mget-t ,type ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defmacro mget-float-N (type ptr index)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mget-t ,type ,ptr (logand +mem-word/mask+
                               (+ ,(- +msizeof-word+ (msizeof type))
                                  (logand +mem-word/mask+
                                          (* ,index +msizeof-word+))))))

(defmacro mset-float-0 (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mset-t ,value ,type ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defmacro mset-float-N (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mset-t ,value ,type ,ptr (logand +mem-word/mask+
                                       (+ ,(- +msizeof-word+ (msizeof type))
                                          (logand +mem-word/mask+
                                                  (* ,index +msizeof-word+))))))

(defmacro mget-float/inline (type ptr index)
  (declare (type (member :float :double :sfloat :dfloat) type))
  (if (mem-float/inline type)
      (if +mem/little-endian+
          `(mget-float-0 ,type ,ptr ,index)
          `(mget-float-N ,type ,ptr ,index))
      `(error "STMX-PERSIST: cannot use inline ~As on this architecture" ,(cffi-type-name type))))

(defmacro mset-float/inline (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  (if (mem-float/inline type)
      (if +mem/little-endian+
          `(mset-float-0 ,type ,ptr ,index ,value)
          `(mset-float-N ,type ,ptr ,index ,value))
      `(error "STMX-PERSIST: cannot use inline ~As on this architecture" ,(cffi-type-name type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mset-unboxed (ptr index value)
  "Write an unboxed value to memory store. Supported types are:
boolean, unbound slots, character and medium-size integer
\(on 64bit architectures can also write single-floats)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type (or boolean symbol character mem-int single-float double-float) value))

  (let ((tag +mem-tag-symbol+)
        (val +mem-nil+))

    (cond
      ;; value is an integer?
      ((typep value 'mem-int)
       (return-from mset-unboxed (mset-int ptr index value)))

      ;; value is a character?
      ((characterp value) (setf tag +mem-tag-character+
                                val (char-code value)))
      ;; value is T ?
      ((eq value t)       (setf val +mem-t+))

      ;; value is NIL ?
      ((eq value nil))

      ;; value is +unbound-tvar+ ?
      ((eq value stmx::+unbound-tvar+) (setf val +mem-unbound+))

      ;; value is a single-float?
      ((and +mem-sfloat/inline+ (typep value 'single-float))
       (mset-fulltag-and-value ptr index +mem-tag-sfloat+ 0)
       (mset-float/inline :sfloat ptr index value)
       (return-from mset-unboxed t))

      ;; value is a double-float?
      #-(and)
      ((and +mem-dfloat/inline+ (typep value 'double-float))
       (mset-fulltag-and-value ptr index +mem-tag-dfloat+ 0)
       (mset-float/inline :dfloat ptr index value)
       (return-from mset-unboxed t))

      ;; default case: value cannot be be stored as unboxed type, return NIL
      (t (return-from mset-unboxed nil)))

    (mset-fulltag-and-value ptr index tag val)))

     

(defun mget-unboxed (ptr index)
  "Read an unboxed value (boolean, unbound slot, character or
medium-size integer) or a pointer from memory store.
\(on 64bit architectures can also read single-floats)"
  (declare (type maddress ptr)
           (type mem-size index))

  (let ((value (mget-word ptr index)))

    (if (zerop (ash value (- +mem-int/bits+))) ;; found a mem-int?

        ;; not a mem-int
        (multiple-value-bind (fulltag value) (%to-fulltag-and-value value)

          (case fulltag
            (#.+mem-tag-symbol+ ;; found a symbol

             (case value
               (#.+mem-unallocated+ nil) ;; should not happen :(
               (#.+mem-unbound+ stmx::+unbound-tvar+) ;; unbound slot
               (#.+mem-t+       t)
               (#.+mem-nil+     nil)
               (otherwise       (values value fulltag)))) ;; generic symbol

            (#.+mem-tag-character+ ;; found a character
             (code-char (logand value +character/mask+)))

            (#.+mem-tag-sfloat+ ;; found a single-float
             (mget-float/inline :sfloat ptr index))

            (#.+mem-tag-dfloat+ ;; found a double-float
             (mget-float/inline :dfloat ptr index))

            (otherwise ;; found a boxed value or a pointer
             (values value fulltag))))

        ;; found a mem-int
        (%to-int value))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro mwrite-box-0 (ptr index boxed-type owner)
  "Write to memory the 0-th word of a boxed value"
  `(mset-fulltag-and-value ,ptr ,index ,boxed-type ,owner))

(defmacro mwrite-box-1 (ptr index tag-payload-specific allocated-words)
  "Write to memory the 1-st word of a boxed value"
  `(mset-fulltag-and-value ,ptr ,index ,tag-payload-specific ,allocated-words))

(defmacro mread-box-0 (ptr index)
  `(mget-fulltag-and-value ,ptr ,index))

(defmacro mread-box-1 (ptr index)
  `(mget-fulltag-and-value ,ptr ,index))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-bignum-word/bits+ (1- +mem-int/bits+))
(defconstant +mem-bignum-word/mask+ (1- (ash 1 +mem-bignum-word/bits+)))

(defconstant +mem-bignum/max-words+ (- +most-positive-size+ 3)
  "Maximum number of CPU words in a mem-bignum")

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


(defun mwrite-box/bignum (ptr index owner n)
  "Try to reuse the memory starting at (PTR+INDEX) and write bignum N into it.
TODO: if memory cannot be reused, free it and allocate a new block.

ABI: bignum is stored as box prefix, followed by (bignum-words n) and sign,
followed by an array of words."
  (declare (type maddress ptr)
           (type mem-pointer owner)
           (type mem-size index)
           (type integer n))

  (let* ((n-words (bignum-words n))
         ;; box prefix: 2 words. bignum-words prefix: 1 word
         (allocated-words (the mem-size (+ 3 n-words))))

    ;; TODO: if memory is not large enough, allocate a new block

    (mwrite-box-0 ptr index +mem-box-bignum+ owner)

    (mwrite-box-1 ptr (incf (the mem-size index))
                  0 allocated-words)
    
    (mset-fulltag-and-value ptr (incf (the mem-size index))
                            (if (< n 0) 1 0) n-words)

    (%mwrite-bignum-recurse ptr index n-words n)))
      

(defun mread-box/bignum (ptr index)
  "Read a bignum from the boxed memory starting at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index))
  
  ;; read sign and bignum-words from 2-nd word
  (multiple-value-bind (sign n-words)
      (mget-fulltag-and-value ptr (incf (the mem-size index) 2))

    (%mread-bignum-recurse ptr index n-words sign)))


(defun %mread-pos-bignum-loop (ptr index n-words)
  "Read unsigned bignum"
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
  "Read negative bignum"
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
  "Read a bignum from the boxed memory starting at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index))
  
  ;; read sign and bignum-words from 2-nd word
  (multiple-value-bind (sign n-words)
      (mget-fulltag-and-value ptr (incf (the mem-size index) 2))

    (%mread-bignum-recurse ptr index n-words sign)))



  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
