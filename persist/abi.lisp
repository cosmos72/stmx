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
(deftype mem-fulltag () '(unsigned-byte #.+mem-fulltag/bits+))
(deftype mem-tag     () '(unsigned-byte #.+mem-tag/bits+))
(deftype mem-int     () '(integer #.+most-negative-int+ #.+most-positive-int+))


(defun get-abi ()
  '((:file-version  . 1)
    (:bits-per-byte . #.+mem-byte/bits+)
    (:bits-per-tag  . #.+mem-tag/bits+)
    (:bits-per-pointer . #.+mem-pointer/bits+)
    (:bits-per-word . #.+mem-word/bits+)
    (:sizeof-word   . #.+msizeof-word+)
    (:word-endianity . #.(let ((fmt (format nil "~A~D~A" "#x~" (* 2 +msizeof-word+) ",'0X")))
                           (format nil fmt +mem-word/endianity+)))))
    
  
  
(defmacro %to-value (value)
  `(logand +mem-pointer/mask+ (ash ,value ,(- +mem-pointer/shift+))))

(defmacro %to-fulltag (value)
  `(logand +mem-fulltag/mask+ (ash ,value ,(- +mem-fulltag/shift+))))

(defmacro %to-value-and-fulltag (val)
  (let ((value     (gensym "VALUE-")))
    `(let ((,value ,val))
       (values
        (%to-value ,value)
        (%to-fulltag ,value)))))
       
  
(defun mget-value-and-fulltag (ptr index)
  (declare (type mpointer ptr)
           (type fixnum index))
  (%to-value-and-fulltag (mget-word ptr index)))



       
(defun mset-value-and-fulltag (ptr index value fulltag)
  (declare (type mpointer ptr)
           (type fixnum index)
           (type mem-pointer value)
           (type mem-fulltag fulltag))

  (setf (mget-word ptr index)
        (logior
         (ash value   +mem-pointer/shift+)
         (ash fulltag +mem-fulltag/shift+)))
  nil)



(defun mset-int (ptr index value)
  (declare (type mpointer ptr)
           (type fixnum index)
           (type mem-int value))

  (setf (mget-word ptr index)
        (logand +mem-word/mask+
                (logior +mem-int/flag+ value)))
  nil)


(defmacro %to-int (val)
  (let ((value     (gensym "VALUE-"))
        (int-value (gensym "INT-VALUE-"))
        (sign      (gensym "SIGN-")))
    `(let* ((,value ,val)
            (,int-value (logand +mem-int/value-mask+ ,value))
            (,sign (logand +mem-int/sign-mask+ ,value)))
       (the mem-int (- ,int-value ,sign)))))
  
(defun mget-int (ptr index)
  (declare (type mpointer ptr)
           (type fixnum index))

  (%to-int (mget-word ptr index)))



(declaim (inline mset-character mget-character))

(defun mset-character (ptr index value)
  (declare (type mpointer ptr)
           (type fixnum index)
           (type character value))

  (mset-value-and-fulltag ptr index (char-code value) +mem-fulltag-character+))


(defun mget-character (ptr index)
  (declare (type mpointer ptr)
           (type fixnum index))

  (code-char (%to-value (mget-word ptr index))))


(defun mset-primitive (ptr index value &optional fulltag)
  "Copy a primitive value (boolean, character, medium-size integer or pointer) to memory store."
  (declare (type mpointer ptr)
           (type fixnum index)
           (type (or boolean character mem-int mem-pointer) value)
           (type (or null mem-fulltag) fulltag))

  (let ((tag +mem-fulltag-keyword+)
        (val +mem-nil+))

    (cond
      ;; value is tagged pointer?
      (fulltag (setf tag fulltag
                     val (the mem-pointer value)))

      ;; value is integer?
      ((typep value 'mem-int) (return-from mset-primitive (mset-int ptr index value)))

      ;; value is character?
      ((characterp value) (setf tag +mem-fulltag-character+
                                val (char-code value)))

      ;; assume value is boolean
      ((eq t value)       (setf val +mem-t+))

      ;; default case
      (t))

    (mset-value-and-fulltag ptr index val tag)))

     

(defun mget-primitive (ptr index)
  "Get a primitive value (boolean, character, medium-size integer or pointer) from memory store."
  (declare (type mpointer ptr)
           (type fixnum index))

  (let ((value (mget-word ptr index)))

    (if (zerop (ash value (- +mem-int/bits+))) ;; found a mem-int?

        ;; not a mem-int
        (multiple-value-bind (value fulltag) (%to-value-and-fulltag value)

          (case fulltag
            (#.+mem-fulltag-keyword+ ;; found a boolean or a keyword

             (case value
               (#.+mem-t+   t)
               (#.+mem-nil+ nil)
               (otherwise (values value fulltag)))) ;; found a keyword

            (#.+mem-fulltag-character+ ;; found a character
             (code-char (logand value +character/mask+)))

            (otherwise ;; found a pointer
             (values value fulltag))))

        ;; found a mem-int
        (%to-int value))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bignum-words (n)
  "Return the number of words needed to store bignum N in memory."
  (declare (type integer n))

  (let* ((len (integer-length n))
         (words (truncate (+ len +mem-word/bits+ -1) ;; round up
                          +mem-word/bits+)))
    (unless (typep words 'mem-int)
      (error "bignum too large for object store: it requires ~S words, maximum supported is ~S words"
             words +most-positive-int+))
    (the mem-int words)))


(defun mwrite-bignum (ptr index n &optional (n-words (bignum-words n)))
  "Write bignum N into the memory starting at (PTR+INDEX)."
  (declare (type mpointer ptr)
           (type fixnum index)
           (type integer n)
           (type mem-int n-words))

  (when (< n 0)
    ;; FIXME: write N, not (LOGNOT N)
    (setf n (lognot n))
    (setf n-words (lognot n-words)))

  (mset-int ptr index n-words)

  (let ((bits +mem-word/bits+)
        (mask +mem-word/mask+)
        (i n))
    
    (loop do
         (mset-word ptr (incf (the mem-int index)) (logand i mask))
         (setf i (ash i (- bits)))
         (when (or (= 0 i) (= -1 i))
           (return)))))



(defun mread-bignum (ptr index)
  "Read a bignum from the memory starting at (PTR+INDEX)."
  (declare (type mpointer ptr)
           (type fixnum index))

  (let* ((bits +mem-word/bits+)
         (n-words (mget-int ptr index))
         (negative (< n-words 0))
         (n 0))

    (when negative
      (setf n-words (lognot n-words)))

    (loop for shift from 0 below (the fixnum (* bits n-words)) by bits
       for i = (mget-word ptr (incf (the fixnum index))) do
         (setf n (logior n (ash i shift))))

    (if negative
        ;; FIXME: read N, not (LOGNOT N)
        (lognot n)
        n)))



  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mwrite-string (ptr index string &optional (start 0) (end (length string)))
  "Write (END-START) characters from STRING to the memory starting at (PTR+INDEX).
Characters will be stored using the general-purpose representation."
  (declare (type mpointer ptr)
           (type fixnum index)
           (type simple-array string)
           (type ufixnum start end))

  (loop for i from start below end do
       (mset-character ptr (the fixnum (+ index i))
                       (svref string i))))


(defun mread-string (ptr index result-string &optional (start 0) (end (length result-string)))
  "Read (END-START) characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING. Return RESULT-STRING.
Characters will be read using the general-purpose representation."
  (declare (type mpointer ptr)
           (type fixnum index)
           (type simple-array result-string)
           (type ufixnum start end))

  (loop for i from start below end do
       (setf (svref result-string i)
             (mget-character ptr (the fixnum (+ index i)))))
  result-string)


(defun mwrite-base-string (ptr index string &optional (start 0) (end (length string)))
  "Write (END-START) single-byte characters from STRING into the memory starting at (PTR+INDEX).
Characters are written using the compact, single-byte representation.
For this reason the codes of all characters to be stored must be in the range
0 ... +most-positive-byte+ (typically 0 ... 255)"
  (declare (type mpointer ptr)
           (type fixnum index)
           (type simple-array string)
           (type ufixnum start end))

  (loop for i from start below end do
       (%mset-t (the (unsigned-byte #.+mem-byte/bits+)
                  (char-code
                   (svref string i))) :uchar
                ptr (the fixnum (+ index i)))))



(defun mread-base-string (ptr index result-string &optional (start 0) (end (length result-string)))
  "Read (END-START) single-byte characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING.  Return RESULT-STRING.
Characters are read from memory using the compact, single-byte representation.
For this reason only codes in the range 0 ... +most-positive-byte+ can be read
\(typically 0 ... 255)"
  (declare (type mpointer ptr)
           (type fixnum index)
           (type simple-array result-string)
           (type ufixnum start end))

  (loop for i from start below end do
       (setf (svref result-string i)
             (code-char
              (the (unsigned-byte #.+mem-byte/bits+)
                (%mget-t :uchar ptr (the fixnum (+ index i)))))))
  result-string)
