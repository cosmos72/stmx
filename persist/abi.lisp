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



(defconstant +mem-pointer/shift+ 0)
(defconstant +mem-pointer/mask+  (1- (ash 1 +mem-pointer/bits+)))
(defconstant +most-positive-pointer+ +mem-pointer/mask+)

(defconstant +mem-fulltag/shift+ +mem-pointer/bits+)
(defconstant +mem-fulltag/mask+  (1- (ash 1 +mem-fulltag/bits+)))
(defconstant +most-positive-fulltag+ +mem-fulltag/mask+)

;; reserve most significant bit to mark integers
(defconstant +mem-tag/bits+      (1- +mem-fulltag/bits+))
(defconstant +mem-tag/mask+      (1- (ash 1 +mem-tag/bits+)))
;; fulltags larger than +most-positive-tag+ indicate an integer (actually, mem-int) value
(defconstant +most-positive-tag+ +mem-tag/mask+)


(defconstant +mem-int/bits+      (1-  +mem-word/bits+))
(defconstant +mem-int/mask+      (ash +mem-word/mask+ -1))
;; most significant bit in a word. if set, means the other bits are a signed integer
(defconstant +mem-int/flag+      (1+  +mem-int/mask+))
;; value bits in a signed integer
(defconstant +mem-int/value-mask+ (ash +mem-int/mask+ -1))
;; sign bit in a signed integer
(defconstant +mem-int/sign-mask+  (1+ +mem-int/value-mask+))

(defconstant +most-positive-int+ (ash +mem-int/mask+ -1))
(defconstant +most-negative-int+ (- -1 +most-positive-int+))


(deftype mem-word    () '(unsigned-byte #.+mem-word/bits+))
(deftype mem-pointer () '(unsigned-byte #.+mem-pointer/bits+))
(deftype mem-fulltag () '(unsigned-byte #.+mem-fulltag/bits+))
(deftype mem-tag     () '(unsigned-byte #.+mem-tag/bits+))
(deftype mem-int     () '(integer #.+most-negative-int+ #.+most-positive-int+))


(defmacro %to-value-and-fulltag (val)
  (let ((value     (gensym "VALUE-")))
    `(let ((,value ,val))
       (values
        (logand +mem-pointer/mask+ (ash ,value (- +mem-pointer/shift+)))
        (logand +mem-fulltag/mask+ (ash ,value (- +mem-fulltag/shift+)))))))
       
  
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



(defconstant +mem-unallocated+ 0)
(defconstant +mem-nil+ 1)
(defconstant +mem-t+   2)

(defconstant +mem-fulltag-keyword+  0)
(defconstant +mem-fulltag-character+ 1)
(defconstant +mem-fulltag-last+      +mem-tag/bits+)

(defconstant +mem-character/mask+ #x1FFFFF) ;; assume Unicode

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (< +most-positive-pointer+ #x100FFF) ;; assume Unicode
    
    (error "cannot compile STMX-PERSIST: characters are at least ~S bits,
    cannot fit them in the ~S bits reserved by ABI." 21 +mem-pointer/bits+)))

(defun mset-character (ptr index value)
  (declare (type mpointer ptr)
           (type fixnum index)
           (type character value))

  (mset-value-and-fulltag ptr index (char-code value) +mem-fulltag-character+))


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
             (code-char (logand value +mem-character/mask+)))

            (otherwise ;; found a pointer
             (values value fulltag))))

        ;; found a mem-int
        (%to-int value))))

