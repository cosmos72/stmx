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

(deftype ufixnum () '(and fixnum (integer 0)))
(deftype mpointer () 'cffi-sys:foreign-pointer)


(eval-when (:compile-toplevel :load-toplevel)
  (defun parse-type (type)
    (case type
      (:uchar  :unsigned-char)
      (:ushort :unsigned-short)
      (:uint   :unsigned-int)
      (:ulong  :unsigned-long)
      (:ullong :unsigned-long-long)
      (:word   :unsigned-long) ;; this is the ONLY code mapping :word to a CFFI type
      (otherwise type))))


(defmacro %msizeof (type)
  `(cffi-sys:%foreign-type-size ,(if (keywordp type)
                                     (parse-type type) 
                                     `(parse-type ,type))))

(defmacro msizeof (type)
  (if (keywordp type)
      (%msizeof type)
      `(%msizeof ,type)))




(defconstant +msizeof-char+    (msizeof :char))
(defconstant +msizeof-short+   (msizeof :short))
(defconstant +msizeof-int+     (msizeof :int))
(defconstant +msizeof-long+    (msizeof :long))
(defconstant +msizeof-float+   (msizeof :float))
(defconstant +msizeof-double+  (msizeof :double))
(defconstant +msizeof-pointer+ (msizeof :pointer))

(defconstant +msizeof-uchar+   (msizeof :uchar))
(defconstant +msizeof-ushort+  (msizeof :ushort))
(defconstant +msizeof-uint+    (msizeof :uint))
(defconstant +msizeof-ulong+   (msizeof :ulong))
(defconstant +msizeof-ullong+  (msizeof :ullong))
(defconstant +msizeof-word+    (msizeof :word))

(defmacro malloc (size)
  `(cffi-sys:%foreign-alloc ,size))

(defmacro mfree (ptr)
  `(cffi-sys:foreign-free ,ptr))


(defmacro mget-t (type ptr &optional (offset 0))
  `(cffi-sys:%mem-ref ,ptr ,(parse-type type) ,offset))

(defmacro mset-t (value type ptr &optional (offset 0))
  `(cffi-sys:%mem-set ,value ,ptr ,(parse-type type) ,offset))

(defsetf mget-t (type ptr &optional (offset 0)) (value)
  `(mset-t ,value ,type ,ptr ,offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition unsupported-arch (simple-error)
    ()))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (let ((most-positive-word 1)
        (bits-per-word 1))
    
    ;; we need at least a 32-bit architecture
    (when (< +msizeof-word+ 4)
      (error "cannot build STMX-PERSIST: unsupported architecture.
    size of CPU word is ~S bytes, expecting at least 4 bytes" +msizeof-word+))

    ;; determine number of bits per CPU word
    (cffi-sys:with-foreign-pointer (p +msizeof-word+)
      (loop for i = 1 then (logior (ash i 1) 1)
         for bits = 1 then (1+ bits)
         do
           (handler-case
               (progn
                 (setf (mget-t :word p) i)
                 (let ((j (mget-t :word p)))
                   (unless (eql i j)
                     (error "reading value '~S' stored in a CPU word returns '~S'" i j)))

                 (setf most-positive-word i
                       bits-per-word bits))

             (condition ()
               (defconstant +most-positive-word+ most-positive-word)
               (defconstant +mem-word/mask+  most-positive-word)
               (defconstant +mem-word/bits+  bits-per-word)
               (return)))))))
               
;;  8 bits reserved for tags on 32-bit architectures
;; 16 bits reserved for tags on 64-bit architectures
;; ...
(defconstant +mem-fulltag/bits+ (truncate +mem-word/bits+ 4))
(defconstant +mem-pointer/bits+ (- +mem-word/bits+ +mem-fulltag/bits+))

;; we need at least a 32-bit architecture
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (< +mem-word/bits+ 32)
    (error "cannot build STMX-PERSIST: unsupported architecture.
    size of CPU word is ~S bits, expecting at least 32 bits" +mem-word/bits+))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mget-word (ptr &optional (index 0))
  `(mget-t :word ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defmacro mset-word (value ptr &optional (index 0))
  `(mset-t ,value :word ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defsetf mget-word (ptr &optional (index 0)) (value)
  `(mset-word ,value ,ptr ,index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !mdump (stream ptr &optional (offset-start 0) (offset-end (1+ offset-start)))
  "mdump is only used for debugging. it assumes sizeof(char) == 1"
  (declare (type mpointer ptr)
           (type fixnum offset-start offset-end))
  (loop for offset from offset-start below offset-end do
       (format stream "~2,'0X " (mget-t :uchar ptr offset))))


(defun !mdump-reverse (stream ptr &optional (offset-start 0) (offset-end (1+ offset-start)))
  "mdump-reverse is only used for debugging. it assumes sizeof(char) == 1"
  (declare (type mpointer ptr)
           (type fixnum offset-start offset-end))
  (loop for offset from offset-end above offset-start do
       (format stream "~2,'0X " (mget-t :uchar ptr (1- offset)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !mfill (ptr size &key (value 0) (increment 0))
  "mfill is only used for debugging. it assumes sizeof(char) == 1 and 8 bits in a char"
  (declare (type mpointer ptr)
           (type ufixnum size)
           (type (unsigned-byte 8) value increment))
  (loop for offset from 0 below size do
       (setf (mget-t :uchar ptr offset) value)
       (setf value (logand #xFF (+ value increment)))))


(cffi:defcfun ("memset" %memset) :void (ptr :pointer) (fill-char :int) (size :unsigned-long))

(declaim (inline memset mzero))

(defun memset (ptr fill-byte size)
  (declare (type mpointer ptr)
           (type ufixnum size)
           (type (unsigned-byte 8) fill-byte))
  (%memset ptr fill-byte size))

(defun mzero (ptr size)
  (declare (type mpointer ptr)
           (type ufixnum size))
  (memset ptr 0 size))
           


