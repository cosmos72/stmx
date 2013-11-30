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
(deftype maddress () 'cffi-sys:foreign-pointer)

(defconstant +null-pointer+ (if (boundp '+null-pointer+)
                                (symbol-value '+null-pointer+)
                                (cffi-sys:null-pointer)))

(defconstant +bad-fd+ -1)

(eval-when (:compile-toplevel :load-toplevel)

  #-(and)
  (pushnew :stmx-persist/debug *features*)

  (defun parse-type (type)
    (case type
      (:uchar  :unsigned-char)
      (:ushort :unsigned-short)
      (:uint   :unsigned-int)
      (:ulong  :unsigned-long)
      (:ullong :unsigned-long-long)
      (:sfloat :float)         ;; this is the ONLY code mapping :sfloat to a CFFI type
      (:dfloat :double)        ;; this is the ONLY code mapping :dfloat to a CFFI type
      (:byte   :unsigned-char) ;; this is the ONLY code mapping :byte to a CFFI type
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

(defconstant +msizeof-sfloat+   (msizeof :sfloat))
(defconstant +msizeof-dfloat+   (msizeof :dfloat))
(defconstant +msizeof-byte+    (msizeof :byte))
(defconstant +msizeof-word+    (msizeof :word))


(defmacro %mget-t (type ptr &optional (offset 0))
  `(cffi-sys:%mem-ref ,ptr ,(parse-type type) ,offset))

(defmacro %mset-t (value type ptr &optional (offset 0))
  `(cffi-sys:%mem-set ,value ,ptr ,(parse-type type) ,offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition unsupported-arch (simple-error)
    ()))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun cffi-type-name (sym)
    (declare (type symbol sym))
    (string-downcase (symbol-name (parse-type sym))))

  ;; we need at least a 32-bit architecture
  (when (/= +msizeof-byte+ 1)
    (error "cannot build STMX-PERSIST: unsupported architecture.
    size of ~S is ~S bytes, expecting exactly 1 byte"
           (cffi-type-name :byte) +msizeof-byte+))


  (when (< +msizeof-word+ 4)
    (error "cannot build STMX-PERSIST: unsupported architecture.
    size of ~S is ~S bytes, expecting at least 4 bytes"
           (cffi-type-name :byte) +msizeof-word+))

  ;; determine number of bits per CPU word
  (defun %detect-bits-per-word ()
    (declare (optimize (speed 0) (safety 3))) ;; ABSOLUTELY NECESSARY!

    (let ((bits-per-word 1))
    
      (cffi-sys:with-foreign-pointer (p +msizeof-word+)
        (loop for i = 1 then (logior (ash i 1) 1)
           for bits = 1 then (1+ bits)
           do
             (handler-case
                 (progn
                   (%mset-t i :word p)
                   
                   #+stmx-persist/debug
                   (log:debug "(i #x~X) (bits ~D) ..." i bits)
                 
                   (let ((j (%mget-t :word p)))
                     #+stmx-persist/debug
                     (log:debug " read back: #x~X ..." j)
                     
                     (unless (eql i j)
                       (error "reading value '~S' stored in a CPU word returned '~S'" i j))
                   
                     #+stmx-persist/debug
                     (log:debug "ok"))

                   (setf bits-per-word bits))

               (condition ()
                 (return-from %detect-bits-per-word bits-per-word))))))))



(defconstant +mem-word/bits+ (%detect-bits-per-word))
(defconstant +mem-word/mask+  (1- (ash 1 +mem-word/bits+)))
(defconstant +most-positive-word+ +mem-word/mask+)

(defconstant +mem-byte/bits+  (truncate +mem-word/bits+ +msizeof-word+))
(defconstant +mem-byte/mask+     (1- (ash 1 +mem-byte/bits+)))
(defconstant +most-positive-byte+ +mem-byte/mask+)



;; we need at least 7 bits per byte (to store ASCII characters in a single byte)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (< +mem-byte/bits+ 7)
    (error "cannot build STMX-PERSIST: unsupported architecture.
    each byte contains only ~S bits, expecting at least 8 bits" +mem-byte/bits+))) 



;; we need at least a 32-bit architecture
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (< +mem-word/bits+ 32)
    (error "cannot build STMX-PERSIST: unsupported architecture.
    size of CPU word is ~S bits, expecting at least 32 bits" +mem-word/bits+))) 



(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %detect-endianity ()
    (cffi-sys:with-foreign-pointer (p +msizeof-word+)
      (let ((little-endian 0)
            (big-endian 0))

        (loop for i from 0 below +msizeof-word+
             for bits = (logand (1+ i) +mem-byte/mask+) do

             (setf little-endian (logior little-endian (ash bits (* i +mem-byte/bits+)))
                   big-endian    (logior bits (ash big-endian +mem-byte/bits+)))

             (%mset-t bits :byte p i))

        (let ((endianity (%mget-t :word p)))
          (unless (or (eql endianity little-endian)
                      (eql endianity big-endian))
            (error "cannot build STMX-PERSIST: unsupported architecture.
    CPU word endianity is #x~X, expecting either #x~X (little-endian) or #x~X (big-endian)"
                   endianity little-endian big-endian))

          (defconstant +mem/little-endian+ (eql little-endian endianity))
          
          endianity)))))



(defconstant +mem-word/endianity+ (%detect-endianity))
               






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mget-word (ptr index)
  `(%mget-t :word ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defmacro mset-word (ptr index value)
  `(%mset-t ,value :word ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defsetf mget-word mset-word)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun !mdump (stream ptr &optional (start-byte 0) (end-byte (1+ start-byte)))
  "mdump is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type fixnum start-byte end-byte))
  (loop for offset from start-byte below end-byte do
       (format stream "~2,'0X " (%mget-t :byte ptr offset))))


(defun !mdump-reverse (stream ptr &optional (start-byte 0) (end-byte (1+ start-byte)))
  "mdump-reverse is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type fixnum start-byte end-byte))
  (loop for offset from end-byte above start-byte do
       (format stream "~2,'0X" (%mget-t :byte ptr (1- offset)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !mfill (ptr n-bytes &key (value 0) (increment 0))
  "mfill is only used for debugging. it assumes sizeof(byte) == 1 and 8 bits in a byte"
  (declare (type maddress ptr)
           (type ufixnum n-bytes)
           (type (unsigned-byte 8) value increment))
  (loop for offset from 0 below n-bytes do
       (%mset-t value :byte ptr offset)
       (setf value (logand #xFF (+ value increment)))))


(declaim (inline null-pointer? memset mzero memcpy))

(defun null-pointer? (ptr)
  (declare (type maddress ptr))
  (cffi-sys:null-pointer-p ptr))
           

(defun memset (ptr fill-byte n-bytes)
  (declare (type maddress ptr)
           (type (unsigned-byte 8) fill-byte)
           (type ufixnum n-bytes))
  (osicat-posix:memset ptr fill-byte n-bytes))

(defun mzero (ptr n-bytes)
  (declare (type maddress ptr)
           (type ufixnum n-bytes))
  (memset ptr 0 n-bytes))
           
(defun memcpy (dst src n-bytes)
  (declare (type maddress dst src)
           (type ufixnum n-bytes))
  (osicat-posix:memcpy dst src n-bytes))
  
           

(declaim (notinline malloc mfree))

(defun malloc (n-bytes)
  (cffi-sys:%foreign-alloc n-bytes))

(defun mfree (ptr)
  (cffi-sys:foreign-free ptr))

(defun open-fd (filename &optional (truncate-len -1))
  (declare (type integer truncate-len))

  (let ((fd (osicat-posix:open filename (logior osicat-posix:o-rdwr osicat-posix:o-creat))))
    (declare (type integer fd))
    (unless (eql truncate-len -1)
      (osicat-posix:ftruncate fd truncate-len))
    fd))

(defun close-fd (fd)
  (declare (type (integer 0) fd))
  (osicat-posix:close fd))

(defun mmap (fd len)
  (declare (type (integer 0) fd len))
  (osicat-posix:mmap +null-pointer+ len
                     (logior osicat-posix:prot-read osicat-posix:prot-write)
                     osicat-posix:map-shared
                     fd 0))

(defun munmap (ptr len)
  (osicat-posix:munmap ptr len))
  
