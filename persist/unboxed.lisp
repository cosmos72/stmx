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

(enable-#?-syntax)

(eval-always
  (when (< +most-positive-pointer+ +most-positive-character+)
    
    (error "cannot compile STMX-PERSIST: assuming ~S-bit characters (i.e. Unicode),
    cannot fit them in the ~S bits reserved by ABI." +character/bits+ +mem-pointer/bits+)))



(deftype mem-word    () '(unsigned-byte #.+mem-word/bits+))
(deftype mem-int     () '(  signed-byte #.+mem-int/bits+))
(deftype mem-uint    () '(unsigned-byte #.+mem-int/value-bits+))

(deftype mem-fulltag () '(unsigned-byte #.+mem-fulltag/bits+))
(deftype mem-tag     () '(unsigned-byte #.+mem-tag/bits+))

;; pointer offsets stored in MMAP area. To better use the available bits,
;; they are in units of the type pointed to, i.e. increasing them by 1
;; makes them point to the next object of the same type
(deftype mem-pointer () '(unsigned-byte #.+mem-pointer/bits+))

;; pointer offsets used internally by STMX-PERSIST. They are in units of a CPU word,
;; so to convert from mem-pointer to mem-size you must multiply by the number of words
;; required to store the object pointed to.
(deftype mem-size    () '(unsigned-byte #.(- +mem-word/bits+ (integer-length (1- +msizeof-word+)))))


(deftype mem-unboxed () 
  "Union of all types that can be stored as unboxed in memory store"
  '(or mem-int character boolean symbol
    #?+sp/sfloat/inline single-float
    #?+sp/dfloat/inline double-float))


(defun !mdump-words (stream ptr &optional (start-index 0) (end-index (1+ start-index)))
  "mdump-words is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type mem-size start-index end-index))
  (loop
     for start-byte = (* +msizeof-word+ start-index) then end-byte
     for end-byte   = (+ +msizeof-word+ start-byte)
     while (<= end-byte (* +msizeof-word+ end-index))
     do
       (#.(if +mem/little-endian+
              '!mdump-reverse
              '!mdump-forward)
          stream ptr start-byte end-byte)
       (format stream " ")))


(declaim (inline mem-size+ mem-size+1 mem-size+2 mem-size- mem-size-1))

(defun mem-size+ (a &optional (b 0) (c 0))
  (declare (type mem-size a b c))
  (the mem-size (+ a b c)))

(defun mem-size+1 (a)
  (mem-size+ a 1))

(defun mem-size+2 (a)
  (mem-size+ a 2))

(defun mem-size- (a b)
  (declare (type mem-size a b))
  (the mem-size (- a b)))

(defun mem-size-1 (a)
  (mem-size- a 1))



(defmacro incf-mem-size (place &optional (delta 1))
  `(incf (the mem-size ,place) ,delta))


    
  

(defmacro %to-fulltag (value)
  `(logand +mem-fulltag/mask+ (ash ,value ,(- +mem-fulltag/shift+))))

(defmacro %to-value (value)
  `(logand +mem-pointer/mask+ (ash ,value ,(- +mem-pointer/shift+))))

(defmacro %to-fulltag-and-value (val)
  (let ((value     (gensym "VALUE-")))
    `(let ((,value ,val))
       (values
        (%to-fulltag ,value)
        (%to-value ,value)))))
       

(declaim (inline mem-invalid-index? mget-fulltag mget-value mget-fulltag-and-value))

(defun mem-invalid-index? (ptr index)
  (declare (ignore ptr)
           (type mem-size index))
  (zerop index))

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
           (type mem-size index)
           (type mem-fulltag fulltag)
           (type mem-pointer value))

  (setf (mget-word ptr index)
        (logior
         (ash fulltag +mem-fulltag/shift+)
         (ash value   +mem-pointer/shift+)))
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


(defmacro %to-int/sign (value)
  `(logand +mem-int/sign-mask+ ,value))

(defmacro %to-int/value (value)
  `(logand +mem-int/value-mask+ ,value))

(defmacro %to-int (value)
  (with-gensyms (word int-value int-sign)
    `(let* ((,word ,value)
            (,int-value (%to-int/value ,word))
            (,int-sign  (%to-int/sign  ,word)))
       (the mem-int (- ,int-value ,int-sign)))))
  

(declaim (inline mget-int/value mget-int))
(defun mget-int (ptr index)
  "Return the mem-int stored at (PTR+INDEX)"
  (declare (type maddress ptr)
           (type mem-size index))

  (%to-int (mget-word ptr index)))


(defun mget-int/value (ptr index)
  "Return the two's complement value of mem-int stored at (PTR+INDEX),
ignoring any sign bit"
  (declare (type maddress ptr)
           (type mem-size index))

  (the mem-uint (%to-int/value (mget-word ptr index))))





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
  (if (mem-float/inline? type)
      (if +mem/little-endian+
          `(mget-float-0 ,type ,ptr ,index)
          `(mget-float-N ,type ,ptr ,index))
      `(error "STMX-PERSIST: cannot use inline ~As on this architecture" ,(cffi-type-name type))))

(defmacro mset-float/inline (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  (if (mem-float/inline? type)
      (if +mem/little-endian+
          `(mset-float-0 ,type ,ptr ,index ,value)
          `(mset-float-N ,type ,ptr ,index ,value))
      `(error "STMX-PERSIST: cannot use inline ~As on this architecture" ,(cffi-type-name type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mvalue-is-unboxed? (value)
  "Return T if VALUE can be stored as an unboxed value to memory store."
  
  (if (typep value 'mem-unboxed)
      t nil))


(defun mset-unboxed (ptr index value)
  "Write an unboxed value to memory store. Supported types are:
boolean, unbound slots, character and medium-size integer
\(on 64bit architectures can also write single-floats)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-unboxed value))

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
      #?+sp/sfloat/inline
      ((typep value 'single-float)
       (mset-fulltag-and-value ptr index +mem-tag-sfloat+ 0)
       (mset-float/inline :sfloat ptr index value)
       (return-from mset-unboxed t))

      ;; value is a double-float?
      #?+sp/dfloat/inline
      ((typep value 'double-float)
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

            #?+sp/sfloat/inline
            (#.+mem-tag-sfloat+ ;; found a single-float
             (mget-float/inline :sfloat ptr index))

            #?+sp/dfloat/inline
            (#.+mem-tag-dfloat+ ;; found a double-float
             (mget-float/inline :dfloat ptr index))

            (otherwise ;; found a boxed value or a pointer
             (values value fulltag))))

        ;; found a mem-int
        (%to-int value))))



  
