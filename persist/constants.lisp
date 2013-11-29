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


;; ABI definition. The values are tuned for good balance
;; between performance and size of addressable store


;;  8 bits reserved for tags on 32-bit architectures
;; 16 bits reserved for tags on 64-bit architectures
;; ...
;; the rest of each CPU word is used for pointer index or character value
(defconstant +mem-fulltag/bits+ (truncate +mem-word/bits+ 4))
(defconstant +mem-pointer/bits+ (- +mem-word/bits+ +mem-fulltag/bits+))

;; integers (actually, mem-int) are one bit less than CPU words
(defconstant +mem-int/bits+      (1-  +mem-word/bits+))

;; assume characters are Unicode without any encoding, i.e. 21 bits
(defconstant +character/bits+ 21) 
(defconstant +character/mask+ (1- (ash 1 +character/bits+)))
(defconstant +most-positive-character+ #x100FFF)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NO USER-SERVICEABLE PARTS BELOW THIS LINE.                                 ;;
;; I.e. everything else is computed from the CPU architecture                 ;;
;; and the constants above                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-fulltag/shift+ +mem-pointer/bits+)
(defconstant +mem-fulltag/mask+  (1- (ash 1 +mem-fulltag/bits+)))
(defconstant +most-positive-fulltag+ +mem-fulltag/mask+)

;; reserve most significant fulltag bit to mark integers
;; fulltags larger than +most-positive-tag+ indicate an integer (actually, mem-int) value
(defconstant +mem-tag/bits+      (1- +mem-fulltag/bits+))
(defconstant +mem-tag/mask+      (1- (ash 1 +mem-tag/bits+)))
(defconstant +most-positive-tag+ +mem-tag/mask+)


(defconstant +mem-pointer/shift+ 0)
(defconstant +mem-pointer/mask+  (1- (ash 1 +mem-pointer/bits+)))
(defconstant +most-positive-pointer+ +mem-pointer/mask+)

;; pointers and memory sizes must have the same bits
(defconstant +mem-size/bits+     +mem-pointer/bits+)
(defconstant +mem-size/shift+    +mem-pointer/shift+)
(defconstant +mem-size/mask+     +mem-pointer/mask+)
(defconstant +most-positive-size+ +most-positive-pointer+)




(defconstant +mem-int/mask+      (1- (ash 1 +mem-int/bits+)))
;; bits in a word that exceed a mem-int. if all set to one,
;; it means the rest of the word must be interpreted as a mem-int
(defconstant +mem-int/flag+      (- +mem-word/mask+ +mem-int/mask+))

;; value bits in a mem-int
(defconstant +mem-int/value-mask+ (ash +mem-int/mask+ -1))
;; sign bit in a mem-int
(defconstant +mem-int/sign-mask+  (1+ +mem-int/value-mask+))

(defconstant +most-positive-int+ (ash +mem-int/mask+ -1))
(defconstant +most-negative-int+ (lognot +most-positive-int+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant +mem-sfloat/bits+ (* +msizeof-sfloat+ +mem-byte/bits+))
(defconstant +mem-dfloat/bits+ (* +msizeof-dfloat+ +mem-byte/bits+))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %mem-float/inline (type)
    (declare (type (member :float :double :sfloat :dfloat type)))
    (let ((size (msizeof type)))
      (<= (* size +mem-byte/bits+) +mem-pointer/bits+))))

(defmacro mem-float/inline (type)
  (if (keywordp type)
      (%mem-float/inline type)
      `(%mem-float/inline ,type)))

(defconstant +mem-sfloat/inline+ (mem-float/inline :sfloat))
(defconstant +mem-dfloat/inline+ (mem-float/inline :dfloat))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hard-coded constants. DO NOT CHANGE!                                       ;;
;; They are read from and written to the store, so they are part of the ABI   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-unallocated+  0)
(defconstant +mem-unbound+      1 "persistent representation of unbound slot")
(defconstant +mem-nil+          2 "persistent representation of T")
(defconstant +mem-t+            3 "persistent representation of NIL")

(defconstant +mem-tag-symbol+    0)
(defconstant +mem-tag-character+ 1 "tag for inline characters")
(defconstant +mem-tag-sfloat+    2 "tag for inline single-floats")
(defconstant +mem-tag-dfloat+    3 "tag for inline double-floats")
(defconstant +mem-tag-box+       4 "tag for boxed values that do NOT contain pointers")
(defconstant +mem-tag-box-gc+    5 "tag for boxed values that may contain pointers")
(defconstant +mem-tag-last+      +mem-tag/bits+)


(defconstant +mem-box-free+      0 "box is not allocated")
(defconstant +mem-box-bignum+    1 "box is a bignum")
(defconstant +mem-box-ratio+     2 "box is a ratio")
(defconstant +mem-box-sfloat+    3 "box is a single-float")
(defconstant +mem-box-dfloat+    4 "box is a double-float")
(defconstant +mem-box-complex-sfloat+   5 "box is a complex of single-float")
(defconstant +mem-box-complex-dfloat+   6 "box is a complex of double-float")
(defconstant +mem-box-complex-rational+ 7 "box is a complex of fixnum, bignum or ratio")
(defconstant +mem-box-list+             8 "box is a complex of fixnum, bignum or ratio")
(defconstant +mem-box-path+             9 "box is a filesystem path")
(defconstant +mem-box-vector+          10 "box is a 1-dimensional array, vector or string")
(defconstant +mem-box-array+           11 "box is a N-dimensional array")
(defconstant +mem-box-hash-table+      12 "box is a hash-table")

