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

(defconstant +mem-pointer/shift+ 0)
(defconstant +mem-pointer/mask+  (1- (ash 1 +mem-pointer/bits+)))
(defconstant +most-positive-pointer+ +mem-pointer/mask+)

(defconstant +mem-fulltag/shift+ +mem-pointer/bits+)
(defconstant +mem-fulltag/mask+  (1- (ash 1 +mem-fulltag/bits+)))
(defconstant +most-positive-fulltag+ +mem-fulltag/mask+)

;; reserve most significant fulltag bit to mark integers
;; fulltags larger than +most-positive-tag+ indicate an integer (actually, mem-int) value
(defconstant +mem-tag/bits+      (1- +mem-fulltag/bits+))
(defconstant +mem-tag/mask+      (1- (ash 1 +mem-tag/bits+)))
(defconstant +most-positive-tag+ +mem-tag/mask+)

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




(defconstant +mem-unallocated+ 0)
(defconstant +mem-nil+ 1)
(defconstant +mem-t+   2)

(defconstant +mem-fulltag-keyword+  0)
(defconstant +mem-fulltag-character+ 1)
(defconstant +mem-fulltag-last+      +mem-tag/bits+)


