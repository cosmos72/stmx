;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :stmx.lang)

(enable-#?-syntax)

;;;; ** thread-safe, atomic counter


(deftype atomic-counter-num ()
  #?+fixnum-is-large 'fixnum
  #?-fixnum-is-large 'integer)

(eval-always
  #?-(and atomic-ops fixnum-is-large-powerof2)
  (add-feature 'atomic-counter-mutex))

(deftype atomic-counter-slot-type ()
  #?+(and atomic-ops fixnum-is-large-powerof2) 'atomic-num ;; sb-ext:word on SBCL...
  #?-(and atomic-ops fixnum-is-large-powerof2) 'atomic-counter-num)


(defstruct (atomic-counter (:constructor %make-atomic-counter))
  (version 0 :type atomic-counter-slot-type)
  #?+atomic-counter-mutex
  (mutex (make-lock "ATOMIC-COUNTER")))



(declaim (ftype (function () atomic-counter) make-atomic-counter)
         (inline
           make-atomic-counter))

(defun make-atomic-counter ()
  "Create and return a new ATOMIC-COUNTER."
  (%make-atomic-counter))
    

(deftype positive-fixnum () '(and fixnum (integer 1)))

(declaim (ftype (function (atomic-counter &optional positive-fixnum) atomic-counter-num)
                incf-atomic-counter)
         
         #?+(and atomic-ops fixnum-is-large-powerof2)
         (inline incf-atomic-counter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro incf-atomic-place (place &optional (delta 1) &key place-mutex)
  "Increase atomic PLACE by DELTA and return its new value."
  (declare (ignorable place-mutex))

  (with-gensym _delta
    `(let1 ,_delta ,delta

       #?+(and atomic-ops fixnum-is-large-powerof2)
       (the atomic-counter-num
         (logand most-positive-fixnum
                 (+ ,_delta ;; atomic-incf returns the OLD value!
                    (logand most-positive-fixnum
                            (atomic-incf ,place ,_delta)))))

       #?-(and atomic-ops fixnum-is-large-powerof2)
       ;; locking version
       (with-lock (,place-mutex)
         (the atomic-counter-num
           #?+fixnum-is-large-powerof2
           ;; fast modulus arithmetic
           (setf ,place (logand most-positive-fixnum (+ ,place ,delta)))
      
           #?-fixnum-is-large-powerof2
           (progn
             #?+fixnum-is-large
             ;; fixnum arithmetic
             (setf ,place
                   (let ((n ,place))
                     (the atomic-counter-num
                       (if (> n (the atomic-counter-num (- most-positive-fixnum ,_delta)))
                           0
                           (+ _,delta n)))))

             #?-fixnum-is-large
             ;; general version: slow bignum arithmetic
             (incf ,place ,_delta)))))))


(defun incf-atomic-counter (counter &optional (delta 1))
  "Increase atomic COUNTER by DELTA and return its new value."
  (declare (type atomic-counter counter)
           (type fixnum delta))


  #?+(and atomic-ops fixnum-is-large-powerof2)
  (incf-atomic-place (atomic-counter-version counter) delta)

  #?-(and atomic-ops fixnum-is-large-powerof2)
  ;; locking version
  (incf-atomic-place (atomic-counter-version counter) delta
                     :place-mutex (atomic-counter-mutex counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (atomic-counter) atomic-counter-num) get-atomic-counter)
         (ftype (function (atomic-counter positive-fixnum) atomic-counter-num) get-atomic-counter-plus-delta)
         (ftype (function (atomic-counter atomic-counter-num) atomic-counter-num) set-atomic-counter)

         #?+(and atomic-ops fixnum-is-large-powerof2)
         (inline get-atomic-counter get-atomic-counter-plus-delta set-atomic-counter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-atomic-place (place)
  "Return current value of atomic PLACE."
       
  #?+(and atomic-ops fixnum-is-large-powerof2)
  `(progn
     (mem-read-barrier)
     (the fixnum
       (logand most-positive-fixnum ,place)))

  #?-(and atomic-ops fixnum-is-large-powerof2)
  `(the atomic-counter-num ,place))


(defun get-atomic-counter (counter)
  "Return current value of atomic COUNTER."
  (declare (type atomic-counter counter))
       
  #?+(and atomic-ops fixnum-is-large-powerof2)
  (get-atomic-place (atomic-counter-version counter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-atomic-place-plus-delta (place delta)
  "Return DELTA plus current value of atomic PLACE."
       
  #?+fixnum-is-large-powerof2
  `(the atomic-counter-num
     (logand most-positive-fixnum
             (+ ,delta
                (get-atomic-place ,place))))

  #?-fixnum-is-large-powerof2
  (progn
    #?+fixnum-is-large
    (with-gensyms (n _delta)
      `(let ((,_delta delta)
             (,n (get-atomic-place ,place)))
         (the fixnum
           (if (> ,n (the fixnum (- most-positive-fixnum ,_delta)))
               0
               (+ ,_delta ,n)))))

      #?-fixnum-is-large
      `(+ ,delta (get-atomic-place ,place))))




(defun get-atomic-counter-plus-delta (counter delta)
  "Return DELTA plus current value of atomic COUNTER."
  (declare (type atomic-counter counter)
           (type positive-fixnum delta))
       
  (get-atomic-place-plus-delta (atomic-counter-version counter) delta))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro set-atomic-place (place value &key place-mutex)
  "Set and return value of atomic PLACE."
  (declare (ignorable place-mutex))
       
  #?+(and atomic-ops fixnum-is-large-powerof2)
  `(progn
     (mem-write-barrier)
     (the atomic-counter-num
       (setf ,place ,value)))

  #?-(and atomic-ops fixnum-is-large-powerof2)
  ;; locking version
  (the atomic-counter-num
    (with-lock (,place-mutex)
      (setf ,place ,value))))


(defun set-atomic-counter (counter value)
  "Set and return value of atomic COUNTER."
  (declare (type atomic-counter counter)
           (type atomic-counter-num value))
       
  #?+(and atomic-ops fixnum-is-large-powerof2)
  (set-atomic-place (atomic-counter-version counter) value)

  #?-(and atomic-ops fixnum-is-large-powerof2)
  ;; locking version
  (set-atomic-place (atomic-counter-version counter) value
                    :place-mutex (atomic-counter-mutex counter)))
