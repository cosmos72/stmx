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

;;;; ** atomic counter


(deftype counter-num ()
  #?+fixnum-is-large 'fixnum
  #?-fixnum-is-large 'integer)

;; public alias for 'counter-num type
(deftype atomic-counter-num () 'counter-num)

#?+(and atomic-ops fixnum-is-large-powerof2)
(eval-always
  (defstruct (atomic-counter (:constructor %make-atomic-counter))
    (version 0 :type atomic-num))

  (declaim (inline incf-atomic-counter
                   get-atomic-counter)))


#?-(and atomic-ops fixnum-is-large-powerof2)
(eval-always
  (defstruct (atomic-counter (:constructor %make-atomic-counter))
    (version 0 :type counter-num)
    (lock (make-lock "ATOMIC-COUNTER"))))



(declaim (ftype (function () atomic-counter) make-atomic-counter)
         (inline
           make-atomic-counter))

(defun make-atomic-counter ()
  "Create and return a new ATOMIC-COUNTER."
  (%make-atomic-counter))
    

(deftype positive-fixnum () '(and fixnum (integer 1)))

(declaim (ftype (function (atomic-counter &optional positive-fixnum) counter-num)
                incf-atomic-counter))

(defun incf-atomic-counter (counter &optional (delta 1))
  "Increase atomic COUNTER by DELTA and return its new value."
  (declare (type atomic-counter counter)
           (type positive-fixnum delta))


  #?+(and atomic-ops fixnum-is-large-powerof2)
  (the counter-num
    (logand most-positive-fixnum
            (+ delta ;; atomic-incf returns the OLD value!
               (logand most-positive-fixnum
                       (atomic-incf (atomic-counter-version counter) delta)))))

  #?-(and atomic-ops fixnum-is-large-powerof2)
  ;; locking version
  (with-lock ((atomic-counter-lock counter))
    (the counter-num
      #?+fixnum-is-large-powerof2
      ;; fast modulus arithmetic
      (setf (atomic-counter-version counter)
            (logand most-positive-fixnum
                    (+ (atomic-counter-version counter) delta)))
      
      #?-fixnum-is-large-powerof2
      (progn
        #?+fixnum-is-large
        ;; fixnum arithmetic
        (setf (atomic-counter-version counter)
              (let ((n (atomic-counter-version counter)))
                (the fixnum
                  (if (> n (the fixnum (- most-positive-fixnum delta)))
                      0
                      (+ delta n)))))

        #?-fixnum-is-large
        ;; general version: slow bignum arithmetic
        (incf (atomic-counter-version counter) delta)))))


(declaim (ftype (function (atomic-counter) counter-num) get-atomic-counter)
         (ftype (function (atomic-counter positive-fixnum) counter-num) get-atomic-counter-plus-delta)
         (ftype (function (atomic-counter counter-num) counter-num) set-atomic-counter)

         #?+(and atomic-ops fixnum-is-large-powerof2)
         (inline get-atomic-counter get-atomic-counter-plus-delta set-atomic-counter))


(defun get-atomic-counter (counter)
  "Return current value of atomic COUNTER."
  (declare (type atomic-counter counter))
       
  #?+(and atomic-ops fixnum-is-large-powerof2)
  (progn
    (mem-read-barrier)
    (the fixnum
      (logand most-positive-fixnum
              (atomic-counter-version counter))))

  #?-(and atomic-ops fixnum-is-large-powerof2)
  ;; locking version
  (the counter-num
    (with-lock ((atomic-counter-lock counter))
      (atomic-counter-version counter))))


(defun get-atomic-counter-plus-delta (counter delta)
  "Return DELTA plus current value of atomic COUNTER."
  (declare (type atomic-counter counter)
           (type positive-fixnum delta))
       
  #?+fixnum-is-large-powerof2
  (logand most-positive-fixnum
          (+ delta
             (get-atomic-counter counter)))

  #?-fixnum-is-large-powerof2
  (let ((n (get-atomic-counter counter)))
    #?+fixnum-is-large
    (the fixnum
      (if (> n (the fixnum (- most-positive-fixnum delta)))
          0
          (+ delta n)))
    #?-fixnum-is-large
    (+ delta n)))



(defun set-atomic-counter (counter value)
  "Set and return value of atomic COUNTER."
  (declare (type atomic-counter counter)
           (type counter-num value))
       
  #?+(and atomic-ops fixnum-is-large-powerof2)
  (the counter-num
    (setf (atomic-counter-version counter) value))

  #?-(and atomic-ops fixnum-is-large-powerof2)
  ;; locking version
  (the counter-num
    (with-lock ((atomic-counter-lock counter))
      (setf (atomic-counter-version counter) value))))
