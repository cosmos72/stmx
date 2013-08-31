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


(deftype atomic-counter-slot-type ()
  #?+fast-atomic-counter 'atomic-num ;; sb-ext:word on SBCL...
  #?-fast-atomic-counter 'atomic-counter-num)


(defstruct (atomic-counter (:constructor %make-atomic-counter))
  (version 0 :type atomic-counter-slot-type)
  #?-fast-atomic-counter
  (mutex (make-lock "ATOMIC-COUNTER")))


#?-fast-atomic-counter
(defmethod make-load-form ((obj atomic-counter) &optional environment)
  (declare (ignore environment))
  `(%make-atomic-counter :version ,(atomic-counter-version obj)))


(declaim (ftype (function () atomic-counter) make-atomic-counter)
         (inline
           make-atomic-counter))

(defun make-atomic-counter ()
  "Create and return a new ATOMIC-COUNTER."
  (%make-atomic-counter))
    

(deftype positive-fixnum () '(and fixnum (integer 1)))

(declaim (ftype (function (atomic-counter &optional positive-fixnum) atomic-counter-num)
                incf-atomic-counter)
         
         #?+fast-atomic-counter
         (inline incf-atomic-counter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro incf-atomic-place (place &optional (delta 1) &key place-mutex)
  "Increase atomic PLACE by DELTA and return its new value."
  (declare (ignorable place-mutex))

  #?+fast-atomic-counter
  (with-gensym delta-var
    `(let1 ,delta-var ,delta
       (the atomic-counter-num
         (logand most-positive-fixnum
                 (+ ,delta-var ;; atomic-incf returns the OLD value!
                    (logand most-positive-fixnum
                            (atomic-incf ,place ,delta-var)))))))

  #?-fast-atomic-counter
  (with-gensym delta-var
    `(let1 ,delta-var ,delta
       ;; locking version
       (with-lock (,place-mutex)
         (the atomic-counter-num
           ;; fast modulus arithmetic
           #?+fixnum-is-large-powerof2
           (setf ,place (logand most-positive-fixnum (+ ,place ,delta-var)))
      
           #?-fixnum-is-large-powerof2
           (progn
             ;; fixnum arithmetic
             #?+fixnum-is-large
             (setf ,place
                   (let ((n ,place))
                     (the atomic-counter-num
                       (if (> n (the atomic-counter-num (- most-positive-fixnum ,delta-var)))
                           0
                           (+ ,delta-var n)))))

             #?-fixnum-is-large
             ;; general version: slow bignum arithmetic
             (incf ,place ,delta-var)))))))


(defun incf-atomic-counter (counter &optional (delta 1))
  "Increase atomic COUNTER by DELTA and return its new value."
  (declare (type atomic-counter counter)
           (type fixnum delta))


  #?+fast-atomic-counter
  (incf-atomic-place (atomic-counter-version counter) delta)

  #?-fast-atomic-counter
  ;; locking version
  (incf-atomic-place (atomic-counter-version counter) delta
                     :place-mutex (atomic-counter-mutex counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (atomic-counter) atomic-counter-num) get-atomic-counter)
         (ftype (function (atomic-counter positive-fixnum) atomic-counter-num) get-atomic-counter-plus-delta)
         (ftype (function (atomic-counter atomic-counter-num) atomic-counter-num) set-atomic-counter)

         (inline get-atomic-counter)

         #?+fast-atomic-counter
         (inline get-atomic-counter-plus-delta set-atomic-counter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-atomic-place (place)
  "Return current value of atomic PLACE."
       
  `(progn
     #?+mem-rw-barriers (mem-read-barrier)

     (the atomic-counter-num
       #?+fast-atomic-counter (logand most-positive-fixnum ,place)
       #?-fast-atomic-counter ,place)))


(defun get-atomic-counter (counter)
  "Return current value of atomic COUNTER."
  (declare (type atomic-counter counter))
       
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
    (with-gensyms (n delta-var)
      `(let ((,n (get-atomic-place ,place))
             (,delta-var delta))
         (the fixnum
           (if (> ,n (the fixnum (- most-positive-fixnum ,delta-var)))
               0
               (+ ,delta-var ,n)))))

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

  #?+fast-atomic-counter
  (declare (ignore place-mutex))
       
  #?+fast-atomic-counter
  `(progn
     #?+mem-rw-barriers (mem-write-barrier)
     (the atomic-counter-num
       (setf ,place ,value)))

  #?-fast-atomic-counter
  ;; locking version
  `(the atomic-counter-num
     (with-lock (,place-mutex)
       (setf ,place ,value))))


(defun set-atomic-counter (counter value)
  "Set and return value of atomic COUNTER."
  (declare (type atomic-counter counter)
           (type atomic-counter-num value))
       
  #?+fast-atomic-counter
  (set-atomic-place (atomic-counter-version counter) value)

  #?-fast-atomic-counter
  ;; locking version
  (set-atomic-place (atomic-counter-version counter) value
                    :place-mutex (atomic-counter-mutex counter)))
