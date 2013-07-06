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


(declaim (optimize (compilation-speed 0) (space 0) (debug 0) (safety 0) (speed 3)))

(ql:quickload "stmx")

(in-package :stmx.util)


(defmacro x3 (&rest body)
  `(dotimes (,(gensym) 3)
     ,@body))


(defmacro with-timing (&rest body)
  (with-gensyms (start result end)
    `(let* ((,start (get-internal-real-time))
            (,result (locally ,@body))
            (,end (get-internal-real-time)))
       (format t " ~3$ seconds of real time~%"
               (/ (- (float ,end) (float ,start)) internal-time-units-per-second))
       ,result)))


(defmacro 1m (&rest body)
  "loops 1 million times on index variable I."
  (with-gensym successes
    `(with-timing
         (let ((,successes 0))
           (declare (type fixnum ,successes))
           (dotimes (i 1000000)
             (when (locally ,@body)
               (incf ,successes)))
           (print ,successes)))))


(defmacro 10m (&rest body)
  "loops 10 million times on index variable I."
  (with-gensym successes
    `(with-timing
         (let ((,successes 0))
           (declare (type fixnum ,successes))
           (dotimes (i 10000000)
             (when (locally ,@body)
               (incf ,successes)))
           (print ,successes)))))


(defvar *v* (tvar 0))
(defvar *m*  (new 'rbmap :pred #'fixnum<)) 
(defvar *tm* (new 'tmap  :pred #'fixnum<)) 
(defvar *h*  (new 'ghash-table :test #'fixnum= :hash #'identity)) 
(defvar *th* (new 'thash-table :test #'fixnum= :hash #'identity)) 


;; some initial values
(setf (get-gmap *m* 1) 0)
(setf (get-gmap *tm* 1) 0)
(setf (get-ghash *h* 1) 0)
(setf (get-ghash *th* 1) 0)


(defun example-hw-atomic-nil ()
  (x3
   (1m
    (hw-atomic (helper)
      t
      nil))))


;; recommendation: avoid accessing global variables from hardware transactions,
;; it has a strong tendency to cause aborts (global constants are ok)
;;
;; if you really need, read the global variables before starting the transaction
;; and store them in local variables.
;;
;; If you need to access/modify some shared state through global variables,
;; the variables must be CONSes, STRUCTs or OBJECTs so that you can still read them
;; before starting the transaction.
;;
;; reading and modifying their contents from within a hardware transactions is ok.


(defun example-hw-atomic-$ (&optional (v *v*))
  (declare (type tvar v))
  (x3
   (1m
    (hw-atomic (helper)
      ($-hwtx v)
      nil))))

(defun example-hw-atomic-setf-$ (&optional (v *v*))
  (declare (type tvar v))
  (x3
   (1m
    (hw-atomic (helper)
      (setf ($-hwtx v helper) 1)
      nil))))


(defun example-hw-atomic-incf-$ (&optional (v *v*))
  (declare (type tvar v))
  (x3
   (1m
    (hw-atomic (helper)
      (incf (the fixnum ($-hwtx v helper)))
      nil))))


(defun example-hw-atomic-n-incf-$ (&optional (n 10) (v *v*))
  (declare (type fixnum n)
           (type tvar v))
  (x3
   (1m
    (hw-atomic (helper)
      ;; remember to return non-NIL, indicating success
      (loop repeat n do
           (incf (the fixnum ($-hwtx v helper)))
         finally (return t))
      nil))))


(defun example-hw-atomic-orelse ()
  (x3
   (1m
    (hw-atomic (helper)
      ;; remember to return non-NIL, indicating success
      (progn
        (orelse)
        t)
      nil))))

(defun example-hw-atomic-get-gmap (&optional (tm *tm*))
  (declare (type tmap tm))
  (x3
   (1m
    (hw-atomic (helper)
      (get-gmap tm 1)

      ;; fallback
      (atomic (get-gmap tm 1)
              nil)))))

(defun example-hw-atomic-incf-gmap (&optional (tm *tm*))
  (declare (type tmap tm))
  (x3
   (1m
    (hw-atomic (helper)
      (incf (the fixnum (get-gmap tm 1)))

      ;; fallback
      (atomic (incf (the fixnum (get-gmap tm 1)))
              nil)))))


(defun example-hw-atomic-grow-gmap (&optional (n 10) (tm *tm*))
  (declare (type fixnum n)
           (type tmap tm))
  (x3
   (1m
    (hw-atomic (helper)
      (progn
        (when (zerop (mod i n)) (clear-gmap tm))
        (set-gmap tm i t))

      ;; fallback
      (atomic
       (when (zerop (mod i n)) (clear-gmap tm))
       (set-gmap tm i t)
       nil)))))
