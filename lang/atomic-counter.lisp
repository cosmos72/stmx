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

;;;; ** atomic counter

#+(and stmx-have-sbcl.atomic-ops stmx-fixnum-is-power-of-two)
(eval-always
 (defstruct (atomic-counter (:constructor %make-atomic-counter))
   (version 1 :type sb-ext:word))

 (declaim (ftype (function (atomic-counter) fixnum)
                 incf-atomic-counter)
          (inline incf-atomic-counter)))


#-(and stmx-have-sbcl.atomic-ops stmx-fixnum-is-power-of-two)
(defstruct (atomic-counter (:constructor %make-atomic-counter))
  (version 0 :type fixnum)
  (lock (make-lock "ATOMIC-COUNTER")))


(declaim (ftype (function () atomic-counter) make-atomic-counter)
         (inline make-atomic-counter get-atomic-counter
                 #+stmx-have-sbcl.atomic-ops incf-atomic-counter))


(defun make-atomic-counter ()
  "Create and return a new ATOMIC-COUNTER."
  (%make-atomic-counter))
    



(defun incf-atomic-counter (counter)
  "Increase COUNTER by one and return its new value."
  (declare (type atomic-counter counter))


  #+stmx-fixnum-is-power-of-two
  ;; (1+ most-positive-fixnum) is a power of two
  (the fixnum

    #+stmx-have-sbcl.atomic-ops
    (logand most-positive-fixnum
            (sb-ext:atomic-incf (atomic-counter-version counter)))

    #-stmx-have-sbcl.atomic-ops
    ;; locking version
    (with-lock-held ((atomic-counter-lock counter))
      (setf (atomic-counter-version counter)
            (logand most-positive-fixnum
                    (1+ (atomic-counter-version counter))))))

  #-stmx-fixnum-is-power-of-two
  ;; generic version
  (with-lock-held ((atomic-counter-lock counter))
    (setf (atomic-counter-version counter)
          (let ((n (atomic-counter-version counter)))
            (the fixnum
              (if (= n most-positive-fixnum)
                  0
                  (1+ n)))))))


(defun get-atomic-counter (counter)
  "Return current value of ATOMIC-COUNTER."
  (declare (type atomic-counter counter))
       
  #+(and stmx-fixnum-is-power-of-two stmx-have-sbcl.atomic-ops)
  ;; (1+ most-positive-fixnum) is a power of two
  (progn
    (sb-thread:barrier (:read))
    (the fixnum
      (logand most-positive-fixnum
              (1-
               (logand most-positive-fixnum
                       (atomic-counter-version counter))))))

  #-(and stmx-fixnum-is-power-of-two stmx-have-sbcl.atomic-ops)
  ;; locking version
  (with-lock-held ((atomic-counter-lock counter))
    (atomic-counter-version counter)))
