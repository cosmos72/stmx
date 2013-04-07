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


(in-package :stmx.test)

(def-suite orelse-suite :in suite)
(in-suite orelse-suite)

(defmacro are-true (&body body)
  `(progn
     ,@(loop for form in body
          collect `(is-true ,form))))

;; take1, take2 and take3 are equivalent

(transaction
 (defun take1 (c)
   (orelse (values t (take c)) nil)))

(transaction
 (defun take2 (c)
   (nonblocking (take c))))

(transaction
 (defun take3 (c)
   (try-take c)))

;; put1, put2 and put3 are equivalent

(transaction
 (defun put1 (c val)
   (orelse (values t (put c val)) nil)))

(transaction
 (defun put2 (c val)
   (nonblocking (put c val))))

(transaction
 (defun put3 (c val)
   (try-put c val)))

(defun orelse-test ()
  (loop for place in (list (new 'tcell) (new 'tvar)) do
       (loop for takef in (list #'take1 #'take2 #'take3) do
            (loop for putf in (list #'put1 #'put2 #'put3)
               for rand = (random most-positive-fixnum) do

                 (multiple-value-bind (took? value) (funcall takef place)
                   (are-true (not took?)
                             (null value)
                             (empty? place)))

                 (multiple-value-bind (put? value) (funcall putf place rand)
                   (are-true put?
                             (= value rand)
                             (full? place)))

                 (multiple-value-bind (put? value) (funcall putf place rand)
                   (are-true (not put?)
                             (null value)
                             (full? place)))

                 (multiple-value-bind (took? value) (funcall takef place)
                   (are-true took?
                             (= value rand)
                             (empty? place)))))))
      
(test orelse
  (orelse-test))

(test orelse-atomic
  (atomic
   (orelse-test)))


(defun orelse-func (iterations cells names)
  (declare (type fixnum iterations)
           (type vector cells names))

  (let ((x nil)
        (name nil))
    (dotimes (i iterations)

      (when x ;; skip this on first iteration
        (incf x)

        (atomic
         (orelse
          (progn
            (log:debug "trying to put ~A in cell ~A" x (setf name (aref names 0)))
            (put (aref cells 0) x))
          (progn
            (log:debug "RETRIED, putting ~A in cell ~A" x (setf name (aref names 1)))
            (put (aref cells 1) x))))

        (log:debug "put ~A in cell ~A" x name))

      (atomic
       (orelse
        (progn
          ;; Warning: (setf name ...) and (setf x ...) are NOT transactional
          ;; because x and name are normal variables, not TVARs or TOBJs.
          ;; This still works as expected because each branch in orelse sets both x and name,
          ;; so the transaction that succeeds will overwrite both
          (log:debug "trying to take from cell ~A" (setf name (aref names 2)))
          (setf x (take (aref cells 2))))
        (progn
          (log:debug "RETRIED, taking from cell ~A" (setf name (aref names 3)))
          (setf x (take (aref cells 3))))))

      (log:debug "took ~A from cell ~A" x name))

    x))


(defun rotate-list (l n)
  (declare (type list l)
           (type fixnum n))
  (let1 fragment (loop for i from 1 to n collect (pop l))
    (append l fragment)))

(defun to-vector (seq)
  (coerce seq 'vector))

(defun orelse-thread4-test (&optional (iterations 1))
  "This test runs a pass-the-ball algorithm with 4 threads
that take turns consuming (take) and producing (put) values in 4 cells.

Two threads consume values from cells 1 or 2, increase the value by one,
and produce into cells 3 or 4.
Two other threads consume values from cells 3 or 4, increase the value by one,
and produce into cells 1 or 2.

Consuming a value \"from cells x or y\" means (atomic (orelse (take cell-x) (take cell-y))),
i.e. if taking from the first cell fails, the second cell is used.
If that fails too, the thread sleeps until one of the two cells is available for (take)
- this sleeping and retrying behaviour is automatically implemented by
\(atomic (orelse <action1> <action2>)).

Similarly, producing a value \"into cells x or y\" means
\(atomic (orelse (put cell-x value) (put cell-y value))),
i.e. if putting into the first cell fails, the second cell is used.
If that fails too, the thread sleeps until one of the two cells is available for (put)
- again, this sleeping and retrying behaviour is automatically implemented by
\(atomic (orelse <action1> <action2>)).

The test first creates the threads, then atomically puts the four values
\(0 0.25 0.5 0.75) in the cells to trigger the pass-the-ball among the threads,
and finishes after each thread executed ITERATIONS loops, returning the final cell values."

  (declare (type fixnum iterations))
  (let* ((names '("A" "B" "C" "D"))
         (cells (loop for n in names collect (new 'tcell)))
         
         (cells1 (to-vector cells))
         (names1 (to-vector names))

         (cells2 (to-vector (rotate-list cells 2)))
         (names2 (to-vector (rotate-list names 2))))

    (flet ((f1 ()
             (orelse-func iterations cells1 names1))
           (f2 ()
             (orelse-func iterations cells2 names2)))

      (let1 ths (list (make-thread #'f1 :name "A")
                      (make-thread #'f1 :name "B")
                      (make-thread #'f2 :name "C")
                      (make-thread #'f2 :name "D"))

        (sleep 0.01)

        (log:debug "setting the four cell values...")
        (atomic
         (put (aref cells1 0) 0)
         (put (aref cells1 1) 0.25)
         (put (aref cells1 2) 0.5)
         (put (aref cells1 3) 0.75))
        (log:debug "...cells values set")

        (values
         (loop for th in ths
            collect (let1 x (join-thread th)
                      (log:debug "thread ~A returned ~A" (thread-name th) x)
                      x))

         (atomic
          (loop for cell in cells
             collect (empty? cell))))))))

(test orelse-thread4
  (let1 iterations 1000
    (multiple-value-bind (values empty-cells?)
        (orelse-thread4-test iterations)

      (loop for e in empty-cells?
         do (is-true e))

      (let1 remainders (sort (loop for v in values collect (mod v 1)) #'<)
        (is-true (equal remainders '(0 0.25 0.5 0.75))))

      (let1 total (loop for v in values sum (truncate v))
        (is-true (= total (* 4 (1- iterations))))))))
