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

;; try-take is already a transaction, wrapping it again is redundant
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

;; try-put is already a transaction, wrapping it again is redundant
(transaction
 (defun put3 (c val)
   (try-put c val)))

(defun orelse-test ()
  (loop for place in (list (new 'tcell) (tvar)) do
       (loop for takef in (list #'take1 #'take2 #'take3) do
            (loop for putf in (list #'put1 #'put2 #'put3)
               for unique = (gensym) do
                 (locally

                     (declare (type function takef putf))
                   
                   (multiple-value-bind (took? value) (funcall takef place)
                     (are-true (not took?)
                               (null value)
                               (empty? place)))

                   (multiple-value-bind (put? value) (funcall putf place unique)
                     (are-true put?
                               (eq value unique)
                               (full? place)))

                   (multiple-value-bind (put? value) (funcall putf place unique)
                     (are-true (not put?)
                               (null value)
                               (full? place)))

                   (multiple-value-bind (took? value) (funcall takef place)
                     (are-true took?
                               (eq value unique)
                               (empty? place))))))))
      
(test orelse
  (orelse-test))

(test orelse-atomic
  (atomic
   (orelse-test)))


(defun orelse-func (iterations cells names)
  (declare (type fixnum iterations)
           (type simple-vector cells names))

  (let ((x 0.0)
        (name nil))
    (declare (type single-float x))
    (dotimes (i iterations)

      (atomic
       (orelse
        (progn
          ;; Warning: (setf name ...) and (setf x ...) are NOT transactional
          ;; because x and name are normal variables, not TVARs or TOBJs.
          ;; This still works as expected because each branch in orelse sets both x and name,
          ;; so the transaction that succeeds will overwrite both
          (setf name (svref names 2))
          (log:trace "trying to take from cell ~A" name)
          (setf x (take (svref cells 2))))
        (progn
          (setf name (svref names 3))
          (log:trace "RETRIED, taking from cell ~A" name)
          (setf x (take (svref cells 3))))))
    
      (log:debug "took ~A from cell ~A" x name)
      (incf x)

      (atomic
       (orelse
         (progn
           (setf name (svref names 0))
           (log:trace "trying to put ~A in cell ~A" x name)
           (put (svref cells 0) x))
         (progn
           (setf name (svref names 1))
           (log:trace "RETRIED, putting ~A in cell ~A" x name)
           (put (svref cells 1) x))))

      (log:debug "put  ~A in cell ~A" x name))

    (log:debug "put  ~A in cell ~A [DONE]" x name)
    x))


(defun rotate-list (l n)
  (declare (type list l)
           (type fixnum n))
  (let1 fragment (loop for i from 1 to n collect (pop l))
    (append l fragment)))

(defun to-vector (seq)
  (coerce seq 'simple-vector))

(defun orelse-thread4 (&optional (iterations 1))
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

      (let1 ths (list (start-thread #'f1 :name "A")
                      (start-thread #'f1 :name "B")
                      (start-thread #'f2 :name "C")
                      (start-thread #'f2 :name "D"))

        (sleep 0.01)

        (log:debug "setting the four cell values...")
        (atomic
         (dotimes (i 4)
           (put (svref cells1 i) (* i 0.25))
           (log:debug "put ~A in cell ~A (may retry)"
                      (* i 0.25) (svref names1 i))))
        (log:debug "...cells values set")

        (values
         (loop for th in ths
            collect (let1 x (wait4-thread th)
                      (log:debug "thread ~A returned ~A" (thread-name th) x)
                      x))

         (atomic
          (loop for cell in cells
             collect (take cell))))))))


(defun orelse-thread4-test (&optional (iterations 1))
  (multiple-value-bind (values cells)
      (orelse-thread4 iterations)

    (loop for list in (list values cells) do
         (loop for e in list do
              (is-true (numberp e))))

    (let1 remainders (sort (loop for v in cells collect (mod v 1)) #'<)
      (is-true (equalp '(0.0 0.25 0.5 0.75) remainders)))

    (let1 total (apply #'+ cells)
      (is-true (= total (+ 1.5 (* 4 iterations)))))))

(test orelse-thread4
  (orelse-thread4-test 10000))
