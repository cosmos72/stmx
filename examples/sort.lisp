(in-package :cl-user)

(defpackage #:stmx.example2
  (:use #:cl
        #:bordeaux-threads
        #:stmx
        #:stmx.util)

  (:import-from #:stmx #:new))


(in-package :stmx.example2)
  
(defun sxhash< (arg1 arg2)
  (< (sxhash arg1) (sxhash arg2)))

(defun sxhash> (arg1 arg2)
  (> (sxhash arg1) (sxhash arg2)))

(let ((l (loop for i from 1 to 10 collect (random 1000))))
  (time
   (dotimes (i 1000000)
     (setf l (sort l #'stmx::sxhash<)))))

(time
 (dotimes (i 1000000)
   (let ((l (loop for i from 1 to 10 collect (random 1000))))
     (setf l (sort l #'stmx::sxhash<)))))

(let* ((v (make-array (list 100) :element-type 'fixnum)))
  (loop for i from 0 to (1- (length v)) do
       (setf (aref v i) (the fixnum (random 1000))))
  (time
   (dotimes (i 1000000)
     (setf v (sort v #'stmx::sxhash<)))))


(let* ((l (loop for i from 1 to 10 collect (random 1000)))
       (v (make-array (list 10) :element-type 'fixnum :initial-contents l)))
  (time
   (dotimes (i 1000000)
     (setf v (sort v #'stmx::sxhash<)))))



