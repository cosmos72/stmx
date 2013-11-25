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




(defvar *fd* +bad-fd+)
(defvar *flen* (* 10 1024 1024 1024)) ;; 10GBytes
(defvar *p* +null-pointer+)

(defun open-store (&key (filename "mmap") (truncate-len *flen*))
  (setf *fd* (open-fd filename truncate-len))
  (setf *p* (mmap *fd* truncate-len)))

(defun close-store ()
  (unless (null-pointer? *p*)
    (munmap *p* *flen*)
    (setf *p* +null-pointer+))
  (unless (eql +bad-fd+ *fd*)
    (close-fd *fd*)
    (setf *fd* +bad-fd+))
  nil)



