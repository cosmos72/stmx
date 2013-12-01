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


(defconstant +bad-fd+ -1)

(deftype fd () 'fixnum)


(declaim (inline bad-fd?))
(defun bad-fd? (fd)
  (declare (type fd fd))
  (< fd 0))


(defvar *fd* +bad-fd+)
;; example MMAP length: 1GB on 32bit archs, 4TB on 64bit archs
(defvar *flen* (ash 1 (+ 18 (truncate (* 12 (/ +mem-word/bits+ 32))))))
(defvar *p* +null-pointer+)


(defun open-fd (filename &optional (truncate-bytes -1))
  (declare (type integer truncate-bytes))

  (let ((fd (osicat-posix:open filename (logior osicat-posix:o-rdwr osicat-posix:o-creat))))
    (declare (type fd fd))
    (unless (eql truncate-bytes -1)
      (osicat-posix:ftruncate fd truncate-bytes))
    fd))

(defun close-fd (fd)
  (declare (type fd fd))
  (osicat-posix:close fd))

(defun mmap (fd n-bytes)
  (declare (type fd fd)
           (type mem-word n-bytes))
  (osicat-posix:mmap +null-pointer+ n-bytes
                     (logior osicat-posix:prot-read osicat-posix:prot-write)
                     osicat-posix:map-shared
                     fd 0))

(defun munmap (ptr n-bytes)
  (declare (type maddress ptr)
           (type mem-word n-bytes))
  (osicat-posix:munmap ptr n-bytes))
  

(defun init-store (ptr head total-n-words)
  "Initialize the free-list when loading a file full of zeros,
and write it back to file"
  (let ((next (new-mfree-cell +mem-box/min-words+
                              (mem-size- total-n-words +mem-box/min-words+))))
    
    (setf (mfree-cell-next head) next)
    (mwrite-free-cell ptr next)
    (mwrite-free-cell ptr head)))



(defun open-store (&key (filename "mmap") (truncate-bytes *flen*))
  (declare (type mem-word truncate-bytes))

  (let* (;; open and truncate file
         (fd (setf *fd* (open-fd filename truncate-bytes)))

         ;; MMAP opened file
         (ptr (setf *p* (mmap fd truncate-bytes)))

         ;; load free-list from MMAP area
         (head (setf *mfree* (mread-free-list ptr 0))))

    (when (mfree-cell-null? head)
      (init-store ptr head (truncate truncate-bytes +msizeof-word+)))

    head))

    

         



(defun close-store ()
  (unless (null-pointer? *p*)
    (munmap *p* *flen*)
    (setf *p* +null-pointer+))
  (setf *mfree* nil)
  (unless (eql +bad-fd+ *fd*)
    (close-fd *fd*)
    (setf *fd* +bad-fd+)))




