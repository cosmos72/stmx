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

(defvar *flen* 0)

;; example MMAP length: 256MB on 32bit archs, 16TB on 64bit archs
(defconstant +max-flen+
  (let* ;; assume 1/4 of addressable memory can be mmapped
      ((arch-max-bytes (ash 1 (- +mem-word/bits+ 2)))
       
       ;; compute maximum bytes addressable by a stmx-persist:mem-pointer
       (persist-max-bytes (* +msizeof-word+ (box-pointer->size +most-positive-pointer+)))
                    
       ;; assume maximum size of a file is (1- 16TB). this is just an example, and not really needed
       (file-max-bytes (1- (ash 1 44)))

       (max-bytes (min arch-max-bytes persist-max-bytes file-max-bytes)))
    
    ;; assume mmap pagesize is 4k. this is just an example, and not really needed
    (logand (1- max-bytes) -4096)))


(defvar *p* +null-pointer+)


(defun open-fd (filename &optional (min-bytes 0))
  (declare (type (integer 0) min-bytes))

  (let* ((fd (osicat-posix:open filename (logior osicat-posix:o-rdwr osicat-posix:o-creat)))
         (file-bytes (osicat-posix:stat-size (osicat-posix:fstat fd))))
    (declare (type fd fd))
    (unless (zerop min-bytes)
      (when (< file-bytes min-bytes)
        (osicat-posix:ftruncate fd min-bytes)
        (setf file-bytes min-bytes)))
    (values fd file-bytes)))

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
  

(defun init-store (ptr total-n-words)
  "Initialize the free-list when loading a file full of zeros,
and write it back to file"
  (declare (type maddress ptr)
           (type mem-size total-n-words))
  (mem-free ptr +mem-box/min-words+ (mem-size- total-n-words +mem-box/min-words+)))



(defun open-store (&key (filename "mmap") (min-bytes 4096))
  (declare (type mem-word min-bytes))

  ;; open file and (if needed) extend it
  (multiple-value-bind (fd bytes) (open-fd filename min-bytes)
    (setf *fd* fd
          *flen* bytes)
                                        
    (let* (;; MMAP opened file
           (ptr (setf *p* (mmap fd bytes)))
           ;; load free-list from MMAP area
           (head (setf *mfree* (mread-free-list ptr 0))))

      (when (mfree-cell-null? head)
        (init-store ptr (truncate bytes +msizeof-word+)))

      head)))

    

         



(defun close-store ()
  (unless (null-pointer? *p*)
    (munmap *p* *flen*)
    (setf *p* +null-pointer+))
  (setf *mfree* nil)
  (unless (eql +bad-fd+ *fd*)
    (close-fd *fd*)
    (setf *fd* +bad-fd+
          *flen* 0)))




