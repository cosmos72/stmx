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




(define-constant-once +zero-magic+ #.(make-string 4 :initial-element (code-char 0)))

(define-constant-once +short-magic+ #.(if +mem/little-endian+ "stmx" "xmts"))

(define-constant-once +long-magic+ (coerce '(#.(code-char 4) #\s #\t #\m #\x
                                             #.(code-char 7) #\p #\e #\r #\s #\i #\s #\t
                                             #.(code-char 1) #.(code-char 10) #.(code-char 0))
                                           'string))
(defun mwrite-magic (ptr)
  (declare (type maddress ptr))
  (loop for i from 0 below 4 do
       (mset-byte ptr i (char-code (char +short-magic+ i))))
  (mset-byte ptr 4 +mem-tag/bits+)
  (mset-byte ptr 5 +msizeof-word+)
  (mset-byte ptr 6 +msizeof-sfloat+)
  (mset-byte ptr 7 +msizeof-dfloat+)
  nil)


(defun mread-magic (ptr)
  (declare (type maddress ptr))
  (let ((magic (make-string 4)))
    (loop for i from 0 below 4 do
       (setf (char magic i) (code-char (mget-byte ptr i))))

    (when (equal magic +zero-magic+)
      (return-from mread-magic nil))

    (unless (equal magic +short-magic+)
      (error "STMX-PERSIST: unsupported file. expecting magic string (~{~S ~}), found (~{~S ~})"
             (coerce +short-magic+ 'list)
             (coerce magic 'list))))

  (loop for i from 4 below 8
     for pair in '((bits-per-tag   . #.+mem-tag/bits+)
                   (sizeof-word    . #.+msizeof-word+)
                   (sizeof-single-float . #.+msizeof-sfloat+)
                   (sizeof-double-float . #.+msizeof-dfloat+))
     for name = (first pair)
     for value = (rest pair)
     for ch = (mget-byte ptr i)
     unless (eql ch value) do
       (error "STMX-PERSIST: unsupported file. expecting ~S = ~S, found ~S"
              name value ch))
  t)
  

(defun get-abi ()
  '((:file-version   . 1)
    (:bits-per-byte  . #.+mem-byte/bits+)
    (:bits-per-tag   . #.+mem-tag/bits+)
    (:bits-per-pointer . #.+mem-pointer/bits+)
    (:bits-per-word  . #.+mem-word/bits+)
    (:sizeof-byte    . #.+msizeof-byte+)
    (:sizeof-word    . #.+msizeof-word+)
    (:sizeof-single-float . #.+msizeof-sfloat+)
    (:sizeof-double-float . #.+msizeof-dfloat+)
    (:little-endian  . #.+mem/little-endian+)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bad-fd+ -1)

(deftype fd () 'fixnum)


(declaim (inline bad-fd?))
(defun bad-fd? (fd)
  (declare (type fd fd))
  (< fd 0))


(defvar *fd* +bad-fd+)

(declaim (type mem-size *fwords*))
(defvar *fwords* 0)

;; example MMAP length: 256MB on 32bit archs, 127TB on 64bit archs
(defconstant +max-fwords+
  (let* ;; assume 1/4 of addressable memory can be actually used
      ((arch-max-bytes (ash 1 (- +mem-word/bits+ 2)))
       
       ;; assume maximum size of a mmap area is 127TB.
       ;; this is just an example, and not really needed
       (mmap-max-bytes #x7FE000000000)

       ;; compute maximum bytes addressable by a stmx-persist:mem-pointer
       (persist-max-bytes (* +msizeof-word+ (box-pointer->size +most-positive-pointer+)))
                    
       (max-bytes (min arch-max-bytes mmap-max-bytes persist-max-bytes ))

       ;; round down to a multiple of +pagesize+
       (max-bytes-rounded (logand max-bytes (- +pagesize+))))

    (truncate max-bytes-rounded +msizeof-word+)))


(defvar *p* +null-pointer+)


(defun open-fd (filename &optional (min-words 0))
  (declare (type mem-size min-words))

  (let* ((fd (the fd (osicat-posix:open filename (logior osicat-posix:o-rdwr osicat-posix:o-creat))))
         (file-words (truncate
                      (the (integer 0) (osicat-posix:stat-size (osicat-posix:fstat fd)))
                      +msizeof-word+)))
    (unless (zerop min-words)
      (when (< file-words min-words)
        (osicat-posix:ftruncate fd (* min-words +msizeof-word+))
        (setf file-words min-words)))

    (the (values fd mem-size)
      (values fd (min +most-positive-size+ file-words)))))


(defun close-fd (fd)
  (declare (type fd fd))
  (osicat-posix:close fd))


(cffi:defcfun ("mmap" !mmap :convention :cdecl :library :default) :pointer
  (start :pointer) (length osicat-posix::size) (prot :int) (flags :int)
  (fd :int) (offset osicat-posix::off)) 


(defun mmap (fd n-words)
  (declare (type fd fd)
           (type mem-size n-words))
  (osicat-posix:mmap +null-pointer+ (* n-words +msizeof-word+)
                     (logior osicat-posix:prot-read osicat-posix:prot-write)
                     osicat-posix:map-shared
                     fd 0))

(defun munmap (ptr n-words)
  (declare (type maddress ptr)
           (type mem-size n-words))
  (osicat-posix:munmap ptr (* n-words +msizeof-word+)))


(defun msync (ptr n-words &key sync)
  (declare (type maddress ptr)
           (type mem-size n-words)
           (type boolean sync))
  (osicat-posix:msync ptr (* n-words +msizeof-word+)
                      (if sync
                          #.(logior osicat-posix:ms-sync  osicat-posix:ms-invalidate)
                          #.(logior osicat-posix:ms-async osicat-posix:ms-invalidate))))

  

(defun init-store (ptr total-n-words)
  "Invoked when loading an unitialized file. Initialize the magic and free-list,
and write them back to file"
  (declare (type maddress ptr)
           (type mem-size total-n-words))
  
  (mwrite-magic ptr)
  (prog1
      (init-free-list ptr total-n-words)
    (msync ptr total-n-words)))




(defun open-store (&key (filename "mmap") (min-words #.(truncate +pagesize+ +msizeof-word+)))
  (declare (type mem-size min-words))

  ;; open file and (if needed) extend it
  (multiple-value-bind (fd words) (open-fd filename min-words)
    (setf *fd* fd
          *fwords* words)
                                        
    (let ((ptr))

      (unwind-protect
           (progn
             (setf ptr (mmap fd words)
                   *p* ptr)
             (if (mread-magic ptr)
                 (mread-free-list ptr)
                 (init-store ptr words)))

        (unless ptr
          (close-fd *fd*))))))
    

         



(defun close-store ()
  (unless (null-pointer? *p*)
    (munmap *p* *fwords*)
    (setf *p* +null-pointer+))
  (setf *mfree* nil)
  (unless (eql +bad-fd+ *fd*)
    (close-fd *fd*)
    (setf *fd* +bad-fd+
          *fwords* 0)))




