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

(def-suite ghash-table-suite :in suite)
(in-suite ghash-table-suite)



(test new-ghash-table
  (let1 h (new 'ghash-table :test-fun #'= :hash-fun #'identity)
    (is (= 0 (ghash-table-count h)))
    (do-ghash (key value) h
      (fail "unexpected entry ~A = ~A in empty ghash-table" key value))))



(defun ghash-table-to-sorted-keys (ghash pred)
  (declare (type ghash-table ghash)
           (type function pred))
  (sort (ghash-table-keys ghash) pred))


(defun ghash-table-to-sorted-pairs (ghash pred)
  (declare (type ghash-table ghash)
           (type function pred))
  (sort (ghash-table-pairs ghash) pred :key #'first))


(defun ghash-table-to-sorted-values (ghash pred)
  (declare (type ghash-table ghash)
           (type function pred))
  (loop for pair in (ghash-table-to-sorted-pairs ghash pred)
     collect (rest pair)))
         


(defun is-equal-ghash-and-hash-table (ghash hash pred)
  (declare (type ghash-table ghash)
           (type hash-table hash)
           (type function pred))
  (is (= (hash-table-count hash)
         (ghash-table-count ghash)))
  (is (equal (ghash-table-to-sorted-keys ghash pred)
             (hash-table-to-sorted-keys hash pred)))
  (is (equal (ghash-table-to-sorted-values ghash pred)
             (hash-table-to-sorted-values hash pred)))
  (is (equal (ghash-table-to-sorted-pairs ghash pred)
             (hash-table-to-sorted-pairs hash pred))))


(defun is-equal-ghash-table (ghash1 ghash2 pred)
  (declare (type ghash-table ghash1 ghash2)
           (type function pred))
  (is (equal (ghash-table-to-sorted-pairs ghash1 pred)
             (ghash-table-to-sorted-pairs ghash2 pred))))
  

(defun test-ghash-table (pred &key (count 16))
  (declare (type fixnum count))
  (let ((ghash (new 'ghash-table :test-fun #'eql :hash-fun #'identity))
        (hash  (make-hash-table :test 'eq)))
    (dotimes (i count)
      (let ((key   (random count))
            (value i))
        (set-ghash ghash key value)
        (set-hash  hash key value)
        (is-equal-ghash-and-hash-table ghash hash pred)))
    (dotimes (i count)
      (let ((key i))
        (rem-ghash ghash key)
        (rem-hash  hash  key)
        (is-equal-ghash-and-hash-table ghash hash pred)))))
      

           

(test ghash-table
  (test-ghash-table #'fixnum<))


           
    
