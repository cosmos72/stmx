;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
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

(def-suite txhash-suite :in suite)
(in-suite txhash-suite)



(test new-txhash
  (let1 h (make-txhash-table)
    (is (= 0 (txhash-table-count h)))
    (do-txhash (key value) h
      (fail "unexpected entry ~A = ~A in empty txhash" key value))))



(defun txhash-table-keys (txh)
  (declare (type txhash-table txh))
  (let1 keys nil
    (do-txhash (key) txh
      (push key keys))
    keys))

(defun txhash-table-pairs (txh)
  (declare (type txhash-table txh))
  (let1 pairs nil
    (do-txhash (key value) txh
      (push (cons key value) pairs))
    pairs))


(defun txhash-table-to-sorted-keys (txh pred)
  (declare (type txhash-table txh)
           (type function pred))
  (sort (txhash-table-keys txh) pred))


(defun txhash-table-to-sorted-pairs (txh pred)
  (declare (type txhash-table txh)
           (type function pred))
  (sort (txhash-table-pairs txh) pred :key #'first))


(defun txhash-table-to-sorted-values (txh pred)
  (declare (type txhash-table txh)
           (type function pred))
  (loop for pair in (txhash-table-to-sorted-pairs txh pred)
     collect (rest pair)))
         


(defun is-equal-txhash-and-hash-table (txh hash pred)
  (declare (type txhash-table txh)
           (type hash-table hash)
           (type function pred))
  (is (= (hash-table-count hash)
         (txhash-table-count txh)))
  (is (equal (txhash-table-to-sorted-keys txh pred)
             (hash-table-to-sorted-keys hash pred)))
  (is (equal (txhash-table-to-sorted-values txh pred)
             (hash-table-to-sorted-values hash pred)))
  (is (equal (txhash-table-to-sorted-pairs txh pred)
             (hash-table-to-sorted-pairs hash pred))))


(defun is-equal-txhash (txh1 txh2 pred)
  (declare (type txhash-table txh1 txh2)
           (type function pred))
  (is (equal (txhash-table-to-sorted-pairs txh1 pred)
             (txhash-table-to-sorted-pairs txh2 pred))))
  

(defun test-txhash (pred &key (count 4))
  (declare (type fixnum count))
  (let ((txh   (make-txhash-table))
        (hash  (make-hash-table :test 'eq)))
    (dotimes (i count)
      (let ((key (tvar))
            (value (random count)))
        (set-txhash txh key value)
        (set-hash  hash key value)
        (is-equal-txhash-and-hash-table txh hash pred)))))
           

(test txhash
  (test-txhash #'tvar>))


           
    
