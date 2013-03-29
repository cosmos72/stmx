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

(def-suite bmap-suite :in suite)
(in-suite bmap-suite)

(test new-bmap
  (let1 m (new 'bmap :pred #'<)
    (is-true (bmap-empty? m))
    (is (= 0 (bmap-count m)))
    (do-bmap (key value) m
      (fail "unexpected entry ~A = ~A in empty bmap" key value))))


(defun hash-table-to-sorted-keys (hash pred)
  (declare (type hash-table hash)
           (type function pred))
  (sort (hash-table-keys hash) pred))


(defun hash-table-to-sorted-pairs (hash pred)
  (declare (type hash-table hash)
           (type function pred))
  (sort (hash-table-pairs hash) pred :key #'first))


(defun hash-table-to-sorted-values (hash pred)
  (declare (type hash-table hash)
           (type function pred))
  (let1 pairs (hash-table-to-sorted-pairs hash pred)
    (loop for (key . value) in pairs
         collect value)))
         


(defun is-equal-bmap-and-hash-table (bmap hash)
  (declare (type bmap bmap)
           (type hash-table hash))
  (let1 pred (bmap-pred bmap)
    (is (equal (bmap-keys  bmap)
               (hash-table-to-sorted-keys hash pred)))
    (is (equal (bmap-values  bmap)
               (hash-table-to-sorted-values hash pred)))
    (is (equal (bmap-pairs bmap)
               (hash-table-to-sorted-pairs hash pred)))))


(test fill-bmap
  (let* ((bmap  (new 'bmap :pred #'<))
         (hash  (make-hash-table :test 'eql))
         (count 501))
    (dotimes (i count)
      (let* ((key (random count))
             (value (- key)))
        (set-bmap bmap key value)
        (set-hash hash key value))
      (when (zerop (mod i 100))
        (is-true (equal-bmap-and-hash-table bmap hash))))

    (dotimes (i (* 2 count))
      (let1 key (random count)
        (if (zerop (mod i 100))
            (progn
              (is (eql (hash-table-count hash) (bmap-count bmap)))
              (is-equal-bmap-and-hash-table bmap hash)
              (is (eql (rem-hash hash key) (rem-bmap bmap key))))
            (progn
              (rem-hash hash key)
              (rem-bmap bmap key)))))

    (let1 keys (hash-table-keys hash)
      (loop for key in keys do
           (is-true (rem-bmap bmap key))
           (is-true (rem-hash hash key))
           (is-equal-bmap-and-hash-table bmap hash)))))





    
