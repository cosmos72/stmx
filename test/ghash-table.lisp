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

(def-suite ghash-table-suite :in suite)
(in-suite ghash-table-suite)



(def-test new-ghash-table (:compile-at :definition-time)
  (let1 h (new 'ghash-table :test '= :hash 'identity)
    (is (= 0 (ghash-table-count h)))
    (do-ghash (key value) h
      (fail "unexpected entry ~A = ~A in empty ghash-table" key value))))



(defun ghash-table-to-sorted-keys (ghash comp)
  (declare (type ghash-table ghash)
           (type function comp))
  (sort (ghash-keys ghash) comp))


(defun ghash-table-to-sorted-pairs (ghash comp)
  (declare (type ghash-table ghash)
           (type function comp))
  (sort (ghash-pairs ghash) comp :key #'first))


(defun ghash-table-to-sorted-values (ghash comp)
  (declare (type ghash-table ghash)
           (type function comp))
  (loop for pair in (ghash-table-to-sorted-pairs ghash comp)
     collect (rest pair)))
         


(defun sort-and-compare-ghash-and-hash-table (ghash hash comp)
  "Compare ghash-table and hash-table by sorting their keys and values with COMP"
  (declare (type ghash-table ghash)
           (type hash-table hash)
           (type function comp))
  (is (eq (zerop (hash-table-count hash))
          (ghash-table-empty? ghash)))
  (is (= (hash-table-count hash)
         (ghash-table-count ghash)))
  (is (equal (ghash-table-to-sorted-keys ghash comp)
             (hash-table-to-sorted-keys hash comp)))
  (is (equal (ghash-table-to-sorted-values ghash comp)
             (hash-table-to-sorted-values hash comp)))
  (is (equal (ghash-table-to-sorted-pairs ghash comp)
             (hash-table-to-sorted-pairs hash comp))))


(defun sort-and-compare-ghash-table (ghash1 ghash2 comp)
  "Compare two ghash-tables by sorting their keys and values with COMP"
  (declare (type ghash-table ghash1 ghash2)
           (type function comp))
  (is (equal (ghash-table-to-sorted-pairs ghash1 comp)
             (ghash-table-to-sorted-pairs ghash2 comp))))
  

(defun test-ghash-table (ghash comp &key (count 16))
  (declare (type ghash-table ghash)
           (type function comp)
           (type fixnum count))
  (let1 hash (make-hash-table :test 'eql)
    (dotimes (i count)
      (let ((key   (random count))
            (value i))
        (set-ghash ghash key value)
        (set-hash  hash key value)
        (sort-and-compare-ghash-and-hash-table ghash hash comp)))
    (dotimes (i count)
      (let ((key i))
        (rem-ghash ghash key)
        (rem-hash  hash  key)
        (sort-and-compare-ghash-and-hash-table ghash hash comp)))))


(defun test-new-ghash-table (comp &key (count 16))
  (dolist (ghash
            (list
             (new 'ghash-table :test 'eql)
             (new 'ghash-table :test 'eql     :hash 'identity)
             (new 'ghash-table :test 'equal)
             (new 'ghash-table :test 'equal   :hash 'identity)
             (new 'ghash-table :test '=       :hash 'sxhash)
             (new 'ghash-table :test '=       :hash 'identity)
             (new 'ghash-table :test 'fixnum= :hash 'sxhash)
             (new 'ghash-table :test 'fixnum= :hash 'identity)))
    (test-ghash-table ghash comp :count count)))


(def-test ghash-table (:compile-at :definition-time)
  (test-new-ghash-table #'fixnum<))





(defun equalp-ghash-and-hash-table (ghash hash)
  "Compare ghash-table and hash-table with EQUALP"
  (declare (type ghash-table ghash)
           (type hash-table hash))
  (is (eq (zerop (hash-table-count hash))
          (ghash-table-empty? ghash)))
  (is (= (hash-table-count hash)
         (ghash-table-count ghash)))
  (do-ghash (k v1) ghash
    (multiple-value-bind (v2 present) (gethash k hash)
      (is-true present "key ~S has value ~S in GHASH-TABLE, but is missing from HASH-TABLE"
               k v1)
      (when present
        (is (equalp v1 v2) "key ~S has value ~S in HASH-TABLE, but value ~S in GHASH-TABLE"
            k v1 v2))))

  (loop for k being the hash-keys in hash using (hash-value v2) do
    (multiple-value-bind (v1 present) (get-ghash ghash k)
      (is-true present "key ~S has value ~S in HASH-TABLE, but is missing from GHASH-TABLE"
               k v2)
      (when present
        (is (equalp v1 v2) "key ~S has value ~S in HASH-TABLE, but value ~S in GHASH-TABLE"
            k v1 v2)))))


       


    
(defstruct %pair
  a b)

(defun make-cuckoo-hash-table ()
  (let ((h1 (make-hash-table :test 'equalp))
        (h2 (make-hash-table :test 'equalp))
        (h  (make-hash-table :test 'equalp)))
    (setf (gethash 1 h1) 2
          (gethash 2 h2) 1
          (gethash h1 h) h2
          (gethash h2 h) h1)
    h))


(defun test-ghash-table-equalp ()
  "hash tables using 'EQUALP test are tricky,
because they cannot rely on SXHASH for hashing."
  
  (let ((h (make-hash-table :test 'equalp))
        (g (new 'ghash-table :test 'equalp)))

    (macrolet ((with-test-data ((k v) &body body) 
                 `(loop for ,v from 0
                     for ,k in `(0 1 1/5 -7/3 ,(1- most-negative-fixnum) ,(1+ most-positive-fixnum)
                                  #\a #\z #c(1.0 2.0)
                                  (make-pathname :directory "foo" :name "bar" :type "baz")
                                  ((aa . ab) . (ba . bb))
                                  #(x y) #(#(xx xy) #(yx yy)) #2A((mm mn) (nm nn))
                                  ;; the second struct will replace the first
                                  ;; since they are EQUALP
                                  ,(make-%pair :a 'a :b 'b)
                                  ,(make-%pair :a 'a :b 'b)
                                  ,(make-%pair :a (make-%pair :a 'aa :b 'ab)
                                               :b (make-%pair :a 'ba :b 'bb))
                                  ;; the second hash-table will replace the first
                                  ;; since they are EQUALP
                                  ,(make-cuckoo-hash-table)
                                  ,(make-cuckoo-hash-table))
                     do (progn ,@body))))

      (with-test-data (k v)
        (setf (gethash k h) v))

      ;; instantiate again the test data, do NOT reuse it
      ;; in order to torture-test ghash-table
      (with-test-data (k v)
        (setf (get-ghash g k) v)))

    (equalp-ghash-and-hash-table g h)))

      

(def-test ghash-table-equalp (:compile-at :definition-time)
  (test-ghash-table-equalp))
