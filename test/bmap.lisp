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

(let1 pkg (find-package (symbol-name 'stmx.util))
  (defmacro _ (obj slot-name)
    (let1 actual-slot-name (find-symbol (symbol-name slot-name) pkg)
      `(slot-value ,obj ',actual-slot-name))))


(defun fsck-bmap-at (node)
  "Check bmap invariants: no consecutive red nodes and
all paths to leaves must have the same number of black nodes.

Return two values: the total number of nodes in subtree starting at node,
and the number of black nodes in all paths from node to leaves"
  (declare (type (or null bnode) node))
  (unless node
    (return-from fsck-bmap-at (values 0 0)))

  (let ((left (_ node left))
        (right (_ node right)))
    (when (red? node)
      (when (red? left)
        (fail "node ~A and its left child ~A are both red"
                   (_ node key) (_ left key)))
      (when (red? right)
        (fail "node ~A and its right child ~A are both red"
                   (_ node key) (_ right key))))

    (multiple-value-bind (nodes-left blacks-left) (fsck-bmap-at left)
      (multiple-value-bind (nodes-right blacks-right) (fsck-bmap-at right)
        (unless (= blacks-left blacks-right)
          (fail "node ~A has ~A black nodes in left subtree, but ~A in right subtree"
                     (_ node key) blacks-left blacks-right))
        (values (+ 1 nodes-left nodes-right)
                (if (black? node) (1+ blacks-left) blacks-left))))))
            

(defun fsck-bmap (m)
  "Check all bmap invariants: no consecutive red nodes,
all paths to leaves must have the same number of black nodes,
bmap-count must be the actual nodes count, root must be black."
  (declare (type bmap m))
  (let* ((root (_ m root))
         (count (_ m count))
         (nodes (fsck-bmap-at root)))
    (unless (black? root)
      (fail "bmap ~A root node ~A is red" m (_ root key)))
    (unless (= nodes count)
      (fail "bmap ~A node count is ~A, but actually has ~A nodes"
                 m count nodes))))


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
        (is-equal-bmap-and-hash-table bmap hash))
      (fsck-bmap bmap))

    (dotimes (i (* 2 count))
      (let1 key (random count)
        (if (zerop (mod i 100))
            (progn
              (is (eql (hash-table-count hash) (bmap-count bmap)))
              (is-equal-bmap-and-hash-table bmap hash)
              (is (eql (rem-hash hash key) (rem-bmap bmap key))))
            (progn
              (rem-hash hash key)
              (rem-bmap bmap key))))
      (fsck-bmap bmap))

    (let1 keys (hash-table-keys hash)
      (loop for key in keys do
           (is-true (rem-bmap bmap key))
           (is-true (rem-hash hash key))
           (is-equal-bmap-and-hash-table bmap hash)
           (fsck-bmap bmap)))))






           

           
    
