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

(defmacro _ (obj slot-name)
  (let* ((pkg (find-package (symbol-name 'stmx.util)))
         (actual-slot-name (find-symbol (symbol-name slot-name) pkg)))
    `(slot-value ,obj ',actual-slot-name)))



(defun fail-at (obj ref txt &rest args)
  (declare (type string txt))
  (fail "~A~%~A~%______reference______~%~A"
        (apply #'format nil txt args)
        (print-object-contents nil obj)
        (print-object-contents nil ref)))
  
(defun fsck-bmap-at (m ref node seen)
  "Check bmap invariants: no consecutive red nodes and
all paths to leaves must have the same number of black nodes.

Return two values: the total number of nodes in subtree starting at node,
and the number of black nodes in all paths from node to leaves"
  (declare (type bmap m)
           (type (or null bnode) node)
           (type hash-table seen))

  (unless node
    (return-from fsck-bmap-at (values 0 0)))

  (when (gethash node seen)
    (fail-at nil ref "duplicated node ~A" (_ node key)))

  (setf (gethash node seen) t)

  (let ((left (_ node left))
        (right (_ node right)))
    (when (red? node)
      (when (red? left)
        (fail-at m ref "node ~A and its left child ~A are both red~%~A"
                 (_ node key) (_ left key)))
      (when (red? right)
        (fail-at m ref "node ~A and its right child ~A are both red"
                 (_ node key) (_ right key))))

    (multiple-value-bind (nodes-left blacks-left) (fsck-bmap-at m ref left seen)
      (multiple-value-bind (nodes-right blacks-right) (fsck-bmap-at m ref right seen)
        (unless (= blacks-left blacks-right)
          (fail-at m ref "node ~A has ~A black nodes in left subtree, but ~A in right subtree"
                   (_ node key) blacks-left blacks-right))
        (values (+ 1 nodes-left nodes-right)
                (if (black? node) (1+ blacks-left) blacks-left))))))
            

(defun fsck-bmap (m &optional ref)
  "Check all bmap invariants: no consecutive red nodes,
all paths to leaves must have the same number of black nodes,
bmap-count must be the actual nodes count, root must be black."
  (declare (type bmap m))

  (let* ((root  (_ m root))
         (count (_ m count))
         (seen  (make-hash-table :test 'eq :size count))
         (nodes (fsck-bmap-at m ref root seen)))
    (unless (black? root)
      (fail-at m ref "bmap ~A root node ~A is red" m (_ root key)))
    (unless (eql nodes count)
      (fail-at m ref "bmap ~A node count is ~A, but actually has ~A nodes"
               m ref count nodes))
    nil))


(test new-bmap
  (let1 m (new 'bmap :pred #'<)
    (is-true (bmap-empty? m))
    (is (= 0 (bmap-count m)))
    (do-bmap (key value) m
      (fail "unexpected entry ~A = ~A in empty bmap" key value))))


(defun build-bmap (pred &optional list)
  (declare (type function pred)
           (type list list))

  (let1 m (new 'bmap :pred pred)
    (labels ((to-bnode (list)
               (declare (type list list))
               (let1 node (new-bnode m (first list) (second list))
                 (setf (_ node color) (if (third list) +red+ +black+))
                 node))
             (list-children (list)
               (declare (type list list))
               (cdddr list))
             (%build-bmap (list)
               (declare (type list list))
               (unless list
                 (return-from %build-bmap nil))
               (let* ((node (to-bnode list))
                      (children (list-children list)))
                 (setf (_ node left)  (%build-bmap (first children)))
                 (setf (_ node right) (%build-bmap (second children)))
                 node)))

      (setf (_ m root) (%build-bmap list))
      m)))
    
  
           

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
  (loop for (key . value) in (hash-table-to-sorted-pairs hash pred)
     collect value))
         


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


(test bmap
  (let* ((m1    (new 'bmap :pred #'<))
         (m2    (clone-bmap m1))
         (hash  (make-hash-table :test 'eql))
         (count 100))
    (dotimes (i count)
      (let* ((key (random count))
             (value (- key)))
        (set-bmap m1 key value)
        (set-hash hash key value)
        (is-equal-bmap-and-hash-table m1 hash)
        (fsck-bmap m1 m2)
        (set-bmap m2 key value)))
        

    (dotimes (i (* 2 count))
      (let1 key (random count)
        (is (eql (hash-table-count hash) (bmap-count m1)))
        (is-equal-bmap-and-hash-table m1 hash)
        (is (eql (rem-hash hash key) (rem-bmap m1 key)))
        (fsck-bmap m1 m2)
        (rem-bmap m2 key)))

    (let1 keys (hash-table-keys hash)
      (loop for key in keys do
           (is-true (rem-bmap m1 key))
           (is-true (rem-hash hash key))
           (is-equal-bmap-and-hash-table m1 hash)
           (fsck-bmap m1 m2)
           (rem-bmap m2 key)))))
           



           
    
