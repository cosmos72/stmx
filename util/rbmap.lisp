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


(in-package :stmx.util)

;;;; ** Red-black tree implementation of sorted binary map.
;;;; For a transactional version, see tmap.lisp

(declaim (type bit +red+ +black+))
(defconstant +red+   1)
(defconstant +black+ 0)


(defclass rbnode (bnode)
  ;; allow COLOR to also be a TVAR, otherwise subclass TNODE cannot work
  ((color :initform +red+ :type (or bit tvar) :accessor color-of))
  (:documentation "Node for red-black trees"))


(defclass rbmap (bmap)
  ()
  (:documentation "Red-black tree implementation of sorted binary map"))


(declaim (inline red? black? flip-color))

(defun red? (node)
  "Return t if NODE is red. Nil nodes are assumed to be black"
  (declare (type (or null rbnode) node))
  (and node (eq +red+ (_ node color))))

(defun black? (node)
  "Return t if NODE is black. Nil nodes are assumed to be black"
  (declare (type (or null rbnode) node))
  (not (red? node)))

(defun flip-color (node)
  "Flip NODE color. Return the new color."
  (declare (type rbnode node))
  (with-rw-slots (color) node
    (setf color (the bit (- (the bit 1) (the bit color))))))


(declaim (inline is-left-rbnode-child?))
(defun is-left-rbnode-child? (node parent)
  (declare (type rbnode node parent))
  (eq node (_ parent left)))

(declaim (inline is-left-rbnode-child-red?))
(defun is-left-rbnode-child-red? (node)
  (declare (type rbnode node))
  (red? (_ node left)))




(defmethod bmap/new-node ((m rbmap) key value)
  (new 'rbnode :key key :value value))


(defmethod bmap/copy-node ((m rbmap) node)
  (declare (type rbmap m)
           (type rbnode node))
  (let1 copy (bmap/new-node m (_ node key) (_ node value))
    (setf (_ copy color) (_ node color))
    copy))
  

(defun %rbmap-rebalance-after-insert (m stack)
  "Rebalance red-black tree after adding a child to (first stack).
Return (values node stack) for some rebalanced node and its path from root.
If stack is nil, returned node is the new root to set."
  (declare (type rbmap m)
           (type list stack)
           (ignore m))

  (prog ((node (pop stack))
         left-node?
         parent)
     (declare (type (or null rbnode) node parent))
     (declare (type boolean left-node?))
           
     ;; 0) if X is black, we're DONE.
     rule-0
     (log.debug-bmap node parent stack "rule-0")
     (when (black? node)
       (return (values node stack)))

     ;;    Otherwise, red node X* must have a red child C*: double-red violation
     ;;      X*
     ;;      |      note: | means either left child / or right child \
     ;;      C*
     (setf parent (pop stack))

     ;; 1) if node X* has a red brother B*, then X* parent's A must be black
     ;;         A                     X*
     ;;        / \                   / \   then move up to A and:
     ;;       X*  B*  flip colors   X   B  if A is root, set it to black -> DONE.
     ;;       |       of X, A, B    |      otherwise move up to A's parent -> call it X and:
     ;;       C*                    C*     if X is root -> DONE
     ;;                                    otherwise, go to 0)
     (log.debug-bmap node parent stack "rule-1")
     (unless parent
       ;; node is root
       (return (values node stack)))

     (with-ro-slots (left right) parent
       (when (and (red? left) (red? right))
         (flip-color parent) (flip-color left) (flip-color right)
         (setf node (pop stack))
         (unless node
           ;; no more nodes, parent is root
           (return (values parent stack)))
         (when stack
           (go rule-0))
         ;; node is root
         (return (values node stack))))


     ;; 2) otherwise, X* must have a black brother B (possibly nil).
     ;;    Remember X* has a red child C*. If B and C* are on the same side of X*
     ;;         A                             A
     ;;        / \   rotate around X:        / \   now move to C*
     ;;       X*  B  rotate-left if         C*  B       v
     ;;        \     X* is left child, ->  /       call it X* and go to 3)
     ;;         C*   rotate-right if      X*    
     ;;              X* is right child
     (log.debug-bmap node parent stack "rule-2")
     (setf left-node? (is-left-rbnode-child? node parent))
     (unless (eq left-node? (is-left-rbnode-child-red? node))
       (setf node (rotate-bnode-around node parent :left left-node?))
       (log.debug-bmap node parent stack "rule-2, after rotate-bnode-around"))

     ;; 3) X* must have a black brother B and a red child C*,
     ;;    both must be on the same side of X*
     ;;         A                             X*                       X
     ;;        / \   rotate around X:        / \                      / \
     ;;       X*  B  rotate-right if        C*  A    swap colors     C*  A*
     ;;      /       A* is left child, ->        \   of X* and A ->       \  DONE.
     ;;     C*       rotate-left if               B                        B
     ;;              A* is right child
     (log.debug-bmap node parent stack "rule-3")
     (rotate-bnode-around parent (first stack) :left (not left-node?))
     (log.debug "before rotatef, node color = ~A, parent color = ~A"
                (_ node color) (_ parent color))
     (rotatef (_ node color) (_ parent color))
     (log.debug "after  rotatef, node color = ~A, parent color = ~A"
                (_ node color) (_ parent color))

     (log.debug-bmap node parent stack
                (format nil "rule-3, after (rotate-bnode-~A parent)"
                        (if left-node? "right" "left")))
     (return (values node stack))))



(defmethod bmap/rebalance-after-insert ((m rbmap) child stack)
  (declare (type rbnode child)
           (type list stack))
  "Rebalance red-black-tree M after inserting CHILD."

  (let1 node (first stack)
    (cond
      ((null node) (setf (_ child color) +black+)) ;; CHILD is root node.

      ((red? node)
       (multiple-value-bind (node stack) (%rbmap-rebalance-after-insert m stack)
         (unless stack
           (setf (_ m root) node)
           (when node
             (setf (_ node color) +black+)))))))
  nil)




(defun %rbmap-remove-black-node-at (m node stack)
  "Remove from red-black tree M black leaf NODE, whose path is STACK.
Return some node in rebalanced tree and its stack as multiple values"
  (declare (type rbmap m)
           (type rbnode node)
           (type list stack)
           (ignore m))

  (let1 parent (pop stack)
    (unless parent
      ;; 0) node X to remove was root, nothing to do
      (return-from %rbmap-remove-black-node-at nil))

    ;; being black, node X must be root or must have a brother. the brother must be:
    ;; - either a black node (with 0, 1 or 2 red children leaves)
    ;; - or a red node with two black children (and each of them with 0, 1 or 2 red children leaves)

    ;; also, callers guarantee that black node X has at most one child.


    (prog ;; replace black node X but remember his parent X and brother B.
          ((left-node? (replace-bnode node
				      (or (_ node left) (_ node right))
				      parent))
           brother)

     rule-1
     (setf brother (if left-node? (_ parent right) (_ parent left)))
     (log.debug-bmap node parent stack "start" :left-node? left-node? :brother brother)
          
     ;; 1.1) if X has a red brother B, their parent A must be black and B must have
     ;;      two black children C and D. flip colors of parent A and brother B.
     ;;       A                      A*
     ;;      / \     flip colors    / \
     ;;    (X)  B*   of A and B:  (X)  B
     ;;        / \                    / \
     ;;       C   D                  C   D
     (when (red? brother)             
       (rotatef (_ parent color) (_ brother color))
       ;; 1.2) if X is left child, rotate-left parent otherwise rotate-right parent
       ;;       A*                     B
       ;;      / \    rotate around   / \   there is still a missing black
       ;;    (X)  B   parent A:      A*  D  in the hole left by removing X...
       ;;        / \                / \     update the notion of X's brother
       ;;       C   D             (X)  C    (it's now the black node C) and continue

       (setf brother (if left-node? (_ brother left) (_ brother right)))
       ;; rotate around parent. grandparent (if exists) must also be linked
       ;; to former brother instead of former parent
       (push (rotate-bnode-around parent (first stack) :left left-node?) stack))
           
     (log.debug-bmap node parent stack "rule-1" :brother brother)

     ;; X must have a black brother B.
     ;; 2) if both B's children are black (or nil), make the brother red.
     ;;      this may cause double-red between brother B and parent A
     ;; 2.1) if such double-red occurs, set parent's color to black -> DONE
     (with-ro-slots (left right) brother
       (when (and (black? left) (black? right))
         (setf (_ brother color) +red+)
         (when (red? parent)
           (setf (_ parent color) +black+)
           (log.debug-bmap node parent stack "rule-2.1 DONE" :brother brother)
           (return (values parent stack)))
         ;; 2.2) otherwise move to parent (update the notion of X: it's the former parent) and:
         ;;        if we reached the root -> DONE
         ;;        otherwise go to 0)
         (setf node parent)
         (unless (setf parent (pop stack))
           (log.debug-bmap node parent stack "rule-2.2 DONE" :brother brother)
           (return (values node stack)))
         (setf left-node? (is-left-rbnode-child? node parent))
         (go rule-1))


       (log.debug-bmap node parent stack "rule-2.2" :brother brother)

       ;; B must have either two red children, or one black child (not nil) and one red.
       ;; 3.1) if B's far child is black, rotate around B to replace it with its former red child:
       ;;       A?                     A?
       ;;      / \    rotate around   / \     there can still be a missing black
       ;;    (X)  B   brother B:    (X)  C*   in the hole left by removing X...
       ;;        / \                    / \   update the notion of X's brother
       ;;       C*  D                      B  (it's now the black node C) and continue
       ;;      / \ / \                    / \
       ;;                                    D
       ;;                                   / \
       (when (black? (if left-node? right left))
         (setf brother (rotate-bnode-around brother parent :left (not left-node?)))))

     (log.debug-bmap node parent stack "rule-3.1" :brother brother)

     ;; brother B may have changed, recompute its left and right children
     (with-ro-slots (left right) brother
       ;; 3.2) set B's far-child to black, copy parent color into B, set parent to black
       (if left-node?
            (setf (_ right color) +black+)
            (setf (_ left color) +black+))
       (with-rw-slots (color) parent
         (setf (_ brother color) color)
         (setf color +black+))

       (log.debug-bmap node parent stack "rule-3.2" :brother brother :left left :right right))

     ;; 3.3) if node is left rotate-left around parent, otherwise rotate-right around parent
     (setf parent
           (rotate-bnode-around parent (first stack) :left left-node?))
     (log.debug-bmap node parent stack "rule-3.3")
     (return (values parent stack)))))



(defun %rbmap-remove-at (m stack)
  "Remove (first stack) from red-black tree M and return (values some-node that-node-stack)
from rebalanced tree. Some-node will be nil only if the tree is empty after removal."
  (declare (type bmap m)
           (type list stack))

  (let* ((node (first stack))
         (parent (second stack)))

    (with-ro-slots (left right) node
      (when (and left right)
        ;; node with two children. implementation choice:
        ;; replace node with successor (min of right subtree) if successor is red or has 1 child,
        ;; otherwise replace node with predecessor (max of left subtree)
        (let1 other-stack stack
          (loop for left = right then (_ left left)
             while left do
               (push left other-stack))

          (let1 other-node (first other-stack)
            (unless (or (red? other-node) (_ other-node left) (_ other-node right))
              (setf other-stack stack)
              (loop for right = left then (_ right right)
                 while right do
                   (push right other-stack))))

          (log.debug-bmap node parent stack "before swap with successor or predecessor")

          (let ((other-node (first other-stack))
                (other-parent (second other-stack)))

            (rotatef (_ node key)   (_ other-node key))
            (rotatef (_ node value) (_ other-node value))

            ;; proceed by removing other-node instead
            (setf node   other-node
                  parent other-parent
                  stack  other-stack))
                  
          (log.debug-bmap node parent stack "after swap with successor or predecessor"))))


    (pop stack) ;; node will not be in tree anymore

    (when (red? node)
      ;; red node with < 2 children, must be a leaf (the only child would be black, impossible)
      (replace-bnode node nil parent)
      (log.debug-bmap node parent stack "after delete red node")
      (return-from %rbmap-remove-at (values parent (rest stack))))
    
    ;; black node with < 2 children: either has no children, or one red leaf child
    (with-ro-slots (left right) node
      (when-bind child (or left right)
        (setf (_ child color) +black+)
        (replace-bnode node child parent)
        (log.debug-bmap node parent stack "after replace black node")
        (return-from %rbmap-remove-at (values child stack))))
    
    ;; the hard case: black node with no children
    (log.debug-bmap node parent stack "before remove-black-node-at")
    (return-from %rbmap-remove-at (%rbmap-remove-black-node-at m node stack))))
          


(defmethod bmap/remove-at ((m rbmap) stack)
  "Remove (first STACK) from red-black tree M and rebalance it."
  (declare (type list stack))

  (multiple-value-bind (node stack) (%rbmap-remove-at m stack)
    (with-rw-slots (root) m
      (let1 new-root (or (first (last stack)) node)
        (when (not (eq root new-root))
          (log.debug "root change ~A -> ~A with stack ~{~A~^ ~}"
                     (if root (_ root key) nil)
                     (if new-root (_ new-root key) nil)
                     (loop for s in stack collect (_ s key)))
          (when (setf root new-root)
            (setf (_ new-root color) +black+)))))))



(defmethod print-bnode (stream (node rbnode) &optional (depth 0))
  (declare (type fixnum depth))
  (let1 depth+1 (the fixnum (1+ depth))
    (print-bnode stream (_ node right) depth+1)
    (dotimes (i depth) (format stream "  "))
    (format stream "[~A] ~A = ~A~%"
            (if (red? node) "R" "B")
            (_ node key) (_ node value))
    (print-bnode stream (_ node left) depth+1)))

