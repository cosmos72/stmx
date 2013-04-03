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

;;;; ** Sorted map, implemented with red-black trees.
;;;; For a transactional version, see tmap.lisp

(declaim (type bit +red+ +black+))
(defconstant +red+   1)
(defconstant +black+ 0)

(defclass bnode ()
  ((left  :initform nil   :type (or null bnode))
   (right :initform nil   :type (or null bnode))
   (key   :initarg :key)
   (value :initarg :value)
   (color :initform +red+ :type bit))

  (:documentation "Node of red-black tree"))


(defclass bmap ()
  ((root  :initform nil :type (or null bnode))
   (pred  :initarg :pred
          :initform (error "missing :pred argument")
          :type function
          :reader bmap-pred)
   (count :initform 0
          :type fixnum
          :reader bmap-count))

  (:documentation "Sorted map, implemented with red-black tree"))


(defmacro _ (obj slot-name)
  `(slot-value ,obj ',slot-name))


(defun print-bnode (stream node &optional (depth 0))
  (declare (type (or null bnode) node))
  (when node
    (print-bnode stream (_ node right) (1+ depth))
    (dotimes (i depth) (format stream "  "))
    (format stream "[~A] ~A = ~A~%"
            (if (red? node) "R" "B")
            (_ node key) (_ node value))
    (print-bnode stream (_ node left)  (1+ depth))))

(defmethod print-object-contents (stream (node bnode))
  (print-bnode stream node))

(defmethod print-object-contents (stream (m bmap))
  (print-bnode stream (_ m root)))

(defmethod print-bmap (stream m)
  (declare (type bmap m))
  (print-object-contents stream m))

#|
(defun log-debug-bmap (node parent stack txt &rest args &key &allow-other-keys)
  (declare (type (or null bnode) node parent)
           (type list stack)
           (type string txt))
  (let1 root (if stack
                 (first (last stack))
                 (or parent node))
    (log:debug "~A, root ~A, parent ~A, node ~A ~{~A~^ ~}~%~A" txt
               (if root   (_ root   key) nil)
               (if parent (_ parent key) nil)
               (if node   (_ node   key) nil)
               (loop for arg in args collect (if (typep arg 'bnode) (_ arg key) arg))
               (print-object-contents nil root))))

(defmacro log-debug (&rest args)
  `(log:debug ,@args))
|#

(defmacro log-debug (&rest args)
  (declare (ignore args))
  nil)

(defmacro log-debug-bmap (&rest args)
  (declare (ignore args))
  nil)





(declaim (inline bmap-empty? red? black? flip-color))

(defun bmap-empty? (m)
  "Return t if M is empty, otherwise return nil."
  (declare (type bmap m))
  (= 0 (_ m count)))

(defun red? (node)
  "Return t if NODE is red. Nil nodes are assumed to be black"
  (declare (type (or null bnode) node))
  (and node (eq +red+ (_ node color))))

(defun black? (node)
  "Return t if NODE is black. Nil nodes are assumed to be black"
  (declare (type (or null bnode) node))
  (not (red? node)))

(defun flip-color (node)
  "Flip NODE color. Return the new color."
  (declare (type bnode node))
  (with-slots (color) node
    (setf color (the bit (- (the bit 1) (the bit color))))))



(deftype comp-keyword () '(member :< :> :=))

(declaim (inline compare-keys))
(defun compare-keys (pred key1 key2)
  "Compare KEY1 agains KEY2 using the comparison function PRED.
Return :< if KEY1 compares as lesser than KEY2,
return :> if KEY1 compares as greater than KEY2,
return := if KEY1 and KEY2 compare as equal."
  (declare (type function pred))
  (the comp-keyword
    (cond
      ((funcall pred key1 key2) :<)
      ((funcall pred key2 key1) :>)
      (t :=))))

  
(defun get-bmap (m key &optional default)
   "Find KEY in M and return its value and T as multiple values.
If M does not contain KEY, return (values DEFAULT NIL)."
   (declare (type bmap m))

   (let ((node (_ m root))
         (pred (_ m pred)))
     (loop while node 
          for xkey = (_ node key) do
          (case (compare-keys pred key xkey)
            (:< (setf node (_ node left)))
            (:> (setf node (_ node right)))
            (:= (return-from get-bmap (values (_ node value) t)))))
     (values default nil)))



(defun rotate-left (node)
  "Rotate left the subtree around node. Return new subtree root."
  (declare (type bnode node))
  (log-debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node right)
    (setf (_ node right) (_ x left))
    (setf (_ x left)     node)
    (log-debug "after:~%~A" (print-object-contents nil x))
    x))


(defun rotate-right (node)
  "Rotate right the subtree around node. Return new subtree root."
  (declare (type bnode node))
  (log-debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node left)
    (setf (_ node left)  (_ x right))
    (setf (_ x right)    node)
    (log-debug "after:~%~A" (print-object-contents nil x))
    x))


(defun rotate-around (node parent &key left)
  (declare (type bnode node)
           (type (or null bnode) parent)
           (type boolean left))
  
  (let1 new-node
      (if left
          (rotate-left node)
          (rotate-right node))

    ;; connect parent to rotated node
    (when parent
      (if (is-left-child? node parent)
          (setf (_ parent left) new-node)
          (setf (_ parent right) new-node)))
    new-node))



(defun find-key-and-stack (m key &optional stack)
  "Return stack of visited nodes from root to insertion point for KEY,
and comparison between the KEY to insert and last visited node's key,
as multiple values"
   (declare (type bmap m))
   (let ((node (_ m root))
         (pred (the function (_ m pred)))
         (comp nil))
     (loop while node
        for xkey = (_ node key) do
          (push node stack)
          (case (setf comp (compare-keys pred key xkey))
            (:< (setf node (_ node left)))
            (:> (setf node (_ node right)))
            (t (return))))

     (values (the list stack)
             (the (or null comp-keyword) comp))))
     

(defun new-bnode (m key value)
   (declare (type bmap m))
   (incf (_ m count))
   (new 'bnode :key key :value value))


(defun set-bmap (m key value)
  "Add KEY to M if not present, and associate KEY to VALUE in M.
Return VALUE."
   (declare (type bmap m))

   (multiple-value-bind (stack comp) (find-key-and-stack m key)
     (let1 node (first stack)
       
       (when (eq := comp)
         ;; key already present
         (setf (_ node value) value)
         (return-from set-bmap value))

       ;; no such key, create node for it
       (let1 child (new-bnode m key value)
         (unless node
           ;; bmap is empty
           (with-slots (root) m
             (setf root child)
             (setf (_ root color) +black+))
           (return-from set-bmap value))

         ;; key not present, insert it
         (if (eq :< comp)
             (setf (_ node left) child)
             (setf (_ node right) child)))

       (when (red? node)
         (multiple-value-bind (node stack) (rebalance-after-insert m stack)
           (unless stack
             (setf (_ m root) node)
             (when node
               (setf (_ node color) +black+)))))))
   value)


(declaim (inline (setf get-bmap)))
(defun (setf get-bmap) (value m key)
  "Add KEY to M if not present, and associate VALUE to KEY in M.
Return VALUE."
  (set-bmap m key value))



(declaim (inline is-left-child?))
(defun is-left-child? (node parent)
  (declare (type bnode node parent))
  (eq node (_ parent left)))


(declaim (inline is-left-child-red?))
(defun is-left-child-red? (node)
  (declare (type bnode node))
  (red? (_ node left)))

(defun rebalance-after-insert (m stack)
  "Rebalance tree after inserting (first stack).
Return (values node stack) for some rebalanced node and its path from root.
If stack is nil, returned node is the new root to set."
  (declare (type bmap m)
           (type list stack)
           (ignore m))

  (prog ((node (pop stack))
         left-node?
         parent)
     (declare (type (or null bnode) node parent))
     (declare (type boolean left-node?))
           
     ;; 0) if X is black, we're DONE.
     rule-0
     (log-debug-bmap node parent stack "rule-0")
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
     (log-debug-bmap node parent stack "rule-1")
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
     (log-debug-bmap node parent stack "rule-2")
     (setf left-node? (is-left-child? node parent))
     (unless (eq left-node? (is-left-child-red? node))
       (setf node (rotate-around node parent :left left-node?))
       (log-debug-bmap node parent stack "rule-2, after rotate-around"))

     ;; 3) X* must have a black brother B and a red child C*,
     ;;    both must be on the same side of X*
     ;;         A                             X*                       X
     ;;        / \   rotate around X:        / \                      / \
     ;;       X*  B  rotate-right if        C*  A    swap colors     C*  A*
     ;;      /       A* is left child, ->        \   of X* and A ->       \  DONE.
     ;;     C*       rotate-left if               B                        B
     ;;              A* is right child
     (log-debug-bmap node parent stack "rule-3")
     (rotate-around parent (first stack) :left (not left-node?))
     (log-debug "before rotatef, node color = ~A, parent color = ~A"
                (_ node color) (_ parent color))
     (rotatef (_ node color) (_ parent color))
     (log-debug "after  rotatef, node color = ~A, parent color = ~A"
                (_ node color) (_ parent color))

     (log-debug-bmap node parent stack
                (format nil "rule-3, after (rotate-~A parent)"
                        (if left-node? "right" "left")))
     (return (values node stack))))
           



     
         

(defun replace-child-node (old-node new-node parent)
  "Unlink old-node from it parent and replace it with new-node.
Return t if left child was replaced, nil if right child was replaced"
  (declare (type (or null bnode) old-node new-node parent))
  (when parent
    (let1 left-child? (is-left-child? old-node parent)
      (if left-child?
          (setf (_ parent left) new-node)
          (setf (_ parent right) new-node))
      left-child?)))
  



(defun remove-black-node-at (m node stack)
  "Remove black leaf NODE, its path is STACK.
Return some node in rebalanced tree and its stack as multiple values"
  (declare (type bmap m)
           (type bnode node)
           (type list stack)
           (ignore m))

  (let1 parent (pop stack)
    (unless parent
      ;; 0) X was root, nothing to do
      (return-from remove-black-node-at nil))

    ;; being black, node X must be root or must have a brother. the brother must be:
    ;; - either a black node (with 0, 1 or 2 red children leaves)
    ;; - or a red node with two black children (and each of them with 0, 1 or 2 red children leaves)

    ;; also, callers guarantee that black node X has at most one child.


    (prog ;; replace black node X but remember his parent X and brother B.
          ((left-node? (replace-child-node node
                                           (or (_ node left) (_ node right))
                                           parent))
           brother)

     rule-1
     (setf brother (if left-node? (_ parent right) (_ parent left)))
     (log-debug-bmap node parent stack "start" :left-node? left-node? :brother brother)
          
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
       (push (rotate-around parent (first stack) :left left-node?) stack))
           
     (log-debug-bmap node parent stack "rule-1" :brother brother)

     ;; X must have a black brother B.
     ;; 2) if both B's children are black (or nil), make the brother red.
     ;;      this may cause double-red between brother B and parent A
     ;; 2.1) if such double-red occurs, set parent's color to black -> DONE
     (with-ro-slots (left right) brother
       (when (and (black? left) (black? right))
         (setf (_ brother color) +red+)
         (when (red? parent)
           (setf (_ parent color) +black+)
           (log-debug-bmap node parent stack "rule-2.1 DONE" :brother brother)
           (return (values parent stack)))
         ;; 2.2) otherwise move to parent (update the notion of X: it's the former parent) and:
         ;;        if we reached the root -> DONE
         ;;        otherwise go to 0)
         (setf node parent)
         (unless (setf parent (pop stack))
           (log-debug-bmap node parent stack "rule-2.2 DONE" :brother brother)
           (return (values node stack)))
         (setf left-node? (is-left-child? node parent))
         (go rule-1))


       (log-debug-bmap node parent stack "rule-2.2" :brother brother)

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
         (setf brother (rotate-around brother parent :left (not left-node?)))))

     (log-debug-bmap node parent stack "rule-3.1" :brother brother)

     ;; brother B may have changed, recompute its left and right children
     (with-ro-slots (left right) brother
       ;; 3.2) set B's far-child to black, copy parent color into B, set parent to black
       (if left-node?
            (setf (_ right color) +black+)
            (setf (_ left color) +black+))
       (with-slots (color) parent
         (setf (_ brother color) color)
         (setf color +black+))

       (log-debug-bmap node parent stack "rule-3.2" :brother brother :left left :right right))

     ;; 3.3) if node is left rotate-left around parent, otherwise rotate-right around parent
     (setf parent
           (rotate-around parent (first stack) :left left-node?))
     (log-debug-bmap node parent stack "rule-3.3")
     (return (values parent stack)))))



(defun swap-node-with-descendant (stack1 stack2 larger-node2?)
  (declare (type list stack1 stack2)
           (type boolean larger-node2?))
           
  (let ((node1   (first  stack1))
        (parent1 (second stack1))
        (node2   (first  stack2))
        (parent2 (second stack2)))

    (declare (type bnode node1 node2))
           
    (rotatef (_ node1 left)  (_ node2 left))
    (rotatef (_ node1 right) (_ node2 right))
    (rotatef (_ node1 color) (_ node2 color))

    (rotatef (first stack1) (first stack2))

    (when parent1
      (if (is-left-child? node1 parent1)
          (setf (_ parent1 left)  node2)
          (setf (_ parent1 right) node2)))

    (when (eq node1 parent2)
      (if larger-node2?
          (setf (_ node2 right) node1)
          (setf (_ node2 left)  node1))
      (return-from swap-node-with-descendant))

    (when parent2
      (if (is-left-child? node2 parent2)
          (setf (_ parent2 left)  node1)
          (setf (_ parent2 right) node1)))
    nil))




       
            
(defun remove-at (m stack)
  "Remove (first stack) from tree and return (values some-node that-node-stack)
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
        (let ((other-stack stack)
              (other-is-successor? t))

          (loop for left = right then (_ left left)
             while left do
               (push left other-stack))

          (let1 other-node (first other-stack)
            (unless (or (red? other-node) (_ other-node left) (_ other-node right))
              (setf other-stack stack
                    other-is-successor? nil)
              (loop for right = left then (_ right right)
                 while right do
                   (push right other-stack))))

          (log-debug-bmap node parent stack "before swap-with-descendant")

          (swap-node-with-descendant stack other-stack other-is-successor?)

          ;; proceed by removing node - it was moved, reload it
          (setf stack  other-stack
                node   (first stack)
                parent (second stack))

          (log-debug-bmap node parent stack "after swap-with-descendant"))))


    (pop stack) ;; node will not be in tree anymore

    (when (red? node)
      ;; red node with < 2 children, must be a leaf (the only child would be black, impossible)
      (replace-child-node node nil parent)
      (log-debug-bmap node parent stack "after delete red node")
      (return-from remove-at (values parent (rest stack))))
    
    ;; black node with < 2 children: either has no children, or one red leaf child
    (with-ro-slots (left right) node
      (when-bind child (or left right)
        (setf (_ child color) +black+)
        (replace-child-node node child parent)
        (log-debug-bmap node parent stack "after replace black node")
        (return-from remove-at (values child stack))))
    
    ;; the hard case: black node with no children
    (log-debug-bmap node parent stack "before remove-black-node-at")
    (return-from remove-at (remove-black-node-at m node stack))))
          


(defun rem-bmap (m key)
  "Find and remove KEY and its associated value from M.
Return t if KEY was removed, nil if not found."
  (declare (type bmap m))

  (with-slots (root) m
    (unless root
      (return-from rem-bmap nil))

    (multiple-value-bind (stack comp) (find-key-and-stack m key)

      (unless (eq := comp)
        (return-from rem-bmap nil))

      (multiple-value-bind (node stack) (remove-at m stack)
        ;; if root changed, stack will be at most 1 element
        (let1 new-root (or (first (last stack)) node)
          (when (not (eq root new-root))
            (log-debug "root change ~A -> ~A with stack ~{~A~^ ~}"
                       (if root (_ root key) nil)
                       (if new-root (_ new-root key) nil)
                       (loop for s in stack collect (_ s key)))
            (when (setf root new-root)
              (setf (_ new-root color) +black+)))))

      (decf (_ m count))
      t)))


(defun min-bmap (m)
  "Return the smallest key in M, its value, and t as multiple values,
or (values nil nil nil) if M is empty."
  (declare (type bmap m))
  (with-ro-slots (root) m
    (if root
        (loop for node = root then child
           for child = (_ node left)
           while child
           finally (return (values (_ node key) (_ node value) t)))
        (values nil nil nil))))
           

(defun max-bmap (m)
  "Return the largest key in M, its value, and t as multiple values,
or (values nil nil nil) if M is empty"
  (declare (type bmap m))
  (with-ro-slots (root) m
    (if root
        (loop for node = root then child
           for child = (_ node right)
           while child
           finally (return (values (_ node key) (_ node value) t)))
        (values nil nil nil))))
           


(defun clear-bmap (m)
  "Remove all keys and values from M. Return M."
  (declare (type bmap m))
  (setf (_ m root) nil)
  (setf (_ m count) 0)
  m)



(defun clone-bmap (m)
  "Create and return a copy of bmap M.
Keys and values in M are copied, not cloned"
  (declare (type bmap m))
  (let1 mclone (new 'bmap :pred (_ m pred))
    (labels ((clone-bnodes (node)
             (declare (type (or null bnode) node))
             (unless node
               (return-from clone-bnodes nil))
             (let1 clone (new-bnode m (_ node key) (_ node value))
               (setf (_ clone color) (_ node color))
               (setf (_ clone left)  (clone-bnodes (_ node left)))
               (setf (_ clone right) (clone-bnodes (_ node right)))
               clone)))
      (setf (_ mclone root) (clone-bnodes (_ m root)))
      (setf (_ mclone count) (_ m count))
      mclone)))
           


(defun fwd-traverse-bmap-at (m node func)
  (declare (type bmap m)
           (type (or null bnode) node)
           (type function func))
  (when node
    (fwd-traverse-bmap-at m (_ node left) func)
    (funcall func (_ node key) (_ node value))
    (fwd-traverse-bmap-at m (_ node right) func))
  nil)


(defun rev-traverse-bmap-at (m node func)
  (declare (type bmap m)
           (type (or null bnode) node)
           (type function func))
  (when node
    (rev-traverse-bmap-at m (_ node right) func)
    (funcall func (_ node key) (_ node value))
    (rev-traverse-bmap-at m (_ node left) func))
  nil)


(declaim (inline map-bmap map-bmap-from-end))
(defun map-bmap (m func)
  "Invoke FUNC in order on each key/value pair contained in M:
first invoke it on the smallest key, then the second smallest...
finally invoke FUNC on the largest key. Return nil.

FUNC must be a function accepting two arguments: key and value.

Adding or removing keys from M during this call (even from other threads)
has undefined consequences. Not even the current key can be removed."
  (declare (type bmap m)
           (type function func))
  (fwd-traverse-bmap-at m (_ m root) func))


(defun map-bmap-from-end (m func)
  "Invoke FUNC in reverse order on each key/value pair contained in M:
first invoke it on the largest key, then the second largest...
finally invoke FUNC on the smallest key. Return nil.

FUNC must be a function accepting two arguments: key and value.

Adding or removing keys from M during this call (even from other threads)
has undefined consequences. Not even the current key can be removed."
  (declare (type bmap m)
           (type function func))
  (rev-traverse-bmap-at m (_ m root) func))




(defmacro do-bmap ((key &optional value &key from-end) m &body body)
  "Execute BODY in order on each key/value pair contained in M:
first execute it on the smallest key, then the second smallest...
finally execute BODY on the largest key. Return nil.

If :FROM-END is true, BODY will be executed first on the largest key,
then on the second largest key... and finally on the smallest key.

Adding or removing keys from M during this call (even from other threads)
has undefined consequences. Not even the current key can be removed."

  (let* ((dummy (gensym))
         (func-body `(lambda (,key ,(or value dummy))
                       ,@(unless value `((declare (ignore ,dummy))))
                       ,@body)))
    (if from-end
        `(map-bmap-from-end ,m ,func-body)
        `(map-bmap          ,m ,func-body))))


(defun bmap-keys (m &optional to-list)
  "Return an ordered list of all keys contained in M."
  (declare (type bmap m)
           (type list to-list))
  (do-bmap (key value :from-end t) m
    (declare (ignore value))
    (push key to-list))
  to-list)

(defun bmap-values (m &optional to-list)
  "Return a list of all values contained in M.
The values are returned in the order given by their keys:
first the value associated to the smallest key, and so on."
  (declare (type bmap m)
           (type list to-list))
  (do-bmap (key value :from-end t) m
    (declare (ignore key))
    (push value to-list))
  to-list)


(defun bmap-pairs (m &optional to-alist)
  "Return an ordered list of pairs (key . value) containing
all entries in M."
  (declare (type bmap m)
           (type list to-alist))
  (do-bmap (key value :from-end t) m
    (push (cons key value) to-alist))
  to-alist)


(defun add-to-bmap (m &rest keys-and-values)
  "N-ary version of SET-BMAP and (SETF (GET-BMAP ...) ...):
Given a list of alternating keys and values,
add or replace each of them into M. Return M."
  (declare (type bmap m)
           (type list keys-and-values))
  (loop while keys-and-values
     for key = (pop keys-and-values)
     for value = (pop keys-and-values) do
       (set-bmap m key value))
  m)


(defun remove-from-bmap (m &rest keys)
  "N-ary version of REM-BMAP:
remove a list of keys from M. Return M."
  (declare (type bmap m)
           (type list keys))
  (loop for key in keys do
       (rem-bmap m key))
  m)

    