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
(defconstant +red+   0)
(defconstant +black+ 1)

(defstruct bnode
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



(defun clear-bmap (m)
  "Remove all keys and values from M. Return M."
  (declare (type bmap m))
  (setf (_ m root) nil)
  (setf (_ m count) 0)
  m)


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
    (setf color (the bit (- 1 color)))))


(deftype comp-keyword () '(member :< :> :=))

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
  (log:debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node right)
    (setf (_ node right) (_ x left))
    (setf (_ x left)     node)
    (log:debug "after:~%~A" (print-object-contents nil x))
    x))


(defun rotate-right (node)
  "Rotate right the subtree around node. Return new subtree root."
  (declare (type bnode node))
  (log:debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node left)
    (setf (_ node left)  (_ x right))
    (setf (_ x right)    node)
    (log:debug "after:~%~A" (print-object-contents nil x))
    x))


(defun find-insertion-point (m key)
  "Return stack of visited nodes from root to insertion point for KEY,
and comparison between the KEY to insert and last visited node's key,
as multiple values"
   (declare (type bmap m))
   (let ((node (_ m root))
         (pred (_ m pred))
         (stack nil)
         (comp nil))
     (loop while node
        for xkey = (_ node key) do
          (push node stack)
          (case (setf comp (compare-keys pred key xkey))
            (:< (setf node (_ node left)))
            (:> (setf node (_ node right)))
            (:= (return))))
     (values (the list stack)
             (the (or null comp-keyword) comp))))
     
(defun show-insertion-point (m key)
  (multiple-value-bind (stack comp)
      (find-insertion-point m key)
    (values
     (loop for node in stack collect (_ node key))
     comp)))

(defun new-bnode (m key value)
   (declare (type bmap m))
   (incf (_ m count))
   (new 'bnode :key key :value value))

(defun set-bmap (m key value)
  "Add KEY to M if not present, and associate KEY to VALUE in M.
Return VALUE."
   (declare (type bmap m))

   (multiple-value-bind (stack comp) (find-insertion-point m key)
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
         (rebalance-after-insert m stack))))
   value)

(declaim (inline is-left-child?))
(defun is-left-child? (node parent)
  (declare (type bnode node parent))
  (eq node (_ parent left)))


(declaim (inline is-left-child-red?))
(defun is-left-child-red? (node)
  (declare (type bnode node))
  (red? (_ node left)))

(defun show-bmap (m node txt)
  (declare (type bmap m)
           (type bnode node)
           (type string txt))
  (log:debug "~A, node is ~A~%~A" txt (_ node key) (print-bmap nil m)))

(defun rebalance-after-insert (m stack)
  (declare (type bmap m)
           (type list stack))

  (prog ((node (pop stack))
         node-is-left
         parent)
     (declare (type (or null bnode) node parent))
     (declare (type boolean node-is-left))
           
     ;; 0) if A is black, we're DONE.
     rule-0
     (show-bmap m node "rule-0")
     (when (black? node)
       (return))

     ;;    Otherwise, red node A* must have a red child B*: double-red violation
     ;;      A*
     ;;      |      note: | means either left child / or right child \
     ;;      B*
     (setf parent (pop stack))

     ;; 1) if node A* has a red sibling C*, then A* parent's X must be black
     ;;         X                     X*
     ;;        / \                   / \   then move up to X and:
     ;;       A*  C*  flip colors   A   C  if X is root, set it to black -> DONE.
     ;;       |       of X, A, C    |      otherwise move up to X's parent -> call it A and:
     ;;       B*                    B*     if A is root -> DONE
     ;;                                    otherwise, go to 0)
     (show-bmap m node "rule-1")
     (unless parent
       (setf (_ node color) +black+)
       (return))

     (with-ro-slots (left right) parent
       (when (and (red? left) (red? right))
         (flip-color parent) (flip-color left) (flip-color right)
         (setf node (pop stack))
         (when node
           (if stack
               (go rule-0)
               ;; node is root
               (return)))
         ;; parent is root
         (setf (_ parent color) +black+)
         (return)))


     ;; 2) otherwise, A* must have a black sibling C (possibly nil).
     ;;    Remember A* has a red child B*. If B* and C are on the same side of A*
     ;;         X                             X
     ;;        / \   rotate around A:        / \   now move to B*               
     ;;       A*  C  rotate-left if         B*  C       v
     ;;        \     A* is left child, ->  /       call it A* and go to 3)
     ;;         B*   rotate-right if      A*    
     ;;              A* is right child
     (show-bmap m node "rule-2")
     (setf node-is-left (is-left-child? node parent))
     (unless (eq node-is-left (is-left-child-red? node))
       (if node-is-left
           (progn
             (setf node (rotate-left node))
             (setf (_ parent left) node)
             (show-bmap m node "rule-2, after rotate-left"))
           (progn
             (setf node (rotate-right node))
             (setf (_ parent right) node)))
             (show-bmap m node "rule-2, after rotate-right"))

     ;; 3) A* must have a black sibling C and a red child B*,
     ;;    both must be on the same side of A*
     ;;         X                             A*                       A
     ;;        / \   rotate around X:        / \                      / \
     ;;       A*  C  rotate-right if        B*  X    swap colors     B*  X*
     ;;      /       A* is left child, ->        \   of A* and X ->       \  DONE.
     ;;     B*       rotate-left if               C                        C
     ;;              A* is right child
     (show-bmap m node "rule-3")
     (if node-is-left 
         (rotate-right parent)
         (rotate-left parent))
     (log:debug "before rotatef, node color = ~A, parent color = ~A" (_ node color) (_ parent color))
     (rotatef (_ node color) (_ parent color))
     (log:debug "after  rotatef, node color = ~A, parent color = ~A" (_ node color) (_ parent color))

     ;;   we just need to attach node to its new parent
     (if-bind pparent (first stack)
       (if (is-left-child? parent pparent)
           (setf (_ pparent left) node)
           (setf (_ pparent right) node))
       (setf (_ m root) node))
     (show-bmap m node
                (format nil "rule-3, after (rotate-~A parent)"
                        (if node-is-left "right" "left")))
     (return)))
           

           
       
          


  
     
     


     
         

(declaim (inline (setf get-bmap)))
(defun (setf get-bmap) (value m key)
  "Add KEY to M if not present, and associate VALUE to KEY in M.
Return VALUE."
  (set-bmap m key value))




(defun move-red-left (node)
   (declare (type bnode node))
   (log:debug "before:~%~A" (print-object-contents nil node))
   (flip-colors node)
   (with-slots (right) node
     (when (red? (_ right left))
       (setf right (rotate-right right))
       (setf node  (rotate-left  node))
       (flip-colors node)))
   (log:debug "after:~%~A" (print-object-contents nil node))
   node)


(defun move-red-right (node)
   (declare (type bnode node))
   (log:debug "before:~%~A" (print-object-contents nil node))
   (flip-colors node)
   (when (red? (_ (_ node left) left))
       (setf node (rotate-right node))
       (flip-colors node))
   (log:debug "after:~%~A" (print-object-contents nil node))
   node)


(defun remove-min-at (node)
  "Remove node with smallest key in subtree starting at node.
Return new subtree root and removed node as multiple values."
  (declare (type bnode node))
  (with-ro-slots (left) node
    (unless left
      (return-from remove-min-at (values nil node)))
    (when (and left (black? left) (black? (_ left left)))
      (setf node (move-red-left node))))

  (multiple-value-bind (new-left removed-node) (remove-min-at (_ node left))
    (setf (_ node left) new-left)
    (values (fix-up node) removed-node)))
  


(defun remove-at (m node key)
  (declare (type bmap m)
           (type (or null bnode) node))

  (unless node
    (return-from remove-at nil))

  (let1 pred (_ m pred)
    (if (funcall pred key (_ node key))
        (progn
          (with-ro-slots (left) node
            (when (and left (black? left) (black? (_  left left)))
              (setf node (move-red-left node))))
          ;; keep searching in left subtree
          (setf (_ node left) (remove-at m (_ node left) key)))
        (progn
          (when (red? (_ node left))
            (setf node (rotate-right node)))
          (when (and (eq := (compare-keys pred key (_ node key))) (null (_ node right)))
            (decf (_ m count))
            (return-from remove-at nil))
          (with-ro-slots (right) node
            (when (and right (black? right) (black? (_ right left)))
              (setf node (move-red-right node))))
          (if (eq := (compare-keys pred key (_ node key)))
              ;; must remove current node.
              ;; find node with smallest key in right subtree,
              ;; remove it, and insert it back in place of current node
              (multiple-value-bind (new-right reuse-node)
                  (remove-min-at (_ node right))
                (log:debug "removing node (~A ~A), replacing with (~A ~A)"
                           (_ node key) (_ node value)
                           (_ reuse-node key) (_ reuse-node value))
                (with-slots (left right color) reuse-node
                  (setf left  (_ node left))
                  (setf right new-right)
                  (setf color (_ node color))
                  (decf (_ m count))
                  (setf node reuse-node)))
              ;; keep searching in right subtree
              (setf (_ node right) (remove-at m (_ node right) key))))))
  (fix-up node))


(defun rem-bmap (m key)
  "Find and remove KEY and its associated value from M.
Return t if KEY was removed, nil if not found."
  (declare (type bmap m))

  (error "rem-bmap must be rewritten! it assumes left-leaning red-black tree")

  (with-slots (root) m
    (if root
        (let1 orig-count (_ m count)
          (when (setf root (remove-at m root key))
            (setf (_ root color) +black+))
          (< (_ m count) orig-count))
        nil)))


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

    