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

;;;; ** Generic sorted binary map
;;;; For a red-black trees implementation, see rbmap.lisp
;;;; For a transactional version, see tmap.lisp

(defclass bnode ()
  ((left  :initform nil  :type (or null bnode)   :accessor left-of)
   (right :initform nil  :type (or null bnode)   :accessor right-of)
   (key   :initarg :key                          :accessor key-of)
   (value :initarg :value                        :accessor value-of))
  (:documentation "Generic binary tree node"))


(defclass bmap ()
  ((root  :initform nil  :type (or null bnode)   :accessor root-of)
   (pred  :initarg :pred :type function          :accessor pred-of
          :initform (error "missing :pred argument"))
   (count :initform 0    :type fixnum            :accessor count-of))
  (:documentation "Generic binary tree"))


;; for some reason, SBCL invokes slot-value-using-class
;; only from slot accessors, not from (slot-value ...)

(defmacro _ (obj slot-name)
  `(slot-value ,obj ',slot-name))

#|
(eval-always
  (let1 of (symbol-name '-of)
    (defmacro _ (obj slot-name)
      (let1 accessor (intern (concatenate 'string (symbol-name slot-name) of))
      `(,accessor ,obj)))))
|#


(defun bmap-pred (m)
  (declare (type bmap m))
  (pred-of m))

(defun bmap-count (m)
  (declare (type bmap m))
  (count-of m))

  

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

(defmacro log-debug-bmap (&rest args &key &allow-other-keys)
  (declare (ignore args))
  nil)





(declaim (inline bmap-empty?))
(defun bmap-empty? (m)
  "Return t if M is empty, otherwise return nil."
  (declare (type bmap m))
  (= 0 (_ m count)))





#|
(defgeneric bmap/new-node (m key value)
  (:documentation "Create and return a new node appropriate for binary tree M.
Methods must NOT invoke (incf (_ m count))."))

(defgeneric bmap/copy-node (m node)
  (:documentation "Create and return a copy of NODE appropriate for binary tree M.
Methods must NOT invoke (incf (_ m count))."))



(defgeneric bmap/rebalance-after-insert (m child stack)
  (:documentation "Rebalance binary tree M after inserting CHILD."))


(defgeneric bmap/remove-at (m stack)
  (:documentation "Remove (first STACK) from binary tree M and rebalance it.
Methods are supposed to explicitly (setf (_ m root) ...) when needed,
but must NOT invoke (decf (_ m count))."))
|#













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
            (t (return-from get-bmap (values (_ node value) t)))))
     (values default nil)))


(declaim (inline is-left-child?))
(defun is-left-child? (node parent)
  (declare (type bnode node parent))
  (eq node (_ parent left)))


(declaim (inline is-left-child-red?))
(defun is-left-child-red? (node)
  (declare (type bnode node))
  (red? (_ node left)))


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
         (pred (_ m pred))
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
       (let1 child (bmap/new-node m key value)
         (incf (_ m count))
         (if node
             (if (eq :< comp)
                 (setf (_ node left) child)
                 (setf (_ node right) child))
             ;; bmap is empty
             (setf (_ m root) child))

         (bmap/rebalance-after-insert m child stack))))
   value)


(declaim (inline (setf get-bmap)))
(defun (setf get-bmap) (value m key)
  "Add KEY to M if not present, and associate VALUE to KEY in M.
Return VALUE."
  (declare (type bmap m))
  (set-bmap m key value))





     
         

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

      (bmap/remove-at m stack)
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
  (the bmap m))


(defun copy-bmap-into (m mcopy)
  "Fill M2 with a copy of bmap M1.
Copies all keys and values from M1 into M2
and removes any other key/value already present in M2."
  (declare (type bmap m mcopy))
  (labels ((copy-nodes (node)
             (declare (type (or null bnode) node))
             (unless node
               (return-from copy-nodes nil))
             (let1 copy (bmap/copy-node m node)
               (setf (_ copy left)  (copy-nodes (_ node left)))
               (setf (_ copy right) (copy-nodes (_ node right)))
               copy)))
    (setf (_ mcopy root) (copy-nodes (_ m root)))
    (setf (_ mcopy count) (_ m count))
    mcopy))


(defun copy-bmap (m)
  "Create and return a copy of binary tree M.
Keys and values in M are shallow copied."
  (declare (type bmap m))
  (let1 mcopy (new (class-of m) :pred (_ m pred))
    (copy-bmap-into m mcopy)
    mcopy))

           


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

    