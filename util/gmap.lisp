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


(in-package :stmx.util)

;;;; ** Generic sorted binary map
;;;; For a red-black trees implementation, see rbmap.lisp
;;;; For a transactional version, see tmap.lisp

(defclass gmap-node ()
  ;; allow LEFT and RIGHT to also be TVARS, otherwise subclass TNODE cannot work
  ((left  :initform nil  :type (or null gmap-node tvar) :accessor left-of)
   (right :initform nil  :type (or null gmap-node tvar) :accessor right-of)
   (key        :initarg :key                        :accessor key-of)
   (value      :initarg :value                      :accessor value-of))
  (:documentation "Generic binary tree node"))


(defclass gmap ()
  ;; allow ROOT to also be a TVAR, otherwise subclass TMAP cannot work
  ((root  :initform nil  :type (or null gmap-node tvar) :accessor root-of)
   (pred  :initarg :pred :type function             :accessor pred-of
          :initform (error "missing :pred argument instantiating ~A or a subclass" 'gmap))
   ;; allow COUNT to also be a TVAR, otherwise subclass TMAP cannot work
   (count :initform 0    :type (or fixnum tvar)     :accessor count-of))
  (:documentation "Generic binary tree"))



(let1 gmap-class (find-class 'gmap)
  (defmethod make-instance ((class (eql gmap-class)) &rest initargs &key &allow-other-keys)
    "GMAP is not supposed to be instantiated directly. For this reason,
\(make-instance 'gmap ...) will signal an error. Many Lispers may consider this
bad style; I prefer to be notified early if I try to do something plainly wrong."
    (declare (ignore initargs))
    (error "Cannot instantiate abstract class ~A" class)))




;;;; ** Public API


(defun gmap-pred (m)
  "Return predicate used by binary tree M to sort keys."
  (declare (type gmap m))
  (the function (_ m pred)))


(defun gmap-count (m)
  "Return number of elements in binary tree M."
  (declare (type gmap m))
  (the fixnum (_ m count)))


(defun gmap-empty? (m)
  "Return t if binary tree M is empty, otherwise return nil."
  (declare (type gmap m))
  (zerop (gmap-count m)))




(defgeneric rem-gmap (m key)
  (:documentation "Find and remove KEY and its associated value from binary tree M.
Return t if KEY was removed, nil if not found."))


(defgeneric clear-gmap (m)
  (:documentation "Remove all keys and values from M. Return M."))


(defgeneric add-to-gmap (m &rest keys-and-values)
  (:documentation "N-ary version of SET-GMAP and (SETF (GET-GMAP ...) ...):
Given a list of alternating keys and values,
add or replace each of them into M. Return M."))


(defgeneric remove-from-gmap (m &rest keys)
  (:documentation "N-ary version of REM-GMAP:
remove a list of keys from M. Return M."))



(defgeneric min-gmap (m)
  (:documentation "Return the smallest key in binary tree M, its value, and t as multiple values,
or (values nil nil nil) if M is empty."))

(defgeneric max-gmap (m)
  (:documentation "Return the largest key in binary tree M, its value, and t as multiple values,
or (values nil nil nil) if M is empty."))
  


(defgeneric copy-gmap (m)
  (:documentation "Create and return a copy of binary tree M.
Keys and values in M are shallow copied."))


(defgeneric copy-gmap-into (mcopy m)
  (:documentation "Fill MCOPY with a copy of gmap M and return MCOPY.
Copies all keys and values from M into MCOPY
and removes any other key/value already present in MCOPY."))



(defgeneric map-gmap (m func)
  (:documentation "Invoke FUNC in order on each key/value pair contained in M:
first invoke it on the smallest key, then the second smallest...
finally invoke FUNC on the largest key. Return nil.

FUNC must be a function accepting two arguments: key and value.

Adding or removing keys from M during this call (even from other threads)
has undefined consequences. Not even the current key can be removed."))


(defgeneric map-gmap-from-end (m func)
  (:documentation "Invoke FUNC in reverse order on each key/value pair contained in M:
first invoke it on the largest key, then the second largest...
finally invoke FUNC on the smallest key. Return nil.

FUNC must be a function accepting two arguments: key and value.

Adding or removing keys from M during this call (even from other threads)
has undefined consequences. Not even the current key can be removed."))



(defmacro do-gmap ((key &optional value &key from-end) m &body body)
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
        `(map-gmap-from-end ,m ,func-body)
        `(map-gmap          ,m ,func-body))))


(defgeneric gmap-keys (m &optional to-list)
  (:documentation "Return an ordered list of all keys contained in M."))


(defgeneric gmap-values (m &optional to-list)
  (:documentation "Return a list of all values contained in M.
The values are returned in the order given by their keys:
first the value associated to the smallest key, and so on."))


(defgeneric gmap-pairs (m &optional to-alist)
  (:documentation "Return an ordered list of pairs (key . value) containing
all entries in M."))



;;;; ** Abstract methods to be implemented by subclasses


(defgeneric gmap/new-node (m key value)
  (:documentation "Create and return a new node appropriate for binary tree M.
Methods must NOT invoke (incf (_ m count))."))

(defgeneric gmap/copy-node (m node)
  (:documentation "Create and return a copy of NODE appropriate for binary tree M.
Methods must NOT invoke (incf (_ m count))."))

(defgeneric gmap/rebalance-after-insert (m child stack)
  (:documentation "Rebalance binary tree M after inserting CHILD."))

(defgeneric gmap/remove-at (m stack)
  (:documentation "Remove (first STACK) from binary tree M and rebalance it.
Methods are supposed to explicitly (setf (_ m root) ...) when needed,
but must NOT invoke (decf (_ m count))."))






;;;; ** Debugging utilities

#|
(defun log.debug-gmap (node parent stack txt &rest args &key &allow-other-keys)
  (declare (type (or null gmap-node) node parent)
           (type list stack)
           (type string txt))
  (let1 root (if stack
                 (first (last stack))
                 (or parent node))
    (log:debug "~A, root ~A, parent ~A, node ~A ~{~A~^ ~}~%~A" txt
               (if root   (_ root   key) nil)
               (if parent (_ parent key) nil)
               (if node   (_ node   key) nil)
               (loop for arg in args collect (if (typep arg 'gmap-node) (_ arg key) arg))
               (print-object-contents nil root))))
|#

(defmacro log.debug-gmap (&rest args &key &allow-other-keys)
  (declare (ignore args))
  nil)


(defgeneric print-gmap-node (stream node &optional depth))

(defmethod print-gmap-node (stream (node null) &optional (depth 0))
  (declare (ignore stream node depth))
  nil)

(defmethod print-gmap-node (stream (node gmap-node) &optional (depth 0))
  (declare (type (or null gmap-node) node)
           (type fixnum depth))
  (let1 depth+1 (the fixnum (1+ depth))
    (print-gmap-node stream (_ node right) depth+1)
    (dotimes (i depth) (format stream "  "))
    (format stream "~A = ~A~%" (_ node key) (_ node value))
    (print-gmap-node stream (_ node left) depth+1)))


(defmethod print-object-contents (stream (node gmap-node))
  (print-gmap-node stream node))

(defmethod print-object-contents (stream (m gmap))
  (print-gmap-node stream (_ m root)))

(defmethod print-gmap (stream m)
  (declare (type gmap m))
  (print-object-contents stream m))



;;;; ** Base implementation


(deftype comp-keyword () '(member :< :> :=))

(declaim (inline compare-keys))
(defun gmap-compare-keys (pred key1 key2)
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



(defun get-gmap (m key &optional default)
  "Find KEY in binary tree M and return its value and T as multiple values.
If M does not contain KEY, return (values DEFAULT NIL)."
  (declare (type gmap m))
  (let ((node (_ m root))
        (pred (_ m pred)))
    (loop while node 
       for xkey = (_ node key) do
         (case (gmap-compare-keys pred key xkey)
           (:< (setf node (_ node left)))
           (:> (setf node (_ node right)))
           (t (return-from get-gmap (values (_ node value) t)))))
    (values default nil)))




(defun find-key-and-stack (m key &optional stack)
  "Return stack of visited nodes from root to insertion point for KEY,
and comparison between the KEY to insert and last visited node's key,
as multiple values"
   (declare (type gmap m))
   (let ((node (_ m root))
         (pred (the function (_ m pred)))
         (comp nil))
     (loop while node
        for xkey = (_ node key) do
          (push^ node stack)
          (case (setf comp (gmap-compare-keys pred key xkey))
            (:< (setf node (_ node left)))
            (:> (setf node (_ node right)))
            (t (return))))
     (values (the list stack)
             (the (or null comp-keyword) comp))))
     

(defun set-gmap (m key value)
  "Add KEY to binary tree M if not present, and associate KEY to VALUE in M.
Return VALUE."
  (declare (type gmap m))
  (multiple-value-bind (stack comp) (find-key-and-stack m key)
    (let1 node (first stack)
       
      (if (eq := comp)
          ;; key already present
          (setf (_ node value) value)

          ;; no such key, create node for it
          (let1 child (gmap/new-node m key value)
            (incf (the fixnum (_ m count)))
            (if node
                (if (eq :< comp)
                    (setf (_ node left) child)
                    (setf (_ node right) child))
                ;; gmap is empty
                (setf (_ m root) child))

            (gmap/rebalance-after-insert m child stack))))
      
    (free-list^ stack)
    value))



(declaim (inline (setf get-gmap)))
(defun (setf get-gmap) (value m key)
  "Add KEY to binary tree M if not present, and associate VALUE to KEY in M.
Return VALUE."
  (declare (type gmap m))
  (set-gmap m key value))




(defmethod rem-gmap ((m gmap) key)
  "Find and remove KEY and its associated value from binary tree M.
Return t if KEY was removed, nil if not found."
  (with-rw-slots (root) m
    (unless root
      (return-from rem-gmap nil))

    (multiple-value-bind (stack comp) (find-key-and-stack m key)
      (let1 found? (eq := comp)
        (when found?
          (gmap/remove-at m stack)
          (decf (the fixnum (_ m count))))

        (free-list^ stack)
        found?))))






(defmethod clear-gmap ((m gmap))
  "Remove all keys and values from M. Return M."
  (setf (_ m root) nil)
  (setf (_ m count) 0)
  (the gmap m))



(defmethod add-to-gmap ((m gmap) &rest keys-and-values)
  "N-ary version of SET-GMAP and (SETF (GET-GMAP ...) ...):
Given a list of alternating keys and values,
add or replace each of them into M. Return M."
  (declare (type gmap m)
           (type list keys-and-values))
  (loop while keys-and-values
     for key = (pop keys-and-values)
     for value = (pop keys-and-values) do
       (set-gmap m key value))
  m)


(defmethod remove-from-gmap ((m gmap) &rest keys)
  "N-ary version of REM-GMAP:
remove a list of keys from M. Return M."
  (declare (type list keys))
  (loop for key in keys do
       (rem-gmap m key))
  m)


(defmethod min-gmap ((m gmap))
  "Return the smallest key in M, its value, and t as multiple values,
or (values nil nil nil) if M is empty."
  (with-ro-slots (root) m
    (if root
        (loop for node = root then child
           for child = (_ node left)
           while child
           finally (return (values (_ node key) (_ node value) t)))
        (values nil nil nil))))
           

(defmethod max-gmap ((m gmap))
  "Return the largest key in M, its value, and t as multiple values,
or (values nil nil nil) if M is empty"
  (declare (type gmap m))
  (with-ro-slots (root) m
    (if root
        (loop for node = root then child
           for child = (_ node right)
           while child
           finally (return (values (_ node key) (_ node value) t)))
        (values nil nil nil))))
           


(defmethod copy-gmap ((m gmap))
  "Create and return a copy of binary tree M.
Keys and values in M are shallow copied."
  (declare (type gmap m))
  (let1 mcopy (new (class-of m) :pred (_ m pred))
    (copy-gmap-into mcopy m)))


(defmethod copy-gmap-into ((mcopy gmap) (m gmap))
  "Fill MCOPY with a copy of gmap M and return MCOPY.
Copies all keys and values from M into MCOPY
and removes any other key/value already present in MCOPY."
  (labels ((copy-nodes (node)
             (declare (type (or null gmap-node) node))
             (unless node
               (return-from copy-nodes nil))
             (let1 copy (gmap/copy-node m node)
               (setf (_ copy left)  (copy-nodes (_ node left)))
               (setf (_ copy right) (copy-nodes (_ node right)))
               copy)))
    (setf (_ mcopy root) (copy-nodes (_ m root))
          (_ mcopy count) (_ m count))
    mcopy))


(defun fwd-traverse-gmap-at (m node func)
  (declare (type gmap m)
           (type (or null gmap-node) node)
           (type function func))
  (when node
    (fwd-traverse-gmap-at m (_ node left) func)
    (funcall func (_ node key) (_ node value))
    (fwd-traverse-gmap-at m (_ node right) func))
  nil)


(defun rev-traverse-gmap-at (m node func)
  (declare (type gmap m)
           (type (or null gmap-node) node)
           (type function func))
  (when node
    (rev-traverse-gmap-at m (_ node right) func)
    (funcall func (_ node key) (_ node value))
    (rev-traverse-gmap-at m (_ node left) func))
  nil)


(defmethod map-gmap ((m gmap) func)
  (declare (type function func))
  (fwd-traverse-gmap-at m (_ m root) func))


(defmethod map-gmap-from-end ((m gmap) func)
  (declare (type function func))
  (rev-traverse-gmap-at m (_ m root) func))




(defmethod gmap-keys ((m gmap) &optional to-list)
  "Return an ordered list of all keys contained in M."
  (declare (type list to-list))
  (do-gmap (key value :from-end t) m
    (declare (ignore value))
    (push^ key to-list))
  to-list)


(defmethod gmap-values ((m gmap) &optional to-list)
  "Return a list of all values contained in M.
The values are returned in the order given by their keys:
first the value associated to the smallest key, and so on."
  (declare (type list to-list))
  (do-gmap (key value :from-end t) m
    (declare (ignore key))
    (push^ value to-list))
  to-list)


(defmethod gmap-pairs ((m gmap) &optional to-alist)
  "Return an ordered list of pairs (key . value) containing
all entries in M."
  (declare (type list to-alist))
  (do-gmap (key value :from-end t) m
    (push^ (cons^ key value) to-alist))
  to-alist)


    



;;;; ** Helper functions used by subclasses
         

(declaim (inline is-left-gmap-node-child?))
(defun is-left-gmap-node-child? (node parent)
  (declare (type gmap-node node parent))
  (eq node (_ parent left)))


(defun rotate-gmap-node-left (node)
  "Rotate left the subtree around node. Return new subtree root."
  (declare (type gmap-node node))
  (log.debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node right)
    (setf (_ node right) (_ x left))
    (setf (_ x left)     node)
    (log.debug "after:~%~A" (print-object-contents nil x))
    x))


(defun rotate-gmap-node-right (node)
  "Rotate right the subtree around node. Return new subtree root."
  (declare (type gmap-node node))
  (log.debug "before:~%~A" (print-object-contents nil node))
  (let1 x (_ node left)
    (setf (_ node left)  (_ x right))
    (setf (_ x right)    node)
    (log.debug "after:~%~A" (print-object-contents nil x))
    x))



(defun rotate-gmap-node-around (node parent &key left)
  "Rotate left or right the subtree around node. Return new subtree root
and also update parent's link to subtree root."
  (declare (type gmap-node node)
           (type (or null gmap-node) parent)
           (type boolean left))
  
  (let1 new-node
      (if left
          (rotate-gmap-node-left node)
          (rotate-gmap-node-right node))

    ;; connect parent to rotated node
    (when parent
      (if (is-left-gmap-node-child? node parent)
          (setf (_ parent left) new-node)
          (setf (_ parent right) new-node)))
    new-node))


(defun replace-gmap-node (old-node new-node parent)
  "Unlink old-node from it parent and replace it with new-node.
Return t if left child was replaced, nil if right child was replaced"
  (declare (type (or null gmap-node) old-node new-node parent))
  (when parent
    (let1 left-child? (is-left-gmap-node-child? old-node parent)
      (if left-child?
          (setf (_ parent left) new-node)
          (setf (_ parent right) new-node))
      left-child?)))

(defprint-object (obj gmap)
  (format t "~S ~S ~S ~S" :count (_ obj count) :pred (_ obj pred)))