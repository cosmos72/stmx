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

;;;; ** Balanced sorted map, implemented with left-leaning red-black trees.
;;;; For a transactional version, see tmap.lisp

(declaim (type bit +red+ +black+))
(defconstant +red+   0)
(defconstant +black+ 1)

(defclass bnode ()
  ((left  :initform nil   :type (or null bnode))
   (right :initform nil   :type (or null bnode))
   (key   :initarg :key)
   (value :initarg :value)
   (color :initform +red+ :type bit))

  (:documentation "Node of left-leaning red-black tree"))


(defclass bmap ()
  ((root  :initform nil :type (or null bnode))
   (pred  :initarg :pred
          :initform (error "missing :pred argument")
          :type function
          :reader bmap-pred)
   (count :initform 0
          :type fixnum
          :reader bmap-count))

  (:documentation "Left-leaning red-black tree"))


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
  (with-slots (root count) m
    (setf root nil)
    (setf count 0)
    m))


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

(defun flip-colors (node)
  "Flip the colors of NODE and its left and right children, if any"
  (declare (type bnode node))
  (flip-color (_ node left))
  (flip-color (_ node right))
  (flip-color node))


(defun compare-keys (pred key1 key2)
  "Compare KEY1 agains KEY2 using the comparison function PRED.
Return -1 if KEY1 compares as lesser than KEY2,
return  1 if KEY1 compares as greater than KEY2,
return  0 if KEY1 and KEY2 compare as equal."
  (declare (type function pred))
  (the fixnum
    (cond
      ((funcall pred key1 key2) -1)
      ((funcall pred key2 key1)  1)
      (t 0))))

  
(defun get-bmap (m key &optional default)
   "Find KEY in M and return its value and T as multiple values.
If M does not contain KEY, return (values DEFAULT NIL)."
   (declare (type bmap m))

   (let ((node (_ m root))
         (pred (_ m pred)))
     (loop while node 
          for xkey = (_ node key) do
          (case (compare-keys pred key xkey)
            (-1 (setf node (_ node left)))
            ( 1 (setf node (_ node right)))
            (t  (return-from get-bmap (values (_ node value) t)))))
     (values default nil)))



(defun rotate-left (node)
   (declare (type bnode node))
   (log:debug "before:~%~A" (print-object-contents nil node))
   (let1 x (_ node right)
     (setf (_ node right) (_ x left))
     (setf (_ x left)     node)
     (setf (_ x color)    (_ node color))
     (setf (_ node color) +red+)
     (log:debug "after:~%~A" (print-object-contents nil x))
     x))


(defun rotate-right (node)
   (declare (type bnode node))
   (log:debug "before:~%~A" (print-object-contents nil node))
   (let1 x (_ node left)
     (setf (_ node left)  (_ x right))
     (setf (_ x right)    node)
     (setf (_ x color)    (_ node color))
     (setf (_ node color) +red+)
     (log:debug "after:~%~A" (print-object-contents nil x))
     x))


(defun fix-up (node)
  (declare (type bnode node))

  (when (and (black? (_ node left)) (red? (_ node right)))
    (setf node (rotate-left node)))

  (when (and (red? (_ node left)) (red? (_ (_ node left) left)))
    (setf node (rotate-right node)))

  (when (and (red? (_ node left)) (red? (_ node right)))
    (flip-colors node))

  node)


(defun set-bmap-at (m node key value)
   (declare (type bmap m)
            (type (or null bnode) node))

   (log:debug "m = ~A, node = ~A, key = ~A, value = ~A" m node key value)

   (unless node
     (incf (_ m count))
     (return-from set-bmap-at (new 'bnode :key key :value value)))

   (with-slots (left right) node

     (let ((xkey (_ node key))
           (pred (_ m pred)))
       (case (compare-keys pred key xkey)
         (-1 (setf left  (set-bmap-at m left  key value)))
         ( 1 (setf right (set-bmap-at m right key value)))
         (t  (setf (_ node value) value))))

     (fix-up node)))
   

(defun set-bmap (m key value)
  "Add KEY to M if not present, and associate KEY to VALUE in M.
Return VALUE."
   (declare (type bmap m))

   (with-slots (root) m
     (setf root (set-bmap-at m root key value))
     (setf (_ root color) +black+))
   value)

(defun (setf get-bmap) (value m key)
  "Add KEY to M if not present, and associate VALUE to KEY in M.
Return VALUE."
  (set-bmap m key value))




(defun move-red-left (node)
   (declare (type bnode node))
   (flip-colors node)
   (with-slots (right) node
     (when (red? (_ right left))
       (setf right (rotate-right right))
       (setf node  (rotate-left  node))
       (flip-colors node)))
   node)


(defun move-red-right (node)
   (declare (type bnode node))
   (flip-colors node)
   (when (red? (_ (_ node left) left))
       (setf node (rotate-right node))
       (flip-colors node))
   node)


(defun remove-min-at (node store-key-value-here)
  (declare (type bnode node)
           (type bnode store-key-value-here))
  (with-ro-slots (left) node
    (unless left
      (setf (_ store-key-value-here key)   (_ node key))
      (setf (_ store-key-value-here value) (_ node value))
      (return-from remove-min-at nil))
    (when (and left (black? left) (black? (_ left left)))
      (setf node (move-red-left node))))
    
  (setf (_ node left) (remove-min-at (_ node left) store-key-value-here))
  (fix-up node))
  


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
          (setf (_ node left) (remove-at m (_ node left) key)))
        (progn
          (when (red? (_ node left))
            (setf node (rotate-right node)))
          (when (and (zerop (compare-keys pred key (_ node key))) (null (_ node right)))
            (decf (_ m count))
            (return-from remove-at nil))
          (with-ro-slots (right) node
            (when (and right (black? right) (black? (_ right left)))
              (setf node (move-red-right node))))
          (if (zerop (compare-keys pred key (_ node key)))
              (progn
                (setf (_ node right) (remove-min-at (_ node right) node))
                (decf (_ m count)))
              (setf (_ node right) (remove-at m (_ node right) key)))))
    (fix-up node)))


(defun rem-bmap (m key)
  "Find and remove KEY and its associated value from M.
Return t if KEY was removed, nil if not found."
  (declare (type bmap m))
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

    