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

(defconstant +red+   (the bit 0))
(defconstant +black+ (the bit 1))

(defclass bnode ()
  ((left  :initform nil   :type (or null bnode))
   (right :initform nil   :type (or null bnode))
   (key   :initarg :key)
   (value :initarg :value)
   (color :initform +red+ :type bit)))


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




(declaim (inline bmap-count red? black? invert-color invert-colors compare-keys))

(defun red? (node)
  "NIL nodes are assumed to be black"
  (declare (type (or null bnode) node))
  (and node (eq +red+ (_ node color))))

(defun black? (node)
  "NIL nodes are assumed to be black"
  (declare (type (or null bnode) node))
  (not (red? node)))

(defun invert-color (node)
  "Invert the color of node"
  (declare (type bnode node))
  (with-slots (color) node
    (setf color (the bit (- 1 color)))))

(defun invert-colors (node)
  "Invert the colors of node and its left and right children, if any"
  (declare (type bnode node))
  (invert-color (_ node left))
  (invert-color (_ node right))
  (invert-color node))

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

  
(defun find-bmap (m key &optional default)
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
            (t  (return-from find-bmap (values (_ node value) t)))))
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
    (invert-colors node))

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
  "Add KEY to M if not present, and associate VALUE to KEY in M.
Return VALUE."
   (declare (type bmap m))

   (with-slots (root) m
     (setf root (set-bmap-at m root key value))
     (setf (_ root color) +black+))
   value)

(declaim (inline get-bmap (setf get-bmap)))
(defun get-bmap (key m &optional default)
   "Find KEY in M and return its value and T as multiple values.
If M does not contain KEY, return (values DEFAULT NIL)."
   (find-bmap m key default))

(defun (setf get-bmap) (value m key)
  "Add KEY to M if not present, and associate VALUE to KEY in M.
Return VALUE."
  (set-bmap m key value))




(defun move-red-left (node)
   (declare (type bnode node))
   (invert-colors node)
   (with-slots (right) node
     (when (red? (_ right left))
       (setf right (rotate-right right))
       (setf node  (rotate-left  node))
       (invert-colors node)))
   node)


(defun move-red-right (node)
   (declare (type bnode node))
   (invert-colors node)
   (when (red? (_ (_ node left) left))
       (setf node (rotate-right node))
       (invert-colors node))
   node)


(defun remove-min-at (node key-value-cons)
  (declare (type bnode node)
           (type cons key-value-cons))
  (with-ro-slots (left) node
    (unless left
      (setf (first key-value-cons) (_ node key))
      (setf (rest  key-value-cons) (_ node value))
      (return-from remove-min-at nil))
    (when (and (black? left) (black? (_ left left)))
      (setf node (move-red-left node))))
    
  (setf (_ node left) (remove-min-at (_ node left) key-value-cons))
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
              (let1 key-value-cons (cons nil nil)
                (setf (_ node right) (remove-min-at (_ node right) key-value-cons))
                (setf (_ node key)   (first key-value-cons))
                (setf (_ node value) (rest key-value-cons))
                (decf (_ m count)))
              (setf (_ node right) (remove-at m (_ node right) key)))))
    (fix-up node)))

    
(defun rem-bmap (m key)
  (let1 orig-count (_ m count)
    (with-slots (root) m
      (setf root (remove-at m root key))
      (when root
        (setf (_ root color) +black+))
      (< (_ m count) orig-count))))
