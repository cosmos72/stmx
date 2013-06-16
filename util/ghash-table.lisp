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


;;;; * support class for GHASH-TABLE

(defclass ghash-pair ()
  ((key   :initform nil :initarg :key)
   (value :initform nil :initarg :value)
   (next  :initform nil :initarg :next :type (or null ghash-pair tvar))))


(declaim (type (or null ghash-pair) *ghash-pair-pool*))
(defvar *ghash-pair-pool* nil "Thread-local pool of free GHASH-PAIRs.")

(declaim (type fixnum +ghash-pair-pool-max-size+ *ghash-pair-pool-size*))
(defconstant +ghash-pair-pool-max-size+ 16384)
(defvar *ghash-pair-pool-size* 0)

(eval-always
  (ensure-thread-initial-bindings '(*ghash-pair-pool* . nil)
                                  '(*ghash-pair-pool-size* . 0)))

(declaim (inline new-ghash-pair-from-pool))
(defun new-ghash-pair-from-pool (key value next)
  (declare (type (or null ghash-pair) next))

  (the ghash-pair
    (if-bind pair *ghash-pair-pool*
      (progn
        (setf *ghash-pair-pool* (_ pair next)
              (_ pair key  ) key
              (_ pair value) value
              (_ pair next ) next)
        (decf (the fixnum *ghash-pair-pool-size*))
        pair)
      (new 'ghash-pair :key key :value value :next next))))


(defun free-ghash-pair-to-pool (pair)
  "Free a single GHASH-PAIR and add it to the pool.
Return NIL if pool is full after adding pair."
  (declare (type ghash-pair pair))
  (let1 pool-size *ghash-pair-pool-size*
    (when (< pool-size +ghash-pair-pool-max-size+)
        (setf (_ pair key  ) nil
              (_ pair value) nil
              (_ pair next ) *ghash-pair-pool*
              *ghash-pair-pool-size* (the fixnum (incf pool-size))
              *ghash-pair-pool*      pair)
        (return-from free-ghash-pair-to-pool (/= pool-size +ghash-pair-pool-max-size+))))
  nil)

(defun free-ghash-pairs-to-pool (pairs)
  "Free a list of GHASH-PAIR and add them to the pool.
Return NIL if pool is full after adding pairs."
  (declare (type ghash-pair pairs))

  (let1 initial-pool-size *ghash-pair-pool-size*
    (when (< initial-pool-size +ghash-pair-pool-max-size+)

      (loop for pair = pairs then next
         for next = (_ pair next)
         for pool-size from (1+ initial-pool-size)
         while (progn
                 (setf (_ pair key  ) nil
                       (_ pair value) nil)
                 next)
         for space-left = (< pool-size +ghash-pair-pool-max-size+)
         while space-left
         finally
           (setf (_ pair next) *ghash-pair-pool*
                 *ghash-pair-pool-size* pool-size
                 *ghash-pair-pool* pairs)
           (return space-left)))))






;;;; ** GHASH-TABLE

(declaim (type fixnum +ghash-default-capacity+ +ghash-max-capacity+))

(defconstant +ghash-default-capacity+ 4
  "Default initial capacity of a GHASH-TABLE.")

(defconstant +ghash-max-capacity+  (ash 1 (1- (integer-length most-positive-fixnum)))
  "Maximum capacity of a GHASH-TABLE.
Equal to MOST-POSITIVE-FIXNUM rounded down to nearest power of 2.")


(defun ghash-error-missing-arg (arg-name)
  (error "cannot instantiate ~S or a subclass: missing ~S argument"
         'ghash-table arg-name))


(defclass ghash-table ()
  ((vec      :type (or simple-vector simple-tvector tvar))

   (test-fun :type (function (t t) boolean) :initarg :test-fun
             :initform (ghash-error-missing-arg :test-fun))

   (hash-fun :type (function (t) fixnum) :initarg :hash-fun
             :initform (ghash-error-missing-arg :hash-fun))

   (count    :type (or fixnum tvar)      :initform 0))

  (:documentation
   "Generic hash-table. Requires explicit :test-fun and :hash-fun arguments at creation."))



(defmethod initialize-instance :after ((hash ghash-table) &key
                                       (initial-capacity +ghash-default-capacity+))

  (declare (type (integer #.+ghash-default-capacity+ #.+ghash-max-capacity+) initial-capacity))

  (unless (zerop (logand (1- initial-capacity) initial-capacity))
    ;; initial-capacity is not a power of 2: round up to nearset power of 2
    (setf initial-capacity (ash 1 (integer-length initial-capacity))))

  (setf (_ hash vec) (make-array initial-capacity :initial-element nil)))


(defun ghash-table-count (hash)
  "Return the number of KEY/VALUE entries in GHASH-TABLE hash."
  (declare (type ghash-table hash))
  (the (integer 0 #.most-positive-fixnum) (_ hash count)))

(defun ghash-table-empty? (hash)
  "Return T if GHASH-TABLE is empty, i.e. if it contains no entries."
  (declare (type ghash-table hash))
  (zerop (_ hash count)))


(defmacro do-ghash-pairs ((pair) hash &body body)
  "Execute BODY on each GHASH-PAIR pair contained in HASH. Return NIL."
  (with-gensyms (h vec i n left next loop-name)
    `(let* ((,h    (the ghash-table ,hash))
            (,vec  (the simple-vector (_ ,h vec)))
            (,n    (the fixnum (length ,vec)))
            (,left (ghash-table-count ,h)))
       (declare (type fixnum ,left))
           
       (dotimes (,i ,n)
         (when (zerop ,left)
           (return))
         (loop named ,loop-name
            for ,pair = (svref ,vec ,i) then ,next
            while ,pair
            for ,next = (_ ,pair next)
            do
              (decf ,left)
              (locally ,@body))))))
                


(defmacro do-ghash ((key &optional value) hash &body body)
  "Execute BODY on each KEY/VALUE contained in HASH. Return NIL."
  (with-gensym pair
    `(do-ghash-pairs (,pair) ,hash
       (let ((,key (_ ,pair key))
             ,@(when value `((,value (_ ,pair value)))))
         ,@body))))
                

         
(declaim (inline ghash-mask ghash-subscript find-ghash-pair get-ghash))

(defun ghash-mask (vec-len)
  "Return the bitmask to use for hash indexes."
  (declare (type fixnum vec-len))
  (the fixnum (1- vec-len)))


(defun ghash-subscript (hash-code vec &optional (vec-len (length vec)))
  "Return the array subscript in HASH corresponding to HASH-CODE."
  (declare (type simple-vector vec)
           (type fixnum hash-code vec-len))
  (the fixnum (logand
               (ghash-mask vec-len)
               (logxor hash-code
                       (ash hash-code -10)))))


(defun find-ghash-pair (hash key)
  "If KEY is present in HASH, return the GHASH-PAIR containing KEY.
Otherwise return NIL."
  (declare (type ghash-table hash))

  (let* ((test-fun (_ hash test-fun))
         (hash-fun (_ hash hash-fun))
         (hash-code (the fixnum (funcall hash-fun key)))
         (vec       (the simple-vector (_ hash vec)))
         (subscript (ghash-subscript hash-code vec))
         (head      (svref vec subscript)))

    (the (or null ghash-pair)
      (loop for pair = head then (_ pair next)
         while pair do
           (when (funcall test-fun key (_ pair key))
             (return pair))))))


(defun get-ghash (hash key &optional default)
  "If KEY is associated to VALUE in HASH, return (values VALUE t)
Otherwise return (values DEFAULT nil)."
  (declare (type ghash-table hash))

  (if-bind pair (find-ghash-pair hash key)
    (values (_ pair value) t)
    (values default nil)))

    
  
(defun rehash-ghash (hash)
  (declare (type ghash-table hash))

  (let* ((vec1 (the simple-vector (_ hash vec)))
         (n2   (the fixnum (ash (length vec1) 1)))
         (vec2 (the simple-vector (make-array n2 :initial-element nil)))
         (hash-fun (_ hash hash-fun)))

    (do-ghash-pairs (pair) hash
      (let* ((key (_ pair key))
             (hash-code (funcall hash-fun key))
             (subscript (ghash-subscript hash-code vec2 n2))
             (head (svref vec2 subscript)))
        
        (setf (_ pair next) head
              (svref vec2 subscript) pair)))

    (setf (_ hash vec) vec2)))
        
        
       

(defun set-ghash (hash key value)
  "Add KEY to HASH, associating it to VALUE. Return VALUE."
  (declare (type ghash-table hash))

  (let* ((test-fun (_ hash test-fun))
         (hash-fun (_ hash hash-fun))
         (hash-code (funcall hash-fun key))
         (vec (the simple-vector (_ hash vec)))
         (subscript (ghash-subscript hash-code vec))
         (head (svref vec subscript)))

    (loop for pair = head then (_ pair next)
       while pair do
         (when (funcall test-fun key (_ pair key))
           (return-from set-ghash (setf (_ pair value) value))))

    (setf (svref vec subscript) (new-ghash-pair-from-pool key value head))

    (when (>= (the fixnum (incf (_ hash count)))
              (length vec))
      (rehash-ghash hash))

    value))



(declaim (inline (setf get-ghash)))
(defun (setf get-ghash) (value hash key)
  "Add KEY to HASH, associating it to VALUE. Return VALUE."
  (declare (type ghash-table hash))

  (set-ghash hash key value))
  

          
(defun rem-ghash (hash key)
  "Remove KEY from HASH.
Return T if KEY was present in HASH, otherwise return NIL."
  (declare (type ghash-table hash))

  (let* ((test-fun (_ hash test-fun))
         (hash-fun (_ hash hash-fun))
         (hash-code (funcall hash-fun key))
         (vec (the simple-vector (_ hash vec)))
         (subscript (ghash-subscript hash-code vec))
         (head (svref vec subscript)))

    (loop for prev = nil then pair
       for pair = head then next
       while pair
       for next = (_ pair next)
       do
         (when (funcall test-fun key (_ pair key))
           (if prev
               (setf (_ prev next) next)
               (setf (svref vec subscript) next))
           (free-ghash-pair-to-pool pair)
           (decf (the fixnum (_ hash count)))
           (return t)))))
               

(declaim (type fixnum +ghash-threshold-capacity+))
(defconstant +ghash-threshold-capacity+ 64)

(defun clear-ghash (hash)
  "Remove all keys and values from HASH. Return HASH."
  (declare (type ghash-table hash))

  (unless (zerop (_ hash count))
    (let* ((vec (the simple-vector (_ hash vec)))
           (n (length vec)))
      (dotimes (i n)
        (let1 pair (svref vec i)
          (when pair
            (free-ghash-pairs-to-pool pair)
            (setf (svref vec i) nil)))))
    (setf (_  hash count) 0))
  hash)




(defun copy-ghash-table-into (dst src)
  "Clear DST, then copy SRC contents into it. Return NIL."
  (declare (type ghash-table src dst))
  (clear-ghash dst)
  (do-ghash (k v) src
    (set-ghash dst k v)))



(defun ghash-table-keys (src &optional to-list)
  "Return a list containing the keys in ghash-table SRC.
If TO-LIST is not nil, it will be appended to the returned list.
TO-LIST contents is not destructively modified."
  (declare (type ghash-table src)
           (type list to-list))
  (do-ghash (key) src
    (push key to-list))
  to-list)


(defun ghash-table-values (src &optional to-list)
  "Return a list containing the values in ghash-table SRC.
If TO-LIST is not nil, it will be appended to the returned list.
TO-LIST contents is not destructively modified."
  (declare (type ghash-table src)
           (type list to-list))
  (do-ghash (key value) src
    (declare (ignore key))
    (push value to-list))
  to-list)


(defun ghash-table-pairs (src &optional to-alist)
  "Return an alist containing a (key . value) pair for each entry
in ghash-table SRC.
If TO-ALIST is not nil, it will be appended to the returned alist.
TO-ALIST contents is not destructively modified."
  (declare (type ghash-table src)
           (type list to-alist))
  (do-ghash (key value) src
    (push (cons key value) to-alist))
  to-alist)
  


(defprint-object (obj ghash-pair :type t :identity nil)
  (loop for pair = obj then (_ pair next)
     while pair do
       (unless (eq pair obj)
         (format t "~&"))
       (format t "~S ~S ~S ~S"
               :key (_ pair key) :value (_ pair value))))


