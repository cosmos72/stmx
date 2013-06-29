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


;;;; ** GHASH-TABLE

(eval-always
  (declaim (type fixnum +ghash-default-capacity+ +ghash-max-capacity+))

  (defconstant +ghash-default-capacity+ 4
    "Default initial capacity of a GHASH-TABLE.")
  
  (defconstant +ghash-max-capacity+  (ash 1 (1- (integer-length most-positive-fixnum)))
    "Maximum capacity of a GHASH-TABLE.
Equal to MOST-POSITIVE-FIXNUM rounded down to nearest power of 2."))


(deftype ghash-vector       () '(or simple-vector simple-tvector))

(deftype ghash-test-fun     () '(function (t t) boolean))
(deftype ghash-hash-fun     () '(function (t)   fixnum))
(deftype ghash-aref-fun     () '(function (ghash-vector fixnum) t))
(deftype ghash-set-aref-fun () '(function (t ghash-vector fixnum) t))




(defgeneric ghash/new-pair (hash key value next)
  (:documentation "Allocate a GHASH-PAIR, initialize it with KEY, VALUE and NEXT and return it."))

(defgeneric ghash/new-vec  (hash capacity)
  (:documentation "Allocate a new GHASH-VECTOR with length = CAPACITY,
initialize all its elements to NIL and return it."))




#-(or sbcl cmucl)
(defun %setf-svref (value vec subscript)
  "On several CL implementations, (setf svref) is not a function."
  (declare (type ghash-vector vec)
           (type fixnum subscript))
  (setf (svref vec subscript) value))



(defclass ghash-table ()
  ((vec          :type (or ghash-vector tvar))
   (test-fun     :type ghash-test-fun  :initarg :test  :initform #'eql)
   (hash-fun     :type ghash-hash-fun  :initarg :hash)
   (aref-fun     :type ghash-aref-fun     :initform #'svref)
   (set-aref-fun :type ghash-set-aref-fun :initform #+(or sbcl cmucl) #'(setf svref)
                                                    #-(or sbcl cmucl) #'%setf-svref)
   (count        :type (or fixnum tvar)   :initform 0))

  (:documentation
   "Generic hash-table. Allows custom :test argument at creation - default is #'eql.
If :test is not one of #'eq #'eql or #'equal, also requires explicit :hash
argument at creation.

Not so useful by itself (standard CL:HASH-TABLE is usually faster),
it is the base for transactional hash-table implementation THASH-TABLE."))



(defmethod initialize-instance :after ((hash ghash-table) &rest other-keys &key
                                       (initial-capacity +ghash-default-capacity+))

  (declare (ignore other-keys)
           (type (integer #.+ghash-default-capacity+ #.+ghash-max-capacity+) initial-capacity))

  (unless (zerop (logand (1- initial-capacity) initial-capacity))
    ;; initial-capacity is not a power of 2: round up to nearset power of 2
    (setf initial-capacity (ash 1 (integer-length initial-capacity))))

  (unless (slot-boundp hash 'hash-fun)
    ;; provide default hash-fun when test-fun is #'eq #'eql or #'equal
    (let1 test-fun (_ hash test-fun)
      (if (or (eq test-fun #'eq)
              (eq test-fun #'eql)
              (eq test-fun #'equal))
          (setf (_ hash hash-fun) #'sxhash)
          (error "missing ~S argument, cannot instantiate ~S with custom ~S"
                 :hash (type-of hash) :test))))

  (setf (_ hash vec) (ghash/new-vec hash initial-capacity)))
        


(defun ghash-table-count (hash)
  "Return the number of KEY/VALUE entries in GHASH-TABLE hash."
  (declare (type ghash-table hash))
  (the (integer 0 #.most-positive-fixnum) (_ hash count)))

(defun ghash-table-empty? (hash)
  "Return T if GHASH-TABLE is empty, i.e. if it contains no entries."
  (declare (type ghash-table hash))
  (zerop (the fixnum (_ hash count))))



(defmacro do-ghash-pairs ((pair) hash &body body)
  "Execute BODY on each GHASH-PAIR pair contained in HASH. Return NIL."
  (with-gensyms (h vec i n left aref-fun next loop-name)
    `(let* ((,h    (the ghash-table ,hash))
            (,vec  (the ghash-vector (_ ,h vec)))
            (,n    (the fixnum (length ,vec)))
            (,left (ghash-table-count ,h))
            (,aref-fun (the function (_ ,h aref-fun))))
       (declare (type fixnum ,left))
           
       (dotimes (,i ,n)
         (when (zerop ,left)
           (return))
         (loop named ,loop-name
            for ,pair = (funcall ,aref-fun ,vec ,i) then ,next
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
  (let1 unsigned-hash-code (logand most-positive-fixnum hash-code)
    (the fixnum (logand
                 (ghash-mask vec-len)
                 (logxor unsigned-hash-code
                       (ash unsigned-hash-code -10))))))

(declaim (inline find-ghash-pair))
(defun find-ghash-pair (hash key)
  "If KEY is present in HASH, return the GHASH-PAIR containing KEY.
Otherwise return NIL."
  (declare (type ghash-table hash))

  (let* ((test-fun  (the function (_ hash test-fun)))
         (aref-fun  (the function (_ hash aref-fun)))
         (hash-fun  (the function (_ hash hash-fun)))
         (hash-code (the fixnum         (funcall hash-fun key)))
         (vec       (the ghash-vector   (_ hash vec)))

         (subscript (ghash-subscript hash-code vec))
         (head      (funcall aref-fun vec subscript)))

    ;; (the (or null ghash-pair)
      (loop for pair = head then (_ pair next)
         while pair do
           (when (funcall test-fun key (_ pair key))
             (return pair)))))

(declaim (ftype (function (#-ecl ghash-table #+ecl t t &optional t) (values t boolean)) get-ghash)
         (notinline get-ghash))

(defun get-ghash (hash key &optional default)
  "If KEY is associated to VALUE in HASH, return (values VALUE t)
Otherwise return (values DEFAULT nil)."
  (declare (type ghash-table hash))

  (let1 pair (find-ghash-pair hash key)
    (if pair
        (values (_ pair value) t)
        (values default nil))))

    
(defun rehash-ghash (hash)
  (declare (type ghash-table hash))

  (let* ((vec1 (the ghash-vector (_ hash vec)))
         (n2   (the fixnum       (ash (length vec1) 1)))
         (vec2 (the ghash-vector (ghash/new-vec hash n2)))
         (hash-fun     (the function (_ hash hash-fun)))
         (aref-fun     (the function (_ hash aref-fun)))
         (set-aref-fun (the function (_ hash set-aref-fun))))

    (do-ghash-pairs (pair) hash
      (let* ((key (_ pair key))
             (hash-code (the fixnum (funcall hash-fun key)))
             (subscript (the fixnum (ghash-subscript hash-code vec2 n2)))
             (head (funcall aref-fun vec2 subscript)))
        
        (setf (_ pair next) head)
        (funcall set-aref-fun pair vec2 subscript)))

    (setf (_ hash vec) vec2)))
        
        










       
(declaim (ftype (function (ghash-table t t) t) set-ghash)
         (notinline set-ghash))

(defun set-ghash (hash key value)
  "Add KEY to HASH, associating it to VALUE. Return VALUE."
  (declare (type ghash-table hash))

  (let* ((aref-fun     (the function     (_ hash aref-fun)))
         (set-aref-fun (the function     (_ hash set-aref-fun)))
         (hash-fun     (the function     (_ hash hash-fun)))
         (hash-code    (the fixnum       (funcall hash-fun key)))
         (vec          (the ghash-vector (_ hash vec)))
         (subscript    (ghash-subscript hash-code vec))
         (head         (funcall aref-fun vec subscript))
         (test-fun     (the function     (_ hash test-fun))))

    (loop for pair = head then (_ pair next)
       while pair do
         (when (funcall test-fun key (_ pair key))
           (return-from set-ghash (setf (_ pair value) value))))

    (let1 pair (ghash/new-pair hash key value head)
      (funcall set-aref-fun pair vec subscript))

    (let1 count (incf (the fixnum (_ hash count)))
      (when (<= (length vec) count (ash +ghash-max-capacity+ -1))
        (rehash-ghash hash)))

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

  (let* ((aref-fun     (the function     (_ hash aref-fun)))
         (set-aref-fun (the function     (_ hash set-aref-fun)))
         (hash-fun     (the function     (_ hash hash-fun)))
         (hash-code    (the fixnum       (funcall hash-fun key)))
         (vec          (the ghash-vector (_ hash vec)))
         (subscript    (ghash-subscript hash-code vec))
         (head         (funcall aref-fun vec subscript))
         (test-fun     (the function     (_ hash test-fun))))

    (loop for prev = nil then pair
       for pair = head then next
       while pair
       for next = (_ pair next)
       do
         (when (funcall test-fun key (_ pair key))
           (if prev
               (setf (_ prev next) next)
               (funcall set-aref-fun next vec subscript))
           (decf (the fixnum (_ hash count)))
           (return t)))))
               

(declaim (type fixnum +ghash-threshold-capacity+))
(defconstant +ghash-threshold-capacity+ 64)

(defun clear-ghash (hash)
  "Remove all keys and values from HASH. Return HASH."
  (declare (type ghash-table hash))

  (unless (ghash-table-empty? hash)
    (let* ((vec (the simple-vector (_ hash vec)))
           (n (length vec))
           (set-aref-fun (the function (_ hash set-aref-fun))))
      (dotimes (i n)
        (funcall set-aref-fun nil vec i)))
    (setf (_  hash count) 0))
  hash)



(defun copy-ghash (hash)
  "Return a copy of ghash-table HASH.
The copy will have the same class, test and hash functions, and elements."
  (declare (type ghash-table hash))
  (let1 copy (new (class-of hash) :test (_ hash test-fun) :hash (_ hash hash-fun))
    (do-ghash (key value) hash
      (set-ghash copy key value))
    copy))


(defun ghash-keys (src &optional to-list)
  "Return a list containing the keys in ghash-table SRC.
If TO-LIST is not nil, it will be appended to the returned list.
TO-LIST contents is not destructively modified."
  (declare (type ghash-table src)
           (type list to-list))
  (do-ghash (key) src
    (push key to-list))
  to-list)


(defun ghash-values (src &optional to-list)
  "Return a list containing the values in ghash-table SRC.
If TO-LIST is not nil, it will be appended to the returned list.
TO-LIST contents is not destructively modified."
  (declare (type ghash-table src)
           (type list to-list))
  (do-ghash-pairs (pair) src
    (push (_ pair value) to-list))
  to-list)


(defun ghash-pairs (src &optional to-alist)
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



(defmethod ghash/new-pair ((hash ghash-table) key value next)
  ;; Allocate a GHASH-PAIR, initialize it with KEY, VALUE and NEXT and return it.
  (declare (ignore hash)
           (type (or null ghash-pair) next))
  (new 'ghash-pair :key key :value value :next next))


(defmethod ghash/new-vec ((hash ghash-table) capacity)
  ;; Allocate a new GHASH-VECTOR with length = CAPACITY,
  ;; initialize all its elements to NIL and return it.
  (declare (ignore hash)
           (type fixnum capacity))

  (make-array capacity :initial-element nil))


