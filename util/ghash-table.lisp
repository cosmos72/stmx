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
   (test-fun     :type ghash-test-fun)
   (hash-fun     :type ghash-hash-fun)
   (aref-fun     :type ghash-aref-fun     :initform #'svref)
   (set-aref-fun :type ghash-set-aref-fun :initform #+(or sbcl cmucl) #'(setf svref)
                 #-(or sbcl cmucl) #'%setf-svref)
   ;; (eq count nil) means unknown count
   (count        :type (or null fixnum tvar) :initform 0)

   (test-sym     :type symbol             :initarg :test  :initform 'eql)
   (hash-sym     :type symbol             :initarg :hash  :initform nil))

  (:documentation
   "Generic hash-table. Allows custom :test argument at creation - default is 'eql.
If :test is not one of 'eq 'eql or 'equal, also requires explicit :hash
argument at creation.

Not so useful by itself (standard CL:HASH-TABLE is usually faster),
it is the base for transactional hash-table implementation THASH-TABLE."))



(defmethod initialize-instance :after ((hash ghash-table) &rest other-keys &key
                                       (initial-capacity +ghash-default-capacity+))

  (declare (ignore other-keys)
           (type (and fixnum (integer 0)) initial-capacity))

  (cond
    ((< initial-capacity +ghash-default-capacity+)
     (setf initial-capacity +ghash-default-capacity+))
    ((> initial-capacity +ghash-max-capacity+)
     (setf initial-capacity +ghash-max-capacity+))
    (t
     (unless (zerop (logand (1- initial-capacity) initial-capacity))
       ;; initial-capacity is not a power of 2: round up to nearest power of 2
       (setf initial-capacity (ash 1 (integer-length initial-capacity))))))

  (with-rw-slots (test-sym hash-sym) hash
    (check-type test-sym symbol)
    (check-type hash-sym symbol)

    (unless hash-sym
      ;; provide default hash-fun when test-fun is one of: 'eq 'eql 'equal or 'equalp
      (cond
        ((or (eq test-sym 'eq) (eq test-sym 'eql) (eq test-sym 'equal))
         (setf hash-sym 'sxhash))

        ((eq test-sym 'equalp)
         (setf hash-sym 'sxhash-equalp))

        (t
         (error "missing ~S argument, cannot instantiate ~S with custom ~S ~S"
                :hash (type-of hash) :test test-sym))))
    
    ;; convert :test and :hash from symbols to actual functions
    (setf (_ hash test-fun) (fdefinition test-sym)
          (_ hash hash-fun) (fdefinition hash-sym)
          ;; allocate internal vector
          (_ hash vec) (ghash/new-vec hash initial-capacity))))
        

(defun ghash-table-test (hash)
  "Return the symbol used by ghash-table HASH to compare keys."
  (declare (type ghash-table hash))
  (the symbol (_ hash test-sym)))


(defun ghash-table-hash (hash)
  "Return the symbol used by ghash-table HASH to hash keys."
  (declare (type ghash-table hash))
  (the symbol (_ hash hash-sym)))



(defmacro do-ghash-pairs ((pair &optional index) hash &body body)
  "Execute BODY on each GHASH-PAIR pair contained in HASH. Return NIL."
  (with-gensyms (h vec vlen i count n end aref-fun next outer-loop inner-loop)
    (let ((count (or index count)))
      `(let* ((,h    (the ghash-table ,hash))
              (,vec  (the ghash-vector (_ ,h vec)))
              (,vlen (the fixnum (length ,vec)))
              (,count 0)
              (,n    (_ ,h count)) ;; NIL if unknown count
              (,end  (or ,n most-positive-fixnum))
              (,aref-fun (the function (_ ,h aref-fun))))
         (declare (type fixnum ,count ,end))

         (block nil
           (loop named ,outer-loop
              for ,i below ,vlen
              while (< ,count ,end)
              do
                (loop named ,inner-loop
                   for ,pair = (funcall ,aref-fun ,vec ,i) then ,next
                   with ,next = nil
                   while ,pair
                   do
                     (setf ,next (_ ,pair next))
                     (incf (the fixnum ,count))
                     (locally ,@body))) ;; (return) will exit from (block nil) above

           ;; we iterated on the whole hash table,
           ;; so any chance of concurrent modification is lost.
           ;; we may as well set the actual COUNT
           (unless (and ,n (= ,count ,n))
             (setf (_ ,h count) ,count))
           nil)))))
                

(defmacro do-ghash ((key &optional value index) hash &body body)
  "Execute BODY on each KEY/VALUE contained in HASH. Return NIL."
  (with-gensym pair
    `(do-ghash-pairs (,pair ,index) ,hash
       (let ((,key (_ ,pair key))
             ,@(when value `((,value (_ ,pair value)))))
         ,@body))))
                

(defun ghash-table-count (hash)
  "Return the number of KEY/VALUE entries in ghash-table HASH."
  (declare (type ghash-table hash))
  (when-bind n (_ hash count)
    (return-from ghash-table-count (the fixnum n)))

  ;; (do-ghash-pairs) computes (_ hash count) unless body exits early
  (do-ghash-pairs (pair) hash)
  (the fixnum (_ hash count)))
          

(defun ghash-table-count> (hash count)
  "Return T if ghash-table HASH contains more than COUNT key/value entries."
  (declare (type ghash-table hash)
           (type fixnum count))
  (when (< count 0)
    (return-from ghash-table-count> t))
  (when-bind n (_ hash count)
    (return-from ghash-table-count> (> (the fixnum n) count)))
  
  (do-ghash-pairs (pair index) hash
    (when (> index count)
      (return-from ghash-table-count> t)))
  nil)
          
(declaim (inline ghash-table-count<=))
(defun ghash-table-count<= (hash count)
  "Return T if ghash-table HASH contains at most COUNT key/value entries."
  (not (ghash-table-count> hash count)))


(declaim (inline ghash-table-empty?))
(defun ghash-table-empty? (hash)
  "Return T if GHASH-TABLE is empty, i.e. if it contains no entries."
  (declare (type ghash-table hash))
  (ghash-table-count<= hash 0))



(declaim (inline ghash-mask))
(defun ghash-mask (vec-len)
  "Return the bitmask to use for hash indexes."
  (declare (type fixnum vec-len))
  (the fixnum (1- vec-len)))

(declaim (inline ghash-subscript))
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
         (test-fun     (the function     (_ hash test-fun)))
         (bucket-len   0))

    (loop for pair = head then (_ pair next)
       while pair do
         (incf (the fixnum bucket-len))
         (when (funcall test-fun key (_ pair key))
           (return-from set-ghash (setf (_ pair value) value))))

    (let ((pair (ghash/new-pair hash key value head))
          (count-or-nil (_ hash count)))

      (funcall set-aref-fun pair vec subscript)
      ;; do not update COUNT, just set to NIL *before* calling other functions
      (setf (_ hash count) nil)

      (flet ((%need-rehash ()
               (let ((vlen (length vec)))
                 (cond
                   ;; capacity already maxed out?
                   ((> vlen (ash +ghash-max-capacity+ -1))
                    nil)
                   (count-or-nil
                    (> (the fixnum count-or-nil) vlen))
                   (t
                    (and (> bucket-len 2)
                         (ghash-table-count> hash vlen)))))))
        
        ;; rehash-ghash sets (_ hash count)
        (when (%need-rehash)
          (rehash-ghash hash)))))
  value)


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
       with next = nil
       while pair
       do
         (setf next (_ pair next))
         (when (funcall test-fun key (_ pair key))
           (if prev
               (setf (_ prev next) next)
               (funcall set-aref-fun next vec subscript))
           ;; do not update COUNT. just set it to NIL, i.e. "unknown"
           (setf (_ hash count) nil)
           (return t)))))
               

(declaim (type fixnum +ghash-threshold-capacity+))
(defconstant +ghash-threshold-capacity+ 64)

(defun clear-ghash (hash)
  "Remove all keys and values from HASH. Return HASH."
  (declare (type ghash-table hash))

  (let ((count (_ hash count)))
    (unless (and count (zerop count))
      (let* ((vec (the simple-vector (_ hash vec)))
             (n (length vec))
             (set-aref-fun (the function (_ hash set-aref-fun))))
        (dotimes (i n)
          (funcall set-aref-fun nil vec i)))
      (setf (_  hash count) 0)))
  hash)



(defun copy-ghash (hash)
  "Return a copy of ghash-table HASH.
The copy will have the same class, test and hash functions, and elements."
  (declare (type ghash-table hash))
  (let1 copy (new (class-of hash) :test (_ hash test-sym) :hash (_ hash hash-sym))
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
  

(defprint-object (obj ghash-table)
  (format t "~S ~S ~S ~S ~S ~S" :count (ghash-table-count obj)
          :test (ghash-table-test obj) :hash (ghash-table-hash obj)))


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


