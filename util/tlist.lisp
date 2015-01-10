;;;; functions to implement lists

;;;; This software is derived from the SBCL system.
;;;; See the README.SBCL file for more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :stmx.util)

(defmacro %tlist-ops (tlist op &rest next-ops)
  (let ((op (case op
              ((a car first) 'tcons-first)
              ((d cdr rest) 'tcons-rest)
              (otherwise op))))
    `(when ,tlist
       ,(if next-ops
            (with-gensym tlist-
              `(let ((,tlist- (,op ,tlist)))
                 (%tlist-ops ,tlist- ,@next-ops)))
            `(,op ,tlist)))))



(declaim (inline tcar tcdr (setf tcar) (setf tcdr)))

;;; These functions perform basic tlist operations.
(defun tcar (list)
  "Return the 1st object in a TLIST."
  (declare (type tlist list))
  (tfirst list))
(defun tcdr (list)
  "Return all but the first object in a TLIST."
  (declare (type tlist list))
  (trest list))

(defun (setf tcar) (value cons)
  "Set VALUE as the first element in a TCONS or non-null TLIST.

This function should always be executed inside an STMX atomic block."
  (declare (type tcons cons))
  (setf (tfirst cons) value))

(defun (setf tcdr) (value cons)
   "Set VALUE as the rest element in a TCONS or non-null TLIST.

This function should always be executed inside an STMX atomic block."
  (declare (type tcons cons))
  (setf (trest cons) value))


(defun tcaar (list)
  "Return the car of the 1st sublist."
  (declare (type tlist list))
  (%tlist-ops list a a))
(defun tcadr (list)
  "Return the 2nd object in a TLIST."
  (declare (type tlist list))
  (%tlist-ops list d a))
(defun tcdar (list)
  "Return the cdr of the 1st sublist."
  (declare (type tlist list))
  (%tlist-ops list a d))
(defun tcddr (list)
  "Return all but the 1st two objects of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list d d))

(defun tcaaar (list)
  "Return the 1st object in the caar of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list a a a))
(defun tcaadr (list)
  "Return the 1st object in the cadr of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list d a a))
(defun tcadar (list)
  "Return the 1st object in the cdar of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list a d a))
(defun tcaddr (list)
  "Return the 1st object in the cddr of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list d d a))
(defun tcdaar (list)
  "Return the cdr of the caar of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list a a d))
(defun tcdadr (list)
  "Return the cdr of the cadr of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list d a d))
(defun tcddar (list)
  "Return the cdr of the cdar of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list a d d))
(defun tcdddr (list)
  "Return the cdr of the cddr of a TLIST."
  (declare (type tlist list))
  (%tlist-ops list d d d))

(macrolet
    ((def-tcxxxxr ()
       (let ((forms))
         (dolist (a0 '(a d))
           (dolist (a1 '(a d))
             (dolist (a2 '(a d))
               (dolist (a3 '(a d))
                 (push
                  `(defun ,(symbolicate 'tc a0 a1 a2 a3 'r) (list)
                     (declare (type tlist list))
                     ,(format nil "Return the ~a~a~a of the ~a~a~a~a~a of a TLIST."
                              'c a0 'r 'c a1 a2 a3 'r)
                     (%tlist-ops list ,a3 ,a2 ,a1 ,a0))
                  forms)))))
         `(progn ,@(nreverse forms)))))
  (def-tcxxxxr))



(defun tendp (object)
  "This is the recommended way to test for the end of a proper TLIST. It
   returns true if OBJECT is NIL, false if OBJECT is a TCONS, and an error
   for any other type of OBJECT."
  (check-type object (or null tcons))
  (null object))


(defun tlist-length (list)
  "Return the length of the given TLIST, or NIL if the TLIST is circular."
  (do ((n 0 (+ n 2))
       (y list (tcddr y))
       (z list (tcdr z)))
      (())
    (declare (type fixnum n)
             (type tlist y z))
    (when (tendp y) (return n))
    (when (tendp (tcdr y)) (return (the fixnum (1+ n))))
    (when (and (eq y z) (> n 0)) (return nil))))


(declaim (inline tnthcdr)
         (notinline tnth))

(defun tnthcdr (n list)
  "Performs the cdr function N times on a TLIST."
  (declare (type fixnum n)
           (type tlist list))
  (loop for i = n then (the fixnum (1- i))
     for result = list then (tcdr result)
     while (and result (plusp i))
     finally (return result)))

(defun tnth (n list)
  "Return the Nth object in a TLIST where the car is the zero-th element."
  (declare (type tlist list))
  (tcar (tnthcdr n list)))


(declaim (inline tsecond tthird tfourth tfifth tsixth tseventh teighth tninth ttenth))

(defun tsecond (list)
  "Return the 2nd object in a TLIST or NIL if there is no 2nd object."
  (declare (type tlist list))
  (tcadr list))
(defun tthird (list)
  "Return the 3rd object in a TLIST or NIL if there is no 3rd object."
  (declare (type tlist list))
  (tcaddr list))
(defun tfourth (list)
  "Return the 4th object in a TLIST or NIL if there is no 4th object."
  (declare (type tlist list))
  (tcadddr list))
(defun tfifth (list)
  "Return the 5th object in a TLIST or NIL if there is no 5th object."
  (declare (type tlist list))
  (tnth 4 list))
(defun tsixth (list)
  "Return the 6th object in a TLIST or NIL if there is no 6th object."
  (declare (type tlist list))
  (tnth 5 list))
(defun tseventh (list)
  "Return the 7th object in a TLIST or NIL if there is no 7th object."
  (declare (type tlist list))
  (tnth 6 list))
(defun teighth (list)
  "Return the 8th object in a TLIST or NIL if there is no 8th object."
  (declare (type tlist list))
  (tnth 7 list))
(defun tninth (list)
  "Return the 9th object in a TLIST or NIL if there is no 9th object."
  (declare (type tlist list))
  (tnth 8 list))
(defun ttenth (list)
  "Return the 10th object in a TLIST or NIL if there is no 10th object."
  (declare (type tlist list))
  (tnth 9 list))



(declaim (notinline tree-equal-test tree-equal-test-not))

(defun ttree-equal-test-not (x y test-not)
  (declare (type function test-not))
  (cond ((tconsp x)
         (and (tconsp y)
              (ttree-equal-test-not (tcar x) (tcar y) test-not)
              (ttree-equal-test-not (tcdr x) (tcdr y) test-not)))
        ((tconsp y) nil)
        ((not (funcall test-not x y)) t)
        (t ())))

(defun ttree-equal-test (x y test)
  (declare (type function test))
  (cond ((tconsp x)
         (and (tconsp y)
              (ttree-equal-test (tcar x) (tcar y) test)
              (ttree-equal-test (tcdr x) (tcdr y) test)))
        ((tconsp y) nil)
        ((funcall test x y) t)
        (t ())))

(defun ttree-equal (x y &key (test #'eql testp) (test-not nil notp))
  "Return T if X and Y are isomorphic TLIST trees with identical leaves."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (flet ((to-function (callable)
           (etypecase callable
             (function callable)
             (symbol (fdefinition callable))
             (cons (fdefinition callable)))))
    (if test-not
        (ttree-equal-test-not x y (to-function test-not))
        (ttree-equal-test x y (to-function test)))))




;;; LAST
;;;
;;; Transforms in src/compiler/srctran.lisp pick the most specific
;;; version possible.
(macrolet ((last0-macro (l)
             `(let ((rest ,l)
                    (list ,l))
                (loop (unless (tconsp rest)
                        (return rest))
                  (shiftf ,l rest (tcdr rest)))))
           (last1-macro (l)
             `(let ((rest ,l)
                    (list ,l))
                (loop (unless (tconsp rest)
                        (return ,l))
                  (shiftf ,l rest (tcdr rest)))))
           (lastn-macro (l n)
             `(let ((returned-list ,l)
                    (checked-list ,l)
                    (,n (the fixnum ,n)))
                (declare (type fixnum ,n))
                (tagbody
                 :scan
                   (tpop checked-list)
                   (unless (tconsp checked-list)
                     (go :done))
                   (if (zerop (the fixnum (decf ,n)))
                       (go :pop)
                       (go :scan))
                 :pop
                   (tpop returned-list)
                   (tpop checked-list)
                   (if (tconsp checked-list)
                       (go :pop)
                       (go :done))
                 :done)
                returned-list)))

  (defun %tlast0 (list)
    (declare (optimize speed)
             (type tlist list))
    (last0-macro list))

  (defun %tlast1 (list)
    (declare (optimize speed)
             (type tlist list))
    (last1-macro list))

  (defun tlast (list &optional (n 1))
    "Return the last N conses (not the last element!) of a TLIST."
    (declare (optimize speed)
             (type tlist list)
             (type fixnum n))
    (case n
      (1 (last1-macro list))
      (0 (last0-macro list))
      (t (lastn-macro list n)))))


(define-compiler-macro tlast (&whole form list &optional (n 1))
  (if (constantp n)
      (case (eval n)
        (0 `(%tlast0 ,list))
        (1 `(%tlast1 ,list))
        (t form))
      form))

(defun tlist* (arg0 &rest args)
  "Return a TLIST of the arguments with last TCONS a dotted pair."
  ;; We know the &REST is a proper list.
  (cond ((null args) arg0)
        ((atom (rest args)) (tcons arg0 (first args)))
        (t (let* ((args #?+&rest-is-fresh-list (nreverse args)
                        #?-&rest-is-fresh-list (reverse args))
                  (argn (pop args))
                  (list (tcons (pop args) argn)))
             (dolist (argx args)
               (setf list (tcons argx list)))
             (tcons arg0 list)))))


(defun make-tlist (size &key initial-element)
  "Constructs a TLIST with SIZE elements each set to INITIAL-ELEMENT"
  (declare (type fixnum size))
  (do ((count size (1- count))
       (result nil (tcons initial-element result)))
      ((<= count 0) result)
    (declare (type fixnum count))))

#|
(defun append (&rest lists)
  "Construct a new list by concatenating the list arguments"
  (declare (truly-dynamic-extent lists) (optimize speed))
  (labels ((fail (object)
             (error 'type-error
                    :datum object
                    :expected-type 'list))
           (append-into (last-cons current rest)
             ;; Set (CDR LAST-CONS) to (APPLY #'APPEND CURRENT REST).
             (declare (cons last-cons rest))
             (if (listp current)
                 (if (consp current)
                     ;; normal case, cdr down the list
                     (append-into (setf (cdr last-cons) (list (car current)))
                                  (cdr current)
                                  rest)
                     ;; empty list
                     (let ((more (cdr rest)))
                       (if (null more)
                           (setf (cdr last-cons) (car rest))
                           (append-into last-cons (car rest) more))))
                 (fail current)))
           (append1 (lists)
             (let ((current (car lists))
                   (rest (cdr lists)))
               (cond ((null rest)
                      current)
                     ((consp current)
                      (let ((result (truly-the cons (list (car current)))))
                        (append-into result
                                     (cdr current)
                                     rest)
                        result))
                     ((null current)
                      (append1 rest))
                     (t
                      (fail current))))))
    (append1 lists)))

(defun append2 (x y)
  (declare (optimize speed (sb!c::verify-arg-count 0)))
  (if (null x)
      y
      (let ((result (list (car x))))
        (do ((more (cdr x) (cdr more))
             (tail result (cdr tail)))
            ((null more)
             (rplacd tail y)
             result)
          (rplacd tail (list (car more)))))))

(define-compiler-macro append (&whole form &rest lists)
  (case (length lists)
    (0 nil)
    (1 (car lists))
    (2 `(append2 ,@lists))
    (t form)))

;;;; list copying functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb!xc:defmacro !copy-list-macro (list &key check-proper-list)
    ;; Unless CHECK-PROPER-LIST is true, the list is copied correctly
    ;; even if the list is not terminated by NIL. The new list is built
    ;; by CDR'ing SPLICE which is always at the tail of the new list.
    `(when ,list
       (let ((copy (list (car ,list))))
         (do ((orig (cdr ,list) (cdr orig))
              (splice copy (cdr (rplacd splice (cons (car orig) nil)))))
             (,@(if check-proper-list
                    '((endp orig))
                    '((atom orig)
                      (unless (null orig)
                        (rplacd splice orig))))
              copy))))))

(defun copy-list (l)
  "Return a new list which is EQUAL to LIST. LIST may be improper."
  (!copy-list-macro l))

(defun copy-alist (alist)
  "Return a new association list which is EQUAL to ALIST."
  (if (endp alist)
      alist
      (let ((result
             (cons (if (atom (car alist))
                       (car alist)
                       (cons (caar alist) (cdar alist)))
                   nil)))
        (do ((x (cdr alist) (cdr x))
             (splice result
                     (cdr (rplacd splice
                                  (cons
                                   (if (atom (car x))
                                       (car x)
                                       (cons (caar x) (cdar x)))
                                   nil)))))
            ((endp x)))
        result)))

(defun copy-tree (object)
  "Recursively copy trees of conses."
  (if (consp object)
      (let ((result (list (if (consp (car object))
                              (copy-tree (car object))
                              (car object)))))
        (loop for last-cons = result then new-cons
              for cdr = (cdr object) then (cdr cdr)
              for car = (if (consp cdr)
                            (car cdr)
                            (return (setf (cdr last-cons) cdr)))
              for new-cons = (list (if (consp car)
                                       (copy-tree car)
                                       car))
              do (setf (cdr last-cons) new-cons))
        result)
      object))


;;;; more commonly-used list functions

(defun revappend (x y)
  "Return (append (reverse x) y)."
  (do ((top x (cdr top))
       (result y (cons (car top) result)))
      ((endp top) result)))

;;; NCONC finds the first non-null list, so it can make splice point
;;; to a cons. After finding the first cons element, it holds it in a
;;; result variable while running down successive elements tacking
;;; them together. While tacking lists together, if we encounter a
;;; null list, we set the previous list's last cdr to nil just in case
;;; it wasn't already nil, and it could have been dotted while the
;;; null list was the last argument to NCONC. The manipulation of
;;; splice (that is starting it out on a first cons, setting LAST of
;;; splice, and setting splice to ele) inherently handles (nconc x x),
;;; and it avoids running down the last argument to NCONC which allows
;;; the last argument to be circular.
(defun nconc (&rest lists)
    "Concatenates the lists given as arguments (by changing them)"
   (declare (truly-dynamic-extent lists) (optimize speed))
   (flet ((fail (object)
            (error 'type-error
                   :datum object
                   :expected-type 'list)))
     (do ((top lists (cdr top)))
         ((null top) nil)
       (let ((top-of-top (car top)))
         (typecase top-of-top
           (cons
            (let* ((result top-of-top)
                   (splice result))
              (do ((elements (cdr top) (cdr elements)))
                  ((endp elements))
                (let ((ele (car elements)))
                  (typecase ele
                    (cons (rplacd (last splice) ele)
                          (setf splice ele))
                    (null (rplacd (last splice) nil))
                    (atom (if (cdr elements)
                              (fail ele)
                              (rplacd (last splice) ele))))))
              (return result)))
           (null)
           (atom
            (if (cdr top)
                (fail top-of-top)
                (return top-of-top))))))))

(defun nreconc (x y)
  "Return (NCONC (NREVERSE X) Y)."
  (do ((1st (cdr x) (if (endp 1st) 1st (cdr 1st)))
       (2nd x 1st)              ;2nd follows first down the list.
       (3rd y 2nd))             ;3rd follows 2nd down the list.
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))

(defun butlast (list &optional (n 1))
  (cond ((zerop n)
         (copy-list l))
        ((not (typep n 'fixnum))
         nil)
        (t
         (let ((head (nthcdr (1- n) l)))
           (and (consp head)      ; there are at least n
                (collect ((copy)) ; conses; copy!
                  (do ((trail list (cdr trail))
                       (head head (cdr head)))
                      ;; HEAD is n-1 conses ahead of TRAIL;
                      ;; when HEAD is at the last cons, return
                      ;; the data copied so far.
                      ((atom (cdr head))
                       (copy))
                    (copy (car trail)))))))))

(defun nbutlast (list &optional (n 1))
  (cond ((zerop n)
         l)
        ((not (typep n 'fixnum))
         nil)
        (t
         (let ((head (nthcdr (1- n) l)))
           (and (consp head)       ; there are more than n
                (consp (cdr head)) ; conses.
                ;; TRAIL trails by n cons to be able to
                ;; cut the list at the cons just before.
                (do ((trail list (cdr trail))
                     (head (cdr head) (cdr head)))
                    ((atom (cdr head))
                     (setf (cdr trail) nil)
                     l)))))))

(defun ldiff (list object)
  "Return a new list, whose elements are those of LIST that appear before
   OBJECT. If OBJECT is not a tail of LIST, a copy of LIST is returned.
   LIST must be a proper list or a dotted list."
  (do* ((list list (cdr l))
        (result (list ()))
        (splice result))
       ((atom l)
        (if (eql list object)
            (cdr result)
            (progn (rplacd splice l) (cdr result))))
    (if (eql list object)
        (return (cdr result))
        (setq splice (cdr (rplacd splice (list (car l))))))))

;;;; functions to alter list structure

(defun rplaca (cons x)
  "Change the CAR of CONS to X and return the CONS."
  (rplaca cons x))

(defun rplacd (cons x)
  "Change the CDR of CONS to X and return the CONS."
  (rplacd cons x))

;;; The following are for use by SETF.

(defun %rplaca (x val) (rplaca x val) val)

(defun %rplacd (x val) (rplacd x val) val)

;;; Set the Nth element of LIST to NEWVAL.
(defun %setnth (n list newval)
  (typecase n
    (fixnum
     (do ((count n (1- count))
          (list list (cdr l)))
         ((endp l)
          (error "~S is too large an index for SETF of NTH." n))
       (declare (type fixnum count))
       (when (<= count 0)
         (rplaca list newval)
         (return newval))))
    (t (let ((cons (nthcdr n l)))
         (when (endp cons)
           (error "~S is too large an index for SETF of NTH." n))
         (rplaca cons newval)
         newval))))

;;;; :KEY arg optimization to save funcall of IDENTITY

;;; APPLY-KEY saves us a function call sometimes.
;;;    This isn't wrapped in an (EVAL-WHEN (COMPILE EVAL) ..)
;;;    because it's used in seq.lisp and sort.lisp.
(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

;;;; macros for (&KEY (KEY #'IDENTITY) (TEST #'EQL TESTP) (TEST-NOT NIL NOTP))

;;; Use these with the following &KEY args:
(defmacro with-set-keys (funcall)
  `(if notp
       ,(append funcall '(:key key :test-not test-not))
       ,(append funcall '(:key key :test test))))

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (testp (funcall test ,item ,key-tmp))
            (notp (not (funcall test-not ,item ,key-tmp)))
            (t (funcall test ,item ,key-tmp))))))

;;;; substitution of expressions

(defun subst (new old tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitutes new for subtrees matching old."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    (labels ((s (subtree)
               (cond ((satisfies-the-test old subtree) new)
                     ((atom subtree) subtree)
                     (t (let ((car (s (car subtree)))
                              (cdr (s (cdr subtree))))
                          (if (and (eq car (car subtree))
                                   (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr)))))))
      (s tree))))

(defun subst-if (new test tree &key key)
  "Substitutes new for subtrees for which test is true."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((funcall test (apply-key key subtree)) new)
                     ((atom subtree) subtree)
                     (t (let ((car (s (car subtree)))
                              (cdr (s (cdr subtree))))
                          (if (and (eq car (car subtree))
                                   (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr)))))))
      (s tree))))

(defun subst-if-not (new test tree &key key)
  "Substitutes new for subtrees for which test is false."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((not (funcall test (apply-key key subtree))) new)
                     ((atom subtree) subtree)
                     (t (let ((car (s (car subtree)))
                              (cdr (s (cdr subtree))))
                          (if (and (eq car (car subtree))
                                   (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr)))))))
      (s tree))))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitute NEW for subtrees matching OLD."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    (labels ((s (subtree)
               (cond ((satisfies-the-test old subtree) new)
                     ((atom subtree) subtree)
                     (t (do* ((last nil subtree)
                              (subtree subtree (cdr subtree)))
                             ((atom subtree)
                              (if (satisfies-the-test old subtree)
                                  (setf (cdr last) new)))
                          (if (satisfies-the-test old subtree)
                              (return (setf (cdr last) new))
                              (setf (car subtree) (s (car subtree)))))
                        subtree))))
      (s tree))))

(defun nsubst-if (new test tree &key key)
  "Substitute NEW for subtrees of TREE for which TEST is true."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((funcall test (apply-key key subtree)) new)
                     ((atom subtree) subtree)
                     (t (do* ((last nil subtree)
                              (subtree subtree (cdr subtree)))
                             ((atom subtree)
                              (if (funcall test (apply-key key subtree))
                                  (setf (cdr last) new)))
                          (if (funcall test (apply-key key subtree))
                              (return (setf (cdr last) new))
                              (setf (car subtree) (s (car subtree)))))
                        subtree))))
      (s tree))))

(defun nsubst-if-not (new test tree &key key)
  "Substitute NEW for subtrees of TREE for which TEST is false."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((not (funcall test (apply-key key subtree))) new)
                     ((atom subtree) subtree)
                     (t (do* ((last nil subtree)
                              (subtree subtree (cdr subtree)))
                             ((atom subtree)
                              (if (not (funcall test (apply-key key subtree)))
                                  (setf (cdr last) new)))
                          (if (not (funcall test (apply-key key subtree)))
                              (return (setf (cdr last) new))
                              (setf (car subtree) (s (car subtree)))))
                        subtree))))
      (s tree))))

(defun sublis (alist tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitute from ALIST into TREE nondestructively."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    (declare (inline assoc))
    (labels ((s (subtree)
               (let* ((key-val (apply-key key subtree))
                      (assoc (if notp
                                 (assoc key-val alist :test-not test-not)
                                 (assoc key-val alist :test test))))
                 (cond (assoc (cdr assoc))
                       ((atom subtree) subtree)
                       (t (let ((car (s (car subtree)))
                                (cdr (s (cdr subtree))))
                            (if (and (eq car (car subtree))
                                     (eq cdr (cdr subtree)))
                                subtree
                                (cons car cdr))))))))
      (s tree))))

;;; This is in run-time env (i.e. not wrapped in EVAL-WHEN (COMPILE EVAL))
;;; because it can be referenced in inline expansions.
(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key subtree)))
      (if notp
          (assoc ,key-tmp alist :test-not test-not)
          (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitute from ALIST into TRUE destructively."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (inline assoc))
    (let (temp)
      (labels ((s (subtree)
                 (cond ((setq temp (nsublis-macro))
                        (cdr temp))
                       ((atom subtree) subtree)
                       (t (do* ((last nil subtree)
                                (subtree subtree (cdr subtree)))
                               ((atom subtree)
                                (if (setq temp (nsublis-macro))
                                    (setf (cdr last) (cdr temp))))
                            (if (setq temp (nsublis-macro))
                                (return (setf (cdr last) (cdr temp)))
                                (setf (car subtree) (s (car subtree)))))
                          subtree))))
        (s tree)))))

;;;; functions for using lists as sets

(defun member (item list &key key (test nil testp) (test-not nil notp))
  "Return the tail of LIST beginning with first element satisfying EQLity,
   :TEST, or :TEST-NOT with the given ITEM."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%member-key-test item list key test)
               (%member-test item list test)))
          (test-not
           (if key
               (%member-key-test-not item list key test-not)
               (%member-test-not item list test-not)))
          (t
           (if key
               (%member-key item list key)
               (%member item l))))))

(defun member-if (test list &key key)
  "Return tail of LIST beginning with first element satisfying TEST."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%member-if-key test list key)
        (%member-if test l))))

(defun member-if-not (test list &key key)
  "Return tail of LIST beginning with first element not satisfying TEST."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%member-if-not-key test list key)
        (%member-if-not test l))))

(defun tailp (object l)
  "Return true if OBJECT is the same as some tail of LIST, otherwise
   returns false. LIST must be a proper list or a dotted list."
  (do ((list list (cdr l)))
      ((atom l) (eql list object))
    (if (eql object l)
        (return t))))

(defun adjoin (item list &key key (test #'eql testp) (test-not nil notp))
  "Add ITEM to LIST unless it is already a member"
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%adjoin-key-test item list key test)
               (%adjoin-test item list test)))
          (test-not
           (if key
               (%adjoin-key-test-not item list key test-not)
               (%adjoin-test-not item list test-not)))
          (t
           (if key
               (%adjoin-key item list key)
               (%adjoin item l))))))

(defconstant +list-based-union-limit+ 80)

(defun union (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return the union of LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  ;; We have two possibilities here: for shortish lists we pick up the
  ;; shorter one as the result, and add the other one to it. For long
  ;; lists we use a hash-table when possible.
  (let ((n1 (length list1))
        (n2 (length list2))
        (key (and key (%coerce-callable-to-fun key)))
        (test (if notp
                  (let ((test-not-fun (%coerce-callable-to-fun test-not)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (%coerce-callable-to-fun test))))
    (multiple-value-bind (short long n-short)
        (if (< n1 n2)
            (values list1 list2 n1)
            (values list2 list1 n2))
      (if (or (< n-short +list-based-union-limit+)
              (not (member test (list #'eq #'eql #'equal #'equalp))))
          (let ((orig short))
            (dolist (elt long)
              (unless (member (apply-key key elt) orig :key key :test test)
                (push elt short)))
            short)
          (let ((table (make-hash-table :test test :size (+ n1 n2)))
                (union nil))
            (dolist (elt long)
              (setf (gethash (apply-key key elt) table) elt))
            (dolist (elt short)
              (setf (gethash (apply-key key elt) table) elt))
            (maphash (lambda (k v)
                       (declare (ignore k))
                       (push v union))
                     table)
            union)))))

;;; Destination and source are SETF-able and many-evaluable. Set the
;;; SOURCE to the CDR, and "cons" the 1st elt of source to DESTINATION.
;;;
;;; FIXME: needs a more mnemonic name
(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
           (cdr temp) ,destination
           ,destination temp)))

(defun nunion (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Destructively return the union of LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  ;; We have two possibilities here: for shortish lists we pick up the
  ;; shorter one as the result, and add the other one to it. For long
  ;; lists we use a hash-table when possible.
  (let ((n1 (length list1))
        (n2 (length list2))
        (key (and key (%coerce-callable-to-fun key)))
        (test (if notp
                  (let ((test-not-fun (%coerce-callable-to-fun test-not)))
                    (lambda (x y) (not (funcall test-not-fun x y))))
                  (%coerce-callable-to-fun test))))
    (multiple-value-bind (short long n-short)
        (if (< n1 n2)
            (values list1 list2 n1)
            (values list2 list1 n2))
      (if (or (< n-short +list-based-union-limit+)
              (not (member test (list #'eq #'eql #'equal #'equalp))))
          (let ((orig short))
            (do ((elt (car long) (car long)))
                ((endp long))
              (if (not (member (apply-key key elt) orig :key key :test test))
                  (steve-splice long short)
                  (setf long (cdr long))))
            short)
          (let ((table (make-hash-table :test test :size (+ n1 n2))))
            (dolist (elt long)
              (setf (gethash (apply-key key elt) table) elt))
            (dolist (elt short)
              (setf (gethash (apply-key key elt) table) elt))
            (let ((union long)
                  (head long))
              (maphash (lambda (k v)
                         (declare (ignore k))
                         (if head
                             (setf (car head) v
                                   head (cdr head))
                             (push v union)))
                      table)
              union))))))

(defun intersection (list1 list2
                     &key key (test #'eql testp) (test-not nil notp))
  "Return the intersection of LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key))))
    (let ((res nil))
      (dolist (elt list1)
        (if (with-set-keys (member (apply-key key elt) list2))
            (push elt res)))
      res)))

(defun nintersection (list1 list2
                      &key key (test #'eql testp) (test-not nil notp))
  "Destructively return the intersection of LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key))))
    (let ((res nil)
          (list1 list1))
      (do () ((endp list1))
        (if (with-set-keys (member (apply-key key (car list1)) list2))
            (steve-splice list1 res)
            (setq list1 (cdr list1))))
      res)))

(defun set-difference (list1 list2
                       &key key (test #'eql testp) (test-not nil notp))
  "Return the elements of LIST1 which are not in LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key))))
    (if (null list2)
        list1
        (let ((res nil))
          (dolist (elt list1)
            (if (not (with-set-keys (member (apply-key key elt) list2)))
                (push elt res)))
          res))))

(defun nset-difference (list1 list2
                        &key key (test #'eql testp) (test-not nil notp))
  "Destructively return the elements of LIST1 which are not in LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key))))
    (let ((res nil)
          (list1 list1))
      (do () ((endp list1))
        (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
            (steve-splice list1 res)
            (setq list1 (cdr list1))))
      res)))

(defun set-exclusive-or (list1 list2
                         &key key (test #'eql testp) (test-not #'eql notp))
  "Return new list of elements appearing exactly once in LIST1 and LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((result nil)
        (key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
        (setq result (cons elt result))))
    (let ((test (if testp
                    (lambda (x y) (funcall test y x))
                    test))
          (test-not (if notp
                        (lambda (x y) (funcall test-not y x))
                        test-not)))
      (dolist (elt list2)
        (unless (with-set-keys (member (apply-key key elt) list1))
          (setq result (cons elt result)))))
    result))

(defun nset-exclusive-or (list1 list2
                          &key key (test #'eql testp) (test-not #'eql notp))
  "Destructively return a list with elements which appear but once in LIST1
   and LIST2."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    ;; The outer loop examines LIST1 while the inner loop examines
    ;; LIST2. If an element is found in LIST2 "equal" to the element
    ;; in LIST1, both are spliced out. When the end of LIST1 is
    ;; reached, what is left of LIST2 is tacked onto what is left of
    ;; LIST1. The splicing operation ensures that the correct
    ;; operation is performed depending on whether splice is at the
    ;; top of the list or not.
    (do ((list1 list1)
         (list2 list2)
         (x list1 (cdr x))
         (splicex ())
         (deleted-y ())
         ;; elements of LIST2, which are "equal" to some processed
         ;; earlier elements of LIST1
         )
        ((endp x)
         (if (null splicex)
             (setq list1 list2)
             (rplacd splicex list2))
         list1)
      (let ((key-val-x (apply-key key (car x)))
            (found-duplicate nil))

        ;; Move all elements from LIST2, which are "equal" to (CAR X),
        ;; to DELETED-Y.
        (do* ((y list2 next-y)
              (next-y (cdr y) (cdr y))
              (splicey ()))
             ((endp y))
          (cond ((let ((key-val-y (apply-key key (car y))))
                   (if notp
                       (not (funcall test-not key-val-x key-val-y))
                       (funcall test key-val-x key-val-y)))
                 (if (null splicey)
                     (setq list2 (cdr y))
                     (rplacd splicey (cdr y)))
                 (setq deleted-y (rplacd y deleted-y))
                 (setq found-duplicate t))
                (t (setq splicey y))))

        (unless found-duplicate
          (setq found-duplicate (with-set-keys (member key-val-x deleted-y))))

        (if found-duplicate
            (if (null splicex)
                (setq list1 (cdr x))
                (rplacd splicex (cdr x)))
            (setq splicex x))))))

(defun subsetp (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return T if every element in LIST1 is also in LIST2."
  (declare (inline member))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key))))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
        (return-from subsetp nil)))
    t))

;;;; functions that operate on association lists

(defun acons (key datum alist)
  "Construct a new alist by adding the pair (KEY . DATUM) to ALIST."
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional (alist '()))
  "Construct an association list from KEYS and DATA (adding to ALIST)."
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) alist)
    (if (or (endp x) (endp y))
        (error "The lists of keys and data are of unequal length."))
    (setq alist (acons (car x) (car y) alist))))

(defun assoc (item alist &key key (test nil testp) (test-not nil notp))
  "Return the cons in ALIST whose car is equal (by a given test or EQL) to
   the ITEM."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%assoc-key-test item alist key test)
               (%assoc-test item alist test)))
          (test-not
           (if key
               (%assoc-key-test-not item alist key test-not)
               (%assoc-test-not item alist test-not)))
          (t
           (if key
               (%assoc-key item alist key)
               (%assoc item alist))))))

(defun assoc-if (predicate alist &key key)
  "Return the first cons in ALIST whose CAR satisfies PREDICATE. If
   KEY is supplied, apply it to the CAR of each cons before testing."
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%assoc-if-key predicate alist key)
        (%assoc-if predicate alist))))

(defun assoc-if-not (predicate alist &key key)
  "Return the first cons in ALIST whose CAR does not satisfy PREDICATE.
  If KEY is supplied, apply it to the CAR of each cons before testing."
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%assoc-if-not-key predicate alist key)
        (%assoc-if-not predicate alist))))

(defun rassoc (item alist &key key (test nil testp) (test-not nil notp))
  (declare (list alist))
  "Return the cons in ALIST whose CDR is equal (by a given test or EQL) to
   the ITEM."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%rassoc-key-test item alist key test)
               (%rassoc-test item alist test)))
          (test-not
           (if key
               (%rassoc-key-test-not item alist key test-not)
               (%rassoc-test-not item alist test-not)))
          (t
           (if key
               (%rassoc-key item alist key)
               (%rassoc item alist))))))

(defun rassoc-if (predicate alist &key key)
  "Return the first cons in ALIST whose CDR satisfies PREDICATE. If KEY
  is supplied, apply it to the CDR of each cons before testing."
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%rassoc-if-key predicate alist key)
        (%rassoc-if predicate alist))))

(defun rassoc-if-not (predicate alist &key key)
  "Return the first cons in ALIST whose CDR does not satisfy PREDICATE.
  If KEY is supplied, apply it to the CDR of each cons before testing."
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%rassoc-if-not-key predicate alist key)
        (%rassoc-if-not predicate alist))))

;;;; mapping functions

;;; a helper function for implementation of MAPC, MAPCAR, MAPCAN,
;;; MAPL, MAPLIST, and MAPCON
;;;
;;; Map the designated function over the arglists in the appropriate
;;; way. It is done when any of the arglists runs out. Until then, it
;;; CDRs down the arglists calling the function and accumulating
;;; results as desired.
(defun map1 (fun-designator original-arglists accumulate take-car)
  (let ((fun (%coerce-callable-to-fun fun-designator)))
    (let* ((arglists (copy-list original-arglists))
           (ret-list (list nil))
           (temp ret-list))
      (do ((res nil)
           (args '() '()))
          ((dolist (x arglists nil) (if (null x) (return t)))
           (if accumulate
               (cdr ret-list)
               (car original-arglists)))
        (do ((l arglists (cdr l)))
            ((null l))
          (push (if take-car (caar l) (car l)) args)
          (setf (car l) (cdar l)))
        (setq res (apply fun (nreverse args)))
        (case accumulate
          (:nconc (setq temp (last (nconc temp res))))
          (:list (rplacd temp (list res))
                 (setq temp (cdr temp))))))))

(defun mapc (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists. Return the second argument."
  (map1 function (cons list more-lists) nil t))

(defun mapcar (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return list of FUNCTION
   return values."
  (map1 function (cons list more-lists) :list t))

(defun mapcan (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return NCONC of FUNCTION
   results."
  (map1 function (cons list more-lists) :nconc t))

(defun mapl (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return NIL."
  (map1 function (cons list more-lists) nil nil))

(defun maplist (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return list of results."
  (map1 function (cons list more-lists) :list nil))

(defun mapcon (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of lists. Return NCONC of results."
  (map1 function (cons list more-lists) :nconc nil))

;;;; Specialized versions

;;; %ADJOIN-*, %ASSOC-*, %MEMBER-*, and %RASSOC-* functions. Deftransforms
;;; delegate to TRANSFORM-LIST-PRED-SEEK and TRANSFORM-LIST-ITEM-SEEK which
;;; pick the appropriate versions. These win because they have only positional
;;; arguments, the TEST, TEST-NOT & KEY functions are known to exist (or not),
;;; and are known to be functions instead of function designators. We are also
;;; able to transform many common cases to -EQ versions, which are
;;; substantially faster then EQL using ones.
(macrolet
    ((def (funs form &optional variant)
       (flet ((%def (name &optional conditional)
                (let* ((body-loop
                        `(do ((list list (cdr l)))
                             ((null l) nil)
                           (declare (list l))
                           (let ((this (car l)))
                             ,(let ((cxx (if (char= #\A (char (string name) 0))
                                             'car    ; assoc, assoc-if, assoc-if-not
                                             'cdr))) ; rassoc, rassoc-if, rassoc-if-not
                                   (ecase name
                                      ((assoc rassoc)
                                       (if funs
                                           `(when this
                                              (let ((target (,cxx this)))
                                                (when ,form
                                                  (return this))))
                                           ;; If there is no TEST/TEST-NOT or
                                           ;; KEY, do the EQ/EQL test first,
                                           ;; before checking for NIL.
                                           `(let ((target (,cxx this)))
                                              (when (and ,form this)
                                                (return this)))))
                                 ((assoc-if assoc-if-not rassoc-if rassoc-if-not)
                                  (aver (equal '(eql x) (subseq form 0 2)))
                                  `(when this
                                     (let ((target (,cxx this)))
                                       (,conditional (funcall ,@(cdr form))
                                                     (return this)))))
                                 (member
                                  `(let ((target this))
                                     (when ,form
                                       (return l))))
                                 ((member-if member-if-not)
                                  (aver (equal '(eql x) (subseq form 0 2)))
                                  `(let ((target this))
                                     (,conditional (funcall ,@(cdr form))
                                                   (return l))))
                                 (adjoin
                                  `(let ((target this))
                                     (when ,form
                                       (return t)))))))))
                       (body (if (eq 'adjoin name)
                                 `(if (let ,(when (member 'key funs)
                                                  `((x (funcall key x))))
                                        ,body-loop)
                                      list
                                      (cons x l))
                                 body-loop)))
                  `(defun ,(intern (format nil "%~A~{-~A~}~@[-~A~]" name funs variant))
                       (x list ,@funs)
                     (declare (optimize speed (sb!c::verify-arg-count 0)))
                     ,@(when funs `((declare (function ,@funs))))
                     ,@(unless (member name '(member assoc adjoin rassoc)) `((declare (function x))))
                     ,body))))
         `(progn
            ,(%def 'adjoin)
            ,(%def 'assoc)
            ,(%def 'member)
            ,(%def 'rassoc)
            ,@(when (and (not variant) (member funs '(() (key)) :test #'equal))
                    (list (%def 'member-if 'when)
                          (%def 'member-if-not 'unless)
                          (%def 'assoc-if 'when)
                          (%def 'assoc-if-not 'unless)
                          (%def 'rassoc-if 'when)
                          (%def 'rassoc-if-not 'unless)))))))
  (def ()
      (eql x target))
  (def ()
      (eq x target)
    eq)
  (def (key)
      (eql x (funcall key target)))
  (def (key)
      (eq x (funcall key target))
    eq)
  (def (key test)
      (funcall test x (funcall key target)))
  (def (key test-not)
      (not (funcall test-not x (funcall key target))))
  (def (test)
      (funcall test x target))
  (def (test-not)
      (not (funcall test-not x target))))
|#
