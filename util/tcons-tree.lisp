;;;; functions to implement list-based trees

;;;; This software is derived from the SBCL system.
;;;; See the README.SBCL file for more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :stmx.util)

(enable-#?-syntax)


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


(defun copy-ttree (tree)
  "Recursively copy trees of TCONSes."
  (if (tconsp tree)
      (let ((result (tlist (let ((car (tcons-first tree)))
                             (if (tconsp car)
                                 (copy-ttree car)
                                 car)))))
        (loop
           for last-cons = result then new-cons
           for cdr = (tcons-rest tree) then (tcons-rest cdr)
           for car = (if (tconsp cdr)
                         (tcons-first cdr)
                         (return (setf (trest last-cons) cdr)))
           for new-cons = (tlist (if (tconsp car)
                                     (copy-ttree car)
                                     car))
           do (setf (trest last-cons) new-cons))
        result)
      tree))



#|
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

|#
