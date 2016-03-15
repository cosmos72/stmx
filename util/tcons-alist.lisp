;;;; functions to implement association lists

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

;;;; functions that operate on association lists


(defun tacons (key datum talist)
  "Construct a new talist by adding the pair (KEY . DATUM) to TALIST."
  (declare (type tlist talist))
  (tcons (tcons key datum) talist))

(defun tpairlis (keys data &optional talist)
  "Construct an association list from KEYS and DATA (adding to TALIST)."
  (declare (type tlist talist)
           (type list keys data))
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) talist)
    (if (or (endp x) (endp y))
        (error "The lists of keys and data are of unequal length."))
    (setf talist (tacons (car x) (car y) talist))))


(defun copy-talist (talist)
  "Return a new association list which is EQUAL to TALIST."
  (if (tendp talist)
      talist
      (let ((result
             (tlist (let ((item (tcar talist)))
                      (if (tatom item)
                          item
                          (copy-tcons item))))))
        (do ((x (tcdr talist) (tcdr x))
             (splice result
                     (tcdr (trplacd splice
                                    (tlist (let ((item (tcar x)))
                                             (if (tatom item)
                                                 item
                                                 (copy-tcons item))))))))
            ((tendp x)))
        result)))

(declaim (inline %coerce-to-fun))
(defun %coerce-to-fun (thing)
  (coerce thing 'function))

(defmacro %t-assoc (item talist key test test-not tcar)
  (labels ((%call (fun &rest args)
             (if fun
                 `(funcall ,fun ,@args)
                 (first args)))
           (%key (item)
             (%call key item))
           (%match (item e)
             (cond
               (test     (%call test item e))
               (test-not `(not ,(%call test-not item e)))
               (t        `(eql ,item ,e)))))
    (with-gensyms (e)
      `(do-tlist (,e ,talist)
         (when ,(%match item (%key `(,tcar ,e)))
           (return ,e))))))

(defmacro %tassoc (item talist key test test-not)
  `(%t-assoc ,item ,talist ,key ,test ,test-not tcar))

(defmacro %trassoc (item talist key test test-not)
  `(%t-assoc ,item ,talist ,key ,test ,test-not tcdr))

(defun tassoc (item talist &key key (test nil testp) (test-not nil notp))
  "Return the tcons in TALIST whose tcar is equal (by a given test or EQL) to
   the ITEM."
  (declare (type tlist talist))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-to-fun key)))
        (test (and testp (%coerce-to-fun test)))
        (test-not (and notp (%coerce-to-fun test-not))))
  
    (cond (test
           (if key
               (%tassoc item talist key test nil)
               (%tassoc item talist nil test nil)))
          (test-not
           (if key
               (%tassoc item talist key nil test-not)
               (%tassoc item talist nil nil test-not)))
          (t
           (if key
               (%tassoc item talist key nil nil)
               (%tassoc item talist nil nil nil))))))


(defun trassoc (item talist &key key (test nil testp) (test-not nil notp))
  "Return the cons in TALIST whose tcdr is equal (by a given test or EQL) to
   the ITEM."
  (declare (type tlist talist))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-to-fun key)))
        (test (and testp (%coerce-to-fun test)))
        (test-not (and notp (%coerce-to-fun test-not))))
    (cond (test
           (if key
               (%trassoc item talist key test nil)
               (%trassoc item talist nil test nil)))
          (test-not
           (if key
               (%trassoc item talist key nil test-not)
               (%trassoc item talist nil nil test-not)))
          (t
           (if key
               (%trassoc item talist key nil nil)
               (%trassoc item talist nil nil nil))))))

#| TODO


(defun assoc-if (predicate alist &key key)
  "Return the first cons in ALIST whose CAR satisfies PREDICATE. If
   KEY is supplied, apply it to the CAR of each cons before testing."
  (let ((predicate (%coerce-to-fun predicate))
        (key (and key (%coerce-to-fun key))))
    (if key
        (%assoc-if-key predicate alist key)
        (%assoc-if predicate alist))))

(defun assoc-if-not (predicate alist &key key)
  "Return the first cons in ALIST whose CAR does not satisfy PREDICATE.
  If KEY is supplied, apply it to the CAR of each cons before testing."
  (let ((predicate (%coerce-to-fun predicate))
        (key (and key (%coerce-to-fun key))))
    (if key
        (%assoc-if-not-key predicate alist key)
        (%assoc-if-not predicate alist))))


(defun rassoc-if (predicate alist &key key)
  "Return the first cons in ALIST whose CDR satisfies PREDICATE. If KEY
  is supplied, apply it to the CDR of each cons before testing."
  (let ((predicate (%coerce-to-fun predicate))
        (key (and key (%coerce-to-fun key))))
    (if key
        (%rassoc-if-key predicate alist key)
        (%rassoc-if predicate alist))))

(defun rassoc-if-not (predicate alist &key key)
  "Return the first cons in ALIST whose CDR does not satisfy PREDICATE.
  If KEY is supplied, apply it to the CDR of each cons before testing."
  (let ((predicate (%coerce-to-fun predicate))
        (key (and key (%coerce-to-fun key))))
    (if key
        (%rassoc-if-not-key predicate alist key)
        (%rassoc-if-not predicate alist))))



(defun sublis (alist tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitute from ALIST into TREE nondestructively."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-to-fun key)))
        (test (if testp (%coerce-to-fun test) test))
        (test-not (if notp (%coerce-to-fun test-not) test-not)))
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
  (let ((key (and key (%coerce-to-fun key)))
        (test (if testp (%coerce-to-fun test) test))
        (test-not (if notp (%coerce-to-fun test-not) test-not)))
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

|#


nil
