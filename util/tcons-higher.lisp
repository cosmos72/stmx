;;;; functions to implement list-based higher order functions

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

;; TODO

#|

;;;; mapping functions

;;; a helper function for implementation of MAPC, MAPCAR, MAPCAN,
;;; MAPL, MAPLIST, and MAPCON
;;;
;;; Map the designated function over the arglists in the appropriate
;;; way. It is done when any of the arglists runs out. Until then, it
;;; CDRs down the arglists calling the function and accumulating
;;; results as desired.
(defun %tmap1 (fun-designator original-arglists accumulate take-car)
  (let ((fun (fdefinition fun-designator)))
    (let* ((arglists (copy-tlist original-arglists))
           (ret-list (tlist nil))
           (temp ret-list))
      (do ((res nil)
           (args nil nil))
          ((dolist (x arglists nil) (if (null x) (return t)))
           (if accumulate
               (tcdr ret-list)
               (tcar original-arglists)))
        (do ((l arglists (tcdr l)))
            ((null l))
          (tpush (if take-car (tcaar l) (tcar l)) args)
          (setf (tcar l) (tcdar l)))
        (setq res (apply fun (tnreverse args)))
        (case accumulate
          (:nconc (setf temp (tlast (tnconc temp res))))
          (:list (trplacd temp (tlist res))
                 (setf temp (tcdr temp))))))))

(defun tmapc (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists. Return the second argument."
  (%tmap1 function (cons list more-lists) nil t))

(defun tmapcar (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return list of FUNCTION
   return values."
  (%tmap1 function (cons list more-lists) :list t))

(defun tmapcan (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return NCONC of FUNCTION
   results."
  (%tmap1 function (cons list more-lists) :nconc t))

(defun tmapl (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return NIL."
  (%tmap1 function (cons list more-lists) nil nil))

(defun tmaplist (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return list of results."
  (%tmap1 function (cons list more-lists) :list nil))

(defun tmapcon (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of lists. Return NCONC of results."
  (%tmap1 function (cons list more-lists) :nconc nil))


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


nil
