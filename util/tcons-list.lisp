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

(enable-#?-syntax)




(defmacro %tlist-get (tlist op &rest next-ops)
  (let ((op (case op
              ((a car first) 'tcons-first)
              ((d cdr rest) 'tcons-rest)
              (otherwise op))))
    `(when ,tlist
       ,(if next-ops
            (with-gensym tlist-
              `(let ((,tlist- (,op ,tlist)))
                 (%tlist-get ,tlist- ,@next-ops)))
            `(,op ,tlist)))))


(eval-when (:compile-toplevel)
  (defun %%tlist-set (value tcons op next-ops)
    (let ((op (case op
                ((a car first) 'tcons-first)
                ((d cdr rest) 'tcons-rest)
                (otherwise op))))
      ;; the slow, exact test is: (unless (tconsp ,tcons) ...)
      ;; instead we only check for nil,
      ;; relying on compiler's safety level for other non-TCONS
      `((unless ,tcons
          (go type-error))
        ,@(if next-ops
              `((setf ,tcons (,op ,tcons))
                ,@(%%tlist-set value tcons (first next-ops) (rest next-ops)))
              `((setf (,op ,tcons) ,value)))))))


(defmacro %tlist-set (value tcons op &rest next-ops)
  `(block nil
     (tagbody
        ,@(%%tlist-set value tcons op next-ops)
        (return ,value)
      type-error
        (error 'type-error :datum ,tcons :expected-type 'tcons))))



(eval-when (:compile-toplevel)
  ;; defun tcaar tcadr tcdar tcddr and their setf
  (defmacro %def-tcxxr ()
    (let ((forms))
      (dolist (a0 '(a d))
        (dolist (a1 '(a d))
          (setf
           forms
           (list*
            `(defun (setf ,(symbolicate 'tc a0 a1 'r)) (value tlist)
               ,(format nil "Set the ~a~a~a of the ~a~a~a of a TLIST.
This function should always be executed inside an STMX atomic block."
                        'c a0 'r 'c a1 'r)
               (declare (type tlist tlist))
               (%tlist-set value tlist ,a1 ,a0))
            `(defun ,(symbolicate 'tc a0 a1 'r) (tlist)
               ,(format nil "Return the ~a~a~a of the ~a~a~a of a TLIST."
                        'c a0 'r 'c a1 'r)
               (declare (type tlist tlist))
               (%tlist-get tlist ,a1 ,a0))
            forms))))
      `(progn ,@(nreverse forms))))


  ;; defun tcaaar tcaadr tcadar tcaddr tcdaar tcdadr tcddar tcdddr and their setf
  (defmacro %def-tcxxxr ()
    (let ((forms))
      (dolist (a0 '(a d))
        (dolist (a1 '(a d))
          (dolist (a2 '(a d))
            (setf
             forms
             (list*
              `(defun (setf ,(symbolicate 'tc a0 a1 a2 'r)) (value tlist)
                 ,(format nil "Set the ~a~a~a of the ~a~a~a~a of a TLIST.
This function should always be executed inside an STMX atomic block."
                          'c a0 'r 'c a1 a2 'r)
                 (declare (type tlist tlist))
                 (%tlist-set value tlist ,a2 ,a1 ,a0))
              `(defun ,(symbolicate 'tc a0 a1 a2 'r) (tlist)
                 ,(format nil "Return the ~a~a~a of the ~a~a~a~a of a TLIST."
                          'c a0 'r 'c a1 a2 'r)
                 (declare (type tlist tlist))
                 (%tlist-get tlist ,a2 ,a1 ,a0))
              forms)))))
      `(progn ,@(nreverse forms))))


  ;; defun tcaaaar tcaaadr tcaadar tcaaddr ... tcdddar tcddddr and their setf
  (defmacro %def-tcxxxxr ()
    (let ((forms))
      (dolist (a0 '(a d))
        (dolist (a1 '(a d))
          (dolist (a2 '(a d))
            (dolist (a3 '(a d))
              (setf
               forms
               (list*
                `(defun (setf ,(symbolicate 'tc a0 a1 a2 a3 'r)) (value tlist)
                   ,(format nil "Set the ~a~a~a of the ~a~a~a~a~a of a TLIST.
This function should always be executed inside an STMX atomic block."
                            'c a0 'r 'c a1 a2 a3 'r)
                   (declare (type tlist tlist))
                   (%tlist-set value tlist ,a3 ,a2 ,a1 ,a0))
                `(defun ,(symbolicate 'tc a0 a1 a2 a3 'r) (tlist)
                   ,(format nil "Return the ~a~a~a of the ~a~a~a~a~a of a TLIST."
                            'c a0 'r 'c a1 a2 a3 'r)
                   (declare (type tlist tlist))
                   (%tlist-get tlist ,a3 ,a2 ,a1 ,a0))
                forms))))))
      `(progn ,@(nreverse forms)))))

(%def-tcxxr)
(%def-tcxxxr)
(%def-tcxxxxr)






(defun tendp (object)
  "This is the recommended way to test for the end of a proper TLIST. It
   returns true if OBJECT is NIL, false if OBJECT is a TCONS, and an error
   for any other type of OBJECT."
  (check-type object tlist)
  (null object))


(defun tlist-length (tlist)
  "Return the length of the given TLIST, or NIL if the TLIST is circular."
  (do ((n 0 (+ n 2))
       (y tlist (tcddr y))
       (z tlist (tcdr z)))
      (())
    (declare (type fixnum n)
             (type tlist y z))
    (when (tendp y) (return n))
    (when (tendp (tcdr y)) (return (the fixnum (1+ n))))
    (when (and (eq y z) (> n 0)) (return nil))))



;; more list operations


(declaim (notinline tnthcdr)
         (inline tnth))

(defun tnthcdr (n tlist)
  "Performs the TCDR function N times on a TLIST."
  (declare (type (and fixnum (integer 0)) n)
           (type tlist tlist))
  (loop for i fixnum = n then (1- i)
     for result = tlist then (tcons-rest result)
     while (and result (plusp i))
     finally (return result)))


(defun tnth (n tlist)
  "Return the Nth object in a TLIST where the TCAR is the zero-th element."
  (declare (type (and fixnum (integer 0)))
           (type tlist tlist))
  (tcar (tnthcdr n tlist)))


(defun (setf tnth) (newval n tlist)
  "Set the Nth element of TLIST to NEWVAL."
  (declare (type (and fixnum (integer 0)) n)
           (type tcons tlist))
  (let ((cons (tnthcdr n tlist)))
    (when (tendp cons)
      (error "~S is too large an index for SETF of TNTH." n))
    (setf (tfirst cons) newval)))



(declaim (inline tsecond tthird tfourth tfifth tsixth tseventh teighth tninth ttenth))

(defun tsecond (tlist)
  "Return the 2nd object in a TLIST or NIL if there is no 2nd object."
  (declare (type tlist tlist))
  (tcadr tlist))
(defun tthird (tlist)
  "Return the 3rd object in a TLIST or NIL if there is no 3rd object."
  (declare (type tlist tlist))
  (tcaddr tlist))
(defun tfourth (tlist)
  "Return the 4th object in a TLIST or NIL if there is no 4th object."
  (declare (type tlist tlist))
  (tcadddr tlist))
(defun tfifth (tlist)
  "Return the 5th object in a TLIST or NIL if there is no 5th object."
  (declare (type tlist tlist))
  (tnth 4 tlist))
(defun tsixth (tlist)
  "Return the 6th object in a TLIST or NIL if there is no 6th object."
  (declare (type tlist tlist))
  (tnth 5 tlist))
(defun tseventh (tlist)
  "Return the 7th object in a TLIST or NIL if there is no 7th object."
  (declare (type tlist tlist))
  (tnth 6 tlist))
(defun teighth (tlist)
  "Return the 8th object in a TLIST or NIL if there is no 8th object."
  (declare (type tlist tlist))
  (tnth 7 tlist))
(defun tninth (tlist)
  "Return the 9th object in a TLIST or NIL if there is no 9th object."
  (declare (type tlist tlist))
  (tnth 8 tlist))
(defun ttenth (tlist)
  "Return the 10th object in a TLIST or NIL if there is no 10th object."
  (declare (type tlist tlist))
  (tnth 9 tlist))


(declaim (inline (setf tsecond) (setf tthird) (setf tfourth) (setf tfifth)
                 (setf tsixth) (setf tseventh) (setf teighth) (setf tninth) (setf ttenth)))

(defun (setf tsecond) (value tlist)
  "Set the 2nd object in a TLIST."
  (declare (type tlist tlist))
  (setf (tcadr tlist) value))
(defun (setf tthird) (value tlist)
  "Set the 3rd object in a TLIST."
  (declare (type tlist tlist))
  (setf (tcaddr tlist) value))
(defun (setf tfourth) (value tlist)
  "Set the 4th object in a TLIST."
  (declare (type tlist tlist))
  (setf (tcadddr tlist) value))
(defun (setf tfifth) (value tlist)
  "Set the 5th object in a TLIST."
  (declare (type tlist tlist))
  (setf (tnth 4 tlist) value))
(defun (setf tsixth) (value tlist)
  "Set the 6th object in a TLIST."
  (declare (type tlist tlist))
  (setf (tnth 5 tlist) value))
(defun (setf tseventh) (value tlist)
  "Set the 7th object in a TLIST."
  (declare (type tlist tlist))
  (setf (tnth 6 tlist) value))
(defun (setf teighth) (value tlist)
  "Set the 8th object in a TLIST."
  (declare (type tlist tlist))
  (setf (tnth 7 tlist) value))
(defun (setf tninth) (value tlist)
  "Set the 9th object in a TLIST."
  (declare (type tlist tlist))
  (setf (tnth 8 tlist) value))
(defun (setf ttenth) (value tlist)
  "Set the 10th object in a TLIST."
  (declare (type tlist tlist))
  (setf (tnth 9 tlist) value))








;;; TLAST
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




(defmacro %tappend-consing (lists &key (do-outer-list 'dolist) (do-inner-list 'dolist))
  ;; cons an intermediate list rather than using SETF on newly created TVARS.
  ;; preferred inside software transactions, where SETF on TVARS is expensive
  (with-gensyms (rev-lists list obj tlist)
    `(let ((,rev-lists))
        (,do-outer-list (,list ,lists)
          (,do-inner-list (,obj ,list)
            (push ,obj ,rev-lists)))

        (let ((,tlist nil))
          (dolist (,obj ,rev-lists)
            (setf ,tlist (tcons ,obj ,tlist)))
          ,tlist))))


(defmacro %tappend-setf-tvar (lists &key (do-outer-list 'dolist) (do-inner-list 'dolist))
  ;; use SETF on newly created TVARS rather than consing an intermediate list.
  ;; preferred outside transactions, where SETF on TVARS is cheap
  (with-gensyms (prev next top list obj)
    `(let* ((,prev (tcons nil nil))
            (,top  ,prev))
       (,do-outer-list (,list ,lists)
         (,do-inner-list (,obj ,list)
           (let ((,next (tcons ,obj nil)))
             (setf (tcons-rest ,prev) ,next
                   ,prev ,next))))
       (tcons-rest ,top))))


(defun tappend (&rest tlists)
  "Construct a new tlist by concatenating the TLIST arguments"
  (declare (dynamic-extent tlists)
           (type list tlists)
           (optimize speed))

  (if (stmx::use-$-swtx?)
      (%tappend-consing tlists :do-inner-list do-tlist)
      (%tappend-setf-tvar tlists :do-inner-list do-tlist)))


(defun tappend-lists (&rest lists)
  "Construct a new tlist by concatenating the LIST arguments"
  (declare (dynamic-extent lists)
           (type list lists)
           (optimize speed))

  (if (stmx::use-$-swtx?)
      (%tappend-consing lists)
      (%tappend-setf-tvar lists)))



(defmacro %copy-tlist-consing (list-or-tlist &key (car 'car) (cdr 'cdr) (consp 'consp))
  ;; cons an intermediate list rather than using SETF on newly created TVARS.
  ;; preferred inside software transactions, where SETF on TVARS is expensive
  (with-gensyms (list obj rest copy tcopy)
    `(loop
        with ,list = ,list-or-tlist
        with ,copy = nil
        for ,obj = (,car ,list) then (,car ,rest)
        for ,rest = (,cdr ,list) then (,cdr ,rest)
        while (,consp ,rest)
        do
          (setf ,copy (cons ,obj ,copy))
        finally
          (let ((,tcopy (tcons ,obj ,rest)))
            (dolist (,obj ,copy)
              (setf ,tcopy (tcons ,obj ,tcopy)))
            (return ,tcopy)))))



(defmacro %copy-tlist-setf-tvar (list-or-tlist &key (car 'car) (cdr 'cdr) (consp 'consp))
  ;; use SETF on newly created TVARS rather than consing an intermediate list.
  ;; preferred outside transactions, where SETF on TVARS is cheap
  (with-gensyms (list copy splice orig next)
    `(loop
        with ,list = ,list-or-tlist
        with ,copy = (tlist (,car ,list))
        with ,splice = ,copy
        for ,orig = (,cdr ,list) then (,cdr ,orig)
        while (,consp ,orig)
        do
          (let ((,next (tcons (,car ,orig) nil)))
            (setf (trest ,splice) ,next
                  ,splice ,next))
        finally
          (unless (null ,orig)
            (setf (trest ,splice) ,orig))
          (return ,copy))))



(defun copy-tlist (tlist)
  "Return a new tlist which has the same elements as TLIST. TLIST may be improper."
  (declare (type tlist tlist))

  (unless tlist
    (return-from copy-tlist nil))

  (if (stmx::use-$-swtx?)
      (%copy-tlist-consing tlist :car tcons-first :cdr tcons-rest :consp tconsp)
      (%copy-tlist-setf-tvar tlist :car tcons-first :cdr tcons-rest :consp tconsp)))


(defun list-to-tlist (list)
  "Return a new tlist which has the same elements as LIST. LIST may be improper."
  (declare (type list list))

  (unless list
    (return-from list-to-tlist nil))

  (if (stmx::use-$-swtx?)
      (%copy-tlist-consing list)
      (%copy-tlist-setf-tvar list)))





;;;; more advanced list functions


(defun trevappend (x y)
  "Return (tappend (treverse x) y)."
  (declare (type tlist x y))
  (do ((top x (tcdr top))
       (result y (tcons (tcar top) result)))
      ((tendp top) result)))


#| TODO

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
|#

(defun tnreconc (x y)
  "Return (TNCONC (TNREVERSE X) Y)."
  (do ((1st (tcdr x) (if (tendp 1st) 1st (tcdr 1st)))
       (2nd x 1st)              ;2nd follows first down the list.
       (3rd y 2nd))             ;3rd follows 2nd down the list.
      ((tatom 2nd) 3rd)
    (trplacd 2nd 3rd)))

#|
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
|#

(defun tldiff (tlist object)
  "Return a new tlist, whose elements are those of TLIST that appear before
   OBJECT. If OBJECT is not a tail of TLIST, a copy of TLIST is returned.
   TLIST must be a proper tlist or a dotted tlist."
  (do* ((tlist tlist (tcdr tlist))
        (result (tlist ()))
        (splice result))
       ((tatom tlist)
        (if (eql tlist object)
            (tcdr result)
            (progn (trplacd splice tlist) (tcdr result))))
    (if (eql tlist object)
        (return (tcdr result))
        (setf splice (tcdr (trplacd splice (tlist (tcar tlist))))))))


