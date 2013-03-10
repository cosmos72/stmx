;; -*- lisp -*-

(in-package :cl-stm2)

;;;; ** Transactional variables

;;;; ** Printing

(defprint-object (obj tvar)
  (princ (if (slot-boundp obj 'value)
             (value-of obj)
             "<unbound>")))

;;;; ** Reading and Writing

(defun read-tvar (var &optional (log (current-tlog)))
  "Record the reading of VAR to LOG.

READ-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

READ-TVAR is just the plumbing behind taking the SLOT-VALUE of a
transactional slot.  Just use readers and accessors as you
normally would on transactional objects."
  (declare (type tvar var)
           (type tlog log))
  (aif2 (gethash var (writes-of log))
        it
        (progn (setf (gethash var (reads-of log))
                     (version-of var))
               (value-of var))))

(defun write-tvar (var val &optional (log (current-tlog)))
  "Record the writing of VAL to VAR to LOG and return VAL.

WRITE-TVAR is only called when transactions are being recorded,
and LOG is normally the special variable *LOG*.

WRITE-TVAR is just the plumbing behind SETF'ing a transactional
slot.  Just use SETF as you normally would on transactional
objects."
  (declare (type tvar var)
           (type tlog log))
  (setf (gethash var (writes-of log)) val))


(defun boundp-tvar (var)
  "Return true if VAR slot 'value is bound."
  (slot-boundp var 'value))

(defun makunbound-tvar (var)
  "Unbind the 'value slot inside VAR."
  (slot-makunbound var 'value))

  

;;;; ** Waiting

(defun unwait-tvar (var)
  "UNWAIT causes all threads waiting for VAR to
change to wake up."
  (declare (type tvar var))
  (with-slots (waiting waiting-lock) var
    (with-lock-held (waiting-lock)
      (until (queue-empty-p waiting)
        (unwait-tlog (dequeue waiting))))))
