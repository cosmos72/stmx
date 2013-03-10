;; -*- lisp -*-

(in-package :cl-stm2)

;;;; ** Transactional variables

(defmethod initialize-instance :after ((var tvar) &key (value nil value?) &allow-other-keys)
  (setf (slot-value var 'vbox)
        (new-vbox :value (if value? value +unbound+))))
  

;;;; ** Accessors

(declaim (inline version-of value-of-unmasked))

(defun version-of (var)
  "Return the version of a tvar versioned box"
  (declare (type tvar var))
  (vbox-version (vbox-of var)))

(defun value-of-unmasked (var)
  "Return the raw, unmasked value inside a tvar versioned box"
  (declare (type tvar var))
  (vbox-value (vbox-of var)))
  
(defun value-of-unbound-error (var)
  "Signal an unbound-slot error and allow the user to continue by specifying or storing a value."
  (declare (type tvar var))
  (restart-case (error 'unbound-slot :instance var :name 'value)
    (use-value (value)
      :report "Specify a value to use."
      :interactive (lambda ()
                     (format t "~&Value to use: ")
                     (list (eval (read))))
      value)
    (store-value (value)
      :report "Specify a value to use and store."
      :interactive (lambda ()
                     (format t "~&Value to use and store: ")
                     (list (eval (read))))
      ;; this will also increase the vbox version - we want that!
      (setf (value-of var) value)
      value)))

  
(defmethod value-of ((var tvar))
  "Return the value inside a tvar versioned box."
  (let1 value (value-of-unmasked var)
    (if (eq value +unbound+)
        (value-of-unbound-error var)
        value)))

(defmethod (setf value-of) (value (var tvar))
  "Set the value inside a tvar versioned box. Also increases the version by 1."
  (let1 old-version (version-of var)
    (setf (vbox-of var) (new-vbox
                         :version (1+ old-version)
                         :value value)))
  value)

(defun tvar-bound? (var)
  "Return true if the tvar versioned box is bound to a value."
  (declare (type tvar var))
  (vbox-bound? (vbox-of var)))

(defun tvar-unbind (var)
  "Restore the tvar versioned box to the unbound state."
  (declare (type tvar var))
  (setf (value-of var) +unbound+)
  var)


  


;;;; ** Printing

(defprint-object (obj tvar)
  (let1 box (vbox-of obj)
    (format t "v~A ~A"
            (vbox-version box)
            (if (eq (vbox-value box) +unbound+)
                "<unbound>"
                (vbox-value box)))))

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
        ;; read atomically the tvar value AND version.
        ;; this is the reason we wrap them in an immutable vbox:
        ;; accessing two different tvar slots, value and version,
        ;; would NOT be atomic.
        (let1 box (vbox-of var)
          (setf (gethash var (reads-of log)) (vbox-version box))
          (vbox-value box))))


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


;;;; ** Waiting

(defun unwait-tvar (var)
  "UNWAIT-TVAR causes all threads waiting for VAR to
change to wake up."
  (declare (type tvar var))
  (with-slots (waiting waiting-lock) var
    (with-lock-held (waiting-lock)
      (until (queue-empty-p waiting)
        (unwait-tlog (dequeue waiting))))))
