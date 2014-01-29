(declaim (optimize (compilation-speed 0) (space 0) (debug 0) (safety 0) (speed 3)))
(ql:quickload "stmx")
(ql:quickload "stmx.test")
(fiveam:run! 'stmx.test:suite)

(in-package :stmx.util)

(defmacro x3 (&rest body)
  `(dotimes (,(gensym) 3)
     ,@body))
(defmacro 1m (&rest body)
  `(time (dotimes (i 1000000)
           ,@body)))
(defmacro 10m (&rest body)
  `(time (dotimes (i 10000000)
           ,@body)))
(defmacro 1g (&rest body)
  `(time (dotimes (i 10000000000)
           ,@body)))
(defvar v (tvar 0))
(defvar m  (new 'rbmap :pred 'fixnum<)) 
(defvar tm (new 'tmap  :pred 'fixnum<)) 
(defvar h  (new 'ghash-table :test 'fixnum= :hash 'identity)) 
(defvar th (new 'thash-table :test 'fixnum= :hash 'identity)) 
;; some initial values
(setf (get-gmap m 1) 0)
(setf (get-gmap tm 1) 0)
(setf (get-ghash h 1) 0)
(setf (get-ghash th 1) 0)

(defmacro sw-atomic (&rest body)
  `(stmx::sw-atomic ,@body))

(defmacro simple-hw-atomic (&rest body)
  `(if (= +hw-transaction-started+ (hw-transaction-begin))
       (multiple-value-prog1
           (progn ,@body)
         (hw-transaction-end))
       (stmx::sw-atomic ,@body)))

(defmacro hw-atomic ((&optional tvar-write-version &key err (test-for-running-tx? nil))
                      &optional (body nil body?) fallback)
  `(stmx::hw-atomic2 (,tvar-write-version :err ,err :test-for-running-tx? ,test-for-running-tx?)
                     ,@(when body? `(,body (sw-atomic ,fallback)))))

(defmacro run1m (&rest body)
  `(let ((v v)
         (m m)
         (tm tm)
         (h h)
         (th th))
     (x3 (1m ,@body))))

(defmacro run10m (&rest body)
  `(let ((v v)
         (m m)
         (tm tm)
         (h h)
         (th th))
     (x3 (10m ,@body))))

(defmacro run1g (&rest body)
  `(let ((v v)
         (m m)
         (tm tm)
         (h h)
         (th th))
     (x3 (1g ,@body))))


(run10m (sw-atomic nil))
(run10m (atomic nil))
(run10m (simple-hw-atomic nil))


(run10m (sw-atomic  ($-tx v)))
(run10m (atomic     ($ v)))
(run10m (hw-atomic  ()
                    ($-hwtx v) ;; hw transaction
                    ($-tx v))) ;; sw transaction


(run10m (sw-atomic  (setf ($-tx v) 1)))
(run10m (atomic     (setf ($ v) 1)))
(run10m (hw-atomic  (wv)
                    (setf ($-hwtx v wv) 1)
                    (setf ($-tx v) 1)))


(run10m (sw-atomic  (incf (the fixnum ($-tx v)))))
(run10m (atomic     (incf (the fixnum ($ v)))))
(run10m (hw-atomic  (wv)
                    (incf (the fixnum ($-hwtx v wv)))
                    (incf (the fixnum ($-tx v)))))


(run10m (sw-atomic  (dotimes (j 10) (incf (the fixnum ($-tx v))))))
(run10m (atomic     (dotimes (j 10) (incf (the fixnum ($ v))))))
(run10m (hw-atomic  (wv)
                    (dotimes (j 10) (incf (the fixnum ($-hwtx v wv))))
                    (dotimes (j 10) (incf (the fixnum ($-tx v))))))
(let ((n 0))
  (x3 (10m (simple-hw-atomic (incf (the fixnum n))))))



(run10m (sw-atomic  (dotimes (j 100) (incf (the fixnum ($-tx v))))))
(run10m (atomic     (dotimes (j 100) (incf (the fixnum ($ v))))))
(run10m (hw-atomic  (wv)
                    (dotimes (j 100) (incf (the fixnum ($-hwtx v wv))))
                    (dotimes (j 100) (incf (the fixnum ($-tx v))))))


(run1m (sw-atomic  (dotimes (j 1000) (incf (the fixnum ($-tx v))))))
(run1m (atomic     (dotimes (j 1000) (incf (the fixnum ($ v))))))
(run1m (hw-atomic  (wv)
                   (dotimes (j 1000) (incf (the fixnum ($-hwtx v wv))))
                   (dotimes (j 1000) (incf (the fixnum ($-tx v))))))


(run10m (sw-atomic (orelse)))
(run10m (atomic (orelse)))
(run10m (simple-hw-atomic (orelse)))


(run1m (sw-atomic (get-gmap tm 1)))
(run1m (atomic    (get-gmap tm 1)))

(run1m (sw-atomic (incf (the fixnum (get-gmap tm 1)))))
(run1m (atomic    (incf (the fixnum (get-gmap tm 1)))))

(run1m (sw-atomic (when (zerop (mod i  100)) (clear-gmap tm))
                  (set-gmap tm i t)))
(run1m (atomic    (when (zerop (mod i  100)) (clear-gmap tm))
                  (set-gmap tm i t)))
