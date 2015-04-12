(declaim (optimize (compilation-speed 0) (space 0) (debug 1) (safety 0) (speed 3)))
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
  `(time (dotimes (i 1000000000)
           ,@body)))
(defvar v (tvar 0))
(defvar c (tcell 0))
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

(defmacro sw-tlog ()
  `(stmx::current-tlog))

(defmacro simple-hw-atomic (&rest body)
  `(if (= +hw-transaction-started+ (hw-transaction-begin))
       (multiple-value-prog1
           (progn ,@body)
         (hw-transaction-end))
       (stmx::sw-atomic ,@body)))

(defmacro hw-atomic ((&optional hw-helper &key err (test-for-running-tx? nil))
                      &optional (body nil body?) fallback)
  `(stmx::hw-atomic2 (,hw-helper :err ,err :test-for-running-tx? ,test-for-running-tx?)
                     ,@(when body? `(,body (sw-atomic ,fallback)))))

(defmacro run1m (&rest body)
  `(let ((v v)
         (c c)
         (m m)
         (tm tm)
         (h h)
         (th th))
     (x3 (1m ,@body))))

(defmacro run10m (&rest body)
  `(let ((v v)
         (c c)
         (m m)
         (tm tm)
         (h h)
         (th th))
     (x3 (10m ,@body))))

(defmacro run1g (&rest body)
  `(let ((v v)
         (c c)
         (m m)
         (tm tm)
         (h h)
         (th th))
     (x3 (1g ,@body))))


(run10m (sw-atomic nil))
(run10m (atomic nil))
(run10m (simple-hw-atomic nil))


(run10m (sw-atomic  ($-swtx (sw-tlog) v)))
(run10m (atomic     ($ v)))
(run10m (hw-atomic  (hw-helper)
                    ($-hwtx hw-helper v) ;; hw transaction
                    ($-swtx (sw-tlog) v))) ;; sw transaction


(run10m (sw-atomic  (setf ($-swtx (sw-tlog) v) 1)))
(run10m (atomic     (setf ($ v) 1)))
(run10m (hw-atomic  (hw-helper)
                    (setf ($-hwtx hw-helper v) 1)
                    (setf ($-swtx (sw-tlog) v) 1)))


(run10m (sw-atomic  (incf (the fixnum ($-swtx (sw-tlog) v)))))
(run10m (atomic     (incf (the fixnum ($ v)))))
(run10m (hw-atomic  (hw-helper)
                    (incf (the fixnum ($-hwtx hw-helper v)))
                    (incf (the fixnum ($-swtx (sw-tlog) v)))))


(run10m (sw-atomic  (let ((sw-helper (sw-tlog)))
                      (dotimes (j 10) (incf (the fixnum ($-swtx sw-helper v)))))))
(run10m (atomic     (dotimes (j 10) (incf (the fixnum ($ v))))))
(run10m (hw-atomic  (hw-helper)
                    (dotimes (j 10) (incf (the fixnum ($-hwtx hw-helper v))))
                    (let ((sw-helper (sw-tlog)))
                      (dotimes (j 10) (incf (the fixnum ($-swtx sw-helper v)))))))
(let ((n 0))
  (x3 (10m (simple-hw-atomic (incf (the fixnum n))))))



(run10m (sw-atomic  (dotimes (j 100) (incf (the fixnum ($-swtx v))))))
(run10m (atomic     (dotimes (j 100) (incf (the fixnum ($ v))))))
(run10m (hw-atomic  (hw-helper)
                    (dotimes (j 100) (incf (the fixnum ($-hwtx v hw-helper))))
                    (dotimes (j 100) (incf (the fixnum ($-swtx v))))))


(run1m (sw-atomic  (dotimes (j 1000) (incf (the fixnum ($-swtx v))))))
(run1m (atomic     (dotimes (j 1000) (incf (the fixnum ($ v))))))
(run1m (hw-atomic  (hw-helper)
                   (dotimes (j 1000) (incf (the fixnum ($-hwtx v hw-helper))))
                   (dotimes (j 1000) (incf (the fixnum ($-swtx v))))))


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








;; 0.455 seconds
(run1g ($-notx v))

;; 1.554 seconds
(run1g ($ v))

;; 31.311 seconds
(run1g (slot-value c 'value))

;; 6.728 seconds
(run1g (setf ($-notx v) 1))

;; 9.066 seconds
(run1g (setf ($ v) 1))

;; ~60 seconds
(run1g (setf (slot-value c 'value) 0))




;; 0.025 seconds
(run1m (tvar))

;; 0.056 seconds
(run1m (tcons 0 0))

;; 0.644 seconds
(run1m (tlist 1 2 3 4 5 6 7 8 9 10))

;; CLOS is slower...

;; 0.251 seconds - v2.0.0 initialize-instance
;; 0.847 seconds - v1.9.0 initialize-instance
(run1m (tcell))

;; 2.594 seconds - v2.0.0 initialize-instance
;; 8.564 seconds - v1.9.0 initialize-instance
(run1m (tcell (tcell (tcell (tcell (tcell (tcell (tcell (tcell (tcell (tcell 1)))))))))))

;; 0.258 seconds - v2.0.0 initialize-instance
;; 0.547 seconds - v1.9.0 initialize-instance
(run1m (tstack))

;; 0.393 seconds - v2.0.0 initialize-instance
;; 0.943 seconds - v1.9.0 initialize-instance
(run1m (new 'tfifo))

;; 0.129 seconds - v2.0.0 initialize-instance
;; 0.127 seconds - v1.9.0 initialize-instance
(run1m (new 'rbmap :pred 'fixnum<))

;; 0.644 seconds - v2.0.0 initialize-instance
;; 1.318 seconds - v1.9.0 initialize-instance
(run1m (new 'tmap  :pred 'fixnum<))

;; 0.245 seconds - v2.0.0 initialize-instance
;; 0.234 seconds - v1.9.0 initialize-instance
(run1m (new 'ghash-table :test 'fixnum= :hash 'identity))

;; 1.124 seconds - v2.0.0 initialize-instance
;; 2.363 seconds - v1.9.0 initialize-instance
(run1m (new 'thash-table :test 'fixnum= :hash 'identity)) 

;; 0.298 seconds
(let ((n 0))
  (declare (type fixnum n))
  (run1m
   (let ((h (make-hash-table)))
     (incf n (hash-table-count h))))
  n)



