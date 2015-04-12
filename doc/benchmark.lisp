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
(defvar *v* (tvar 0))
(defvar *c* (tcell 0))
(defvar *m*  (new 'rbmap :pred 'fixnum<)) 
(defvar *tm* (new 'tmap  :pred 'fixnum<)) 
(defvar *h*  (new 'ghash-table :test 'fixnum= :hash 'identity)) 
(defvar *th* (new 'thash-table :test 'fixnum= :hash 'identity)) 
;; some initial values
(setf (get-gmap *m* 1) 0)
(setf (get-gmap *tm* 1) 0)
(setf (get-ghash *h* 1) 0)
(setf (get-ghash *th* 1) 0)

(defmacro sw-atomic (&rest body)
  `(stmx::sw-atomic ,@body))

(defmacro sw-tlog ()
  `(stmx::current-tlog))

(defmacro simple-hw-atomic (&rest body)
  `(if (= +hw-transaction-started+ (hw-transaction-begin))
       (multiple-value-prog1
           (stmx::with-hwtx ,@body)
         (hw-transaction-end))
       (stmx::sw-atomic ,@body)))

(defmacro hw-atomic ((&optional hw-helper &key err (test-for-running-tx? nil))
                      &optional (body nil body?) fallback)
  `(stmx::hw-atomic2 (,hw-helper :err ,err :test-for-running-tx? ,test-for-running-tx?)
                     ,@(when body? `(,body (sw-atomic ,fallback)))))

(defmacro run1m (&rest body)
  `(let ((v *v*)
         (c *c*)
         (m *m*)
         (tm *tm*)
         (h *h*)
         (th *th*))
     (declare (ignorable v c m tm h th))
     (x3 (1m ,@body))))

(defmacro run10m (&rest body)
  `(let ((v *v*)
         (c *c*)
         (m *m*)
         (tm *tm*)
         (h *h*)
         (th *th*))
     (declare (ignorable v c m tm h th))
     (x3 (10m ,@body))))

(defmacro run1g (&rest body)
  `(let ((v *v*)
         (c *c*)
         (m *m*)
         (tm *tm*)
         (h *h*)
         (th *th*))
     (declare (ignorable v c m tm h th))
     (x3 (1g ,@body))))

;;;; nil
(run10m (sw-atomic nil))
(run10m (atomic nil))
(run10m (simple-hw-atomic nil))

;;;; read-1
(run10m (sw-atomic  ($ v)))
(run10m (atomic     ($ v)))
(run10m (simple-hw-atomic ($ v)))

;;;; write-1
(run10m (sw-atomic  (setf ($ v) 1)))
(run10m (atomic     (setf ($ v) 1)))
(run10m (hw-atomic  (hw-helper)
                    (setf ($-hwtx v hw-helper) 1)
                    (setf ($ v) 1)))

;;;; read-write-1
(run10m (sw-atomic  (incf (the fixnum ($ v)))))
(run10m (atomic     (incf (the fixnum ($ v)))))
(run10m (hw-atomic  (hw-helper)
                    (incf (the fixnum ($-hwtx v hw-helper)))
                    (incf (the fixnum ($-swtx v (sw-tlog))))))


;;;; read-write-10
(run10m (sw-atomic  (let ((sw-helper (sw-tlog)))
                      (dotimes (j 10) (incf (the fixnum ($-swtx v sw-helper)))))))
(run10m (atomic     (dotimes (j 10) (incf (the fixnum ($ v))))))
(run10m (hw-atomic  (hw-helper)
                    (dotimes (j 10) (incf (the fixnum ($-hwtx v hw-helper))))
                    (let ((sw-helper (sw-tlog)))
                      (dotimes (j 10) (incf (the fixnum ($-swtx v sw-helper)))))))


;;;; read-write-100
(run10m (sw-atomic  (let ((sw-helper (sw-tlog)))
                      (dotimes (j 100) (incf (the fixnum ($-swtx v sw-helper)))))))
(run10m (atomic     (dotimes (j 100) (incf (the fixnum ($ v))))))
(run10m (hw-atomic  (hw-helper)
                    (dotimes (j 100) (incf (the fixnum ($-hwtx v hw-helper))))
                    (let ((sw-helper (sw-tlog)))
                      (dotimes (j 100) (incf (the fixnum ($-swtx v sw-helper)))))))

;;;; read-write-1000
(run1m (sw-atomic  (let ((sw-helper (sw-tlog)))
                     (dotimes (j 1000) (incf (the fixnum ($-swtx v sw-helper)))))))
(run1m (atomic     (dotimes (j 1000) (incf (the fixnum ($ v))))))
(run1m (hw-atomic  (hw-helper)
                   (dotimes (j 1000) (incf (the fixnum ($-hwtx v hw-helper))))
                   (let ((sw-helper (sw-tlog)))
                     (dotimes (j 1000) (incf (the fixnum ($-swtx v sw-helper)))))))

;; orelse empty
(run10m (sw-atomic (orelse)))
(run10m (atomic (orelse)))
(run10m (simple-hw-atomic (orelse)))

;; orelse unary
(run1m (sw-atomic (orelse ($ v))))
(run1m (atomic    (orelse ($ v))))

;; orelse retry-1
(run1m (sw-atomic (orelse (retry) ($ v))))
(run1m (atomic    (orelse (retry) ($ v))))

;; orelse retry-2
(run1m (sw-atomic (orelse (retry) (retry) ($ v))))
(run1m (atomic    (orelse (retry) (retry) ($ v))))

;; orelse retry-4
(run1m (sw-atomic (orelse (retry) (retry) (retry) (retry) ($ v))))
(run1m (atomic    (orelse (retry) (retry) (retry) (retry) ($ v))))

;; tmap read-1
(run1m (sw-atomic (get-gmap tm 1)))
(run1m (atomic    (get-gmap tm 1)))
(run1m (simple-hw-atomic (get-gmap tm 1)))
                   

;; tmap read-write-1
(run1m (sw-atomic (incf (the fixnum (get-gmap tm 1)))))
(run1m (atomic    (incf (the fixnum (get-gmap tm 1)))))
(run1m (hw-atomic  ()
                   (incf (the fixnum (get-gmap tm 1)))
                   (incf (the fixnum (get-gmap tm 1)))))

;; grow tmap (up to 10)
(run1m (sw-atomic (when (zerop (mod i  10)) (clear-gmap tm))
                  (set-gmap tm i t)))
(run1m (atomic    (when (zerop (mod i  10)) (clear-gmap tm))
                  (set-gmap tm i t)))

;; grow tmap (up to 100)
(run1m (sw-atomic (when (zerop (mod i  100)) (clear-gmap tm))
                  (set-gmap tm i t)))
(run1m (atomic    (when (zerop (mod i  100)) (clear-gmap tm))
                  (set-gmap tm i t)))

;; grow tmap (up to 1000)
(run1m (sw-atomic (when (zerop (mod i  1000)) (clear-gmap tm))
                  (set-gmap tm i t)))
(run1m (atomic    (when (zerop (mod i  1000)) (clear-gmap tm))
                  (set-gmap tm i t)))



;; thash read-write-1
(run1m (sw-atomic (incf (get-ghash th 1))))
(run1m (atomic    (incf (get-ghash th 1))))


;; grow thash (up to 10)
(run1m (sw-atomic (when (zerop (mod i   10)) (clear-ghash th))
                  (set-ghash th i t)))
(run1m (atomic    (when (zerop (mod i   10)) (clear-ghash th))
                  (set-ghash th i t)))

;; grow thash (up to 100)
(run1m (sw-atomic (when (zerop (mod i  100)) (clear-ghash th))
                  (set-ghash th i t)))
(run1m (atomic    (when (zerop (mod i  100)) (clear-ghash th))
                  (set-ghash th i t)))

;; grow thash (up to 1000)
(run1m (sw-atomic (when (zerop (mod i  1000)) (clear-ghash th))
                  (set-ghash th i t)))
(run1m (atomic    (when (zerop (mod i  1000)) (clear-ghash th))
                  (set-ghash th i t)))


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



