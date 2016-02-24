;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :stmx.test)

(def-suite on-commit-suite :in suite)
(in-suite on-commit-suite)

(def-test on-commit (:compile-at :definition-time)
  (let (x)
    (atomic
     (before-commit
         (is (null x))
         (setf x 'before))
       (after-commit
         (is (eq 'before x))
         (setf x 'after)))
    (is (eq 'after x))))


(def-test before-commit-fails (:compile-at :definition-time)
  (let ((var (tvar 'original)))

    (signals test-error
      (atomic
       (setf ($ var) 'changed)
       (is (eq 'changed ($ var)))
       (before-commit
         (is (eq 'changed ($ var)))
         (is (eq 'original (raw-value-of var)))
         (setf ($ var) 'before-commit))
       (before-commit
         (is (eq 'before-commit ($ var)))
         (is (eq 'original (raw-value-of var)))
         (error 'test-error))
       (before-commit
         (fail "before-commit function unexpectedly invoked after another one signalled an error"))
       (after-commit
         (fail "after-commit function unexpectedly invoked after before-commit signalled an error"))))
    (is (eq 'original ($ var)))
    (is (eq 'original (raw-value-of var)))))


(def-test after-commit-fails (:compile-at :definition-time)
  (let ((var (tvar 'original)))

    (signals test-error
      (atomic
       (setf ($ var) 'changed)
       (is (eq 'changed ($ var)))
       (before-commit
         (is (eq 'changed ($ var)))
         (is (eq 'original (raw-value-of var)))
         (setf ($ var) 'before-commit))
       ;; after-commit blocks are executed in forward order.
       ;; they can read and write transactional memory,
       ;; but they are executed OUTSIDE any transaction
       (after-commit
         (is (eq 'before-commit (raw-value-of var)))
         (is (eq 'before-commit ($ var)))
         (setf ($ var) 'after-commit)
         (error 'test-error))
       (after-commit
         (fail "after-commit function unexpectedly invoked after another one signalled an error"))))

    (is (eq 'after-commit ($ var)))
    (is (eq 'after-commit (raw-value-of var)))))
