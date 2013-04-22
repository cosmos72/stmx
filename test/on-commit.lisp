;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013 Massimiliano Ghilardi
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

(test on-commit
  (let (x)
    (atomic
     (before-commit
         (is (null x))
         (setf x 'before))
       (after-commit
         (is (eq 'before x))
         (setf x 'after)))
    (is (eq 'after x))))


(test before-commit-fails
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


(test after-commit-fails
  (let ((var (tvar 'original)))

    (signals test-error
      (atomic
       (setf ($ var) 'changed)
       (is (eq 'changed ($ var)))
       (before-commit
         (is (eq 'changed ($ var)))
         (is (eq 'original (raw-value-of var)))
         (setf ($ var) 'before-commit))
       ;; after-commit blocks are executed in forward order
       (after-commit
         (is (eq 'before-commit ($ var)))
         (is (eq 'before-commit (raw-value-of var)))
         ;; after-commit functions MUST NOT write to transactional memory
         ;; (setf ($ var) 'after-commit))
         (error 'test-error))
       (after-commit
         (fail "after-commit function unexpectedly invoked after another one signalled an error"))))

    (is (eq 'before-commit ($ var)))
    (is (eq 'before-commit (raw-value-of var)))))
