;; -*- lisp -*-

;; This file is part of STMX-PERSIST.TEST.
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


(in-package :stmx-persist.test)


(defun test-mwrite-float-inline (ptr count)
  (declare (type fixnum count))

  ;; on Core i7-4770,
  ;; HW transactions succeed very often when writing up to 16KBytes
  ;; as long as the mmapped RAM is already dirty:
  ;; each RAM page must be written to *before* the HW transaction.

  ;; WARNING:
  ;; calling (sp::mset-unboxed) instead of (sp::mset-float-inline)
  ;; at low settings of (optimize (speed)) causes *all*  HW transactions to fail!
  ;; the problem disappears by setting (optimize (speed 3)) before loading STMX-PERSIST

  (loop for idx from 0 below count by 512
       for value = (sp::mget-word ptr idx) do
       (sp::mset-word ptr idx value))

  (stmx::hw-atomic2 ()
   (loop for idx from 0 below count
      for value from 0.0 by 0.1 do
        (sp::mset-float-inline :float ptr idx value)
        finally (return :hw-tx))
   :fallback))
