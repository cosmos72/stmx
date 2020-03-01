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

(def-suite thash-table-suite :in suite)
(in-suite thash-table-suite)



(def-test new-thash-table (:compile-at :definition-time)
  (let1 h (new 'thash-table :test '= :hash 'identity)
    (is (= 0 (ghash-table-count h)))
    (do-ghash (key value) h
      (fail "unexpected entry ~A = ~A in empty thash-table" key value))))



(defun test-new-thash-table (pred &key (count 16))
  (dolist (thash
            (list
             (new 'thash-table :test 'eql)
             (new 'thash-table :test 'eql     :hash 'identity)
             (new 'thash-table :test 'equal)
             (new 'thash-table :test 'equal   :hash 'identity)
             (new 'thash-table :test '=       :hash 'sxhash)
             (new 'thash-table :test '=       :hash 'identity)
             (new 'thash-table :test 'fixnum= :hash 'sxhash)
             (new 'thash-table :test 'fixnum= :hash 'identity)))
    (test-ghash-table thash pred :count count)))



(def-test thash-table (:compile-at :definition-time)
  (test-new-thash-table #'fixnum<))

