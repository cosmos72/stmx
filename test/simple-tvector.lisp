;; -*- lisp -*-

;; This file is part of STMX.
;; Copyright (c) 2013-2014 Massimiliano Ghilardi
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

(def-suite simple-tvector-suite :in suite)
(in-suite simple-tvector-suite)


(def-test new-simple-tvector (:compile-at :definition-time)
  (let* ((n 10)
         (tvec (simple-tvector n)))
    ;; default initial-element is 0
    (is (= n (simple-tvector-length tvec)))
    (let1 count 0
      (do-simple-tvector (e) tvec
        (is (= 0 e))
        (incf count))
      (is (= n count)))))

(def-test new-simple-tvector2 (:compile-at :definition-time)
  (let* ((n 10)
         (tvec (simple-tvector n :initial-element n)))
    (let1 count 0
      (do-simple-tvector (e) tvec
        (is (= n e))
        (incf count))
      (is (= n count)))))


(def-test new-simple-tvector3 (:compile-at :definition-time)
  (let* ((n 10)
         (tvec (simple-tvector n :initial-contents
                               (loop for i from 0 to (1- n)
                                    collect i))))
    (let1 count 0
      (do-simple-tvector (e) tvec
        (is (= count e))
        (incf count))
      (is (= n count)))))

      

(def-test svref-simple-tvector (:compile-at :definition-time)
  (let* ((n 10)
         (tvec (simple-tvector n)))
    ;; default initial-element is 0
    (dotimes (i n)
      (is (= 0 (tsvref tvec i)))
      (setf (tsvref tvec i) i))

    (dotimes (i n)
      (is (= i (tsvref tvec i))))

    (let1 count 0
      (do-simple-tvector (e) tvec
        (is (= count e))
        (incf count))
      (is (= n count)))))
