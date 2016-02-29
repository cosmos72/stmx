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

(defun time-to-string (&optional (time (get-universal-time)))
  (multiple-value-bind (ss mm hh day month year day-of-week daylight tz)
      (decode-universal-time time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~A~2,'0D:00"
            year month day hh mm ss (if (minusp tz) #\+ #\-) (abs tz))))

(defun show-failed-test (test &key (interactive t))
  (if interactive
      (inspect test)
      (describe test))
  nil)

(defun loop-run-tests (&key (suite 'suite) (interactive t))
  (loop
     do (format t "~&~A~&" (time-to-string))
     always
       (loop for test in (fiveam:run suite)
          always (or (typep test 'fiveam::test-passed)
                     (show-failed-test test :interactive interactive)))))
              
