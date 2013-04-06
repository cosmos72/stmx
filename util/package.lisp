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


;;;; * STMX.UTIL

(in-package :cl-user)

(defpackage #:stmx.util
  
  (:use #:cl
        #:arnesi
        #:stmx)

  (:import-from #:stmx
                #:with-gensym
                #:with-gensyms
                #:do-hash)

  (:export #:fixnum< #:fixnum>   ;; optimized versions of < > = /= useful with BMAPs
           #:fixnum= #:fixnum/=

           #:bmap  ;; generic binary tree
           #:rbmap ;; sorted map, implemented with red-black trees
           #:tmap  ;; transactional sorted map, implemented with red-black trees

           #:bmap-pred   #:bmap-count  #:bmap-empty?
           #:clear-bmap  #:copy-bmap   #:copy-bmap-into
           #:get-bmap    #:set-bmap    #:rem-bmap  ;; also (setf (get-bmap ...) ...)
           #:min-bmap    #:max-bmap         ;; get smallest and largest key/value
           #:map-bmap    #:do-bmap          ;; iterate on bmap
           #:bmap-keys   #:bmap-values #:bmap-pairs ;; list all keys, values, or pairs
           #:add-to-bmap #:remove-from-bmap ;; add or remove multiple keys

           #:thash-table ;; transactional hash table
           #:thash-count #:thash-empty? #:clear-thash
           #:get-thash #:set-thash #:rem-thash ;; also (setf (get-thash ... ) ... )
           #:map-thash #:do-thash           ;; iterate on thash-table

           #:cell
           #:value-of
           #:empty?
           #:empty!
           #:full?
           #:take
           #:put
           #:try-put
           #:try-take))

