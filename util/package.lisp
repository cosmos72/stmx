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


;;;; * STMX.UTIL

(in-package :cl-user)

(defpackage #:stmx.util
  
  (:use #:cl
        #:stmx.lang
        #:stmx)

  ;; no need for closer-mop version of typep and subtypep;
  ;; they even cause some tests to fail
  #+cmucl
  (:shadowing-import-from #:cl
                          #:typep
                          #:subtypep)

  (:import-from #:alexandria
                #:symbolicate)

  (:import-from #:stmx
                #:+dummy-tvar+ #:peek-$ #:try-take-$ #:try-put-$)

  (:export #:tcell #:tfifo #:tstack #:tchannel #:tport  ;; transactional containers
           
           #:full? #:empty? #:empty! ;; methods for transactional containers
           #:peek   #:take   #:put
           #:try-take    #:try-put

           ;; optimized versions of < > = /= useful with sorted maps
           #:fixnum< #:fixnum>
           #:fixnum= #:fixnum/=

           #:gmap  ;; abstract, generic sorted binary map - see rbmap and tmap implementations
           #:rbmap ;; sorted map, implemented with red-black trees
           #:tmap  ;; transactional sorted map, implemented with red-black trees

           #:gmap-pred   #:gmap-count  #:gmap-empty?
           #:clear-gmap  #:copy-gmap   #:copy-gmap-into
           #:get-gmap    #:set-gmap    #:rem-gmap  ;; also (setf (get-gmap ...) ...)
           #:min-gmap    #:max-gmap         ;; get smallest and largest key/value
           #:map-gmap    #:do-gmap          ;; iterate on gmap or tmap
           #:gmap-keys   #:gmap-values #:gmap-pairs ;; list all keys, values, or pairs
           #:add-to-gmap #:remove-from-gmap ;; add or remove multiple keys

           ;; generic hash table - can be used directly,
           ;; see thash-table for a transactional implementation
           #:ghash-table
           ;; transactional hash table (extends ghash-table)
           #:thash-table

           #:ghash-table-test #:ghash-table-hash
           #:ghash-table-count #:ghash-table-empty? #:clear-ghash
           #:get-ghash #:set-ghash #:rem-ghash   ;; also (setf (get-ghash ... ) ... )
           #:map-ghash #:do-ghash  ;; iterate on ghash-table or thash-table
           ;; list all keys, values, or pairs:
           #:ghash-keys #:ghash-values #:ghash-pairs
           ;; hash function suitable for :test 'equalp
           #:sxhash-equalp

           ;; transactional simple-vector
           #:simple-tvector #:simple-tvector-length
           #:tsvref #:do-simple-tvector

           ;; transactional CONS cell and list
           #:tcons  #:tfirst #:trest  #:tconsp #:tatom  #:tpush  #:tpop
           #:tlist  #:tcar   #:tcdr   #:tcaar  #:tcadr  #:tcdar  #:tcddr
           #:tcaaar #:tcaadr #:tcadar #:tcaddr #:tcdaar #:tcdadr #:tcddar #:tcdddr

           #:tcaaaar #:tcaaadr #:tcaadar #:tcaaddr #:tcadaar #:tcadadr #:tcaddar #:tacdddr
           #:tcdaaar #:tcdaadr #:tcdadar #:tcdaddr #:tcddaar #:tcddadr #:tcdddar #:taddddr

           #:tendp   #:tlist-length #:tnthcdr #:tnth
           #:tsecond #:tthird #:tfourth #:tfifth #:tsixth #:tseventh #:teighth #:tninth #:ttenth
           #:ttree-equal #:ttree-equal-test #:ttree-equal-test-not
           #:tlast   #:tlist* #:make-tlist))
           



