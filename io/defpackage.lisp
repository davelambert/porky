;;; Copyright © 2009 The Open University

(defpackage #:porky.io
    (:use #:common-lisp)
    (:export #:load-database))

(defpackage #:porky.io.support
    (:use #:common-lisp)
    (:export #:deflexer
             #:read-file-to-string
             #:read-stream-to-string))

(defpackage #:porky.io.turtle
    (:use #:common-lisp #:porky.io.support)
  (:export #:turtle-db #:turtle-parser))

(defpackage #:porky.io.ntriples
    (:use #:common-lisp #:porky.io.support)
  (:export #:ntriples-db #:ntriples-parser))
