;;; Copyright © 2009 The Open University

(defpackage porky-system
  (:use :common-lisp :asdf))

(in-package #:porky-system)

(defsystem :porky
  :description "A semantic web toolkit."
  :version "0"
  :licence "Lisp Lesser GPL"
  :author "Dave Lambert <d.j.lambert@gmail.com>"
  :depends-on (#:cl-ppcre #:wilbur #:yacc)
  :components
  ((:module :io
	    :components
	    ((:file "defpackage")
             (:file "io"
                    :depends-on ("defpackage"))
             (:file "support"
                    :depends-on ("defpackage"))

             (:file "ntriples-lexer"
                    :depends-on ("defpackage" "support"))
             (:file "ntriples-grammar"
                    :depends-on ("defpackage" "support" "ntriples-lexer"))
             (:file "ntriples"
                    :depends-on ("defpackage" "io" "ntriples-grammar"))

             (:file "turtle-lexer"
                    :depends-on ("defpackage" "support"))
             (:file "turtle-grammar"
                    :depends-on ("defpackage" "support" "turtle-lexer"))
             (:file "turtle"
                    :depends-on ("defpackage" "io" "turtle-grammar"))))))
