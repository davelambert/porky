;;; Copyright © 2009 The Open University

;;; This refuses to compile unless the referenced functions are
;;; available at compile time.  For ASDF, this means the parser has to
;;; be in its own file, dependent on the functions in ntriples.lisp

(in-package :porky.io.ntriples)

;;; {{{ Grammar
(yacc:define-parser ntriples-parser
  (:start-symbol ntriples-doc)
  (:terminals (:^^ :period :nodeid :uriref :string :long-string :language))
  (:precedence nil)

  (ntriples-doc
   (line*
    #'build-ntriples))

  (line*
   nil
   (triple :period line*
    (lambda (l period l*)
      (declare (ignore period))
      (cons l l*))))

  (triple
   (subject predicate object
    #'build-triple))

  (subject
   (nodeid #'passthru)
   (uriref #'passthru))

  (predicate
   (uriref #'passthru))

  (object
   (nodeid #'passthru)
   (uriref #'passthru)
   (literal #'passthru))

  (literal
   (datatype-string #'passthru)
   (quoted-string #'passthru)
   (languaged-string #'passthru))

  (nodeid
   (:nodeid
    #'build-nodeid))

  (uriref
   (:uriref
    #'build-uriref))

  (quoted-string
   (:string
    #'build-string))

  (languaged-string
   (:string :language
    #'build-string))

  (datatype-string
   (quoted-string :^^ resource
    (lambda (string ^^ resource)
      (declare (ignore ^^))
      (build-datatype-string string resource)))))
;;; }}}
