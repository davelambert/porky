;;; Copyright © 2009 The Open University

;;; This refuses to compile unless the referenced functions are
;;; available at compile time.  For ASDF, this means the parser has to
;;; be in its own file, dependent on the functions in turtle.lisp

(in-package :porky.io.turtle)

;;; {{{ Grammar
(yacc:define-parser turtle-parser

    (:start-symbol turtle-doc)

  (:terminals (:open-bracket :close-bracket :open-paren :close-paren
               :comma :^^ :period :semicolon
               :@base :@prefix
               :nodeid :prefix :qname :uriref
               :a-for-type
               :boolean :decimal :double :integer :string :long-string :language))

  (:precedence nil)

  (turtle-doc
   (statement*
    #'build-turtle))

  (statement*
   nil
   (statement statement*
              (lambda (s s*) (cons s s*))))

  (statement
   (directive :period (lambda (directive sep)
                                (declare (ignore sep))
                                directive))
   (triples :period (lambda (triples sep)
                      (declare (ignore sep))
                      triples)))

  (directive @prefix
             @base)

  (@base (:@base :uriref
                 (lambda (keyword ref)
                   (declare (ignore keyword))
                   (list :@base ref))))

  (@prefix (:@prefix :prefix :uriref
                     (lambda (keyword prefix-name ref)
                       (declare (ignore keyword ))
                       (list :@prefix prefix-name ref))))

  (triples
   (subject predicate-object-list+
            (lambda (subject pol+)
              (build-triples subject pol+))))

  (resource
   (:uriref #'build-uriref)
   (:qname #'build-qname)
   (:prefix #'build-qname))

  (blank
   (:nodeid
    #'build-nodeid)
   (:open-bracket :close-bracket
    (lambda ([ ])
      (declare (ignore [ ]))
      (build-blank)))
   (:open-bracket predicate-object-list+ :close-bracket
    (lambda ([ predicate-object-list+ ])
      (declare (ignore [ ]))
      (build-blank predicate-object-list+)))
   (collection
    #'passthru))

  (collection
   (:open-paren item-list* :close-paren
    (lambda (open-paren item-list* close-paren)
      (declare (ignore open-paren close-paren))
      (build-collection item-list*))))

  (item-list*
   nil
   (object item-list*
    (lambda (object list)
      (cons object list))))

  (predicate-object-list+
   (predicate object-list+ predicate-object-list*
    (lambda (predicate object-list+ predicate-object-list*)
      (build-predicate-object-list predicate object-list+ predicate-object-list*))))

  (predicate-object-list*
   nil
   (:semicolon predicate object-list+ predicate-object-list*
    (lambda (semicolon predicate object-list+ predicate-object-list*)
      (declare (ignore semicolon ))
      (build-predicate-object-list predicate object-list+ predicate-object-list*))))

  (object-list+
   (object object-list*
    #'build-object-list))

  (object-list*
   nil
   (:comma object object-list*
           (lambda (comma object object-list*)
             (declare (ignore comma))
             (build-object-list object object-list*))))

  (subject
   (resource #'passthru)
   (blank #'passthru))

  (object
   (resource #'passthru)
   (blank #'passthru)
   (literal #'passthru))

  (literal
   (:boolean #'build-boolean)
   (:integer #'build-integer)
   (:decimal #'build-decimal)
   (:double #'build-double)
   (datatype-string #'passthru)
   (quoted-string #'passthru)
   (languaged-string #'passthru))

  (quoted-string
   (:string
    #'build-string)
   (:long-string
    #'build-long-string))

  (languaged-string
   (:string :language
    #'build-string)
   (:long-string :language
    #'build-long-string))

  (datatype-string
   (quoted-string :^^ resource
    (lambda (string ^^ resource)
      (declare (ignore ^^))
      (build-datatype-string string resource))))

  (predicate
   (resource #'passthru)
   (:a-for-type
    #'(lambda (a)
        (declare (ignore a))
        (build-uriref "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")))))
;;; }}}
