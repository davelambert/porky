;;; Copyright © 2009 The Open University

(in-package #:porky.io.ntriples)

(defun parse-ntriples (ntriples-string)
  (yacc:parse-with-lexer (ntriples-lexer ntriples-string) ntriples-parser))

(defclass ntriples-parser ()
  ((db :initarg :db
       :initform nil
       :accessor wilbur::parser-db)))

(defmethod wilbur::parse ((parser ntriples-parser) stream locator)
  (setf (wilbur::parser-db parser)
        (ntriples-ast-to-rdf-db
         (parse-ntriples (read-stream-to-string stream)))))
