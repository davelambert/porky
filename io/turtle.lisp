;;; Copyright © 2009 The Open University

(in-package #:porky.io.turtle)

(defun parse-turtle (turtle-string)
  (yacc:parse-with-lexer (turtle-lexer turtle-string) turtle-parser))

(defclass turtle-parser ()
  ((db :initarg :db
       :initform nil
       :accessor wilbur::parser-db)))

(defmethod wilbur::parse ((parser turtle-parser) stream locator)
  (setf (wilbur::parser-db parser)
        (turtle-ast-to-rdf-db
         (parse-turtle (read-stream-to-string stream)))))
