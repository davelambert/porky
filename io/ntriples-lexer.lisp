;;; Copyright © 2009 The Open University

(in-package :porky.io.ntriples)

;;; {{{ Lexer

(deflexer ntriples-lexer

  ;; XXX what does :multi-line-mode do in a line-based format?!
    (ignore :whitespace "[\\s]+" :multi-line-mode t)
    (ignore :comment "#.*\\r?\\n" :multi-line-mode t)

    ;; XXX multilines handled how?
    (token :newline "[\\r\\n]")

    (token :uriref "<[^ \\n>]*>")

    (token :nodeid "_[a-zA-Z0-9_-]*:[a-zA-Z0-9_-]+")

    (token :string "\"(\\\\\"|[^\"\\n])*[^\\\\]?\"")

    (token :period "\\.")

    (token :^^ "\\^\\^")

    (token :language "@[a-z]+(-[a-z0-9]+)*"))

;;; }}}
;;; {{{ Abstract syntax tree construction

(defun passthru (x)
  x)

(defun build-nodeid (nodeid)
  (list :nodeid nodeid))

(defun build-uriref (r)
  (list :uriref r))

(defun build-ntriples (statements)
  (cons :ntriples statements))

(defun build-triple (subject predicate object)
  (list :triple subject predicate object))

(defun build-string (string &optional language)
  (list* :string (subseq string 1 (- (length string) 1))
         (if language
             (list (subseq language 1)))))

(defun build-datatype-string (string datatype)
  (list :datatype-string string datatype))

;;; }}}

;;; {{{ turn lead into gold

(defun intern-in-db (db ast-thing)
  (ecase (first ast-thing)
    ((:string)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:string
                                :language (third ast-thing)))
    ((:nodeid) (intern-bnode db (second ast-thing)))
    ((:uriref) (let ((uriref (second ast-thing)))
                (wilbur::node (subseq uriref 1 (- (length uriref) 1)))))))

(defclass ntriples-db (wilbur:blank-node-db-mixin wilbur:db)
  ((wilbur::blank-node-uri-prefix :initform "_:")
   (bnodes :reader bnodes-of :initform (make-hash-table :test 'equal))))

(defun intern-bnode (db id)
  (let* ((bnodes (bnodes-of db))
         (bnode (gethash id bnodes)))
    (or bnode
        (let ((b (wilbur:node nil)))
          (setf (gethash id bnodes) b)
          b))))

(defun ntriples-ast-to-rdf-db (ntriples-ast)
  "Create a Wilbur store and populate with NTRIPLES-AST."
  (assert (eq :ntriples (first ntriples-ast)))
  (let* ((db (make-instance 'ntriples-db))
         (wilbur:*db* db))
    (loop for triple in (rest ntriples-ast)
          do (apply #'parse-triple db (rest triple)))
    db))

(defun parse-triple (db subject predicate object)
  (let ((s (intern-in-db db subject))
        (p (intern-in-db db predicate))
        (o (intern-in-db db object)))
    (wilbur:db-add-triple db (wilbur:db-make-triple db s p o))))

;;; }}}

