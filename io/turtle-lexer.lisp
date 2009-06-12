;;; Copyright © 2009 The Open University

(in-package :porky.io.turtle)

;;; {{{ Lexer

(deflexer turtle-lexer

    (ignore :whitespace "[\\s\\r\\n]+" :multi-line-mode t)
    (ignore :comment "#.*\\r?\\n" :multi-line-mode t)

    (token :uriref "<[^ \\n>]*>")

    (token :qname "([a-zA-Z][a-zA-Z0-9_-]*)?:[a-zA-Z0-9_-]+")
    (token :prefix "[a-zA-Z][a-zA-Z0-9_-]*:")
    (token :nodeid "_[a-zA-Z0-9_-]*:[a-zA-Z0-9_-]+")

    (token :double "[+-]?[0-9]*\\.[0-9]+[eE][+-]?[0-9]+")
    (token :decimal "[+-]?[0-9]*\\.[0-9]+")
    (token :integer "[+-]?[0-9]+")

    (token :long-string "\"\"\"(\\\\\"|[^\"])*[^\\\\]?\"\"\""
           :multi-line-mode t)
    (token :string "\"(\\\\\"|[^\"\\n])*[^\\\\]?\"")

    (token :boolean "true|false")

    (token :period "\\.")
    (token :semicolon ";")
    (token :prefix ":")
    (token :comma ",")

    (token :open-bracket "\\[")
    (token :close-bracket "\\]")
    (token :open-paren "\\(")
    (token :close-paren "\\)")

    (token :a-for-type "a(?=\\s)")

    (token :^^ "\\^\\^")

    (token :@base "@base")
    (token :@prefix "@prefix")
    (token :language "@[a-z]+(-[a-z0-9]+)*"))

;;; }}}
;;; {{{ Abstract syntax tree construction

(defun passthru (x)
  x)

(defun build-blank (&optional contents)
  (cons :blank contents))

(defun build-nodeid (nodeid)
  (list :nodeid nodeid))

(defun build-qname (r)
  (list :qname r))

(defun build-uriref (r)
  (list :uriref r))

(defun build-object-list (object &optional objects)
  (list* :objects object (rest objects)))

(defun build-collection (objects)
  (list* :collection objects))

(defun build-turtle (statements)
  (cons :turtle statements))

(defun build-triples (subject predicate-objects-lists)
  (cons :triples (cons subject predicate-objects-lists)))

(defun build-predicate-object-list (predicate object-list
                                    &optional predicate-object-lists)
  (cons (list :predicate-object-list predicate object-list)
        predicate-object-lists))

(defun build-boolean (bool)
  (list :boolean bool))

(defun build-integer (integer)
  (list :integer (read-from-string integer)))

(defun build-decimal (decimal)
  (list :decimal (read-from-string decimal)))

(defun build-double (double)
  (list :double (read-from-string double)))

(defun build-string (string &optional language)
  (list* :string (subseq string 1 (- (length string) 1))
         (if language
             (list (subseq language 1)))))

(defun build-long-string (string &optional language)
  (list* :string (subseq string 3 (- (length string) 3))
         (if language
             (list (subseq language 1)))))

(defun build-datatype-string (string datatype)
  (list :datatype-string string datatype))

;;; }}}

;;; {{{ turn lead into gold

(defun intern-in-db (db ast-thing)
  (ecase (first ast-thing)
    ((:blank) (wilbur:node nil))
    ((:boolean)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:boolean))
    ((:decimal)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:decimal))
    ((:double)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:double))
    ((:integer)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:integer))
    ((:string)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:string
                                :language (third ast-thing)))
    ((:nodeid) (intern-bnode db (second ast-thing)))
    ((:qname) (wilbur::unresolved-node (second ast-thing)))
    ((:uriref) (let ((uriref (second ast-thing)))
                (wilbur::node (subseq uriref 1 (- (length uriref) 1)))))))

(defclass turtle-db (wilbur:blank-node-db-mixin wilbur:db)
  ((wilbur::blank-node-uri-prefix :initform "_:")
   (bnodes :reader bnodes-of :initform (make-hash-table :test 'equal))))

(defun intern-bnode (db id)
  (let* ((bnodes (bnodes-of db))
         (bnode (gethash id bnodes)))
    (or bnode
        (let ((b (wilbur:node nil)))
          (setf (gethash id bnodes) b)
          b))))

(defun turtle-ast-to-rdf-db (turtle-ast)
  "Create a Wilbur store and populate with TURTLE-AST."
  (let* ((db (make-instance 'turtle-db))
         (wilbur:*db* db))
    (loop for statement in (rest turtle-ast)
          do (ecase (first statement)
               ((:@prefix)
                  (let ((prefix (subseq (second statement)
                                        0 (- (length (second statement)) 1))))
                    (wilbur:add-namespace prefix (third statement))))
               ((:@base)
                  (error "@base unimplemented"))
               ((:triples)
                  (parse-triples db statement))))
    db))

(defun parse-resource (db statement)
  (case (first statement)
    ((:blank)
       (let ((blank (intern-in-db db statement)))
         (parse-triples-aux db blank (rest statement))
         blank))
    ((:collection)
       (parse-collection db statement))
    (t
       (intern-in-db db statement))))

(defun parse-collection (db collection)
  (labels ((aux (collection)
             (if collection
                 (let* ((tail (aux (rest collection)))
                        (head (parse-resource db (first collection)))
                        (cons (wilbur:node nil)))
                   (wilbur:db-add-triple
                    db (wilbur:db-make-triple db cons !rdf:head head))
                   (wilbur:db-add-triple
                    db (wilbur:db-make-triple db cons !rdf:rest tail))
                   cons)
                 !rdf:nil)))
    (aux (rest collection))))

(defun parse-triples (db statement)
  (parse-triples-aux db (parse-resource db (second statement))
                     (cddr statement)))

(defun parse-triples-aux (db subject predicate-object-lists)
  (loop for predicate-object-list in predicate-object-lists
        do (let ((predicate (intern-in-db db (second predicate-object-list))))
             (loop for object in (rest (third predicate-object-list))
                   do (let* ((object (parse-resource db object))
                             (triple (wilbur:db-make-triple
                                      db subject predicate object)))
                        (wilbur:db-add-triple db triple))))))

;;; }}}

