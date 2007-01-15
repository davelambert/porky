;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  rdf-parser.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a (the
;;;   "License"); you may not use this file except in compliance with the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
;;;   WARRANTY OF ANY KIND, either express or implied. See the License for the specific
;;;   language governing rights and limitations under the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2005 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;                   Louis Theran <theran@pobox.com>
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: rdf-parser.lisp,v 1.3 2006/02/07 01:50:11 ora Exp $
;;;
;;;   Purpose: This file contains an implementation of an RDF parser, using a
;;;   "near streaming" algorithm based on a simple state machine. The parser
;;;   implements all of RDF M+S excluding "aboutEachPrefix" (what, are you
;;;   surprised?) as well as RDFCore.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-SYNTAX-NORMALIZER
;;;

(defclass rdf-syntax-normalizer (sax-filter)
  ())

(defmethod sax-consumer-mode ((self rdf-syntax-normalizer))
  (sax-consumer-mode (sax-producer-consumer self)))

(defmethod start-element ((self rdf-syntax-normalizer)
			  (tag open-tag)
			  mode)
  (let ((attributes (tag-attributes tag))
        (properties nil)
        (consumer (sax-producer-consumer self))
        (namespaces (tag-namespaces tag)))
    (do-string-dict (key value attributes)
      (cond ((null (find key -rdf-attrs- :test #'string=))
             (setf properties (string-dict-add properties key value)
                   attributes (string-dict-del attributes key)))
            ((string= key -rdf-abouteachprefix-uri-)
             (cerror "Ignore" 'feature-not-supported :thing "aboutEachPrefix")
             (setf attributes (string-dict-del attributes key)))))
    (setf (tag-attributes tag) attributes)
    (start-element consumer tag mode)
    (do-string-dict (key value properties)
      (unless (or (string= key -xml-lang-attr-)
                  (string= key "xml:space")
                  (string= key "xml:base"))
        (let ((new-tag (make-instance 'open-tag
                         :string key
                         :base (tag-base tag)
                         :namespaces namespaces)))
          (start-element consumer new-tag (sax-consumer-mode self))
          (char-content consumer value (sax-consumer-mode self))
          (end-element consumer new-tag (sax-consumer-mode self)))))))

(defmethod maybe-use-namespace ((self rdf-syntax-normalizer) prefix uri)
  (maybe-use-namespace (sax-producer-consumer self) prefix uri))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-PARSER
;;;

(defclass temporary-parser-db (interned-literal-db-mixin fast-temporary-db)
  ())

(defclass rdf-parser (sax-consumer)
  ((base
    :initform nil
    :accessor parser-base)
   (locator
    :initform nil
    :accessor parser-locator)
   (db
    :initarg :db
    :initform nil
    :accessor parser-db)
   (states
    :initform nil
    :accessor parser-states)
   (literal
    :accessor parser-literal)
   (rdfcorep 
    :initarg rdfcorep
    :initform t
    :accessor parser-rdfcore-p)
   (harvest-namespaces-p
    :initarg :harvest-namespaces-p
    :initform t
    :reader parser-harvest-namespaces-p)
   (db-class
    :initarg :db-class
    :initform 'temporary-parser-db
    :reader parser-db-class)
   (initial-state
    :initarg :initial-state
    :initform :scan
    :reader parser-initial-state)
   (load-queue
    :initform nil
    :accessor parser-load-queue))
  (:default-initargs
    :producer (make-instance 'xml-parser
			     :consumer (make-instance 'rdf-syntax-normalizer))))

(define-condition close-rdf-element (condition)
  ())

(defstruct (state (:constructor make-state (mode
                                            &optional node
                                                      property
                                                      statement-id
                                                      language
                                                      datatype)))
  mode
  node
  triple
  property
  (statement-id nil)
  (language nil)
  (datatype nil)
  (index 0)
  task-queue)

(defmethod add-state ((parser rdf-parser) mode &rest args)
  (declare (dynamic-extent args))
  (push (apply #'make-state mode args) (parser-states parser)))

(defmethod parser-task-state ((parser rdf-parser))
  (find :description (parser-states parser) :key #'state-mode))

(defmethod sax-consumer-mode ((parser rdf-parser))
  (state-mode (first (parser-states parser))))

(defstruct (task (:constructor make-task (type node &rest parameters)))
  type
  node
  parameters)

(defmacro task-parameter (task parameter)
  `(getf (task-parameters ,task) ,parameter))

(defmethod defer-task ((parser rdf-parser) type node &rest args)
  (declare (dynamic-extent args))
  (pushnew (apply #'make-task type node args)
           (state-task-queue (parser-task-state parser))
           :test #'(lambda (p q)
                     (and (eq (task-type p) (task-type q))
                          (eq (task-node p) (task-node q))))))

(defmethod make-container ((parser rdf-parser)
                           elements
                           &optional container-uri
                                     (container-type-uri -rdf-bag-uri-))
  (let ((node (ensure-node parser container-uri nil))
	(i 0))
    (add-as-triple parser node
		   (ensure-node parser -rdf-type-uri- t)
		   (ensure-node parser container-type-uri t))
    (dolist (element elements)
      (add-as-triple parser node (index-uri (incf i) *db*) element)) ; *db* or parser-db?
    node))

(defmethod initialize-instance :after ((parser rdf-parser) &key &allow-other-keys)
  (let ((normalizer (sax-producer-consumer (sax-consumer-producer parser))))
    (setf (sax-producer-consumer normalizer) parser)
    (unless (parser-db parser)
      (setf (parser-db parser) (make-instance (parser-db-class parser) :emptyp t)))))

(defmethod uri ((parser rdf-parser) uri should-exist-p)
  (let* ((base (first (parser-base parser)))
         (ends-in-hash-p (char= (char base (1- (length base))) #\#)))
    (cond ((or (null uri) (find #\: uri :test #'char=) (char= (char uri 0) #\/))
           uri)
          ((char= (char uri 0) #\#)
           (concatenate 'string base (if ends-in-hash-p (subseq uri 1) uri)))
          ((not should-exist-p)
           (concatenate 'string base (if ends-in-hash-p nil "#") uri))
          (t
           uri))))

(defmethod ensure-node ((parser rdf-parser) uri should-exist-p)
  (cond ((and (stringp uri) (zerop (length uri)))
	 (node (first (parser-base parser))))
	((typep uri 'node)
	 uri)
	(t
	 (node (uri parser uri should-exist-p)))))

(defmethod parse ((parser rdf-parser) stream locator)
  (setf (parser-load-queue parser) nil)
  (catch :terminate-rdf-parser
    (parse (find-first-producer parser) stream locator))
  (dolist (url (parser-load-queue parser))
    (ensure-ontology-loaded parser url))
  (parser-locator parser))

(defmethod ensure-ontology-loaded ((parser rdf-parser) url)
  ;; simplistic ontology loader, can be overridden
  (cond ((db-query *db* (node url) !rdf:type !owl:Ontology)
	 (format t "owl:imports ~a already done~%" url)
	 nil)
	(t
	 (format t "~&Import of ~S~%" url)
	 (db-load *db* url :merge-results-p t :verbosep nil)
	 t)))

(defmethod add-as-triple ((parser rdf-parser)
                          (subject node)
                          (predicate string)
                          object
                          &optional statement-id)
  (add-as-triple parser subject (ensure-node parser predicate t) object statement-id))

(defmethod add-as-triple ((parser rdf-parser)
                          (subject node)
                          (predicate node)
                          object
                          &optional statement-id)
  (let* ((db (parser-db parser))
         (source (parser-locator parser))
	 (triple (db-make-triple db subject predicate object source)))
    (db-add-triple db triple)
    (dolist (state (parser-states parser))
      ;; for higher order statements
      (dolist (task (state-task-queue state))
        (when (and (eq (task-type task) :bagid)
                   (eq subject (task-node task)))
          (push (cons triple statement-id) (task-parameter task :statements))
          (return-from add-as-triple triple))))
    (when statement-id
      ;; no bagid but statement-id exists
      (db-reify triple db (uri parser statement-id nil) source))
    triple))

(defun new-index-uri (parser db)
  (index-uri (incf (state-index (first (parser-states parser)))) db))

(defun parse-db-from-stream (stream locator
                             &rest options
                             &key (parser-class 'rdf-parser)
                             &allow-other-keys)
  (declare (dynamic-extent options))
  (remf options :parser-class)
  (multiple-value-bind (source-node parser)
		       (apply #'parse-from-stream
			      stream locator parser-class options)
    (values (parser-db parser) source-node)))

(defmethod maybe-use-namespace ((self rdf-parser) prefix uri)
  (when (and (parser-harvest-namespaces-p self)
             (not (string-dict-get (dictionary-namespaces *nodes*) prefix)))
    (add-namespace prefix uri)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF PARSER STATE MACHINE TRANSITIONS
;;;

(defmethod start-document ((parser rdf-parser)
			   locator)
  (setf (parser-locator parser) (node locator)
	(parser-base parser) (list locator))
  (add-state parser (parser-initial-state parser)))

(defmethod end-document ((parser rdf-parser)
			 mode)
  (declare (ignore mode))
  nil)

(defmethod start-element :before ((parser rdf-parser)
				  (tag open-tag)
				  mode)
  (declare (ignore mode))
  (push (tag-base tag) (parser-base parser)))

(defmethod end-element :after ((parser rdf-parser)
			       (tag open-tag)
			       mode)
  (declare (ignore mode))
  (pop (parser-base parser)))

(defmethod start-element ((parser rdf-parser)
			  (tag open-tag)
			  (mode (eql :scan)))
  (cond ((string= (token-string tag) -rdf-rdf-uri-)
         (add-state parser :description))
        ((string= (token-string tag) -rdf-description-uri-)
         (start-element parser tag :description))))

(defmethod start-element ((parser rdf-parser)
			  (tag open-tag)
			  (mode (eql :description)))
  (let ((each   (tag-attribute tag -rdf-abouteach-uri-))
	(about  (tag-attribute tag -rdf-about-uri-))
	(id     (tag-attribute tag -rdf-id-uri-))
	(nodeid (tag-attribute tag -rdf-nodeid-uri-)))
    (when (and about id)
      (cerror "Use \"about\"" 'about-and-id-both-present))
    (when (and (or about id) nodeid)
      (cerror "Ignore \"nodeID\"" 'about-and-nodeid-both-present)
      (setf nodeid nil))
    (let ((type   (token-string tag))
	  (node   (ensure-node parser
			       (and (null each) (or about id))
			       (and (null each) (null id)))))
      (if each
	(if (parser-rdfcore-p parser)
	  (cerror "Ignore \"aboutEach\"" 'feature-disabled :feature "aboutEach")
	  (defer-task parser :abouteach node :target (ensure-node parser each t)))
	(let* ((bagid (tag-attribute tag -rdf-bagid-uri-))
	       (state (first (parser-states parser)))
	       (parent (state-node state)))
	  (when bagid
	    (defer-task parser :bagid node :bagid bagid :statements nil))
	  (when parent
	    (attach-to-parent parser parent node (state-statement-id state)))))
      (unless (string= type -rdf-description-uri-)
	(add-as-triple parser node -rdf-type-uri- (ensure-node parser type t)))
      (add-state parser :property node))))

(defmethod attach-to-parent ((parser rdf-parser)
                             (parent node)
                             (child node)
                             &optional statement-id)
  (let ((state (first (parser-states parser))))
    (cond ((eq (state-mode state) :collection)
           (setf parent (state-node state))
           (add-as-triple parser parent -rdf-first-uri- child)
	   (setf (state-triple state)
		 (add-as-triple parser parent -rdf-rest-uri-
				(setf (state-node state) (ensure-node parser nil t)))))
	  ((eq (state-mode state) :daml-collection)
	   (let ((parent (state-node state))
		 (node (ensure-node parser nil t)))
	     (add-as-triple parser parent -rdf-type-uri-
			    (ensure-node parser -daml-list-uri- t))
	     (add-as-triple parser parent -daml-first-uri- child)
	     (add-as-triple parser parent -daml-rest-uri- node)
	     (setf (state-node state) node)))
	  (t
	   (add-as-triple parser parent
                          (state-property (first (parser-states parser)))
                          child statement-id)))))
    
(defmethod start-element ((parser rdf-parser)
			  (tag open-tag)
			  (mode (eql :property)))
  (when (string= (token-string tag) -owl-imports-uri-)
    (pushnew (tag-attribute tag -rdf-resource-uri-) (parser-load-queue parser)))
  (let* ((state (first (parser-states parser)))
         (node (state-node state))
         (property-uri (token-string tag))
         (property (ensure-node parser
                                (cond ((string= property-uri -rdf-li-uri-)
                                       ;; (defer-task parser :container node)
                                       (new-index-uri parser *db*)) ; *db* or parser-db?
                                      (t
                                       property-uri))
                                t))
         (resource-uri (tag-attribute tag -rdf-resource-uri-))
         (statement-id (tag-attribute tag -rdf-id-uri-)))
    (if resource-uri
      (let ((value (ensure-node parser resource-uri t)))
        (setf (state-property state) property)
        (attach-to-parent parser node value statement-id)
        (add-state parser :property value))
      (parse-using-parsetype parser node property
                             (tag-attribute tag -rdf-parsetype-uri-)
                             statement-id
                             (tag-attribute tag -xml-lang-attr-)
                             (tag-attribute tag -rdf-datatype-uri-)))))

(defmethod parse-using-parsetype ((parser rdf-parser) node property parsetype
                                  &optional statement-id language datatype)
  (cond ((null parsetype)
         (add-state parser :description node property statement-id language datatype))
        ((string-equal parsetype "Literal")
         (setf (parser-literal parser) nil)
         (add-state parser :literal node property))
        ((string-equal parsetype "Resource")
         (add-as-triple parser node property (setf node (ensure-node parser nil t))
                        statement-id)
         (add-state parser :property node))
        ((string-equal parsetype "Collection") ; adapted from daml-parser
         (let ((list-node (ensure-node parser nil t)))
           (add-as-triple parser node property list-node)
           (add-state parser :collection list-node)))
	((string= parsetype "daml:collection")
	 (let ((list-node (ensure-node parser nil t)))
	   (add-as-triple parser node property list-node)
	   (add-state parser :daml-collection list-node)))
        (t
         (cerror "Ignore parseType" 'unknown-parsetype :thing parsetype))))
  
(defmethod start-element ((parser rdf-parser)
			  (tag open-tag)
			  (mode (eql :literal)))
  (add-state parser :literal)
  (push tag (parser-literal parser)))

(declaim (special *db*))

(defmethod end-element ((parser rdf-parser)
			(tag open-tag)
			(mode (eql :literal)))
  (let ((state (first (parser-states parser))))
    (call-next-method)
    (cond ((not (null (state-node state)))
	   (let ((string (with-output-to-string (s)
			   (replay (make-instance 'xml-formatter :stream s)
				       (nreverse (parser-literal parser))))))
	     (add-as-triple parser
			    (state-node state)
			    (state-property state)
			    (db-make-literal (or *db* (parser-db parser)) string))))
	  ((not (tag-empty-p tag))
	   (push (tag-counterpart tag) (parser-literal parser))))))

(defmethod end-element ((parser rdf-parser)
			(tag open-tag)
			(mode (eql :scan)))
  nil)

(defmethod end-element :after ((parser rdf-parser)
			       (tag open-tag)
			       (mode (eql :property)))
  (let ((state (parser-task-state parser)))
    (when state
      (dolist (task (shiftf (state-task-queue state) nil))
	(execute-deferred-task parser task (task-type task))))))

(defmethod execute-deferred-task ((parser rdf-parser) task type)
  (let ((db (parser-db parser))
	(source (node (first (parser-base parser)))))
    (ecase type
      ;; (:container
      ;;  (is-container-p db (task-node task) t)
      ;;  t)
      (:abouteach
       (let ((target (task-parameter task :target))
	     (index-predicates nil))
	 (is-container-p db target t)
	 (dolist (triple (db-query db target nil nil))
	   (let ((uri (node-uri (triple-predicate triple))))
	     (when (find uri *index-uris* :test #'string=)
	       (push (ensure-node parser uri t) index-predicates))))
	 (dolist (triple (db-query db (task-node task) nil nil))
	   (db-del-triple db triple)
	   (dolist (p index-predicates)
	     (add-as-triple parser
			    (triple-object (first (db-query db target p nil)))
			    (triple-predicate triple)
			    (triple-object triple)
			    source)))))
      (:bagid
       (let ((statements (task-parameter task :statements)))
	 (when statements
	   (make-container parser
			   (mapcar #'(lambda (s)
				       (destructuring-bind (triple . id) s
					 (db-reify triple db
						   (and id (uri parser id nil))
						   source)))
				   statements)
			   (uri parser (task-parameter task :bagid) nil))))))))

(defmethod end-element :after ((parser rdf-parser)
			       (tag open-tag)
			       (mode (eql :description)))
  (when (string= (token-string tag) -rdf-rdf-uri-)
    (signal 'close-rdf-element)))

(defmethod end-element ((parser rdf-parser)
			(tag open-tag)
			mode)
  (declare (ignore mode))
  (pop (parser-states parser)))

(defmethod char-content ((parser rdf-parser)
			 (content string)
			 (mode (eql :description)))
  (let* ((state (first (parser-states parser)))
         (datatype (state-datatype state)))
    (add-as-triple parser
                   (state-node state)
                   (state-property state)
                   (db-make-literal (or *db* (parser-db parser)) content
				    :language (state-language state)
				    :datatype (and datatype (node datatype)))
                   (state-statement-id state))))

(defmethod char-content ((parser rdf-parser)
			 (content string)
			 (mode (eql :literal)))
  (push content (parser-literal parser)))

(defmethod char-content ((parser rdf-parser)
			 (content string)
			 (mode (eql :scan)))
  ;; ignore character content in :scan mode
  nil)

(defmethod char-content ((parser rdf-parser)
			 (content string)
			 mode)
  (declare (ignore mode))
  (cerror "Ignore" 'illegal-character-content :thing content))

(defmethod start-element ((parser rdf-parser)
			  (tag open-tag)
			  (mode (eql :collection)))
  (start-element parser tag :description))

(defmethod start-element ((parser rdf-parser)
			  (tag open-tag)
			  (mode (eql :daml-collection)))
  (start-element parser tag :description))

(defmethod end-element :before ((parser rdf-parser)
				(tag open-tag)
				(mode (eql :collection)))
  (let* ((db (parser-db parser))
         (triple (db-del-triple db (state-triple (first (parser-states parser))))))
    (add-as-triple parser
                   (triple-subject triple)
                   (triple-predicate triple)
                   (ensure-node parser -rdf-nil-uri- t))))

(defmethod end-element :before ((parser rdf-parser)
				(tag nox:open-tag)
				(mode (eql :daml-collection)))
  (let* ((node (state-node (first (parser-states parser))))
         (db (parser-db parser))
         (triple (db-del-triple db (first (db-query db nil nil node)))))
    (add-as-triple parser
                   (triple-subject triple)
                   (triple-predicate triple)
                   (ensure-node parser -daml-nil-uri- t))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CONSTRUCTORS FOR COLLECTIONS
;;;

(defun rdf-list (&rest items)
  (declare (dynamic-extent items))
  (if items
    (rdf-cons (first items) (apply #'rdf-list (rest items)))
    !rdf:nil))

(defun rdf-cons (first rest &optional uri)
  (let ((pair (node uri)))
    (add-triple (triple pair !rdf:type (node -rdf-list-uri-)))
    (add-triple (triple pair (node -rdf-first-uri-) first))
    (add-triple (triple pair (node -rdf-rest-uri-) rest))
    pair))

(defun daml-list (&rest items)
  (if items
    (daml-cons (first items) (apply #'daml-list (rest items)))
    !daml:nil))

(defun daml-cons (first rest &optional uri)
  (let ((pair (node uri)))
    (add-triple (triple pair !rdf:type (node -daml-list-uri-)))
    (add-triple (triple pair (node -daml-first-uri-) first))
    (add-triple (triple pair (node -daml-rest-uri-) rest))
    pair))
