;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  data.lisp
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
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;
;;;   Purpose: This file contains functionality for managing "RDF data", namely nodes,
;;;   triples, etc.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   DATA MANAGEMENT PROTOCOL
;;;

;;;   NODE PROTOCOL
(defgeneric node-uri (node))
(defgeneric (setf node-uri) (uri node))
(defgeneric node-name-resolved-p (node))
(defgeneric (setf node-name-resolved-p) (resolvedp node))
(defgeneric node-name (node))

;;;   DICTIONARY PROTOCOL
(defgeneric dictionary-nodes (dictionary))
(defgeneric dictionary-namespaces (dictionary))
(defgeneric (setf dictionary-namespaces) (namespaces dictionary))
(defgeneric dictionary-unresolved-nodes (dictionary))
(defgeneric (setf dictionary-unresolved-nodes) (nodes dictionary))
(defgeneric dictionary-node-class (dictionary))
(defgeneric dictionary-add-namespace (dictionary prefix uri))
(defgeneric dictionary-remove-namespace (dictionary prefix))
(defgeneric dictionary-rename-namespace (dictionary old-prefix new-prefix))
(defgeneric dictionary-make-node (dictionary uri))
(defgeneric find-node (dictionary uri))
(defgeneric (setf find-node) (node dictionary uri))
(defgeneric find-unresolved-nodes (dictionary))
(defgeneric dictionary-apropos-list (dictionary pattern))

;;;   TRIPLE PROTOCOL
#|
(defgeneric triple-subject (triple))
(defgeneric triple-predicate (triple))
(defgeneric triple-object (triple))
(defgeneric triple-sources (triple))
(defgeneric (setf triple-sources) (sources triple))
|#

;;;   DB PROTOCOL
(defgeneric db-triples (db))
(defgeneric (setf db-triples) (triples db))
(defgeneric db-literal-class (db))
(defgeneric db-make-literal (db string &rest options))
(defgeneric db-make-triple (db subject predicate object &optional source))
(defgeneric db-add-triple (db triple))
(defgeneric db-del-triple (db triple &optional source))
(defgeneric db-del-source (db source))
(defgeneric db-query-by-source (db source))
(defgeneric db-sources (db))
(defgeneric db-query (db subject predicate object))
(defgeneric db-reify (triple db &optional statement-uri source))
(defgeneric is-container-p (db node &optional errorp))
(defgeneric db-new-container-membership-property (db property))
(defgeneric db-merge (to from &optional source))
(defgeneric db-clear (db))
(defgeneric db-find-cbd (db node))
(defgeneric db-node-local-properties (db node))
(defgeneric db-load (db source &rest options))
;;(defgeneric db-load-using-source (db source &rest options))


;;; --------------------------------------------------------------------------------------
;;;
;;;   TOP-LEVEL NODE API
;;;

(declaim (special *nodes*)) ; forward reference

(defun node (thing)
  (etypecase thing
    (string
     (or (find-node *nodes* thing)
         (setf (find-node *nodes* thing) (dictionary-make-node *nodes* thing))))
    (null
     (dictionary-make-node *nodes* nil))
    (url
     (node (url-string thing)))
    (node
     thing)))

(defun add-namespace (prefix uri)
  (dictionary-add-namespace *nodes* prefix uri))

(defun del-namespace (prefix)
  (dictionary-remove-namespace *nodes* prefix))

(defun namespaces ()
  (mapcar #'first (dictionary-namespaces *nodes*)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF CONDITION CLASSES
;;;
;;;   RDF-ERROR                       abstract
;;;     FEATURE-NOT-SUPPORTED         concrete, continuable
;;;     ABOUT-AND-ID-BOTH-PRESENT     concrete, continuable
;;;     ABOUT-AND-NODEID-BOTH-PRESENT concrete, continuable
;;;     UNKNOWN-PARSETYPE             concrete, continuable
;;;     ILLEGAL-CHARACTER-CONTENT     concrete, continuable
;;;     CONTAINER-REQUIRED            concrete, continuable
;;;     OUT-OF-SEQUENCE-INDEX         concrete
;;;     DUPLICATE-NAMESPACE-PREFIX    concrete
;;;     QUERY-SYNTAX-ERROR            concrete
;;;     DATATYPE-PARSE-ERROR          concrete, continuable
;;;

(define-condition rdf-error (wilbur-error)
  ())

(define-condition feature-not-supported (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- ~S not supported"))

(define-condition feature-disabled (rdf-error)
  ()
  (:default-initargs 
    :format-control "RDF -- ~S is disabled"))

(define-condition about-and-id-both-present (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- \"about\" and \"ID\" both present"))

(define-condition about-and-nodeid-both-present (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- \"about\" and \"nodeID\" both present"))

(define-condition unknown-parsetype (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- unknown parsetype ~S"))

(define-condition illegal-character-content (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- character content not allowed: ~S"))

(define-condition container-required (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- ~S is not a container"))

(define-condition out-of-sequence-index (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- index URI ~S allocated out of sequence"))

(define-condition duplicate-namespace-prefix (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- duplicate namespace prefix ~S"))

(define-condition query-syntax-error (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- no operands for query operator ~A"))

(define-condition cannot-invert-default-value (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- cannot invert a default value expression ~S"))

(define-condition datatype-parse-error (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- cannot parse datatype literal ~S"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE
;;;

(defclass node ()
  ((uri
    :initarg :uri
    :initform nil
    :accessor node-uri)
   (name-resolved-p
    :initarg :name-resolved-p
    :initform t
    :accessor node-name-resolved-p)))

(defmethod print-object ((node node) stream)
  (declare (special *nodes*)) ; forward ref.
  (let ((uri (node-uri node)))
    (cond ((null uri)
           (print-unreadable-object (node stream :type t :identity t)
	     (princ "--" stream)))
          ((node-name-resolved-p node)
           (multiple-value-bind (name successp)
                                (find-short-name *nodes* uri)
             (format stream "!~:[~S~;~A~]" successp name)))
          (t
           (format stream "!~A" uri)))))

(defmethod node-name ((node node))
  (let ((uri (node-uri node)))
    (when uri
      (if (node-name-resolved-p node)
        (find-short-name *nodes* uri)
        uri))))

(defmethod make-load-form ((node node) &optional env)
  (declare (ignore env))
  (if (node-name-resolved-p node)
    `(node ,(node-uri node))
    `(unresolved-node ,(node-uri node))))

(defvar *index-uris* (make-array 32 :fill-pointer 0 :adjustable t))

(defun index-uri (index db)
  (let ((delta (- (length *index-uris*) index)))
    (cond ((>= delta 0)
           (elt *index-uris* (1- index)))
          ((= delta -1)
           (let ((u (node (rdf-uri (format nil "_~S" index)))))
             (vector-push-extend u *index-uris*)
	     ;; doing path-based reasoning may require this:
             (db-new-container-membership-property db u)
             u))
          (t
           (error 'out-of-sequence-index :thing index)))))

(defun index-uri-p (node)
  (find node *index-uris*))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS DICTIONARY
;;;

(defclass dictionary ()
  ((nodes
    :initform (make-hash-table :test #'equal)
    :initarg :nodes
    :reader dictionary-nodes)
   (namespaces
    :initform nil
    :accessor dictionary-namespaces)
   (unresolved-nodes
    :initform (make-hash-table :test #'equal)
    :initarg :unresolved-nodes
    :accessor dictionary-unresolved-nodes)
   (node-class
    :initarg :node-class
    :initform 'node
    :reader dictionary-node-class)))

(defmethod initialize-instance :after ((self dictionary) &rest args)
  (declare (ignore args))
  (dictionary-add-namespace self "rdf" -rdf-uri-)
  (dictionary-add-namespace self "rdfs" -rdfs-uri-)
  (dictionary-add-namespace self "owl" -owl-uri-)
  (dictionary-add-namespace self "xsd" -xsd-uri-))

(defmethod dictionary-add-namespace ((dictionary dictionary) prefix uri)
  (let* ((namespaces (dictionary-namespaces dictionary))
	 (old-uri (string-dict-get namespaces prefix)))
    (cond ((null old-uri)
	   (setf (dictionary-namespaces dictionary)
		 (string-dict-add namespaces prefix uri))
	   (maphash #'(lambda (name node)
			(let ((uri (find-long-name dictionary name)))
			  (when uri
			    (remhash name (dictionary-unresolved-nodes dictionary))
			    (setf (node-uri node) uri
				  (node-name-resolved-p node) t
				  (find-node dictionary uri) node))))
		    (dictionary-unresolved-nodes dictionary)))
	  ((not (string= uri old-uri))
	   (setf prefix (string-downcase (symbol-name (gentemp prefix))))
	   (setf (dictionary-namespaces dictionary)
		 (string-dict-add namespaces prefix uri))))
    prefix))

(defmethod dictionary-remove-namespace ((dictionary dictionary) prefix)
  (setf (dictionary-namespaces dictionary)
        (string-dict-del (dictionary-namespaces dictionary) prefix))
  prefix)

(defmethod dictionary-rename-namespace ((dictionary dictionary)
                                        old-prefix new-prefix)
  (if (string-dict-get (dictionary-namespaces dictionary) new-prefix)
    (error 'duplicate-namespace-prefix :thing new-prefix)
    (let ((uri (string-dict-get (dictionary-namespaces dictionary) old-prefix)))
      (dictionary-remove-namespace dictionary old-prefix)
      (dictionary-add-namespace dictionary new-prefix uri)
      new-prefix)))

(defmethod dictionary-make-node ((dictionary dictionary) uri)
  (make-instance (dictionary-node-class dictionary) :uri uri))

(defmethod find-node ((dictionary dictionary) uri)
  (when uri
    (gethash uri (dictionary-nodes dictionary))))

(defmethod (setf find-node) (node (dictionary dictionary) uri)
  (when uri
    (setf (gethash uri (dictionary-nodes dictionary)) node)))

(defun find-short-name (dictionary uri)
  (reverse-expand-name uri (dictionary-namespaces dictionary)))

(defun find-long-name (dictionary name)
  (expand-name-with-namespace name (dictionary-namespaces dictionary)))

(defun unresolved-node (name)
  (let ((uri (find-long-name *nodes* name)))
    (if uri
      (node uri)
      (let ((unresolved (dictionary-unresolved-nodes *nodes*)))
        (or (gethash name unresolved)
            (setf (gethash name unresolved)
                  (make-instance 'node :uri name :name-resolved-p nil)))))))

(defmethod find-unresolved-nodes ((dictionary dictionary))
  (let ((nodes nil))
    (maphash #'(lambda (uri node)
                 (declare (ignore uri))
                 (push node nodes))
             (dictionary-unresolved-nodes dictionary))
    nodes))

(defmethod dictionary-apropos-list ((dictionary dictionary)
                                    (pattern string))
  (let ((nodes nil))
    (maphash #'(lambda (name node)
		 (when (name-contains-pattern-p name pattern)
		   (push node nodes)))
	     (dictionary-nodes dictionary))
    (sort nodes #'string< :key #'node-uri)))

(defun name-contains-pattern-p (name pattern)
  ;; NOTE: This is a naive string search algorithm; I will switch to, say, Boyer-Moore
  ;;  when I have more time.
  (let ((nn (length name))
        (np (length pattern)))
    (cond ((= nn np)
           (string= name pattern))
          ((> nn np)
           (dotimes (i (- nn np -1))
             (when (string= name pattern :start1 i :end1 (+ i np))
               (return-from name-contains-pattern-p t)))))))

(defvar *nodes* (make-instance 'dictionary))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun inline-node-reader (stream char)
    (declare (ignore char))
    (if (char= (peek-char nil stream t nil t) #\")
      (node (read stream t nil t))
      (unresolved-node (read-using *name-reader* stream t))))

  (defun enable-node-shorthand ()
    (set-macro-character #\! #'inline-node-reader))

  (enable-node-shorthand))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS TRIPLE
;;;

#+:wilbur-triples-as-classes
(defclass triple ()
  ((subject
    :initarg :subject
    :reader triple-subject)
   (predicate
    :initarg :predicate
    :reader triple-predicate)
   (object
    :initarg :object
    :accessor triple-object)
   (sources
    :initarg :sources
    :initform nil
    :accessor triple-sources)))

#-:wilbur-triples-as-classes
(defstruct (triple
	     (:constructor %make-triple (subject predicate object &optional sources)))
  subject
  predicate
  object
  sources)

(defmethod print-object ((triple triple) stream)
  (print-unreadable-object (triple stream :type t :identity t)
    (format stream "~S ~S ~S"
            (triple-subject triple)
            (triple-predicate triple)
            (triple-object triple))))

(defmethod triple= ((triple1 triple) (triple2 triple))
  (and (eq (triple-subject triple1) (triple-subject triple2))
       (eq (triple-predicate triple1) (triple-predicate triple2))
       (eq (triple-object triple1) (triple-object triple2))))

(defmethod triple= (thing1 thing2)
  (declare (ignore thing1 thing2))
  nil)


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS DB
;;;

(defclass db ()
  ((triples
    :initform nil
    :accessor db-triples)
   (source-descs
    :initform nil
    :accessor db-source-descs)
   (literal-class
    :initarg :literal-class
    :initform 'literal ; could also be STRING or any subclass of LITERAL 
    :reader db-literal-class)))

(defmethod initialize-instance :after ((self db) &key (emptyp t) &allow-other-keys)
  (unless emptyp
    (warn "Schema loading not supported for ~S" self)))

(defmethod print-object ((db db) stream)
  (print-unreadable-object (db stream :type t :identity t)
    (format stream "size ~S" (length (db-triples db)))))

(defmethod db-make-literal ((db db) string &rest options)
  (declare (dynamic-extent options))
  (let ((class (db-literal-class db)))
    (if (eq class 'string)
      string
      (apply #'make-instance class :string string options))))

(defmethod db-make-triple ((db db) subject predicate object &optional source)
  #+:wilbur-triples-as-classes
  (make-instance 'triple
    :subject subject :predicate predicate :object object
    :sources (and source (list source)))
  #-:wilbur-triples-as-classes
  (%make-triple subject predicate object (and source (list source))))

(defmethod db-add-triple ((db db) (triple triple))
  (let ((sources (triple-sources triple))
	(old-triple (db-find-triple db triple)))
    (cond (old-triple
	   (let ((old-sources (triple-sources old-triple)))
	     (cond ((or (null sources)
			(subsetp sources old-sources))
		    (values old-triple nil nil))
		   (t
		    (unionf (triple-sources old-triple) sources)
		    (values old-triple nil sources)))))
	  (t
	   (push triple (db-triples db))
	   (values triple t sources)))))

(defmethod db-del-triple ((db db) (triple triple) &optional source)
  (when source
    (let ((new-sources (removef (triple-sources triple) source)))
      (when new-sources
	(return-from db-del-triple (values triple nil new-sources)))))
  (removef (db-triples db) triple)
  (values triple t nil))

(defmethod db-del-source ((db db) (source node))
  (removef (db-triples db) source :key #'triple-sources :test #'find))

(defmethod db-query-by-source ((db db) (source node))
  (remove source (db-triples db) :key #'triple-sources :test-not #'find))

(defmethod db-sources ((db db))
  ;; Bogus implementation, for small databases only, included for "completeness"
  (let ((sources nil))
    (dolist (triple (db-triples db) sources)
      (dolist (source (triple-sources triple))
	(pushnew source sources)))))

(defmethod db-query ((db db) subject predicate object)
  (flet ((matching-triple-p (triple)
	   (and (eq~ (triple-subject triple) subject)
		(eq~ (triple-predicate triple) predicate)
		(eq~ (triple-object triple) object))))
    (declare (dynamic-extent #'matching-triple-p))
    (remove-if-not #'matching-triple-p (db-triples db))))

(defmethod db-find-triple ((db db) (triple triple))
  (find triple (db-triples db) :test #'triple=))

(defmethod db-merge ((to db) (from db) &optional (source nil))
  (dolist (triple (if source
		    (db-query-by-source from source)
		    (db-triples from)))
    (db-add-triple to triple)))

(defmethod db-clear ((db db))
  (setf (db-triples db) nil))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS FAST-TEMPORARY-DB
;;;

(defclass fast-temporary-db (db)
  ())

(defmethod db-add-triple ((db fast-temporary-db) (triple triple))
  (push triple (db-triples db))
  (values triple t (triple-sources triple)))

(defmethod db-del-triple ((db fast-temporary-db) (triple triple) &optional source)
  (declare (ignore source))
  (removef (db-triples db) triple)
  (values triple t nil))


;;; --------------------------------------------------------------------------------------
;;;
;;;   ADDITIONAL DB FUNCTIONALITY
;;;

(defmethod db-reify ((triple triple) (db db)
                     &optional (statement-uri nil)
                               (source nil))
  (let ((node (node statement-uri)))
    (flet ((make-and-add-triple (p o)
	     (db-add-triple db (db-make-triple db node (node p) o source))))
      (make-and-add-triple -rdf-subject-uri-   (triple-subject triple))
      (make-and-add-triple -rdf-predicate-uri- (triple-predicate triple))
      (make-and-add-triple -rdf-object-uri-    (triple-object triple))
      (make-and-add-triple -rdf-type-uri-      (node -rdf-statement-uri-))
      node)))

(defmethod is-container-p ((db db) (node node) &optional errorp)
  ;; We may have to extend this to handle subclasses of containers
  (let ((container-types (list (node -rdf-bag-uri-)
                               (node -rdf-seq-uri-)
                               (node -rdf-alt-uri-))))
    (dolist (triple (db-query db node (node -rdf-type-uri-) nil))
      (when (find (triple-object triple) container-types)
        (return-from is-container-p t)))
    (when errorp
      (cerror "Ignore" 'container-required :thing node))))

(defmethod db-new-container-membership-property ((db db) (property node))
  nil)

(defmethod db-find-cbd ((db db) (node node))
  ;; Calculates the Concise Bounded Description as per Patrick Stickler's spec at
  ;; http://www.w3.org/Submission/2004/SUBM-CBD-20040930/
  (cbd (list node) nil nil nil db))

(defun cbd (nodes triples cbd-nodes cbd-triples db)
  (cond (nodes
	 (let ((n (first nodes)))
	   (if (member n cbd-nodes)
	     (cbd (rest nodes) triples cbd-nodes cbd-triples db)
	     (cbd (rest nodes)
		  (append triples (db-query db n nil nil))
		  (cons n cbd-nodes)
		  cbd-triples
		  db))))
	(triples
	 (let ((tr (first triples)))
	   (if (member tr cbd-triples)
	     (cbd nil (rest triples) cbd-nodes cbd-triples db)
	     (cbd (let ((s (triple-reified-p tr db))
			(o (triple-object tr)))
		    (if (and (typep o 'node)
			     (not (typep o 'literal))
			     (null (node-uri o)))
		      (cons o s)
		      s))
		  (rest triples)
		  cbd-nodes
		  (cons tr cbd-triples)
		  db))))
	(t
	 (values cbd-triples cbd-nodes))))

(defmethod db-node-local-properties ((db db) (node node))
  (remove-duplicates (mapcar #'triple-predicate (db-query db node nil nil))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   "CLASS" TRIPLE-INDEX
;;;

(defun make-triple-index (multiple-components-p)
  (make-hash-table :test (if multiple-components-p #'equal #'eq)))

(defmacro triple-index-get (index &rest components)
  (if (rest components)
    `(%triple-index-get-double ,index ,@components)
    `(%triple-index-get-single ,index ,(first components))))

(defmacro triple-index-add (triple index &rest components)
  (if (rest components)
    `(%triple-index-add-double ,triple ,index ,@components)
    `(%triple-index-add-single ,triple ,index ,(first components))))

(defmacro triple-index-rem (triple index &rest components)
  (if (rest components)
    `(%triple-index-rem-double ,triple ,index ,@components)
    `(%triple-index-rem-single ,triple ,index ,@components)))

(defun triple-index-clear (index)
  (clrhash index))

(defmacro %triple-index-get-single (index component)
  (with-temps (i)
    `(let ((,i ,index))
      (gethash ,component ,i))))

(declaim (inline %triple-index-get-single))

(defun %triple-index-add-single (triple index component)
  (push triple (gethash component index)))

(declaim (inline %triple-index-add-single))

(defun %triple-index-rem-single (triple index component)
  (removef (gethash component index) triple))

(declaim (inline %triple-index-rem-single))

(defmacro %triple-index-get-double (index c1 c2)
  (with-temps (i sub-index)
    `(let* ((,i ,index)
	    (,sub-index (gethash ,c1 ,i)))
      (when ,sub-index
	(gethash ,c2 ,sub-index)))))

(declaim (inline %triple-index-get-double))

(defun ensure-sub-index (key1 index)
  (or (gethash key1 index)
      (setf (gethash key1 index) (make-hash-table :test #'eq :size 30))))

(declaim (inline ensure-sub-index))

(defun %triple-index-add-double (triple index c1 c2)
  (push triple (gethash c2 (ensure-sub-index c1 index))))

(declaim (inline %triple-index-add-double))

(defun %triple-index-rem-double (triple index c1 c2)
  (removef (gethash c2 (ensure-sub-index c1 index)) triple))

(declaim (inline %triple-index-rem-double))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS TRIPLE-COLLECTION
;;;

(defstruct (triple-collection
	     (:constructor %make-triple-collection ()))
  (triples nil)
  (index (make-triple-index t)))

(defun make-triple-collection (&optional triples)
  (let ((collection (%make-triple-collection)))
    (dolist (triple triples)
      (triple-collection-add collection triple))
    collection))

(defun triple-collection-find (collection triple)
  (find (triple-object triple)
	(triple-index-get (triple-collection-index collection)
			  (triple-predicate triple)
			  (triple-subject triple))
	:key #'triple-object))

(defun triple-collection-add (collection triple)
  (or (triple-collection-find collection triple)
      (progn (push triple (triple-collection-triples collection))
	     (triple-index-add triple (triple-collection-index collection)
			       (triple-predicate triple)
			       (triple-subject triple))
	     triple)))

(defun triple-collection-clear (collection)
  (triple-index-clear (triple-collection-index collection))
  (setf (triple-collection-triples collection) nil))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS INDEXED-DB
;;;

(defclass indexed-db (db)
  ((index-sp
    :initform (make-triple-index t)
    :reader db-index-sp)
   (index-po
    :initform (make-triple-index t)
    :reader db-index-po)
   (index-s
    :initform (make-triple-index nil)
    :reader db-index-s)
   (index-p
    :initform (make-triple-index nil)
    :reader db-index-p)
   (index-o
    :initform (make-triple-index nil)
    :reader db-index-o)
   (by-source
    :initform (make-triple-index nil)
    :reader db-by-source))
  (:default-initargs
    :rdf-schema-pathname "wilbur:schemata;rdf-schema.rdf"))

(defmethod initialize-instance :after ((self indexed-db)
                                       &key (emptyp nil)
                                            rdf-schema-pathname
                                       &allow-other-keys)
  (unless emptyp
    (db-load self (make-file-url rdf-schema-pathname)
	     :db self :merge-results-p nil)))

(defmethod db-add-triple ((db indexed-db) (triple triple))
  (multiple-value-bind (actual-triple addedp new-sources)
                       (call-next-method)
    (cond (addedp      ; CASE 1: Triple actually added
	   (let ((s (triple-subject triple))
		 (p (triple-predicate triple))
		 (o (triple-object triple)))
	     (triple-index-add triple (db-index-sp  db) p s  )
	     (triple-index-add triple (db-index-po  db)   p o)
	     (triple-index-add triple (db-index-s   db) s    )
	     (triple-index-add triple (db-index-p   db)   p  )
	     (triple-index-add triple (db-index-o   db)     o)
	     (when new-sources
	       (dolist (source new-sources)
		 (triple-index-add triple (db-by-source db) source)))
	     (values triple t new-sources)))
	  (new-sources ; CASE 2: Only new source(s) added
	   (dolist (source new-sources)
	     (triple-index-add triple (db-by-source db) source))
	   (values actual-triple nil new-sources))
	  (t           ; CASE 3: Nothing added, source null
	   (values actual-triple nil nil)))))

;;; needs to be fixed vis-a-vis sources
(defmethod db-del-triple ((db indexed-db) (triple triple) &optional source)
  (let ((sources (triple-sources triple)))
    (multiple-value-bind (triple deletedp new-sources)
			 (call-next-method)
      (cond (deletedp
	     (let ((s (triple-subject triple))
		   (p (triple-predicate triple))
		   (o (triple-object triple)))
	       (triple-index-rem triple (db-index-sp  db) p s  )
	       (triple-index-rem triple (db-index-po  db)   p o)
	       (triple-index-rem triple (db-index-s   db) s    )
	       (triple-index-rem triple (db-index-p   db)   p  )
	       (triple-index-rem triple (db-index-o   db)     o)
	       (dolist (src sources)
		 (triple-index-rem triple (db-by-source db) src))
	       (values triple t nil)))
	    (t
	     (triple-index-rem triple (db-by-source db) source)
	     (values triple nil new-sources))))))

(defmethod db-query ((db indexed-db) subject predicate object)
  (macrolet ((filter (k tr s)
	       `(remove ,k ,tr :test-not #'eq :key ,s)))
    (cond (subject
	   (cond (predicate
		  (if object
		    (filter object
			    (triple-index-get (db-index-sp db) predicate subject)
			    #'triple-object)
		    (triple-index-get (db-index-sp db) predicate subject)))
		 (object
		  ;; "SO": should we use subject or object index?
		  (filter object
			  (triple-index-get (db-index-s db) subject)
			  #'triple-object))
		 (t
		  (triple-index-get (db-index-s db) subject))))
	  (object
	   (if predicate
	     (triple-index-get (db-index-po db) predicate object)
	     (triple-index-get (db-index-o db) object)))
	  (predicate
	   (triple-index-get (db-index-p db) predicate))
	  (t
	   (db-triples db)))))

(defmethod db-find-triple ((db indexed-db) (triple triple))
  (find (triple-object triple)
	(triple-index-get (db-index-sp db)
			  (triple-predicate triple)
			  (triple-subject triple))
	:key #'triple-object))

(defmethod db-del-source ((db indexed-db) (source node))
  (dolist (triple (db-query-by-source db source))
    (db-del-triple db triple source)))

(defmethod db-query-by-source ((db indexed-db) (source node))
  (triple-index-get (db-by-source db) source))

(defmethod db-sources ((db indexed-db))
  (let ((sources nil))
    (maphash #'(lambda (key data)
                 (declare (ignore data))
                 (push key sources))
             (db-by-source db))
    sources))

(defmethod db-clear :after ((db indexed-db))
  (triple-index-clear (db-index-sp db))
  (triple-index-clear (db-index-po db))
  (triple-index-clear (db-index-s db))
  (triple-index-clear (db-index-p db))
  (triple-index-clear (db-index-o db))
  (triple-index-clear (db-by-source db)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS LOCKED-DB-MIXIN
;;;

(defclass locked-db-mixin ()
  ((triple-lock
    :initform (make-lock)
    :reader db-triple-lock)))

(defmacro with-triple-lock (db &body body)
  `(with-lock ((db-triple-lock ,db)) ,@body))

(defmethod db-add-triple :around ((db locked-db-mixin) (triple triple))
  (with-triple-lock db
    (call-next-method)))

(defmethod db-del-triple :around ((db locked-db-mixin) (triple triple) &optional source)
  (declare (ignore source))
  (with-triple-lock db
    (call-next-method)))

(defmethod db-merge :around ((to locked-db-mixin) (from db) &optional source)
  (declare (ignore source))
  (with-triple-lock to
    (call-next-method)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   "TOP-LEVEL" DATA API
;;;

(defvar *db* nil) ; "current" database

(defun triple (subject predicate object &optional source)
  (db-make-triple *db* subject predicate object source))

(defun add-triple (triple)
  (db-add-triple *db* triple))

(defun del-triple (triple)
  (db-del-triple *db* triple nil))

(defun query (subject predicate object)
  (db-query *db* subject predicate object))

(defun reify (triple &key (statement-uri nil) (source nil))
  (db-reify triple *db* statement-uri source))

(defun local-properties (node)
  (db-node-local-properties *db* node))

(defun all-values (frame path)
  (db-get-values *db* frame path))

(defun add-value (frame path value)
  (db-add-triple *db* (db-make-triple *db* frame path value))
  value)

(defun del-value (frame path &optional value)
  (dolist (triple (db-query *db* frame path value))
    (db-del-triple *db* triple nil)))
