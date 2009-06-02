;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  reasoner.lisp
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
;;;   Purpose: This file implements an RDFS reasoner, based on this paper:
;;;
;;;      Ora Lassila: "Taking the RDF Model Theory Out for a Spin", in: Ian Horrocks &
;;;        James Hendler (eds.): "The Semantic Web - ISWC 2002", Lecture Notes in Computer
;;;        Science 2342, pp.307-317, Springer Verlag, 2002
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   MIXIN CLASS DEDUCTIVE-CLOSURE-DB-MIXIN
;;;

(defclass deductive-closure-db-mixin ()
  ((closurep
    :initarg :closurep
    :initform t
    :accessor db-closure-p)
   (use-rule-engine-p
    :initarg :use-rule-engine-p
    :initform t
    :reader db-use-rule-engine-p)
   (sameas-clusters
    :initform (make-hash-table :test #'eq)
    :reader db-sameas-clusters)
   (ifp-additions
    :initform (make-triple-collection)
    :reader db-ifp-additions))
  (:default-initargs
    :rdf-schema-pathname "wilbur:schemata;true-rdf-schema.rdf"))

(defmacro without-closure ((db) &body body)
  (let ((d (gentemp))
	(closurep (gentemp)))
    `(let* ((,d ,db)
	    (,closurep nil))
      (unwind-protect (progn
			(shiftf ,closurep (db-closure-p ,d) nil)
			,@body)
	(setf (db-closure-p ,d) ,closurep)))))

(defvar *subprop-query* nil)
(defvar *subprops-of-subprop* (list !rdfs:subPropertyOf))

(declaim (special *rewritten-paths*)) ; forward ref.

(defmethod db-add-triple ((db deductive-closure-db-mixin) (triple triple))
  (multiple-value-bind (triple addedp new-sources)
                       (call-next-method)
    (when addedp
      (let ((p (triple-predicate triple))
            (o (triple-object triple)))
        (db-add-triple db (db-make-triple db p !rdf:type !rdf:Property))
	(cond ((eq p !rdf:type)
	       (db-add-triple db (db-make-triple db o !rdf:type !rdfs:Class)))
	      ((eq p !owl:sameAs)
	       (db-update-sameas-clusters db (triple-subject triple)))
	      ((member p *subprops-of-subprop*)
	       (db-clear-reasoner-cache db)
	       (when (member o *subprops-of-subprop*)
		 (setf *subprops-of-subprop*
		         (db-get-values db !rdfs:subPropertyOf (subprop-query))
		       *subprop-query* nil)))
	      ((and (typep o 'literal)
		    (literal-datatype o))
	       (db-add-triple db (db-make-triple db o !rdf:type !rdfs:XMLLiteral))))))
    (values triple addedp new-sources)))

(defmethod db-del-triple ((db deductive-closure-db-mixin) (triple triple)
			  &optional source)
  (declare (ignore source))
  (multiple-value-bind (triple deletedp new-sources)
		       (call-next-method)
    (when (and deletedp (eq (triple-predicate triple) !owl:sameAs))
      (db-update-sameas-clusters db (triple-subject triple))
      (db-update-sameas-clusters db (triple-object triple)))
    (values triple deletedp new-sources)))

(defmethod db-merge :after ((to deductive-closure-db-mixin) (from db) &optional source)
  (declare (ignore source))
  (db-clear-reasoner-cache to))

(defmethod db-new-container-membership-property ((db deductive-closure-db-mixin)
						 (property node))
  (flet ((tr (s p o)
	   (db-add-triple db (db-make-triple db s p o))))
    (tr property !rdf:type !rdfs:ContainerMembershipProperty)
    (tr property !rdfs:subPropertyOf !rdfs:member)))

(defmethod db-get-values :around ((db deductive-closure-db-mixin) (frame node) path)
  ;; We cannot check the status of reasoning, since WilburQL queries are executed without
  ;; reasoning, yet owl:sameAs support has to work... this could be a problem, but for now
  ;; we just ignore the whole matter.
  (let ((other-frames (gethash frame (db-sameas-clusters db))))
    (cond ((eq path !owl:sameAs)
	   (if (db-closure-p *db*)
	     other-frames
	     (call-next-method)))
	  ((rest other-frames)
	   (reduce #'union (mapcar #'(lambda (f)
				       (call-next-method db f path))
				   other-frames)))
	  (t
	   (call-next-method)))))

(defmethod db-get-values ((db deductive-closure-db-mixin) (frame node) path)
  (let ((path (rewrite-path path db)))
    (without-closure (db)
      (call-next-method db frame path))))

(defmethod db-get-values ((db deductive-closure-db-mixin)
			  (frame (eql !rdfs:Resource))
			  (path inverse-slot))
  (let ((link (inverse-slot-node path)))
    (if (or (eq link !rdf:type) (eq link !rdfs:subClassOf))
      (cons :all (call-next-method))
      (call-next-method))))

(defmethod frames-related-p ((source node) path (sink node)
                             (db deductive-closure-db-mixin)
                             action)
  (let ((path (rewrite-path path db)))
    (without-closure (db)
      (call-next-method source path sink db action))))

(defmethod db-update-sameas-clusters ((db deductive-closure-db-mixin)
				      (node node))
  (let ((cluster (without-closure (db)
		   (db-get-values db node '(:rep* (:or !owl:sameAs (:inv !owl:sameAs))))))
	(clusters (db-sameas-clusters db)))
    (if (rest cluster)
      (dolist (i cluster)
	(setf (gethash i clusters) cluster))
      (remhash node clusters))))

(defun show-sameas-clusters (db)
  (maphash #'(lambda (key value)
	       (format t "~&~S: ~S" key value))
	   (db-sameas-clusters db)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   PATH REWRITING
;;;

(defvar *rewritten-paths* (make-hash-table :test #'equal))
(defvar *known-rewrite-rules* (make-hash-table :test #'eq))

(defun rewrite-path (path db)
  (if (or (typep path 'path)
	  (not (db-closure-p db)))
    path
    (or (gethash path *rewritten-paths*)
	(let ((p (rewrite-path-for-subproperties
		  (rewrite-path-for-types (if (db-use-rule-engine-p db)
					    (rewrite-path-using-rule-engine path db)
					    path)
					  db)
		  db)))
	  (setf (gethash path *rewritten-paths*) (make-instance 'path :expression p))))))

(defmacro define-rewrite-function (name (expr db) &body body)
  (let ((op (gentemp))
	(var (gentemp)))
    `(defun ,name (,expr ,db)
       (typecase ,expr
	 (node ,@body)
	 (cons (let ((,op (first ,expr)))
		 (if (eq ,op :value)
		   ,expr
		   (cons ,op
			 (mapcar #'(lambda (,var)
				     (,name ,var ,db))
				 (rest ,expr))))))
	 (t ,expr)))))

(define-rewrite-function rewrite-path-for-types (path db)
  (case path
    (!rdf:type
     '(:or (:seq !rdf:type (:rep* !rdfs:subClassOf))
           (:seq :predicate-of-object !rdfs:range (:rep* !rdfs:subClassOf))
           (:seq :predicate-of-subject !rdfs:domain (:rep* !rdfs:subClassOf))
           (:value !rdfs:Resource)))
    (!rdfs:subClassOf
     ;; '(:or (:rep* !rdfs:subClassOf) (:value !rdfs:Resource))
     '(:or (:seq+ (:rep+ !rdfs:subClassOf) (:value !rdfs:Resource)) :self))
    (t
     path)))

(define-rewrite-function rewrite-path-for-subproperties (path db)
  (let ((props (db-get-values db path (subprop-query))))
    (if (rest props)
      `(:or ,@props)
      (first props))))

(defvar *rule-rewrite-change-p*)

(define-rewrite-function rewrite-path-using-rules (path db)
  (let ((new-path (gethash path *known-rewrite-rules*)))
    (cond (new-path
	   (setf *rule-rewrite-change-p* t)
	   new-path)
	  (t
	   path))))

(defun rewrite-path-using-rule-engine (path db)
  (loop (let ((*rule-rewrite-change-p* nil))
	  (setf path (rewrite-path-using-rules path db))
	  (unless *rule-rewrite-change-p*
	    (return-from rewrite-path-using-rule-engine path)))))

(defun subprop-query ()
  (or *subprop-query*
      (progn
	(clrhash *rewritten-paths*)
	(setf *subprop-query*
	      (make-instance 'path
		:expression `(:rep* (:inv (:or ,@*subprops-of-subprop*))))))))

(defun db-collect-rewrite-rules (db)
  (clrhash *known-rewrite-rules*)
  (dolist (rule (db-get-values db !wilbur:Rule '(:inv !rdf:type)))
    (setf (gethash rule *known-rewrite-rules*)
	  (cond ((db-node-type-p db rule !wilbur:RewriteRule)
		 (read-from-string
		  (literal-string (first (db-get-values db rule !wilbur:path)))))
		((db-node-type-p db rule !wilbur:AccessDaemon)
		 (make-access-daemon rule))))))

(defun db-clear-reasoner-cache (db)
  ;;(when (typep db 'pre-rewrite-cached-access-mixin)
  ;;  (db-clear-cache db))
  (clrhash *path-fsas*)
  (clrhash *rewritten-paths*)
  (setf *subprop-query* nil)
  (db-update-ifps db))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CONVENIENCE CLASS EDB
;;;

(defclass edb (deductive-closure-db-mixin interned-literal-indexed-db)
  ())


;;; --------------------------------------------------------------------------------------
;;;
;;;   TYPE HIERARCHY ANALYSIS
;;;

(defmethod db-node-type-p ((db deductive-closure-db-mixin) (node node) (type node))
  (frames-related-p node !rdf:type type *db* nil))

(defmethod db-node-subtype-p ((db deductive-closure-db-mixin)
			      (type node) (supertype node))
  (frames-related-p type !rdfs:subClassOf supertype db nil))

(defmethod db-node-types ((db db) (node node))
  (sort-types (db-get-values db node !rdf:type) db))

(defun sort-types (types db &key (key #'identity))
  (sort types
	#'(lambda (c1 c2)
	    (db-node-subtype-p db c1 c2))
	:key key))

(defmethod db-node-types-expanded ((db db) (node node))
  (labels ((partition (type other-types)
	     (let* ((supers (sort-types (db-get-values db type '(:rep* !rdfs:subClassOf))
					db))
		    (others (remove-if #'(lambda (type)
					   (member type supers))
				       other-types)))
	       (if others
		 (cons supers (partition (first others) (rest others)))
		 (list supers)))))
    (let ((types (db-node-types db node)))
      (when types
	(partition (first types) (rest types))))))

(defmethod db-node-types-expanded ((db deductive-closure-db-mixin) (node node))
  (labels ((partition (type other-types)
	     (let* ((supers (sort-types (db-get-values db type !rdfs:subClassOf) db))
		    (others (remove-if #'(lambda (type)
					   (member type supers))
				       other-types)))
	       (if others
		 (cons supers (partition (first others) (rest others)))
		 (list supers)))))
    (let ((types (db-node-types db node)))
      (when types
	(partition (first types) (rest types))))))

(defmethod db-node-properties ((db deductive-closure-db-mixin) (node node))
  (let ((triples (make-triple-collection (db-query db node nil nil)))
	(sameas (gethash node (db-sameas-clusters db))))
    (flet ((add-queried-triples (prop)
	     (dolist (value (db-get-values db node prop))
	       (triple-collection-add triples (db-make-triple db node prop value)))))
      (add-queried-triples !rdf:type)
      (add-queried-triples !rdfs:subClassOf)
      (add-queried-triples !rdfs:subPropertyOf)
      (dolist (n sameas)
	(unless (eq n node)
	  (triple-collection-add triples (db-make-triple db node !owl:sameAs n))))
      (triple-collection-triples triples))))

(defmethod db-node-properties-partitioned ((db deductive-closure-db-mixin) (node node))
  (let ((types (db-node-types-expanded db node))
	(triples (make-triple-collection))
	(sameas (pushnew node (gethash node (db-sameas-clusters *db*)))))
    (labels ((add-queried-properties (n property exclude)
	       (dolist (value (db-get-values db n property))
		 (unless (find value exclude)
		   (triple-collection-add triples (triple node property value)))))
	     (collect-properties (n)
	       (dolist (triple (db-query db n nil nil))
		 (unless (eq (triple-predicate triple) !owl:sameAs)
		   (triple-collection-add triples triple)))
	       ;; The following are the 3 properties that have a semantic theory in RDF(S)
	       (add-queried-properties n !rdf:type '(!rdfs:Resource))
	       (add-queried-properties n !rdfs:subClassOf sameas)
	       (add-queried-properties n !rdfs:subPropertyOf sameas)
	       #+:junk
	       (dolist (s sameas)
		 (unless (eq n s)
		   (triple-collection-add triples (db-make-triple db n !owl:sameAs s)))))
	     (triple~ (a b)
	       (and (eq (triple-predicate a) (triple-predicate b))
		    (eq (triple-object a) (triple-object b)))))
      (dolist (n sameas)
	(collect-properties n))
      (let ((properties
	     (remove-duplicates (triple-collection-triples triples) :test #'triple~)))
	(flet ((construct-property-sets (some-types)
		 (let ((props nil)
		       (type-props nil)
		       (used-types nil))
		   (dolist (property properties)
		     (let ((p (triple-predicate property))
			   (o (triple-object property)))
		       (cond ((and (eq p !rdf:type)
				   (find o some-types))
			      (push property type-props)
			      (removef some-types o)
			      (push o used-types)
			      (removef properties property))
			     ((and (not (eq (first (db-get-values db p !rdfs:domain))
					    !rdfs:Resource))
				   (some #'(lambda (type)
					     (unless (eq type !rdfs:Resource)
					       (frames-related-p p !rdfs:domain type
								 db nil)))
					 (append used-types some-types)))
			      (push property props)
			      (removef properties property)))))
		   (append (sort-types type-props db :key #'triple-object) props))))
	  (append (mapcar #'construct-property-sets types)
		  (list (cons (db-make-triple db node !rdf:type !rdfs:Resource)
			      properties))))))))

(defmethod db-node-properties :around ((db deductive-closure-db-mixin) (node node))
  (let ((nodes (gethash node (db-sameas-clusters db))))
    (if nodes
      (reduce #'(lambda (x y)
		  (union x y :test #'triple=))
	      (mapcar #'(lambda (n)
			  (call-next-method db n))
		      nodes))
      (call-next-method))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   INVERSE FUNCTIONAL PROPERTIES
;;;
;;;   This is a hack, and will go away at some point when we figure out a better way to
;;;   do inverse functional properties. In the meantime, I am sorry.
;;;

(defmethod db-update-single-ifp ((db deductive-closure-db-mixin) (ifp node)
				 &optional (obj-subj-map (make-hash-table :test #'eq)))
  (let ((additions (db-ifp-additions db)))
    (dolist (triple (db-query db nil ifp nil))
      (push (triple-subject triple) (gethash (triple-object triple) obj-subj-map)))
    (maphash #'(lambda (value nodes)
		 (declare (ignore value))
		 (when (rest nodes)
		   (let ((root (first nodes)))
		     (dolist (node (rest nodes))
		       (let ((triple (db-make-triple db root !owl:sameAs node)))
			 (db-add-triple db triple)
			 (triple-collection-add additions triple))))))
	     obj-subj-map)))

(defmethod db-update-ifps ((db deductive-closure-db-mixin))
  (dolist (triple (triple-collection-triples (db-ifp-additions db)))
    (let ((triple (db-find-triple db triple)))
      (unless (triple-sources triple)
	(db-del-triple db triple))))
  (triple-collection-clear (db-ifp-additions db))
  (let ((obj-subj-map (make-hash-table :test #'eq)))
    (dolist (ifp (db-get-values db !owl:InverseFunctionalProperty '(:inv !rdf:type)))
      (clrhash obj-subj-map)
      (db-update-single-ifp db ifp obj-subj-map))))
