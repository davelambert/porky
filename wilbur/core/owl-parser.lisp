;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  owl-parser.lisp
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
;;;   Purpose: This file contains a parser for OWL, derived from the old DAML parser.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS OWL-PARSER
;;;

(defclass owl-parser (rdf-parser)
  ())

(defmethod start-element :before ((parser owl-parser)
				  (tag open-tag)
				  (mode (eql :property)))
  (when (string= (token-string tag) -owl-imports-uri-)
    (ensure-ontology-loaded parser (tag-attribute tag -rdf-resource-uri-))))

(defmethod ensure-ontology-loaded ((parser owl-parser) url)
  ;; simplistic ontology loader, can be overridden
  (cond ((db-query *db* (node url) !rdf:type !owl:Ontology nil)
	 (format t "owl:imports ~a already done~%" url))
	(t
	 (format t "Loading ~a~%" url)
	 (db-load *db* url :merge-results-p t))))

(defmethod parse-using-parsetype ((parser daml-parser) node property parsetype
                                  &optional statement-id language datatype)
  (declare (ignore statement-id language datatype))
  (if (not (string= parsetype "collection"))
    (call-next-method)
    (let ((list-node (ensure-node parser nil t)))
      (add-as-triple parser node property list-node)
      (add-state parser :daml-collection list-node))))

(defmethod start-element ((parser daml-parser)
			  (tag open-tag)
			  (mode (eql :daml-collection)))
  (start-element parser tag :description))

(defmethod attach-to-parent ((parser daml-parser)
                             (parent node)
                             (child node)
                             &optional statement-id)
  (declare (ignore statement-id))
  (let ((state (first (parser-states parser))))
    (if (not (eq (state-mode state) :daml-collection))
      (call-next-method)
      (let ((parent (state-node state))
            (node (ensure-node parser nil t)))
        (add-as-triple parser parent -rdf-type-uri-
                       (ensure-node parser -daml-list-uri- t))
        (add-as-triple parser parent -daml-first-uri- child)
        (add-as-triple parser parent -daml-rest-uri- node)
        (setf (state-node state) node)))))

(defmethod end-element :before ((parser daml-parser)
				(tag open-tag)
				(mode (eql :daml-collection)))
  (let* ((node (state-node (first (parser-states parser))))
         (db (parser-db parser))
         (triple (db-del-triple db (first (db-query db nil nil node nil)))))
    (add-as-triple parser
                   (triple-subject triple)
                   (triple-predicate triple)
                   (ensure-node parser -daml-nil-uri- t))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CONSTRUCTORS FOR OWL COLLECTIONS
;;;

(defun owl-list (&rest items)
  (declare (dynamic-extent items))
  (if items
    (owl-cons (first items) (apply #'owl-list (rest items)))
    !owl:nil))

(defun owl-cons (first rest &optional uri)
  (let ((pair (node uri)))
    (add-triple (triple pair !rdf:type (node -owl-list-uri-)))
    (add-triple (triple pair (node -owl-first-uri-) first))
    (add-triple (triple pair (node -owl-rest-uri-) rest))
    pair))
