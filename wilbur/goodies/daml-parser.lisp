;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  daml.lisp
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
;;;   Purpose: This file contains an implementation of a DAML+OIL parser.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   DAML CONSTANTS
;;;

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant -daml+oil-uri-   "http://www.daml.org/2000/12/daml+oil#"))

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant -daml-list-uri-  #.(concatenate 'string -daml+oil-uri- "List"))
  (defconstant -daml-first-uri- #.(concatenate 'string -daml+oil-uri- "first"))
  (defconstant -daml-rest-uri-  #.(concatenate 'string -daml+oil-uri- "rest"))
  (defconstant -daml-nil-uri-   #.(concatenate 'string -daml+oil-uri- "nil")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS DAML-PARSER
;;;

(defclass daml-parser (rdf-parser)
  ())

(defmethod parse-using-parsetype ((parser daml-parser) node property parsetype
                                  &optional statement-id language datatype)
  (declare (ignore statement-id language datatype))
  (if (not (string= parsetype "daml:collection"))
    (call-next-method)
    (let ((list-node (ensure-node parser nil t)))
      (add-as-triple parser node property list-node)
      (add-state parser :daml-collection list-node))))

(defmethod nox:start-element ((parser daml-parser)
                              (tag nox:open-tag)
                              (mode (eql :daml-collection)))
  (nox:start-element parser tag :description))

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

(defmethod nox:end-element :before ((parser daml-parser)
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
;;;   CONSTRUCTORS FOR DAML COLLECTIONS
;;;

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
