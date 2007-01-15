;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  sparql.lisp
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
;;;   Version: $Id: literal.lisp,v 1.1.1.1 2005/09/25 20:40:37 ora Exp $
;;;
;;;   Purpose: 
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defmethod db-sparql-select ((db db) variables patterns)
  (let* ((variables (mapcar #'(lambda (name)
				(make-instance 'sparql-variable :name name))
			    variables))
	 (patterns (mapcar #'(lambda (pattern)
			       (destructuring-bind (s p o) pattern
				 ...
				 (db-make-triple ...))))))
    ...))

(defclass sparql-variable ()
  ((name
    :initarg :name
    :reader sv-name)
   (constraints
    :initform nil
    :accessor sv-constraints)))

(defmethod db-triple-pattern-values ((db interned-literal-indexed-db) (pattern triple))
  (flet ((val (value)
	   (unless (symbolp value) value))
	 (var (value)
	   (when (symbolp value) value)))
    (let ((subject (triple-subject pattern))
	  (predicate (triple-predicate pattern))
	  (object (triple-object pattern)))
      (let ((triples (db-query db (val subject) (val predicate) (val object) nil))
	    (value-hash (make-hash-table :test #'eq)))
	(flet ((collect (reader)
		 (let ((values nil))
		   (clrhash value-hash)
		   (dolist (tr triples)
		     (setf (gethash (funcall reader tr) value-hash) t))
		   (maphash #'(lambda (key value)
				(declare (ignore value))
				(push key values))
			    value-hash)
		   (nreverse values))))
	  (values (cons (var subject) (collect #'triple-subject))
		  (cons (var predicate) (collect #'triple-predicate))
		  (cons (var object) (collect #'triple-object))))))))
