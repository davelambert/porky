;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  http.lisp
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
;;;
;;;   Purpose: ...
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defmacro define-ws-interface (name (&rest args) &body body)
  (let ((generic-args (mapcar #'(lambda (arg)
				  (if (consp arg)
				    (car arg)
				    arg))
			      args)))
    `(progn
      (defgeneric ,name ,generic-args
	(:generic-function-class serv:xr-generic-function))
      (defmethod ,name ,args
	,@body))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defclass wilbur-service (serv:xr-servlet)
  ((db
    :initarg :db
    :initform nil
    :reader ws-db)))

(defmethod ws-db :around ((ws wilbur-service))
  (or (call-next-method) *db*))

(defmethod ws-result ((ws wilbur-service) (node node))
  (let ((uri (node-uri node)))
    (if uri
      (serv:xr-struct :type "node" :uri uri)
      (error "Blank nodes not supported: ~S" node))))

(defmethod ws-result ((ws wilbur-service) (literal literal))
  (let ((datatype (literal-datatype literal)))
    (serv:xr-struct :type "literal"
		    :string (literal-string literal)
		    :datatype (and datatype (node-uri datatype))
		    :language (literal-language literal))))

(define-ws-interface ws.db-load ((ws wilbur-service) (url string))
  (multiple-value-bind (temporary-db source-node errors)
		       (db-load (ws-db ws) url
				:error-handling :collect :merge-results-p t)
    (serv:xr-struct :source (node-uri source-node)
		    :errors errors
		    :count (length (db-triples temporary-db)))))

(define-ws-interface ws.db-get-values ((ws wilbur-service) (frame string) (path string))
  (mapcar #'(lambda (node)
	      (ws-result ws node))
	  (db-get-values (ws-db ws)
			 (node frame)
			 (read-query-from-string path))))
