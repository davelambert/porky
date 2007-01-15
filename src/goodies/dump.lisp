;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  
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
;;;   Version: $Id: wilbur2-file-header.lisp,v 1.1 2004/08/10 16:24:46 ora Exp $
;;;
;;;   Purpose: 
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defclass dump-format ()
  ())

(defmethod dump-output-db ((dump dump-format) (db db) stream &optional source)
  (dump-output-triples dump
		       (if source
			 (db-query-by-source db source)
			 (db-query db nil nil nil))
		       stream
		       source))

(defmethod dump-output-triples ((dump dump-format) triples stream &optional source)
  (dolist (triple triples)
    (dump-output-triple dump triple stream source)))

(defmethod dump-output-triple ((dump dump-format) triple stream &optional source)
  (print (list (triple-source triple)
	       (triple-predicate triple)
	       (triple-object triple)
	       source)
	 stream))

(defmethod dump-input-triple ((dump dump-format) (db db) stream)
  (let ((input (read stream nil nil)))
    (when input
      (destructuring-bind (subject predicate object source) input
	(db-add-triple db (db-make-triple db subject predicate object source))))))

(defmethod dump-input-triples ((dump-format) (db db) stream)
  (loop (let ((triple (dump-input-triple dump db stream)))
	  (unless triple
	    (return-from dump-input-triples)))))
