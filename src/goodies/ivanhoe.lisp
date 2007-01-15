;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  ivanhoe.lisp
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
;;;   Version: $Id: ivanhoe.lisp,v 1.1.1.1 2005/09/25 20:40:37 ora Exp $
;;;
;;;   Purpose: The old "db-hiding" frame API ("Ivanhoe") is grandfathered here.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   OLD "TOP-LEVEL" API
;;;

(defvar *db* nil) ; "current" database

(defun triple (subject predicate object &optional source)
  (db-make-triple *db* subject predicate object source))

(defun add-triple (triple)
  (db-add-triple *db* triple))

(defun del-triple (triple)
  (db-del-triple *db* triple))

(defun query (subject predicate object)
  (db-query *db* subject predicate object))

(defun reify (triple &key (statement-uri nil) (source nil))
  (db-reify triple *db* statement-uri source))


;;; --------------------------------------------------------------------------------------
;;;
;;;   FRAME SYSTEM API IMPLEMENTATION
;;;

(defun frame (uri &rest slot/value-pairs)
  (let ((frame (node uri)))
    (dolist (slot/value-pair slot/value-pairs)
      (destructuring-bind (slot . value) slot/value-pair
        (add-value frame slot value)))
    frame))

(defun own-slots (frame)
  (remove-duplicates (mapcar #'triple-predicate (db-query *db* frame nil nil))))

(defun value (frame &rest paths)
  (declare (dynamic-extent paths))
  (flet ((v (path) (get-value frame path *db*)))
    (declare (dynamic-extent #'v))
  (apply #'values (mapcar #'v paths))))

(defun all-values (frame &rest paths)
  (declare (dynamic-extent paths))
  (flet ((av (path) (get-all-values frame path *db*)))
    (declare (dynamic-extent #'av))
    (apply #'values (mapcar #'av paths))))

(defun add-value (frame path value)
  (db-add-triple *db* (db-make-triple *db* frame path value))
  value)

(defun del-value (frame path &optional value)
  (dolist (triple (db-query *db* frame path value))
    (db-del-triple *db* triple)))

(defun relatedp (source path sink &optional action)
  (frames-related-p source path sink *db* action))

(defun load-db (source &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load *db* source options))
