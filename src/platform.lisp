;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  platform.lisp
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
;;;   Version: $Id: platform.lisp,v 1.1 2006/02/07 01:45:48 ora Exp $
;;;
;;;   Purpose: This file contains various platform-dependent functions and macros.
;;;   Currently, we support MCL, OpenMCL, Allegro and SBCL. There is no reason why Wilbur
;;;   wouldn't run on other Common Lisps too, but some of these functions will have to be
;;;   ported separately.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   FEATURES, PACKAGES, ETC.
;;;

#+(and :mcl (not :openmcl))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :realmcl *features*)
  (require :opentransport))

#+:excl
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; adding this feature suppresses other HTTP client implementations
  (pushnew :http-using-aserve *features*)
  ;; other implementations may have other means of installing Portable AServe
  (require :aserve))

#+:excl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(mp:process-kill mp:process-wait mp:process-run-function)))

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sb-ext:process-wait)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :wilbur *features*)
  (pushnew :wilbur2 *features*))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LOCKS
;;;

#+:mcl
(defmacro with-lock ((lock &rest args) &body body)
  `(with-lock-grabbed (,lock ,@args) ,@body))

#+:excl
(defmacro with-lock ((lock &rest args) &body body)
  `(mp:with-process-lock (,lock ,@args) ,@body))

#+:sbcl
(defmacro with-lock ((lock &rest args) &body body)
  `(sb-thread:with-mutex (,lock ,@args) ,@body))

#-(or :mcl :excl :sbcl)
(defmacro with-lock ((lock &rest args) &body body)
  (declare (ignore lock args body))
  (error "No locking defined (WITH-LOCK) in this implementation"))

#+:excl
(defun make-lock ()
  (mp:make-process-lock))

#+:sbcl
(defun make-lock ()
  (sb-thread:make-mutex))

#-(or :excl :sbcl :mcl) ; MCL already has MAKE-LOCK
(defun make-lock ()
  (error "No locking implemented"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   EXTERNAL PROCESSES, ETC.
;;;

#+:openmcl
(defun simple-external-process (command &rest args)
  (external-process-output-stream
   (run-program command (remove nil args) :output :stream :wait nil)))

#+:excl
(defun simple-external-process (command &rest args)
  (run-shell-command (format nil "~A~{~@[ ~A~]~}" command args)
		     :output :stream :wait nil))

#-(or :openmcl :excl) ; we still need to implement this for SBCL
(defun simple-external-process (command &rest args)
  (error "Cannot execute \"~A~{~@[ ~A~]~}\". External processes not implemented"
	 command args))

#+:openmcl
(defun get-env (key)
  (ccl::getenv key))

#+:excl
(defun get-env (key)
  (sys:getenv key))

#+:sbcl
(defun get-env (key)
  (sb-ext:posix-getenv (string key)))

#-(or :openmcl :excl :sbcl)
(defun get-env (key)
  (error "Cannot get the environment variable ~S" key))

;; This does not really belong here, but there was some weird behavior on Allegro when
;; this macro still resided in xml-util.lisp
(defmacro with-temps ((&rest variables) &body body)
  `(let (,@(mapcar #'(lambda (variable)
		       `(,variable (gentemp)))
		   variables))
    ,@body))
