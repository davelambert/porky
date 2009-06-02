;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  processes.lisp
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
;;;   Copyright (c) 2001-2004 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;
;;;   Purpose: 
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE MANIPULATIONS
;;;

#-:mcl
(eval-when (:compile-toplevel :load-toplevel)
  (import #+:excl '(mp:process-kill
		    mp:process-wait
		    mp:process-run-function)
	  #+:sbcl '(sb-ext:process-wait)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LOCKS
;;;

(defmacro with-lock ((lock &rest args) &body body)
  #+:mcl  `(with-lock-grabbed (,lock ,@args) ,@body)
  #+:excl `(mp:with-process-lock (,lock ,@args) ,@body)
  #+:sbcl `(sb-thread:with-mutex (,lock ,@args) ,@body)
  #-(or :mcl :excl :sbcl)
  (declare (ignore lock args body))
  #-(or :mcl :excl :sbcl)
  (error "No locking defined (WITH-LOCK) in this implementation"))

#-:mcl
(defun make-lock ()
  #+:excl (mp:make-process-lock)
  #+:sbcl (sb-thread:make-mutex)
  #-(or :excl :sbcl)
  (error "No locking implemented"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   SEMAPHORES
;;;

#+:junk
(defclass semaphore ()
  ;; Sorry, brain-dead design, but allows two processes to synchronize.
  ;; We like the OpenMCL threads, etc.
  ((lock
    :initform (make-lock)
    :reader semaphore-lock)
   (value
    :initform nil
    :accessor semaphore-value)))

#-:openmcl
(defun make-semaphore ()
  nil)

#-:openmcl
(defun signal-semaphore (dummy)
  dummy)

#-:openmcl
(defun wait-on-semaphore (dummy)
  dummy)

#+:junk
(defmethod wait-on-semaphore ((semaphore semaphore))
  (process-wait "semaphore wait"
		#'(lambda (s)
		    (with-lock ((semaphore-lock s))
		      (let ((value (semaphore-value s)))
			(when value
			  (setf (semaphore-value s) nil)
			  t))))
		semaphore))


;;; --------------------------------------------------------------------------------------
;;;
;;;   EXTERNAL PROCESSES
;;;

(defun simple-external-process (command &rest args)
  #+:openmcl
  (external-process-output-stream (run-program command (remove nil args)
					       :output :stream :wait nil))
  #+:excl
  (run-shell-command (format nil "~A~{~@[ ~A~]~}" command args)
		     :output :stream :wait nil)
  #-(or :openmcl :excl)
  (error "Cannot execute \"~A~{~@[ ~A~]~}\". External processes not implemented"
	 command args))
