;;; -*- mode: lisp; package: ASDF; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  wilbur.asd
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
;;;   Version: $Id: wilbur.asd,v 1.3 2006/02/07 01:45:52 ora Exp $
;;;
;;;   Purpose: System definition(s) for Wilbur2
;;;
;;;   We no longer support either The CMU Defsystem (by Mark Kantrowitz) nor Franz,
;;;   Inc.'s defsystem (as shipped with Allegro Common Lisp). Instead, after a lot of
;;;   "soul-searching" we -- perhaps a little reluctantly -- have decided to go with
;;;   ASDF. It seems to have become the norm. For your convenience, the function
;;;   MAKE-WILBUR has been defined (internal in the CL-USER package).
;;;
;;;   Wilbur relies on the logical pathname host "wilbur". Here's a sample of how to set
;;;   up the pathname translations on a Unix-style system:
;;;
;;;     (("base;**;*.*"  "/Users/ora/Wilbur/**/*.*") ; this line is the example part
;;;      ("nox;*.*"      "wilbur:base;src;nox;*.*")
;;;      ("core;*.*"     "wilbur:base;src;core;*.*")
;;;      ("goodies;*.*"  "wilbur:base;src;goodies;*.*")
;;;      ("libs;**;*.*"  "wilbur:base;src;libs;**;*.*")
;;;      ("doc;*.*"      "wilbur:base;doc;*.*")
;;;      ("schemata;*.*" "wilbur:base;schemata;*.*"))
;;;
;;;   There's code below that attempts to define the above rules. It is not always easy
;;;   to understand how the Common Lisp logical pathname translations work. Please check
;;;   the translations (using TRANSLATE-LOGICAL-PATHNAME) before assuming that there are
;;;   bugs in Wilbur. :-)
;;;


(in-package "ASDF")


;;; --------------------------------------------------------------------------------------
;;;
;;;   COMPATIBILITY STUFF
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; OK, this is a hack, but here goes anyway...
  (when (find-package "UFFI")
    (pushnew :uffi *features*)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LOGICAL PATHNAME STUFF
;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let* ((p (probe-file *load-pathname*))
	 (d (pathname-directory p)))
    (setf (logical-pathname-translations "wilbur")
	  `(("base;**;*.*"
	     ,(merge-pathnames (make-pathname :name :wild :type :wild :version :wild
					      :directory '(:relative :wild-inferiors))
			       (make-pathname :name nil :type nil
					      :directory (subseq d 0 (1- (length d)))
					      :defaults p)))
	    ("nox;*.*"      "wilbur:base;src;nox;*.*")
	    ("core;*.*"     "wilbur:base;src;core;*.*")
	    ("goodies;*.*"  "wilbur:base;src;goodies;*.*")
	    ("libs;**;*.*"  "wilbur:base;src;libs;**;*.*")
	    ("doc;*.*"      "wilbur:base;doc;*.*")
	    ("schemata;*.*" "wilbur:base;schemata;*.*")))
    (format t "~&; \"wilbur:base;\" = ~A~%" (translate-logical-pathname "wilbur:base;"))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   ASDF SYSTEM DEFINITION FOR WILBUR2
;;;

(defsystem :wilbur
    :name "wilbur"
    :author "Ora Lassila mailto:ora.lassila@nokia.com"
    :version "2"
    :licence "NOKOS 1.0a"
    :description "WILBUR2: Nokia's Semantic Web Toolkit for CLOS"
    :depends-on (#+lispworks drakma)
    :components ((:file "packages")
		 (:file "platform" :depends-on ("packages"))
		 (:module :nox
		  :components ((:file "core-constants")
			       (:file "xml-util"
				      :depends-on ("core-constants"))
			       (:file "xml-parser"
				      :depends-on ("xml-util")))
		  :depends-on ("packages" "platform"))
		 (:module :core
		  :components ((:file "data")
			       (:file "literal"
				      :depends-on ("data"))
			       (:file "rdf-parser"
				      :depends-on ("data" "literal"))
			       (:file "http")
			       (:file "data-sources"
				      :depends-on ("data" "http"))
			       (:file "wilbur-ql"
				      :depends-on ("data" "literal"))
			       (:file "reasoner"
				      :depends-on ("wilbur-ql")))
		  :depends-on (:nox))
		 (:module :goodies
		  :components (; (:file "rdf-inspector")
			       ; (:file "ivanhoe")
			       (:file "index-and-match"))
		  :depends-on (:nox :core))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HOPEFULLY USEFUL STUFF
;;;

(defun cl-user::make-wilbur (&optional (compilep nil))
  ;; OK, notice we have a HANDLER-BIND here... I give up, I cannot figure out
  ;; how the constant definition process works on SBCL. Perhaps that's the way
  ;; ANSI CL is supposed to work, but it seems difficult to get it right. I
  ;; find it easier to just suppress the "constant-redefined-with-new-value"
  ;; errors :-)
  (handler-bind (#+:sbcl (sb-ext:defconstant-uneql #'continue))
    (asdf:operate (if compilep 'asdf:compile-op 'asdf:load-op) :wilbur)))

#+:junk ; uncomment if you want triples to be classes instead of structures
(pushnew :wilbur-triples-as-classes *features*)
