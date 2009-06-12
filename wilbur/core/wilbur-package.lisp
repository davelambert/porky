;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  wilbur-package.lisp
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
;;;   Version: $Id: wilbur-package.lisp,v 1.2 2006/02/04 21:18:59 ora Exp $
;;;
;;;   Purpose: Definition of the package "WILBUR".
;;;


(in-package "CL-USER")


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE WILBUR
;;;

(defpackage "WILBUR"
  (:nicknames "NOKIA-RDF-CL"
	      "WILBUR-RDF"
	      "W")
  (:use "COMMON-LISP"
	#+:mcl "CCL"
	#+:excl "EXCL"
	#+:excl "SOCKET"
	#+:excl "MOP"
	#+:sbcl "SB-SYS")
  (:import-from "NOKIA-XML-CL"
		"-RDF-URI-"             ; from rdf-constants.lisp
		"-RDFS-URI-"
		"-XSD-URI-"
		"RDF-URI"
		"RDFS-URI"
		"-RDF-ATTRS-"
		"-RDF-ATTR-MAP-"
		"-RDF-ID-URI-"
		"-RDF-RESOURCE-URI-"
		"-RDF-ABOUT-URI-"
		"-RDF-ABOUTEACH-URI-"
		"-RDF-ABOUTEACHPREFIX-URI-"
		"-RDF-BAGID-URI-"
		"-RDF-PARSETYPE-URI-"
		"-RDF-DATATYPE-URI-"
		"-RDF-NODEID-URI-"
		"-XML-LANG-ATTR-"
		"-RDF-DESCRIPTION-URI-"
		"-RDF-TYPE-URI-"
		"-RDF-RDF-URI-"
		"-RDF-LI-URI-"
		"-RDF-STATEMENT-URI-"
		"-RDF-SUBJECT-URI-"
		"-RDF-PREDICATE-URI-"
		"-RDF-OBJECT-URI-"
		"-RDF-BAG-URI-"
		"-RDF-SEQ-URI-"
		"-RDF-ALT-URI-"
		"-RDF-FIRST-URI-"
		"-RDF-REST-URI-"
		"-RDF-NIL-URI-"
		"-RDFS-RESOURCE-URI-"
		"-RDFS-CLASS-URI-"
		"-RDFS-SUBCLASSOF-URI-"
		"-RDFS-SUBPROPERTYOF-URI-"
		"-RDFS-SEEALSO-URI-"
		"-RDFS-ISDEFINEDBY-URI-"
		"-RDFS-CONSTRAINTRESOURCE-URI-"
		"-RDFS-CONSTRAINTPROPERTY-URI-"
		"-RDFS-RANGE-URI-"
		"-RDFS-DOMAIN-URI-"
		"-RDFS-COMMENT-URI-"
		"-RDFS-LABEL-URI-"
		"-RDFS-LITERAL-URI-"
		"-RDFS-CONTAINER-URI-")
  (:export "-RDF-URI-"                  ; from rdf-constants.lisp
	   "-RDFS-URI-"
	   "RDF-URI"
	   "RDFS-URI"
	   "XSD-URI"
	   "OWL-URI"
	   "-RDF-ATTRS-"
	   "-RDF-ATTR-MAP-"
	   "-RDF-ID-URI-"
	   "-RDF-RESOURCE-URI-"
	   "-RDF-ABOUT-URI-"
	   "-RDF-ABOUTEACH-URI-"
	   "-RDF-ABOUTEACHPREFIX-URI-"
	   "-RDF-BAGID-URI-"
	   "-RDF-PARSETYPE-URI-"
	   "-RDF-DATATYPE-URI-"
	   "-RDF-NODEID-URI-"
	   "-XML-LANG-ATTR-"
	   "-RDF-DESCRIPTION-URI-"
	   "-RDF-TYPE-URI-"
	   "-RDF-RDF-URI-"
	   "-RDF-LI-URI-"
	   "-RDF-STATEMENT-URI-"
	   "-RDF-SUBJECT-URI-"
	   "-RDF-PREDICATE-URI-"
	   "-RDF-OBJECT-URI-"
	   "-RDF-BAG-URI-"
	   "-RDF-SEQ-URI-"
	   "-RDF-ALT-URI-"
	   "-RDFS-RESOURCE-URI-"
	   "-RDFS-CLASS-URI-"
	   "-RDFS-SUBCLASSOF-URI-"
	   "-RDFS-SUBPROPERTYOF-URI-"
	   "-RDFS-SEEALSO-URI-"
	   "-RDFS-ISDEFINEDBY-URI-"
	   "-RDFS-CONSTRAINTRESOURCE-URI-"
	   "-RDFS-CONSTRAINTPROPERTY-URI-"
	   "-RDFS-RANGE-URI-"
	   "-RDFS-DOMAIN-URI-"
	   "-RDFS-COMMENT-URI-"
	   "-RDFS-LABEL-URI-"
	   "-RDFS-LITERAL-URI-"
	   "-RDFS-CONTAINER-URI-"
	   "-OWL-LIST-URI-"
	   "-OWL-FIRST-URI-"
	   "-OWL-REST-URI-"
	   "-OWL-NIL-URI-"
	   "-OWL-IMPORTS-URI-"
	   "NODE"                       ; from rdf-data.lisp
	   "TRIPLE"
	   "ADD-TRIPLE"
	   "DEL-TRIPLE"
	   "QUERY"
	   "REIFY"
	   "ADD-NAMESPACE"
	   "DEL-NAMESPACE"
	   "NAMESPACES"
	   "RDF-ERROR"
	   "*DB*"
	   "FEATURE-NOT-SUPPORTED"      ; this is not the one from NOX!
	   "ABOUT-AND-ID-BOTH-PRESENT"
	   "ABOUT-AND-NODEID-BOTH-PRESENT"
	   "UNKNOWN-PARSETYPE"
	   "ILLEGAL-CHARACTER-CONTENT"
	   "CONTAINER-REQUIRED"
	   "OUT-OF-SEQUENCE-INDEX"
	   "DUPLICATE-NAMESPACE-PREFIX"
	   "NODE-URI"
	   "NODE-NAME-RESOLVED-P"
	   "INDEX-URI"
	   "DICTIONARY"
	   "DICTIONARY-NODES"
	   "DICTIONARY-NAMESPACES"
	   "DICTIONARY-UNRESOLVED-NODES"
	   "DICTIONARY-NODE-CLASS"
	   "DICTIONARY-ADD-NAMESPACE"
	   "DICTIONARY-REMOVE-NAMESPACE"
	   "DICTIONARY-RENAME-NAMESPACE"
	   "FIND-NODE"
	   "FIND-SHORT-NAME"
	   "FIND-LONG-NAME"
	   "DICTIONARY-APROPOS-LIST"
	   "*NODES*"
	   "TRIPLE-SUBJECT"
	   "TRIPLE-PREDICATE"
	   "TRIPLE-OBJECT"
	   "TRIPLE-SOURCE"
	   "DB"
	   "DB-TRIPLES"
	   "*DB*"
	   "DB-ADD-TRIPLE"
	   "DB-DEL-TRIPLE"
	   "DB-MAKE-TRIPLE"
	   "DB-QUERY"
	   "DB-REIFY"
	   "DB-DEL-SOURCE"
	   "DB-QUERY-BY-SOURCE"
	   "DB-SOURCES"
	   "IS-CONTAINER-P"
	   "DB-MERGE"
	   "DB-CLEAR"
	   "DB-FIND-CBD"
	   "DB-LOAD"
	   "DB-LOAD-USING-SOURCE"
	   "SOURCE-LOCATOR"
	   "SOURCE-OPEN-STREAM"
	   "SOURCE-CLOSE-STREAM"
	   "SOURCE-WITH-MODIFICATION"
	   "SOURCE-ORIGINAL-STREAM"
	   "SOURCE-MODIFICATION"
	   "INDEXED-DB"
	   "RDF-SYNTAX-NORMALIZER"      ; from rdf-parser.lisp
	   "RDF-PARSER"
	   "PARSER-DB"
	   "CLOSE-RDF-ELEMENT"
	   "MAKE-CONTAINER"
	   "PARSE-DB-FROM-STREAM"
	   "PARSE-DB-FROM-FILE"
	   "ATTACH-TO-PARENT"
	   "PARSE-USING-PARSETYPE"
	   "EXECUTE-DEFERRED-TASK"
	   "TASK"
	   "DEFER-TASK"
	   "TASK-TYPE"
	   "TASK-NODE"
	   "TASK-PARAMETER"
	   "PARSER-NODE"
	   "PARSER-PROPERTY"
	   "ENABLE-NODE-SHORTHAND"
	   "URL"                        ; from http-client.lisp
	   "HTTP-URL"
	   "FILE-URL"
	   "URL-STRING"
	   "URL-PATH"
	   "URL-HOST"
	   "URL-PORT"
	   "MAKE-URL"
	   "OPEN-HTTP-STREAM"
	   "HTTP-MESSAGE"
	   "HTTP-STATUS"
	   "HTTP-VERSION"
	   "HTTP-HEADERS"
	   "HTTP-BODY"
	   "GET-HEADER"
	   "HTTP-HEAD"
	   "HTTP-GET"
	   "WITH-HTTP-RESPONSE"
	   "PARSE-HTTP-DATE"
	   "PARSE-ISO8601-DATE"
	   "ISO8601-DATE-STRING"
	   "DAML-PARSER"                ; from daml-parser.lisp
	   "-DAML+OIL-URI-"
	   "-DAML-LIST-URI-"
	   "-DAML-FIRST-URI-"
	   "-DAML-REST-URI-"
	   "-DAML-NIL-URI-"
	   "DAML-LIST"
	   "DAML-CONS"
	   "LOAD-DB"                    ; from ivanhoe.lisp
	   "LOAD-DB-FROM-STREAM"
	   #-(and :openmcl :darwin :uffi) "*HTTP-PROXY*"
	   "FIND-HTTP-PROXY"
	   "PATH"
	   "PATH-EXPRESSION"
	   "INVERT-PATH"
	   "WALK-USING-FSA"
	   "COLLECT-USING-FSA"
	   "GET-VALUE"
	   "GET-ALL-VALUES"
	   "FRAME"
	   "OWN-SLOTS"
	   "VALUE"
	   "ALL-VALUES"
	   "ADD-VALUE"
	   "DEL-VALUE"
	   "RELATEDP"
	   "MAKE-LOCK"			; from processes.lisp
	   "WITH-LOCK"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   WILBUR FEATURES
;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :wilbur *features*)
  (pushnew :wilbur2 *features*))
