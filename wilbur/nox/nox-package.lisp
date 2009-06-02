;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  nox-package.lisp
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
;;;   Purpose: Definition for the package NOX
;;;


(in-package "CL-USER")


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE NOX
;;;

(defpackage "NOX"
  (:nicknames "NOKIA-XML-CL"
	      "WILBUR-XML")
  (:use "COMMON-LISP"
	#+:mcl "CCL"
	#+:excl "EXCL"
	#+:sbcl "SB-SYS")
  (:export "XML-ERROR"                  ; from xml-util.lisp
	   "ERROR-THING"
	   "SYNTAX-ERROR"
	   "PI-TERMINATION-PROBLEM"
	   "DTD-TERMINATION-PROBLEM"
	   "UNEXPECTED-END-TAG"
	   "ERROR-EXPECTATION"
	   "UNKNOWN-DECLARATION"
	   "UNKNOWN-CHARACTER-REFERENCE"
	   "MALFORMED-URL"
	   "FEATURE-NOT-SUPPORTED"
	   "MISSING-DEFINITION"
	   "ERROR-DEFINITION-TYPE"
	   "MISSING-ENTITY-DEFINITION"
	   "MISSING-NAMESPACE-DEFINITION"
	   "XML-WARNING"
	   "*CURRENT-PARSER*"
	   "READ-USING"
	   "STRING-DICT-GET"
	   "STRING-DICT-GET-BY-VALUE"
	   "STRING-DICT-ADD"
	   "STRING-DICT-DEL"
	   "DO-STRING-DICT"
	   "MAKE-FILE-URL"
	   "MAKE-HTTP-URL"
	   "PARSE-URL"
	   "TOKEN"
	   "TOKEN-STRING"
	   "OPEN-TAG"
	   "CLOSE-TAG"
	   "ENTITY-DECLARATION"
	   "ENTITY-NAME"
	   "COMMENT"
	   "CHAR-CONTENT"
	   "TAG-COUNTERPART"
	   "TAG-ATTRIBUTE"
	   "TAG-ATTRIBUTES"
	   "TAG-EMPTY-P"
	   "TAG-NAMESPACES"
	   "START-ELEMENT"
	   "END-ELEMENT"
	   "CHAR-CONTENT"
	   "PROC-INSTRUCTION"
	   "START-DOCUMENT"
	   "END-DOCUMENT"
	   "MAYBE-USE-NAMESPACE"
	   "SAX-CONSUMER"
	   "SAX-CONSUMER-PRODUCER"
	   "SAX-CONSUMER-MODE"
	   "SAX-PRODUCER"
	   "SAX-PRODUCER-CONSUMER"
	   "SAX-FILTER"
	   "FIND-FIRST-PRODUCER"
	   "-WHITESPACE-CHARS-"
	   "WITH-RESOURCE-FROM-POOL"
	   "DEFINE-RESOURCE-POOL"
	   "COLLAPSE-WHITESPACE"
	   "*NAME-READER*"              ; from xml-parser.lisp
	   "XML-PARSER"
	   "GET-ENTITY"
	   "GET-CANONICAL-URI"
	   "PARSE"
	   "EXPAND-NAME-WITH-NAMESPACE"
	   "PARSE-FROM-STREAM"
	   "PARSE-FROM-FILE"
	   "XML-FORMATTER"
	   "REPLAY"
	   "REVERSE-EXPAND-NAME"
	   "TREE-PARSER"
	   "STRING->KEYWORD"
	   "PARSER-INTERPRET-CONTENT"
	   "-RDF-URI-"                  ; from rdf-constants.lisp
	   "-RDFS-URI-"
	   "-XSD-URI-"
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
	   "-RDFS-CONTAINER-URI-"
	   "-OWL-LIST-URI-"
	   "-OWL-FIRST-URI-"
	   "-OWL-REST-URI-"
	   "-OWL-NIL-URI-"
	   "-OWL-IMPORTS-URI-"))
