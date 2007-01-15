;;; -*- package: NOX; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  rdf-constants.lisp
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
;;;   Version: $Id: core-constants.lisp,v 1.1.1.1 2005/09/25 20:40:37 ora Exp $
;;;
;;;   Purpose: This file contains definitions for various constants used by the
;;;   RDF parser (mostly URIs). Given that the XML parser has to deal with the
;;;   issue of RDF M+S vagueness on the namespaces of RDF attributes (such as
;;;   "about"), the definitions in this file are in the NOX package.
;;;
;;;   Generally, I hate this stuff since I never seem to get the constant
;;;   definitions right vis-a-vis compile time vs. load-time. :-(
;;;


(in-package "NOX")


;;; --------------------------------------------------------------------------------------
;;;
;;;   HELPERS
;;;
  
(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-uri-  #."http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  (defconstant -rdfs-uri- #."http://www.w3.org/2000/01/rdf-schema#")
  (defconstant -xsd-uri-  #."http://www.w3.org/2001/XMLSchema#")
  (defconstant -owl-uri-  #."http://www.w3.org/2002/07/owl#")
  (defconstant -daml-uri- #."http://www.daml.org/2000/12/daml+oil#")
  
  (defmacro rdf-uri (string)  `(concatenate 'string -rdf-uri- ,string))
  (defmacro rdfs-uri (string) `(concatenate 'string -rdfs-uri- ,string))
  (defmacro xsd-uri (string)  `(concatenate 'string -xsd-uri- ,string))
  (defmacro owl-uri (string)  `(concatenate 'string -owl-uri- ,string))
  (defmacro daml-uri (string) `(concatenate 'string -daml-uri- ,string))

  (defconstant -alternate-rdf-uri-
    #."http://www.w3.org/TR/REC-rdf-syntax/")
  (defconstant -alternate-rdfs-uri-
    #."http://www.w3.org/TR/1999/PR-rdf-schema-19990303#"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S ATTRIBUTE URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-id-uri-              #.(rdf-uri "ID"))
  (defconstant -rdf-resource-uri-        #.(rdf-uri "resource"))
  (defconstant -rdf-about-uri-           #.(rdf-uri "about"))
  (defconstant -rdf-abouteach-uri-       #.(rdf-uri "aboutEach"))
  (defconstant -rdf-abouteachprefix-uri- #.(rdf-uri "aboutEachPrefix"))
  (defconstant -rdf-bagid-uri-           #.(rdf-uri "bagID"))
  (defconstant -rdf-parsetype-uri-       #.(rdf-uri "parseType"))
  (defconstant -rdf-datatype-uri-        #.(rdf-uri "datatype"))
  (defconstant -rdf-nodeid-uri-          #.(rdf-uri "nodeID"))
  (defconstant -xml-lang-attr-           "xml:lang"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S RESOURCE, PROPERTY, ETC. URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-description-uri- #.(rdf-uri "Description"))
  (defconstant -rdf-type-uri-        #.(rdf-uri "type"))
  (defconstant -rdf-rdf-uri-         #.(rdf-uri "RDF"))
  (defconstant -rdf-li-uri-          #.(rdf-uri "li"))
  (defconstant -rdf-statement-uri-   #.(rdf-uri "Statement"))
  (defconstant -rdf-subject-uri-     #.(rdf-uri "subject"))
  (defconstant -rdf-predicate-uri-   #.(rdf-uri "predicate"))
  (defconstant -rdf-object-uri-      #.(rdf-uri "object"))
  (defconstant -rdf-xmlliteral-uri-  #.(rdf-uri "XMLLiteral"))
  (defconstant -rdf-bag-uri-         #.(rdf-uri "Bag"))
  (defconstant -rdf-seq-uri-         #.(rdf-uri "Seq"))
  (defconstant -rdf-alt-uri-         #.(rdf-uri "Alt"))
  (defconstant -rdf-list-uri-        #.(rdf-uri "List"))
  (defconstant -rdf-first-uri-       #.(rdf-uri "first"))
  (defconstant -rdf-rest-uri-        #.(rdf-uri "rest"))
  (defconstant -rdf-nil-uri-         #.(rdf-uri "nil")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF SCHEMA URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdfs-resource-uri-           #.(rdfs-uri "Resource"))
  (defconstant -rdfs-class-uri-              #.(rdfs-uri "Class"))
  (defconstant -rdfs-subclassof-uri-         #.(rdfs-uri "subClassOf"))
  (defconstant -rdfs-subpropertyof-uri-      #.(rdfs-uri "subPropertyOf"))
  (defconstant -rdfs-seealso-uri-            #.(rdfs-uri "seeAlso"))
  (defconstant -rdfs-isdefinedby-uri-        #.(rdfs-uri "isDefinedBy"))
  (defconstant -rdfs-constraintresource-uri- #.(rdfs-uri "ConstraintResource"))
  (defconstant -rdfs-constraintproperty-uri- #.(rdfs-uri "ConstraintProperty"))
  (defconstant -rdfs-range-uri-              #.(rdfs-uri "range"))
  (defconstant -rdfs-domain-uri-             #.(rdfs-uri "domain"))
  (defconstant -rdfs-comment-uri-            #.(rdfs-uri "comment"))
  (defconstant -rdfs-label-uri-              #.(rdfs-uri "label"))
  (defconstant -rdfs-literal-uri-            #.(rdfs-uri "Literal"))
  (defconstant -rdfs-datatype-uri-           #.(rdfs-uri "Datatype"))
  (defconstant -rdfs-container-uri-          #.(rdfs-uri "Container "))
  (defconstant -rdfs-member-uri-             #.(rdfs-uri "member")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   XSD URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -xsd-string-uri-             #.(xsd-uri "string"))
  (defconstant -xsd-boolean-uri-            #.(xsd-uri "boolean"))
  ;;(defconstant -xsd-decimal-uri-            #.(xsd-uri "decimal"))
  (defconstant -xsd-float-uri-              #.(xsd-uri "float"))
  (defconstant -xsd-double-uri-             #.(xsd-uri "double"))
  (defconstant -xsd-datetime-uri-           #.(xsd-uri "dateTime"))
  ;;(defconstant -xsd-time-uri-               #.(xsd-uri "time"))
  (defconstant -xsd-date-uri-               #.(xsd-uri "date"))
  ;;(defconstant -xsd-gyearmonth-uri-         #.(xsd-uri "gYearMonth"))
  ;;(defconstant -xsd-gyear-uri-              #.(xsd-uri "gYear"))
  ;;(defconstant -xsd-gmonthday-uri-          #.(xsd-uri "gMonthDay"))
  ;;(defconstant -xsd-gday-uri-               #.(xsd-uri "gDay"))
  ;;(defconstant -xsd-gmonth-uri-             #.(xsd-uri "gMonth"))
  ;;(defconstant -xsd-hexbinary-uri-          #.(xsd-uri "hexBinary"))
  ;;(defconstant -xsd-base64binary-uri-       #.(xsd-uri "base64Binary"))
  ;;(defconstant -xsd-anyuri-uri-             #.(xsd-uri "anyURI"))
  (defconstant -xsd-normalizedstring-uri-   #.(xsd-uri "normalizedString"))
  ;;(defconstant -xsd-token-uri-              #.(xsd-uri "token"))
  ;;(defconstant -xsd-language-uri-           #.(xsd-uri "language"))
  ;;(defconstant -xsd-nmtoken-uri-            #.(xsd-uri "NMTOKEN"))
  ;;(defconstant -xsd-name-uri-               #.(xsd-uri "Name"))
  ;;(defconstant -xsd-ncname-uri-             #.(xsd-uri "NCName"))
  (defconstant -xsd-integer-uri-            #.(xsd-uri "integer"))
  ;;(defconstant -xsd-nonpositiveinteger-uri- #.(xsd-uri "nonPositiveInteger"))
  ;;(defconstant -xsd-negativeinteger-uri-    #.(xsd-uri "negativeInteger"))
  ;;(defconstant -xsd-long-uri-               #.(xsd-uri "long"))
  (defconstant -xsd-int-uri-                #.(xsd-uri "int"))
  ;;(defconstant -xsd-short-uri-              #.(xsd-uri "short"))
  ;;(defconstant -xsd-byte-uri-               #.(xsd-uri "byte"))
  ;;(defconstant -xsd-nonnegativeinteger-uri- #.(xsd-uri "nonNegativeInteger"))
  ;;(defconstant -xsd-unsignedlong-uri-       #.(xsd-uri "unsignedLong"))
  ;;(defconstant -xsd-unsignedint-uri-        #.(xsd-uri "unsignedInt"))
  ;;(defconstant -xsd-unsignedshort-uri-      #.(xsd-uri "unsignedShort"))
  ;;(defconstant -xsd-unsignedbyte-uri-       #.(xsd-uri "unsignedByte"))
  ;;(defconstant -xsd-positiveinteger-uri-    #.(xsd-uri "positiveInteger"))
  )


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF ATTRIBUTE LISTS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-attrs- '#.`(,-rdf-id-uri-
				,-rdf-resource-uri-
				,-rdf-about-uri-
				,-rdf-abouteach-uri-
				,-rdf-abouteachprefix-uri-
				,-rdf-bagid-uri-
				,-rdf-parsetype-uri-
				,-rdf-datatype-uri-
				,-rdf-nodeid-uri-
				,-xml-lang-attr-))

  (defconstant -rdf-attr-map- #.`'((,"ID"              . ,-rdf-id-uri-)
				   (,"resource"        . ,-rdf-resource-uri-)
				   (,"about"           . ,-rdf-about-uri-)
				   (,"aboutEach"       . ,-rdf-abouteach-uri-)
				   (,"aboutEachPrefix" . ,-rdf-abouteachprefix-uri-)
				   (,"bagID"           . ,-rdf-bagid-uri-)
				   (,"parseType"       . ,-rdf-parsetype-uri-)
				   (,"datatype"        . ,-rdf-datatype-uri-)
				   (,"nodeID"          . ,-rdf-nodeid-uri-))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   OWL URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -owl-list-uri-      #.(owl-uri "List"))
  (defconstant -owl-first-uri-     #.(owl-uri "first"))
  (defconstant -owl-rest-uri-      #.(owl-uri "rest"))
  (defconstant -owl-nil-uri-       #.(owl-uri "nil"))
  (defconstant -owl-imports-uri-   #.(owl-uri "imports")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   DAML+OIL URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -daml-list-uri-  #.(daml-uri "List"))
  (defconstant -daml-first-uri- #.(daml-uri "first"))
  (defconstant -daml-rest-uri-  #.(daml-uri "rest"))
  (defconstant -daml-nil-uri-   #.(daml-uri "nil")))
