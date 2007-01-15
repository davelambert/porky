;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  literal.lisp
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
;;;   Version: $Id: literal.lisp,v 1.2 2006/02/04 21:18:52 ora Exp $
;;;
;;;   Purpose: Definition of the class LITERAL and associated functionality
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   LITERAL PROTOCOL
;;;

(defgeneric literal-string (literal))
(defgeneric literal-language (literal))
(defgeneric literal-datatype (literal))
(defgeneric literal-value (literal))
(defgeneric (setf literal-value) (value literal))
(defgeneric compute-literal-value (literal datatype string))
(defgeneric literal= (literal other-literal))
(defgeneric compute-literal-value-error (literal datatype string &rest options))
(defgeneric literal-value->string (datatype value))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS LITERAL
;;;

(defclass literal ()
  ((string
    :initarg :string
    :initform nil
    :reader literal-string)
   (language
    :initarg :language
    :initform nil
    :reader literal-language)
   (datatype
    :initarg :datatype
    :initform nil
    :reader literal-datatype)
   (value
    :accessor literal-value)))

(defmethod literal-string ((literal string))
  literal)

(defmethod literal-language ((literal string))
  nil)

(defmethod literal-datatype ((literal string))
  nil)

(defmethod literal-value ((literal string))
  literal)

(defmethod literal-value :around ((literal literal))
  (if (slot-boundp literal 'value)
    (call-next-method)
    (setf (literal-value literal)
	  (compute-literal-value literal
				 (literal-datatype literal) (literal-string literal)))))

(defmethod print-object ((self literal) stream)
  (princ #\# stream)
  (print-literal-for-ntriples self stream))

(defmethod compute-literal-value ((literal literal)
                                  (datatype null)
                                  string)
  string)

(defun %literal= (string datatype language other-literal)
  (and (string= string (literal-string other-literal))
       (if datatype
	 (eql datatype (literal-datatype other-literal))
	 (let ((other-language (literal-language other-literal)))
	   (or (and (null language) (null other-language))
	       (string-equal language other-language))))))

(defmethod literal= ((literal string) (other-literal string))
  (string= literal other-literal))

(defmethod literal= ((literal literal) (other-literal literal))
  (%literal= (literal-string literal)
	     (literal-datatype literal) (literal-language literal)
	     other-literal))

(defmethod literal= ((literal literal) (other-literal string))
  (%literal= other-literal nil nil literal))

(defmethod literal= ((literal string) (other-literal literal))
  (%literal= literal nil nil other-literal))

(defmethod literal= (literal other-literal)
  (declare (ignore literal other-literal))
  nil)

(defmethod literal-language-match-p ((literal literal) language)
  (string-equal (literal-language literal) language :end1 (length language)))

(defmethod literal-language-match-p (thing language)
  (declare (ignore thing language))
  nil)

(defmethod compute-literal-value-error ((literal literal)
                                        (datatype node)
                                        string
                                        &key (value string)
                                             (warn-only-p nil))
  (unless warn-only-p
    (cerror (format nil "Use value ~S instead" value)
            'datatype-parse-error :thing string))
  (warn "Ignoring literal datatype ~S for literal ~S" datatype string)
  value)
                                        

;;; --------------------------------------------------------------------------------------
;;;
;;;   LITERAL PARSING
;;;
;;;   This is the template for COMPUTE-LITERAL-VALUE methods:
;;;
;;;   (defmethod compute-literal-value ((literal literal)
;;;                                     (datatype (eql !xsd:...))
;;;                                     string)
;;;     ...)
;;;

(defmethod compute-literal-value ((literal literal)
                                  (datatype node)
                                  string)
  (compute-literal-value-error literal datatype string :warn-only-p t))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:string))
                                  string)
  string)

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:boolean))
                                  string)
  (cond ((or (string= string "1") (string= string "true"))  t)
        ((or (string= string "0") (string= string "false")) nil)
        (t (compute-literal-value-error literal datatype string :value nil))))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:float))
                                  string)
  (compute-literal-value-float literal datatype string))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:double))
                                  string)
  (compute-literal-value-float literal datatype string))

(defun compute-literal-value-float (literal datatype string)
  (multiple-value-bind (value n)
                       (read-from-string string :eof-error-p nil)
    (if (and value (numberp value) (= n (length string)))
      (float value)
      (compute-literal-value-error literal datatype string :value 1.0))))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:dateTime))
                                  string)
  (or (ignore-errors (parse-iso8601-date string))
      (compute-literal-value-error literal datatype string :value 0)))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:date))
                                  string)
  (or (and (= (length string) 10)
	   (ignore-errors (parse-iso8601-date string)))
      (compute-literal-value-error literal datatype string :value 0)))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:normalizedString))
                                  string)
  (flet ((illegalp (c)
	   (or (char= c #\Return)
	       (char= c #\Linefeed)
	       (char= c #\Tab))))
    (declare (dynamic-extent #'illegalp))
    (if (find-if #'illegalp string)
      (compute-literal-value-error literal datatype string
				   :value (substitute-if #\Space #'illegalp string))
      string)))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:integer))
                                  string)
  (compute-literal-value-integer literal datatype string))

(defmethod compute-literal-value ((literal literal)
				  (datatype (eql !xsd:int))
				  string)
  (compute-literal-value-integer literal datatype string))

(defun compute-literal-value-integer (literal datatype string)
  (or (parse-integer string :junk-allowed t)
      (compute-literal-value-error literal datatype string :value 0)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   OUTPUT FUNCTIONS
;;;

(defmethod print-literal-for-ntriples ((literal literal) stream)
  (let ((datatype (literal-datatype literal)))
    (format stream "~S~@[@~A~]~@[^^<~A>~]"
            (literal-string literal)
            (literal-language literal)
            (and datatype (find-short-name *nodes* (node-uri datatype))))))

(defmethod literal-value->string ((datatype (eql !xsd:string))
                                  (value string))
  value)

(defmethod literal-value->string ((datatype (eql !xsd:boolean))
                                  value)
  (if value "true" "false"))

(defmethod literal-value->string ((datatype (eql !xsd:float))
                                  (value float))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype (eql !xsd:double))
                                  (value float))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype (eql !xsd:dateTime))
                                  (value integer))
  (iso8601-date-string value))

(defmethod literal-value->string ((datatype (eql !xsd:date))
                                  (value integer))
  (iso8601-date-string value t))

(defmethod literal-value->string ((datatype (eql !xsd:normalizedString))
                                  (value string))
  (flet ((illegalp (c)
	   (or (char= c #\Return)
	       (char= c #\Linefeed)
	       (char= c #\Tab))))
    (declare (dynamic-extent #'illegalp))
    (assert (not (find-if #'illegalp value)))
    value))

(defmethod literal-value->string ((datatype (eql !xsd:integer))
				  (value integer))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype (eql !xsd:int))
				  (value integer))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype null)
				  (value string))
  value)


;;; --------------------------------------------------------------------------------------
;;;
;;;   LITERAL SHORTHAND SYNTAX
;;;

(defun literal (string &rest options)
  (declare (dynamic-extent options))
  (apply #'db-make-literal *db* string options))

(defmethod make-load-form ((literal literal) &optional env)
  (declare (ignore env))
  (let ((datatype (literal-datatype literal))
	(language (literal-language literal)))
    `(literal ,(literal-string literal)
              ,@(and datatype `(:datatype ,datatype))
              ,@(and language `(:language ,language)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun inline-literal-reader (stream char arg)
    (declare (ignore arg))
    (unread-char char stream)
    (let ((string (read stream t nil t)))
      ;; later, when I get around to it, we will also read datatype and language
      (literal string)))

  (defun enable-literal-shorthand ()
    (set-dispatch-macro-character #\# #\" #'inline-literal-reader))

  (enable-literal-shorthand))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS INTERNED-LITERAL
;;;   MIXIN CLASS INTERNED-LITERAL-DB-MIXIN
;;;   CLASS INTERNED-LITERAL-INDEXED-DB
;;;

(defclass interned-literal (literal node)
  ())

(defmethod literal= ((literal interned-literal) (other-literal interned-literal))
  (eq literal other-literal))

(defclass interned-literal-db-mixin () ; mix with class db
  ((literal-index
    :initform (make-hash-table :test #'equal)
    :reader db-literal-index))
  (:default-initargs
   :literal-class 'interned-literal))

(defmethod db-literal-index-get ((db interned-literal-db-mixin) string
				 &key datatype language)
  (find-if #'(lambda (literal)
	       (%literal= string datatype language literal))
	   (gethash string (db-literal-index db))))

(defmethod (setf db-literal-index-get) ((literal interned-literal)
					(db interned-literal-db-mixin) string)
  ;; "It is an error" to call this without first checking for the prior existence
  ;; of the literal in the index
  (push literal (gethash string (db-literal-index db)))
  literal)

(defmethod db-make-literal ((db interned-literal-db-mixin) string &rest options)
  (or (apply #'db-literal-index-get db string options)
      (setf (db-literal-index-get db string) (call-next-method))))

(defclass interned-literal-indexed-db (interned-literal-db-mixin indexed-db)
  ())
