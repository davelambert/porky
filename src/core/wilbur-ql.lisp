;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  wilbur-ql.lisp
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
;;;   Version: $Id: wilbur-ql.lisp,v 1.2 2006/02/04 21:19:02 ora Exp $
;;;
;;;   Purpose: This file implements the Wilbur Query Language (WilburQL) which essentially
;;;   is a simple API on top of the RDF data manager (in "core-data.lisp"). Much of the
;;;   functionality is modeled after the BEEF frame system:
;;;
;;;      Ora Lassila: "BEEF Reference Manual - A Programmer's Guide to the BEEF Frame
;;;        System", Second Version, Report HTKK-TKO-C46, Otaniemi (Finland), Department of
;;;        Computer Science, Helsinki University of Technology, 1991
;;;
;;;      Juha Hynynen and Ora Lassila: "On the Use of Object-Oriented Paradigm in a
;;;        Distributed Problem Solver", AI Communications 2(3): 142-151 (1989)
;;;
;;;   A description of the WilburQL itself can be found in the following paper:
;;;
;;;      Ora Lassila: "Taking the RDF Model Theory Out for a Spin", in: Ian Horrocks &
;;;        James Hendler (eds.): "The Semantic Web - ISWC 2002", Lecture Notes in Computer
;;;        Science 2342, pp.307-317, Springer Verlag, 2002
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS PATH
;;;
;;;   The path grammar implementation is derived from BEEF frame system.
;;;

(defclass path ()
  ((expression
    :accessor path-expression)
   (fsa
    :accessor path-fsa)))

(defmethod print-object ((path path) stream)
  (print-unreadable-object (path stream :type t)
    (prin1 (path-expression path) stream)))

(defmethod initialize-instance :after ((self path) &rest args &key expression)
  (declare (ignore args))
  (multiple-value-bind (fsa expression)
		       (make-path-fsa expression)
    (setf (path-fsa self) fsa
	  (path-expression self) expression)))

(defmethod invert ((path path))
  (make-instance 'path :expression (invert-path (path-expression path))))

(defstruct (path-node
            (:conc-name pn-)
            (:copier nil)
            (:constructor new-pn (link)))
  (link nil :read-only t)                    ; slot name i.e. link in the path
  (follows nil))                             ; possible followers of this node

(defstruct (path-fsa-state
            (:conc-name ps-)
            (:copier nil)
            (:constructor new-ps (positions)))
  (positions nil :read-only t)               ; positions defining this state
  (transitions nil))                         ; transitions from this state

(defstruct (path-fsa-transition
            (:conc-name pt-)
            (:copier nil)
            (:constructor new-pt (input index)))
  (input nil :read-only t)                   ; input symbol (= predicate name)
  (index nil :read-only t))                  ; index of the target state

(defmethod print-object ((self path-fsa-transition) stream)
  (print-unreadable-object (self stream :type t)
    (let ((input (pt-input self)))
      (typecase input
        (inverse-slot
         (format stream ":inv ~S->~D" (inverse-slot-node input) (pt-index self)))
        (node
         (format stream "~S->~D" input (pt-index self)))
        (default-value
         (format stream "(:value ~S)->~D" (default-value-value input) (pt-index self)))
        (t
         (format stream "~S->~D" input (pt-index self)))))))

(defparameter *path-fsas* (make-hash-table :test #'equal))

(defun canonical-path (expr)
  (etypecase expr
    (cons
     (destructuring-bind (op arg &rest args) expr
       (if arg
         (ecase op
           ((:rep* :rep+ :inv :value :filter :restrict :test :daemon)
            (assert (null args) nil "Extra operands for ~S in ~S" op expr)
            (case op
              (:rep+     (canonical-path `(:seq ,arg (:rep* ,arg))))
              (:rep*     `(,op ,(canonical-path arg)))
              (:inv      (canonical-path (invert-path arg)))
              (:value    (make-default-value arg))
	      (:filter   (make-instance 'path-uri-filter :key arg))
	      (:restrict (make-instance 'path-node-restriction :key arg))
	      (:test     (make-instance 'functional-restriction :key arg))
	      (:daemon   (make-access-daemon arg))))
           ((:seq :seq+ :or)
            (if args
              (let ((arg (canonical-path arg))
                    (args (mapcar #'canonical-path args)))
		(unless (or (atom arg) (not (eq op (first arg))))
                  (psetq arg (second arg)
                         args (append (cddr arg) args)))
                (cond ((rest args)
                       (canonical-path `(,op ,arg (,op ,@args))))
		      ((eq op :or)
                       `(,op ,(canonical-path (first args)) ,(canonical-path arg)))
                      (t
                       `(,op ,(canonical-path arg) ,(canonical-path (first args))))))
              (canonical-path arg))))
         (error 'query-syntax-error :thing op))))
    (string
     (node expr))
    (keyword
     (assert
      (member expr '(:members :any :predicate-of-object :predicate-of-subject :self)))
     expr)
    ((or node inverse-slot default-value path-filter access-daemon)
     expr)))

(defun invert-path (expr)
  (labels ((remove-defaults (x)
	     (remove 'default-value x :key #'type-of))
	   (i (p)
             (etypecase p
               (cons
                (ecase (first p)
                  (:or
                   `(,(first p)
                     ,@(mapcar #'i (reverse (remove-defaults (rest p))))))
                  ((:seq :seq+)
                   `(,(first p) ,@(mapcar #'i (reverse (remove-defaults (rest p))))))
                  (:rep*
                   `(:rep* ,(i (second p))))))
               (node
                (make-inverse-slot p))
               (inverse-slot
                (inverse-slot-node p))
	       (keyword
		(if (eq p :self) :self (make-inverse-slot p)))
               (default-value
                (error 'cannot-invert-default-value :thing (default-value-value p)))
	       (path-filter
		p))))
    (i (canonical-path expr))))

(defun make-path-fsa (expr)
  (when expr
    (let* ((e (canonical-path expr)))
      (values (or (gethash e *path-fsas*)
		  (setf (gethash e *path-fsas*) (construct-new-path-fsa e)))
	      e))))

(defvar *fsa-states/temporary* (make-array 8 :adjustable t :fill-pointer 0))

(defun construct-new-path-fsa (expr &aux (inputs nil))
  (labels ((decorate (x)
             (if (atom x)
               (let ((node (list (new-pn x))))
                 (pushnew x inputs)
                 (values node node nil))
               (case (pop x)
                 (:seq  (multiple-value-bind (first1 last1 null1) (decorate (first x))
                          (multiple-value-bind (first2 last2 null2) (decorate (second x))
                            (add-followers last1 first2)
                            (values (if null1 (union first1 first2) first1)
                                    (if null2 (union last1 last2) last2)
                                    (and null1 null2)))))
                 (:seq+ (multiple-value-bind (first1 last1 null1) (decorate (first x))
                          (multiple-value-bind (first2 last2) (decorate (second x))
                            (add-followers last1 first2)
                            (values (if null1 (union first1 first2) first1)
                                    (union last1 last2)
                                    null1))))
                 (:or   (multiple-value-bind (first1 last1 null1) (decorate (first x))
                          (multiple-value-bind (first2 last2 null2) (decorate (second x))
                            (values (union first1 first2)
                                    (union last1 last2)
                                    (or null1 null2)))))
                 (:rep* (multiple-value-bind (first last) (decorate (first x))
                          (add-followers last first)
                          (values first last t))))))
           (add-followers (from to)
             (dolist (i from) (unionf (pn-follows i) to)))
           (add-state (positions)
             (or (position positions *fsa-states/temporary*
                           :key #'ps-positions
                           :test #'(lambda (x y)
                                     (and (subsetp x y) (subsetp y x))))
                 (vector-push-extend (new-ps positions) *fsa-states/temporary*))))
    (setf (fill-pointer *fsa-states/temporary*) 0)
    (add-state (decorate `(:seq ,expr nil)))
    (do ((i 0 (1+ i)))
        ((= i (length *fsa-states/temporary*)))
      (let ((state (elt *fsa-states/temporary* i)))
        (dolist (input inputs)
          (let ((positions nil))
            (dolist (p (ps-positions state))
              (when (eq (pn-link p) input)
                (unionf positions (pn-follows p))))
            (when positions
              (let ((index (add-state positions)))
                (when input
                  (push (new-pt input index) (ps-transitions state)))))))))
    (map 'simple-vector #'(lambda (s)
			    (cons (and (member nil (ps-positions s) :key #'pn-link) t)
				  (reverse (ps-transitions s))))
	 *fsa-states/temporary*)))

(defmacro with-hash-pool ((var pool) &body body)
  `(let* ((,pool ,pool)
	  (,var (clrhash (or (pop ,pool) (make-hash-table :test #'eq)))))
    (unwind-protect (progn ,@body)
      (clrhash ,var))))

(defvar *walk-states/temporary* (list (make-hash-table :test #'eq)
                                      (make-hash-table :test #'eq)
                                      (make-hash-table :test #'eq)))

(defvar *collect-nodes/temporary* (list (make-hash-table :test #'eq)
					(make-hash-table :test #'eq)
					(make-hash-table :test #'eq)))

(defun walk-using-fsa (root fsa action db)
  (with-hash-pool (states *walk-states/temporary*)
    (labels ((walk (f i)
               (unless (member i (gethash f states) :test #'=)
                 (push i (gethash f states))
                 (let ((transitions (svref fsa i)))
                   (or (when (first transitions)
                         (funcall action f))
                       (when (typep f 'node)
                         (dolist (link (rest transitions))
                           (dolist (v (db-get-values db f (pt-input link)))
                             (let ((values (walk v (pt-index link))))
                               (when values
                                 (return-from walk-using-fsa values)))))))))))
      (declare (dynamic-extent #'walk))
      (when fsa
        (walk root 0)))))

(defun walk-using-fsa-remembering-path (root fsa action db)
  (with-hash-pool (states *walk-states/temporary*)
    (labels ((walk (f i pn pp)
	       (format t "~&WALK: ~S ~S ~S ~S ~S" f i pn pp (gethash f states))
               (unless (member i (gethash f states) :test #'=)
                 (push i (gethash f states))
		 (format t "~&PUSH: ~S ~S" f i)
                 (let ((transitions (svref fsa i))
		       (pn (cons f pn)))
                   (or (when (first transitions)
                         (funcall action f pn pp))
                       (when (typep f 'node)
                         (dolist (link (rest transitions))
			   (multiple-value-bind (values predicates)
						(db-get-values db f (pt-input link))
			     (assert (= (length values) (length predicates)))
			     (loop for v in values
				   for p in predicates
				   do (progn
					(format t "~&LOOP: ~S ~S" v p)
					(let ((values (walk v (pt-index link)
							    pn (cons p pp))))
					  (when values
					    (return-from walk-using-fsa-remembering-path
					      values)))))))))))))
      (declare (dynamic-extent #'walk))
      (when fsa
        (walk root 0 nil nil)))))

(defun collect-using-fsa (root fsa db)
  (with-hash-pool (node-hash *collect-nodes/temporary*)
    (let ((nodes nil))
      (flet ((collect-results (n)
	       (unless (gethash n node-hash)
		 (setf (gethash n node-hash) t)
		 (push n nodes)
		 nil)))
	(declare (dynamic-extent #'collect-results))
	(walk-using-fsa root fsa #'collect-results db)
	(nreverse nodes)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   STRUCTURE CLASS INVERSE-SLOT
;;;   STRUCTURE CLASS DEFAULT-VALUE
;;;

(defvar *inverse-slots* (make-hash-table :test #'eq))

(defun make-inverse-slot (node)
  (or (gethash node *inverse-slots*)
      (setf (gethash node *inverse-slots*) (%make-inverse-slot node))))

(defstruct (inverse-slot
	     (:copier nil)
	     (:constructor %make-inverse-slot (node)))
  node)

(defmethod print-object ((self inverse-slot) stream)
  (print-unreadable-object (self stream :type t)
    (let ((node (inverse-slot-node self)))
      (if (typep node 'node)
	(multiple-value-bind (name shortp)
			     (find-short-name *nodes* (node-uri (inverse-slot-node self)))
	  (format stream (if shortp "!~A" "!~S") name))
	(prin1 node stream)))))

(defstruct (default-value
	     (:copier nil)
	     (:constructor make-default-value (value)))
  value)

(defmethod print-object ((self default-value) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (default-value-value self) stream)))


(defstruct (access-daemon
	     (:copier nil)
	     (:constructor make-access-daemon (property)))
  property)

(defmethod print-object ((self access-daemon) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (access-daemon-property self) stream)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS PATH-FILTER
;;;   CLASS PATH-URI-FILTER
;;;   CLASS FUNCTIONAL-RESTRICTION
;;;

(defclass path-filter ()
  ((key
    :initarg :key
    :reader path-filter-key)))

(defmethod print-object ((self path-filter) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (path-filter-key self) stream)))

(defgeneric path-filter-match-p (filter node))

(defclass path-uri-filter (path-filter)
  ())

(defmethod path-filter-match-p ((filter path-uri-filter) (node node))
  (name-contains-pattern-p (node-uri node) (path-filter-key filter)))

(defmethod path-filter-match-p ((filter path-uri-filter) (literal literal))
  (name-contains-pattern-p (literal-string literal) (path-filter-key filter)))

(defclass path-node-restriction (path-filter)
  ())

(defmethod path-filter-match-p ((filter path-node-restriction) (node node))
  (eq node (path-filter-key filter)))

(defmethod path-filter-match-p ((filter path-node-restriction) (literal literal))
  nil)

(defclass functional-restriction (path-filter)
  ())

(defmethod path-filter-match-p ((filter functional-restriction) node)
  (funcall (path-filter-key filter) node))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE (FRAME SYSTEM API ADDITIONS)
;;;

#+:junk ; I am not sure this is necessary
(defmethod db-get-values :around ((db db) (frame node) path)
  (declare (ignore path))
  (let ((values (call-next-method)))
    (if (member :all values)
      (list :all)
      values)))

(defmethod db-get-values ((db db) frame path)
  (%db-get-values db (and (not (eq frame :all)) frame) path))

#+:junk
(defmethod db-get-values ((db db) (frame literal) (path node))
  (list (or (literal-datatype frame) !rdfs:Resource)))

(defun collect-inverse-members (node db)
  (mapcan #'(lambda (u)
	      (mapcar #'triple-subject (db-query db nil u node)))
	  (coerce *index-uris* 'list)))

(defun db-extract (db frame path triples key)
  (declare (ignore db frame path))
  (when triples
    (mapcar key triples)))

(defun %db-get-values (db frame path)
  ;; Here's where Wilbur spends its time...
  (etypecase path
    (node
     (mapcar #'triple-object (db-query db frame path nil)))
    (inverse-slot
     (let ((slot (inverse-slot-node path)))
       (etypecase slot
	 (node
	  (mapcar #'triple-subject (db-query db nil slot frame)))
	 (symbol
	  (ecase slot
	    (:predicate-of-object
	     (mapcar #'triple-object (db-query db nil frame nil)))
	    (:predicate-of-subject
	     (mapcar #'triple-subject (db-query db nil frame nil)))
	    (:members
	     (collect-inverse-members frame db))
	    (:any
	     (mapcar #'triple-subject (db-query db nil nil frame))))))))
    (path
     (collect-using-fsa frame (path-fsa path) db))
    (default-value
     (list (default-value-value path)))
    (symbol
     (ecase path
       (:predicate-of-object
	(mapcar #'triple-predicate
		(remove-duplicates (db-query db nil nil frame) :key #'triple-predicate)))
       (:predicate-of-subject
	(mapcar #'triple-predicate
		(remove-duplicates (db-query db frame nil nil) :key #'triple-predicate)))
       (:self
	(list frame))
       (:members
	(loop for i from 1
	      for v = (first (db-get-values db frame (node (index-uri i db))))
	      while v collect v))
       (:any
	(mapcar #'triple-object (db-query db frame nil nil)))))
    (cons
     (collect-using-fsa frame (make-path-fsa path) db))
    (path-filter
     (and (path-filter-match-p path frame) (list frame)))
    (access-daemon
     (db-compute-daemon-values db frame (access-daemon-property path)))))

(defmethod db-compute-daemon-values ((db db) frame slot)
  (case slot
    (!wilbur:timeStamp
     (list (db-make-literal db (iso8601-date-string (get-universal-time))
			    :datatype !xsd:dateTime)))
    (!wilbur:tripleCount
     (when (find frame (db-sources db))
       (list (db-make-literal db (prin1-to-string (length (db-query-by-source db frame)))
			      :datatype !xsd:integer))))))

(defmethod frames-related-p ((source node)
                             path
                             (sink node)
                             (db db)
                             action)
  (%frames-related-p source path sink db action))

(defun %frames-related-p (source path sink db action)
  (typecase path
    (node
     (not (null (db-query db source path sink))))
    (inverse-slot
     (frames-related-p sink (inverse-slot-node path) source db action))
    (cons
     (frames-related-p source (make-instance 'path :expression path) sink db action))
    (path
     (flet ((is-sink-p (node)
	      (when action
		(funcall action node))
	      (eq node sink)))
       (declare (dynamic-extent #'is-sink-p))
       (walk-using-fsa source (path-fsa path) #'is-sink-p db)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   ADDITIONAL FUNCTIONALITY
;;;

(defun triple-reified-p (triple db)
  (let ((s-statements (db-query db nil !rdf:subject (triple-subject triple))))
    (when s-statements
      (let ((o-statements (db-query db nil !rdf:object (triple-object triple))))
	(when o-statements
	  (let ((predicate (triple-predicate triple)))
	    (flet ((predicate-not-found (node)
		     (null (db-query db node !rdf:predicate predicate))))
	      (declare (dynamic-extent #'predicate-not-found))
	      (remove-if #'predicate-not-found
			 (intersection (mapcar #'triple-subject s-statements)
				       (mapcar #'triple-subject o-statements))))))))))

(defmethod get-some-values (frame path db index)
  (assert (null index))
  (db-get-values db frame path))
