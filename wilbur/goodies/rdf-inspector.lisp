;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  rdf-inspector.lisp
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
;;;   Purpose: This file contains various kinds of functionality for visualizing as well
;;;   as browsing RDF data and WilburQL queries (using the MCL Inspector, PowerGrapher
;;;   and GraphWiz). Eventually, this functionality should be bundled with the
;;;   (forthcoming) RDF browser.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE-INSPECTOR
;;;
;;;   Extension of the MCL Inspector to allow browsing of RDF graphs.
;;;

(defclass node-inspector (inspector::usual-inspector)
  ((out-links
    :accessor inspector-out-links)
   (in-links
    :accessor inspector-in-links)))

(defmethod inspector::inspector-class ((node node))
  'node-inspector)

(defmethod initialize-instance :after ((i node-inspector) &rest args)
  (declare (ignore args))
  (flet ((sort-triples (triples)
           (sort (copy-list triples) #'string<
                 :key #'(lambda (x)
                          (node-uri (triple-predicate x))))))
    (let ((node (inspector::inspector-object i)))
      (setf (inspector-out-links i) (sort-triples (query node nil nil))
            (inspector-in-links i) (sort-triples (query nil nil node))))))

(defmethod inspector::compute-line-count ((i node-inspector))
  (+ (length (inspector-out-links i)) (length (inspector-in-links i)) 3))

(defmethod inspector::line-n ((i node-inspector) n)
  (let ((node (inspector::inspector-object i))
        (k (+ (length (inspector-out-links i)) 2)))
    (cond ((zerop n)
           (values (node-uri node) "URI" :colon))
          ((= n 1)
           (values nil "Properties" :comment))
          ((< n k)
           (let ((triple (elt (inspector-out-links i) (- n 2))))
             (values (triple-object triple)
                     (node-name (triple-predicate triple))
                     :colon)))
          ((= n k)
           (values nil "Incoming" :comment))
          (t
           (let ((triple (elt (inspector-in-links i) (- n k 1))))
             (values (triple-subject triple)
                     (node-name (triple-predicate triple))
                     :colon))))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-CLASS-TREE
;;;   CLASS RDF-CLASS-NODE
;;;
;;;   Extension of PowerGrapher to allow visualization of RDF graphs and class trees.
;;;

(defclass rdf-class-tree (pg:tree)
  ())

(defmethod pg:compute-root-nodes ((self rdf-class-tree)
                                  &key root (level most-positive-fixnum)
                                  &allow-other-keys)
  (let ((class (node root)))
    (list (pg:make-node self 'rdf-class-node class level nil :class class))))

(defclass rdf-class-node (wu:selectable-rectangle-mixin pg:text-node)
  ((class
    :initarg :class
    :reader pg:node-key)))

(defmethod pg:compute-node-text ((self rdf-class-node))
  (find-short-name *nodes* (node-uri (pg:node-key self))))

(defmethod pg:compute-node-children ((self rdf-class-node) level)
  (mapcar #'(lambda (triple)
              (let ((class (triple-subject triple)))
                (pg:make-node (pg:node-collection self)
                              (class-of self) class level self
                              :class class)))
          (query nil !rdfs:subClassOf (pg:node-key self))))

(defmethod wu:item-action ((self rdf-class-node))
  (inspect (pg:node-key self)))

(defun make-rdf-class-tree (root &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'pg:tree-window
         :root root
         :window-title (find-short-name *nodes* (node-uri root))
         :tree-class 'rdf-class-tree
         args))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-QUERY-TREE
;;;
;;;   Extension of PowerGrapher to allow visualization of WQL queries.
;;;

(defclass rdf-query-tree (pg:tree)
  ((children
    :initform nil
    :accessor tree-children)
   (terminal-nodes
    :initform nil
    :accessor tree-terminal-nodes)))

(defmethod pg:compute-root-nodes ((self rdf-query-tree)
                                  &key root (level most-positive-fixnum) query
                                  &allow-other-keys)
  (let ((n (node root))
        (children (make-hash-table :test #'eq))
        (terminal-nodes nil))
    (instrumented-walk-using-fsa n (make-path-fsa query)
                                 #'(lambda (node terminalp transitions)
                                     (when terminalp
                                       (pushnew node terminal-nodes))
                                     (dolist (tr transitions)
                                       (dolist (value (db-get-slot-values *db* node
                                                                          (pt-input tr)))
                                         (pushnew value (gethash node children))))
                                     nil)
                                 *db*)
    (setf (tree-children self) children
          (tree-terminal-nodes self) terminal-nodes)
    (list (pg:make-node self 'rdf-query-node n level nil :node n))))

(defclass rdf-query-node (pg:text-node)
  ((node
    :initarg :node
    :reader pg:node-key)
   (terminalp
    :initarg :terminalp
    :reader node-terminal-p)))

(defmethod view-draw-contents :after ((self rdf-query-node))
  (when (member (pg:node-key self) (tree-terminal-nodes (pg:node-collection self)))
    (wu:draw-rect self #@(1 1) (subtract-points (view-size self) #@(1 1)) :black nil)))

(defmethod pg:compute-node-children ((self rdf-query-node) level)
  (let ((node (pg:node-key self))
        (tree (pg:node-collection self)))
    (etypecase node
      (string nil)
      (node (mapcar #'(lambda (child)
                        (pg:make-node tree (class-of self) child level self
                                      :node child))
                    (gethash node (tree-children tree)))))))

(defmethod pg:compute-node-text ((self rdf-query-node))
  (let ((key (pg:node-key self)))
    (etypecase key
      (string (format nil "~S" key))
      (node (let ((uri (node-uri key)))
              (if uri
                (find-short-name *nodes* uri)
                (format nil "(~S)" (sxhash key))))))))

(defun make-rdf-query-tree (root query &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'pg:tree-window
         :root root
         :query query
         :tree-class 'rdf-query-tree
         :window-title (prin1-to-string query)
         args))


;;; --------------------------------------------------------------------------------------
;;;
;;;   Code to allow visualization of RDF graphs and WQL queries using GraphViz.
;;;

(defun make-rdf-query-dot (root query stream)
  (let ((n (node root))
        (links (make-hash-table :test #'eq))
        (terminal-nodes nil))
    (flet ((node= (x y)
             (eq (if (typep x 'inverse-slot)
                   (inverse-slot-node x)
                   x)
                 (if (typep y 'inverse-slot)
                   (inverse-slot-node x)
                   y))))
      (instrumented-walk-using-fsa n (make-path-fsa query)
                                   #'(lambda (node terminalp transitions)
                                       (when terminalp
                                         (pushnew node terminal-nodes))
                                       (dolist (tr transitions)
                                         (dolist (value (db-get-slot-values *db* node
                                                                            (pt-input tr)))
                                           (pushnew  (cons value (pt-input tr))
                                                     (gethash node links)
                                                     :test #'(lambda (x y)
                                                               (and (eq (car x) (car y))
                                                                    (node= (cdr x) (cdr y)))))))
                                       nil)
                                   *db*)
      (format stream "digraph G {~%")
      (maphash #'(lambda (node links)
                   (dolist (link links)
                     (destructuring-bind (child . prop) link
                       (etypecase prop
                         (inverse-slot
                          (format stream "  ~S -> ~S [label=~S];~%"
                                  (find-short-name *nodes* (node-uri node))
                                  (find-short-name *nodes* (node-uri child))
                                  (find-short-name *nodes* (node-uri (inverse-slot-node prop)))))
                         (node
                          (format stream "  ~S -> ~S [label=~S];~%"
                                  (find-short-name *nodes* (node-uri node))
                                  (find-short-name *nodes* (node-uri child))
                                  (find-short-name *nodes* (node-uri prop))))))))
               links)
      (dolist (node terminal-nodes)
        (format stream "  ~S [peripheries=2];~%" (find-short-name *nodes* (node-uri node))))
      (format stream "}~%"))))

(defun instrumented-walk-using-fsa (root fsa action db)
  (let* ((*walk-states/temporary* *walk-states/temporary*)
         (states (clrhash (or (pop *walk-states/temporary*)
                              (make-hash-table :test #'eq)))))
    (labels ((w (f i)
               (unless (member i (gethash f states) :test #'=)
                 (push i (gethash f states))
                 (let ((transitions (svref fsa i)))
                   (or (funcall action f (first transitions) (rest transitions))
                       (when (typep f 'node)
                         (dolist (link (rest transitions))
                           (dolist (v (db-get-slot-values db f (pt-input link)))
                             (let ((values (w v (pt-index link))))
                               (when values
                                 (return-from instrumented-walk-using-fsa
                                   values)))))))))))
      (declare (dynamic-extent #'w))
      (when fsa
        (w root 0)))))

(defun make-dot (db output-file)
  (with-open-file (stream output-file
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
    (make-dot-into-stream db stream)))

(defun make-dot-into-stream (db stream)
  (let ((nodes (make-hash-table :test 'eq)))
    (flet ((normalize (node)
             (cond ((typep node 'literal)
                    (let ((sym (gentemp)))
                      (setf (gethash sym nodes) (literal-string node))
                      sym))
                   ((null (node-uri node))
                    (let ((sym (gentemp)))
                      (setf (gethash sym nodes) node)
                      sym))
                   (t
                    (find-short-name *nodes* (node-uri node))))))
      (format stream "digraph G {~%")
      (dolist (tr (db-triples db))
        (let ((s (normalize (triple-subject tr)))
              (p (normalize (triple-predicate tr)))
              (o (normalize (triple-object tr))))
          (format stream "~S -> ~S [label=~S];~%" s o p)))
      (maphash #'(lambda (node label)
                   (cond ((stringp label)
                          (format stream "~S [shape=plaintext, label=\"\\\"~A\\\"\"];~%"
                                  node label))
                         ((null label)
                          (unless label
                            (format stream "~S [label=\"\"];~%"
                                    node)))))
               nodes)
      (format stream "}~%"))))

(defun fsa->dot (fsa stream)
  (format stream "digraph G {~%")
  (dotimes (i (length fsa))
    (destructuring-bind (terminalp &rest transitions) (elt fsa i)
      (dolist (tr transitions)
        (format stream "  ~S -> ~S [label=~S];~%"
                i (pt-index tr)
                (find-short-name *nodes* (node-uri (pt-input tr)))))
      (when terminalp
        (format stream "  ~S [peripheries=2];~%" i))))
  (format stream "}~%"))
