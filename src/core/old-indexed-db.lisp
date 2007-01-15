(defclass indexed-db (db)
  ((by-subject
    :initform (make-triple-index nil)
    :reader db-by-subject)
   (by-predicate
    :initform (make-triple-index nil)
    :accessor db-by-predicate)
   (by-object
    :initform (make-triple-index nil)
    :accessor db-by-object)
   (by-source
    :initform (make-triple-index nil)
    :reader db-by-source))
  (:default-initargs
    :rdf-schema-pathname "wilbur:schemata;rdf-schema.rdf"))

(defmethod initialize-instance :after ((self indexed-db)
                                       &key (emptyp nil)
                                            rdf-schema-pathname
                                       &allow-other-keys)
  (unless emptyp
    (db-load self (make-file-url rdf-schema-pathname)
	     :db self :merge-results-p nil)))

(defmethod db-add-triple ((db indexed-db) (triple triple))
  (multiple-value-bind (actual-triple addedp new-sources)
                       (call-next-method)
    (cond (addedp      ; CASE 1: Triple actually added
	   (triple-index-add triple (db-by-subject db) (triple-subject triple))
	   (triple-index-add triple (db-by-predicate db) (triple-predicate triple))
	   (triple-index-add triple (db-by-object db) (triple-object triple))
	   (when new-sources
	     (dolist (source new-sources)
	       (triple-index-add triple (db-by-source db) source)))
	   (values triple t new-sources))
	  (new-sources ; CASE 2: Only new source(s) added
	   (dolist (source new-sources)
	     (triple-index-add triple (db-by-source db) source))
	   (values actual-triple nil new-sources))
	  (t           ; CASE 3: Nothing added, source null
	   (values actual-triple nil nil)))))

;;; needs to be fixed vis-a-vis sources
(defmethod db-del-triple ((db indexed-db) (triple triple) &optional source)
  (multiple-value-bind (triple deletedp new-sources)
		       (call-next-method)
    (cond (deletedp
	   (triple-index-rem triple (db-by-subject db) (triple-subject triple))
	   (triple-index-rem triple (db-by-predicate db) (triple-predicate triple))
	   (triple-index-rem triple (db-by-object db) (triple-object triple))
	   (dolist (s (triple-sources triple))
	     (triple-index-rem triple (db-by-source db) s))
	   (values triple t nil))
	  (t
	   (triple-index-rem triple (db-by-source db) source)
	   (values triple nil new-sources)))))

(defmethod db-query ((db indexed-db) subject predicate object)
  (flet ((filter (tr k s)
           (if k (remove k tr :test-not #'eq :key s) tr)))
    (declare (dynamic-extent #'filter))
    (cond (subject
           (filter (filter (triple-index-get (db-by-subject db) subject)
                           predicate #'triple-predicate)
                   object #'triple-object))
          (object
           (filter (triple-index-get (db-by-object db) object)
                   predicate #'triple-predicate))
          (predicate
           (triple-index-get (db-by-predicate db) predicate))
          (t
           (db-triples db)))))

(defmethod db-del-source ((db indexed-db) (source node))
  (dolist (triple (db-query-by-source db source))
    (db-del-triple db triple source)))

(defmethod db-query-by-source ((db indexed-db) (source node))
  (triple-index-get (db-by-source db) source))

(defmethod db-sources ((db indexed-db))
  (let ((sources nil))
    (maphash #'(lambda (key data)
                 (declare (ignore data))
                 (push key sources))
             (db-by-source db))
    sources))

(defmethod db-clear :after ((db indexed-db))
  (triple-index-clear (db-by-subject db))
  (triple-index-clear (db-by-predicate db))
  (triple-index-clear (db-by-object db))
  (triple-index-clear (db-by-source db)))
