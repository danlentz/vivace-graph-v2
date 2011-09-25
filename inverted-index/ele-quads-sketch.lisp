;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :db)

;; NODES

(def (class* eas) node ()
  ((name  :accessor node-name :initarg :name :index t)
    (data :accessor node-data :initform nil :initarg :data))
  (:metaclass persistent-metaclass))


(def constructor node ()
  (unless (slot-boundp -self- 'name)
    (setf (node-name -self-)
      (uuid:make-v5-uuid uuid:+namespace-url+ (princ-to-string (ele::oid -self-))))))


(def print-object node () 
  (princ (node-name -self-)))
  

;; the "Quad"

(def (class* eas) quad (node)
  ((subject    :accessor subject   :index t :initform nil :initarg :s)
    (predicate :accessor predicate :index t :initform nil :initarg :p)
    (object    :accessor object    :index t :initform nil :initarg :o)
    (context   :accessor context   :index t :initform nil :initarg :c))
  (:metaclass persistent-metaclass))


(def constructor quad ()
  (setf (node-name -self-)
    (uuid:make-v5-uuid uuid:+namespace-url+
      (format nil "~A ~A ~A ~A"
        (subject -self-)
        (predicate -self-)
        (object -self-)
        (context -self-)))))


(def (function e) quad (&optional s p o c)
  (make-instance 'quad :s s :p p :o o :c c))


(def (function e) subjects (quads)
  (mapcar #'subject quads))


(def (function e) objects (quads)
  (mapcar #'subject quads))


(def (function e) predicates (quads)
  (mapcar #'predicate quads))


(def (function e) contexts (quads)
  (mapcar #'context quads))


(defmethod print-object ((q quad) stream)
  (format stream "#<QUAD: ~A ~A ~A ~A>" (subject q) (predicate q) (object q) (context q)))

;; (assert (uuid:uuid= (db:node-name (db:quad)) (db:node-name (db:quad))))


(defun oid (obj)
  (elephant::oid obj))


(def (function e) node-p (thing)
  (subtypep (type-of thing) 'node))


(def (function e) as-node (name)
  (if (node-p name)  name
    (find-node name)))


(def (function e) find-node (name &optional (error nil))
  (if (node-p name) name
    (let ((obj (get-instance-by-value 'node 'name name)))
      (if obj obj
        (when error (cerror "Make thing?" "Can't find thing ~A" name)
	  (add-node name))))))

  
(def (function e) quad-match (quad subject predicate object &optional context)  
  (and
    (or (null subject)   (eq (subject   quad) subject))
    (or (null predicate) (eq (predicate quad) predicate))       
    (or (null object)    (eq (object    quad) object))
    (or (null context)   (eq (context   quad) context))))


(def (function e) find-quad (sub pred obj &optional ctx)
  (let ((sub   (when sub  (as-node sub)))
         (pred (when pred (as-node pred)))	
	 (obj  (when obj  (as-node obj)))
         (ctx  (when ctx  (as-node ctx))))
    (let ((quad
	   (catch 'found 
	     (map-inverted-index ;; this is the key operator. expand on this.
	      (lambda (key quad)
		(declare (ignore key))
		(when (quad-match quad sub nil obj ctx)
		  (throw 'found quad)))
               'quad 'predicate :value pred))))
      quad)))


(def (function e) find-quads (subject predicate object &optional context)
  (let ((idx (or
               (and predicate 'predicate)
               (and subject   'subject)
               (and object    'object)
               (and context   'context)))
         (predicate (when predicate (as-node predicate)))
         (subject   (when subject   (as-node subject)))
         (object    (when object    (as-node object)))
         (context   (when context   (as-node context)))
         results)
         (assert idx)
         (flet ((find-quad (key quad)
		  (declare (ignore key))
		  (when (quad-match quad subject predicate object context)
		    (push quad results))))
           (declare (dynamic-extent (function find-quad)))
           (map-inverted-index #'find-quad 'quad idx :value (or subject predicate object context)))
         results))



(def (function e) add-quad (sub pred obj ctx)
  (with-transaction ()
    (let ((quad (find-quad sub pred obj ctx)))
      (if quad quad
        (make-instance 'quad
          :s (as-node sub)
          :p (as-node pred)
          :o (as-node obj)
          :c (as-node ctx))))))


(def (function e) add-node (name &optional data)
  (with-transaction ()
    (let ((node (find-node name nil)))
      (if node node
	  (make-instance 'node :name name :data data)))))


(def (function e) quad-exists-p (sub pred obj ctx)
  (when (find-quad (as-node sub) (as-node pred) (as-node obj) (as-node ctx))
    t))
