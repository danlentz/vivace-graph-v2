;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;
;;; FLEXI-BTREE
;;;
;;; Implement an enhanced BTree variant based on CL-BTREE, however is not limited to
;;; just numbers and strings, but can accomodate just about any kind of lisp object or
;;; datatype for both keys and values.  The goal is to produce a flexible and versatile
;;; persistent index somewhat similar in spirit to PLANKS-BTREE, but with efficiently
;;; buffered de/serialization, block-oriented disk io, proper disk-block caching semantics,
;;; write-ahead journaling log, and a polished, well-tested interface.
;;;
;;; By comparison, the following describes the original cl-btree
;;; semantics: Default type uses 32-bit unsigned integers as keys and
;;; values. The other type uses string as keys and values. The string
;;; B-tree can store anything readable as keys and values. The size of
;;; key strings or values is not fixed. String B-tree uses simply prin1
;;; to write and read to read keys and values from cl-swap-file block
;;; stream.
;;;
;;; More information on CL-BTREE may be found on the SourceForge project page
;;; https://sourceforge.net/projects/cl-btree/
;;;
;;; or on CLIKI at http://www.cliki.net/cl-btree


(in-package :vivace-graph-v2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fallback implementation for relations that have not been explicitly defined 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod b-tree-impl::key= (x y)
  "by default, equivalence holds if arguments are equal, eql, eq,
char-equal, =, are conses whose cars and cdrs are equalp, arrays of
congruent dimension whose active elements are equalp, structures of
common type whose slot-values are equalp, or are hash tables with
congruent test function and number of entries whose keys are all
associated with equalp values.  May not terminate for circular
arguments."
  (equalp x y))

(defmethod b-tree-impl::key< (x y)
  "If not explicitly defined, fall back to a (hopefully) consistent
ordering relation based on string comparison of lexical form.  As long
as this comparison evaluates consistently for any two given objects, this will be
sufficient to provide proper operation of the b-tree, even if the
result is otherwise meaningless."
  (string-lessp (write-to-string x) (write-to-string y)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computed implementations implicitly defined by the other, explicit relations 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod b-tree-impl::|KEY>|  (x y)
  "y < x implies x > y" 
  (b-tree-impl::key< y x))

(defmethod b-tree-impl::|KEY>=| (x y)
  (or
    (b-tree-impl::key= x y)
    (b-tree-impl::key< y x)))

(defmethod b-tree-impl::|KEY<=| (x y)
  (or
    (b-tree-impl::key= x y)
    (b-tree-impl::key< x y)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; explicit specification of equivalence relations 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; previously defined in cl-btree-impl
;; (defgeneric key= (a b)
;;   (:method ((a integer) (b integer))
;;     (= a b))
;;   (:method ((a string) (b string))
;;     (string= a b))
;;   (:method ((a string) (b symbol))
;;     (string= a (symbol-name b)))
;;   (:method ((a symbol) (b symbol))
;;     (string= (symbol-name a) (symbol-name b)))
;;   (:method ((a symbol) (b string))
;;     (string= (symbol-name a) b)))
  
(defmethod b-tree-impl::key= ((x puri:uri) (y puri:uri))
  (puri:uri= x y))

(defmethod b-tree-impl::key= ((x puri:uri) y)
  (b-tree-impl::key= (puri::uri-string x) y))

(defmethod b-tree-impl::key= (x (y puri:uri))
  (b-tree-impl::key= x (puri::uri-string y)))

(defmethod b-tree-impl::key= ((x pathname) y)
  (b-tree-impl::key= (namestring (truename x)) y))

(defmethod b-tree-impl::key= (x (y pathname))
  (b-tree-impl::key= x (namestring (truename y))))

(defmethod b-tree-impl::key= ((x local-time:timestamp) (y local-time:timestamp))
  (local-time:timestamp= x y))

(defmethod b-tree-impl::key= ((x uuid:uuid) (y uuid:uuid))
  (uuid:uuid= x y))

(defmethod b-tree-impl::key= ((x uuid:uuid) y)
  (b-tree-impl::key= (princ-to-string x) y))

(defmethod b-tree-impl::key= (x (y uuid:uuid))
  (b-tree-impl::key= x (princ-to-string y)))

(defmethod b-tree-impl::key= ((x symbol) (y symbol))
  (eq x y))

(defmethod b-tree-impl::key= ((x standard-object) (y standard-object))
  "shallow comparison of standard-objects based on their class and slot-values"

  (let ((x-class (class-of x))
         (y-class (class-of y)))

      (and (eq x-class y-class)
      (notany #'null
        (mapcar #'(lambda (slot-name)
                    (let ((x-boundp (slot-boundp x slot-name))
                           (y-boundp (slot-boundp y slot-name)))
                      (or (and (not x-boundp) (not y-boundp))
                        (and x-boundp y-boundp
                          (equalp (slot-value x slot-name) (slot-value y slot-name))))))
          (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots x-class)))))))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; explicit specification of ordering relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; previously defined in cl-btree-impl 
;; (defgeneric key< (a b)
;;   (:method ((a integer) (b integer))
;;     (< a b))
;;   (:method ((a string) (b string))
;;     (string< a b))
;;   (:method ((a string) (b symbol))
;;     (string< a (symbol-name b)))
;;   (:method ((a symbol) (b symbol))
;;     (string< (symbol-name a) (symbol-name b)))
;;   (:method ((a symbol) (b string))
;;     (string< (symbol-name a) b)))
  


(defmethod b-tree-impl::key< ((x puri:uri) y)
  (b-tree-impl::key< (princ-to-string x) y))

(defmethod b-tree-impl::key< (x (y puri:uri))
  (b-tree-impl::key< x (princ-to-string y)))

(defmethod b-tree-impl::key< ((x pathname) y)
  (b-tree-impl::key< (namestring (truename x)) y))

(defmethod b-tree-impl::key< (x (y pathname))
  (b-tree-impl::key< x (namestring (truename y))))

(defmethod b-tree-impl::key< ((x symbol) y)
  (b-tree-impl::key< (symbol-name x) y))

(defmethod b-tree-impl::key< (x (y symbol))
  (b-tree-impl::key< x (symbol-name y)))

(defmethod b-tree-impl::key< ((x uuid:uuid) y)
  (b-tree-impl::key< (uuid:print-bytes nil x) y))

(defmethod b-tree-impl::key< (x (y uuid:uuid))
  (b-tree-impl::key< x (uuid:print-bytes nil y)))

(defmethod b-tree-impl::key< ((x number) y)
  (b-tree-impl::key< (write-to-string x) y))

(defmethod b-tree-impl::key< (x (y number))
  (b-tree-impl::key< x (write-to-string y)))

(defmethod b-tree-impl::key< ((x timestamp) (y timestamp))
  (timestamp< x y))

(defmethod b-tree-impl::key< ((x number) (y timestamp))
  (< (timestamp-to-universal x) y))

(defmethod b-tree-impl::key< ((x timestamp) (y number))
  (< x (timestamp-to-universal y)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serializer/Deserializer Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flexi-serialize   (payload destination)
  (hu.dwim.serializer:serialize payload :output destination))

(defun flexi-deserialize (source)
  (hu.dwim.serializer:deserialize source))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flexi B-tree Constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun b-tree-impl::flexi-b-tree-create (filespec &key (minimum-degree 3) (if-exists :error)
                                          (block-size 4096))
  (declare (type integer minimum-degree))
  (b-tree-impl::initialize-b-tree (b-tree-impl::make-b-tree
                                    :minimum-degree minimum-degree
                                    :min-keys       (1- minimum-degree)
                                    :max-keys       (1- (* 2 minimum-degree))
                                    :max-children   (* 2 minimum-degree)
                                    :key-reader     #'flexi-deserialize
                                    :key-writer     #'flexi-serialize
                                    :value-reader   #'flexi-deserialize
                                    :value-writer   #'flexi-serialize
                                    :swap-file      (swap-file:create filespec
                                                      :if-exists if-exists
                                                      :block-size block-size))))

(defun b-tree-impl::flexi-b-tree-open (filespec &key (if-exists :overwrite)
                                        (if-does-not-exist :error))
  (b-tree-impl::read-header (b-tree-impl::make-b-tree
                              :key-reader     #'flexi-deserialize
                              :key-writer     #'flexi-serialize
                              :value-reader   #'flexi-deserialize
                              :value-writer   #'flexi-serialize
                              :swap-file      (swap-file:open filespec
                                                :if-exists if-exists
                                                :if-does-not-exist if-does-not-exist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CL-BTREE API REDEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun b-tree::create (filespec &key (type :default) (minimum-degree 3) (if-exists :error)
                        (block-size 4096))
  (declare (type integer minimum-degree))
  (case type
    (:string   (b-tree-impl::string-b-tree-create filespec
                 :minimum-degree minimum-degree :if-exists if-exists :block-size block-size))
    (:flexi    (b-tree-impl::flexi-b-tree-create  filespec
                 :minimum-degree minimum-degree :if-exists if-exists :block-size block-size))
    (t         (b-tree-impl::b-tree-create filespec
                 :minimum-degree minimum-degree :if-exists if-exists :block-size block-size))))

(defun b-tree::open (filespec &key (type :default) (minimum-degree 3) (block-size 4096)
                      (if-exists :overwrite) (if-does-not-exist :error if-does-not-exist-p))
  (unless (or (null if-exists) (member if-exists '(:overwrite :append :error)))
    (error "Unsupported :if-exists option: ~a." if-exists))
  (unless (or (null if-does-not-exist) (member if-does-not-exist '(:error :create)))
    (error "Unsupported :if-does-not-exist option: ~a." if-does-not-exist)) 
  (when (and (eql if-exists :append) (not if-does-not-exist-p)) (setq if-does-not-exist :create))  
  (if (probe-file filespec)
    (cond
      ((null if-exists)
        nil)
      ((eql if-exists :error)
        (error "B-Tree file exists: ~a." filespec))
      ((or (eql if-exists :overwrite) (eql if-exists :append))
        (case type
          (:string (b-tree-impl::string-b-tree-open filespec :if-exists :overwrite))
          (:flexi  (b-tree-impl::flexi-b-tree-open  filespec :if-exists :overwrite))
          (t       (b-tree-impl::b-tree-open filespec :if-exists :overwrite)))))
    (cond
      ((null if-does-not-exist)
        nil)
      ((eql if-does-not-exist :error)
        (error "B-Tree file ~a does not exist." filespec))
      ((eql if-does-not-exist :create)
        (b-tree::create filespec :type type :minimum-degree minimum-degree :block-size block-size)))))



