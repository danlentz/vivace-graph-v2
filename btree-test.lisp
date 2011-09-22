;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

;; temporary

(in-package :cl-user)

(ql:quickload :hu.dwim.stefil+hu.dwim.def+swank)
(ql:quickload :hu.dwim.defclass-star+hu.dwim.def)
(ql:quickload :hu.dwim.defclass-star+swank)
(ql:quickload :hu.dwim.def+swank)

(defpackage :b-tree-flexi-tests
  (:use :common-lisp :hu.dwim.def :hu.dwim.stefil :hu.dwim.serializer))

(in-package :b-tree-flexi-tests)

;;(def 
;; temporarily using assert for now until we get testing more organized

(flet ((is (x y)
         (assert (b-tree-impl::key= x y)))
        (is-not (x y)
          (assert (not (b-tree-impl::key= x y)))))
  
  (is     (make-instance 'standard-object) (make-instance 'standard-object))  
  (is     (puri:uri "http://x.net")        (puri:uri "http://x.net"))
  (is-not (puri:uri "http://x.net")        (puri:uri "http://y.net"))
  (is     (find-class 'uuid:uuid)          (class-of (uuid:make-v4-uuid)))
  (is-not (uuid:make-v4-uuid)              (uuid:make-v4-uuid))
  (is     (uuid:make-null-uuid)            (uuid:make-null-uuid))
  (is     (local-time:today)               (local-time:today))
  (is-not (local-time:now)                 (local-time:today))) 
    
