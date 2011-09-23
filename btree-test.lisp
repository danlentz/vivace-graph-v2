;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

;; temporary

(in-package :cl-user)

(ql:quickload :hu.dwim.stefil+hu.dwim.def+swank)
(ql:quickload :hu.dwim.defclass-star+hu.dwim.def)
(ql:quickload :hu.dwim.defclass-star+swank)
(ql:quickload :hu.dwim.def+swank)

(defpackage :b-tree-flexi-tests
  (:use :common-lisp :hu.dwim.def :hu.dwim.stefil :hu.dwim.serializer)
  (:shadowing-import-from :vg :aprog1 :it))

(in-package :b-tree-flexi-tests)

(def suite* (btree :in root-suite))

(def (test :auto-call nil) same (x y)
  (is (b-tree-impl::key= x y)))

(def (test :auto-call nil) different (x y)
  (is (not (b-tree-impl::key= x y))))


(def test relations/key= ()
  "there is definitely room for debate about the definition of some of
the individual equality semantics, but these tests are to verify their
operation as currently defined. In fact i'm sure at least some of the
equality semantics need to change"
  (same      (make-instance 'standard-object) (make-instance 'standard-object))  
  (same      (puri:uri "http://x.net")        (puri:uri "http://x.net"))
  (different (puri:uri "http://x.net")        (puri:uri "http://y.net"))
  (same      (find-class 'uuid:uuid)          (class-of (uuid:make-v4-uuid)))
  (different (uuid:make-v4-uuid)              (uuid:make-v4-uuid))
  (same      (uuid:make-null-uuid)            (uuid:make-null-uuid))
  (same      (uuid:make-null-uuid)            (princ-to-string (uuid:make-null-uuid)))
  (different "123"                            123)
  (same      123                              123)
  (same      'abcdefg                         "ABCDEFG")
  (same      :spoc                            "SPOC")
  (different :spoc                            'spoc)
  (same      'spoc                            'spoc)
  (different '#:spoc                          '#:spoc)  
  (same      (user-homedir-pathname)          *default-pathname-defaults*)
  (different (user-homedir-pathname)          #p"/tmp")
  (same      "/private/etc/"                  #p"/etc")
  (same      "x"                              "x")
  (different "abc"                            "def")
  (same      (lisp-implementation-version)    (lisp-implementation-version))
  (same      (asdf:find-system :asdf)         (asdf:find-system :asdf))
  (different (asdf:find-system :asdf)         (asdf:find-system :uuid))
  (same      #(1 :a #\x)                      #(1 :a #\x))
  (different '(1 :a #\x)                      #(1 :a #\x))
  (same      (make-hash-table)                (make-hash-table))
  (different (make-hash-table)                (aprog1 (make-hash-table) (setf (gethash 1 it) t)))
  (same      (local-time:today)               (local-time:today))
  (different (local-time:now)                 (local-time:today)))


    
