;; ASDF package description for vivace-graph-v2-test     -*- Lisp -*-

(in-package :cl-user)

(defpackage #:vivace-graph-v2-test-system (:use #:common-lisp #:asdf))

(in-package #:vivace-graph-v2-test-system)

(defsystem vivace-graph-v2-test
  :name "Vivace Graph Tests"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.2"
  :description "Vivace Graph Version 2 Test Suite"
  :long-description "Vivace Graph Version 2 Test Suite."
  :depends-on (:vivace-graph-v2
                :hu.dwim.def
                :hu.dwim.stefil
                :hu.dwim.defclass-star
                :hu.dwim.serializer
                :cl-fad
                :fiveam)
  :components ((:file "vivace-graph-v2-test-package")
                (:file "test-aux"             :depends-on ("vivace-graph-v2-test-package"))
                (:file "btree-test"           :depends-on ("test-aux"))
                (:file "test-scenarios"       :depends-on ("vivace-graph-v2-test-package"))
                (:file "vivace-graph-v2-test" :depends-on ("test-scenarios"))))



(defmethod asdf:perform :before ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  (declare (ignore op) (ignore sys))
  (asdf:load-system :vivace-graph-v2-test)
  (princ (make-string *print-right-margin* :initial-element #\=))
  (format t "~&~%Exectuting tests:~%"))
  
(defmethod asdf:perform ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  (declare (ignore op) (ignore sys))
  (format t "~%Journaling~%") 
  (eval (read-from-string "(wal-tests::run-all-tests         :wal-tests)"))  
  (format t "~%Binary IO~%") 
  (eval (read-from-string "(binary-file-tests::run-all-tests :binary-file-tests)"))
  (format t "~%Block IO~%") 
  (eval (read-from-string "(swap-file-tests::run-all-tests   :swap-file-tests)"))
  (format t "~%BTree~%") 
  (eval (read-from-string "(b-tree-impl-tests::run-all-tests :b-tree-impl-tests)"))
  (eval (read-from-string "(b-tree-api-tests::run-all-tests  :b-tree-api-tests)")))

(defmethod asdf:perform :after ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  (declare (ignore op) (ignore sys))
  (format t "~%Done.~%")
  (princ (make-string *print-right-margin* :initial-element #\=))
  (terpri))

(defmethod asdf:operation-done-p ((op test-op)(sys (eql (find-system :vivace-graph-v2))))
  "testing is always performed on-demand"
  (declare (ignore op) (ignore sys))
  nil)

(defmethod asdf:perform ((op test-op)(sys (eql (find-system :vivace-graph-v2-test))))
  (declare (ignore op) (ignore sys))
  (asdf:test-system :vivace-graph-v2))

(defmethod asdf:operation-done-p ((op test-op)(sys (eql (find-system :vivace-graph-v2-test))))
  "testing is always performed on-demand"
  (declare (ignore op) (ignore sys))
  nil)
