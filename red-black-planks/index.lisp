;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;;

(in-package :red-black-planks)

(defparameter *heap-file* #p"/tmp/heap-btree.db")

(defvar *f-array* (funds:make-f-array 256))
(defvar *avl-tree* (funds:make-avl-tree))
(defvar *heap* (funds:make-heap))



(defclass f-array-persistent-red-black-tree (persistent-red-black-tree)
  ((objects :initform (make-f-array 256)  :accessor objects)))

(defun make-f-array-persistent-red-black-tree () 
  (let ((tree nil))
    (with-rb-transaction
      ((setf tree  (make-instance 'f-array-persistent-red-black-tree)))
      tree)))

(defvar *rbtree* (make-f-array-persistent-red-black-tree))

