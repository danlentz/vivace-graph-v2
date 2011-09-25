;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :cl-user)

(defpackage :red-black-planks
  (:nicknames :rbp)
  (:use :common-lisp 
    :local-time
    :planks.btree :hh-redblack :funds
    :hu.dwim.def :hu.dwim.stefil))
