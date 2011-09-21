;;;; -*- Mode:Lisp; Syntax:common-lisp; coding: utf-8-unix;

;;; Adapted from the Handy PRINTV Macro Written by Dan Corkill
;;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;;; Licensed under Apache License 2.0 

(in-package :vg)

(defun printv-separator ()
  (format *trace-output* "~&;; ~60,,,'-<-~>~%")
  (force-output *trace-output*))

(defun printv-form-printer (form)
  (typecase form
    ;; String (label):
    (string (format *trace-output* "~&;; ~a~%" form))
    ;; Evaluated form:
    ((or cons (and symbol (not keyword)))
      (format *trace-output* "~&;;   ~w =>" form))
    ;; Self-evaluating form:
    (t (format *trace-output* "~&;;  ~s~%" form)))
  (force-output *trace-output*))

(defun printv-values-printer (values-list)
  (format *trace-output* "~:[ [returned 0 values]~;~:*~{ ~w~^;~}~]~%"  values-list)
  (force-output *trace-output*))

(defun printv-expander (forms &optional values-trans-fn) ;; Allow for customized printv'ers:
  (let ((result-sym (gensym)))
    `(let ((*print-readably* nil) ,result-sym)
       ,@(loop for form in forms nconcing
           (cond
             ;; Separator requested?
             ((eq form ':hr)
               ;; list used for splicing protection...
               (list '(printv-separator)))
             ;; Evaluated form:
             ((or (consp form) (and (symbolp form) (not (keywordp form))))
               `((printv-form-printer ',form)
                  (printv-values-printer
                    (setf ,result-sym ,(if values-trans-fn
                                         `(funcall ,values-trans-fn
                                            (multiple-value-list ,form))
                                         `(multiple-value-list ,form))))))
             ;; Self-evaluating form:
             (t `((printv-form-printer 
                    (car (setf ,result-sym (list ,form))))))))
       (values-list ,result-sym))))

(defmacro printv (&rest forms)
  (printv-expander forms))

(defmacro :pv (&rest forms)
  (printv-expander forms))

