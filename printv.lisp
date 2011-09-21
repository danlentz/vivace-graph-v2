;;;; -*- Mode:Lisp; Syntax:common-lisp; coding: utf-8-unix;

;;; Adapted from the Handy PRINTV Macro Written by Dan Corkill
;;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;;; Licensed under Apache License 2.0 

(in-package :vivace-graph-v2)

(defun printv-minor-separator ()
  (format *trace-output* "~&;; ~60,,,'-<-~>~%")
  (force-output *trace-output*))

(defun printv-major-separator ()
  (format *trace-output* "~&;;~%")
  (princ
    (concatenate 'string ";; "
      (make-string (- *print-right-margin* 5) :initial-element #\=)) *trace-output*)
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
             ;; Markup form:
             ((eq form ':ff) (list '(printv-major-separator)))
             ((eq form ':hr) (list '(printv-minor-separator)))
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


#||

 (assert (eql 7
             (printv :ff "example" :hr ""
                     nil t (make-instance 'standard-object)  *package*
                     (gethash 'x (make-hash-table)) (+ 3 4) :ff)))

;;
;; ===============================================================================================
;; example
;; ------------------------------------------------------------
;; 
;;   NIL => NIL
;;   T => T
;;   (MAKE-INSTANCE 'STANDARD-OBJECT) => #<STANDARD-OBJECT {10043C4A31}>
;;   *PACKAGE* => #<PACKAGE "VIVACE-GRAPH-V2">
;;   (GETHASH 'X (MAKE-HASH-TABLE)) => NIL; NIL
;;   (+ 3 4) => 7
;;
;; ===============================================================================================

||#
