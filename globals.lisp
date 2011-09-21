(in-package #:vivace-graph-v2)

(defparameter *graph-words* (find-package :graph-words))

(defparameter *literals*    (make-hash-table :synchronized t :test 'equalp))
(defparameter *nodes*       (make-hash-table :synchronized t :test 'equalp))

(defparameter *store* nil)
(defparameter *store-table* (make-hash-table :synchronized t :test 'eql))

(defvar *default-context*   (uuid:make-null-uuid))

(defvar *default-location-defaults* (ensure-directories-exist
                                      (asdf:system-relative-pathname
                                        (asdf:find-system :vivace-graph-v2)
                                        "data/" :type "db")))

(defvar *constituent* nil
  "dynamic indication of current node's statement constituent type")

(defvar *depth* nil
  "dynamic indication of depth during descent in hierarchical index")

 

(defparameter *namespaces* (make-hash-table :synchronized t :test 'equalp))

(defparameter *read-uncommitted* t)

(defparameter *compression-enabled?* t)

;; Graphs
(defvar *graph* nil)
(defvar *graph-table* nil)

;; Logging
(defvar *syslog-program* "vivace-graph-v2")
(defvar *syslog-facility* sb-posix:log-local7)
(progn
  (defparameter *syslog-priorities* (make-hash-table))
  (setf (gethash :emerg *syslog-priorities*) sb-posix:log-emerg)
  (setf (gethash :alert *syslog-priorities*) sb-posix:log-alert)
  (setf (gethash :crit *syslog-priorities*) sb-posix:log-crit)
  (setf (gethash :err *syslog-priorities*) sb-posix:log-err)
  (setf (gethash :warning *syslog-priorities*) sb-posix:log-warning)
  (setf (gethash :warn *syslog-priorities*) sb-posix:log-warning)
  (setf (gethash :notice *syslog-priorities*) sb-posix:log-notice)
  (setf (gethash :info *syslog-priorities*) sb-posix:log-info)
  (setf (gethash :debug *syslog-priorities*) sb-posix:log-debug))

;; Prolog specials
(defparameter *occurs-check* t)
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0 "Counter for generating variable names.")
(defvar *functor* nil "The Prolog functor currently being compiled.")
(defvar *select-list* nil "Accumulator for prolog selects.")
(defvar *cont* nil "Continuation container for step-wise queries.")
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql))
(defparameter *prolog-trace* nil)

