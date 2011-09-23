;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

;; temporary
(in-package :cl-user)
(ql:quickload :hu.dwim.stefil+hu.dwim.def+swank)
(ql:quickload :hu.dwim.defclass-star+hu.dwim.def)
(ql:quickload :hu.dwim.defclass-star+swank)
(ql:quickload :hu.dwim.def+swank)

(defpackage :b-tree-flexi-tests
  (:use :common-lisp :hu.dwim.def :hu.dwim.stefil :hu.dwim.serializer)
  (:shadowing-import-from :vg :aprog1 :awhen :it))

(in-package :b-tree-flexi-tests)

(def suite* (btree :in root-suite))

(def (test :auto-call nil) same (x y)
  (is (b-tree-impl::key= x y)))

(def (test :auto-call nil) different (x y)
  (is (not (b-tree-impl::key= x y))))

(def (test :auto-call nil) is-not (thing)
  (is (not thing)))

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

(defvar *btree*      nil)
(defvar *type*       nil)
(defvar *dbfile*     nil)
(defvar *hashtable*  nil)

(def fixture default-b-tree
  (let* ((filename  (princ-to-string (uuid:make-v4-uuid)))
          (dirname  (namestring (ensure-directories-exist #P"/tmp/")))
          (walfile  (make-pathname :directory dirname :name filename :type "wal"))
          (*dbfile* (make-pathname :directory dirname :name filename :type "db"))
          (*type*   :default)
          (*btree*  (b-tree:open *dbfile* :type *type* :block-size 64 :if-exists :append)))
    (unwind-protect (-body-)
      (progn
        (b-tree:close *btree*)
        (awhen (probe-file *dbfile*) (delete-file it))
        (awhen (probe-file walfile)  (delete-file it))))))

(def fixture string-b-tree
  (let* ((filename  (princ-to-string (uuid:make-v4-uuid)))
          (dirname  (namestring (ensure-directories-exist #P"/tmp/")))
          (walfile  (make-pathname :directory dirname :name filename :type "wal"))
          (*dbfile* (make-pathname :directory dirname :name filename :type "db"))
          (*type*   :string)            
          (*btree*  (b-tree:open *dbfile* :type *type* :block-size 64 :if-exists :append)))
    (unwind-protect (-body-)
      (progn
        (b-tree:close *btree*)
        (awhen (probe-file *dbfile*) (delete-file it))
        (awhen (probe-file walfile) (delete-file it))))))

(def fixture flexi-b-tree
  (let* ((filename  (princ-to-string (uuid:make-v4-uuid)))
          (dirname  (namestring (ensure-directories-exist #P"/tmp/")))
          (walfile  (make-pathname :directory dirname :name filename :type "wal"))
          (*dbfile* (make-pathname :directory dirname :name filename :type "db"))
          (*type*   :flexi)
          (*btree*  (b-tree:open *dbfile* :type *type* :block-size 64 :if-exists :append)))
    (unwind-protect (-body-)
      (progn
        (b-tree:close *btree*)
        (awhen (probe-file *dbfile*) (delete-file it))
        (awhen (probe-file walfile) (delete-file it))))))


(def with-macro* with-flexi-and-default-btrees ()
  "compound testing harness that provides a convenient and compact
means to test flexi-b-tree in direct comparison with DEFAULT-B-TREE
against the exact same sets of code in order to ensure that
flexi-btree maintains exactly the same semantics and 100% backward
compatibility with the original, more limited implementations."
  (:printv "" "default-b-tree" :hr "")
  (time
    (with-fixture default-b-tree
      (-body-)))
  (:printv "" "flexi-b-tree" :hr "")
  (time
    (with-fixture flexi-b-tree
      (-body-))))


(def with-macro* with-flexi-and-string-btrees ()
  "compound testing harness that provides a convenient and compact
means to test flexi-b-tree in direct comparison with STRING-B-TREE
against the exact same sets of code in order to ensure that
flexi-btree maintains exactly the same semantics and 100% backward
compatibility with the original, more limited implementations."
  (:printv "" "string-b-tree" :hr "")
  (time
    (with-fixture string-b-tree
      (-body-)))
  (:printv "" "flexi-b-tree" :hr "")
  (time
    (with-fixture flexi-b-tree
      (-body-))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flexi vs Default BTREE: Basic API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def test basics/default.0 ()
  (with-fixture default-b-tree
    (let ((btree *btree*))
      (is (typep btree 'b-tree-impl::b-tree))
      (b-tree:insert btree 1 100)
      (is (eql 100 (b-tree:search btree 1)))
      (is (equalp '(100 1) (multiple-value-list (b-tree:min btree))))
      (is (equalp '(100 1) (multiple-value-list (b-tree:max btree))))
      (is (equalp '((1 100)) (b-tree:map 'list btree #'(lambda (k v) (list k v)))))
      (b-tree:delete btree 1)
      (is-not (b-tree:search btree 1))
      (is-not (b-tree:min btree))
      (is-not (b-tree:max btree))
      (is-not (b-tree:map 'list btree #'identity)))))


(def test basics/flexi-vs-default.0 ()
  (with-flexi-and-default-btrees ()    
    (let ((btree *btree*))
      (is (typep btree 'b-tree-impl::b-tree))
      (b-tree:insert btree 1 100)
      (is (eql 100 (b-tree:search btree 1)))
      (is (equalp '(100 1) (multiple-value-list (b-tree:min btree))))
      (is (equalp '(100 1) (multiple-value-list (b-tree:max btree))))
      (is (equalp '((1 100)) (b-tree:map 'list btree #'(lambda (k v) (list k v)))))
      (b-tree:delete btree 1)
      (is-not (b-tree:search btree 1))
      (is-not (b-tree:min btree))
      (is-not (b-tree:max btree))
      (is-not (b-tree:map 'list btree #'identity)))))

#|
;; 
;; default-b-tree
;; ------------------------------------------------------------
;; 
.........
Evaluation took:
  0.015 seconds of real time
  0.004111 seconds of total run time (0.001934 user, 0.002177 system)
  26.67% CPU
  42,617,792 processor cycles
  130,416 bytes consed
  
;; 
;; flexi-b-tree
;; ------------------------------------------------------------
;; 
.........
Evaluation took:
  0.004 seconds of real time
  0.004269 seconds of total run time (0.002248 user, 0.002021 system)
  100.00% CPU
  11,148,914 processor cycles
  163,344 bytes consed
|#  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flexi vs String BTREE: Basic API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test basics/string.0 ()
  (with-fixture string-b-tree
    (let ((btree *btree*))
      (is (typep btree 'b-tree-impl::b-tree))
      (b-tree:insert btree "a" "z")
      (is (equal "z" (b-tree:search btree "a")))
      (is (equalp '("z" "a") (multiple-value-list (b-tree:min btree))))
      (is (equalp '("z" "a") (multiple-value-list (b-tree:max btree))))
      (is (equalp '(("a" "z")) (b-tree:map 'list btree #'(lambda (k v) (list k v)))))
      (b-tree:delete btree "a")
      (is-not (b-tree:search btree "a"))
      (is-not (b-tree:min btree))
      (is-not (b-tree:max btree))
      (is-not (b-tree:map 'list btree #'identity)))))


(def test basics/flexi-vs-string.0 ()
  (with-flexi-and-string-btrees ()
    (let ((btree *btree*))
      (is (typep btree 'b-tree-impl::b-tree))
      (b-tree:insert btree "a" "z")
      (is (equal "z" (b-tree:search btree "a")))
      (is (equalp '("z" "a") (multiple-value-list (b-tree:min btree))))
      (is (equalp '("z" "a") (multiple-value-list (b-tree:max btree))))
      (is (equalp '(("a" "z")) (b-tree:map 'list btree #'(lambda (k v) (list k v)))))
      (b-tree:delete btree "a")
      (is-not (b-tree:search btree "a"))
      (is-not (b-tree:min btree))
      (is-not (b-tree:max btree))
      (is-not (b-tree:map 'list btree #'identity)))))

#|
;; 
;; string-b-tree
;; ------------------------------------------------------------
;; 
.........
Evaluation took:
  0.028 seconds of real time
  0.003169 seconds of total run time (0.001566 user, 0.001603 system)
  10.71% CPU
  78,807,106 processor cycles
  178,832 bytes consed
  
;; 
;; flexi-b-tree
;; ------------------------------------------------------------
;; 
.........
Evaluation took:
  0.003 seconds of real time
  0.003119 seconds of total run time (0.001645 user, 0.001474 system)
  100.00% CPU
  9,381,540 processor cycles
  153,056 bytes consed
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Persistence Check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def test persistent/flexi-vs-default ()
;;   (with-flexi-and-default-btrees ()
;;     (b-tree:insert *btree* 2 200)
;;     (is (eql 200 (b-tree:search *btree* 2)))
;;     (b-tree:close *btree*)
;;     (setf *btree* (b-tree:open *dbfile* :type *type* :block-size 64 :if-exists :append))
;;     (is (eql 200 (b-tree:search *btree* 2)))))


(def test basic/string.1 ()
  (with-fixture string-b-tree
    (is (typep *btree* 'b-tree-impl:b-tree))
    (b-tree:insert *btree* 'my-key "This is a test value.")
    (is (equal "This is a test value." (b-tree:search *btree* 'my-key)))))


(def test persistent/string.0 ()
  (when (probe-file "/tmp/b-tree-test.db")
    (delete-file "/tmp/b-tree-test.db"))
  (when (probe-file "/tmp/b-tree-test.wal")
    (delete-file "/tmp/b-tree-test.wal"))
  (let ((btree (b-tree:open "/tmp/b-tree-test.db" :type :string
                 :block-size 64 :if-exists :append))) 
    (b-tree:insert btree 'my-key "This is a test value.")
    (is (equalp "This is a test value." (b-tree:search btree 'my-key)))
    (b-tree:close btree))
  (let ((btree (b-tree:open "/tmp/b-tree-test.db" :type :string
                 :block-size 64 :if-exists :append)))
    (is (equalp "This is a test value." (b-tree:search btree 'my-key)))
    (b-tree:close btree)))

;; (def test persistent/flexi.0 ()
;;   (when (probe-file "/tmp/b-tree-test.db")
;;     (delete-file "/tmp/b-tree-test.db"))
;;   (when (probe-file "/tmp/b-tree-test.wal")
;;     (delete-file "/tmp/b-tree-test.wal"))
;;   (let ((btree (b-tree:open "/tmp/b-tree-test.db" :type :flexi
;;   :block-size 64 :if-exists :append))) 
;;     (b-tree:insert btree 'my-key "This is a test value.")
;;     (is (equalp "This is a test value." (b-tree:search btree 'my-key)))
;;     (b-tree:close btree))
;;   (let ((btree (b-tree:open "/tmp/b-tree-test.db" :type :flexi
;;   :block-size 64 :if-exists :append)))
;;     (is (equalp "This is a test value." (b-tree:search btree 'my-key)))
;;     (b-tree:close btree)))
