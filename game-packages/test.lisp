;; Package in lisp is namespace in C++ and...package in java
;; System is like a library, organizes a bunch of files together.

;; So, namespace test-package I suppose.


(defpackage #:test-package
  (:nicknames :test)
  (:use :cl))

(in-package #:test)

(export '(test-function))

(defun test-function()
  (print "test success"))


(defclass entity ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)))
