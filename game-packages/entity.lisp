;; Package in lisp is namespace in C++ and...package in java
;; System is like a library, organizes a bunch of files together.

;; So, namespace test-package I suppose.


(defpackage #:entity
  (:nicknames :ent)
  (:use :cl :constants :delta-time))

(in-package :entity)

(export '(
          test-function))


(defclass entity ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)))
