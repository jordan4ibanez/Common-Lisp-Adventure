;; Package in lisp is namespace in C++ and...package in java
;; System is like a library, organizes a bunch of files together.

;; So, namespace test-package I suppose.


(defpackage #:entity
  (:nicknames :ent)
  (:use :cl :delta-time :cloml))

(in-package :entity)

(export '(
          entity
          ))


(defclass entity ()
    ((id
       :initarg :id
       :accessor id)
     (pos
       :initform :pos
       :accessor pos)))
