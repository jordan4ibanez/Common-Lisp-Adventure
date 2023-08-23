(defpackage #:constants
  (:nicknames :constants)
  (:use :cl))

(in-package :constants)

(export '(
          true
          false
          yes
          no))

(defconstant true t)
(defconstant false nil)

(defun yes()
  true)
(defun no()
  false)
