(defpackage #:internal-opengl
  (:nicknames :igl)
  (:use :cl :cl-glfw3 :cl-opengl :uiop))

(in-package :internal-opengl)

(export '(
          ))

;; This is one of my java packages translated to lisp, might be sloppy!
(defconstant current-dir (uiop:getcwd))

(defstruct shader
  (name nil :type string)
  (program-id -1 :type integer)
  (uniforms (make-hash-table) :type hash-table))

(defvar *shaders* (make-hash-table))

; (defclass Shader direct-superclasses direct-slots)
; (error "~A does not exist" 'test)

(print *shaders*)

(setf (gethash 'cool *shaders*) 23)

