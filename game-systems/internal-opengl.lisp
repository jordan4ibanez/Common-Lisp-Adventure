(defpackage #:internal-opengl
  (:nicknames :igl)
  (:use :cl :cl-glfw3 :cl-opengl :str))

(in-package :internal-opengl)

(export '())

;; This is one of my java packages translated to lisp, might be sloppy!

(defstruct shader
  (name nil :type string)
  (program-id -1 :type integer)
  (uniforms (make-hash-table) :type hash-table))

(defvar *shaders* (make-hash-table))

;; (defclass Shader direct-superclasses direct-slots)
;; (error "~A does not exist" 'test)

;; (print *shaders*)

;; (setf (gethash 'cool *shaders*) 23)


;; (loop for i from 1 to 10 do
;;       (format t "hi ~a~%" i))


;; That was surprisingly easy
(print
 (str:from-file
  (truename "shaders/frag.frag")))

;; GL shader
;;note: for some reason this does not works
;; (print gl:fragment-shader)

;;note: So this is how we get access to frag shader type 
;; (print 'fragment-shader)
