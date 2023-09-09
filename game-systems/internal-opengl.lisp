(defpackage #:internal-opengl
  (:nicknames :igl)
  (:use :cl :cl-glfw3 :cl-opengl :str))

(in-package :internal-opengl)

(export '(new-shader))

;; This is one of my java packages translated to lisp, might be sloppy!

(defstruct shader
  (name nil :type string)
  (program-id -1 :type integer)
  (uniforms (make-hash-table) :type hash-table))

;; This is so make-shader still exists as longhand.
(defun game-make-shader (name program-id)
  "Optional constructor bolt on function for shaders."
  (make-shader :name name :program-id program-id))

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

;; A helper function to turn the shader file location into a string
(defun shader-location-to-string (location)
  (str:from-file (truename location)))

;; So this is the constructor function for creating a new shader
(defun new-shader (shader-name vert-source-code-location frag-source-code-location)
  (let ((vert
          (gl:create-shader :vertex-shader))
        (vert-code
          (shader-location-to-string vert-source-code-location))
        (frag
          (gl:create-shader :fragment-shader))
        (frag-code
          (shader-location-to-string frag-source-code-location))
        (program-id 0))
    ;; Assign shader source components.
    (gl:shader-source vert vert-code)
    (gl:shader-source frag frag-code)
    ;; Compile shader source.
    (gl:compile-shader vert)
    (gl:compile-shader frag)
    ;; Now bring our actual program into existence
    (setf program-id (gl:create-program))
    ;; Now attach the components.
    (gl:attach-shader program-id vert)
    (gl:attach-shader program-id frag)
    ;; Now link the program
    (gl:link-program program-id)
    ;; And if we didn't get an error, create an object from the shader and store it for further use!
    (setf (gethash shader-name *shaders*)
          ;; (make-instance 'shader :name shader-name :program-id program-id)
          (game-make-shader shader-name program-id)
          )))

;; FIXME: need to enable GLFW!
