(defpackage #:internal-opengl
  (:nicknames :igl)
  (:use :cl :cl-glfw3 :cl-opengl :str))

(in-package :internal-opengl)

(export '(game-new-shader
          game-use-shader
          game-get-shader
          game-delete-shader
          game-has-shader
          game-new-shader-uniform))

;; This is one of my java packages translated to lisp, might be sloppy!

(defstruct shader
  (name nil :type string)
  (program-id -1 :type integer)
  (uniforms (make-hash-table :test 'equal) :type hash-table))

;; This is so make-shader still exists as longhand.
(defun game-make-shader (name program-id)
  "Optional constructor bolt on function for shaders."
  (make-shader :name name :program-id program-id))

;; Holds all the shaders
(defvar *shaders* (make-hash-table :test 'equal))

(defun game-get-shader (shader-name)
  (gethash shader-name *shaders*))

(defun game-get-shader-program-id (shader-name)
  (shader-program-id (game-get-shader shader-name)))

(defun game-get-shader-uniforms (shader-name)
  "Returns the hash table of the shader's uniforms"
  (shader-uniforms (game-get-shader shader-name)))

(defun game-new-shader-uniform (shader-name uniform-name)
  (let* ((program-id (game-get-shader-program-id shader-name))
         (uniform-location (gl:get-uniform-location program-id uniform-name)))
    (if (< uniform-location 0)
        (error (format t "ERROR! Shader (~a) uniform (~a) does not exist! Got (~a)!~%" shader-name uniform-name uniform-location))
        (format t "Shader (~a) uniform (~a) at location (~a)~%" shader-name uniform-name uniform-location))
    (setf (gethash uniform-name (game-get-shader-uniforms shader-name)) uniform-location)))

(defun game-get-shader-uniform (shader-name uniform-name)
  "Returns the uniform's location"
  (gethash uniform-name (game-get-shader-uniforms shader-name)))

(defun game-delete-shader (shader-name)
  (remhash shader-name *shaders*))

(defun game-has-shader (shader-name)
  (if (game-get-shader shader-name)
      t
      nil))

;; (defclass Shader direct-superclasses direct-slots)
;; (error "~A does not exist" 'test)

;; (print *shaders*)

;; (setf (gethash 'cool *shaders*) 23)


;; (loop for i from 1 to 10 do
;;       (format t "hi ~a~%" i))


;; That was surprisingly easy
;; (print
;;  (str:from-file
;;   (truename "shaders/frag.frag")))

;; GL shader
;;note: for some reason this does not works
;; (print gl:fragment-shader)

;;note: So this is how we get access to frag shader type 
;; (print 'fragment-shader)

;; A helper function to turn the shader file location into a string
(defun shader-location-to-string (location)
  (str:from-file (truename location)))

;; So this is the constructor function for creating a new shader
(defun game-new-shader (shader-name vert-source-code-location frag-source-code-location)
  ;; Automatically refresh the shader in the current OpenGL context for REPL.
  ;; Because: Shader is now stale, the OpenGL context is a different pointer.
  (if (game-has-shader shader-name)
      (progn
        (format t "WARNING! Overwriting shader (~a)!~%" shader-name)
        (game-delete-shader shader-name)))
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
    (setf (gethash shader-name *shaders*) (game-make-shader shader-name program-id))
    (format t "New shader (~a) created!~%" shader-name)))

;; (defun game-create-uniform (shader-name uniform-name)
;;   (let* ((program-id (game-get-shader-program-id shader-name))
;;          (location (gl:get-uniform-location )))))


;; (format t "~a~%" (gethash "main" *shaders*))

;; (if (gethash "main" *shaders*)
;;     "Key exists"
;;     "Key does not exist")

;; (defun print-hash-entry (key value)
;;     (format t "The value associated with the key ~S is ~S~%" key value))

;; (maphash #'print-hash-entry *shaders*)

(defun game-use-shader (shader-name)
  (if (game-has-shader shader-name)
      (progn
        (format t "Using shader (~a)~%" shader-name)
        (let ((shader-struct (game-get-shader shader-name)))
          (format t "~a~%" shader-struct)
          (gl:use-program (shader-program-id shader-struct))))
      (format t "ERROR: Tried to use non-existent shader! (~a) does not exist!" shader-name)))
