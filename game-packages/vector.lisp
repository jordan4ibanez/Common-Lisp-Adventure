(defpackage #:vector
  (:nicknames :vec)
  (:use :cl :constants))

(in-package :vector)

(export '(
          vec2
          vec3
          vec4
          new-vec
          print-vec
          make-vec2
          make-vec3
          make-vec4
          vec-get-x
          vec-get-y
          vec-get-z
          vec-get-w))

;; Base structures. Data containers, do not need OOP flexibility.
(defstruct vec2
  (x 0.0 :type float)
  (y 0.0 :type float))

(defstruct vec3
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(defstruct vec4
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float)
  (w 0.0 :type float))

;; Constructor with auto dispatch. Just dumps integers into floating point.
(defun new-vec(x y &optional z w)
  (cond ((not (null w)) (make-vec4 :x (float x) :y (float y) :z (float z) :w (float w)))
        ((not (null z)) (make-vec3 :x (float x) :y (float y) :z (float z)))
        (true (make-vec2 :x (float x) :y (float y)))))

;; Functional slot access.
; (let ((test-vector (make-vec2 :x 0.0 :y 0.0)))
;   (print (format false "vec2(~a,~a)" (vec2-x test-vector) (vec2-y test-vector))))

;; OOP slot access methods.

;; Vector printer.
(defgeneric print-vec(vec)
  (:documentation "Prints out a vector."))

(defmethod print-vec((vec vec2))
  (format true "vec2(~a, ~a)" (vec2-x vec) (vec2-y vec)))

(defmethod print-vec((vec vec3))
  (format true "vec3(~a, ~a, ~a)" (vec3-x vec) (vec3-y vec) (vec3-z vec)))

(defmethod print-vec((vec vec4))
  (format true "vec4(~a, ~a, ~a, ~a)" (vec4-x vec) (vec4-y vec) (vec4-z vec) (vec4-w vec)))

;; Get X.
(defgeneric vec-get-x(vec)
  (:documentation "Get the X component of a vector-2,3,4."))

(defmethod vec-get-x((vec vec2))
  (vec2-x vec))

(defmethod vec-get-x((vec vec3))
  (vec3-x vec))

(defmethod vec-get-x((vec vec4))
  (vec4-x vec))

;; Get Y.
(defgeneric vec-get-y(vec)
  (:documentation "Get the Y component of a vector-2,3,4."))

(defmethod vec-get-y((vec vec2))
  (vec2-y vec))

(defmethod vec-get-y((vec vec3))
  (vec3-y vec))

(defmethod vec-get-y((vec vec4))
  (vec4-y vec))

;; Get Z.
(defgeneric vec-get-z(vec)
  (:documentation "Get the Z component of a vector-3,4."))

(defmethod vec-get-z((vec vec3))
  (vec3-z vec))

(defmethod vec-get-z((vec vec4))
  (vec4-z vec))

;; Get W.
(defgeneric vec-get-w(vec)
  (:documentation "Get the W component of a Vector-4."))

(defmethod vec-get-w((vec vec4))
  (vec4-w vec))
