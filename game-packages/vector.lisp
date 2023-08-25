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
          get-x
          get-y
          get-z
          get-w
          add
          sub
          div
          ))

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
(defgeneric get-x(vec)
  (:documentation "Get the X component of a vector-2,3,4."))

(defmethod get-x((vec vec2))
  (vec2-x vec))

(defmethod get-x((vec vec3))
  (vec3-x vec))

(defmethod get-x((vec vec4))
  (vec4-x vec))

;; Get Y.
(defgeneric get-y(vec)
  (:documentation "Get the Y component of a vector-2,3,4."))

(defmethod get-y((vec vec2))
  (vec2-y vec))

(defmethod get-y((vec vec3))
  (vec3-y vec))

(defmethod get-y((vec vec4))
  (vec4-y vec))

;; Get Z.
(defgeneric get-z(vec)
  (:documentation "Get the Z component of a vector-3,4."))

(defmethod get-z((vec vec3))
  (vec3-z vec))

(defmethod get-z((vec vec4))
  (vec4-z vec))

;; Get W.
(defgeneric get-w(vec)
  (:documentation "Get the W component of a Vector-4."))

(defmethod get-w((vec vec4))
  (vec4-w vec))

;; Note: This has been reduces to simplified types because this file might
;; end up a few ten thousand lines long if I don't hold back.

;; Add.
(defgeneric add(vector1 addend))

;; Vec2 & Vec2.
(defmethod add((vector1 vec2) (addend vec2))
  (new-vec
   (+ (get-x vector1) (get-x addend))
   (+ (get-y vector1) (get-y addend))))

;; Vec2 & Floating Scalar.
(defmethod add((vector1 vec2) (addend float))
  (new-vec
   (+ (get-x vector1) addend)
   (+ (get-y vector1) addend)))

;; Vec2 & Integral Scalar.
(defmethod add((vector1 vec2) (addend integer))
  (new-vec
   (+ (get-x vector1) (float addend))
   (+ (get-y vector1) (float addend))))

;; Vec3 & Vec3.
(defmethod add((vector1 vec3) (addend vec3))
 (new-vec
  (+ (get-x vector1) (get-x addend))
  (+ (get-y vector1) (get-y addend))
  (+ (get-z vector1) (get-z addend))))

;; Vec3 & Floating Scalar
(defmethod add((vector1 vec3) (addend float))
 (new-vec
  (+ (get-x vector1) addend)
  (+ (get-y vector1) addend)
  (+ (get-z vector1) addend)))

;; Vec3 & Integral Scalar.
(defmethod add((vector1 vec3) (addend integer))
 (new-vec
  (+ (get-x vector1) (float addend))
  (+ (get-y vector1) (float addend))
  (+ (get-z vector1) (float addend))))

;; Vec4 & Vec4.
(defmethod add((vector1 vec4) (addend vec4))
 (new-vec
  (+ (get-x vector1) (get-x addend))
  (+ (get-y vector1) (get-y addend))
  (+ (get-z vector1) (get-z addend))
  (+ (get-w vector1) (get-w addend))))

;; Vec4 & Floating Scalar.
(defmethod add((vector1 vec4) (addend float))
 (new-vec
  (+ (get-x vector1) addend)
  (+ (get-y vector1) addend)
  (+ (get-z vector1) addend)
  (+ (get-w vector1) addend)))

;; Vec4 & Integral Scalar.
(defmethod add((vector1 vec4) (addend integer))
 (new-vec
  (+ (get-x vector1) (float addend))
  (+ (get-y vector1) (float addend))
  (+ (get-z vector1) (float addend))
  (+ (get-w vector1) (float addend))))


;; Subtract.
(defgeneric sub(vector1 addend))

;; Vec2 & Vec2.
(defmethod sub((vector1 vec2) (addend vec2))
  (new-vec
   (- (get-x vector1) (get-x addend))
   (- (get-y vector1) (get-y addend))))

;; Vec2 & Floating Scalar.
(defmethod sub((vector1 vec2) (addend float))
  (new-vec
   (- (get-x vector1) addend)
   (- (get-y vector1) addend)))

;; Vec2 & Integral Scalar.
(defmethod sub((vector1 vec2) (addend integer))
  (new-vec
   (- (get-x vector1) (float addend))
   (- (get-y vector1) (float addend))))

;; Vec3 & Vec3.
(defmethod sub((vector1 vec3) (addend vec3))
  (new-vec
   (- (get-x vector1) (get-x addend))
   (- (get-y vector1) (get-y addend))
   (- (get-z vector1) (get-z addend))))

;; Vec3 & Floating Scalar
(defmethod sub((vector1 vec3) (addend float))
  (new-vec
   (- (get-x vector1) addend)
   (- (get-y vector1) addend)
   (- (get-z vector1) addend)))

;; Vec3 & Integral Scalar.
(defmethod sub((vector1 vec3) (addend integer))
  (new-vec
   (- (get-x vector1) (float addend))
   (- (get-y vector1) (float addend))
   (- (get-z vector1) (float addend))))

;; Vec4 & Vec4.
(defmethod sub((vector1 vec4) (addend vec4))
  (new-vec
   (- (get-x vector1) (get-x addend))
   (- (get-y vector1) (get-y addend))
   (- (get-z vector1) (get-z addend))
   (- (get-w vector1) (get-w addend))))

;; Vec4 & Floating Scalar.
(defmethod sub((vector1 vec4) (addend float))
  (new-vec
   (- (get-x vector1) addend)
   (- (get-y vector1) addend)
   (- (get-z vector1) addend)
   (- (get-w vector1) addend)))

;; Vec4 & Integral Scalar.
(defmethod sub((vector1 vec4) (addend integer))
  (new-vec
   (- (get-x vector1) (float addend))
   (- (get-y vector1) (float addend))
   (- (get-z vector1) (float addend))
   (- (get-w vector1) (float addend))))


 ;; Multiply.
 (defgeneric div(vector1 addend))

 ;; Vec2 & Vec2.
 (defmethod div((vector1 vec2) (addend vec2))
   (new-vec
    (/ (get-x vector1) (get-x addend))
    (/ (get-y vector1) (get-y addend))))

 ;; Vec2 & Floating Scalar.
 (defmethod div((vector1 vec2) (addend float))
   (new-vec
    (/ (get-x vector1) addend)
    (/ (get-y vector1) addend)))

 ;; Vec2 & Integral Scalar.
 (defmethod div((vector1 vec2) (addend integer))
   (new-vec
    (/ (get-x vector1) (float addend))
    (/ (get-y vector1) (float addend))))

 ;; Vec3 & Vec3.
 (defmethod div((vector1 vec3) (addend vec3))
   (new-vec
    (/ (get-x vector1) (get-x addend))
    (/ (get-y vector1) (get-y addend))
    (/ (get-z vector1) (get-z addend))))

 ;; Vec3 & Floating Scalar
 (defmethod div((vector1 vec3) (addend float))
   (new-vec
    (/ (get-x vector1) addend)
    (/ (get-y vector1) addend)
    (/ (get-z vector1) addend)))

 ;; Vec3 & Integral Scalar.
 (defmethod div((vector1 vec3) (addend integer))
   (new-vec
    (/ (get-x vector1) (float addend))
    (/ (get-y vector1) (float addend))
    (/ (get-z vector1) (float addend))))

 ;; Vec4 & Vec4.
 (defmethod div((vector1 vec4) (addend vec4))
   (new-vec
    (/ (get-x vector1) (get-x addend))
    (/ (get-y vector1) (get-y addend))
    (/ (get-z vector1) (get-z addend))
    (/ (get-w vector1) (get-w addend))))

 ;; Vec4 & Floating Scalar.
 (defmethod div((vector1 vec4) (addend float))
   (new-vec
    (/ (get-x vector1) addend)
    (/ (get-y vector1) addend)
    (/ (get-z vector1) addend)
    (/ (get-w vector1) addend)))

 ;; Vec4 & Integral Scalar.
 (defmethod div((vector1 vec4) (addend integer))
   (new-vec
    (/ (get-x vector1) (float addend))
    (/ (get-y vector1) (float addend))
    (/ (get-z vector1) (float addend))
    (/ (get-w vector1) (float addend))))
