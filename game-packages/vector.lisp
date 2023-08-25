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
          invert))

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

;; To list.
(defgeneric to-list(vec))

(defmethod to-list((vec vec2))
  (list (get-x vec) (get-y vec)))

(defmethod to-list((vec vec3))
  (list (get-x vec) (get-y vec) (get-z vec)))

(defmethod to-list((vec vec4))
  (list (get-x vec) (get-y vec) (get-z vec) (get-w vec)))
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
(defgeneric sub(vector1 subtrahend))

;; Vec2 & Vec2.
(defmethod sub((vector1 vec2) (subtrahend vec2))
  (new-vec
   (- (get-x vector1) (get-x subtrahend))
   (- (get-y vector1) (get-y subtrahend))))

;; Vec2 & Floating Scalar.
(defmethod sub((vector1 vec2) (subtrahend float))
  (new-vec
   (- (get-x vector1) subtrahend)
   (- (get-y vector1) subtrahend)))

;; Vec2 & Integral Scalar.
(defmethod sub((vector1 vec2) (subtrahend integer))
  (new-vec
   (- (get-x vector1) (float subtrahend))
   (- (get-y vector1) (float subtrahend))))

;; Vec3 & Vec3.
(defmethod sub((vector1 vec3) (subtrahend vec3))
  (new-vec
   (- (get-x vector1) (get-x subtrahend))
   (- (get-y vector1) (get-y subtrahend))
   (- (get-z vector1) (get-z subtrahend))))

;; Vec3 & Floating Scalar
(defmethod sub((vector1 vec3) (subtrahend float))
  (new-vec
   (- (get-x vector1) subtrahend)
   (- (get-y vector1) subtrahend)
   (- (get-z vector1) subtrahend)))

;; Vec3 & Integral Scalar.
(defmethod sub((vector1 vec3) (subtrahend integer))
  (new-vec
   (- (get-x vector1) (float subtrahend))
   (- (get-y vector1) (float subtrahend))
   (- (get-z vector1) (float subtrahend))))

;; Vec4 & Vec4.
(defmethod sub((vector1 vec4) (subtrahend vec4))
  (new-vec
   (- (get-x vector1) (get-x subtrahend))
   (- (get-y vector1) (get-y subtrahend))
   (- (get-z vector1) (get-z subtrahend))
   (- (get-w vector1) (get-w subtrahend))))

;; Vec4 & Floating Scalar.
(defmethod sub((vector1 vec4) (subtrahend float))
  (new-vec
   (- (get-x vector1) subtrahend)
   (- (get-y vector1) subtrahend)
   (- (get-z vector1) subtrahend)
   (- (get-w vector1) subtrahend)))

;; Vec4 & Integral Scalar.
(defmethod sub((vector1 vec4) (subtrahend integer))
  (new-vec
   (- (get-x vector1) (float subtrahend))
   (- (get-y vector1) (float subtrahend))
   (- (get-z vector1) (float subtrahend))
   (- (get-w vector1) (float subtrahend))))


 ;; Divide.
(defgeneric div(vector1 divisor))

 ;; Vec2 & Vec2.
(defmethod div((vector1 vec2) (divisor vec2))
  (new-vec
   (/ (get-x vector1) (get-x divisor))
   (/ (get-y vector1) (get-y divisor))))

 ;; Vec2 & Floating Scalar.
(defmethod div((vector1 vec2) (divisor float))
  (new-vec
   (/ (get-x vector1) divisor)
   (/ (get-y vector1) divisor)))

 ;; Vec2 & Integral Scalar.
(defmethod div((vector1 vec2) (divisor integer))
  (new-vec
   (/ (get-x vector1) (float divisor))
   (/ (get-y vector1) (float divisor))))

 ;; Vec3 & Vec3.
(defmethod div((vector1 vec3) (divisor vec3))
  (new-vec
   (/ (get-x vector1) (get-x divisor))
   (/ (get-y vector1) (get-y divisor))
   (/ (get-z vector1) (get-z divisor))))

 ;; Vec3 & Floating Scalar
(defmethod div((vector1 vec3) (divisor float))
  (new-vec
   (/ (get-x vector1) divisor)
   (/ (get-y vector1) divisor)
   (/ (get-z vector1) divisor)))

 ;; Vec3 & Integral Scalar.
(defmethod div((vector1 vec3) (divisor integer))
  (new-vec
   (/ (get-x vector1) (float divisor))
   (/ (get-y vector1) (float divisor))
   (/ (get-z vector1) (float divisor))))

 ;; Vec4 & Vec4.
(defmethod div((vector1 vec4) (divisor vec4))
  (new-vec
   (/ (get-x vector1) (get-x divisor))
   (/ (get-y vector1) (get-y divisor))
   (/ (get-z vector1) (get-z divisor))
   (/ (get-w vector1) (get-w divisor))))

 ;; Vec4 & Floating Scalar.
(defmethod div((vector1 vec4) (divisor float))
  (new-vec
   (/ (get-x vector1) divisor)
   (/ (get-y vector1) divisor)
   (/ (get-z vector1) divisor)
   (/ (get-w vector1) divisor)))

 ;; Vec4 & Integral Scalar.
(defmethod div((vector1 vec4) (divisor integer))
  (new-vec
   (/ (get-x vector1) (float divisor))
   (/ (get-y vector1) (float divisor))
   (/ (get-z vector1) (float divisor))
   (/ (get-w vector1) (float divisor))))


;; Multiply.
(defgeneric mul(vector1 multiplier))

;; Vec2 & Vec2.
(defmethod mul((vector1 vec2) (multiplier vec2))
  (new-vec
   (* (get-x vector1) (get-x multiplier))
   (* (get-y vector1) (get-y multiplier))))

;; Vec2 & Floating Scalar.
(defmethod mul((vector1 vec2) (multiplier float))
  (new-vec
   (* (get-x vector1) multiplier)
   (* (get-y vector1) multiplier)))

;; Vec2 & Integral Scalar.
(defmethod mul((vector1 vec2) (multiplier integer))
  (new-vec
   (* (get-x vector1) (float multiplier))
   (* (get-y vector1) (float multiplier))))

;; Vec3 & Vec3.
(defmethod mul((vector1 vec3) (multiplier vec3))
  (new-vec
   (* (get-x vector1) (get-x multiplier))
   (* (get-y vector1) (get-y multiplier))
   (* (get-z vector1) (get-z multiplier))))

;; Vec3 & Floating Scalar
(defmethod mul((vector1 vec3) (multiplier float))
  (new-vec
   (* (get-x vector1) multiplier)
   (* (get-y vector1) multiplier)
   (* (get-z vector1) multiplier)))

;; Vec3 & Integral Scalar.
(defmethod mul((vector1 vec3) (multiplier integer))
  (new-vec
   (* (get-x vector1) (float multiplier))
   (* (get-y vector1) (float multiplier))
   (* (get-z vector1) (float multiplier))))

;; Vec4 & Vec4.
(defmethod mul((vector1 vec4) (multiplier vec4))
  (new-vec
   (* (get-x vector1) (get-x multiplier))
   (* (get-y vector1) (get-y multiplier))
   (* (get-z vector1) (get-z multiplier))
   (* (get-w vector1) (get-w multiplier))))

;; Vec4 & Floating Scalar.
(defmethod mul((vector1 vec4) (multiplier float))
  (new-vec
   (* (get-x vector1) multiplier)
   (* (get-y vector1) multiplier)
   (* (get-z vector1) multiplier)
   (* (get-w vector1) multiplier)))

;; Vec4 & Integral Scalar.
(defmethod mul((vector1 vec4) (multiplier integer))
  (new-vec
   (* (get-x vector1) (float multiplier))
   (* (get-y vector1) (float multiplier))
   (* (get-z vector1) (float multiplier))
   (* (get-w vector1) (float multiplier))))

;; Invert (Vec * -1). Useful for random things. Wordy alternative to (mul vec -1)
(defgeneric invert(vector))

(defmethod invert((vector1 vec2))
 (mul vector1 -1))

(defmethod invert((vector1 vec3))
 (mul vector1 -1))

(defmethod invert((vector1 vec4))
 (mul vector1 -1))


 ;; MACROS ARE AMAZING AHHHHHHHHHHHHHHHHHHH
 ;; Thanks icantthinkofagoodname on discord!!!
; (defmacro boilerplate (fun-name operation)
;   `(defgeneric ,fun-name(vec))
;   `(defmethod ,fun-name((vec vec2)))
;
;   `(defmethod ,fun-name((vec vec3))
;      ()))

; (boilerplate testing *)
;
; (testing (new-vec 2 4))
