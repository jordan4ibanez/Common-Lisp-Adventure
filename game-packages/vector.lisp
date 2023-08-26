(defpackage #:vector
  (:nicknames :vec)
  (:use :cl :constants))

(in-package :vector)

(export '(
          vec2
          vec3
          vec4
          new-vec
          new-vec-from-list
          to-list
          print-vec
          get-x
          get-y
          get-z
          get-w
          add
          sub
          div
          mul
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

;; Constructor with auto dispatch for lists. Just dumps integers into floating point.
(defun new-vec-from-list(input-list)
 (cond ((= (length input-list) 2) (make-vec2 :x (float (nth 0 input-list)) :y (float (nth 1 input-list))))
       ((= (length input-list) 3) (make-vec3 :x (float (nth 0 input-list)) :y (float (nth 1 input-list)) :z (float (nth 2 input-list))))
       ((= (length input-list) 4) (make-vec4 :x (float (nth 0 input-list)) :y (float (nth 1 input-list)) :z (float (nth 2 input-list)) :w (float (nth 3 input-list))))))

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

;; TODO: IMPLEMENT THIS HOLY MOLY
; From famicon guy in discord:
; `(progn
;    (defmethod ...)
;    (defmethod …)
;    (defmethod …))

;; Remove a bunch of boilerplate functions.
(defmacro init-generic (fun-name)
  `(progn
    (defgeneric ,fun-name(vector1 operator))))

(defmacro boilerplate (fun-name operation vector-type)
    `(progn
        (defmethod ,fun-name((vector1 integer) (operator integer)) (print "hi"))
        (defmethod ,fun-name((vector1 ,vector-type) (operator ,vector-type))
          (new-vec-from-list (loop for x in (to-list vector1) for y in (to-list operator) collect (,operation x y))))
        (defmethod ,fun-name((vector1 ,vector-type) (operator float))
          (new-vec-from-list (loop for x in (to-list vector1) collect (,operation x operator))))
        (defmethod ,fun-name((vector1 ,vector-type) (operator integer))
          (new-vec-from-list (loop for x in (to-list vector1) collect (,operation x (float operator)))))))

;; Note: This has been reduces to simplified types because this file might
;; end up a few ten thousand lines long if I don't hold back.

;; TODO: get this to work
;; ;'vec3 'vec4))); (operation-list (list '* '+ '- '/)))
; (eval-when (:compile-toplevel :load-toplevel :execute)
; (eval-when (:compile-toplevel :load-toplevel :execute)
; (loop for thing in (list 'vec3) do
;                 (eval (macroexpand`(boilerplate add + ,thing))))

;; This is an unholy function
(loop for x in '(1 2 3) do
        (format t "~%---------~%")
        (loop for y in '(4 5 6) do
                (format t "(~a,~a)" x y)))

(loop for fun-name in '(mul add div sub) for operation in '(* + / -) do
        (eval (macroexpand `(init-generic ,fun-name)))
        (loop for vec-type in '(vec2 vec3 vec4) do
             (eval (macroexpand `(boilerplate ,fun-name ,operation ,vec-type)))))

;; Invert (Vec * -1). Useful for random things. Wordy alternative to (mul vec -1)
; (defgeneric invert(vector))

; (defmethod invert((vector1 vec2))
;  (mul vector1 -1))

; (defmethod invert((vector1 vec3))
;  (mul vector1 -1))

; (defmethod invert((vector1 vec4))
;  (mul vector1 -1))