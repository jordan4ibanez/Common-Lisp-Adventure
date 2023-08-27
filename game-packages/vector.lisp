(defpackage #:vector
  (:nicknames :vec)
  (:use :cl))

(in-package :vector)

(export '(
          vec2
          vec3
          vec4
          new-vec
          new-vec-from-list
          to-list
          print-vec
          get-components
          vec-type-component-amount
          get-x
          get-y
          get-z
          get-w
          set-x
          set-y
          set-z
          set-w
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
        (t (make-vec2 :x (float x) :y (float y)))))

;; Constructor with auto dispatch for lists. Just dumps integers into floating point.
(defun new-vec-from-list(input-list)
 (cond ((= (length input-list) 2) (make-vec2 :x (float (nth 0 input-list)) :y (float (nth 1 input-list))))
       ((= (length input-list) 3) (make-vec3 :x (float (nth 0 input-list)) :y (float (nth 1 input-list)) :z (float (nth 2 input-list))))
       ((= (length input-list) 4) (make-vec4 :x (float (nth 0 input-list)) :y (float (nth 1 input-list)) :z (float (nth 2 input-list)) :w (float (nth 3 input-list))))))

;; Functional slot access.
; (let ((test-vector (make-vec2 :x 0.0 :y 0.0)))
;   (print (format nil "vec2(~a,~a)" (vec2-x test-vector) (vec2-y test-vector))))

;; OOP slot access methods.

;; Vector printer.
(defgeneric print-vec(vec)
  (:documentation "Prints out a vector."))

(defmethod print-vec((vec vec2))
  (format t "vec2(~a, ~a)" (vec2-x vec) (vec2-y vec)))

(defmethod print-vec((vec vec3))
  (format t "vec3(~a, ~a, ~a)" (vec3-x vec) (vec3-y vec) (vec3-z vec)))

(defmethod print-vec((vec vec4))
  (format t "vec4(~a, ~a, ~a, ~a)" (vec4-x vec) (vec4-y vec) (vec4-z vec) (vec4-w vec)))

;; Allows initializing raw generics from code.
(defmacro init-generic (fun-name) `(progn (defgeneric ,fun-name(vec operator))))

;; Get number of components in the vector.
(defmacro init-get-components(vec-type return-val) `(progn (defmethod get-components((vec ,vec-type)) ,return-val)))
(defgeneric get-components(vec))
(loop for vec-type in '(vec2 vec3 vec4) for return-val in '(2 3 4) do (eval `(init-get-components ,vec-type ,return-val)))

;; A very specific function to help with macros. Maybe?
;; Pass it 'vec2 'vec3 or 'vec4 and you get 2 3 or 4
(defun vec-type-component-amount(vec-type)
  (cond ((eql vec-type 'vec2) 2)
        ((eql vec-type 'vec3) 3)
        ((eql vec-type 'vec4) 4)
        (t 0)))

;; Now this is just absurd.
;; Combo runner for setters & getters, automatically inferred.
;; TODO -- This is a reference
; (defmacro getter-setter()
;   (cons 'progn (loop for axis in '(x y z w) for count in '(1 2 3 4) collect
;           ; (format t "~a~%" fun-name)g
;           (let ((fun-name-get (read-from-string (format nil "get-~a" axis)))
;                 (fun-name-set (read-from-string (format nil "set-~a" axis))))
;             ;; Set generics.
;             `(defgeneric ,fun-name-get(vec))
;             `(defgeneric ,fun-name-set(vec new-value))
;             (loop for vec-type in '(vec2 vec3 vec4) collect
;                     (if (<= count (vec-type-component-amount vec-type))
;                         (let ((slot-call (read-from-string (format nil "~a-~a" vec-type axis))))
;                             ; (format t "(~a, ~a, ~a, ~a, ~a)" count fun-name-set vec-type axis slot-call)
;                             ;; Getter for xyzw
;                             `(defmethod ,fun-name-get((vec ,vec-type)) 
;                                   (,slot-call vec))
;                             ;; Setter for xyzw
;                             ;; Floating point new value.
;                             `(defmethod ,fun-name-set((vec ,vec-type)(new-value float))
;                                   (setf (,slot-call vec) new-value))
;                             `(defmethod ,fun-name-set((vec ,vec-type)(new-value integer))
;                                   (setf (,slot-call vec) (float new-value))))))))))
;             ; (format t "~%")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro compile-thing()
    `(progn
        (defun blah() (print "blah")))))

;; To list.
;; TODO: THIS IS CAUSING AN ERROR!!!
;; TODO: This error is from the getters and setters not loading into memory
; (defgeneric to-list(vec))

; (defmethod to-list((vec vec2))
;   (list (get-x vec) (get-y vec)))

; (defmethod to-list((vec vec3))
;   (list (get-x vec) (get-y vec) (get-z vec)))

; (defmethod to-list((vec vec4))
;   (list (get-x vec) (get-y vec) (get-z vec) (get-w vec)))

;; TODO: END ERROR!!!

; ;; NOTE: This is where macros begin in this file.

; ;; Remove a bunch of boilerplate functions.
; (defmacro boilerplate-vec-operations (fun-name operation vector-type)
;     `(progn
;         (defmethod ,fun-name((vec integer) (operator integer)) (print "hi"))
;         (defmethod ,fun-name((vec ,vector-type) (operator ,vector-type))
;           (new-vec-from-list (loop for x in (to-list vec) for y in (to-list operator) collect (,operation x y))))
;         (defmethod ,fun-name((vec ,vector-type) (operator float))
;           (new-vec-from-list (loop for x in (to-list vec) collect (,operation x operator))))
;         (defmethod ,fun-name((vec ,vector-type) (operator integer))
;           (new-vec-from-list (loop for x in (to-list vec) collect (,operation x (float operator)))))))

; ;; Note: This has been reduces to simplified types because this file might
; ;; end up a few ten thousand lines long if I don't hold back.

; ;; This is an unholy procedure
; ; (loop for fun-name in '(mul add div sub) for operation in '(* + / -) do
; ;         (eval `(init-generic ,fun-name))
; ;         (loop for vector-type in '(vec2 vec3 vec4) do
; ;             (eval `(boilerplate-vec-operations ,fun-name ,operation ,vector-type))))

; ; ;; Invert (Vec * -1). Useful for random things. Wordy alternative to (mul vec -1)

; ; (defgeneric invert(vector))

; ; (defmethod invert((vector1 vec2))
; ;  (mul vector1 -1))

; ; (defmethod invert((vector1 vec3))
; ;  (mul vector1 -1))

; ; (defmethod invert((vector1 vec4))
; ;  (mul vector1 -1))


; ; (defvar my-var (new-vec 0 0))