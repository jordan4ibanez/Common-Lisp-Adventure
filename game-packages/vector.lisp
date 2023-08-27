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
          new-list-from-vec
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
          invert
          cool))

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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun new-vec (x y &optional z w)
    (cond ((not (null w)) (make-vec4 :x (float x) :y (float y) :z (float z) :w (float w)))
          ((not (null z)) (make-vec3 :x (float x) :y (float y) :z (float z)))
          (t (make-vec2 :x (float x) :y (float y)))))

  ;; Constructor with auto dispatch for lists. Just dumps integers into floating point.
  (defun new-vec-from-list (input-list)
    (cond ((= (length input-list) 2) (make-vec2 :x (float (nth 0 input-list)) :y (float (nth 1 input-list))))
          ((= (length input-list) 3) (make-vec3 :x (float (nth 0 input-list)) :y (float (nth 1 input-list)) :z (float (nth 2 input-list))))
          ((= (length input-list) 4) (make-vec4 :x (float (nth 0 input-list)) :y (float (nth 1 input-list)) :z (float (nth 2 input-list)) :w (float (nth 3 input-list))))))

  ;; Creates a list from a vector.
  (defun new-list-from-vec (vec)
    (cond ((eql (type-of vec) 'vec2) (list (get-x vec) (get-y vec)))
          ((eql (type-of vec) 'vec3) (list (get-x vec) (get-y vec) (get-z vec)))
          ((eql (type-of vec) 'vec4) (list (get-x vec) (get-y vec) (get-z vec) (get-w vec)))))

  ;; A very specific function to help with macros. Maybe?
  ;; Pass it 'vec2 'vec3 or 'vec4 and you get 2 3 or 4
  (defun vec-type-component-amount (vec-type)
    (cond ((eql vec-type 'vec2) 2)
          ((eql vec-type 'vec3) 3)
          ((eql vec-type 'vec4) 4)
          (t 0))))


;; OOP slot access methods.

;; Vector printer.
(defgeneric print-vec (vec)
  (:documentation "Prints out a vector."))

(defmethod print-vec ((vec vec2))
  (format t "vec2(~a, ~a)" (vec2-x vec) (vec2-y vec)))

(defmethod print-vec ((vec vec3))
  (format t "vec3(~a, ~a, ~a)" (vec3-x vec) (vec3-y vec) (vec3-z vec)))

(defmethod print-vec ((vec vec4))
  (format t "vec4(~a, ~a, ~a, ~a)" (vec4-x vec) (vec4-y vec) (vec4-z vec) (vec4-w vec)))

;; Allows initializing raw generics from code.
(defmacro init-generic (fun-name) `(progn (defgeneric ,fun-name(vec operator))))

;; Get number of components in the vector.
(defgeneric get-components (vec))
(defmacro vector-sizes ()
  (cons 'progn (loop for vec-type in '(vec2 vec3 vec4) for return-val in '(2 3 4) :collect
       `(defmethod get-components ((vec ,vec-type)) ,return-val))))
      ;  `(init-get-components ,vec-type ,return-val)))
(vector-sizes)

;; Now this is just absurd.
;; Combo runner for setters & getters, automatically inferred based on vector width.
(defmacro getters-and-setters()
  (cons 'progn
        (loop for axis in '(x y z w) for count in '(1 2 3 4) nconc ;; nconc is a flat list for future reference.
                (let ((fun-name-get (intern (string-upcase (format nil "get-~a" axis))))
                      (fun-name-set (intern (string-upcase (format nil "set-~a" axis)))))
                  `(defgeneric ,fun-name-get (vec))
                  `(defgeneric ,fun-name-set (vec new-value))
                  (loop for vec-type in '(vec2 vec3 vec4) nconc
                          (let ((slot-call (intern (string-upcase (format nil "~a-~a" vec-type axis)))))
                            (if (<= count (vec-type-component-amount vec-type))
                                (progn `(
                                         (defmethod ,fun-name-get ((vec ,vec-type)) (,slot-call vec))
                                         (defmethod ,fun-name-set ((vec ,vec-type)(new-value float))
                                           (setf (,slot-call vec) new-value))
                                         (defmethod ,fun-name-set ((vec ,vec-type)(new-value integer))
                                           (setf (,slot-call vec) (float new-value))))))))))))
(getters-and-setters)

; (defmacro operations()
;   (cons 'progn
;         (loop for operation in '(+ - / *) for fun-name in '(add sub div mul) :nconc
;           `(defun cool() (print "bye")))))
; (operations)

; (defun test-123() (print "hi"))

; (defgeneric inv (vec)
;   (:documentation "Inverts a vector.")
;   (:method ((vec vec2)) (mul vec -1))
;   (:method ((vec vec2)) (print "hi"))
;   (:method ((vec vec2)) (print "hi")))
