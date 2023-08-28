;; A quick note.
;; Anyone that says to write any of this out as a macro:
;;
;; This is easier to understand and maintain for myself.
;; I am doing this for fun. And macroing this does not feel fun at all.
;; If you would like to implement this as a macro, see "macro.lisp" in
;; the root directory. That should get you started.

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
          inv))

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
(defun new-vec (x y &optional z w)
  (cond ((not (null w)) (make-vec4 :x (float x) :y (float y) :z (float z) :w (float w)))
        ((not (null z)) (make-vec3 :x (float x) :y (float y) :z (float z)))
        (t              (make-vec2 :x (float x) :y (float y)))))

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

;; Pass it 'vec2 'vec3 or 'vec4 and you get 2 3 or 4
(defun vec-type-component-amount (vec-type)
  (cond ((eql vec-type 'vec2) 2)
        ((eql vec-type 'vec3) 3)
        ((eql vec-type 'vec4) 4)
        (t 0)))

;; Vector printer.
(defgeneric print-vec (vec)
  (:documentation "Prints out a vector.")
  (:method ((vec vec2)) (format t "vec2(~a, ~a)" (vec2-x vec) (vec2-y vec)))
  (:method ((vec vec3)) (format t "vec3(~a, ~a, ~a)" (vec3-x vec) (vec3-y vec) (vec3-z vec)))
  (:method ((vec vec4)) (format t "vec4(~a, ~a, ~a, ~a)" (vec4-x vec) (vec4-y vec) (vec4-z vec) (vec4-w vec))))

;; OOP getter methods.

(defgeneric get-x (vec)
  (:documentation "Get X component of vec2 vec3 vec4.")
  (:method ((vec vec2)) (vec2-x vec))
  (:method ((vec vec3)) (vec3-x vec))
  (:method ((vec vec4)) (vec4-x vec)))

(defgeneric get-y (vec)
  (:documentation "Get Y component of vec2 vec3 vec4.")
  (:method ((vec vec2)) (vec2-y vec))
  (:method ((vec vec3)) (vec3-y vec))
  (:method ((vec vec4)) (vec4-y vec)))

(defgeneric get-z (vec)
  (:documentation "Get Z component of vec3 vec4.")
  (:method ((vec vec3)) (vec3-z vec))
  (:method ((vec vec4)) (vec4-z vec)))

(defgeneric get-w (vec)
  (:documentation "Get W component of vec4.")
  (:method ((vec vec4)) (vec4-z vec)))

;; OOP setter methods.
;; Returns the modified vector.

(defgeneric set-x (vec new-value)
  (:documentation "Set X component of vec2 vec3 vec4.
   Returns the modified vector.")
  ;; Float.
  (:method ((vec vec2) (new-value float))
           (setf (vec2-x vec) new-value)
           vec)
  (:method ((vec vec3) (new-value float))
           (setf (vec3-x vec) new-value)
           vec)
  (:method ((vec vec4) (new-value float))
           (setf (vec4-x vec) new-value)
           vec)
  ;; Integer.
  (:method ((vec vec2) (new-value integer))
           (setf (vec2-x vec) (float new-value))
           vec)
  (:method ((vec vec3) (new-value integer))
           (setf (vec3-x vec) (float new-value))
           vec)
  (:method ((vec vec4) (new-value integer))
           (setf (vec4-x vec) (float new-value))
           vec))

(defgeneric set-y (vec new-value)
  (:documentation "Set Y component of vec2 vec3 vec4.
   Returns the modified vector.")
  ;; Float.
  (:method ((vec vec2) (new-value float))
           (setf (vec2-y vec) new-value)
           vec)
  (:method ((vec vec3) (new-value float))
           (setf (vec3-y vec) new-value)
           vec)
  (:method ((vec vec4) (new-value float))
           (setf (vec4-y vec) new-value)
           vec)
  ;;Integer
  (:method ((vec vec2) (new-value integer))
           (setf (vec2-y vec) (float new-value))
           vec)
  (:method ((vec vec3) (new-value integer))
           (setf (vec3-y vec) (float new-value))
           vec)
  (:method ((vec vec4) (new-value integer))
           (setf (vec4-y vec) (float new-value))
           vec))

(defgeneric set-z (vec new-value)
  (:documentation "Set Z component of vec3 vec4.
   Returns the modified vector.")
  ;; Float.
  (:method ((vec vec3) (new-value float))
           (setf (vec3-z vec) new-value)
           vec)
  (:method ((vec vec4) (new-value float))
           (setf (vec4-z vec) new-value)
           vec)
  ;; Integer.
  (:method ((vec vec3) (new-value integer))
           (setf (vec3-z vec) (float new-value))
           vec)
  (:method ((vec vec4) (new-value integer))
           (setf (vec4-z vec) (float new-value))
           vec))

(defgeneric set-w (vec new-value)
  (:documentation "Set W component of vec3 vec4.
   Returns the modified vector.")
  ;; Float.
  (:method ((vec vec4) (new-value float))
           (setf (vec4-w vec) new-value)
           vec)
  ;; Integer.
  (:method ((vec vec4) (new-value integer))
           (setf (vec4-w vec) (float new-value))
           vec))

;;! Mutable operations.
;; Note: All mutable methods return input to allow chaining.
;; I think I've seen this before, hmm.
;; Vec is explicitly returned for readability.

(defgeneric add (vec other)
  (:documentation "Add a vector to other vector of same type or a number.
   \"vec\" is mutated during this procedure!
   Chainable.")
  ;; Vec + Vec.
  (:method ((vec vec2) (other vec2))
           (set-x vec (+ (get-x vec) (get-x other)))
           (set-y vec (+ (get-y vec) (get-y other)))
           vec)
  (:method ((vec vec3) (other vec3))
           (set-x vec (+ (get-x vec) (get-x other)))
           (set-y vec (+ (get-y vec) (get-y other)))
           (set-z vec (+ (get-z vec) (get-z other)))
           vec)
  (:method ((vec vec4) (other vec4))
           (set-x vec (+ (get-x vec) (get-x other)))
           (set-y vec (+ (get-y vec) (get-y other)))
           (set-z vec (+ (get-z vec) (get-z other)))
           (set-w vec (+ (get-w vec) (get-w other)))
           vec)
  ;; Vec + scalar float.
  (:method ((vec vec2) (other float))
           (set-x vec (+ (get-x vec) other))
           (set-y vec (+ (get-y vec) other))
           vec)
  (:method ((vec vec3) (other float))
           (set-x vec (+ (get-x vec) other))
           (set-y vec (+ (get-y vec) other))
           (set-z vec (+ (get-z vec) other))
           vec)
  (:method ((vec vec4) (other float))
           (set-x vec (+ (get-x vec) other))
           (set-y vec (+ (get-y vec) other))
           (set-z vec (+ (get-z vec) other))
           (set-w vec (+ (get-w vec) other))
           vec)
  ;; Vec + scalar integer.
  (:method ((vec vec2) (other integer))
           (set-x vec (+ (get-x vec) (float other)))
           (set-y vec (+ (get-y vec) (float other)))
           vec)
  (:method ((vec vec3) (other integer))
           (set-x vec (+ (get-x vec) (float other)))
           (set-y vec (+ (get-y vec) (float other)))
           (set-z vec (+ (get-z vec) (float other)))
           vec)
  (:method ((vec vec4) (other integer))
           (set-x vec (+ (get-x vec) (float other)))
           (set-y vec (+ (get-y vec) (float other)))
           (set-z vec (+ (get-z vec) (float other)))
           (set-w vec (+ (get-w vec) (float other)))
           vec))

(defgeneric sub (vec other)
  (:documentation "Subtract a vector to other vector of same type or a number.
   \"vec\" is mutated during this procedure!
   Chainable.")
  ;; Vec - Vec.
  (:method ((vec vec2) (other vec2))
           (set-x vec (- (get-x vec) (get-x other)))
           (set-y vec (- (get-y vec) (get-y other)))
           vec)
  (:method ((vec vec3) (other vec3))
           (set-x vec (- (get-x vec) (get-x other)))
           (set-y vec (- (get-y vec) (get-y other)))
           (set-z vec (- (get-z vec) (get-z other)))
           vec)
  (:method ((vec vec4) (other vec4))
           (set-x vec (- (get-x vec) (get-x other)))
           (set-y vec (- (get-y vec) (get-y other)))
           (set-z vec (- (get-z vec) (get-z other)))
           (set-w vec (- (get-w vec) (get-w other)))
           vec)
  ;; Vec - scalar float.
  (:method ((vec vec2) (other float))
           (set-x vec (- (get-x vec) other))
           (set-y vec (- (get-y vec) other))
           vec)
  (:method ((vec vec3) (other float))
           (set-x vec (- (get-x vec) other))
           (set-y vec (- (get-y vec) other))
           (set-z vec (- (get-z vec) other))
           vec)
  (:method ((vec vec4) (other float))
           (set-x vec (- (get-x vec) other))
           (set-y vec (- (get-y vec) other))
           (set-z vec (- (get-z vec) other))
           (set-w vec (- (get-w vec) other))
           vec)
  ;; Vec - scalar integer.
  (:method ((vec vec2) (other integer))
           (set-x vec (- (get-x vec) (float other)))
           (set-y vec (- (get-y vec) (float other)))
           vec)
  (:method ((vec vec3) (other integer))
           (set-x vec (- (get-x vec) (float other)))
           (set-y vec (- (get-y vec) (float other)))
           (set-z vec (- (get-z vec) (float other)))
           vec)
  (:method ((vec vec4) (other integer))
           (set-x vec (- (get-x vec) (float other)))
           (set-y vec (- (get-y vec) (float other)))
           (set-z vec (- (get-z vec) (float other)))
           (set-w vec (- (get-w vec) (float other)))
           vec))

(defgeneric mul (vec other)
  (:documentation "Multiply a vector to other vector of same type or a number.
   \"vec\" is mutated during this procedure!
   Chainable.")
  ;; Vec * Vec.
  (:method ((vec vec2) (other vec2))
           (set-x vec (* (get-x vec) (get-x other)))
           (set-y vec (* (get-y vec) (get-y other)))
           vec)
  (:method ((vec vec3) (other vec3))
           (set-x vec (* (get-x vec) (get-x other)))
           (set-y vec (* (get-y vec) (get-y other)))
           (set-z vec (* (get-z vec) (get-z other)))
           vec)
  (:method ((vec vec4) (other vec4))
           (set-x vec (* (get-x vec) (get-x other)))
           (set-y vec (* (get-y vec) (get-y other)))
           (set-z vec (* (get-z vec) (get-z other)))
           (set-w vec (* (get-w vec) (get-w other)))
           vec)
  ;; Vec * scalar float.
  (:method ((vec vec2) (other float))
           (set-x vec (* (get-x vec) other))
           (set-y vec (* (get-y vec) other))
           vec)
  (:method ((vec vec3) (other float))
           (set-x vec (* (get-x vec) other))
           (set-y vec (* (get-y vec) other))
           (set-z vec (* (get-z vec) other))
           vec)
  (:method ((vec vec4) (other float))
           (set-x vec (* (get-x vec) other))
           (set-y vec (* (get-y vec) other))
           (set-z vec (* (get-z vec) other))
           (set-w vec (* (get-w vec) other))
           vec)
  ;; Vec * scalar integer.
  (:method ((vec vec2) (other integer))
           (set-x vec (* (get-x vec) (float other)))
           (set-y vec (* (get-y vec) (float other)))
           vec)
  (:method ((vec vec3) (other integer))
           (set-x vec (* (get-x vec) (float other)))
           (set-y vec (* (get-y vec) (float other)))
           (set-z vec (* (get-z vec) (float other)))
           vec)
  (:method ((vec vec4) (other integer))
           (set-x vec (* (get-x vec) (float other)))
           (set-y vec (* (get-y vec) (float other)))
           (set-z vec (* (get-z vec) (float other)))
           (set-w vec (* (get-w vec) (float other)))
           vec))

(defgeneric div (vec other)
  (:documentation "Divide a vector to other vector of same type or a number.
   \"vec\" is mutated during this procedure!
   Chainable.")
  ;; Vec / Vec.
  (:method ((vec vec2) (other vec2))
           (set-x vec (/ (get-x vec) (get-x other)))
           (set-y vec (/ (get-y vec) (get-y other)))
           vec)
  (:method ((vec vec3) (other vec3))
           (set-x vec (/ (get-x vec) (get-x other)))
           (set-y vec (/ (get-y vec) (get-y other)))
           (set-z vec (/ (get-z vec) (get-z other)))
           vec)
  (:method ((vec vec4) (other vec4))
           (set-x vec (/ (get-x vec) (get-x other)))
           (set-y vec (/ (get-y vec) (get-y other)))
           (set-z vec (/ (get-z vec) (get-z other)))
           (set-w vec (/ (get-w vec) (get-w other)))
           vec)
  ;; Vec / scalar float.
  (:method ((vec vec2) (other float))
           (set-x vec (/ (get-x vec) other))
           (set-y vec (/ (get-y vec) other))
           vec)
  (:method ((vec vec3) (other float))
           (set-x vec (/ (get-x vec) other))
           (set-y vec (/ (get-y vec) other))
           (set-z vec (/ (get-z vec) other))
           vec)
  (:method ((vec vec4) (other float))
           (set-x vec (/ (get-x vec) other))
           (set-y vec (/ (get-y vec) other))
           (set-z vec (/ (get-z vec) other))
           (set-w vec (/ (get-w vec) other))
           vec)
  ;; Vec / scalar integer.
  (:method ((vec vec2) (other integer))
           (set-x vec (/ (get-x vec) (float other)))
           (set-y vec (/ (get-y vec) (float other)))
           vec)
  (:method ((vec vec3) (other integer))
           (set-x vec (/ (get-x vec) (float other)))
           (set-y vec (/ (get-y vec) (float other)))
           (set-z vec (/ (get-z vec) (float other)))
           vec)
  (:method ((vec vec4) (other integer))
           (set-x vec (/ (get-x vec) (float other)))
           (set-y vec (/ (get-y vec) (float other)))
           (set-z vec (/ (get-z vec) (float other)))
           (set-w vec (/ (get-w vec) (float other)))
           vec))

;;! Immutable operations.

(defgeneric add-new (vec other)
  (:documentation "Add a vector to other vector of same type or a number. Returns a NEW vector!")
  ;; Vec + Vec.
  (:method ((vec vec2) (other vec2))
           (new-vec
             (+ (get-x vec) (get-x other))
             (+ (get-y vec) (get-y other))))
  (:method ((vec vec3) (other vec3))
           (new-vec
             (+ (get-x vec) (get-x other))
             (+ (get-y vec) (get-y other))
             (+ (get-z vec) (get-z other))))
  (:method ((vec vec4) (other vec4))
           (new-vec
             (+ (get-x vec) (get-x other))
             (+ (get-y vec) (get-y other))
             (+ (get-z vec) (get-z other))
             (+ (get-w vec) (get-w other))))
  ;; Vec + scalar float.
  (:method ((vec vec2) (other float))
           (new-vec
             (+ (get-x vec) other)
             (+ (get-y vec) other)))
  (:method ((vec vec3) (other float))
           (new-vec
             (+ (get-x vec) other)
             (+ (get-y vec) other)
             (+ (get-z vec) other)))
  (:method ((vec vec4) (other float))
           (new-vec
             (+ (get-x vec) other)
             (+ (get-y vec) other)
             (+ (get-z vec) other)
             (+ (get-w vec) other)))
  ;; Vec + scalar integer.
  (:method ((vec vec2) (other integer))
           (new-vec
             (+ (get-x vec) (float other))
             (+ (get-y vec) (float other))))
  (:method ((vec vec3) (other integer))
           (new-vec
             (+ (get-x vec) (float other))
             (+ (get-y vec) (float other))
             (+ (get-z vec) (float other))))
  (:method ((vec vec4) (other integer))
           (new-vec
             (+ (get-x vec) (float other))
             (+ (get-y vec) (float other))
             (+ (get-z vec) (float other))
             (+ (get-w vec) (float other)))))

(defgeneric sub-new (vec other)
  (:documentation "Subtract a vector to other vector of same type or a number. Returns a NEW vector!")
  ;; Vec - Vec.
  (:method ((vec vec2) (other vec2))
           (new-vec
             (- (get-x vec) (get-x other))
             (- (get-y vec) (get-y other))))
  (:method ((vec vec3) (other vec3))
           (new-vec
             (- (get-x vec) (get-x other))
             (- (get-y vec) (get-y other))
             (- (get-z vec) (get-z other))))
  (:method ((vec vec4) (other vec4))
           (new-vec
             (- (get-x vec) (get-x other))
             (- (get-y vec) (get-y other))
             (- (get-z vec) (get-z other))
             (- (get-w vec) (get-w other))))
  ;; Vec - scalar float.
  (:method ((vec vec2) (other float))
           (new-vec
             (- (get-x vec) other)
             (- (get-y vec) other)))
  (:method ((vec vec3) (other float))
           (new-vec
             (- (get-x vec) other)
             (- (get-y vec) other)
             (- (get-z vec) other)))
  (:method ((vec vec4) (other float))
           (new-vec
             (- (get-x vec) other)
             (- (get-y vec) other)
             (- (get-z vec) other)
             (- (get-w vec) other)))
  ;; Vec - scalar integer.
  (:method ((vec vec2) (other integer))
           (new-vec
             (- (get-x vec) (float other))
             (- (get-y vec) (float other))))
  (:method ((vec vec3) (other integer))
           (new-vec
             (- (get-x vec) (float other))
             (- (get-y vec) (float other))
             (- (get-z vec) (float other))))
  (:method ((vec vec4) (other integer))
           (new-vec
             (- (get-x vec) (float other))
             (- (get-y vec) (float other))
             (- (get-z vec) (float other))
             (- (get-w vec) (float other)))))

(defgeneric mul-new (vec other)
  (:documentation "Multiply a vector to other vector of same type or a number. Returns a NEW vector!")
  ;; Vec * Vec.
  (:method ((vec vec2) (other vec2))
           (new-vec
             (* (get-x vec) (get-x other))
             (* (get-y vec) (get-y other))))
  (:method ((vec vec3) (other vec3))
           (new-vec
             (* (get-x vec) (get-x other))
             (* (get-y vec) (get-y other))
             (* (get-z vec) (get-z other))))
  (:method ((vec vec4) (other vec4))
           (new-vec
             (* (get-x vec) (get-x other))
             (* (get-y vec) (get-y other))
             (* (get-z vec) (get-z other))
             (* (get-w vec) (get-w other))))
  ;; Vec * scalar float.
  (:method ((vec vec2) (other float))
           (new-vec
             (* (get-x vec) other)
             (* (get-y vec) other)))
  (:method ((vec vec3) (other float))
           (new-vec
             (* (get-x vec) other)
             (* (get-y vec) other)
             (* (get-z vec) other)))
  (:method ((vec vec4) (other float))
           (new-vec
             (* (get-x vec) other)
             (* (get-y vec) other)
             (* (get-z vec) other)
             (* (get-w vec) other)))
  ;; Vec * scalar integer.
  (:method ((vec vec2) (other integer))
           (new-vec
             (* (get-x vec) (float other))
             (* (get-y vec) (float other))))
  (:method ((vec vec3) (other integer))
           (new-vec
             (* (get-x vec) (float other))
             (* (get-y vec) (float other))
             (* (get-z vec) (float other))))
  (:method ((vec vec4) (other integer))
           (new-vec
             (* (get-x vec) (float other))
             (* (get-y vec) (float other))
             (* (get-z vec) (float other))
             (* (get-w vec) (float other)))))


(defgeneric div-new (vec other)
  (:documentation "Divide a vector to other vector of same type or a number. Returns a NEW vector!")
  ;; Vec / Vec.
  (:method ((vec vec2) (other vec2))
           (new-vec
             (/ (get-x vec) (get-x other))
             (/ (get-y vec) (get-y other))))
  (:method ((vec vec3) (other vec3))
           (new-vec
             (/ (get-x vec) (get-x other))
             (/ (get-y vec) (get-y other))
             (/ (get-z vec) (get-z other))))
  (:method ((vec vec4) (other vec4))
           (new-vec
             (/ (get-x vec) (get-x other))
             (/ (get-y vec) (get-y other))
             (/ (get-z vec) (get-z other))
             (/ (get-w vec) (get-w other))))
  ;; Vec / scalar float.
  (:method ((vec vec2) (other float))
           (new-vec
             (/ (get-x vec) other)
             (/ (get-y vec) other)))
  (:method ((vec vec3) (other float))
           (new-vec
             (/ (get-x vec) other)
             (/ (get-y vec) other)
             (/ (get-z vec) other)))
  (:method ((vec vec4) (other float))
           (new-vec
             (/ (get-x vec) other)
             (/ (get-y vec) other)
             (/ (get-z vec) other)
             (/ (get-w vec) other)))
  ;; Vec / scalar integer.
  (:method ((vec vec2) (other integer))
           (new-vec
             (/ (get-x vec) (float other))
             (/ (get-y vec) (float other))))
  (:method ((vec vec3) (other integer))
           (new-vec
             (/ (get-x vec) (float other))
             (/ (get-y vec) (float other))
             (/ (get-z vec) (float other))))
  (:method ((vec vec4) (other integer))
           (new-vec
             (/ (get-x vec) (float other))
             (/ (get-y vec) (float other))
             (/ (get-z vec) (float other))
             (/ (get-w vec) (float other)))))

(defgeneric inv (vec)
  (:documentation "Inverts a vector.")
  ;;! FIXME: NEEDS TO JUST USE THE NEW MUTABLE API!
  (:method ((vec vec2))
           (setf (vec2-x vec) (* (get-x vec) -1))
           (setf (vec2-y vec) (* (get-y vec) -1)))
  (:method ((vec vec3)) (setq vec (mul vec -1.0)))
  (:method ((vec vec4)) (setq vec (mul vec -1.0))))

