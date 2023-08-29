;; A quick note.
;; Anyone that says to write any of this out as a macro:
;;
;; This is easier to understand and maintain for myself.
;; I am doing this for fun. And macroing this does not feel fun at all.
;; If you would like to implement this as a macro, see "macro.lisp" in
;; the root directory. That should get you started.

;;TODO: Translate JOML.

; (defpackage #:vector
;   (:nicknames :vec)
;   (:use :cl :game-math :matrix))

(in-package :cloml)

(export '(
          vec2
          vec3
          vec4
          new-vec
          blank-vec
          clone-vec
          clone-into-vec
          new-vec-from-list
          new-list-from-vec
          vec-type-component-amount
          print-vec
          get-x
          get-y
          get-z
          get-w
          set-x
          set-y
          set-z
          set-w
          set-vec2
          set-vec3
          set-vec4
          add
          sub
          mul
          div
          vec2-add
          vec3-add
          vec4-add
          vec2-sub
          vec3-sub
          vec4-sub
          vec2-mul
          vec3-mul
          vec4-mul
          vec2-div
          vec3-div
          vec4-div
          add-new
          sub-new
          mul-new
          div-new
          vec2-add-new
          vec3-add-new
          vec4-add-new
          vec2-sub-new
          vec3-sub-new
          vec4-sub-new
          vec2-mul-new
          vec3-mul-new
          vec4-mul-new
          vec2-div-new
          vec3-div-new
          vec4-div-new
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

(defgeneric blank-vec (size)
  (:documentation "Create a new vec initialized to 0.0 for all components.")
  (:method ((size integer))
           (cond ((eq size 2) (make-vec2 :x 0.0 :y 0.0))
                 ((eq size 3) (make-vec3 :x 0.0 :y 0.0 :z 0.0))
                 ((eq size 4) (make-vec4 :x 0.0 :y 0.0 :z 0.0 :w 0.0)))))

;; Simple vec cloning utility. Creates a new vec.
(defgeneric clone-vec (vec)
  (:documentation "Clone a vector. Creates a new vec.")
  (:method ((vec vec2)) (new-vec (get-x vec) (get-y vec)))
  (:method ((vec vec3)) (new-vec (get-x vec) (get-y vec) (get-z vec)))
  (:method ((vec vec4)) (new-vec (get-x vec) (get-y vec) (get-z vec) (get-w vec))))

;; Clones one vector INTO another.
;; So (clone-into-vec A B) A will take all the values of B.
(defgeneric clone-into-vec (vec other)
(:documentation "Clones a vector into another.
(clone-into-vec A B) A takes on all values of B.
Chainable.")
  (:method ((vec vec2) (other vec2))
           (set-x vec (get-x other))
           (set-y vec (get-y other))
           vec)
  (:method ((vec vec3) (other vec3))
           (set-x vec (get-x other))
           (set-y vec (get-y other))
           (set-z vec (get-z other))
           vec)
  (:method ((vec vec4) (other vec4))
           (set-x vec (get-x other))
           (set-y vec (get-y other))
           (set-z vec (get-z other))
           (set-w vec (get-w other))
           vec))

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

;; Pass it 'vec2 'vec3 or 'vec4 and you get 2 3 or 4.
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

;;! Specialty setters
;; Useful for messing with REPL or maybe you want to do something special?

(defgeneric set-vec2 (vec x y)
(:documentation "Set the values of a vec2.
\"vec\" is mutated during this procedure!
Chainable.")
  (:method ((vec vec2) (x float) (y float))
           (set-x vec x)
           (set-y vec y)
           vec)
  (:method ((vec vec2) (x integer) (y integer))
           (set-x vec (float x))
           (set-y vec (float y))
           vec))

(defgeneric set-vec3 (vec x y z)
(:documentation "Set the values of a vec3.
\"vec\" is mutated during this procedure!
Chainable.")
  (:method ((vec vec3) (x float) (y float) (z float))
           (set-x vec x)
           (set-y vec y)
           (set-z vec z)
           vec)
  (:method ((vec vec2) (x integer) (y integer) (z integer))
           (set-x vec (float x))
           (set-y vec (float y))
           (set-z vec (float z))
           vec))

(defgeneric set-vec4 (vec x y z w)
(:documentation "Set the values of a vec4.
\"vec\" is mutated during this procedure!
Chainable.")
  (:method ((vec vec4) (x float) (y float) (z float) (w float))
           (set-x vec x)
           (set-y vec y)
           (set-z vec z)
           (set-w vec w)
           vec)
  (:method ((vec vec2) (x integer) (y integer) (z integer) (w integer))
           (set-x vec (float x))
           (set-y vec (float y))
           (set-z vec (float z))
           (set-w vec (float w))
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

;;* Mutable type specific generic operations.
;;* Unfortunately, due to mixed typing, I have to make these functions.
;;* There's probably a better way to do this part though.

(defun vec2-add (vec x y)
"Add a numeric x y value to a vec2.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (+ (get-x vec) (float x)))
  (set-y vec (+ (get-y vec) (float y)))
  vec)

(defun vec3-add (vec x y z)
"Add a numeric x y z value to a vec3.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (+ (get-x vec) (float x)))
  (set-y vec (+ (get-y vec) (float y)))
  (set-z vec (+ (get-z vec) (float z)))
  vec)

(defun vec4-add (vec x y z w)
"Add a numeric x y z w value to a vec4.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (+ (get-x vec) (float x)))
  (set-y vec (+ (get-y vec) (float y)))
  (set-z vec (+ (get-z vec) (float z)))
  (set-w vec (+ (get-w vec) (float w)))
  vec)

(defun vec2-sub (vec x y)
"Subtract a numeric x y value from a vec2.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (- (get-x vec) (float x)))
  (set-y vec (- (get-y vec) (float y)))
  vec)

(defun vec3-sub (vec x y z)
"Subtract a numeric x y z value from a vec3.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (- (get-x vec) (float x)))
  (set-y vec (- (get-y vec) (float y)))
  (set-z vec (- (get-z vec) (float z)))
  vec)

(defun vec4-sub (vec x y z w)
"Subtract a numeric x y z w value from a vec4.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (- (get-x vec) (float x)))
  (set-y vec (- (get-y vec) (float y)))
  (set-z vec (- (get-z vec) (float z)))
  (set-w vec (- (get-w vec) (float w)))
  vec)

(defun vec2-mul (vec x y)
"Multiply a vec2 by a numeric x y value.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (* (get-x vec) (float x)))
  (set-y vec (* (get-y vec) (float y)))
  vec)

(defun vec3-mul (vec x y z)
"Multiply a vec3 by a numeric x y z value.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (* (get-x vec) (float x)))
  (set-y vec (* (get-y vec) (float y)))
  (set-z vec (* (get-z vec) (float z)))
  vec)

(defun vec4-mul (vec x y z w)
"Multiply a vec4 by a numeric x y z w value.
\"vec\" is mutated during this procedure!
Chainable."  
  (set-x vec (* (get-x vec) (float x)))
  (set-y vec (* (get-y vec) (float y)))
  (set-z vec (* (get-z vec) (float z)))
  (set-w vec (* (get-w vec) (float w)))
  vec)

(defun vec2-div (vec x y)
"Divide a vec2 by a numeric x y value.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (/ (get-x vec) (float x)))
  (set-y vec (/ (get-y vec) (float y)))
  vec)

(defun vec3-div (vec x y z)
"Divide a vec3 by a numeric x y z value.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (/ (get-x vec) (float x)))
  (set-y vec (/ (get-y vec) (float y)))
  (set-z vec (/ (get-z vec) (float z)))
  vec)

(defun vec4-div (vec x y z w)
"Divide a vec4 by a numeric x y z w value.
\"vec\" is mutated during this procedure!
Chainable."
  (set-x vec (/ (get-x vec) (float x)))
  (set-y vec (/ (get-y vec) (float y)))
  (set-z vec (/ (get-z vec) (float z)))
  (set-w vec (/ (get-w vec) (float w)))
  vec)


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

;;* Immutable type specific generic operations.
;;* Unfortunately, due to mixed typing, I have to make these functions.
;;* There's probably a better way to do this part though.

(defun vec2-add-new (vec x y)
"Add a vec2 by a numeric x y value.
Returns a new vec2"
  (new-vec 
    (+ (get-x vec) (float x))
    (+ (get-y vec) (float y))))

(defun vec3-add-new (vec x y z)
"Add a vec3 by a numeric x y z value.
Returns a new vec3"
  (new-vec 
    (+ (get-x vec) (float x))
    (+ (get-y vec) (float y))
    (+ (get-z vec) (float z))))

(defun vec4-add-new (vec x y z w)
"Add a vec4 by a numeric x y z w value.
Returns a new vec4"
  (new-vec 
    (+ (get-x vec) (float x))
    (+ (get-y vec) (float y))
    (+ (get-z vec) (float z))
    (+ (get-w vec) (float w))))

(defun vec2-sub-new (vec x y)
"Subtract a numeric x y value from a vec2.
Returns a new vec2"
  (new-vec 
    (- (get-x vec) (float x))
    (- (get-y vec) (float y))))

(defun vec3-sub-new (vec x y z)
"Subtract a numeric x y z value from a vec2.
Returns a new vec3"
  (new-vec 
    (- (get-x vec) (float x))
    (- (get-y vec) (float y))
    (- (get-z vec) (float z))))

(defun vec4-sub-new (vec x y z w)
"Subtract a numeric x y z w value from a vec2.
Returns a new vec4"
  (new-vec 
    (- (get-x vec) (float x))
    (- (get-y vec) (float y))
    (- (get-z vec) (float z))
    (- (get-w vec) (float w))))

(defun vec2-mul-new (vec x y)
"Multiply a vec2 by a numeric x y value.
Returns a new vec2"
  (new-vec 
    (* (get-x vec) (float x))
    (* (get-y vec) (float y))))

(defun vec3-mul-new (vec x y z)
"Multiply a vec3 by a numeric x y z value.
Returns a new vec3"
  (new-vec 
    (* (get-x vec) (float x))
    (* (get-y vec) (float y))
    (* (get-z vec) (float z))))

(defun vec4-mul-new (vec x y z w)
"Multiply a vec4 by a numeric x y z w value.
Returns a new vec4"
  (new-vec 
    (* (get-x vec) (float x))
    (* (get-y vec) (float y))
    (* (get-z vec) (float z))
    (* (get-w vec) (float w))))

(defun vec2-div-new (vec x y)
"Divide a vec2 by a numeric x y value.
Returns a new vec2"
  (new-vec 
    (/ (get-x vec) (float x))
    (/ (get-y vec) (float y))))

(defun vec3-div-new (vec x y z)
"Divide a vec3 by a numeric x y z value.
Returns a new vec3"
  (new-vec 
    (/ (get-x vec) (float x))
    (/ (get-y vec) (float y))
    (/ (get-z vec) (float z))))

(defun vec4-div-new (vec x y z w)
"Divide a vec4 by a numeric x y z w value.
Returns a new vec4"
  (new-vec 
    (/ (get-x vec) (float x))
    (/ (get-y vec) (float y))
    (/ (get-z vec) (float z))
    (/ (get-w vec) (float w))))

(defgeneric inv (vec)
  (:documentation "Inverts a vec.")
  (:method ((vec vec2)) (mul vec -1) vec)
  (:method ((vec vec3)) (mul vec -1) vec)
  (:method ((vec vec4)) (mul vec -1) vec))

