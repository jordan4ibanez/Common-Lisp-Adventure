; (defpackage #:game-math
;   (:nicknames :gm)
;   (:use :cl))

(in-package :cloml)

(export '(pi-half
          fma
          cos-from-sin))

(defconstant pi-half (/ pi 2.0))
(defconstant pi-2 (* pi 2.0))

(defun fma (x y z)
  (+ (* x y) z))

(defun cos-from-sin (sin angle)
  (let ((cos (sqrt (- 1.0 (* sin sin))))
        (a (+ angle pi-half)))
    (let ((b (* (- a (/ a pi-2)) pi2)))       
      (if (< b 0.0)
          (setf b (+ b pi2)))
      (if (>= b pi) (* cos -1.0))
      cos)))
