(defpackage #:game-math
  (:nicknames :gm)
  (:use :cl))

(in-package :game-math)

(export '(
          fma
          ))
(defun fma (x y z)
  (+ (* x y) z))