; (defpackage #:game-math
;   (:nicknames :gm)
;   (:use :cl))

(in-package :cloml)

(export '(fma))

(defun fma (x y z)
  (+ (* x y) z))
