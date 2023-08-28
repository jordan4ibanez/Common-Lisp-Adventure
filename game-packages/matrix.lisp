(defpackage #:matrix
  (:nicknames :mat)
  (:use :cl :game-math :vector))

(in-package :vector)

;; This is JOML mat4f translated (as best as I can.)
;; This package is going to use a lot of shorthand variable names.
;; This is so I don't end up breaking my fingers trying to type it all.

(defstruct mat4
  (m00 1.0 :type float)(m01 0.0 :type float)(m02 0.0 :type float)(m03 0.0 :type float)
  (m10 0.0 :type float)(m11 1.0 :type float)(m12 0.0 :type float)(m13 0.0 :type float)
  (m20 0.0 :type float)(m21 0.0 :type float)(m22 1.0 :type float)(m23 0.0 :type float)
  (m30 0.0 :type float)(m31 0.0 :type float)(m32 0.0 :type float)(m33 1.0 :type float))

;; m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
; (defun new-mat4-raw (mat m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))

;;Basic getters.
(defun get-m00 (mat)
"Get 0X0 in a mat4"
  (mat4-m00 mat))
(defun get-m01 (mat)
"Get 0X1 in a mat4"
  (mat4-m01 mat))
(defun get-m02 (mat)
"Get 0X2 in a mat4"
  (mat4-m02 mat))
(defun get-m03 (mat)
"Get 0X3 in a mat4"
  (mat4-m03 mat))
(defun get-m10 (mat)
"Get 1X0 in a mat4"
  (mat4-m10 mat))
(defun get-m11 (mat)
"Get 1X1 in a mat4"
  (mat4-m11 mat))
(defun get-m12 (mat)
"Get 1X2 in a mat4"
  (mat4-m12 mat))
(defun get-m13 (mat)
"Get 1X3 in a mat4"
  (mat4-m13 mat))
(defun get-m20 (mat)
"Get 2X0 in a mat4"
  (mat4-m20 mat))
(defun get-m21 (mat)
"Get 2X1 in a mat4"
  (mat4-m21 mat))
(defun get-m22 (mat)
"Get 2X2 in a mat4"
  (mat4-m22 mat))
(defun get-m23 (mat)
"Get 2X3 in a mat4"
  (mat4-m23 mat))
(defun get-m30 (mat)
"Get 3X0 in a mat4"
  (mat4-m30 mat))
(defun get-m31 (mat)
"Get 3X1 in a mat4"
  (mat4-m31 mat))
(defun get-m32 (mat)
"Get 3X2 in a mat4"
  (mat4-m32 mat))
(defun get-m33 (mat)
"Get 3X3 in a mat4"
  (mat4-m33 mat))

;; Basic setters.
(defun set-m00 (mat v)
"Set 0X0 in a mat4."
  (setf (mat4-m00 mat) (float v))
  mat)
(defun set-m01 (mat v)
"Set 0X1 in a mat4."
  (setf (mat4-m01 mat) (float v))
  mat)
(defun set-m02 (mat v)
"Set 0X2 in a mat4."
  (setf (mat4-m02 mat) (float v))
  mat)
(defun set-m03 (mat v)
"Set 0X3 in a mat4."
  (setf (mat4-m03 mat) (float v))
  mat)
(defun set-m10 (mat v)
"Set 1X0 in a mat4."
  (setf (mat4-m10 mat) (float v))
  mat)
(defun set-m11 (mat v)
"Set 1X1 in a mat4."
  (setf (mat4-m11 mat) (float v))
  mat)
(defun set-m12 (mat v)
"Set 1X2 in a mat4."
  (setf (mat4-m12 mat) (float v))
  mat)
(defun set-m13 (mat v)
"Set 1X3 in a mat4."
  (setf (mat4-m13 mat) (float v))
  mat)
(defun set-m20 (mat v)
"Set 2X0 in a mat4."
  (setf (mat4-m20 mat) (float v))
  mat)
(defun set-m21 (mat v)
"Set 2X1 in a mat4."
  (setf (mat4-m21 mat) (float v))
  mat)
(defun set-m22 (mat v)
"Set 2X2 in a mat4."
  (setf (mat4-m22 mat) (float v))
  mat)
(defun set-m23 (mat v)
"Set 2X3 in a mat4."
  (setf (mat4-m23 mat) (float v))
  mat)
(defun set-m30 (mat v)
"Set 3X0 in a mat4."
  (setf (mat4-m30 mat) (float v))
  mat)
(defun set-m31 (mat v)
"Set 3X1 in a mat4."
  (setf (mat4-m31 mat) (float v))
  mat)
(defun set-m32 (mat v)
"Set 3X2 in a mat4."
  (setf (mat4-m32 mat) (float v))
  mat)
(defun set-m33 (mat v)
"Set 3X3 in a mat4."
  (setf (mat4-m33 mat) (float v))
  mat)