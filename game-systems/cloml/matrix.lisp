(in-package :cloml)

;; This is quite the monster list here.

(export '(new-mat4
          new-mat4-raw
          ;; This was organized like this so this file isn't a mile long.
          get-m00 get-m01 get-m02 get-m03
          get-m10 get-m11 get-m12 get-m13
          get-m20 get-m21 get-m22 get-m23
          get-m30 get-m31 get-m32 get-m33

          set-m00 set-m01 set-m02 set-m03
          set-m10 set-m11 set-m12 set-m13
          set-m20 set-m21 set-m22 set-m23
          set-m30 set-m31 set-m32 set-m33

          set-mat4-raw
          mat4-identity
          set-mat4
          clone-mat4
          mat4-set-transposed))

;; This is JOML mat4f translated (as best as I can.)
;; This package is going to use a lot of shorthand variable names.
;; This is so I don't end up breaking my fingers trying to type it all.
;; Also, some of this may look like "why the hell are you doing it like that?".
;; I'm translating it, dumbness may be changed later on.

;; 6 bits.
(defconstant plane-nx 0)
(defconstant plane-px 1)
(defconstant plane-ny 2)
(defconstant plane-py 3)
(defconstant plane-nz 4)
(defconstant plane-pz 5)

;; 8 bits.
(defconstant corner-nxnynz 0)
(defconstant corner-pxnynz 1)
(defconstant corner-pxpynz 2)
(defconstant corner-nxpynz 3)
(defconstant corner-pxnypz 4)
(defconstant corner-nxnypz 5)
(defconstant corner-nxpypz 6)
(defconstant corner-pxpypz 7)

;; Bitshifted constants.
;;TODO NOTE: 1 is (1 << 1) | -1 is (1 >> 1) in Dlang!
;; (defconstant property-perspective (ash 1 0))
;; (defconstant property-affine      (ash 1 1))
;; (defconstant property-identity    (ash 1 2))
;; (defconstant property-translation (ash 1 3))
;; (defconstant property-orthonormal (ash 1 4))



(defstruct mat4
           ;; (properties (logior (logior (logior property-orthonormal property-translation) property-affine) property-identity) :type integer)
           (m00 1.0 :type float)(m01 0.0 :type float)(m02 0.0 :type float)(m03 0.0 :type float)
           (m10 0.0 :type float)(m11 1.0 :type float)(m12 0.0 :type float)(m13 0.0 :type float)
           (m20 0.0 :type float)(m21 0.0 :type float)(m22 1.0 :type float)(m23 0.0 :type float)
           (m30 0.0 :type float)(m31 0.0 :type float)(m32 0.0 :type float)(m33 1.0 :type float))

;; m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33

(defun new-mat4 ()
  (new-mat4-raw 1 0 0 0
                0 1 0 0
                0 0 1 0
                0 0 0 1))

;; I suppose this function is for when you really hate yourself.
(defun new-mat4-raw (m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
  (make-mat4 :m00 (float m00) :m01 (float m01) :m02 (float m02) :m03 (float m03)
             :m10 (float m10) :m11 (float m11) :m12 (float m12) :m13 (float m13)
             :m20 (float m20) :m21 (float m21) :m22 (float m22) :m23 (float m23)
             :m30 (float m30) :m31 (float m31) :m32 (float m32) :m33 (float m33)))

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

(defun set-mat4-raw (mat m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
  (set-m00 mat m00)
  (set-m01 mat m01)
  (set-m02 mat m02)
  (set-m03 mat m03)

  (set-m10 mat m10)
  (set-m11 mat m11)
  (set-m12 mat m12)
  (set-m13 mat m13)

  (set-m20 mat m20)
  (set-m21 mat m21)
  (set-m22 mat m22)
  (set-m23 mat m23)

  (set-m30 mat m30)
  (set-m31 mat m31)
  (set-m32 mat m32)
  (set-m33 mat m33)
  mat)

(defun mat4-identity (mat)
  (set-m00 mat 1.0)
  (set-m01 mat 0.0)
  (set-m02 mat 0.0)
  (set-m03 mat 0.0)
  
  (set-m10 mat 0.0)
  (set-m11 mat 1.0)
  (set-m12 mat 0.0)
  (set-m13 mat 0.0)

  (set-m20 mat 0.0)
  (set-m21 mat 0.0)
  (set-m22 mat 1.0)
  (set-m23 mat 0.0)

  (set-m30 mat 0.0)
  (set-m31 mat 0.0)
  (set-m32 mat 0.0)
  (set-m33 mat 1.0)
  mat)

(defun set-mat4 (mat other)
  (set-m00 mat (get-m00 other))
  (set-m01 mat (get-m01 other))
  (set-m02 mat (get-m02 other))
  (set-m03 mat (get-m03 other))

  (set-m10 mat (get-m10 other))
  (set-m11 mat (get-m11 other))
  (set-m12 mat (get-m12 other))
  (set-m13 mat (get-m13 other))

  (set-m20 mat (get-m20 other))
  (set-m21 mat (get-m21 other))
  (set-m22 mat (get-m22 other))
  (set-m23 mat (get-m23 other))

  (set-m30 mat (get-m30 other))
  (set-m31 mat (get-m31 other))
  (set-m32 mat (get-m32 other))
  (set-m33 mat (get-m33 other)))

(defun clone-mat4 (mat)
  (let ((clone-of-mat (new-mat4)))
    (set-m00 clone-of-mat (get-m00 mat))
    (set-m01 clone-of-mat (get-m01 mat))
    (set-m02 clone-of-mat (get-m02 mat))
    (set-m03 clone-of-mat (get-m03 mat))
    
    (set-m10 clone-of-mat (get-m10 mat))
    (set-m11 clone-of-mat (get-m11 mat))
    (set-m12 clone-of-mat (get-m12 mat))
    (set-m13 clone-of-mat (get-m13 mat))

    (set-m20 clone-of-mat (get-m20 mat))
    (set-m21 clone-of-mat (get-m21 mat))
    (set-m22 clone-of-mat (get-m22 mat))
    (set-m23 clone-of-mat (get-m23 mat))

    (set-m30 clone-of-mat (get-m30 mat))
    (set-m31 clone-of-mat (get-m31 mat))
    (set-m32 clone-of-mat (get-m32 mat))
    (set-m33 clone-of-mat (get-m33 mat))
    clone-of-mat))



(defun mat4-set-transposed (mat)
  (let ((nm10 (get-m01 mat)) (nm12 (get-m21 mat)) (nm13 (get-m31 mat))
        (nm20 (get-m02 mat)) (nm21 (get-m12 mat)) (nm30 (get-m03 mat)) 
        (nm31 (get-m13 mat)) (nm32 (get-m23 mat)))
        
    (set-mat4-raw mat (get-m00 mat) (get-m10 mat) (get-m20 mat) (get-m30 mat)
                  nm10 (get-m11 mat) nm12 nm13
                  nm20 nm21 (get-m22 mat) (get-m32 mat)
                  nm30 nm31 nm32 (get-m33 mat))))

(defun mat4-set-translation (mat vec)
  (set-m30 mat (get-x vec))
  (set-m31 mat (get-y vec))
  (set-m32 mat (get-z vec)))

;; This is gonna be a bit complicated
(defun mat4-set-rotation (mat angle-x angle-y angle-z)
  ;; Climb the tower
  (let ((sin-x (sin angle-x))
        (sin-y (sin angle-y))
        (sin-z (sin angle-z)))
    (let ((cos-x (cos-from-sin sin-x angle-x))
          (cos-y (cos-from-sin sin-y angle-y))
          (cos-z (cos-from-sin sin-z angle-z)))
      (let ((m-sin-x (* sin-x -1.0))
            (m-sin-y (* sin-y -1.0))
            (m-sin-z (* sin-y -1.0)))
        (let (;; rotate X
              (nm11 cos-x)
              (nm12 sin-x)
              (nm21 m-sin-x)
              (nm22 cos-x))
          (let (;; rotate Y
                (nm00 cos-y)
                (nm01 (* nm21 m-sin-y))
                (nm02 (* nm22 m-sin-y)))
            (set-m20 mat sin-y)
            (set-m21 mat (* nm21 cos-y))
            (set-m22 mat (* nm22 cos-y))
            ;;rotate Z
            (set-m00 mat (* nm00 cos-z))
            (set-m01 mat (+ (* nm01 cos-z) (* nm11 sin-z)))
            (set-m02 mat (+ (* nm02 cos-z) (* nm12 sin-z)))
            (set-m10 mat (* nm00 m-sin-z))
            (set-m11 mat (+ (* nm01 m-sin-z) (* nm11 cos-z)))
            (set-m12 mat (+ (* nm02 m-sin-z) (* nm12 cos-z))))))))
  mat)
