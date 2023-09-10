(defpackage #:window
  (:use :cl :cl-glfw3 :cl-opengl :cloml))

(in-package :window)

(export '(render
          set-viewport
          quit-on-escape
          update-viewport
          pass-through-update-viewport
          set-title
          set-clear-color-scalar
          set-clear-color
          *clear-color*))

(defvar *window-size* (new-vec 0 0))

(defvar *clear-color* (new-vec 0 0 0 1))

;; You have to reload the game to make this re-initialize unfortunately.
(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (when (and (eq key :e) (eq action :press))
    (print "cool"))
  (when t
    (print (format *standard-output* "~a | ~a" key action))))

(defun render ()
  (gl:clear-color (get-x *clear-color*) (get-y *clear-color*) (get-z *clear-color*) (get-w *clear-color*))
  (gl:clear :color-buffer)
;;note: This is just a debugging test, I don't recommend raw pushing matrices to your gl program lmao.
  
  )
  ;; (gl:with-pushed-matrix
    ;; (gl:color 0.1 0.1 0.1)
    ;; (gl:rect -25 -25 25 25)))

(defun set-title (new-title)
  (glfw:set-window-title new-title))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height))
  ;; (gl:matrix-mode :projection)
  ;; (gl:load-identity)
  ;; (gl:ortho -50 50 -50 50 -1 1)
  ;; (gl:matrix-mode :modelview)
  ;; (gl:load-identity))

(defun set-clear-color-scalar (scalar)
  (setf *clear-color* (new-vec scalar scalar scalar 1.0)))

(defun set-clear-color (r g b)
  (setf *clear-color* (new-vec r g b 1.0)))


;; So we shove it into a custom thing WOOOO!
(defun pass-through-update-viewport(w h)
  (format t "window resized to ~a, ~a~%" w h)
  (set-viewport w h))
;; ^
;; |
;; (Code generator) You have to reload the game to make this re-initialize unfortunately.
(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (pass-through-update-viewport w h))
