(defpackage #:window
  (:use :cl :cl-glfw3 :cl-opengl :cloml))

(in-package :window)

(export '(
          render
          set-viewport
          quit-on-escape
          update-viewport))

(defvar *window-size* (new-vec 0 0))

;; You have to reload the game to make this re-initialize unfortunately.
(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (when (and (eq key :e) (eq action :press))
    (print "cool"))
  (when t (print "hi")))

(defun render ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 0.1 0.1 0.1)
    (gl:rect -25 -25 25 25)))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  )
  ; (gl:matrix-mode :projection)
  ; (gl:load-identity)
  ; (gl:ortho -50 50 -50 50 -1 1)
  ; (gl:matrix-mode :modelview)
  ; (gl:load-identity))


;; (Code generator) You have to reload the game to make this re-initialize unfortunately.
(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (pass-through-update-viewport w h))

;; So we shove it into a custom thing WOOOO!
(defun pass-through-update-viewport(w h)
  (format t "window resized to ~a, ~a~%" w h)
  (set-viewport w h))
