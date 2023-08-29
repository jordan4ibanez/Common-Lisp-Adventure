;; Auto load all this when compiling.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-glfw3)
  (use-package :cl-glfw3)
  (ql:quickload :cl-opengl)
  (use-package :cl-opengl)
  (ql:quickload :trivial-main-thread)
  (use-package :trivial-main-thread)
  ;; Load up super-load.
  (ql:quickload :super-loader)
  (use-package :super-loader))

;; Now jump into another eval-when to enable usage of eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
  ; (load "super-load.lisp")
  ;; Now step into local packages.
  (super-load "game-systems/cloml")
  ;; Legacy learning things. DEFINITELY should be systems.
  (load "game-systems/delta-time.lisp")
  (use-package :delta-time)
  (load "game-systems/entity.lisp")
  (use-package :entity)
  (load "game-systems/internal-opengl.lisp")
  (use-package :internal-opengl)
  (load "game-systems/window.lisp")
  (use-package :window))



;; Pushes a new item to the end of a list.
(defun push-last(the-item the-listy)
  (push the-item (cdr (last the-listy))))
; (defvar cool-test (new-vec 3 3 3))

; (defvar cccc (new-vec-from-list (loop for x in (to-list cool-test) collect (* x 5.0))))

;; Game update function.
(defun game-update()
  (delta:calculate-delta-time)
  (if (delta:fps-update) (glfw:set-window-title (format nil "My Cool Game | FPS: ~a" (get-fps)))))

; (print (new-vec-from-list (loop for x in (to-list (new-vec 1 2 3)) collect (* x 2))))

 ;; This is run every frame of the game.
(defun game-tick-procedure()
  (poll-events)
  (game-update)
  (render)
  (swap-buffers))

(defun main-loop()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "Window test" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'window:quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)
      (loop until (window-should-close-p)
        do (game-tick-procedure)))))

(defun run()
  (sb-int:with-float-traps-masked (:invalid)
    (main-loop)))

;; Note to self:
(loop for i from 0 to 100 do (print "REMEMBER TO CLONE SUPER-LOADER INTO LOCAL PACKAGES!"))

(run)
