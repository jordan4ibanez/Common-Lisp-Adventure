;; Auto load all this when compiling.
(eval-when (:compile-toplevel)
  ;; Load all remote systems/packages before we step into local.
  (ql:quickload :cl-glfw3)
  (use-package :cl-glfw3)
  (ql:quickload :cl-opengl)
  (use-package :cl-opengl)
  (ql:quickload :trivial-main-thread)
  (use-package :trivial-main-thread)
   ;; Now step into local packages.
  (load "game-packages/constants.lisp")
  (use-package :constants)
  (load "game-packages/delta-time.lisp")
  (use-package :delta-time)
  (load "game-packages/vector.lisp")
  (use-package :vector)
  (load "game-packages/entity.lisp")
  (use-package :entity)
  (load "game-packages/window.lisp")
  (use-package :window))

;; Pushes a new item to the end of a list.
(defun push-last(the-item the-listy)
  (push the-item (cdr (last the-listy))))



;; Game update function.
(defun game-update()
  (delta:calculate-delta-time)
  (if (delta:fps-update) (glfw:set-window-title (format false "My Cool Game | FPS: ~a" (get-fps)))))

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


(run)
