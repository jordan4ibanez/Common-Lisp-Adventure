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
           (load "game-packages/test.lisp")
           (use-package :test-package)
           (load "game-packages/window.lisp")
           (use-package :window))


;; Pushes a new item to the end of a list.
(defun push-last(the-item the-listy)
  (push the-item (cdr (last the-listy))))

(defparameter *repl-output* *standard-output*)

;; Simple time calculation. Also wrappers in FPS calculation.
;; glfwGetTime() is system independent so this needs to be tested on Windows. :T
(defvar *old-time* (glfw:get-time))
(defvar *delta-time* 0.0)
(defvar *frame-time-accumulator* 0.0)
(defvar *fps-accumulator* 0)
(defvar *fps* 0)

;; Wrapper function because mutability of *delta-time* is probably extremely bad.
(defun get-delta()
  *delta-time*)

;; Simple FPS calculation procedure.
(defun *calculate-fps*()
  (setq *frame-time-accumulator* (+ *frame-time-accumulator* (get-delta)))
  (setq *fps-accumulator* (+ *fps-accumulator* 1))
  (cond ((>= *frame-time-accumulator* 1.0)
         (setq *fps* *fps-accumulator*)
         (setq *fps-accumulator* 0)
         (setq *frame-time-accumulator* (- *frame-time-accumulator* 1.0)))))

;; Don't use this anywhere besides in update portion of main loop.
(defun *calculate-delta-time*()
  (let ((current-time (glfw:get-time)))
    (setq *delta-time* (- current-time *old-time*))
    (setq *old-time* current-time))
  (*calculate-fps*))

;; Game update function.
(defun game-update())
  ; (*calculate-delta-time*))

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
