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

;; Standard lispy way to get time in whatever the system resolution is.
(defun get-floating-time()
  (float (get-internal-real-time)))

;; Standard lispy way to get the time units. (system time resolution)
(defun get-floating-time-units()
  (float internal-time-units-per-second))

;; Simple time calculation. Also wrappers in FPS calculation.
(defvar *old-time* 0.0);;(get-floating-time))
(defvar *delta-time* 0.0)
(defvar *frame-time-accumulator* 0.0)
(defvar *fps-accumulator* 0)
(defvar *fps* 0)

;; Wrapper function because mutability of *delta-time* is probably extremely bad.
(defun get-delta()
  *delta-time*)

(defun get-fps()
  *fps*)

;; Simple FPS calculation procedure.
(defun *calculate-fps*()
  (setq *frame-time-accumulator* (+ *frame-time-accumulator* (get-delta)))
  (setq *fps-accumulator* (+ *fps-accumulator* 1))
  (cond ((>= *frame-time-accumulator* 1.0)
         (setq *fps* *fps-accumulator*)
         (setq *fps-accumulator* 0)
         (setq *frame-time-accumulator* (- *frame-time-accumulator* 1.0)))))

;; Deprecated delta time calculation. Calculates to seconds.
;; DO NOT USE THIS, IT'S NOT CROSS PLATFORM!
; ;; Also it's been ransacked to only work as a double check to the current calculation.
(defvar *deprecated-delta-time* 0.0)
(defvar *deprecated-old-time* 0.0)
(defun *deprecated-glfw-calculate-delta-time*()
  (let ((current-time (glfw:get-time)))
    (setq *deprecated-delta-time* (- current-time *deprecated-old-time*))
    (setq *deprecated-old-time* current-time))
  *deprecated-delta-time*)

;; This is a test in place method for utilizing built in functionality
;; Don't use this anywhere besides in update portion of main loop.
(defun *calculate-delta-time*()
  (let ((current-time (get-floating-time)))
    (setq *delta-time* (/ (- current-time *old-time*) (get-floating-time-units)))
    ;; Uncomment this program to test against glfw3! (you also gotta uncomment the other function)
    (format true "BEGIN TEST:~%GLFW: ~a~%CL:   ~a~%END TEST~%" (*deprecated-glfw-calculate-delta-time*) *delta-time*)
    (setq *old-time* current-time))
  (*calculate-fps*))

(run)


;; Game update function.
(defun game-update()
  (*calculate-delta-time*)
  (glfw:set-window-title (format false "My Cool Game | FPS: ~a" (get-fps))))



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
