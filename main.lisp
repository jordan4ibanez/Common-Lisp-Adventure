;; Auto load all this when compiling.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-glfw3)
  (use-package :cl-glfw3)
  (ql:quickload :cl-opengl)
  (use-package :cl-opengl)
  (ql:quickload :trivial-main-thread)
  (use-package :trivial-main-thread)
  (ql:quickload :str)
  (use-package :str)
  (ql:quickload :infix-math)
  (use-package :infix-math)
  ;; Load up super-load.
  (ql:quickload :super-loader)
  (use-package :super-loader))

;; Now jump into another eval-when to enable usage of eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
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

;;TODO: this is my todo
;;tt 1.) we gotta make a render api pipline
;;tt 2.) implement shaders into the game
;;tt 3.) render a triangle!
;;tt 4.) make the triangle spin

;; Pushes a new item to the end of a list.
(defun push-last(the-item the-listy)
  (push the-item (cdr (last the-listy))))
;; (defvar cool-test (new-vec 3 3 3))

(defun game-initialize ()
  (setf %gl:*gl-get-proc-address* #'get-proc-address)
  (set-key-callback 'window:quit-on-escape)
  (set-window-size-callback 'update-viewport)
  (gl:clear-color 0 0 0 0)
  (set-viewport 600 400)
  (igl:game-new-shader "main" "shaders/vert.vert" "shaders/frag.frag")
  (igl:game-use-shader "main")
  (format t "Hello, yes I am initialized!~%"))

;; Game update function.
;;note: Testing of window clear color
(defvar scalar-thing 0.0)
(defvar scalar-multiplier 0.25)
(defvar up t)
(defvar enable-flashing-debug nil)

(defun game-update()
  (delta:calculate-delta-time)
  (if enable-flashing-debug
      (let ((dtime (* (delta:get-delta) scalar-multiplier)))
        (if up
            
            (progn
              (setf scalar-thing (+ scalar-thing dtime))
              (if (>= scalar-thing 1.0)
                  (progn
                    (setf scalar-thing 1.0)
                    (setf up nil))))

            (progn
              (setf scalar-thing (- scalar-thing dtime))
              (if (<= scalar-thing 0.0)
                  (progn
                    (setf scalar-thing 0.0)
                    (setf up t)))))
        (window:set-clear-color-scalar scalar-thing)))
  (if (delta:fps-update)
      (window:set-title (format nil "My Cool Game | FPS: ~a" (get-fps)))))

;; (print (new-vec-from-list (loop for x in (to-list (new-vec 1 2 3)) collect (* x 2))))

;; (window:set-clear-color 0.5 0.5 0.5)
;; (window:clear-color 1.0 1.0 1.0 1.0)

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
      (game-initialize)
      (loop until (window-should-close-p)
            do (game-tick-procedure)))))

(defun run()
  (sb-int:with-float-traps-masked (:invalid)
    (main-loop)))

(run)
