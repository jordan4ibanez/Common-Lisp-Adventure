(defconstant false nil)
(defconstant true t)

(defun yes()
  true)
(defun no()
  false)

;; Pushes a new item to the end of a list.
(defun push-last(the-item the-listy)
  (push the-item (cdr (last the-listy))))

(defparameter *repl-output* *standard-output*)

;; Outputs to the REPL terminal.
; (defun printf(any)
;   (print any *repl-output*))
;
; ;; Automatically formats output.
; (defun print-format(any)
;   (format *repl-output* any))
;
; ;; Print with a "/n".
; (defun println(&rest args)
;   (push-last "~%" args)
;   (print-format(apply #'concatenate 'string args)))


;; Auto load all this when compiling.
(eval-when (:compile-toplevel)
  (ql:quickload :cl-glfw3)
  (use-package :cl-glfw3)
  (ql:quickload :cl-opengl)
  (use-package :cl-opengl)
  (ql:quickload :trivial-main-thread)
  (use-package :trivial-main-thread)
  (ql:quickload :local-time)
  (use-package :local-time))


(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (when (and (eq key :e) (eq action :press))
    (print "cool")))


(defun render ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect -25 -25 25 25)))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))


;; Simple time calculation. Also wrappers in FPS calculation.
;; glfwGetTime() is system independent so this needs to be tested on Windows. :T
(defvar old-time (glfw:get-time))
(defvar *delta-time*)
(defvar fps-accumulator 0)


(defun calculate-delta-time()
  (let ((current-time (glfw:get-time)))
    (setq *delta-time* (- current-time old-time))
    (setq old-time current-time)))

(defun get-delta()
  *delta-time*)


  ; (setq counter (+ counter 1))
  ; (if (> counter 60) (setq counter 0))
  ;
  ; (cond ((eq 60 counter)
  ;        (print "SETTING")))
  ; (cond ((eq 30 counter)
  ;        (print "something else"))))






(defun basic-window-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "Window test" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)
      (loop until (window-should-close-p)
        do (render)
        ; do (dynamic)
        do (calculate-delta-time)
        do (swap-buffers)
        do (poll-events)))))

(defun run()
  (sb-int:with-float-traps-masked (:invalid)
    (basic-window-example)))

(run)
