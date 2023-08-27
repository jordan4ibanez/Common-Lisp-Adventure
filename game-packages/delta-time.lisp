(defpackage #:delta-time
  (:nicknames :delta)
  (:use :cl :cl-glfw3 :constants))

(in-package :delta-time)

(export '(
          get-delta
          get-fps
          fps-update
          calculate-delta-time))




;; Standard lispy way to get time in whatever the system resolution is.
(defun get-floating-time()
  (float (get-internal-real-time)))

;; Standard lispy way to get the time units. (system time resolution)
(defun get-floating-time-units()
  (float internal-time-units-per-second))

;; Simple time calculation. Also wrappers in FPS calculation.
(defvar old-time 0.0);;(get-floating-time))
(defvar delta-time 0.0)
(defvar frame-time-accumulator 0.0)
(defvar fps-accumulator 0)
(defvar fps 0)
(defvar fps-updated nil)

;; Wrapper function because mutability of delta-time is probably extremely bad.
(defun get-delta()
  delta-time)

(defun get-fps()
  fps)
(defun fps-update()
  fps-updated)

;; Simple FPS calculation procedure.
(defun calculate-fps()
  (setq frame-time-accumulator (+ frame-time-accumulator (get-delta)))
  (setq fps-accumulator (+ fps-accumulator 1))
  ;; Do a fps & fps update. Update flag for frame.
  (cond ((>= frame-time-accumulator 1.0)
         (progn
           (setq fps fps-accumulator)
           (setq fps-accumulator 0)
           (setq frame-time-accumulator (- frame-time-accumulator 1.0))
           (setq fps-updated true)))
        ;; Else there's no update, reset flag.
        (true (progn)
              (setq fps-updated nil))))


;; Deprecated delta time calculation. Calculates to seconds.
;; DO NOT USE THIS, IT'S NOT CROSS PLATFORM!
; ;; Also it's been ransacked to only work as a double check to the current calculation.
; (defvar deprecated-delta-time 0.0)
; (defvar deprecated-old-time 0.0)
; (defun deprecated-glfw-calculate-delta-time()
;   (let ((current-time (glfw:get-time)))
;     (setq deprecated-delta-time (- current-time deprecated-old-time))
;     (setq deprecated-old-time current-time))
;   deprecated-delta-time)

;; This is a test in place method for utilizing built in functionality
;; Don't use this anywhere besides in update portion of main loop.
(defun calculate-delta-time()
  (let ((current-time (get-floating-time)))
    (setq delta-time (/ (- current-time old-time) (get-floating-time-units)))
    ;; Uncomment this program to test against glfw3! (you also gotta uncomment the other function)
    ; (format true "BEGIN TEST:~%GLFW: ~a~%CL:   ~a~%END TEST~%" (deprecated-glfw-calculate-delta-time) delta-time)
    (setq old-time current-time))
  (calculate-fps))
