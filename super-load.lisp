(defpackage #:super-load
  (:nicknames :loadenstein3d)
  (:use :cl))

(export '(
          super-load 
           ))
; (defun super-load(relative-path)
; "Loads an asdf system based on the relative path of the current working directory (root of project).
; This function is primarily aimed at game dev.
; You can use this to load your project specific local systems in a traditional Java/Lua/Python-like manor.
; The folder which encapsulates your system must match the name of your system.
; The .asd file which identifies your system much match the name of your system.
; Example: (super-load \"game-things/my-cool-system\")
; Now system :my-cool-system has been loaded, packages contained inside of it are freely available."
;   (let ((system-name (nth 0 (last (split-sequence:split-sequence #\/ relative-path)))))
;     (let ((real-path (concatenate 'string (format nil "~a" (uiop:getcwd)) relative-path "/" system-name ".asd")))
;       (handler-case
;         (progn
;           ;; This might only work on POSIX systems.
;           (asdf:load-asd real-path)
;           (quicklisp:quickload system-name)
;           (use-package (intern (string-upcase system-name))))
;       (error ()
;         (error (format nil "super-load, ERROR! System (~a) was not found in:~%~a~%(Did you make a typo?)" system-name real-path)))))))

(use-package :super-load)