;; Thanks to ICan'tThinkOfAGoodName in the Lisp Discord for helping out with making this work on Windows!
(defpackage #:super-load
  (:nicknames :loadenstein3d)
  (:use :cl))

(export '(
          super-load 
           ))

(defun super-load(relative-path)
"Loads an asdf system based on the relative path of the current working directory (root of project).
This function is primarily aimed at game dev.
You can use this to load your project specific local systems in a traditional Java/Lua/Python-like manor.
The folder which encapsulates your system must match the name of your system.
The .asd file which identifies your system much match the name of your system.
Arg: relative-path is a list of strings. Pretend each space is walking into a new folder.
Example: (super-load \"game-things/my-cool-system\")
Now system :my-cool-system has been loaded, packages contained inside of it are freely available."
  ;; Note, Windows systems use "/" for file hierarchy.
  (let ((old-relative-path relative-path))
    ;; This is why you don't have to append "/" to your string!
    (setq relative-path (concatenate 'string relative-path "/"))
    (handler-case
        (let ((system-name (car (last (pathname-directory relative-path))))
              (real-path (truename relative-path)))
          (let ((asd-file-name (concatenate 'string system-name ".asd")))
            (let ((completed-asd-path (merge-pathnames real-path asd-file-name)))
              (progn
                (asdf:load-asd completed-asd-path)
                (quicklisp:quickload system-name)
                (use-package (intern (string-upcase system-name)))))))
      (error ()
        (error (format nil "super-load, ERROR! System (~a) was not found in:~%~a~%(Did you make a typo?)" 
              (car (last (split-sequence:split-sequence #\/ old-relative-path)))
              (concatenate 'string (format nil "~a" (uiop:getcwd)) old-relative-path)))))))

(use-package :super-load)