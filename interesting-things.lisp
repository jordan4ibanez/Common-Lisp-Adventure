(defconstant false nil)
(defconstant true t)

(defun yes()
  true)
(defun no()
  false)

;;hmmm very interesting, we can also just do work on things in global state like C.
;; REPL really doesn't like resetting the vars lmao. 
(defvar *global-thing* 1.0)
(dotimes (number 100)
  (setq *global-thing* (+ *global-thing* 1.1))
  (print *global-thing*))

(let ((my-cool-vector (make-array 0 :fill-pointer 0 :adjustable true)))
  (dotimes (number 10)
    (vector-push-extend (* number 3) my-cool-vector))
  (print my-cool-vector))