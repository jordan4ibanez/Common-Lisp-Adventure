
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

;; Java wrappered notes

; note: (not X) where X is a (func) or boolean
(setf *random-state* (make-random-state true))

; for (int number = 10; number < 10; number++) {scope}
(dotimes (number 10)
  ; if (1 == random(0,1)) {scope}
  (if (eq 1 (random 2))
      (print "cool")
  ;else
      (print "not cool")
  )
  (if (>= 10 (random 50))
      (print "test"))
)

