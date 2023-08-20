(defconstant false nil)
(defconstant true t)

(defun yes()
  true)
(defun no()
  false)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record(cd)
  (push cd *db*))

(defun dump-db()
  (dolist (cd *db*)
    (format true "~{~a:~10t~a~%~}~%" cd)))

(defun clear-db()
  (setq *db* nil))

(add-record (make-cd "come as you are" "nirvana" 10 true))

(dump-db)

(clear-db)

(defun prompt-read(prompt)
  (format *query-io* "~a:" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed true) 0)
    (y-or-n-p "Ripped")))

(defun add-cds()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? ")) (return))))

; (add-cds)

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

;;hmmm very interesting, we can also just do work on things in global state like C.
;; REPL really doesn't like resetting the vars lmao. 
(defvar *global-thing* 1.0)
(dotimes (number 100)
  (setq *global-thing* (* *global-thing* 1.1))
  (print *global-thing*))