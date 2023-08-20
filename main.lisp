(defconstant false nil)
(defconstant true t)

; (defun testing()
;   (let ((x 5))
;     (list x x x x x x)))

; (let ((keys '(a b c d e f))
;       (elements '(1 2 3 4 5 6)))
;   (print "test"))

; (let ((cool-test '(:a "ye" :b "neh")))
;       (print (getf cool-test :a)))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

; (print (make-cd "all star" "smash mouth" 5.0 true))

(defvar *db* nil)

(defun push-record(cd)
  (push cd *db*))

(defun dump-db()
  (dolist (cd *db*)
    (format true "~{~a:~10t~a~%~}~%" cd)))

(defun clear-db()
  (setq *db* nil))

(push-record (make-cd "come as you are" "nirvana" 10 true))

(dump-db)

(clear-db)