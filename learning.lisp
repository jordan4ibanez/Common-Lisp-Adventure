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


; (dump-db)

; (clear-db)

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

(defun save-db(filename)
  (with-open-file(out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

; (save-db "testing")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))

; (load-db "testing")

(print "starting")

(add-record (make-cd "come as you are" "nirvana" 10 true))
(add-record (make-cd "come as you are" "nirvana" 10 true))

; (print *db*)
; (print "ahhhhhhhhhhhhhhh")
; (print (remove-if-not
;     #'(lambda (cd) (equal (getf cd :artist) "nirvana")) *db*))

; (remove-if-not #'(lambda (cd) (equal (getf cd :artist) "nirvana")) *db*)

; (defun select (selector-fn)
;   (remove-if-not selector-fn *db*))

; (defun artist-selector (artist))
; (select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))

; (defun my-function(&key first second third)
;   (if first (print first))
;   (if second (print second))
;   (if third (print third)))

; (my-function :first 123)

(defun where (&key title artist rating (ripped false ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  true)
       (if artist   (equal (getf cd :artist) artist) true)
       (if rating   (equal (getf cd :rating) rating) true)
       (if ripped-p (equal (getf cd :ripped) ripped) true))))

; (defun select-by-artist (artist)
;   (remove-if-not
;       #'(lambda (cd) (equal (getf cd :artist) artist))
;     *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))


; (print (select (lambda (cd) (equal (getf cd :artist) "nirvana"))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
    (mapcar
        #'(lambda (row)
            (when (funcall selector-fn row)
                  (if title    (setf (getf row :title)  title))
                  (if artist   (setf (getf row :artist) artist))
                  (if rating   (setf (getf row :rating) rating))
                  (if ripped-p (setf (getf row :ripped) ripped)))
            row) *db*)))

(update (where :artist "pantera") :rating 1000)

(dump-db)

(add-cds)

(defvar *test* '(1 2 3 4 5 6 7 8))
(mapcar
    #'(lambda (row)
        (print row)
        row) *test*)
