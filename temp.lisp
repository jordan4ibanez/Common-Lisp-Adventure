;; To list.
;; TODO: THIS IS CAUSING AN ERROR!!!
;; TODO: This error is from the getters and setters not loading into memory
; (defgeneric to-list(vec))

; (defmethod to-list((vec vec2))
;   (list (get-x vec) (get-y vec)))

; (defmethod to-list((vec vec3))
;   (list (get-x vec) (get-y vec) (get-z vec)))

; (defmethod to-list((vec vec4))
;   (list (get-x vec) (get-y vec) (get-z vec) (get-w vec)))

;; TODO: END ERROR!!!

; ;; NOTE: This is where macros begin in this file.

; ;; Remove a bunch of boilerplate functions.
; (defmacro boilerplate-vec-operations (fun-name operation vector-type)
;     `(progn
;         (defmethod ,fun-name((vec integer) (operator integer)) (print "hi"))
;         (defmethod ,fun-name((vec ,vector-type) (operator ,vector-type))
;           (new-vec-from-list (loop for x in (to-list vec) for y in (to-list operator) collect (,operation x y))))
;         (defmethod ,fun-name((vec ,vector-type) (operator float))
;           (new-vec-from-list (loop for x in (to-list vec) collect (,operation x operator))))
;         (defmethod ,fun-name((vec ,vector-type) (operator integer))
;           (new-vec-from-list (loop for x in (to-list vec) collect (,operation x (float operator)))))))

; ;; Note: This has been reduces to simplified types because this file might
; ;; end up a few ten thousand lines long if I don't hold back.

; ;; This is an unholy procedure
; ; (loop for fun-name in '(mul add div sub) for operation in '(* + / -) do
; ;         (eval `(init-generic ,fun-name))
; ;         (loop for vector-type in '(vec2 vec3 vec4) do
; ;             (eval `(boilerplate-vec-operations ,fun-name ,operation ,vector-type))))

; ; ;; Invert (Vec * -1). Useful for random things. Wordy alternative to (mul vec -1)

; ; (defgeneric invert(vector))

; ; (defmethod invert((vector1 vec2))
; ;  (mul vector1 -1))

; ; (defmethod invert((vector1 vec3))
; ;  (mul vector1 -1))

; ; (defmethod invert((vector1 vec4))
; ;  (mul vector1 -1))


; ; (defvar my-var (new-vec 0 0))