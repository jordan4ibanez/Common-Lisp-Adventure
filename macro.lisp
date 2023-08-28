;; Now this is just absurd.
;; Combo runner for setters & getters, automatically inferred based on vector width.
(defmacro getters-and-setters()
  (cons 'progn
        (loop for axis in '(x y z w) for count in '(1 2 3 4) nconc ;; nconc is a flat list for future reference.
                (let ((fun-name-get (intern (string-upcase (format nil "get-~a" axis))))
                      (fun-name-set (intern (string-upcase (format nil "set-~a" axis)))))
                  `(defgeneric ,fun-name-get (vec))
                  `(defgeneric ,fun-name-set (vec new-value))
                  (loop for vec-type in '(vec2 vec3 vec4) nconc
                          (let ((slot-call (intern (string-upcase (format nil "~a-~a" vec-type axis)))))
                            (if (<= count (vec-type-component-amount vec-type))
                                (progn `(
                                         (defmethod ,fun-name-get ((vec ,vec-type)) (,slot-call vec))
                                         (defmethod ,fun-name-set ((vec ,vec-type)(new-value float))
                                           (setf (,slot-call vec) new-value))
                                         (defmethod ,fun-name-set ((vec ,vec-type)(new-value integer))
                                           (setf (,slot-call vec) (float new-value))))))))))))

;; Get number of components in the vector.
(defgeneric get-components (vec))
(defmacro vector-sizes ()
  (cons 'progn (loop for vec-type in '(vec2 vec3 vec4) for return-val in '(2 3 4) :collect
       `(defmethod get-components ((vec ,vec-type)) ,return-val))))
      ;  `(init-get-components ,vec-type ,return-val)))
(vector-sizes)

;; Allows initializing raw generics from code.
(defmacro init-generic (fun-name) `(progn (defgeneric ,fun-name(vec operator))))