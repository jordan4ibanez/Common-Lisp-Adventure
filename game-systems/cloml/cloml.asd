;;;; cloml.asd

(asdf:defsystem "cloml"
  ; :serial t
  :description "JOML translated to lisp the best I can."
  :author "jordan4ibanez"
  :version "0.0.0"
  :license "GPLV3"
  ; :depends-on (#:cffi #:alexandria)
  :components ((:file "package")
               (:file "vector")
               (:file "matrix" :depends-on ("vector"))
               (:file "game-math")))