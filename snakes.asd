;;;; snakes.asd

(asdf:defsystem #:snakes
  :serial t
  :description "Python style generators for Common Lisp."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:cl-cont #:closer-mop #:fiveam #:iterate #:cl-utilities)
  :components ((:file "package")
	       (:file "util")
               (:file "snakes" :depends-on ("util"))
               (:file "do-generators")
               (:file "itertools")
               (:file "iterate")))


