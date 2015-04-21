;;;; snakes.asd

(asdf:defsystem #:snakes
  :serial t
  :description "Describe snakes here"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:cl-cont #:closer-mop #:fiveam #:iterate #:cl-utilities)
  :components ((:file "package")
	       (:file "util")
               (:file "snakes" :depends-on ("util"))
               (:file "do-generators")
               (:file "itertools")))

(asdf:defsystem #:snakes-iterate
  :serial t
  :description "Driver for snakes generator consumption in iterate macro."
  :depends-on (#:snakes #:iterate)
  :components ((:file "iterate")))


