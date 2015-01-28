;;;; pygen.asd

(asdf:defsystem #:pygen
  :serial t
  :description "Describe pygen here"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:arnesi #:closer-mop #:fiveam)
  :components ((:file "package")
	       (:file "util")
               (:file "pygen" :depends-on ("util"))
               (:file "do-generators")
               (:file "itertools")))

(asdf:defsystem #:pygen-iterate
  :serial t
  :description "Driver for pygen generator consumption in iterate macro."
  :depends-on (#:pygen #:iterate)
  :components ((:file "iterate")))


