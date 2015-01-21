;;;; pygen.asd

(asdf:defsystem #:pygen
  :serial t
  :description "Describe pygen here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:arnesi #:closer-mop #:fiveam)
  :components ((:file "package")
	       (:file "util")
               (:file "pygen" :depends-on ("util"))
               (:file "do-generators")
               (:file "itertools")))


