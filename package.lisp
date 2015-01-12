;;;; package.lisp

(defpackage #:pygen
  (:use #:cl #:arnesi)
  (:export
   #:stop-iteration
   #:with-yield
   #:yield
   #:defgenerator
   #:basic-generator
   #:generatorp
   #:gen-lambda
   #:do-generator
   #:mapc-generator
   #:mapcar-generator
   #:yield-all
   #:take
   #:do-generators
   #:multi-gen
   ;Adaptors
   #:function->generator
   #:value-func->generator
   #:list->generator
   #:list->generator-with-tail
   #:generator->list
   #:sequence->generator
   ;Construction tools
   #:gen-lambda-with-sticky-stop
   #:sticky-stop
   #:next-generator-value))

(defpackage #:pygen-test
  (:use #:cl #:pygen #:fiveam))