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
   ;Adaptors
   #:function->generator
   #:value-func->generator
   #:list->generator
   #:list->generator-with-tail
   #:generator->list
   #:sequence->generator))

(defpackage #:pygen-test
  (:use #:cl #:pygen #:fiveam))