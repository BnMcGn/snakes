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
   #:do-generator-value-list
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
   #:next-generator-value
   ;Itertools
   #:icount
   #:cycle
   #:repeat
   #:chain
   #:izip
   #:izip-longest
   #:compress
   #:dropwhile
   #:takewhile
   #:groupby
   #:ifilter
   #:ifilter-false
   #:islice
   #:imap
   #:starmap
   #:tee))

(defpackage #:pygen-test
  (:use #:cl #:pygen #:fiveam))