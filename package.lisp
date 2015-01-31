;;;; package.lisp

(defpackage #:snakes
  (:use #:cl #:arnesi)
  (:export
   #:generator-stop
   #:with-yield
   #:yield
   #:defgenerator
   #:basic-generator
   #:generatorp
   #:do-generator
   #:do-generator-value-list
   #:mapc-generator
   #:mapcar-generator
   #:yield-all
   #:take
   #:consume
   #:do-generators
   #:multi-gen
   #:*snakes-multi-mode*
   ;Adaptors
   #:function->generator
   #:value-func->generator
   #:list->generator
   #:list->generator-with-tail
   #:generator->list
   #:sequence->generator
   #:file->generator
   ;Construction tools
   #:gen-lambda
   #:gen-lambda-with-sticky-stop
   #:sticky-stop
   ;Itertools
   #:icount
   #:cycle
   #:repeat
   #:chain
   #:enumerate
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
   #:tee
   #:product
   #:permutations
   #:combinations
   #:combinations-with-replacement))

(defpackage #:snakes-test
  (:use #:cl #:snakes #:fiveam))

