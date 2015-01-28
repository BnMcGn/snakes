
(defpackage #:pygen-iterate
  (:use #:cl #:iterate)
  (:export
   #:in-generator))

(defmacro-driver (FOR var IN-GENERATOR gen)
  "Iterate through a pygen generator"
  (let ((g (gensym))
	(tmp (gensym))
	(kwd (if generate 'generate 'for)))
    `(progn
       (with ,g = ,gen)
       (,kwd ,var next (pygen::if-generator (,tmp ,g)
				     ,tmp
				     (terminate))))))



