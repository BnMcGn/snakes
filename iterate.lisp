
(defpackage #:snakes-iterate
  (:use #:cl #:iterate)
  (:export
   #:in-generator))

(in-package :snakes-iterate)

(defmacro-driver (FOR var IN-GENERATOR gen)
  "Iterate through a snakes generator"
  (let ((g (gensym))
	(tmp (gensym))
	(kwd (if generate 'generate 'for)))
    `(progn
       (with ,g = ,gen)
       (,kwd ,var next (snakes::if-generator (,tmp ,g)
				     ,tmp
				     (terminate))))))



