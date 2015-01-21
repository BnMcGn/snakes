;;;; pygen.lisp

(in-package #:pygen)

;;; "pygen" goes here. Hacks and glory await!

(define-condition stop-iteration (condition)
  ()
  (:documentation "Generators send this to signal that they have reached their end"))

(define-condition pygen-error (error) ())

(define-condition insufficient-items (pygen-error) ()
  (:documentation "Generator has run out of items before expected."))

(defparameter *pygen-multi-mode* :values)

(defclass basic-generator ()
     ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((g basic-generator) &key function) 
  (with-slots () g 
    (when function
      (closer-mop:set-funcallable-instance-function g function))))

(defgeneric generatorp (obj))
(defmethod generatorp ((obj t)) nil)
(defmethod generatorp ((obj basic-generator)) t)

(defmacro gen-lambda (params &body rest)
  "Version of lambda that wraps the anonymous function in an instance of the basic-generator class. This allows the lambda to be identified as a generator by its type. Basic-generator objects are derived from funcallable-standard-class, therefore they can be called as functions."
  `(let ((values-handler (create-values-handler)))
     (make-instance 
      'basic-generator 
      :function (lambda ,params ,@rest))))

(defun create-values-handler ()
  "Returns a function to transform ready-to-be-yielded values according to *pygen-multi-mode*. This should be set up at generator creation time, not at consumption time, so that generators in a chain can be set individually."
  (case *pygen-multi-mode*
    (:values #'identity)
    (:list #'list)
    (otherwise
     (error "*pygen-multi-mode* setting not supported"))))

(defmacro with-yield (&body body)
  (let ((point (gensym))
        (current (gensym)))
    `(let (,point ,current)
       (with-call/cc
	 (labels ((yield (&rest values)
		    (setf ,current values)
		    (let/cc k
		      (setf ,point k))))
	   ,@body
	   (setf ,point nil)))
       (gen-lambda ()
	 (cond ((null ,point)
		(signal 'stop-iteration))
	       (t
		(let ((current ,current))
		  (kall ,point nil)
		  ;values-handler: see gen-lambda
		  (apply #'values (funcall values-handler current)))))))))

(defmacro defgenerator (name arguments &body body)
  `(defun ,name ,arguments
     (with-yield
       ,@body)))

(defun next-generator-value (gen)
  "This function exists because handler-case can't be used in code that might
end up being used under with-call/cc, such as code under with-yield"
  (handler-case
      (apply #'values t (multiple-value-list (funcall gen)))
    (stop-iteration ()
      (values nil nil))))

(defmacro do-generator ((var &rest vars-and-generator) &body body)
  (let ((g (gensym))
	(sig (gensym)))
    `(let ((,g ,(car (last vars-and-generator))))
       (loop 
	  do (multiple-value-bind 
		   (,sig ,var ,@(butlast vars-and-generator)) 
		 (next-generator-value ,g)
	       (unless ,sig
		 (return))
	       ,@body)))))

(defmacro do-generator-value-list ((var generator) &body body)
  "Like do-generator, except all values emitted by the generator are placed in a list under the user defined variable."
  (let ((g (gensym))
	(sig (gensym)))
    `(let ((,g ,generator))
       (loop 
	  do (multiple-value-destructure (,sig . ,var)
		 (next-generator-value ,g)
	       (unless ,sig
		 (return))
	       ,@body)))))

(defun mapc-generator (function generator)
  (do-generator (item generator)
    (funcall function item))
  (values))

(defun mapcar-generator (function generator)
  (let ((result ()))
    (do-generator (item generator)
      (push (funcall function item) result))
    (nreverse result)))

(defmacro yield-all (generator)
  "Walks through generator, re-yielding all of the items in it. Only meant for use inside of a defgenerator or with-yield block."
  (let ((x (gensym)))
    `(do-generator (,x ,generator)
       (yield ,x))))


(defmacro sticky-stop-lambda (params &body body)
  "Once a generator function has signalled a stop-iteration, it should continue to do so every time it is called. This macro helps with obeying that rule." 
  (let ((state (gensym)))
    `(let ((,state t))
       (lambda ,params
       (if ,state
	   (labels ((sticky-stop ()
		      (setf ,state nil)
		      (signal 'stop-iteration)))
	     ,@body)
	   (signal 'stop-iteration))))))

(defmacro gen-lambda-with-sticky-stop (&rest rest)
  `(make-instance 
    'basic-generator 
    :function (sticky-stop-lambda ,@rest)))

(defun take (n gen &key (fail-if-short t))
  "Takes n items from generator gen, returning them as a list. If the generator  contains less than n items, take will return what is available if fail-if-short is set to nil, otherwise it will raise an error."
  (let ((stor (multiple-value-destructure (sig . rest) 
		  (next-generator-value gen)
		(when sig
		  (mapcar #'list rest)))))
    (when stor
      (handler-case
	  (dotimes (i (1- n))
	    (loop for val in (multiple-value-list (funcall gen))
		 for j from 0
		 do (push val (elt stor j))))
	(stop-iteration ()
	  (if fail-if-short
	      (error 'insufficient-items "Insufficient items in generator")
	      (apply #'values (mapcar #'reverse stor)))))
      (apply #'values (mapcar #'nreverse stor)))))
  
(defun consume (n gen &key (fail-if-short t))
  "Takes N items from generator gen, returning nothing. If the generator contains less than n items, consume will empty the generator silently if fail-if-short is set to nil, otherwise it will raise an error."
  (handler-case
      (dotimes (i n)
	 (funcall gen))
      (stop-iteration ()
	(if fail-if-short
	    (error 'insufficient-items "Insufficient items in generator")
	    nil)))
    nil)

;;;;;;;;;;;;;;;;;;;;;;;
;Adaptors
;;;;;;;;;;;;;;;;;;;;;;;

(defun function->generator (source-func predicate)
  "Returns a generator that repeatedly calls source-func, tests the result against the predicate function, terminates when the predicate tests false, otherwise yielding the result"
    (gen-lambda-with-sticky-stop ()  
      (let ((res (funcall source-func)))
	(if (funcall predicate res)
	    res
	    (sticky-stop)))))

(defun value-func->generator (source)
  "Converts to a generator that sort of function which uses the second value as
a completion signal. Will keep emitting the first return value of the source function until the second value is a nil."
  (gen-lambda-with-sticky-stop ()
    (multiple-value-bind (res signal) (funcall source)
      (if signal
	  res
	  (sticky-stop)))))

(defun list->generator (source)
  (let ((data source))
    (gen-lambda ()
      (if (not data)
	  (signal 'stop-iteration)
	  (prog1
	      (car data)
	    (setf data (cdr data)))))))

(defun list->generator-with-tail (source)
  (let ((data source))
    (gen-lambda ()
      (if (not data)
	  (signal 'stop-iteration)
	  (prog1
	      data
	    (setf data (cdr data)))))))

(defun generator->list (gen)
  (let ((stor (multiple-value-destructure (sig . rest) 
		  (next-generator-value gen)
		(when sig
		  (mapcar #'list rest)))))
    (do-generator-value-list (g gen)
      (setf stor
	    (loop for pile in stor
		 for new in g
		 collect (cons new pile))))
    (apply #'values (mapcar #'nreverse stor))))

(defun sequence->generator (seq)
  (let ((seqlen (length seq))
	(i 0))
    (gen-lambda ()
      (if (> i (1- seqlen))
	  (signal 'stop-iteration)
	  (prog1
	      (elt seq i)
	    (incf i))))))
;;;Ideas:

;adaptors for SERIES, others?
;file to generator
;port itertools, my tools, perhaps pytoolz
