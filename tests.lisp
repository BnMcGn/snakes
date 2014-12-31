;;;; -*- lisp -*-

(in-package :pygen-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite pygen-test))

(in-suite pygen-test)

(defvar data '(1 2 3 4 5))

(test with-yield
  (is (functionp (with-yield 
		   (dolist (x data) (yield x)))))
  (is (= 1 (funcall
	    (with-yield
	      (dolist (x data) (yield x))))))
  (is (equal data
	     (generator->list
	      (with-yield 
		(dolist (x data) (yield x))))))
  (is (equal data
	     (generator->list
	      (with-yield
		(do-generator (x (list->generator data))
			     (yield x)))))))

(defgenerator some-numbers ()
  (loop for i from 1 upto 3 do (yield i))
  (dotimes (j 3) (yield (- 10 j))))

(test defgenerator
  (is (functionp (some-numbers)))
  (is (= 33 (reduce #'+ (generator->list (some-numbers))))))

(test multiple-value-generator
  (is (= 2
	 (do-generator
	     (a b (with-yield 
		    (dolist (x data)
		      (yield x (1+ x)))))
	   a
	   (return b)))))

(test mapcar-generator
  (is (= 39 (reduce #'+ (mapcar-generator #'1+ (some-numbers))))))

(defgenerator flatten (list)
  (when list
    (cond
      ((atom list)
       (yield list))
      (t
       (yield-all (flatten (car list)))
       (yield-all (flatten (cdr list)))))))

(test yield-all
  (is (equal '(a b c d e f g h)
	     (mapcar-generator 
	      #'identity 
	      (flatten '((a (b c) d) (e) f (g h)))))))

(test stop-iteration
  (signals stop-iteration 
    (funcall (list->generator nil))))


(test adaptors
  (is (equal '(#\a #\s #\d #\f)
	 (generator->list (sequence->generator "asdf"))))
  (is
   (equal '(1 2 3)
	  (generator->list
	   (value-func->generator
	    (let ((x 0))
	      (lambda ()
		(incf x)
		(if (< 3 x)
		    (values nil nil)
		    (values x t)))))))))