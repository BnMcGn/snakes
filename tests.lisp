;;;; -*- lisp -*-

(defpackage #:snakes-test
  (:shadowing-import-from #:snakes #:repeat)
  (:use #:cl #:snakes #:fiveam #:iterate #:snakes #:snakes-iterate)
  

(in-package :snakes-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite snakes-test))

(in-suite snakes-test)

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

(test generator-stop
  (eq 'generator-stop
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

(test do-generators
  (is (= 40 
	 (reduce #'+ 
		 (generator->list 
		  (with-yield 
		    (do-generators ((g (some-numbers))
				    (:list l data))
		      (yield (+ l g))))))))
  (is (= 48
	 (reduce #'+ 
		 (generator->list 
		  (with-yield 
		    (do-generators ((g (some-numbers))
				    (:list l data :fill-value 0))
		      (yield (+ l g)))))))))

(test consume
  (is (= 3
	 (let ((gen (some-numbers)))
	   (consume 2 gen)
	   (funcall gen)))))

(test icount
  (is (equal '(10 12 14 16)
	     (take 4
		   (icount 10 2)))))

(test cycle
  (is (= 163
       (reduce #'+
	       (take 30 
		(cycle '(4 5 6 7)))))))

(test repeat
  (is (= 20
	 (reduce #'+
		 (generator->list
		  (repeat 2 10))))))

(test chain
  (is (= 66
       (reduce #'+
	       (generator->list
		(chain (some-numbers) (some-numbers)))))))

(test izip-longest
  (is (= 179
	 (let ((x 0))
	   (do-generator (a b 
			    (izip-longest 
			     (list->generator data) 
			     (some-numbers) :fill-value 10))
	     (incf x (* a b)))
	   x))))

(test compress
  (is (equal '(a c e f)
	     (generator->list
	      (compress (list->generator '(a b c d e f))
			(list->generator '(t nil t nil t t)))))))

(test dropwhile
  (is (equal '(6 4 1)
	     (generator->list
	      (dropwhile (lambda (x) (< x 5))
			 (list->generator '(1 4 6 4 1)))))))

(test takewhile
  (is (equal '(1 4)
	     (generator->list
	      (takewhile (lambda (x) (< x 5))
			 (list->generator '(1 4 6 4 1)))))))

(test groupby
  (is (= 4
	 (length (car 
	   (nth-value 1 (take 1 
			      (groupby 
			       (list->generator '(A A A A B B B C C))))))))))

(test ifilter
  (is (= 20
	 (reduce #'+ (generator->list
		      (ifilter #'evenp (some-numbers))))))
  (is (= 13
	 (reduce #'+ (generator->list
		      (ifilter-false #'evenp (some-numbers)))))))

(test islice
  (is (equal '(a b)
	     (generator->list (islice (list->generator '(a b c d e f g)) 2))))
  (is (equal '(c d)
	     (generator->list (islice (list->generator '(a b c d e f g)) 2 4))))
  (is (equal '(c d e f g)
	     (generator->list 
	      (islice (list->generator '(a b c d e f g)) 2 nil))))
  (is (equal '(a c e g)
	     (generator->list
	      (islice (list->generator '(a b c d e f g)) 0 nil 2)))))

(test imap 
  (is (equal '(8 15 24)
	 (generator->list 
	  (imap #'* (list->generator  '(2 3 4)) (list->generator '(4 5 6)))))))

(test starmap
  (is (equal '(32 9 1000)
	 (generator->list (starmap #'expt (list->generator 
					   '((2 5) (3 2) (10 3))))))))

(test tee
  (is (= 33
	 (reduce #'+
		 (multiple-value-bind
		       (a b) (tee (some-numbers) 2)
		   (consume 6 a)
		   (generator->list b))))))

(test product
  (is (equal '(1 1 1 1 1 2)
	     (take 6
		   (product data data)))))

(test permutations
  (is (equal '((1 2))
	     (take 1
		   (let ((*snakes-multi-mode* :list))
		     (permutations data 2))))))

(test combinations-with-replacement
  (is (= 6 
	 (length (nth-value 1
			    (generator->list
			     (combinations-with-replacement
			      '(2 3 4) 2)))))))

(test combinations
  (is (= 3 (length (nth-value 1
			      (generator->list
			       (combinations
				'(2 3 4) 2)))))))

(test iterate
  (is (equal data (generator->list 
		   (with-yield 
		     (iterate:iterate (for i in data)
		        (yield i))))))
  (is (= 33 (iterate (for (x) in-generator (some-numbers))
		  (summing x))))
  (is (= 39 
	 (reduce #'+
	   (generator->list
	    (with-yield
	      (iterate (for (x) in-generator (some-numbers))
		       (yield (1+ x)))))))))

