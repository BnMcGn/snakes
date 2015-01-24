
(in-package :pygen)

(defgenerator icount (n &optional (step 1))
  "Make a generator that returns evenly spaced values starting with n. Step can be fractional. Eg:
   (counter 10) -> 10 11 12 13 14 ...
   (counter 2.5 0.5) -> 2.5 3.0 3.5 ..."
  (unless (numberp n)
    (error "n must be a number"))
  (unless (numberp step)
    (error "step must be a number"))
  (loop for i from n by step
     do (yield i)))

(defgenerator cycle (list-or-gen)
  "Repeatedly cycles through a list or generator, yielding its elements indefinitely. Eg:
   (cycle '(a b c d)) -> a b c d a b c d a b ...
Note that this tool may consume considerable storage if the source iterable is long."
  (let ((data 
	 (cond 
	   ((generatorp list-or-gen)
	    (let ((accum nil))
	      (do-generator (g list-or-gen)
		(yield g)
		(push g accum))
	      (nreverse accum)))
	   ((listp list-or-gen) list-or-gen)
	   (t (error "Gen must be a list or generator")))))
    (loop do
	 (dolist (x data)
	   (yield x)))))

(defgenerator repeat (item &optional n)
  "Returns a single item indefinitely, or up to n times."
  (if n
      (dotimes (i n)
	(yield item))
      (loop do (yield item))))

(defgenerator chain (&rest generators)
  "Chains a number of generators together head to tail. Eg:
   (chain (list->generator '(1 2 3) (list->generator '(6 7))) -> 1 2 3 6 7"
  (dolist (g generators)
    (yield-all g)))

(defgenerator izip (&rest generators)
  "Steps through multiple generators in parallel, emitting each of their items as values. Stops with the end of the shortest source generator."
  (do-generator-value-list (vars (apply #'multi-gen generators))
    (apply #'yield (mapcar #'car vars))))

(defgenerator izip-longest (&rest generators-and-fill-value)
  "Steps through multiple generators in parallel, emitting each of their items as values. Keeps going until the end of the longest source generator, padding the rest out with the value specified by :fill-value"
  (multiple-value-bind (keywords gens)
      (extract-keywords '(:fill-value) generators-and-fill-value)
    (let ((fillspec (if keywords
			(cdr (assoc :fill-value keywords))
			nil)))
      (do-generator-value-list 
	  (vars
	   (apply #'multi-gen
		  (loop for g in gens
		       collect (list g :fill-value fillspec))))
	(apply #'yield (mapcar #'car vars))))))

(defun enumerate (generator)
  (izip (icount 0) generator))
 
(defgenerator compress (data selectors)
  "Moves through the data and selectors generators in parallel, only yielding elements of data that are paired with a value from selectors that evaluates to true. Eg:
   (compress (list->generator '(a b c d e f))
             (list->generator '(t nil t nil t t))) -> a c e f"
  (do-generator (d s (izip data selectors))
    (when s
      (yield d))))

(defgenerator dropwhile (predicate generator)
  "Goes through generator, dropping the items until the predicate fails, then emits the rest of the source generator's items. Eg:
   (dropwhile (lambda (x) (< x 5))
     (list->generator '(1 4 6 4 1))) -> 6 4 1"
  (do-generator (item generator)
    (unless (funcall predicate item)
      (yield item)
      (return)))
  (yield-all generator))

(defgenerator takewhile (predicate generator)
  "Emits items from generator until predicate fails. Eg:
   (takewhile (lambda (x) (< x 5))
     (list->generator '(1 4 6 4 1))) -> 1 4"
  (do-generator (item generator)
    (unless (funcall predicate item)
      (return))
    (yield item)))

;Differs from python version on some points.
(defgenerator groupby (generator &key (key #'identity) (comp #'eq))
  "Groups the items from generator by the results of the key func. Each item emitted by groupby will be a pair: the key value, followed by a list of items that produced the key value when passed to the key function. Note: groupby does NOT sort the source generator. In other words it will only group matching items that are adjacent. To get SQL style GROUP BY functionality, sort the source generator before passing it to groupby. Eg:
   (groupby (list->generator '(A A A A B B B C C))) ->
     (A (A A A A)) (B (B B B)) ..."
  (let ((last (gensym))
	(res nil))
    (do-generator (g generator)
      (let ((currkey (funcall key g)))
	(if (funcall comp last currkey)
	    (push g res)
	    (progn
	      (when res (yield last (nreverse res)))
	      (setf last currkey)
	      (setf res (cons g nil))))))))

(defgenerator ifilter (predicate generator)
  "Emits only those items in generator that are true by predicate. The generator equivalent of remove-if-not"
  (do-generator (g generator)
    (when (funcall predicate g)
      (yield g))))

(defgenerator ifilter-false (predicate generator)
  "Emits only those items in generator that are false by predicate. The generator equivalent of remove-if"
  (do-generator (g generator)
    (unless (funcall predicate g)
      (yield g))))

(defgenerator islice 
    (generator stop-or-start 
	       &optional (stop nil has-stop-p) step)
"Emits a subrange of generator. If only stop-or-start is set, islice will emit up to it and stop. If both stop-or-start and stop are set, islice will emit the stretch between stop and start. If step is set, then islice will emit every step-th item between start and stop. If stop is set to nil, then islice will continue through the source generator until it terminates. Eg:
   (islice (list->generator '(a b c d e f g)) 2) -> A B
   (islice (list->generator '(a b c d e f g)) 2 4) -> C D
   (islice (list->generator '(a b c d e f g)) 2 nil) -> C D E F G
   (islice (list->generator '(a b c d e f g)) 0 nil 2) -> A C E G"
  (let 
      ((start (if has-stop-p stop-or-start nil))
       (stop (or stop (unless has-stop-p stop-or-start)))
       (step (or step 1)))
    (when start
      (consume start generator))
    (if stop
	(dotimes (i (floor (/ (- stop (or start 0)) step)))
	  (let ((data (take step generator :fail-if-short nil)))
	    (unless data (return))
	    (yield (car data))))
	(loop do
	     (let ((data (take step generator :fail-if-short nil)))
	       (unless data (return))
	       (yield (car data)))))))

(defgenerator imap (function &rest generators)
  "The generator equivalent of the mapcar function. Applies function to the 
values of the supplied generators, emitting the result. Eg:
   (imap #'* (list->generator  '(2 3 4)) (list->generator '(4 5 6))) -> 8 15 24"
  (do-generator-value-list (vals (apply #'multi-gen generators))
    (yield (apply function (mapcar #'car vals)))))

(defgenerator starmap (function generator)
  "Sequentially applies the function to the output of the generator. Like imap, but assumes that the contents of the generator are already merged. Eg:
   (starmap #'expt (list->generator '((2 5) (3 2) (10 3)))) -> 32 9 1000"
  (do-generator (val generator)
    (yield (apply function val))))

(defun append! (lst obj)
  (setf (cdr (last lst)) obj))
      
(defun tee (generator &optional (n 2))
  "Creates independent copies of generator, returned as values. If the child generators are consumed at different times, tee will store all of the items from the least consumed child generator through to the most. It can, if used incautiously, require considerable memory. 

Note also that this implementation of tee does not create independent copies of the parent items. Modifying items from a child generator and tampering with the parent generator have undefined consequences."
  (let ((stor (cons :x nil)) ;First nil is a dummy value
	(stop-marker (gensym)))
    (labels ((get-next ()
	       (let* ((data (multiple-value-list 
			    (next-generator-value generator)))
		      (curr (if (car data)
				(cons (cdr data) nil)
				(cons stop-marker nil))))
		 (append! stor curr)
		 (setf stor curr))))
      (with-collectors (gens<)
	(dotimes (i n)
	  (gens<
	   (let ((ldata stor))  
	     (gen-lambda-with-sticky-stop ()
	       (unless (cdr ldata)
		 (get-next))
	       (setf ldata (cdr ldata))
	       (if (eq (car ldata) stop-marker)
		   (sticky-stop)
		   (apply #'values (ensure-list (car ldata))))))))
	(apply #'values (gens<))))))

(defgenerator product (&rest lists)
"Iterates through each of the supplied lists with nested dolists, yielding an element from each for every iteration of the innermost loop. The yielded values cycle in an odometer-like fashion, with the first value changing seldom and the last changing on every yield. Eg:
  (product '(a b c d) '(x y)) -(values)-> (a x) (a y) (b x) (b y) (c x) (c y)...
"
  (labels ((listholder (lists)
	     (if (null lists)
		 (lambda (stack)
		   (apply #'yield (reverse stack)))
		 (let ((nextfunc
			(listholder (cdr lists))))
		   (lambda (stack)
		     (dolist (itm (car lists))
		       (funcall nextfunc (cons itm stack))))))))
    (funcall (listholder lists) nil)))

(defgenerator permutations (list &optional r)
  "Emits every possible permutation of the items in list in a set of size r. If r is not specified, the size of list is used. Eg:
   (permutations '(a b c d) 2) -(values)-> (a b) (a c) (a d) (b a) (b c) (b d) 
 (c a) (c b) (c d) (d a) (d b) (d c)"
  (let ((r (or r (length list))))
    (labels ((proc (data output)
	       (if (= r (length output))
		   (apply #'yield (reverse output))
		   (dotimes (i (length data))
		     (proc (xsubseq data i i :type 'list)
			   (cons (elt data i) output))))))
      (proc list nil))))

(defgenerator combinations (list r)
  "Emits every possible combination of the items in list in sets of size r. Eg:
   (combinations '(a b c d) 2) -(values)-> (a b) (a c) (a d) (b c) (b d) (c d)"
  (if (< (length list) r)
    (lambda () (signal 'stop-iteration))
    (labels ((proc (data output r)
	       (if (= 0 r)
		 (apply #'yield (reverse output))
		 (loop for i from 0 to r
		    for d on data
		    do (proc (cdr d)
			     (cons (car d) output)
			     (1- r))))))
      (proc list nil r))))

(defgenerator combinations-with-replacement (list r)
  "Emits every possible combination, including repetitions, of the items in list in sets of size r. Eg: 
   (combinations-with-replacement '(a b c) 2) -(values)-> (a a) (a b) (a c) 
 (b b) (b c) (c c)"
  (labels ((proc (data output r)
	     (if (= 0 r)
		 (apply #'yield (reverse output))
		 (loop for d on data
		      do (proc d
			       (cons (car d) output)
			       (1- r))))))
    (if (and (length list) (<= r (length list) ))
	(proc list nil r)
	(lambda () (signal 'stop-iteration)))))