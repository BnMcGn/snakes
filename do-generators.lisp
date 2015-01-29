
(in-package :snakes)

(defun next-generator-value (gen)
  (let ((data (multiple-value-list (funcall gen))))
    (if (eq 'generator-stop (car data))
	(values nil nil)
	(apply #'values t data))))

(defun next-generator-value-filled (fill)
  "Returns a closure that acts like next-generator-value, but returns fill when the generator runs out. The :filled keyword instead of T in the first value 
slot still evaluates as true, but indicates that the generator has ended."
  (lambda (gen)
    (let ((x (multiple-value-list (next-generator-value gen))))
      (if (car x)
	  (apply #'values x)
	  (values :filled fill)))))

(defun fill-info (fillspec)
  (if fillspec
      (values t (cdr (assoc :fill-value fillspec)))
      (values nil nil)))

(defun get-generator (g)
  (if (atom g)
      g
      (car g)))

(defun get-nextval-func (genspec)
  (if (atom genspec)
      #'next-generator-value
      (multiple-value-bind
	    (sig val) (fill-info 
		       (extract-keywords '(:fill-value) genspec))
	  (if sig 
	      (next-generator-value-filled val)
	      #'next-generator-value))))
			  
(defun multi-gen (&rest genspecs)
  (let ((generators (mapcar #'get-generator genspecs))
	(valfuncs (mapcar #'get-nextval-func genspecs))
	(stor nil)
	(sigs nil))
    (gen-lambda-with-sticky-stop ()
      (loop for g in generators
	 for v in valfuncs
	 do (let ((data (multiple-value-list (funcall v g))))
	      (if (car data)
		  (progn
		    (push (car data) sigs)
		    (push (cdr data) stor))
		  (sticky-stop))))
      (when
	  (= 0 (length (remove-if (lambda (x) (eq x :filled)) sigs)))
	(sticky-stop))
      (let ((tmp stor))
	(setf stor nil)
	(setf sigs nil)
	(apply #'values (nreverse tmp))))))



(defun proc-list-spec (spec)
  (if (eq :list (car spec))
      (labels ((proc (rspec)
		 (when (atom rspec)
		   (error ":list spec needs at least a var a list"))
		 (if (or (keywordp (second rspec)) (not (cdr rspec)))
		     (cons `(list->generator ,(car rspec)) (cdr rspec))
		     (cons (car rspec) (proc (cdr rspec))))))
	(proc (cdr spec)))
      spec))

(defun process-genspecs (genspecs)
  (with-collectors ((varnames<) (short-specs<))
    (dolist (genspec genspecs)
      (multiple-value-bind (fill spec)
	  (extract-keywords '(:fill-value) genspec)
	(varnames< (butlast spec))
	(short-specs< 
	 (if fill
	     (list
	      'list
	      (last-car spec)
	      :fill-value
	      (cdr (assoc :fill-value fill)))
	     (last-car spec)))))
    (values (varnames<) (short-specs<))))

(defun bind-clause (datavar varnames body)
  `(let
     ,(with-collectors (col<)
	  (loop for vnames in varnames
	     for i from 0
	     do (loop for vn in vnames
		   for j from 0
		     do
		       (col< `(,vn (elt (elt ,datavar ,i) ,j)))))
	  (col<))
     ,@body))
	    
  
(defmacro do-generators (genspecs &body body)
  "Simultaneously iterate through multiple generators at once. By default 
do-generators terminates when the shortest generator ends, but individual 
generators can be padded out with the :fill-value keyword. Do-generators can 
also iterate over lists in parallel with generators. To use lists, start the genspec with the :list keyword. Eg:
  (do-generators ((a (repeat 2))
			 (:list b '(4 5 6))
			 (c (list->generator '(8 9)) :fill-value 30))
	   (print (* a (+ b c))))
24 
28 
72 
NIL"
  (let ((genspecs (mapcar #'proc-list-spec genspecs))
	(genr (gensym))
	(data (gensym))
	(sig (gensym)))
    (multiple-value-bind (varnames specs)
	(process-genspecs genspecs)
      
      `(let ((,genr (multi-gen ,@specs)))
	 (loop do
	   (destructuring-bind (,sig . ,data) 
	       (multiple-value-list (next-generator-value ,genr))
	     (unless ,sig
	       (return))
	     ,(bind-clause data varnames body)))))))
