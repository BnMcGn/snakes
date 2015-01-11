
(in-package :pygen)



(defun default-handler (spec)
  (let ((data (gensym)))
    (values data
	    (last-car spec)
	    (butlast spec) ;Var names
	    `(next-generator-value ,data))))

(defun list-handler (spec)
    (let ((data (gensym)))
      (values data ;Gensym for list storage
	      (last-car spec) ;List source form. Once-only-ed.
	      (butlast (cdr spec)) ;Var name - should only have one!
	      (list-update data)))) ;Form that returns item from list
  
;gensym sourceform vars update-form

(defun list-update (resource)
  `(if (consp ,resource)
       (values
	t
	(prog1
	    (car ,resource)
	  (setf ,resource (cdr ,resource))))
       (values nil nil)))

(defun iterate-clause (stor varnames update-form fill-val)
  `(destructuring-bind (sig . data)
       (multiple-value-list ,update-form)
     (if sig
	 (push (subseq data 0 ,(length varnames)) ,stor)
	 ,(if fill-val
	     `(push (list ,@(loop for i from 1 to (length varnames)
			       collect (car fill-val))) ,stor)
	     '(return))))) ;This item is finished. Terminate do-generators.

(defun iterate-clauses (stor varnameses update-forms fill-vals)
  (if (null varnameses)
      nil
      (cons 
       (iterate-clause 
	stor (car varnameses) (car update-forms) (car fill-vals))
       (iterate-clauses
       stor (cdr varnameses) (cdr update-forms) (cdr fill-vals)))))

(defun process-genspecs (genspecs)
  (with-collectors ((varnames) (fillvals) (orig-sources) 
		    (sources) (update-forms))
    (dolist (genspec genspecs)
      (multiple-value-bind (fill spec)
	  (extract-keywords '(:fill-value) genspec)
	(multiple-value-bind (source orig-source names update-form)
	    (if (eq :list (car spec))
		(list-handler spec)
		(default-handler spec))
	  (sources source)
	  (orig-sources orig-source)
	  (varnames names)
	  (update-forms update-form))
	(multiple-value-bind (sig filler)
	    (fill-info fill)
	  (if sig
	      (fillvals (list filler))
	      (fillvals nil)))))
    (values (varnames) (fillvals) (orig-sources) (sources) (update-forms))))

(defun var-clause (datavar vars body &optional (counter nil))
  (let ((counter (or counter (1- (length vars)))))
    (if vars
	(var-clause
	 datavar
	 (cdr vars)
	 `((destructuring-bind 
		 ,(car vars)
	       (elt ,datavar ,counter)
	     ,@body))
	 (1- counter))
	body)))

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
  (multiple-value-bind 
	(varnames fillvals orig-sources sources update-forms)
      (process-genspecs genspecs)
    (let ((stor (gensym)))
      `(let ((,stor nil)
	     ,@(loop for osource in orig-sources
		  for src in sources
		  collect (list src osource)))
	 (loop do 
	    (progn
	      (setf ,stor nil)
	      ,@(iterate-clauses stor varnames update-forms fillvals)
	      ,@(var-clause stor varnames body)))))))

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
	 do (multiple-value-bind
		  (sig vals) (funcall v g)
	      (if sig
		  (progn
		    (push sig sigs)
		    (push vals stor))
		  (sticky-stop))))
      (when
	  (= 0 (length (remove-if (lambda (x) (eq x :filled)) sigs)))
	(sticky-stop))
      (let ((tmp stor))
	(setf stor nil)
	(setf sigs nil)
	(values (nreverse tmp))))))
       