;;;; util.lisp

(in-package #:snakes)

(defmacro def-as-func (var func-form)
  `(setf (symbol-function ',var) ,func-form))

(defun sequence->list (seq)
  (loop for x across seq
       collect x))

(defun fetch-keyword (key alist &key (parameter t) (checklist t))
  "Find if a key is in a list. If parameter is true, return the next item 
  after it. if checklist is true, test the first element of any sublists for the   key and if found return rest of list as parameter."
  (let ((keytest (if checklist 
		      (lambda (x y)
			(or (eql x y)
			    (and (consp y) (eql x (car y)))))
		      #'eql)))
    (aif (loop for x on alist
       do (when (funcall keytest key (car x))
	    (return (list t (if (atom (car x)) (second x) (cdar x))))))
	 (if parameter (values-list it) t)
	 nil)))

(defun xsubseq (sequence start end &key (type 'sequence)) 
  "Returns sequence with start->end chopped out of it"
  (concatenate type
   (subseq sequence 0 start)
   (subseq sequence (1+ end))))

;removes found keywords from list, returning cleaned list as second val
(defun extract-keywords (keywords alist &optional stack)
  (if keywords
      (let ((pos (position (car keywords) alist)))
	(extract-keywords 
	   (cdr keywords)
	   (if pos
	       (xsubseq alist pos (1+ pos) :type 'list)
	       alist)
	   (if pos
	       (acons (car keywords) (elt alist (1+ pos)) stack)
	       stack)))
      (values stack alist)))
  

(defun last-car (list)
  (car (last list)))

(defmacro multiple-value-destructure (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list (multiple-value-list ,expression)
     ,@body))

(defun keyword-splitter (data &key (flatten t))
  (if data
      (cond ((and (consp (car data)) (keywordp (caar data))
		 (if flatten
		     (cons 
		      (car data) 
		      (keyword-splitter (cdr data) :flatten flatten))
		     (cons
		      (list (car data))
		      (keyword-splitter (cdr data) :flatten flatten)))))
	    ((keywordp (car data))
	     (multiple-value-bind 
		   (this next) 
		 (divide-list 
		  (cdr data) 
		  (lambda (x) (or (keywordp x)
				  (and (consp x) (keywordp (car x))))))
	       (cons (cons (car data) this) 
		     (keyword-splitter next :flatten flatten))))
	    (t (error "Missing keyword")))))



(defun keyword-value (key alist)
  (if alist
      (if (eq key (car alist))
	  (if (> (length alist) 1)
	      (values (second alist) t)
	      (values nil nil))
	  (keyword-value key (cdr alist)))
      (values nil nil)))

(defun set-keyword-value (key alist newval)
  (if alist
      (if (eq key (car alist))
	  (if (> (length alist) 1)
	      (setf (car (cdr alist)) newval)
	      (setf (cdr alist) (cons newval nil)))
	  (if (null (cdr alist))
	      (setf (cdr alist) (list key newval))
	      (set-keyword-value key (cdr alist) newval)))
      (error "Can't set nil")))

(defsetf keyword-value set-keyword-value)

(defmacro with-any/all/none (&body body)
  (let ((name (gensym)))
    `(block ,name
       (labels ((returner (rval) (return-from ,name rval)))
	 (macrolet ((any (test &optional (retval t))
		      `(when ,test (returner ,retval)))
		    (all (test &optional retval)
		      `(when (not ,test) (returner ,retval)))
		    (none (test &optional retval)
		      `(when ,test (returner ,retval))))
	   ,@body)))))



(defmacro do-file-by-line ((line stream-or-path) &body body)
  (let ((stream (gensym)))
  `(cond 
     ((pathnamep ,stream-or-path)
      (with-open-file (,stream ,stream-or-path)
	(loop 
	   for ,line = (read-line ,stream nil 'eof)
	   until (eq ,line 'eof)
	   do (progn ,@body))))
     ((streamp ,stream-or-path)
      (loop
	   for ,line = (read-line ,stream-or-path nil 'eof)
	   until (eq ,line 'eof)
	   do (progn ,@body)))
     (t (error "Not a stream or path!")))))

(defmacro do-list-with-rest ((head tail source) &body body)
  (once-only (source)
    `(let ((,head nil))
       (loop for ,tail on ,source
	  do (prog1
		 ,@body
	       (push (car ,tail) ,head))))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

