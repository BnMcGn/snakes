;;;; util.lisp

(in-package #:snakes)

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

