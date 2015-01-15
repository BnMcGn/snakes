Pygen
=====

Python style generators for Common Lisp.  It includes yield and a port of itertools.

Generators
==========

Generators are functions that lazily return a sequence of items, one item per call. Eg:

    CL-USER> (defvar genr (list->generator '(1 2 3 4)))
    GENR
    CL-USER> (funcall genr)
    1
    CL-USER> (funcall genr)
    2
    CL-USER> (funcall genr)
    3

Generators retain state information between calls. They're useful for stepping through finite resources such as files and lists, but they can also be infinite, as when used for creating mathematical sequences.

Perhaps their most useful feature is an ability to be chained together, analogous to the way in which unix utilities can be piped together. They have a clean interface that encourages the creation of stackable, reusable tools.

While generators are not the most computationally efficient way to solve most problems, they're elegant in use and great for rapid prototyping.

Examples
========

Creating
--------
    >(defgenerator some-numbers ()
       (loop for i from 1 upto 3 do (yield i))
       (print 'some-message)
       (loop for i from 8 upto 9 do (yield i)))

Note: defgenerator doesn't create a generator. It defines a function that returns a new generator each time it is called. 

Consuming
---------

	> (mapc-generator #'print (some-numbers))
	1
	2
	SOME-MESSAGE
	3
	8
	9

Anonymous generator
-------------------
    (with-yield
      (dotimes (i 10)
        (yield (expt 2 i))))

    #<BASIC-GENERATOR {C0131CD}>

More consumption options
------------------------
    > (generator->list (with-yield
    		         (dotimes (i 10)
    			   (yield (expt 2 i)))))
    (1 2 4 8 16 32 64 128 256 512)

    > (take 3 (with-yield
                (dotimes (i 10)
	          (yield (expt 2 i)))))
    (1 2 4)

    > (do-generator (item (some-numbers))
                     (when (< 3 item)
                       (return item)))
    SOME-MESSAGE 
    8

Pipe-able generator
-------------------
    > (defgenerator add2 (some-gen)
         (do-generator (x some-gen)
           (yield (+ 2 x))))
    ADD2
    > (generator->list (add2 (some-numbers)))
    SOME-MESSAGE 
    (3 4 5 10 11)	      

Yield-all
---------
    > (defgenerator flatten (list)
        (when list
          (cond
            ((atom list)
             (yield list))
            (t
             (yield-all (flatten (car list)))
             (yield-all (flatten (cdr list)))))))
    > (mapc-generator #'print (flatten '((a (b c) d) (e) f (g h))))
    A
    B
    C
    D
    E
    F
    G
    H


Implementation 
==============

Like in python, pygen generators raise a signal - stop-iteration - when they terminate. Some lisp generator implementations use the second value to signal termination. Pygen is implemented with a signal stop to free up the value channels for user purposes.

Author
======

Ben McGunigle (bnmcgn at gmail.com)

Based on [this code](https://groups.google.com/forum/#!topic/comp.lang.lisp/2QK9yJGS_hk) by Frédéric Jolliton.

Copyright
=========

Copyright (c) 2014-2015 Ben McGunigle

License
=======

Apache License version 2.0
