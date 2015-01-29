Pygen
=====

Python style generators for Common Lisp.  Includes a port of itertools.

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

Homepage
========

Pygen can be found here: [https://github.com/BnMcGn/pygen](https://github.com/BnMcGn/pygen)

Installation
============

Copy the files to your-local-quicklisp-project-dir/pygen and load with `(ql:quickload 'pygen)`.

Examples
========

Creating
--------
    > (defgenerator some-numbers ()
        (loop for i from 1 upto 3 do (yield i))
        (print 'some-message)
        (loop for i from 8 upto 9 do (yield i)))

When the generator is first called, it commences execution at the beginning of its body. When a yield call is reached, the generator returns the yielded value, surrendering execution to the calling code. The next time the generator is called, it continues execution just past the yield where it last stopped. If or when execution reaches the end of the block, the generator terminates.
 
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
    > (with-yield
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

Yield-all should only be used inside of a defgenerator or with-yield block. It expects a generator, which it consumes and emits through its enclosing generator.

More generator examples can be found in itertools.lisp. Documentation for the original python itertools is [here](https://docs.python.org/2/library/itertools.html)

For Python Programmers
======================

Examples
--------

Seeing that pygen was created to make the python emigré (author included) more comfortable in Common Lisp, here are some transitional examples:

Python: 

    (x/2 for x in iter)

Pygen:
    
    (with-yield
      (do-generator (x iter)
        (yield (/ x 2))))

Pygen's goal is to recreate the concept of python generators while staying within the bounds of lisp idiom convention. Its goal is not to look like python, so it doesn't implement for... in style syntax. Not to say it can't be done: see [ergolib](https://github.com/rongarret/ergolib).

Python:

    [x/2 for x in iter]

Pygen:

    (collecting
      (do-generator (x iter)
        (collect (/ x 2))))

Don't use generators if you want to immediately build a list. You're better off using either the collect keyword from the loop macro or one of the collectors macros. Collecting, above, comes from the [cl-utilities](http://common-lisp.net/project/cl-utilities/doc/) which also has a nice, simple with-collectors macro. The [arnesi](http://www.common-lisp.net/project/bese/arnesi.html) library has a more powerful with-collectors, plus some interesting alternatives.

Python:

    for i, val in enumerate(iter):
    	...

Pygen:
    
    (do-generator (i val (enumerate iter))
      ...

Python:

    ([x for x in y] for y in [[1, 2, 3], [5, 4]] if pred(x))

Pygen:

    (with-yield
      (loop for y in '((1 2 3) (5 4))
        do (yield 
             (loop for x in y	
             	when (funcall pred x)
 	       	  collect x))))

Pygen yield works with loop, do, dolist and recursive iteration.

About values
------------

Common Lisp has a feature called values (see the values function and stuff that starts multiple-value-) that is not found in python. It is the return value analogue of optional parameters. 

For example, the floor function:

    > (floor 2.56)
    2
    0.55999994

It returns both the integer value that you would expect, plus the fractional portion of the number as the second value. In normal operation that second value will be discarded. To access it, something like multiple-value-bind, multiple-value-list or nth-value is needed. 

Pygen generators are multiple-values capable. If yield is called with multiple parameters, it will by default emit them as separate values. This behavior can be modified with the `*pygen-multi-mode*` variable. Its default setting is :values. Set it to :list during generator *creation* for more python-like behavior in generators like izip, permutations, and combinations.

    > (generator->list (combinations '(1 2 3) 2))
    (1 1 2)
    (2 3 3)
    > (generator->list (let ((*pygen-multi-mode* :list))
                         (combinations '(1 2 3) 2)))
    ((1 2) (1 3) (2 3))

Yield as a function
-------------------

In pygen, yield is a locally defined function, not a keyword. It can only be called from within a with-yield or defgenerator block. Unlike the python version, it can be called from functions defined with the block, even when those functions are stacked a few layers deep in recursion. For an example see the definition of products, permutations, combinations or combinations-with-replacement in the itertools.lisp file.

Generators are functions
------------------------

Pygen generators don't have a "next" method. They are directly funcallable; that is to say, they are the "next" method.


Other generator creation issues
===============================

Can plain functions or closures be used as generators? Yes. For example:

    > (lambda (x)
        (lambda () x))

will work as an infinite generator that returns only one value. The caveat being that the generatorp function will not be able to recognize it as a generator. So far that's not a problem: nothing uses generatorp, but future code may do so. Therefore:

    > (lambda (x)
        (gen-lambda () x))

You can use the gen-lambda macro for the inner lambda. It wraps your lambda in a (funcallable) basic-generator object.

Stopping the generator correctly is the other problem.

Unlike python, which raises an exception to signal generator termination, pygen generators emit the generator-stop symbol from the pygen package. The initial implementation of pygen used a signal as a stop marker. Because with-yield/yield is based on the Arnesi CPS transformer, code meant to be run inside of a with-yield or defgenerator block could not handle signals. This necessitated some inefficient work-arounds. The other option - using the values channel to signal cessation - would mean that the values channels would not be free for user purposes.

Once a generator emits pygen:generator-stop, it should continue to do so every time it is called. There is a convenience macro to make this behavior easier to accomplish: gen-lambda-with-sticky-stop

    > (defun function->generator (source-func predicate)
      "Returns a generator that repeatedly calls source-func, tests the result against the predicate function, terminates when the predicate tests false, otherwise yielding the result"
        (gen-lambda-with-sticky-stop ()  
          (let ((res (funcall source-func)))
	    (if (funcall predicate res)
	        res
	        (sticky-stop)))))

When sticky-stop is called it returns the generator-stop symbol. In subsequent calls to the function, none of the body code will be run. Generator-stop will be returned each time.

Adaptors
========
How to interface with other Common Lisp iteration and generation tools.

Loop
----
Consuming a pygen generator from within loop:


    > (loop with g = (some-numbers)
    	for val = (funcall g)
	until (eq 'generator-stop val)
	...	
 
Iterate
-------
Iterate can step over generators in the same way as loop. There is also an extension for consuming pygen generators in iterate.

      > (ql:quickload 'pygen-iterate)
      > (use-package 'pygen-iterate)
      > (iter (for x in-generator (some-numbers))
              (print x))
      (1)
      (2)
      SOME-MESSAGE
      (3)
      (8)
      (9)

      > (iter (for (x) in-generator (some-numbers))
              (print x))
      1
      2
      SOME-MESSAGE
      3
      8
      9

The in-generator extension will bind the values list from the generator to the variable by default. Wrapping one or more variables in a list, as in the second example above, causes the individual values to be destructured into the variables. Eg:

    > (iter (for (i x) in-generator (enumerate (list->generator '(3 2 1))))
        (summing (* i x)))
    4

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
