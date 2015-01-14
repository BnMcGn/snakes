Pygen
=====

Python style generators for Common Lisp.  It includes yield and a port of itertools.

Generators
==========

Generators are functions that lazily return a sequence of items, one item per call. Eg:

>CL-USER> (defvar genr (list->generator '(1 2 3 4)))
>GENR
>CL-USER> (funcall genr)
>1
>CL-USER> (funcall genr)
>2
>CL-USER> (funcall genr)
>3

Generators retain state information between calls. They're useful for stepping through finite resources such as files and lists, but they can also be infinite, as when used for creating mathematical sequences.

Perhaps their most useful feature is an ability to be chained together, analogous to the way in which unix utilities can be piped together. They have a clean interface that encourages the creation of stackable, reusable tools.

While generators are not the most computationally efficient way to solve most problems, they're elegant in use and great for rapid prototyping.

Examples
========

Creating
--------
>(defgenerator some-numbers ()
>  (loop for i from 1 upto 3 do (yield i))
>  (print 'some-message)
>  (loop for i from 8 upto 9 do (yield i)))

Consuming
---------

>> (mapc-generator #'print (some-numbers))
>1
>2
>SOME-MESSAGE
>3
>8
>9

(defgenerator flatten (list)
  (when list
    (cond
      ((atom list)
       (yield list))
      (t
       (yield-all (flatten (car list)))
       (yield (flatten (cdr list)))))))


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
