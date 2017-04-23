emacs-common-lisp

***
Eval lisp code by sbcl with seamless syntax.

This repository intends to share ideas.
***
#usage
	Install this library.
	Load this library.
	Install sbcl.
	Eval lisp code. (if slime is connecting, this library use slime connection.)

***
#example
`(let ((x '(1 2 3))) (emacs-common-lisp :i x (apply '+ x))) ;;=> 6`
