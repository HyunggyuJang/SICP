;;;; load-ec.scm (also called load-ec-compiler.scm)

;;;;This is like load-eceval.scm except that it loads the version
;;;; of eceval that interfaces with compiled code
;;;;It doesn't load the compiler itself -- loading the compiler is up to you.

(load "regsim")				;reg machine simulator
(load "support")			;simulation of machine operations

;;**NB** eceval-compiler *must* be loaded after support.scm (eceval-support),
;;  so that the version of user-print in eceval-compiler will override
;;  the version in support.scm
(load "eceval-compiler")		;eceval itself
					;and interface to compiled code
