;;; -*-Scheme-*-

;;; The structure "problem-sets" must be loaded from a file whenever
;;; the set of available problem sets changes, or when the default
;;; problem set changes.  Files should appear with name and extension, but
;;; without device, directory, or version; these will be supplied
;;; automatically.
;;;
;;; The first number is used as the "default" problem set

(define problem-sets
  '(1  (1
	(load "crypto.scm"))
       ))
