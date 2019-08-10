(define nil '())

;;; Grab Scheme magic applicator.

(define apply-in-underlying-scheme
  (access apply system-global-environment))


(define (pset-dir-name name)
  (u6001-dir (string-append "psets/ps9/" name)))

(define (work-dir-name name)
  (u6001-dir (string-append "work/" name)))


;;;The following mess is to get around the fact that the copy option
;;;loses in the current Scheme distribution.  I think this is fixed in
;;;the most recent source, but we didn't distribute this for spring98.

(define (copy-file-and-make-buffer filename)
  (define (open-buffer file)
    ((in-package (->environment '(edwin)) find-file-noselect)
     filename
     #f))
  (copy-file (pset-dir-name filename) (work-dir-name filename))
  (open-buffer (pset-dir-name filename)))

(for-each copy-file-and-make-buffer
	  '("syntax.scm"
	    "regsim.scm"
	    "support.scm"
	    "compiler.scm"
	    "eccomplr.scm"))


(for-each (lambda (name) (load (pset-dir-name name)))
	  '("syntax.scm"
	    "regsim.scm"
	    "support.scm"
	    "compiler.scm"))

;;**NB** eccomplr *must* be loaded after support,
;;  so that the version of user-print in eccomplr will override
;;  the version in support

(load (pset-dir-name "eccomplr.scm"))

