(define (compiled-code name)
  (let ((architecture
	 (if (and (eq? 'UNIX microcode-id/operating-system)
		  (string-ci=? "HP-UX" microcode-id/operating-system-variant))
	     "hpux"
	     "i86")))
    (string-append "compiled-" architecture "/" name)))

(define (pset-dir-name name)
  (u6001-dir (string-append "psets/ps7/" name)))

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
	  '("objsys.scm" "objtypes.scm" "search-rooms.scm"
			 "secret-maze.scm" "setup.scm"))

(for-each (lambda (name) (load (pset-dir-name name)))
	  '("objsys.scm" "objtypes.scm" "search-rooms.scm"
			 "secret-maze.scm"))

(load (pset-dir-name (compiled-code "our-secret")))

;;; The setup world is full of good stuff.
(load (pset-dir-name "setup"))

