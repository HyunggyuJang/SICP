;;;; Loader for Scheme augmented with AMB
;;;  as in section 4.3 of SICP.

;;; Grab Scheme magic applicator.

(define apply-in-underlying-scheme
  (access apply system-global-environment))


;;; Expression representation
(load "syntax")

;;; Evaluator data structures
;;;  environment representation, procedure representation
(load "evdata")

;;; Support for read-eval-print driver loop
(load "rep-support")

;;; Support for LET (as noted in footnote 56, p.428)
(load "let")

;;; Actual code of evaluator for AMB evaluator of section 4.3
(load "ambeval")

;;; Code to support setup of initial environment.
(load "setup")


