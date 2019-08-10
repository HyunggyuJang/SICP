;;;; file: aliens.scm
;;;

;;; In this system there is a table data structure that associates a
;;; sequence of keys with a list of values.  One may add new
;;; associations or add new elements to an existing list of
;;; associations.

;;; The table is organized as a tree.  Each node of the tree holds a
;;; list of values.  The key sequence to access any of the lists of
;;; values in the tree are the labels on the edges of the tree.
;;; The first key in a sequence labels an edge connecting the root of
;;; the tree to a first-layer node.

;;; The table is represented as a list, whose first element is a list
;;; of values at that node.  The rest is a list of branches, each of
;;; which is a list whose first element is a key component and whose
;;; rest is a subtable.

;;; ADD-TO-TABLE takes such a table and produces a new one that
;;; includes the association of the given value with the given
;;; sequence of keys.

(define (add-to-table val table keys)
  (if (null? keys)          
      ;; keys ran out so insert new value here.
      (make-table (cons val (table-values table))
                  (table-branches table))
      ;; more keys to go, so descend deeper into tree.
      (make-table (table-values table)  
                  (insert-in-branches val 
                                     (table-branches table) 
                                     keys))))


;;; INSERT-IN-BRANCHES is a subprocedure used only by ADD-TO-TABLE.
;;; It assumes there is at least one key.
;;; Search the branches looking for a match on the first key.
;;; Return the new list of branches with the appropriate subtable
;;; augmented. 
(define (insert-in-branches val branches keys)
  (cond ((null? branches)
         ;; Make a new list with one new branch.
         (list
	  (make-branch (car keys) 
		       (add-to-table val
				     (make-empty-table)
				     (cdr keys)))))
        ((eq? (car keys) (branch-key (car branches)))
         ;; found the right branch to augment: Add the value to
         ;; the subtable for that branch
         (cons
	  (make-branch (branch-key (car branches))
		       (add-to-table val
				     (branch-subtable (car branches))
				     (cdr keys)))
	  (cdr branches)))
        (else (cons (car branches)
                    (insert-in-branches val (cdr branches) keys)))))


;;; Low-level code to build a table

(define (make-empty-table)
  (make-table '() '()))

(define (make-table values list-of-branches)
  (cons values list-of-branches))

(define table-values car)
(define table-branches cdr)

(define (make-branch key subtable)
  (cons key subtable))

(define branch-key car)
(define branch-subtable cdr)

(define (install table things)
  ;; things is a list of freshthings
  ;; each is a list of name and a list of attributes
  (cond ((null? things)
         table)
        (else
         (install (add-to-table (car (car things))
                                table
                                (cadr (car things)))
                  (cdr things)))))

(define entering-class
  (install (make-empty-table)
           '((hexapod1 (methane 260K-270K infrared random))
             (hexapod2 (methane 260K-270K infrared random))
             (hexapod3 (methane 260K-270K infrared night))
             (hexapod4 (methane 260K-270K))
             (amoeboid1 (sulfur 350K-370K))
             (amoeboid2 (sulfur 350K-370K ultraviolet random))
             (amoeboid3 (sulfur 350K-370K ultraviolet random))
             (amoeboid4 (sulfur 350K-370K visible day))
             (gnork (ethanol 290K-300K))
             (gnorkette (ethanol 290K-300K ultraviolet))
             (snork (ethanol 290k-300k))
             (tork (ethanol 260K-270K))
             (bork (ethanol 260K-270K))
             (fred (oxygen 290K-300K visible))
             (mary (oxygen 290K-300K visible day))
             (mike (oxygen 290K-300K)))))

(define *order* '(atmosphere temperature spectrum cycle))

;;; Utilities
;;; standard aggregate operations

(define (map proc lst)
  (cond ((null? lst) `())
        (else (cons (proc (car lst))
                    (map proc (cdr lst))))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst)
               (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (accumulate proc init lst)
  (cond ((null? lst) init)
        (else (proc (car lst) (accumulate proc init (cdr lst))))))
