;; Exercise 5.2
(controller
    (assign p (const 1))
    (assign c (const 1))
 test-c
    (test (op >) (reg c) (reg n))
    (branch (label fact-done))
    (assign p (op *) (reg p) (reg c))
    (assign c (op +) (reg c) (const 1))
    (goto (label test-c))
 fact-done)

;; Exercise 5.3
;;; Version 1
(controller
    (assign g (const 1.0))
 test-g
    (test (op g-e?) (reg g))
    (branch (label sqrt-done))
    (assign g (op imp) (reg g))
    (goto (label test-g))
 sqrt-done)

;;; Version 2
;;; middle level procedures

;;;; good-enough?
(assign s-q (op sq) (reg g))
(assign d (op -) (reg s-q) (reg x))
(assign ad (op abs) (reg d))
(test (op <) (reg ad) (const 0.001))

;;;; improve
(assign x/g (op /) (reg x) (reg g))
(assign imped (op avg) (reg g) (reg x/g))

;;; low level procedures
;;;; square
(assign cp (reg g))
(assign squared (op *) (reg cp) (reg g))

;;;; abs
(test (op <) (reg d) (const 0))
(branch (label abs-fetch))
(assign absed (op neg) (reg d))
(goto (label abs-done))
abs-fetch
(assign absed (reg d))
abs-done

;;;; average
(assign s (op +) (reg x) (reg g))
(assing aved (op /) (reg s) (const 2))

;;; And linking all together
(controller
 (assign g (const 1.0))
 test-g
;;; good-enough? {
 ;; sq {
 (assign d (reg g))
 (assign t (op *) (reg d) (reg g))
 ;; }
 (assign d (op -) (reg t) (reg x))
 ;; abs {
 (test (op <) (reg d) (const 0))
 (branch (label abs-fetch))
 (assign t (op neg) (reg d))
 (goto (label abs-done))
 abs-fetch
 (assign t (reg d))
 abs-done
 ;; }
 (test (op <) (reg t) (const 0.001))
;;; }
 (branch (label sqrt-done))
;;; improve {
 (assign d (op /) (reg x) (reg g))
 ;; average {
 (assign t (op +) (reg x) (reg g))
 (assing g (op /) (reg t) (const 2))
 ;; }
;;; }
 (goto (label test-g))
 sqrt-done)

;; Exercise 5.4
;;; a
(controller
    (assign continue (label expt-done))
 expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
 ;; setup to compute b^{n-1}
    (save continue)
    (assign continue (label after-expt-recur))
    (assign n (op -) (reg n) (const 1))
    (goto (label expt-loop))
 after-expt-recur
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))
 base-case
    (assign val (const 1))
    (goto (reg continue))
 expt-done)

;;; b
(controller
    (assign c (reg n))
    (assign p (const 1))
 expt-loop
    (test (op =) (reg c) (const 0))
    (branch (label expt-done))
    (assign c (op -) (reg c) (const 1))
    (assign p (op *) (reg b) (reg p))
    (goto (label expt-loop))
 expt-done)

;; Exercise 5.7
(define expt-recur-machine
  (make-machine
   '(n b val continue)
   `((= ,=) (- ,-) (* ,*))
   '(
        (assign continue (label expt-done))
     expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        ;; setup to compute b^{n-1}
        (save continue)
        (assign continue (label after-expt-recur))
        (assign n (op -) (reg n) (const 1))
        (goto (label expt-loop))
     after-expt-recur
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
     base-case
        (assign val (const 1))
        (goto (reg continue))
     expt-done)))

;; Run expt-recur-machine
;; (set-register-contents! expt-recur-machine 'b 2)
;; (set-register-contents! expt-recur-machine 'n 5)
;; (start expt-recur-machine)
;; (get-register-contents expt-recur-machine 'val)
;; ;Value: 32

(define expt-iter-machine
  (make-machine
   '(n b c p)
   `((= ,=) (- ,-) (* ,*))
   '(
     (assign c (reg n))
     (assign p (const 1))
     expt-loop
     (test (op =) (reg c) (const 0))
     (branch (label expt-done))
     (assign c (op -) (reg c) (const 1))
     (assign p (op *) (reg b) (reg p))
     (goto (label expt-loop))
     expt-done)))

;; Run expt-iter-machine
;; (set-register-contents! expt-iter-machine 'b 2)
;; (set-register-contents! expt-iter-machine 'n 5)
;; (start expt-iter-machine)
;; (get-register-contents expt-iter-machine 'p)
;; ;Value: 32


;; Exercise 5.8
(define (add-label-entry entry labels)
  (if (get-label labels (label-name entry))
      (error "Given label name already exists in the labels" (list entry labels))
      (cons entry labels)))

(define (label-name entry) (car entry))

(define (get-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        false)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts (add-label-entry
                               (make-label-entry next-inst
                                                 insts)
                               labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define test-5.8-machine
  (make-machine
   '(a)
   '()
   '(start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)
   ))

;; Exercise 5.10
(define (register-exp? exp)
  (and (pair? exp)
       (pair? (cdr exp))
       (eq? (cadr exp) 'reg)))

(define (register-exp-reg exp) (car exp))

;; Exercise 5.11
;;; test machine
(define test-5.11-machine
  (make-machine
   '(x y)
   '()
   '((save y)
     (save x)
     (restore y))))
;; test this machine
(set-register-contents! test-5.11-machine 'y 6)
(set-register-contents! test-5.11-machine 'x 3)
(start test-5.11-machine)
;;; b.
(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (push stack (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (let* ((assoc-entry (pop stack))
             (assoc-name (car assoc-entry)))
        (if (eq? assoc-name reg-name)
            (set-contents! reg (pop stack))
            (error "Tried to restore value from register which is not one saved -- MAKE-RESTORE"))
        (advance-pc pc)))))
;;; c.
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stacks '())
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stacks
                       (lambda () (for-each (lambda (stack) (stack 'initialize))
                                            stacks)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin (set! register-table
                     (cons (list name (make-register name))
                           register-table))
                   (set! stacks (cons (list name (make-stack))
                                      stacks))))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stacks) stacks)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stacks (machine 'stacks))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stacks ops)))
     insts)))

(define (make-save inst machine stacks pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (let ((stack (cadr (assoc reg-name stacks))))
        (push stack (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stacks pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine
                            (stack-inst-reg-name inst))))
    (lambda ()
      (let ((stack (cadr (assoc reg-name stacks))))
        (set-contents! reg (pop stack)))
      (advance-pc pc))))

;; Exercise 5.12
;;; test machine
(define fib-machine
  (make-machine
   ;; '(continue n val)
   `((< ,<) (- ,-) (+ ,+))
   '((assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n-1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                            ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
     (goto (label fib-loop))             ; perform recursive call
     afterfib-n-1                        ; upon return, val contains Fib(n-1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n-2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n-1)
     (goto (label fib-loop))
     afterfib-n-2                       ; upon return, val contains Fib(n-2)
     (assign n (reg val))               ; n now contains Fib(n-2)
     (restore val)                      ; val now contains Fib(n-1)
     (restore continue)
     (assign val                        ; Fib(n-1)+Fib(n-2)
             (op +) (reg val) (reg n))
     (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))               ; base case: Fib(n)=n
     (goto (reg continue))
     fib-done)))

;; First task
(define type-dict
  '((assign 1) (test 2) (branch 3) (goto 4)
    (save 5) (restore 6) (perform 7)))

(define (adjoin-ordered precede? item ordered)
  (if (null? ordered)
      (list item)
      (let ((first (first ordered)))
        (cond ((precede? item first)
               (cons item ordered))
              ((precede? first item)
               (cons first
                     (adjoin-ordered
                      precede? item (cdr ordered))))
              (else
               ;; given item already in the ordered
               ordered)))))

(define (inst-precede? inst1 inst2)
  (let ((type1 (car inst1))
        (type2 (car inst2)))
    (let ((mapped1 (cadr (assoc type1 type-dict)))
          (mapped2 (cadr (assoc type2 type-dict))))
      (cond ((< mapped1 mapped2) true)
            ((> mapped1 mapped2) false)
            (else
             ;; same number
             ;; (not (equal? inst1 inst2)) ;just exclude the same one
             (symbol<? (hash-symbol-list (cdr inst1))
                       (hash-symbol-list (cdr inst2)))
             )))))

(define (hash-symbol-list slst)
  (fold-right
   (lambda (s appended)
     (symbol-append s appended))
   '||
   (flatten slst)))

;; Test hash-symbol-list
;; (hash-symbol-list '(a b (c d e) f g))
;; ;Value: abcdefg


;; Tree<A> -> List<A>
(define (flatten tree)
  (tree-map list append '() tree))

;; Test flatten
;; (flatten '(1 (2 3) (4 (5 6) 7)))
;; ;Value: (1 2 3 4 5 6 7)


;; (Leaf<A> -> B), (B, B -> B), B, Tree<A>
;; -> B
(define (tree-map leaf-op combine-op initial tree)
  (cond ((null? tree) initial)
        ((not (pair? tree)) (leaf-op tree))
        (else                           ;pair
         (combine-op
          (tree-map leaf-op combine-op initial
                    (car tree))
          (tree-map leaf-op combine-op initial
                    (cdr tree))))))

(define (make-multivalues-table)
  (let ((local-table '(*table*)))
    (define (lookup-vals key)
      (cond ((assoc key (cdr local-table)) => cdr)
            (else false)))
    (define (insert-value! key value)
      (let ((entry (assoc key (cdr local-table))))
        (if entry
            (set-cdr! entry
                      (cons value (cdr entry)))
            (set-cdr! local-table
                      (cons (cons key (list value))
                            (cdr local-table))))))
    (lambda (m)
      (case m
        ((lookup) lookup-vals)
        ((insert!) insert-value!)
        (else
         (error "Unknown request -- MAKE-MULTIVALUES-TABLE" m))))))

;; Test for make-multivalues-table
;; (define x (make-multivalues-table))
;; ((x 'insert!) 'a 5)
;; ((x 'lookup) 'a)
;; ;; (5)
;; ((x 'insert!) 'a 2)
;; ((x 'lookup) 'a)
;; ;; (2 5)
;; ((x 'insert!) 'b 2)
;; ((x 'lookup) 'b)
;; ;; (2)

;; First request
(define (assemble controller-text machine accept)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    (let ((all-insts
                           (fold-right (lambda (inst ordered)
                                         (adjoin-ordered inst-precede? inst ordered))
                                       '()
                                       (map (lambda (inst) (instruction-text inst))
                                            insts))))
                      (let ((entry-regs
                             (filter-map
                              (lambda (goto-inst)
                                (let ((dest (goto-dest goto-inst)))
                                  (and (register-exp? dest)
                                       (register-exp-reg dest))))
                              (take-while
                               (lambda (inst)
                                 (eq? (car inst) 'goto))
                               (drop-while
                                (lambda (inst)
                                  (not (eq? (car inst) 'goto)))
                                all-insts))))
                            (stack-related-regs
                             (fold-right
                              (lambda (reg-name regs)
                                (adjoin-ordered
                                 symbol<?
                                 reg-name
                                 regs))
                              '()
                              (map (lambda (inst)
                                     (stack-inst-reg-name inst))
                                   (take-while
                                    (lambda (inst)
                                      (or (eq? (car inst) 'save)
                                          (eq? (car inst) 'restore)))
                                    (drop-while
                                     (lambda (inst)
                                       (not (eq? (car inst) 'save)))
                                     all-insts)))))
                            (reg-sources-table
                             (let ((tbl (make-multivalues-table)))
                               (for-each
                                (lambda (inst)
                                  ((tbl 'insert!)
                                   (assign-reg-name inst)
                                   (assign-value-exp inst)))
                                (take-while
                                 (lambda (inst)
                                   (eq? (car inst) 'assign))
                                 all-insts))
                               tbl)))
                        (accept
                         insts
                         (make-labels-package
                          get-label
                          retrive-label-from-insts
                          labels)
                         all-insts
                         entry-regs
                         stack-related-regs
                         reg-sources-table))))))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    (assemble controller-text machine
              (lambda (instructions labels-package
                       all-instructions registers-with-entry
                       stack-inst-regs reg-sources-table)
                ((machine 'install-instruction-sequence)
                 instructions)
                ((machine 'install-labels-package)
                 labels-package)
                ((machine 'install-all-instructions)
                 all-instructions)
                ((machine 'install-registers-with-entry)
                 registers-with-entry)
                ((machine 'install-stack-instruction-registers)
                 stack-inst-regs)
                ((machine 'install-register-sources-table)
                 reg-sources-table)))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (all-instructions '())
        (registers-with-entry '())
        (stack-instruction-registers '())
        (register-sources-table '())    ;initial value -- undefined
        (number-execs 0)
        (trace? #f)
        (labels-package '())
        (break-points '(#f)))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (cdring-down lst n)
        (if (= n 1)
            lst
            (cdring-down (cdr lst)
                         (-1+ n))))
      (define (add-break-point label n)
        (let ((break-point
               (cdring-down ((find-from-key labels-package) label)
                            n)))
          (if (memq break-point (cdr break-points))
              (error "Given break point already in break points -- ADD-BREAK-POINT"
                     (list label n))
              (set-cdr! break-points (cons break-point
                                           (cdr break-points))))))
      (define (remove-break-point label n)
        (let ((break-point
               (cdring-down ((find-from-key labels-package) label)
                            n)))
          (let loop ((items break-points))
            (cond ((null? (cdr items))
                   (error "Given break point not in break points -- REMOVE-BREAK-POINT"
                          (list label n)))
                  ((eq? (cadr items) break-point)
                   (set-cdr! items (cddr items)))
                  (else (loop (cdr items)))))))
      (define (print-statistics)
        (newline)
        (display
         `(tatal-executions = ,number-execs)))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
              (cons (list name (make-register name))
                    register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (try-allocate-and-return-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin (allocate-register name)
                     (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((and (car break-points) ;the right after instruction from the broken
                      (not (eq? (car break-points) insts)))
                 (set-car! break-points #f)
                 (execute))
                ((and (not (eq? (car break-points) insts)) ;ensure this is not the point broken
                      (memq insts (cdr break-points)))
                 (set-car! break-points insts) ;save the broken point
                 'broken)
                (else
                 (let ((inst (car insts)))
                   (if trace?
                       (let ((label-entry ((find-from-val labels-package)
                                           insts)))
                         (if label-entry
                             (begin (newline)
                                    (display (label-name label-entry))))
                         (begin (newline)
                                (display (instruction-text inst)))))
                   ((instruction-execution-proc inst))
                   (set! number-execs (1+ number-execs))
                   (execute))))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'proceed-machine)
               (if (car break-points)
                   (begin (set-contents! pc (car break-points))
                          (execute))
                   (error "There in no broken point to proceed from -- PROCEED-MACHINE")))
              ((eq? message 'set-breakpoint)
               add-break-point)
              ((eq? message 'cancel-breakpoint)
               remove-break-point)
              ((eq? message 'cancel-all-breakpoints)
               (set! break-points '(#f)))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'install-all-instructions)
               (lambda (seq) (set! all-instructions seq)))
              ((eq? message 'install-registers-with-entry)
               (lambda (seq) (set! registers-with-entry seq)))
              ((eq? message 'install-stack-instruction-registers)
               (lambda (seq) (set! stack-instruction-registers seq)))
              ((eq? message 'install-register-sources-table)
               (lambda (tbl) (set! register-sources-table tbl)))
              ((eq? message 'all-instructions) all-instructions)
              ((eq? message 'registers-with-entry) registers-with-entry)
              ((eq? message 'stack-instruction-registers)
               stack-instruction-registers)
              ((eq? message 'sources-of)
               (lambda (reg-name)
                 ((register-sources-table 'lookup) reg-name)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'try-allocate-and-get-register)
               try-allocate-and-return-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'print-statistics) (print-statistics))
              ((eq? message 'initialize-statistics) (set! number-execs 0))
              ((eq? message 'trace-on) (set! trace? #t))
              ((eq? message 'trace-off) (set! trace? #f))
              ((eq? message 'install-labels-package)
               (lambda (labels-pack)
                 (set! labels-package labels-pack)))
              ((eq? message 'trace-on-register)
               (lambda (reg-name)
                 ((lookup-register reg-name) 'trace-on)))
              ((eq? message 'trace-off-register)
               (lambda (reg-name)
                 ((lookup-register reg-name) 'trace-off)))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; Exercise 5.13
(define (make-sure-allocate-register-and-get machine reg-name)
  ((machine 'try-allocate-and-get-register) reg-name))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (make-sure-allocate-register-and-get machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (make-sure-allocate-register-and-get machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (make-save inst machine stack pc)
  (let ((reg (make-sure-allocate-register-and-get machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (make-sure-allocate-register-and-get machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (make-sure-allocate-register-and-get machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

;; Test machine
(define fib-machine
  (make-machine
   `((< ,<) (- ,-) (+ ,+))
   '((assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n-1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                            ; save old value of n
     (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
     (goto (label fib-loop))             ; perform recursive call
     afterfib-n-1                        ; upon return, val contains Fib(n-1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n-2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n-1)
     (goto (label fib-loop))
     afterfib-n-2                       ; upon return, val contains Fib(n-2)
     (assign n (reg val))               ; n now contains Fib(n-2)
     (restore val)                      ; val now contains Fib(n-1)
     (restore continue)
     (assign val                        ; Fib(n-1)+Fib(n-2)
             (op +) (reg val) (reg n))
     (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))               ; base case: Fib(n)=n
     (goto (reg continue))
     fib-done)))

;; Exercise 5.14
(define fact-machine
  (make-machine
   `((= ,=) (- ,-) (* ,*) (read ,read))
   '((assign n (op read))
     (perform (op initialize-stack))
     (assign continue (label fact-done)) ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ; val now contains n(n-1)!
     (goto (reg continue))                 ; return to caller
     base-case
     (assign val (const 1))             ; base case: 1!=1
     (goto (reg continue))              ; return to caller
     fact-done
     (perform (op print-stack-statistics)))))

;; Exercise 5.17
;;; ADT for labels package which going to be installed into machine object
(define (make-labels-package find-from-key find-from-val labels)
  (list find-from-key find-from-val labels))
(define (find-from-key-proc labels-pack)
  (car labels-pack))
(define (find-from-val-proc labels-pack)
  (cadr labels-pack))
(define (labels-data labels-pack)
  (caddr labels-pack))

;; operate on labels-package
(define (find-from-val labels-pack)
  (lambda (val)
    ((find-from-val-proc labels-pack)
     (labels-data labels-pack)
     val)))

(define (find-from-key labels-pack)
  (lambda (key)
    ((find-from-key-proc labels-pack)
     (labels-data labels-pack)
     key)))

;; selector of labels
(define (retrive-label-from-insts items key)
  ((association-procedure eq? cdr) key items))

;; Exercisse 5.18
(define (make-register name)
  (let ((contents '*unassigned*)
        (trace? #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace?
                   (begin (newline)
                          (display `(Register ,name gets ,value from ,contents))))
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! trace? #t))
            ((eq? message 'trace-off)
             (set! trace? #f))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))
;;; test make-register
;; (define x (make-register 'x))
;; (x 'trace-on)
;; ((x 'set) 5)

;; (register x gets 5 from *unassigned*)
;; ;Value: *unassigned*

;; Exercise 5.19
;;; test machine
(define gcd-machine
  (make-machine
   ;; '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (proceed-machine machine)
  (machine 'proceed-machine))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

;; Test feature
;; (set-register-contents! gcd-machine 'a 202)
;; (set-register-contents! gcd-machine 'b 43)
;; (set-breakpoint gcd-machine 'test-b 4)
;; (gcd-machine 'trace-on)
;; (start gcd-machine)
;; (proceed-machine gcd-machine)
;; (cancel-breakpoint gcd-machine 'test-b 4)
;; (proceed-machine gcd-machine)

;; Exercise 5.21
(define count-leaves-machine-a
  (make-machine
   `((null? ,null?)
     (pair? ,pair?)
     (not ,not)
     (+ ,+)
     (car ,car)
     (cdr ,cdr))
   '(
     (assign continue (label count-done))
     count-leaves-loop
     (test (op null?) (reg tree))
     (branch (label base-case))
     ;; (test (op pair?) (reg tree))
     ;; (test (op not) (reg flag))
     (assign t (op pair?) (reg tree))
     (test (op not) (reg t))
     (branch (label count-case))
     (save continue)          ;setup recursive call -- (count-leaves (car tree))
     (assign continue (label after-car-tree))
     (save tree)
     (assign tree (op car) (reg tree))
     (goto (label count-leaves-loop))
     after-car-tree
     (assign continue (label after-cdr-tree))
     (restore tree)                     ;setup recursive call -- (count-leaves (cdr tree))
     (save val)
     (assign tree (op cdr) (reg tree))
     (goto (label count-leaves-loop))
     after-cdr-tree
     (restore t)
     (assign val (op +) (reg val) (reg t))
     (restore continue)
     (goto (reg continue))
     base-case
     (assign val (const 0))
     (goto (reg continue))
     count-case
     (assign val (const 1))
     (goto (reg continue))
     count-done)))

;;; test for count-leaves-machine-a
;; (set-register-contents! count-leaves-machine-a 'tree '(1 2 (3 4 (5 6) (7))))
;; (start count-leaves-machine-a)
;; ;Value: done
;; (get-register-contents count-leaves-machine-a 'val)
;; ;Value: 7

;;; b
(define count-leaves-machine-b
  (make-machine
   ;; '(n continue tree)
   `((null? ,null?)
     (pair? ,pair?)
     (not ,not)
     (1+ ,1+)
     (car ,car)
     (cdr ,cdr))
   '(
     (assign n (const 0))
     (assign continue (label count-done))
     count-loop
     (test (op null?) (reg tree))
     (branch (label base-case))
     (test (op pair?) (reg tree))
     (test (op not) (reg flag))
     (branch (label count-case))
     (save continue)                    ;else clause
     (assign continue (label after-car))
     (save tree)
     (assign tree (op car) (reg tree))
     (goto (label count-loop))
     after-car
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-cdr))
     (goto (label count-loop))
     after-cdr
     (restore continue)
     (goto (reg continue))
     base-case
     (goto (reg continue))
     count-case
     (assign n (op 1+) (reg n))
     (goto (reg continue))
     count-done)))

;; Exercise 5.22
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(define append-machine
  (make-machine
   '(x continue y)
   `((null? ,null?) (pair? ,pair?)
     (car ,car) (cdr ,cdr) (cons ,cons))
   '((assign continue (label append-done))
     append-loop
     (test (op null?) (reg x))
     (branch (label base-case))
     (save continue)
     (assign continue (label after-recur))
     (save x)
     (assign x (op cdr) (reg x))
     (goto (label append-loop))
     after-recur
     (restore x)
     (assign x (op car) (reg x))
     (assign y (op cons) (reg x) (reg y))
     (restore continue)
     (goto (reg continue))
     base-case
     (goto (reg continue))
     append-done)))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; High level description -- assume last-pair as primitive
'(
  (assign l-p (op last-pair) (reg x))
  (perform (op set-cdr!) (reg l-p) (reg y))
  )

;;; last-pair unwinded
'(
  test-l-p
  (assign t (op cdr) (reg x))
  (test (op null?) (reg t))
  (branch (label last-pair-done))
  (assign x (reg t))
  (goto (label test-l-p))
  last-pair-done
  )

;; Then link together
(define append!-machine
  (make-machine
   '(x y l-p t)
   `((set-cdr! ,set-cdr!) (cdr ,cdr) (null? ,null?))
   '(
     (assign l-p (reg x))
     test-l-p
     (assign t (op cdr) (reg l-p))
     (test (op null?) (reg t))
     (branch (label last-pair-done))
     (assign l-p (reg t))
     (goto (label test-l-p))
     last-pair-done
     (perform (op set-cdr!) (reg l-p) (reg y))
     )))

;; Exercise 5.23
`(cond? ,cond?)
`(let? ,let?)
`(let*? ,let*?)
`(cond->if ,cond->if)
`(let->combination ,let->combination)
`(let*->let ,let*->let)
;;operations in eceval-support.scm

;; Exercise 5.23 -- derived forms
(test (op cond?) (reg exp))
(branch (label ev-cond))
(test (op let?) (reg exp))
(branch (label ev-let))
(test (op let*?) (reg exp))
(branch (label ev-let*))
;; end of exercise 5.23

;; derived forms
ev-cond
(assign exp (op cond->if) (reg exp))
(goto (label ev-if))
ev-let
(assign exp (op let->combination) (reg exp))
(goto (label ev-application))
ev-let*
(assign exp (op let*->let) (reg exp))
(goto (label ev-let))
;;

;; Exercise 5.24
ev-cond
;; Input exp env continue
;; Output val
;; Write all (call the ev-sequence)
;; Stack unchanged
(assign unev (op cond-clauses) (reg exp))
(save continue)
cond-clause-loop
;; Input unev env stack (top as return point)
;; Output val
;; Write all
;; Stack top value removed
(assign exp (op cond-first-clause) (reg unev))
(test (op cond-else-clause?) (reg exp))
(branch (label cond-else-clause))
(test (op cond-last-clause?) (reg unev))
(branch (label cond-last-clause))
cond-pred
(save unev)
(save exp)
(save env)
(assign exp (op cond-predicate) (reg exp))
(assign continue (label cond-pred-decide))
(goto (label eval-dispatch))
cond-pred-decide
(restore env)
(restore exp)
(restore unev)
(test (op true?) (reg val))
(branch (label cond-actions))
(assign unev (op cond-rest-clauses) (reg unev))
(goto (label cond-clause-loop))
cond-actions
;; Input exp, env
;; Output val
;; Write all
;; Stack top removed
(assign unev (op cond-actions) (reg exp))
(goto (label ev-sequence))
cond-else-clause
(test (op cond-last-clause?) (reg unev))
(test (op not) (reg flag))
(branch (label bad-cond-syntax))
(goto (label cond-actions))
cond-last-clause
(save exp)
(save env)
(assign exp (op cond-predicate) (reg exp))
(assign continue (label cond-last-decide))
(goto (label eval-dispatch))
cond-last-decide
(restore env)
(restore exp)
(test (op true?) (reg val))
(branch (label cond-actions))
(restore continue)
(goto (reg continue))

;; test Exercise 5.24
;;; from recitation 26 of 2004
(define (list? x)
  (cond ((null? x) true)
        ((pair? x) (list? (cdr x)))))
(define z (list 1))
(set-cdr! z z)
(list? z)
