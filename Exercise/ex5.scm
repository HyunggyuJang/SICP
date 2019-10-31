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
   '(continue n val)
   `((< ,<) (- ,-) (+ ,+) (read ,read))
   '((assign n (op read))
     (perform (op initialize-stack))
     (assign continue (label fib-done))
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
     fib-done
     (perform (op print-stack-statistics)))))

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
   '(n val continue)
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

;; Exercise 5.25 delayed one
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

ev-application
(save continue)
(save env)
(assign unev (op operands) (reg exp))
(save unev)
(assign exp (op operator) (reg exp))
(assign continue (label ev-appl-did-operator))
(goto (label actual-value))
ev-appl-did-operator
(restore unev)
(restore env)
(assign proc (reg val))
(branch (label apply-dispatch))

apply-dispatch
(assign argl (op empty-arglist))
;; Input proc, unev, env, stack -- top value is return point
;; Output val
;; Write all
;; Stack top value removed
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-apply))
(test (op compound-procedure?) (reg proc))
(branch (label compound-apply))
(goto (label unknown-procedure-type))

primitive-apply
(test (op no-operands?) (reg unev))
(branch (label exec-primitive-apply))
(save proc)
primitive-operand-loop
(save argl)
(assign exp (op first-operand) (reg unev))
(test (op last-operand?) (reg unev))
(branch (label prim-last-arg))
(save env)
(save unev)
(assign continue (label prim-accumulate-arg))
(goto (label actual-value))
prim-accumulate-arg
(restore unev)
(restore env)
(restore argl)
(assign argl (op adjoin-arg) (reg val) (reg argl))
(assign unev (op rest-operands) (reg unev))
(goto (label primitive-operand-loop))
prim-last-arg
(assign continue (label prim-accum-last-arg))
(goto (label actual-value))
prim-accum-last-arg
(restore argl)
(assign argl (op adjoin-arg) (reg val) (reg argl))
(restore proc)
(goto (label exec-primitive-apply))
exec-primitive-apply
(assign val (op apply-primitive-procedure)
        (reg proc)
        (reg argl))
(restore continue)
(goto (reg continue))

compound-apply
(test (op no-operands?) (reg unev))
(branch (label exec-compound-apply))
compound-operand-loop
(assign exp (op first-operand) (reg unev))
(test (op last-operand?) (reg unev))
(branch (label compound-last-arg))
(assign val (op delay-it) (reg exp) (reg env))
(assign argl (op adjoin-arg) (reg val) (reg argl))
(assign unev (op rest-operands) (reg unev))
(goto (label compound-operand-loop))
compound-last-arg
(assign val (op delay-it) (reg exp) (reg env))
compound-accum-last-arg
(assign argl (op adjoin-arg) (reg val) (reg argl))
(goto (label exec-compound-apply))

exec-compound-apply
(assign unev (op procedure-parameters) (reg proc))
(assign env (op procedure-environment) (reg proc))
(assign env (op extend-environment)
        (reg unev) (reg argl) (reg env))
(assign unev (op procedure-body) (reg proc))
(goto (label ev-sequence))

ev-if
(save exp)
(save env)
(save continue)
(assign continue (label ev-if-decide))
(assign exp (op if-predicate) (reg exp))
(goto (label actual-value))
ev-if-decide
(restore continue)
(restore env)
(restore exp)
(test (op true?) (reg val))
(branch (label ev-if-consequent))
ev-if-alternative
(assign exp (op if-alternative) (reg exp))
(goto (label eval-dispatch))
ev-if-consequent
(assign exp (op if-consequent) (reg exp))
(goto (label eval-dispatch))

actual-value
;; contract is same as eval-dispatch
(save continue)
(assign continue (label after-eval))
(goto (label eval-dispatch))
after-eval
(restore continue)
(goto (label force-it))

force-it
;; Input val continue
;; Output val
;; Write all
;; Stack unchanged
(test (op thunk?) (reg val))
(branch (label force-thunk))
(test (op evaluated-thunk?) (reg val))
(branch (label force-evaluated))
(goto (reg continue))

force-thunk
(save continue)
(save val)                              ;need later -- obj
(assign continue (label force-result))
(assign exp (op thunk-exp) (reg val))
(assign env (op thunk-env) (reg val))
(goto (label actual-value))

force-result
(restore exp)                           ;clobbering the exp as obj
(restore continue)
(perform (op set-car!) (reg exp) (const evaluated-thunk))
(assign exp (op cdr) (reg exp))
(perform (op set-car!) (reg exp) (reg val))
(perform (op set-cdr!) (reg exp) (const ()))
(goto (reg continue))

force-evaluated
(assign val (op thunk-value) (reg val))
(goto (reg continue))

;; Test ex5.25
(define (try a b)
  (if (= a 0) 1 b))

;; Exercise 5.26
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; Exercise 5.27
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; Exercise 5.29
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; Exercise 5.30
(define test
  (lambda x
    (let ((number-of-arguments (length x)))
      (case number-of-arguments
        ((0) 'nullary)
        ((1) 'unary)
        ((2) 'binary)
        (else 'recursive)))))

;; (define (test-arity arity)
;;   (lambda x
;;     (let ((number-of-arguments (length x)))
;;       (case number-of-arguments
;;         (arity 'arity)
;;         ((0) 'nullary)
;;         ((1) 'unary)
;;         ((2) 'binary)
;;         (else 'recursive)))))

;; Section 5.5.5
(compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next)

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
  compiled-branch16
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call15
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
  true-branch5
  (assign val (const 1))
  (goto (reg continue))
  false-branch4
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)                       ;this would be restored after recursive call
  (save proc)                           ;this one too
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl)                           ;difference -- this one also
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
  compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call6                           ; val contains (- n 1)
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
  compiled-branch10
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call9                           ;val conatins (factorial (- n 1))
  (restore argl)                        ;restore the stacked argument list
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)                        ;restore the defered operation
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
  compiled-branch13
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call12
  after-if3
  after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

(compile
 '(define (factorial-alt n)
    (if (= n 1)
        1
        (* n (factorial-alt (- n 1)))))
 'val
 'next)

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
  compiled-branch16
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call15
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
  true-branch5
  (assign val (const 1))
  (goto (reg continue))
  false-branch4
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (save env)                            ;this is the major difference
  (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
  compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call6                           ;val now contains (- n 1)
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
  compiled-branch10
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call9
  (assign argl (op list) (reg val))
  (restore env)                         ;***
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
  compiled-branch13
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call12
  after-if3
  after-lambda1
  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
  (assign val (const ok))))

;; Exercise 5.34
(compile
 '(define (factorial n)
    (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
                (+ counter 1))))
    (iter 1 1))
 'val
 'next)

((env)
 (val)
 (
  ;; construct the procedure and skip over code for the procedure body
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2                                ;calls to factorial will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; begin the procedure body
  ;; it actually first define internal procedure
  ;; so, it construct the internal procedure and skip over code for the
  ;; interanl procedure body
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))
  entry7                                ;internal procedure call will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  ;; actual iterative process starts
  (save continue)
  (save env)
  ;; compute (> counter n)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
  compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call20                         ;val now contains result of (> counter n)
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch9))
  true-branch10         ;return the value of product that bound to (factorial n)
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
  false-branch9
  ;; compute and return (iter (* counter product) (+ counter 1))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)
  ;; compute (+ counter 1)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
  compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call14                         ;val now contains result of (+ counter 1)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  ;; compute (* counter product)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
  compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call11                   ;val now contains result of (* counter product)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)                        ;restore iter
  (restore continue)
  ;; apply iter
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
  compiled-branch18
  ;; note that a compound procedure here is called tail-recursively
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call17    ;val now contains (factorial n)
  ;; there is no restore operations after this point
  ;; which means there is no defered operations and arguments --
  ;; the stack doesn't grow and get reduced before and after the
  ;; recursive call; this is why it is iterative process but above.
  after-if8
  after-lambda6        ;end of procedure body of iter
  ;; assign the procedure to the variable iter (internally)
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  ;; setup the initial call of iter -- (iter 1 1)
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
  compiled-branch4
  ;; note also that this compound procedure call is tail-recursive
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call3
  after-lambda1                ;end of procedure body of factorial
  ;; assign the procedure to the variable factorial
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

;;EXERCISE 5.35 (FIGURE 5.18)
(assign val
        (op make-compiled-procedure)    ;compilation of lambda expression
        (label entry16)
        (reg env))
(goto (label after-lambda15))
entry16
(assign env (op compiled-procedure-env) (reg proc))
(assign env
        (op extend-environment)
        (const (x))                   ;formal parameter list is (x)
        (reg argl)
        (reg env))
;; the actual procedure body of f
(assign proc
        (op lookup-variable-value)
        (const +)                       ;compliation of application of (+ ...)
        (reg env))
(save continue)
(save proc)
(save env)
;; the last argument of given application was actually another application expression
;; (g ...)
(assign proc (op lookup-variable-value) (const g) (reg env))
(save proc)
;; the last argument of inner application was yet another application (+ ...)
(assign proc (op lookup-variable-value) (const +) (reg env))
;; the last argument of innermost application was 2
(assign val (const 2))
(assign argl (op list) (reg val))
;; the next one (the argument in front of 2) was variable x
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op cons) (reg val) (reg argl))
;; start procedure call
;; which means argument list was (x 2)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch19))
compiled-branch18
(assign continue (label after-call17))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch19
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17                            ;val now contains the value of (+ x 2)
(assign argl (op list) (reg val))
;; turns out (+ x 2) was the only argument of application of procedure g
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch22))
compiled-branch21
(assign continue (label after-call20))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch22
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20                            ;now val contains (g (+ x 2))
(assign argl (op list) (reg val))
(restore env)
;; the next argument of outermost application was variable x
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op cons) (reg val) (reg argl))
;; the outermost application was (+ x (g (+ x 2)))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch25))
compiled-branch24
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch25
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call23                            ;now val contains the value of (+ x (g (+ x 2)))
after-lambda15
;; the whole expression was defintion that binds f to the given lambda expression
(perform (op define-variable!) (const f) (reg val) (reg env))
(assign val (const ok))
;;end of exercise

(compile
 '(define (f x)
    (+ x (g (+ x 2))))
 'val
 'next)

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
  compiled-branch4
  (assign continue (label after-call3))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call3
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
  compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call6
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
  compiled-branch10
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call9
  after-lambda1
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))))

;; Exercise 5.36
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
                                 '((assign argl (const ()))))
      (let ((code-to-get-last-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl)
                                         '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-last-arg
            (preserving '(env)
                        code-to-get-last-arg
                        (code-to-get-rest-args
                         (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(val argl)
           '((assign val (op list) (reg val))
             (assign argl
                     (op append) (reg argl) (reg val)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2))))

;; Case 1
(compile
 '(f 'x 'y)
 'val
 'next)

;; Modified version
((env continue)
 (env proc argl continue val)
 ((save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const y))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (assign val (const x))
  (restore continue)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch3))
  compiled-branch2
  (assign continue (label after-call1))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch3
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call1))

;; Original version

((env)
 (env proc argl continue val)
 ((assign proc (op lookup-variable-value) (const f) (reg env))
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
  compiled-branch5
  (assign continue (label after-call4))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call4))

;; Case 2
(compile
 '((f) 'x 'y)
 'val
 'next)
;; Modified version

((env continue)
 (env proc argl continue val)
 ((save continue)
  (save env)
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (assign argl (const ()))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch9))
  compiled-branch8
  (assign continue (label proc-return10))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return10
  (assign proc (reg val))
  (goto (label after-call7))
  primitive-branch9
  (save continue)
  (assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call7
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const y))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (assign val (const x))
  (restore continue)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
  compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch13
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call11))

;; Original version
((env)
 (env proc argl continue val)
 ((assign proc (op lookup-variable-value) (const f) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
  compiled-branch15
  (assign continue (label proc-return17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return17
  (assign proc (reg val))
  (goto (label after-call14))
  primitive-branch16
  (assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call14
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch20))
  compiled-branch19
  (assign continue (label after-call18))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch20
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call18))

;; Case 3
(compile
 '(f (g 'x) y)
 'val 'next)

;; Modified version
((env continue)
 (env proc argl continue val)
 ((save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (op lookup-variable-value) (const y) (reg env))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save continue)
  (assign val (const x))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch29))
  compiled-branch28
  (assign continue (label after-call27))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch29
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call27
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch32))
  compiled-branch31
  (assign continue (label after-call30))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch32
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call30))

;; Original version
((env)
 (env proc argl continue val)
 ((assign proc (op lookup-variable-value) (const f) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const y) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (const x))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch23))
  compiled-branch22
  (assign continue (label after-call21))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch23
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call21
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch26))
  compiled-branch25
  (assign continue (label after-call24))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch26
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call24))

;; Case 4
(compile '(f (g 'x) 'y) 'val 'next)

;; Modified version
((env continue)
 (env proc argl continue val)
 ((save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const y))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save continue)
  (assign val (const x))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch35))
  compiled-branch34
  (assign continue (label after-call33))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch35
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call33
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch38))
  compiled-branch37
  (assign continue (label after-call36))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch38
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call36))

;; Original version
((env)
 (env proc argl continue val)
 ((assign proc (op lookup-variable-value) (const f) (reg env))
  (save proc)
  (assign val (const y))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (const x))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch41))
  compiled-branch40
  (assign continue (label after-call39))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch41
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call39
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch44))
  compiled-branch43
  (assign continue (label after-call42))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch44
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call42))

;; Exercise 5.38
;;; a
(define (spread-arguments args)
  (let ((a1 (car args))
        (a2 (cadr args)))
    (preserving
     '(env)
     (compile a1 'arg1 'next)
     (preserving
      '(arg1)
      (compile a2 'arg2 'next)
      (make-instruction-sequence
       '(arg1) '() '())))))
;; test
(spread-arguments '(x y))
((env) (arg1 arg2)
 ((assign arg1 (op lookup-variable-value) (const x) (reg env))
  (assign arg2 (op lookup-variable-value) (const y) (reg env))))

(define (compile-= exp target linkage)
  (end-with-linkage
   linkage
   (append-instruction-sequences
    (spread-arguments (operands exp))
    (make-instruction-sequence
     '(arg1 arg2)
     (list target)
     `((assign ,target (op =) (reg arg1) (reg arg2)))))))

;; Test compile-=
;;; simple case
(compile-= '(= 5 2) 'val 'next)
(() (arg1 arg2 val)
 ((assign arg1 (const 5))
  (assign arg2 (const 2))
  (assign val (op =) (reg arg1) (reg arg2))))

(define (=? exp) (tagged-list? exp '=))

;;; recursive case
(compile '(= (= 5 2) (= 2 1))
         'val 'next)

(()
 (arg1 arg2 val)
 ((assign arg1 (const 5))
  (assign arg2 (const 2))
  (assign arg1 (op =) (reg arg1) (reg arg2))
  (save arg1)
  (assign arg1 (const 2))
  (assign arg2 (const 1))
  (assign arg2 (op =) (reg arg1) (reg arg2))
  (restore arg1)
  (assign val (op =) (reg arg1) (reg arg2))))

;; open-code primitive dictionary
(define open-coded-prims '((= =) ;; (* *)
                           (- -) ;; (+ +)
                           ))

(define (open-coded-prims? exp)
  (and (= (length (operands exp)) 2)    ;binary
       (assoc (operator exp) open-coded-prims)))

(define (compile-open-coded-prim exp target linkage op)
  (end-with-linkage
   linkage
   (append-instruction-sequences
    (spread-arguments (operands exp))
    (make-instruction-sequence
     '(arg1 arg2)
     (list target)
     `((assign ,target (op ,op) (reg arg1) (reg arg2)))))))

;; test compile-open-coded-prim
;; The previous one works well in this new scheme
(pp (compile '(= (= 5 2) (= 2 1))
         'val 'next))
(()
 (arg1 arg2 val)
 ((assign arg1 (const 5))
  (assign arg2 (const 2))
  (assign arg1 (op =) (reg arg1) (reg arg2))
  (save arg1)
  (assign arg1 (const 2))
  (assign arg2 (const 1))
  (assign arg2 (op =) (reg arg1) (reg arg2))
  (restore arg1)
  (assign val (op =) (reg arg1) (reg arg2))))

;; Complex one also works well
(pp (compile '(= (+ (- 5 2) (* 1 2)) 5)
             'val 'next))
(()
 (arg1 arg2 val)
 ((assign arg1 (const 5))
  (assign arg2 (const 2))
  (assign arg1 (op -) (reg arg1) (reg arg2))
  (save arg1)
  (assign arg1 (const 1))
  (assign arg2 (const 2))
  (assign arg2 (op *) (reg arg1) (reg arg2))
  (restore arg1)
  (assign arg1 (op +) (reg arg1) (reg arg2))
  (assign arg2 (const 5))
  (assign val (op =) (reg arg1) (reg arg2))))
;; c.
(pp (compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next))

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry55) (reg env))
  (goto (label after-lambda54))
  entry55
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign arg1 (op lookup-variable-value) (const n) (reg env))
  (assign arg2 (const 1))
  (assign val (op =) (reg arg1) (reg arg2))
  (test (op false?) (reg val))
  (branch (label false-branch57))
  true-branch58
  (assign val (const 1))
  (goto (reg continue))
  false-branch57
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (assign arg1 (op lookup-variable-value) (const n) (reg env))
  (assign arg2 (const 1))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch61))
  compiled-branch60
  (assign continue (label proc-return62))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return62
  (assign arg1 (reg val))
  (goto (label after-call59))
  primitive-branch61
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call59
  (restore env)
  (assign arg2 (op lookup-variable-value) (const n) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-if56
  after-lambda54
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

;; d.
;;; design phase

(compile '(+ 1 2 3 4) 'val 'next)

;; should spit out
(assign arg2 (const 4))
(assign arg1 (const 3))
(assign arg2 (op +) (reg arg1) (reg arg2))
(assign arg1 (const 2))
(assign arg2 (op +) (reg arg1) (reg arg2))
(assign arg1 (const 1))
(assign val (op +) (reg arg1) (reg arg2))

;; In general
(compile `(+ ,a1 ,a2 ,a3 ,a4) 'val 'next)

;; should produce
<compile a4; target to arg2; linkage to next; preserving env>
<compile a3; target to arg1; linkage to next; preserving env, arg2>
(assign arg2 (op +) (reg arg1) (reg arg2))
<compile a2; target to arg1; linkage to next; preserving env, arg2>
(assign arg2 (op +) (reg arg1) (reg arg2))
<compile a1; target to arg1; linkage to next; preserving arg2>
(assign target (op +) (reg arg1) (reg arg2))
<code for goto linkage point>


(define (compile-+ exp target linkage)
  (let ((operands (operands exp)))
    (let ((number-of-arguments (length operands)))
      (case number-of-arguments
        ((0)
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '() (list target))
          `((assign ,target (const 0)))))
        ((1)
         (compile (first-operand operands)
                  target linkage))
        (else
         (let ((operands (reverse operands)))
           (end-with-linkage
            linkage
            (preserving
             '(env)
             (compile (first-operand operands)
                      'arg2 'next)
             (apply-recursively- '+ target (rest-operands operands))))))))))

;; detector for +
(define (+? exp) (tagged-list? exp '+))
;; test for compile-+
(pp (compile '(+ 1 2 3 4) 'val 'next))
(()
 (arg2 arg1 val)
 ((assign arg2 (const 4))
  (assign arg1 (const 3))
  (assign arg2 (op +) (reg arg1) (reg arg2))
  (assign arg1 (const 2))
  (assign arg2 (op +) (reg arg1) (reg arg2))
  (assign arg1 (const 1))
  (assign val (op +) (reg arg1) (reg arg2))))

(define (apply-recursively- op target operands)
  (let* ((last-operand? (no-operands? (rest-operands operands)))
         (next-target
          (if last-operand?
              target
              'arg2))
         (code-for-next-op
          (preserving
           '(arg2)
           (compile (first-operand operands)
                    'arg1 'next)
           (make-instruction-sequence
            '(arg1 arg2) (list next-target)
            `((assign ,next-target (op ,op) (reg arg1) (reg arg2)))))))
    (if last-operand?
        code-for-next-op
        (preserving
         '(env)
         code-for-next-op
         (apply-recursively- op target (rest-operands operands))))))

;; detector for *
(define (*? exp) (tagged-list? exp '*))

(define (compile-* exp target linkage)
  (let ((operands (operands exp)))
    (let ((number-of-arguments (length operands)))
      (case number-of-arguments
        ((0)
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '() (list target))
          `((assign ,target (const 1)))))
        ((1)
         (compile (first-operand operands)
                  target linkage))
        (else
         (let ((operands (reverse operands)))
           (end-with-linkage
            linkage
            (preserving
             '(env)
             (compile (first-operand operands)
                      'arg2 'next)
             (apply-recursively- '* target (rest-operands operands))))))))))

;; Exercise 5.39
(define (lexical-address-lookup address env)
  (let ((val
         (list-ref
          (frame-values
           (frame-ref env (frame-number address)))
          (displacement-number address))))
    (if (eq? val '*unassigned*)
        (error "Unassigned variable:"
               (list-ref
                (frame-variables
                 (frame-ref env (frame-number address)))
                (displacement-number address)))
        val)))

(define (lexical-address-set! address val env)
  (set-car!
   (list-tail
    (frame-values
     (frame-ref env (frame-number address)))
    (displacement-number address))
   val))
;; ADT for environment
(define (frame-ref env index) (list-ref env index))

;; ADT for lexical-address
(define (make-lexical-address frame-num displacement-num)
  `(,frame-num ,displacement-num))
(define (frame-number address) (car address))
(define (displacement-number address) (cadr address))

;; Test for lexical-address-lookup
(define test-environment
  (extend-environment
   '(y z) '((* a b x) (+ c d x))
   (extend-environment
    '(a b c d e)
    '(*unassigned* *unassigned* *unassigned* *unassigned* *unassigned*)
    (extend-environment
     '(x y)
     '(3 4)
     the-empty-environment))))

;; variable x
(lexical-address-lookup '(2 0) test-environment)

;Value: 3

;; variable a
(lexical-address-lookup '(1 0) test-environment)

;Unassigned variable: a
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

;; setting the value for a
(lexical-address-set! '(1 0) 1 test-environment)

;Unspecified return value

(lexical-address-lookup '(1 0) test-environment)

;Value: 1

;; Exercise 5.40
;;; environemnt ADT
(define (extend-compile-time-env params env)
  (cons params env))

(define (compile exp target linkage env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage env))
        ((quoted? exp) (compile-quoted exp target linkage env))
        ((variable? exp)
         (compile-variable exp target linkage env))
        ((assignment? exp)
         (compile-assignment exp target linkage env))
        ((definition? exp)
         (compile-definition exp target linkage env))
        ((if? exp) (compile-if exp target linkage env))
        ((lambda? exp) (compile-lambda exp target linkage env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage env))
        ((cond? exp) (compile (cond->if exp) target linkage env))
        ;; ((=? exp) (compile-= exp target linkage env))
        ((+? exp) (compile-+ exp target linkage env))
        ((open-coded-prims? exp) =>
         (lambda (op-binding)
           (compile-open-coded-prim exp target linkage env (cadr op-binding))))
        ((application? exp)
         (compile-application exp target linkage env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-lambda-body exp proc-entry env)
  (let ((formals (lambda-parameters exp))
        (env (extend-compile-time-env formals env)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return env))))

(define (compile-sequence seq target linkage env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next env)
       (compile-sequence (rest-exps seq) target linkage env))))

;; Exercise 5.41
(define (find-variable var env)
  (let traverse-env ((current-env env)
                     (frame-num 0))
    (if (empty-env? current-env)
        'not-found
        (let traverse-frame ((current-vars (frame-vars current-env))
                             (displacement-num 0))
          (cond ((empty-vars? current-vars)
                 (traverse-env (enclosing-env current-env)
                               (1+ frame-num)))
                ((eq? var (first-var current-vars))
                 (make-lexical-address frame-num displacement-num))
                (else
                 (traverse-frame (rest-vars current-vars)
                                 (1+ displacement-num))))))))
;; ADT for compile-time-env
(define (frame-vars compile-time-env)
  (car compile-time-env))
(define (enclosing-env compile-time-env)
  (cdr compile-time-env))
(define (empty-env? compile-time-env) (null? compile-time-env))

;; ADT for compile-time-frame
(define (first-var compile-time-frame) (car compile-time-frame))
(define (rest-vars compile-time-frame) (cdr compile-time-frame))
(define (empty-vars? compile-time-frame) (null? compile-time-frame))

;; test for find-variable
(find-variable 'c '((y z) (a b c d e) (x y)))

;Value: (1 2)

(find-variable 'x '((y z) (a b c d e) (x y)))

;Value: (2 0)

(find-variable 'w '((y z) (a b c d e) (x y)))

;Value: not-found
                                        ;
;; Exercise 5.42
(define (compile-variable exp target linkage env)
  (let ((address (find-variable exp env)))
    (end-with-linkage
     linkage
     (if (eq? address 'not-found)
         (make-instruction-sequence
          '() (list-union '(env) (list target))
          `((assign env (op get-global-environment))
            (assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env))))
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,address)
                    (reg env))))))))

(define (compile-assignment exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next env))
        (address (find-variable var env)))
    (end-with-linkage
     linkage
     (if (eq? address 'not-found)
         (append-instruction-sequences
          get-value-code
          (make-instruction-sequence
           '(val) (list-union (list target) '(env))
           `((assign env (op get-global-environment))
             (perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))))
         (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
           '(env val) (list target)
           `((perform (op lexical-address-set!)
                      (const ,address)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))))))))

;;; Test for new compile-variable
(pp (compile
     '((lambda (x y)
         (lambda (a b c d e)
           ((lambda (y z) (* x y z))
            (* a b x)
            (+ c d x))))
       3
       4)
     'val 'next the-empty-compile-time-env))

((env)
 (env proc argl continue val)
 ((assign proc (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry4) (reg env))
  (goto (reg continue))
  entry4
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (a b c d e)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry6) (reg env))
  (goto (label after-lambda5))
  entry6
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (y z)) (reg argl) (reg env))
  (assign arg2 (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (op *) (reg arg1) (reg arg2))
  (assign arg1 (op lexical-address-lookup) (const (2 0)) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (goto (reg continue))
  after-lambda5
  (assign arg2 (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 3)) (reg env))
  (assign arg2 (op +) (reg arg1) (reg arg2))
  (assign arg1 (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign val (op +) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (assign arg2 (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign arg2 (op *) (reg arg1) (reg arg2))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch9))
  compiled-branch8
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch9
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call7
  after-lambda3
  after-lambda1
  (assign val (const 4))
  (assign argl (op list) (reg val))
  (assign val (const 3))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
  compiled-branch11
  (assign continue (label after-call10))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call10))

;;; Test for new compile-assignment
(pp (compile
     '((lambda (x y)
         (lambda (a b c d e)
           ((lambda (y z)
              (set! a 5)
              (* x y z))
            (* a b x)
            (+ c d x))))
       3
       4)
     'val 'next the-empty-compile-time-env))

((env)
 (env proc argl continue val)
 ((assign proc (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry4) (reg env))
  (goto (reg continue))
  entry4
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (a b c d e)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry6) (reg env))
  (goto (label after-lambda5))
  entry6
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (y z)) (reg argl) (reg env))
  (assign val (const 5))
  (perform (op lexical-address-set!) (const (1 0)) (reg val) (reg env))
  (assign val (const ok))
  (assign arg2 (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (op *) (reg arg1) (reg arg2))
  (assign arg1 (op lexical-address-lookup) (const (2 0)) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (goto (reg continue))
  after-lambda5
  (assign arg2 (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 3)) (reg env))
  (assign arg2 (op +) (reg arg1) (reg arg2))
  (assign arg1 (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign val (op +) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (assign arg2 (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign arg2 (op *) (reg arg1) (reg arg2))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch9))
  compiled-branch8
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch9
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call7
  after-lambda3
  after-lambda1
  (assign val (const 4))
  (assign argl (op list) (reg val))
  (assign val (const 3))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
  compiled-branch11
  (assign continue (label after-call10))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call10))

;; Exercise 5.43
;;; test for new feature!
(pp (compile
     '(lambda (x y)
        (define (test-internal x y z) z)
        (define y 5)
        (+ y (test-internal x y 3)))
     'val 'next the-empty-compile-time-env))

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry4) (reg env))
  (goto (label after-lambda3))
  entry4
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (test-internal y)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry10) (reg env))
  (goto (label after-lambda9))
  entry10
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x y z)) (reg argl) (reg env))
  (assign val (op lexical-address-lookup) (const (0 2)) (reg env))
  (goto (reg continue))
  after-lambda9
  (perform (op lexical-address-set!) (const (0 0)) (reg val) (reg env))
  (assign val (const ok))
  (assign val (const 5))
  (perform (op lexical-address-set!) (const (0 1)) (reg val) (reg env))
  (assign val (const ok))
  (save continue)
  (save env)
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (const 3))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op lexical-address-lookup) (const (1 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch7))
  compiled-branch6
  (assign continue (label proc-return8))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return8
  (assign arg2 (reg val))
  (goto (label after-call5))
  primitive-branch7
  (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call5
  (restore env)
  (assign arg1 (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign val (op +) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-lambda3
  (assign val (const *unassigned*))
  (assign argl (op list) (reg val))
  (assign val (const *unassigned*))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
  compiled-branch12
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call11
  after-lambda1))

;; Exercise 5.44
(define (not-bound? var env)
  (eq? (find-variable var env) 'not-found))

;; Test for this new feature
(pp (compile
     '(lambda (+ * a b x y)
        (+ (* a x) (* b y)))
     'val 'next the-empty-compile-time-env))

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry13) (reg env))
  (goto (label after-lambda12))
  entry13
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (+ * a b x y)) (reg argl) (reg env))
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign val (op lexical-address-lookup) (const (0 5)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 3)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
  compiled-branch18
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call17
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign val (op lexical-address-lookup) (const (0 4)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 2)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
  compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call14
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
  compiled-branch21
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call20
  after-lambda12))

;; Section 5.5.7
(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))
(define (factorial-interpreted n)
    (if (= n 1)
        1
        (* (factorial-interpreted (- n 1)) n)))

;; Exercise 5.45b
(pp (compile
     '(define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n)))
     'val 'next
     the-empty-compile-time-env))

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry11) (reg env))
  (goto (label after-lambda10))
  entry11
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op =) (reg arg1) (reg arg2))
  (test (op false?) (reg val))
  (branch (label false-branch13))
  true-branch14
  (assign val (const 1))
  (goto (reg continue))
  false-branch13
  (save continue)
  (assign arg2 (op lexical-address-lookup) (const (0 0)) (reg env))
  (save arg2)
  (save env)
  (assign env (op get-global-environment))
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (restore env)
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
  compiled-branch16
  (assign continue (label proc-return18))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return18
  (assign arg1 (reg val))
  (goto (label after-call15))
  primitive-branch17
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call15
  (restore arg2)
  (assign val (op *) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-if12
  after-lambda10
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

;; Exercise 5.46

(compile-and-go
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

(pp (compile
     '(define (fib n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))
     'val 'next the-empty-compile-time-env))

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry23) (reg env))
  (goto (label after-lambda22))
  entry23
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (save env)
  (assign env (op get-global-environment))
  (assign proc (op lookup-variable-value) (const <) (reg env))
  (restore env)
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch37))
  compiled-branch36
  (assign continue (label after-call35))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch37
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call35
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch25))
  true-branch26
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (goto (reg continue))
  false-branch25
  (save continue)
  (save env)
  (save env)
  (assign env (op get-global-environment))
  (assign proc (op lookup-variable-value) (const fib) (reg env))
  (restore env)
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 2))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch33))
  compiled-branch32
  (assign continue (label proc-return34))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return34
  (assign arg2 (reg val))
  (goto (label after-call31))
  primitive-branch33
  (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call31
  (restore env)
  (save arg2)
  (save env)
  (assign env (op get-global-environment))
  (assign proc (op lookup-variable-value) (const fib) (reg env))
  (restore env)
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch29))
  compiled-branch28
  (assign continue (label proc-return30))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return30
  (assign arg1 (reg val))
  (goto (label after-call27))
  primitive-branch29
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call27
  (restore arg2)
  (assign val (op +) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-if24
  after-lambda22
  (perform (op define-variable!) (const fib) (reg val) (reg env))
  (assign val (const ok))))

(pp (compile
     '(define (fib n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))
     'val 'next the-empty-compile-time-env))
((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry18) (reg env))
  (goto (label after-lambda17))
  entry18
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value-in-frame) (const <) (reg env) (const 1))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch32))
  compiled-branch31
  (assign continue (label after-call30))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch32
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call30
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch20))
  true-branch21
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (goto (reg continue))
  false-branch20
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value-in-frame) (const fib) (reg env) (const 1))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 2))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch28))
  compiled-branch27
  (assign continue (label proc-return29))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return29
  (assign arg2 (reg val))
  (goto (label after-call26))
  primitive-branch28
  (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call26
  (restore env)
  (save arg2)
  (assign proc (op lookup-variable-value-in-frame) (const fib) (reg env) (const 1))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch24))
  compiled-branch23
  (assign continue (label proc-return25))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  proc-return25
  (assign arg1 (reg val))
  (goto (label after-call22))
  primitive-branch24
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call22
  (restore arg2)
  (assign val (op +) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-if19
  after-lambda17
  (perform (op define-variable!) (const fib) (reg val) (reg env))
  (assign val (const ok))))

;; Exercise 5.47
(compile-and-go
 '(define (f x)
    (g x)))

(pp (compile
     '(define (f x)
        (g x))
     'val 'next the-empty-compile-time-env))

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry8) (reg env))
  (goto (label after-lambda7))
  entry8
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value-in-frame) (const g) (reg env) (const 1))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
  (test (op compound-procedure?) (reg proc))
  (branch (label primitive-branch12))
  compound-branch10
  (save continue)
  (goto (reg compapp))
  compiled-branch11
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call9
  after-lambda7
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))))

;; Debugging we didn't updated the compound branch!
