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
                         all-insts
                         entry-regs
                         stack-related-regs
                         reg-sources-table))))))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    (assemble controller-text machine
              (lambda (instructions
                       all-instructions registers-with-entry
                       stack-inst-regs reg-sources-table)
                ((machine 'install-instruction-sequence)
                 instructions)
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
        (register-sources-table '()))   ;initial value -- undefined
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
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
