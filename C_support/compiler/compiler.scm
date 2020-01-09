(load "../../Exercise/ch5-syntax")

;; TEST compiles
;; (scheme->c '(begin (define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))) (factorial 10)) "factorial.c")
;; (scheme->c '(begin (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)) "fibonacci.c")

(define (scheme->c exp outfile)
  (let ((out (open-output-file (string-append "compiled/" outfile)))
        (joiner (string-joiner 'infix "\n"))
        (preamble (list "#include \"../src/sicp.h\"" "#include \"../src/type.h\"" "#include \"../../include/dbg.h\""))
        (entry-start (list "Object entry()" "{"))
        (entry-contents (statements (compile exp 'val 'next)))
        (entry-end (list "return val;" "}"))
        (main-start (list "int main()" "{"))
        (main-contents
         (list
          "check_mem(heap_Create(30000));" "setup_obarray();"
          "setup_environment();" "initialize_stack();" "env = global_env;"
          "user_print(entry());" "destroy_obarray();" "heap_Destroy();"
          "return 0;" "error:" "heap_Destroy();" "return 1;"))
        (main-end (list "}")))
    (for-each
     (lambda (commands)
       (display (apply joiner commands) out)
       (display "\n" out))
     (list preamble entry-start entry-contents entry-end main-start main-contents main-end))
    (close-port out)))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(cont) '()
          (list "goto *label(cont);")))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          (list (format #f "goto ~a;" linkage))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(cont)
   instruction-sequence
   (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    (list (format #f "~a = ~a;" target (self2c exp))))))

(define (self2c exp)
  (cond ((symbol? exp)
         (format #f "intern(\"~a\")" exp))
        ((string? exp)
         (format #f "make_string_obj(~s)" exp))
        ((integer? exp)
         (format #f "(Object) {.type = OB_EXACT, .data = ~a}" exp))
        ((number? exp)
         (format #f "make_double_obj(~a)" (exact->inexact exp)))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    (unwind-pair (text-of-quotation exp) (list (format #f "~a = unev;" target))))))

(define (unwind-pair exp unwinded)
  (cond ((null? exp)
         (cons "unev = nil;" unwinded))
        ((pair? exp)
         (unwind-pair (cdr exp)
                      (cons "save(unev);"
                            (unwind-pair (car exp)
                                         (cons "unev = cons(unev, restore());"
                                               unwinded)))))
        (else
         (cons (format #f "unev = ~a;" (self2c exp)) unwinded))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
                              (list (format #f "~a = lookup_variable_value(~a, env);"
                                            target
                                            (self2c exp))))))


(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
                                 (list (format #f "~a = set_variable_value(~a, val, env);"
                                               target (self2c var))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       (list (format #f "~a = define_variable(~a, val, env);" target (self2c var))))))))

;;;labels (from footnote)
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (string-replace (symbol->string name) #\- #\_)
                   (number->string (new-label-number)))))
;; end of footnote

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env cont)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           (list (format #f "if (!true_p(val)) goto ~a;" f-branch)))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env cont)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          (list (format
             #f
             "~a = make_compiled_procedure(make_label(&&~a), env);"
             target proc-entry))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      (cons (format #f "~a:" proc-entry)
            (cons "env = compiled_procedure_env(proc);"
                  (unwind-pair
                   formals
                   (list "env = extend_environment(unev, argl, env);")))))
     (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env cont)
     proc-code
     (preserving '(proc cont)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '("argl = nil;"))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 (list "argl = cons(val, nil);")))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           (list "argl = cons(val, argl);")))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        (list
         (format #f "if (proc.type == OB_PRIMITVE) goto ~a;" primitive-branch)))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           (list
            (format #f "~a = apply_primitive_procedure(proc, argl);" target))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          (list (format #f "cont = make_label(&&~a);" linkage)
                "val = compiled_procedure_entry(proc);"
                "goto *label(val);")))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            (list (format #f "cont = make_label(&&~a);" proc-return)
                  "val = compiled_procedure_entry(proc);"
                  "goto *label(val);"
                  (format #f "~a:" proc-return)
                  (format #f "~a = val;" target)
                  (format #f "goto ~a;" linkage)))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc cont) all-regs
          (list "val = compiled_procedure_entry(proc);"
                "goto *label(val);")))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;; footnote
(define all-regs '(env proc val argl cont))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list (format #f "~a:" s)) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving
             (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append (list (format #f "save(~a);" first-reg))
                      (statements seq1)
                      (list (format #f "~a = restore();" first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq)))) ;

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))
