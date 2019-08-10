;;; Part 2

;;;
;;;The following test cases explore the evaluation of simple expressions.
;;;

-37;Value: -37

(* 3 4);Value: 12

(> 10 9.7);Value: #t

(- (if (> 3 4)
       7
       10)
   (/ 16 10));Value: 42/5

(* (- 25 10)
   (+ 6 3));Value: 135

+;Value 2: #[arity-dispatched-procedure 2]

(define double (lambda (x) (* 2 x)));Value: double

double;Value 3: #[compound-procedure 3 double]

(define c 4);Value: c

c;Value: 4

;Editor error: Undefined command: C-M-x

(double (double (+ c 5)));Value: 36

(define times-2 double);Value: times-2

(times-2 c);Value: 8

(define d c);Value: d

(= c d);Value: #t

(cond ((>= c 2) d)
      ((= c (- d 5)) (+ c d))
      (else (abs (- c d))));Value: 4

;;; Part 3

;;;
;;; Practice to pretty-print code
;;;

(define abs
  (lambda (a)
    (if (> a 0)
	a
	(- a))))

;;; Do the same thing with replace <C-j> with <CR> followed by the <tab>.

(define abs
  (lambda (a)
    (if (> a 0)
	a
	(- a))))

;;; the result is same as previous one.

;;; Part 5: Documentation and Administrative Questions

;;; 1. According to the Don't Panic manual, how do you invoke the stepper?
;;; What is the difference between the stepper and the debugger?

;;; Answer:
;;; Type M-s instead of C-x C-e
;;; It's different the way of approaching to the error. The stepper evaluate each subexpression "step by step", 
;;; Whereas debugger give more informations about the error-- the environment, reductions in addition to subexpressions at that point.
;;; 
;;; More specifically, stepper approach to the error from the begining of evaluation; whereas debugger start from error point
;;; and then go to the above level.

;;; 2. According to the Guide to MIT Scheme, which of the words in the scheme expressions you evaluated in Part 2
;;; above are "special forms"?

;;; Answer:
;;; define, lambda if cond else

;;; 6.

;;; Building Abstractions
;;; Controlling Interaction through Conventional Inter faces, and
;;; Designing New Languages

;;; 7. What does the MIT Scheme Reference Manual say about treatment of upper and lower case in expressions?

;;; Scheme doesn't distinguish uppercase and lowercae forms of a letter except within character and string constants;
;;; scheme expression CASE INSENSITIVE.

;;; 8. What are the Edwin commands for creating a new file, and for saving a file? What is the difference between the
;;; *scheme* buffer and the *transcript* buffer?

;;; *scheme* buffer can be modified; it is just REPL console. More obviously, it just print the evaluated value to the buffer.
;;; On the other hand, *transcript* recored all the command that evaluated as well as the evaluated value. It CAN NOT be MODIFIED.
