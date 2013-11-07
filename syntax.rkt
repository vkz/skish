#lang racket
(require macro-debugger/expand
         macro-debugger/stepper-text
         (for-syntax racket/pretty)
         racket/runtime-path)

(define-syntax (exec-epf epf)
  (transcribe-extended-process-form epf))

(define-syntax-rule (& . epf)
  (fork (λ () (exec-epf . epf))))

(define-syntax-rule (run . epf)
  (wait (& . epf)))

(begin-for-syntax

;;; ==================================================
;;; all transcribe- functions return syntax-objects
;;; ==================================================

 ;; map a list of expressions to a begin form, flattening nested begins
 ;; (list #'(begin 3 4) #'5 #'(begin 6) ) -> #'(begin 3 4 5 6)
 ;; syntax -> syntax
 (define (blockify exps)
   (let ([deblocked-exps (apply append (map deblock exps))])
     #`(begin #,@deblocked-exps)))

 ;; map an expression to a list of expressions, flattening begins
 ;; #'(begin (begin 3 4) 5 6 (begin 7 8)) => '(#'3 #'4 #'5 #'6 #'7 #'8)
 ;; syntax -> list-of syntax
 (define (deblock exp)
   (syntax-case exp (begin)
     [(begin e ...) (apply append (map deblock (syntax->list #'(e ...))))]
     [ begin (list)]
     [ e (list #'e)]))

 (define (transcribe-extended-process-form epf)
   (syntax-case epf ()
     [(_ (pf redir ...))
      (let ([redirs (map transcribe-redirection (syntax->list #'(redir ...)))]
            [pf (transcribe-process-form #'pf)])
        (or pf
            (not (null? redirs))
            (raise-syntax-error #f "empty extended process form" epf))
        (blockify `(,@redirs ,pf)))]))

 ;; Racket reader treats | special, escaping it for now with \|
 ;; syntax -> syntax
 (define (transcribe-process-form pf)
   (syntax-case pf (begin \| epf pipe)
     [(begin arg ...)   (transcribe-begin-process-form #'(arg ...))]
     [(\| arg    ...)   (transcribe-simple-pipeline #'(arg ...))]
     [(pipe arg  ...)   (transcribe-simple-pipeline #'(arg ...))]
     [(epf arg   ...)   (transcribe-extended-process-form #'(arg ...))]
     [(proc arg  ...) #'(apply exec-path `(proc arg ...))]
     [ _                (raise-syntax-error #f "Illegal process form" pf)]))

 ;; syntax -> syntax
 (define (transcribe-begin-process-form body)
   ;;   #'(with-stdio-ports* (thunk . body))
   #`(begin #,@body))

 ;; syntax -> syntax
 (define (transcribe-simple-pipeline pfs)
   (syntax-case pfs ()
     [(pf ...)
      (let* [(chunks (reverse (map transcribe-process-form (syntax->list #'(pf ...)))))
             (last-pf (car chunks))
             (first-pfs (reverse (cdr chunks)))
             (forkers (map (λ (chunk)
                              #`(fork/pipe (thunk #,chunk)))
                           first-pfs))]
        (blockify `(,@forkers ,last-pf)))]))

 ;; syntax -> syntax
 (define (transcribe-redirection redir)
   (syntax-case redir (< > << >> = - stdports)
     [(< fdes fname) #'(shell-open `fname 'read fdes)] ;shell-open is dup2 in scsh
     [(< fname)      #'(shell-open `fname 'read 0)]

     [(> fdes fname) #'(shell-open `fname 'create+trunc fdes)]
     [(> fname)      #'(shell-open `fname 'create+trunc 1)]

     [(>> fdes fname) #'(shell-open `fname 'write+append+create fdes)]
     [(>> fname)      #'(shell-open `fname 'write+append+create 1)]

     [(<< fdes exp) #'(move->fdes (open-string-source exp) fdes)]
     [(<< exp)      #'(move->fdes (open-string-source exp) 0)]

     [(= fdes fdes/port) #'(dup->fdes `fdes/port fdes)]
     [(- fdes/port)      #'(close `fdes/port)]
     [ stdports          #'(stdports->stdio)]

     [ _                 (raise-syntax-error #f "Unknown I/O redirection" redir)]))

 ) ;begin-for-syntax

;;; =====================
;;; Debugging and testing
;;; =====================
(define (show stx . hide)
  (printf "~n~a~n~a~nExpansion:~n~a"
          (pretty-format (syntax->datum stx))
          (list->string (build-list 75 (λ (n) #\_)))
          (pretty-format (syntax->datum (expand/hide stx hide)))))

(define (test1 stx . hide)
  (apply show stx hide)
  (printf "~nResult:~n")
  (eval-syntax stx))

(define shell-open (compose displayln list))
(define fork/pipe (compose displayln list))
(define (exec-path prog . rest) (displayln (append (list "executing" prog) rest)))

;;; Test-cases
;;; ==========
(define infile "ReadFromFile")
(define errfile "Errfile")
(define-runtime-path skish-dir ".")

;; (test) will run all cases in test-cases
;; (test 2 4 5) will run only these cases
(define (test . numlist)
  (if (not (empty? numlist))
      (for ((test (in-list numlist)))
        (test1 (list-ref test-cases (sub1 test))))
      (map test1 test-cases)))

(define test-cases
  (list
   ;;case1
   #'(exec-epf ((\| (tail -10) (cat) (grep ".rkt")) (< ,infile) (>> 2 ,errfile)))

   ;;case2
   #'(exec-epf
      ((begin
         (let* ((input-line "Some lone")
                (fmtline (format "Hello ~a" input-line)))
           (exec-epf ((\| (tail -10) (cat) (grep ".rkt")) (< ,infile) (>> 2 ,errfile))))
         )))

   ;;case3
   #'(exec-epf
      ((ls)))

   ;;case4
   #'(exec-epf
      ((ls ,(path->string skish-dir))))

   ;;case5
   #'(exec-epf
      ((\| (ls) (grep "rkt"))))

   ;;case6
   #'(exec-epf
      ((begin
         (displayln "Hello world")) (> OUTFILE)))

   ;;case7
   #'(exec-epf
      ((pipe (ls)
             (begin (display (read-line)))
             (cat))))
   ))
