#lang racket
(require "process.rkt")
(provide exec-epf & run)

(define-syntax (exec-epf epf)
  (transcribe-extended-process-form epf))

(define-syntax-rule (& . epf)
  (fork (λ () (exec-epf . epf))))

(define-syntax-rule (run . epf)
  (wait (& . epf)))

;;; ==============
;;; syntax helpers
;;; ==============
(begin-for-syntax
 ;; all transcribe- functions return syntax-objects

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
 (define (transcribe-process-form pf)
   (syntax-case pf (begin \| epf pipe)
     [(begin arg ...)   (transcribe-begin-process-form #'(arg ...))]
     [(\| arg    ...)   (transcribe-simple-pipeline #'(arg ...))]
     [(pipe arg  ...)   (transcribe-simple-pipeline #'(arg ...))]
     [(epf arg   ...)   (transcribe-extended-process-form #'(arg ...))]
     [(proc arg  ...) #'(apply exec-path `(proc arg ...))]
     [ _                (raise-syntax-error #f "Illegal process form" pf)]))

 (define (transcribe-begin-process-form body)
   #`(begin #,@body))

 (define (transcribe-simple-pipeline pfs)
   (syntax-case pfs ()
     [(pf ...)
      (let* [(chunks (reverse (map transcribe-process-form (syntax->list #'(pf ...)))))
             (last-pf (car chunks))
             (first-pfs (reverse (cdr chunks)))
             (begin-form?
              (λ (f) (equal? 'begin (syntax->datum (car (syntax->list f))))))
             (fork-chunk
              (λ (chunk) #`(fork/pipe (thunk #,chunk) #,(begin-form? chunk))))
             (forkers (map fork-chunk first-pfs))]
        (blockify `(,@forkers ,last-pf)))]))

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
