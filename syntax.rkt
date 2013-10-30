#lang racket
(require macro-debugger/expand)
(require (for-syntax racket/pretty))

(define-syntax (exec-epf epf)
  (let* ([code (transcribe-extended-process-form epf)]
         [code-string (pretty-format (syntax->datum code))]
         [epf-string (pretty-format (cadr (syntax->datum epf)))]
         [delim-string (list->string (build-list (string-length epf-string) (λ (n) #\-)))])
    (displayln
     (format "Transcribing:~n ~a ~n ~a ~n ~a ~n ~a"
             epf-string
             delim-string
             code-string
             delim-string))
    (datum->syntax code "" code)))

;; (define-syntax (exec-epf epf)
;;   (let ([code (transcribe-extended-process-form epf)])
;;     (pretty-print (syntax->datum code))
;;     code))


(define-syntax-rule (& . epf)
  (fork (λ () (exec-epf . epf))))

(define-syntax-rule (run . epf)
  (wait (& . epf)))

;; syntax-helpers
(begin-for-syntax

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

 ;; all transcribe- functions must return syntax-objects
 (define (transcribe-extended-process-form epf)
   (syntax-case epf ()
     ([_ (pf redir ...)]
      (let ([redirs (map transcribe-redirection (syntax->list #'(redir ...)))]
            [pf (transcribe-process-form #'pf)])
        (or pf
            (not (null? redirs))
            (raise-syntax-error #f "empty extended process form" epf))
        (blockify `(,@redirs ,pf))))))

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
   #'(with-stdio-ports* (thunk . body)))

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
     [(< fdes fname) #'(shell-open `fname read fdes)] ;shell-open calls s48-specific file-open - replace with racket
     [(< fname)      #'(shell-open `fname read 0)]

     [(> fdes fname) #'(shell-open `fname create+trunc fdes)]
     [(> fname)      #'(shell-open `fname create+trunc 1)]

     [(>> fdes fname) #'(shell-open `fname write+append+create fdes)]
     [(>> fname)      #'(shell-open `fname write+append+create 1)]

     ;; input comes from the printed representation of exp result
     ;; exp-value is converted to its printed representation using the display
     [(<< fdes exp) #'(move->fdes (open-string-source exp) fdes)] ;move->fdes is just dup2
     [(<< exp)      #'(move->fdes (open-string-source exp) 0)]

     [(= fdes fdes/port) #'(dup->fdes `fdes/port fdes)] ;dup->fdes is dup2 with some crazy port manipulation
     [(- fdes/port)      #'(close `fdes/port)]
     [ stdports          #'(stdports->stdio)]
     [ _                 (raise-syntax-error #f "Unknown I/O redirection" redir)]))

 ;; begin-for-syntax END
 )


;;debugging and testing
(define infile "ReadFromFile")
(define errfile "Errfile")
(exec-epf ((\| (tail -10) (cat) (grep ".rkt")) (< ,infile) (>> 2 ,errfile)))








;; (define (close port/fd)
;;   ((cond ((integer? port/fd)     close-fdes) ;low level fdes manipulation
;;          ((output-port? port/fd) close-output-port)
;;          ((input-port?  port/fd) close-input-port)
;;          (else (raise-argument-error '"- or close" "integer file-descriptor or port" port/fd)))
;;    port/fd))

;; (define (stdports->stdio)
;;   (dup (current-input-port)  0)
;;   (dup (current-output-port) 1)
;;   (dup (current-error-port)   2))

;; (define (open-string-source obj)
;;   (receive (inp outp) (temp-file-channel) ;s48 hack, replace with Racket (make-pipe)
;;     (display obj outp)
;;     (close-output-port outp)
;;     inp))

;; (define (temp-file-channel)
;;   (let* ((fname (create-temp-file))
;;          (iport (open-input-file fname))
;;          (oport (open-output-file fname)))
;;     (delete-file fname)
;;     (values iport oport)))
