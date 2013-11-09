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
 (define (transcribe-process-form pf)
   (syntax-case pf (begin \| epf pipe)
     [(begin arg ...)   (transcribe-begin-process-form #'(arg ...))]
     [(\| arg    ...)   (transcribe-simple-pipeline #'(arg ...))]
     [(pipe arg  ...)   (transcribe-simple-pipeline #'(arg ...))]
     [(epf arg   ...)   (transcribe-extended-process-form #'(arg ...))]
     [(proc arg  ...) #'(apply exec-path `(proc arg ...))]
     [ _                (raise-syntax-error #f "Illegal process form" pf)]))

 (define (transcribe-begin-process-form body)
   ;;   #'(with-stdio-ports* (thunk . body))
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
              (λ (chunk) #`(fork/pipe (thunk #,chunk) (begin-form? chunk))))
             (forkers (map fork-chunk first-pfs))]
        (blockify `(,@forkers ,last-pf)))]))

 ;; ;; syntax -> syntax
 ;; (define (transcribe-simple-pipeline pfs)
 ;;   (syntax-case pfs ()
 ;;     [(pf ...)
 ;;      (let* [(chunks (reverse (map transcribe-process-form (syntax->list #'(pf ...)))))
 ;;             (last-pf (car chunks))
 ;;             (first-pfs (reverse (cdr chunks)))
 ;;             (forkers (map (λ (chunk)
 ;;                              #`(fork/pipe (thunk #,chunk)))
 ;;                           first-pfs))]
 ;;        (blockify `(,@forkers ,last-pf)))]))

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

(define shell-open (compose displayln list))
;;(define fork/pipe (compose displayln list))

(define (fork/pipe stuff [inline? #f])

  ;; another possibility is running (begin ..) code
  ;; in a new Racket process (subprocess . . . "racket" "-e" ...)
  ;; spawning another VM could be too much overhead

  (if inline?
      (let-values ([(i o) (make-pipe)])
        (parameterize ((current-output-port o))
          (stuff)
          (close-output-port o))
        (current-input-port i))

      (let-values ([(proc <-ch) (stuff)])
        (current-input-port <-ch))))

(define (exec-path prog . args)

  (define prog-path (path->string (find-executable-path (stringify prog))))
  (define arglist (map stringify args))


  (let ((prog-path (path->string (find-executable-path "ls")))
        (arglist '()))

    (define-values (p <-ch _ __)
      (apply subprocess #f (current-input-port) #f prog-path arglist))

    (current-input-port <-ch)
    (for/list ((line (in-lines <-ch)))
      line))

  ;; (define-values (p <-ch out err)
  ;;   (apply subprocess #f #f #f prog-path arglist))





  )

(define (stringify dat)
  (let ((o (open-output-string)))
    (display dat o)
    (get-output-string o)))

;; (define (exec-path prog . rest)
;;   (displayln (append (list "executing" prog) rest)))

;;; Test-cases
;;; ==========
(define infile "Input.file")
(define errfile "Err.file")
(define-runtime-path skish-dir ".")

(define (show stx)
  (printf "~n~a~n~a~nExpansion:~n~a"
          (pretty-format (syntax->datum stx))
          (list->string (build-list 75 (λ (n) #\_)))
          (pretty-format (syntax->datum (expand/hide stx '())))))

(define (test1 stx)
  (show stx)
  (printf "~nResult:~n")
  (eval-syntax stx))

;; (test) will run all cases in test-cases
;; (test 2 4 5) will run only these cases
(define (test . nlist)

  (for/list ((n (in-list nlist)))
    (test1 (list-ref test-cases n))))

(define test-cases
  (list
   ;;case0
   #'(exec-epf ((\| (tail -10) (cat) (grep ".rkt")) (< ,infile) (>> 2 ,errfile)))

   ;;case1
   #'(exec-epf
      ((begin
         (let* ((input-line "Some lone")
                (fmtline (format "Hello ~a" input-line)))
           (exec-epf ((\| (tail -10) (cat) (grep ".rkt")) (< ,infile) (>> 2 ,errfile))))
         )))

   ;;case2
   #'(exec-epf
      ((ls)))

   ;;case3
   #'(exec-epf
      ((ls ,(path->string skish-dir))))

   ;;case4
   #'(exec-epf
      ((\| (ls -a) (grep "rkt"))))

   ;;case5
   #'(exec-epf
      ((begin
         (displayln "Hello world")) (> OUTFILE)))

   ;;case6
   #'(exec-epf
      ((pipe (ls)
             (begin (display (read-line)))
             (cat))))
   ))
