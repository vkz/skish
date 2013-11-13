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

;;; =====================
;;; Process machinery
;;; =====================

(define shell-open (compose displayln list))

;; this code assumes `in order' execution
;; perhaps it should be concurrent [???]
(define (fork/pipe stuff [inline? #f])
  (if inline?

      ;; stuff is (begin ...)
      ;; pipe its output back into
      (let* ([tempfile (make-temporary-file)]    ;[!HACK!] file's never deleted
             [tempin (open-input-file tempfile)]
             [tempout (open-output-file tempfile #:exists 'truncate)])
        (parameterize ((current-output-port tempout))
          (stuff)
          (close-output-port tempout))
        (current-input-port tempin))

      ;; stuff is (exec-path ...) i.e. external binary
      ;; [FIX] pipe its output back into (current-input-port)
      (let-values ([(proc <-ch) (stuff)])
        (current-input-port <-ch)
        (subprocess-wait proc))))

(define (make-file-stream-input-port port)
  (cond
   ((file-stream-port? port) port)
   (else
    (let* ([tempfile (make-temporary-file)]    ;[!HACK!] file's never deleted
           [tempin (open-input-file tempfile)]
           [tempout (open-output-file tempfile #:exists 'truncate)]
           [pumpth (thread (thunk (begin
                                    (displayln "THREAD HERE:")
                                    (copy-port port tempout)
                                    (close-output-port tempout)
                                    (displayln "THREAD FINISHED")
                                    )))])
      (sleep 1)
      (printf "SLEEP NO MORE, PASSING ~a~n" tempin)
      tempin))))

;; execute external binary in a new OS process
;; setting (current-input-port) to the child's output
;; prog arg ... -> subprocess? input-port?
(define (exec-path prog . args)
  (define prog-path (path->string (find-executable-path (stringify prog))))
  (define arglist (map stringify args))
  (define-values
    (proc <-ch _ __)
    (apply subprocess
           #f
           (make-file-stream-input-port (current-input-port))
           (current-error-port)
           prog-path
           arglist))
  (printf "~a LAUNCHED~N" prog-path)
  (current-input-port <-ch)
  (values proc <-ch))

(define (stringify dat)
  (let ((o (open-output-string)))
    (display dat o)
    (get-output-string o)))

;;; =====================
;;; Debugging and testing
;;; =====================
(define infile "Input.file")
(define errfile "Err.file")
(define-runtime-path skish-dir ".")

;; Test output is of the form:
;; ___________________________________________________________________________
;;
;; '(exec-epf ((\| (ls -a) (grep "rkt") (wc))))
;; ___________________________________________________________________________
;; Expansion:
;; '(begin
;;    (#%app fork/pipe (lambda () (#%app apply exec-path '(ls -a))) '#f)
;;    (#%app fork/pipe (lambda () (#%app apply exec-path '(grep "rkt"))) '#f)
;;    (#%app apply exec-path '(wc)))
;; Run:
;; 2       2      33

(define (show stx)
  (printf "~n~a~n~n~a~n~a~nExpansion:~n~a"
          (list->string (build-list 75 (λ (n) #\_)))
          (pretty-format (syntax->datum stx))
          (list->string (build-list 75 (λ (n) #\_)))
          (pretty-format (syntax->datum (expand/hide stx '())))))

(define (test1 stx run)
  (show stx)
  (and run
       (begin
         (printf "~nRun:~n")
         (eval-syntax stx)
         (copy-port (current-input-port) (current-output-port)))))

;; (test 4 5) show expansion and run cases 4 and 5
(define (test #:run [run #t] . nlist)
  (for/list ((n (in-list nlist)))
    (test1 (list-ref test-cases n) run)))


;; (test/show 4 5) only show expansion
(define test/show (curry test #:run #f))

(define test-cases
  (list
   ;;case0
   #'(exec-epf ((\| (tail -10) (cat) (grep ".rkt")) (< ,infile) (>> 2 ,errfile)))

   ;;case1
   #'(exec-epf
      ((begin
         (let* ((input-line "Some lone")
                (str (format "Hello ~a" input-line)))
           (with-input-from-string str
             (thunk (exec-epf ((\| (grep "one") (wc)) ))
                    (printf "ALL DONE~n"))))
         )))

   ;;case2
   #'(exec-epf
      ((ls)))

   ;;case3
   #'(exec-epf
      ((ls ,(path->string skish-dir))))

   ;;case4
   #'(exec-epf
      ((\| (ls -a) (grep "rkt") (wc))))

   ;;case5
   #'(exec-epf
      ((begin
         (displayln "Hello world")) (> OUTFILE)))

   ;;case6
   #'(exec-epf
      ((pipe (ls)
             (begin (copy-port (current-input-port) (current-output-port)))
             (wc))))

   ;;case7
   #'(exec-epf
      ((pipe (ls -a) (wc))))


   ;;case8
   #'(exec-epf
      ((pipe (ls)
             (begin (copy-port (current-input-port) (current-output-port)))
             )))

   ))

(module+ test

  ;; no IO redirection yet
  ;; (test 1)                              ;string-port example, only file-stream ports work for now
  (test 2 3 4 7)
  (test 6 8)

  )
