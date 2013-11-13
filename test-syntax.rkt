#lang racket
(require macro-debugger/expand
         macro-debugger/stepper-text
         (for-syntax racket/pretty)
         racket/runtime-path)
(require "syntax.rkt")

(provide test test/show test-cases)

;;; ==========
;;; How to run
;;; ==========
;;; 1. add your test cases to test-cases list
;;; 2. (module+ test (test n m ...))
;;;    n, m, ... index into test-cases pointing at your cases
;;; 3. >> raco test test-syntax.rkt

;;; ==========================
;;; Test output is of the form
;;; ==========================
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


(module+ test

  ;; no IO redirection yet
  ;; (test 1)                              ;string-port example, only file-stream ports work for now
  (test 2 3 4 7)
  (test 6 8)

  )

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
                    (printf "ALL DONE~n")))))))

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


(define-runtime-path skish-dir ".")

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
         (copy-port (current-input-port)
                    (current-output-port)))))
