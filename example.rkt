#lang racket
(require racket/runtime-path
         "syntax.rkt")

;;; >> racket example.rkt

(define-runtime-path skish-dir ".")
(exec-epf ((pipe (ls -a ,skish-dir)
                 (grep rkt)
                 (wc)
                 (begin
                   (copy-port (current-input-port)
                              (current-output-port))))))
