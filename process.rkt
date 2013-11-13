#lang racket

(provide shell-open fork/pipe exec-path)

;;; TODO
;;; 1. (fork/pipe) is clearly hacky
;;; 2. (make-file-stream-input-port) wrapping Racket ports
;;;    into file-stream ports doesn't work, so passing string-port
;;;    into a pipe will break it
;;; 3. Move the code to use (process*/ports) instead of lower-level
;;;    primitive (subprocess)


;; a mock-up
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

;; hacked up version that doesn't even work
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
