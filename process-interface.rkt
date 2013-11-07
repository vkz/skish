#lang racket

;;; EXEC in the end all of these end up calling s48 `exec-with-alias'
;;; an omnibus function that subsumes all other exec functions
;;; `http://s48.org/0.57/manual/s48manual_66.html'

;;; everything can be built for these:
;;; `exec-with-alias'   (s48)
;;; `fork'              (ffi/syscall)
;;; `exit'              (ffi/syscall)

;;; Racket's `subprocess' == fork/pipe + exec-path roughly
;;; Racket's `system' and `process' have too much overhead, cause
;;; run command in a separate shell process


;;; Possible low-level implementation
;;; combine to get `subprocess' + `find-executable-path'

;; with prog lookup in the search-path
(define (exec/env prog env . arglist)
  (void))                               ;calls %exec

(define (exec prog . arglist)
  (void))                               ;calls exec/env with env=#f

;; no lookup when `/prog', lookup otherwise
(define (exec-path/env prog env . arglist)
  (void))                               ;calls %exec or
                                        ;exec-with-alias with lookup
(define (exec-path prog . arglist)
  (void))                               ;calls exec-path/env

;; no prog lookup in the search-path
(define (%exec prog arglist env)
  (void))
;; -> (or string #f)
(define (exec-path-search prog path-list)
  (void))                               ;find-executable-path in Racket

;; thunk -> proc (to parent) or #f (to child)
(define (fork . stuff)
  (void))

;;; Like FORK, but the parent and child communicate via a pipe connecting
;;; the parent's stdin to the child's stdout. This function side-effects
;;; the parent by changing his stdin. (see `really-fork/pipe')

;; thunk -> proc or #f
(define (fork/pipe . stuff)
  (void))

(define (exit #:status [status 0])
  (void))
