#lang racket/base

;; TODO
;; Maybe, tools for language builders. We'll see what we can abstract.

(require
  racket/match
  (prefix-in cmd: ipoe/command)
  (only-in racket/string string-suffix?)
  (for-syntax racket/base)
)

;; =============================================================================

(define HELP-STR "HELP MESSAGE")

(define (print-help)
  (displayln HELP-STR))

(define (print-unknown k)
  (printf "Unrecognized command '~a'. Use `raco ipoe --help` to see a list of available commands.\n" k))

;; =============================================================================

;; Wanted aliases:
;; - show ~ list

(module+ main
  (require racket/cmdline)
  ;; -- parameters?
  (command-line
   #:program "ipoe"
   #:args ARG*
   (if (null? ARG*)
       (print-help)
       (match (string->symbol (car ARG*))
        ;; scrape? get-rhymes? (full report?, repl?)
        ;; share?
        ;; edit? audit? or build this into NEW 
        ['check
         (cmd:check (cdr ARG*))]
        [rkt ;; Shortcut for checking, instead of 'racket FILE.rkt'
         #:when (string-suffix? (car ARG*) ".rkt")
         (cmd:check ARG*)]
        [(or 'db 'dbshell)
         (cmd:dbshell (cdr ARG*))]
        ['help
         (print-help)]
        ['init
         (cmd:init (cdr ARG*))]
        [(or 'new 'add)
         (cmd:new (cdr ARG*))]
        ['remove
         (cmd:remove (cdr ARG*))]
        ['show  ;; TODO
         (cmd:show (cdr ARG*))]
        ['update
         (cmd:update (cdr ARG*))]
        [k
         (print-unknown k)]))))

;; =============================================================================

(module+ test
  (require rackunit)

)
