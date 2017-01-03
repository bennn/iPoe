#lang racket/base

;; Endpoint of `raco ipoe`

(require
  racket/match
  (prefix-in cmd: ipoe/command)
  (only-in racket/string string-suffix?)
  (for-syntax racket/base)
)

;; =============================================================================

(define print-help
  (let* ([pad-width (for/fold ([acc 0]) ([d (in-list cmd:command-descriptions)])
                      (max acc (string-length (car d))))]
         [add-padding (λ (sym) (format "  ~a~a  " sym (make-string (- pad-width (string-length sym)) #\space)))])
    (λ ()
      (printf "Usage: raco ipoe <command> <arg> ...~n~n")
      (printf "Commands:~n")
      (for ([d (in-list cmd:command-descriptions)])
        (printf "~a~a~n" (add-padding (car d)) (cdr d)))
      (void))))

(define (print-unknown k)
  (printf "Unrecognized command '~a'. Use `raco ipoe --help` to see a list of available commands.\n" k))

;; =============================================================================

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
        [rkt
         #:when (string-suffix? (car ARG*) ".rkt")
         ;; Shortcut for checking, instead of 'racket FILE.rkt'
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
        [(or 'show 'list)
         (cmd:show (cdr ARG*))]
        ['update
         (cmd:update (cdr ARG*))]
        [k
         (print-unknown k)]))))

;; =============================================================================

(module+ test
  (require rackunit)

)
