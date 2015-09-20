#lang racket/base

;; TODO
;; Maybe, tools for language builders. We'll see what we can abstract.

(require
  racket/match
  (only-in racket/string string-suffix?)
  (for-syntax racket/base))

;; =============================================================================

(define-syntax (not-implemented stx)
  (define loc (string->symbol (format "ipoe:~a" (syntax-line stx))))
  #`(raise-user-error '#,loc "Not implemented"))

;; -----------------------------------------------------------------------------

(define (ipoe-check fname)
  (not-implemented))

(define (ipoe-init arg*)
  ;; - Check if postgres is installed
  ;; - Check for a init files, an existing database
  ;; - Communicate lots
  (displayln "TODO"))

(define (ipoe-submit arg*)
  (not-implemented))

;; -----------------------------------------------------------------------------

(define HELP-STR "HELP MESSAGE")

(define (print-help)
  (displayln HELP-STR))

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
        ;; scrape?
        ;; query?
        ;; get-rhymes? (full report?)
        ;; add-poem
        ;; help(poem)
        ;; list poems
        ['init
         (ipoe-init (cdr ARG*))]
        ['submit
         (ipoe-submit (cdr ARG*))]
        [rkt #:when (string-suffix? (car ARG*) ".rkt")
         (ipoe-check rkt)]
        [k
         (print-unknown k)]))))

;; =============================================================================

(module+ test
  (require rackunit)

)
