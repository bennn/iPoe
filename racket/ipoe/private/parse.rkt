#lang racket/base

;; Generic parsing tools

(provide
  to-lines
  ;; (-> (U Input-Port String (Listof String)) (Listof String))
  ;; Convert an input source to a list of newline-separated strings
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/port port->lines)
  (only-in racket/string string-split)
)

;; =============================================================================

(define (to-lines arg)
  (cond
    [(input-port? arg)
     (port->lines arg)]
    [(string? arg)
     (string->lines arg)]
    [(list? arg)
     (list->lines arg)]
    [else
     (error 'parse:to-lines (format "Cannot coerce argument '~a' to newline-separated strings."))]))


(define (string->lines arg)
  (string-split arg "\n" #:trim? #f))

;; 2015-07-30: Defined only for (Listof String)
(define (list->lines arg)
  (apply append
         (for/list ([x (in-list arg)])
           (string->lines x))))

;; =============================================================================

(module+ test
  (require rackunit)

  (define-syntax-rule (check-to-lines [in out] ...)
    (begin (check-equal? (to-lines in) out) ...))

  (check-to-lines
    ["" '()] ;; Racket special case.. not sure how I feel about this
    ["a\nb" '("a" "b")]
    [" \n " '(" " " ")]

    ['("yes") '("yes")]
    ['("yo\nlo" "we\npa") '("yo" "lo" "we" "pa")]
  )
)
