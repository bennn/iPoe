#lang racket/base

(provide
  if-windows
)

(require
  (for-syntax
    racket/base
    syntax/parse
    (only-in racket/list last)
))

;; =============================================================================

(define-syntax (if-windows stx)
  (syntax-parse stx
   [(_ yes no)
    (if (windows?)
      (syntax/loc stx yes)
      (syntax/loc stx no))]
   [_ (error 'if-windows
        (format "Expected (if-windows YES NO), got '~a'" (syntax->datum stx)))]))

(begin-for-syntax
  ;; Check if we're on a Unix-like system
  (define (unix?)
    (string=? (path->string
                (last
                  (explode-path (find-system-path 'init-file))))
              ".racketrc"))

  (define (windows?)
    (not (unix?)))

  (define gnu
    windows?)
)
