#lang racket/base

(provide
  (struct-out success)
  (struct-out failure)
  either?
  on-failure
  assert-success
)

(require
  ipoe/private/ui
)

;; =============================================================================

(struct success (
  src ;; Symbol
))

(struct failure (
  src ;; Symbol
  reason ;; String
))

(define (either? x)
  (or (success? x)
      (failure? x)))

(define (on-failure expr f-thunk)
  (unless (success? expr)
    (f-thunk expr)))

(define (assert-success expr #:src src)
  (unless (success? expr)
    (define loc (string->symbol (format "ipoe:~a:~a" src (failure-src expr))))
    (user-error loc (failure-reason expr))))

