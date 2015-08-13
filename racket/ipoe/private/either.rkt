#lang racket/base

(provide
  (struct-out success)
  (struct-out failure)
  either?
  ;; --
  assert-success
  either-monad
  on-failure
)

(require
  ipoe/private/ui
  (for-syntax racket/base syntax/parse)
)

;; =============================================================================
;; Data definition.
;;  An 'either' structure communicates success or failure, and includes
;;  the reason for failure as a user-presentable string.

(struct success (
  src ;; Symbol
))

(struct failure (
  src ;; Symbol
  reason ;; String
))

;; -----------------------------------------------------------------------------

(define (either? x)
  (or (success? x)
      (failure? x)))

(define-syntax-rule (either-error src e)
  (let ([s (string->symbol (format "ipoe:either:~a" src))])
    (error s (format "Expected an `either?` value, got ~a" e))))

(define (assert-success expr #:src src)
  (cond
    [(success? expr)
      expr]
    [(failure? expr)
      (define loc (string->symbol (format "ipoe:~a:~a" src (failure-src expr))))
      (user-error loc (failure-reason expr))]
    [else
      (either-error 'assert-success expr)]))

(define-syntax (either-monad stx)
  (syntax-parse stx
    [(_ e)
     #'(cond [(either? e) => (lambda (x) x)]
             [else        => (lambda (x) (either-error 'either-monad x))])]
    [(_ e f f* ...)
     #'(cond [(failure? e) => (lambda (x) x)]
             [(success? e) => (lambda (x) (either-monad (f x) f* ...))]
             [else         => (lambda (x) (either-error 'either-monad x))])]))

(define (on-failure expr f-thunk)
  (cond
    [(success? expr)
     expr]
    [(failure? expr)
     (f-thunk expr)]
    [else
     (either-error 'on-failure expr)]))

;; =============================================================================

(module+ test
  (require rackunit "rackunit-abbrevs.rkt")

  ;; -- either?
  (check-true* either?
    [(success 'yes)]
    [(failure 'yes "because")])
  (check-false* either?
    [42]
    ["43"]
    ['yolo])

  ;; -- assert-success
  (let ([s (success 'yes)])
    (check-equal? (assert-success s #:src 'either-test) s))
  (check-exn (regexp "either-test.*myval")
             (lambda () (assert-success (failure 'myval "") #:src 'either-test)))
  (check-exn (regexp "either-test2.*myval2")
             (lambda () (assert-success (failure 'myval2 "") #:src 'either-test2)))
  (check-exn exn:fail? ;; TODO better predicate? The result depends on 'ui.rkt'
             (lambda () (assert-success 4 #:src 'either-test)))


  ;; -- either-monad
  ;; -- on-failure
  (let ([s (success 'gotcha)])
    (check-equal? (on-failure s (lambda (x) 1)) s))
  (check-equal? (on-failure (failure 'omf "oh my failure") (lambda (x) 1)) 1)
  (check-exn exn:fail? (lambda () (on-failure "ohno" (lambda (x) 1))))

)
