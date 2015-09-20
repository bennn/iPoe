#lang racket/base

(provide
  (struct-out success)
  (struct-out failure)

  either?
  ;; Either success or failure.

  ;; --

  assert-success
  ;; (-> Any #:src Symbol Any)
  ;; Unless the argument is a success struct, raise an exception

  either-monad
  ;; #'(-> Syntax ... Any)
  ;; Run a sequence of operations.
  ;; If any is a `failure?`, return the failure value immediately.
  ;; Raise an exception if an intermediate computation doesn't return an either?

  on-failure
  ;; (-> Any (-> Any) Any)
  ;; If the first argument is a `failure?`, call the second argument.
  ;; Else return the first argument
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
  value ;; Any (this is a little reckless, but we expect the caller knows what to expect)
) #:transparent )

(struct failure (
  src ;; Symbol
  reason ;; String
) #:transparent )

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
     #'(let ([tmp e])
         (cond [(either? tmp) tmp]
               [else (either-error 'either-monad tmp)]))]
    [(_ e e* ...)
     #'(let ([tmp e])
         (cond [(failure? tmp) tmp]
               [(success? tmp) (either-monad e* ...)]
               [else (either-error 'either-monad tmp)]))]))

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
    [(success 'yes #t)]
    [(failure 'yes "because")])
  (check-false* either?
    [42]
    ["43"]
    ['yolo])

  ;; -- assert-success
  (let ([s (success 'yes #t)])
    (check-equal? (assert-success s #:src 'either-test) s))
  (check-exn (regexp "either-test.*myval")
             (lambda () (assert-success (failure 'myval "") #:src 'either-test)))
  (check-exn (regexp "either-test2.*myval2")
             (lambda () (assert-success (failure 'myval2 "") #:src 'either-test2)))
  (check-exn exn:fail? ;; TODO better predicate? The result depends on 'ui.rkt'
             (lambda () (assert-success 4 #:src 'either-test)))


  ;; -- either-monad
  (define-syntax-rule (test-either-monad [expr ...] result)
    (check-equal? (either-monad expr ...) result))
  (let ([fail (failure 'test-either "test either")]
        [suc1 (success 'test-either 1)]
        [suc2 (success 'test-either 2)])
    (test-either-monad [fail] fail)
    (test-either-monad [suc1] suc1)
    (test-either-monad [suc1 suc2] suc2)
    (test-either-monad [suc2 suc1] suc1)
    (test-either-monad [fail suc1 suc2] fail)
    (test-either-monad [suc2 fail suc1] fail)
    (test-either-monad [suc1 suc2 fail] fail)
    ;; The integer 1 in a "type error", but doesn't appear at runtime.
    (test-either-monad [fail 1] fail))

  (check-exn (regexp "^ipoe:either:either-monad")
             (lambda () (either-monad 1)))
  (check-exn (regexp "^ipoe:either:either-monad")
             (lambda () (either-monad (success 'a 1) (success 'b 2) 1 (success 'c 3))))

  ;; -- on-failure
  (let ([s (success 'gotcha #t)])
    (check-equal? (on-failure s (lambda (x) 1)) s))
  (check-equal? (on-failure (failure 'omf "oh my failure") (lambda (x) 1)) 1)
  (check-exn exn:fail? (lambda () (on-failure "ohno" (lambda (x) 1))))

)
