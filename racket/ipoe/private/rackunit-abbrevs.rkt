#lang racket/base

(provide
  check-true*
  ;; (check-true* f [arg* ...] ...)
  ;; Desugar into `check-true` tests applying `f` to arguments `[arg* ...]`

  check-false*
  ;; (check-false* f [arg* ...] ...)
  ;; Desugar into `check-false` tests applying `f` to arguments `[arg* ...]`

  check-apply*
  ;; (check-apply* f [arg* ... == res] ...)
  ;; Desugar into `check-equal?` matching `f arg* ...` against `res`

)

;; -----------------------------------------------------------------------------

(require
  rackunit
  (for-syntax racket/base syntax/parse)
)

;; =============================================================================

(define-syntax (check-true* stx)
  (syntax-parse stx
    [(_ f [arg* ...] ...)
     #'(begin (check-true (f arg* ...)) ...)]
    [_ (error 'check-true* "Expected (check-true* f [arg* ...] ...). In other words, a function and parentheses-delimited lists of arguments.")]))

(define-syntax (check-false* stx)
  (syntax-parse stx
    [(_ f [arg* ...] ...)
     #'(begin (check-false (f arg* ...)) ...)]
    [_ (error 'check-false* "Expected (check-false* f [arg* ...] ...). In other words, a function and parentheses-delimited lists of arguments.")]))

(define-syntax (check-apply* stx)
  (syntax-parse stx #:datum-literals (==)
    [(_ f [arg* ... == res] ...)
     #'(begin (check-equal? (f arg* ...) res) ...)]
    [_ (error 'check-apply* "Expected (check-apply* f [arg* ... == res] ...). In other words, a function and parentheses-delimited lists of arguments & an == sign & a result value to compare with.")]))

;; =============================================================================

(module+ test

  ;; -- check-true
  (check-true* (lambda (x) x)
    [#t]
    [#t]
  )

  (check-true* (lambda (x y) x)
    [#t #f]
    [#t 'a]
    [#t "yolo"]
  )

  (check-true* (lambda (x #:other-arg y) y)
    [#f #:other-arg #t]
  )

  (check-true* (lambda (x . ys*) (for/and ([y (in-list ys*)]) y))
    [#f #t]
    [#f #t #t #t]
    [#f #t #t #t #t]
  )

  ;; -- check-false*
  (check-false* (lambda (x) x)
    [#f]
    [#f]
  )

  (check-false* (lambda (x y) x)
    [#f #f]
    [#f 'a]
    [#f "yolo"]
  )

  (check-false* (lambda (x #:other-arg y) y)
    [#t #:other-arg #f]
  )

  (check-false* (lambda (x . ys*) (for/and ([y (in-list ys*)]) y))
    [#f #f]
    [#f #t #f #t]
    [#f #t #t #f #t]
  )

  ;; -- check-apply
  (check-apply* (lambda (x) x)
    [#t == #t]
    [#f == #f]
    ['A == 'A]
  )

  (check-apply* (lambda (x . y) (car y))
    ['A 'B '() == 'B]
  )

  (check-apply* (lambda ANY (cdr ANY))
    [1 2 3 4 == '(2 3 4)]
  )

  (check-apply* (lambda (a #:b b #:c [c #f]) (or c b))
    [1 #:b 2 == 2]
    [1 #:b 2 #:c 3 == 3]
  )

)
