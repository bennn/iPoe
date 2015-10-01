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

  check-print
  ;; (check-print f str)
  ;; Run the thunk `f` and assert that it prints output matching `str` to
  ;;  the current output port.
)

;; -----------------------------------------------------------------------------

(require
  rackunit
  (only-in racket/port port->string port->lines)
  (for-syntax racket/base syntax/parse rackunit syntax/stx)
)

;; =============================================================================

;;bg; copied from rackunit library (location.rkt)
(define-for-syntax (syntax->location stx)
  (list (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

(define-syntax (check-true* stx)
  (syntax-parse stx
    [(_ f [arg* ...] ...+)
     (define loc (syntax->location stx))
     (quasisyntax/loc stx
       (with-check-info* (list (make-check-location '#,loc))
         (lambda () (check-true (f arg* ...)) ...)))]
    [_ (error 'check-true* "Expected (check-true* f [arg* ...] ...). In other words, a function and parentheses-delimited lists of arguments.")]))

(define-syntax (check-false* stx)
  (syntax-parse stx
    [(_ f [arg* ...] ...+)
     (define loc (syntax->location stx))
     (quasisyntax/loc stx
       (with-check-info* (list (make-check-location '#,loc))
         (lambda () (check-false (f arg* ...)) ...)))]
    [_ (error 'check-false* "Expected (check-false* f [arg* ...] ...). In other words, a function and parentheses-delimited lists of arguments.")]))

(define-syntax (check-apply* stx)
  (define loc (syntax->location stx))
  (syntax-parse stx #:datum-literals (== !=)
    [(_ f [arg* ... (~or != ==) res] ...+)
     ;; Well-formed call, map each [arg ... res] to a check
     (quasisyntax/loc stx
       (with-check-info* (list (make-check-location '#,loc))
         (lambda ()
           #,@(stx-map
             (lambda (s)
               (syntax-parse s #:datum-literals (== !=)
                [[arg* ... == res]
                 (syntax/loc stx (check-equal? (f arg* ...) res))]
                [[arg* ... != res]
                 (syntax/loc stx (check-not-equal? (f arg* ...) res))]
                [_
                 (syntax/loc stx (void))]))
           stx))))]
    [_ (error 'check-apply* "Expected (check-apply* f [arg* ... == res] ...) or (check-apply* f [arg* ... != res] ...). In other words, a function and parentheses-delimited lists of arguments & equality or dis-equality symbol & a result value to compare with.")]))

(define (check-print spec f)
  (define-values [in out] (make-pipe))
  (define res
    (parameterize ([current-output-port out])
      (let ([tmp (f)])
        (close-output-port out)
        tmp)))
  (cond
   [(string? spec)
    (check-equal? (port->string in) spec)
    res]
   [(list? spec)
    (define ln* (port->lines in))
    (check-equal? (length ln*) (length spec))
    (for ([r (in-list spec)]
          [s (in-list ln*)])
      (check-true (regexp-match? r s)))
    res]
   [else
    (error 'ipoe:check-print "Cannot understand spec '~a'\n" spec)]))


;;; =============================================================================

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

  (check-apply* (lambda (x) x)
    [1 != 2]
    [1 != 3]
    [1 != -1]
  )

  (check-apply* (lambda (x) x)
    [1 == 1]
    [1 != 2]
    [2 == 2]
    [3 != 0])

  ;; -- check-print
  (let ([msg ""])
    (check-equal?
      (check-print
        msg
        (lambda () (display msg)))
      (void)))

  (let ([msg "hello world"])
    (check-equal?
      (check-print
        msg
        (lambda () (display msg)))
      (void)))

  (let ([msg 7])
    (check-equal?
      (check-print
        (format "~a\n" msg)
        (lambda () (displayln msg) 3))
      3))

  (let ([r1 "^0"] [r2 "^1"] [r3 "^2"])
    (check-print
      (list r1 r2 r3)
      (lambda ()
        (for ([i (in-range 3)]) (printf "~a ~a\n" i (gensym))))))

  ;; -- Syntax error to forget function, or test cases
;  (define-syntax-rule (test-syntax-error m ...)
;    (begin
;     (begin
;      (check-exn #rx"check-"
;        (lambda () (m (lambda () x))))
;      (check-exn #rx"check-"
;        (lambda () (m [#t])))) ...))
;
;  (test-syntax-error check-true* check-false* check-apply*)
)
