#lang racket/base

(provide
  check-print
  ;; (check-print f str)
  ;; Run the thunk `f` and assert that it prints output matching `str` to
  ;;  the current output port.
)

;; -----------------------------------------------------------------------------

(require
  rackunit
  (only-in racket/port port->string port->lines)
)

;; =============================================================================

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
    (with-check-info (['expected spec] ['actual ln*])
      (check-equal? (length ln*) (length spec)))
    (for ([r (in-list spec)]
          [s (in-list ln*)])
      (check-true (regexp-match? r s)))
    res]
   [else
    (error 'ipoe:check-print "Cannot understand spec '~a'\n" spec)]))


;;; =============================================================================

(module+ test

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
