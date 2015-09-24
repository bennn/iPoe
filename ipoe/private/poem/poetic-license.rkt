#lang racket/base

(provide
  (struct-out quirk)
  ;; The standard error format

  poetic-license-apply
  ;; (-> License (Listof Quirk) License)
  ;; Apply a poetic license to a list of quirks

  poetic-license-init
  ;; (-> Natural License)
  ;; Create a new poetic license.
  ;; Assumes that all parameters in `parameters.rkt` are accessible

  poetic-license-report
  ;; (-> License Void)
  ;; Print a report of all quirks we applied the license to
)

;; -----------------------------------------------------------------------------

(require
  racket/match
)

;; =============================================================================

;; A `quirk` represents something unexpected that we found while
;;  checking the poem.
;; i.e. spelling errors, stanza mismatches
;; 2015-09-23: They're not errors, they're quirks.
(struct quirk (
  >cost
  ;; (U #f Natural)
  ;; The amount of poetic license this quirk costs

  >string
  ;; String
  ;; Used to report a quirk to the user
) #:transparent )

(struct license (
  credit
  ;; (Boxof Natural)
  ;; Current license credits

  quirk*
  ;; (Boxof (Listof Quirk))
  ;; List of accepted quirks.
) #:transparent )

;; -----------------------------------------------------------------------------

;; Apply the poetic license to a list of quirks.
;; Raise an exception if the license has too little credit.
(define (poetic-license-apply L q*)
  (match q*
   ['() (void)]
   [(cons qh qt)
    (poetic-license-deduct L qh)
    (if (positive? (unbox (license-credit L)))
        (poetic-license-apply L qt)
        (poetic-license-error L qh))]))

;; Subtract the cost from the license
;; (: poetic-license-deduct (-> License Quirk Void))
(define (poetic-license-deduct L q)
  (define c (quirk->cost q))
  (define old-c (unbox (license-credit L)))
  (define new-c (- old-c (or c old-c)))
  (set-box! (license-credit L) new-c)
  (define old-q* (unbox (license-quirk* L)))
  (set-box! (license-quirk* L) (cons q old-q*)))

(define-syntax-rule (poetic-license-error L q)
  (raise-user-error 'ipoe (quirk->string q)))

;; Create a poetic license using dynamically-bound parameters
(define (poetic-license-init c)
  (license (box c)
           (box '())))

(define (poetic-license-report L)
  (displayln "Finished checking poem")
  (for ([q (in-list (reverse (unbox (license-quirk* L))))])
    (display "- ")
    (displayln (quirk->string q)))
  (printf "Remaining poetic license: ~a\n" (unbox (license-credit L))))

;; =============================================================================

(module+ test
  (require
    rackunit
    ipoe/private/util/rackunit-abbrevs
    (only-in racket/list make-list)
  )

  (define (poetic-license->cons L)
    (cons (unbox (license-credit L))
          (unbox (license-quirk* L))))

  ;; -- poetic-license-apply
  (define-syntax-rule (check-exn/license [L q] ...)
    (begin
      (check-exn #rx"ipoe"
        (lambda () (poetic-license-apply L (list q)))) ...))

  (let ([q0 (quirk 0 "hi")]
        [q1 (quirk 5 "ho")]
        [q2 (quirk 67 "hu")]
        [L1 (poetic-license-init 1)]
        [L2 (poetic-license-init 9000)])
  (check-apply* (lambda (L q*)
                  (poetic-license-apply L q*)
                  (poetic-license->cons L))
   [L1 (list q0)
    == (cons 1 (list q0))]
   [L2 (list q1 q1 q2)
    == (cons 8923 (list q2 q1 q1))])
  ;; --
  (check-exn/license
   [L1 (quirk 1 "ohnoes")]
   [L1 (quirk #f "yes")]
   [L1 (quirk 113541 "yolo")]
   [L2 (quirk #f "hosed")]))

  ;; -- poetic-license-deduct
  (check-apply* (lambda (L c)
                  (poetic-license-deduct L (quirk c "test"))
                  (poetic-license->cons L))
   [(poetic-license-init 5) 5
     == (cons 0 (list (quirk 5 "test")))]
   [(poetic-license-init -1) 5
     == (cons -6 (list (quirk 5 "test")))]
   [(poetic-license-init 9) 1
     == (cons 8 (list (quirk 1 "test")))])

  ;; -- poetic-license-init
  (check-apply* (compose1 poetic-license->cons poetic-license-init)
   [-1 == (cons -1 '())]
   [0 == (cons 0 '())]
   [99 == (cons 99 '())])

  ;; -- poetic-license-report
  (let ([q1 (quirk 0 "foo")]
        [q2 (quirk 1 "bar")]
        [q3 (quirk 2 "baz")])
    ;; -- Should have 1 line for each quirk, plus 2 extras
    (let ([L1 (license (box 1) (box (list q1 q2 q3)))])
      (check-equal?
        (check-print
          (make-list 5 #rx"")
          (lambda () (poetic-license-report L1)))
        (void)))
    ;; -- Only the 2 'extra' lines
    (let ([L2 (license (box 99) (box '()))])
      (check-equal?
        (check-print
          (make-list 2 #rx"")
          (lambda () (poetic-license-report L2)))
        (void))))

)