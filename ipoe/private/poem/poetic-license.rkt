#lang racket/base

(provide
  poetic-license-apply
  ;; (-> License (Listof Quirk) License)
  ;; Apply a poetic license to a list of quirks

  poetic-license-init
  ;; (-> License)
  ;; Create a new poetic license.
  ;; Assumes that all parameters in `parameters.rkt` are accessible

  poetic-license-report
  ;; (-> License Void)
  ;; Print a report of all quirks we applied the license to
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/parameters
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


;  (cond
;   [(not extra?)
;    (define d-str (if #,descr (string-append "\n  " #,descr) ""))
;    (user-error '#,name (format "Rhyme scheme OK, but failed extra constraint.~a" d-str))]
;   [(failure? extra?)
;    (user-error '#,name (failure-reason extra?))])

;; -----------------------------------------------------------------------------

;; Apply the poetic license to a list of quirks.
;; Raise an exception if the license has too little credit.
(define (poetic-license-apply L q*)
  (match q*
   ['() (void)]
   [(cons qh qt)
    (define c (quirk->cost qh))
    (poetic-license-deduct L c)
    (if (positive? (unbox (license-credit L)))
        (poetic-license-apply L qt)
        (poetic-license-error L qh))]))

;; Subtract the cost from the license
;; (: poetic-license-deduct (-> License (U Natural #f) Void))
(define (poetic-license-deduct L c)
  (define old-c (unbox (license-credit L)))
  (define new-c (- old-c (or c old-c)))
  (set-box! (license-credit L) new-c))

(define-syntax-rule (poetic-license-error L q)
  (raise-user-error 'ipoe (quirk->string q)))

;; Create a poetic license using dynamically-bound parameters
(define (poetic-license-init)
  (license (box (*poetic-license*))
           (box '())))

(define (poetic-license-report L)
  (displayln "Finished checking poem")
  (for ([q (in-list (unbox (license-quirk* L)))])
    (display "- ")
    (displayln (quirk->string q)))
  (printf "Remaining poetic license: ~a\n" (unbox (license-credit L))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/util/rackunit-abbrevs)

)
