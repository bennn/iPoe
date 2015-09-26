#lang racket/base

(provide
  rkt-file?
  ;; (-> String Boolean)

)

(require
  (only-in racket/string string-suffix?)
)

;; =============================================================================

(define (rkt-file? fname)
  (and (string-suffix? fname ".rkt")
       (file-exists? fname)))

;; =============================================================================

(module+ test
  (require
    rackunit
    ipoe/private/util/rackunit-abbrevs)


  ;; -- rkt-file?
  ;(check-true* (lambda (p) (rkt-file? (path->string (make-resolved-module-path p))))
  ;  ["./new.rkt"]
  ;  ["./create.rkt"]
  ;  ["./../private.rkt"])

  (check-false* rkt-file?
    ["'a"]
    ["1251"]
    ["nope.txt"])
)
