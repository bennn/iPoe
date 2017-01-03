#lang racket/base

;; Common helpers for command-line tools

(provide
  rkt-file?
  ;; (-> String Boolean)
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/string string-suffix?)
)

;; =============================================================================

(define (rkt-file? fname)
  (and (string-suffix? fname ".rkt")
       (file-exists? fname)))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)


  (test-case "rkt-file?"
    (check-true (rkt-file? (path->string (collection-file-path "main.rkt" "ipoe"))))

    (check-false* rkt-file?
      ["'a"]
      ["1251"]
      ["nope.txt"]))
)
