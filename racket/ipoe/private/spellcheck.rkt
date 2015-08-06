#lang racket/base

;; A very simple spellchecker

(provide
  ;; (-> (-> String Boolean))
  spellchecker
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/db
  ipoe/private/parse
  (only-in racket/file file->value))

;; =============================================================================

;; Create a spellchecking function that references the ipoe database
(define (spellchecker)
  (define pgc (db-init))
  (lambda (word)
    (word-exists? pgc (parse-word word))))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- spellcheck
  (define spellcheck (spellchecker))
  (define-syntax-rule (check-spellcheck [in out] ...)
    (begin (check-equal? (spellcheck in) out) ...))
  (check-spellcheck
    ["yes" #t]
    ["volcano" #t]
    ["awsgvdcx" #f]
    ["" #f]
  )

)
