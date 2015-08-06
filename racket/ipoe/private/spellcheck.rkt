#lang racket/base

;; A very simple spellchecker

(provide
  ;; (-> (-> String Boolean))
  spellchecker
)

;; -----------------------------------------------------------------------------

(require
  "db.rkt"
  (only-in racket/file file->value))

;; =============================================================================

;; Create a spellchecking function that references the ipoe database
(define (spellchecker)
  (define pgc (db-init))
  (lambda (word)
    (word-exists? pgc word)))

;; Convert a string to an "equivalent" string that might be in the database.
;; i.e., remove things like '?' and '!'.
(define (normalize word)
  (apply string
         (for/list ([c (in-string word)]
                    #:when (char-alphabetic? c))
           (char-downcase c))))

;; =============================================================================

(module+ test
  (require rackunit)

  (define spellcheck (spellchecker))
  (define-syntax-rule (check-spellcheck [in out] ...)
    (begin (check-equal? (spellcheck in) out) ...))

  (check-spellcheck
    ["yes" #t]
    ["volcano" #t]
    ["awsgvdcx" #f]
    ["" #f]
  )

  (define-syntax-rule (check-normalize [in out] ...)
    (begin (check-equal? (normalize in) out) ...))

  (check-normalize
    ["asdf" "asdf"]
    ["" ""]
    ["cat61" "cat"]
    ["ARGH" "argh"]
    ["waiT?" "wait"]
    ["don't" "dont"]
    ["hel,p" "help"]
  )
)
