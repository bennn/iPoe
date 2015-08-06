#lang racket/base

;; A very simple spellchecker

(provide
  check-spelling
  ;; (-> (Sequenceof String) Void)
  ;; Check all words in a sequence of lines for spelling errors

  spellchecker
  ;; (-> (-> String Boolean))
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/db
  ipoe/private/parse
  ipoe/private/ui
  (only-in racket/string string-split)
  (only-in racket/file file->value))

;; =============================================================================
;; Is there an interesting tradeoff between parameters & closing connections?
(define *pgc* (make-parameter #f))

;; Always succeeds.
;; As of 2015-08-06, just makes an alert if words are mispelled
;; - doesn't raise an error
;; - doesn't offer suggestions
;; (: check-spelling (-> (Sequenceof String) Void))
(define (check-spelling line*)
  (unless (*pgc*) (*pgc* (db-init)))
  (for ([line line*]
        [line-num (in-naturals)])
    (for ([word (in-list (string-split line))]
          [word-num (in-naturals)])
      (define normalized (parse-word word))
      ;; Do nothing for non-words (punctuation)
      (when normalized
        (unless (word-exists? (*pgc*) normalized)
          (alert (format "Warning: mispelled word '~a' on line '~a'" word line-num)))))))

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
