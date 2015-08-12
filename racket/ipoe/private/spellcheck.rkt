#lang racket/base

;; A very simple spellchecker

(provide
  check-spelling
  ;; (-> (Sequenceof String) Void)
  ;; Check all words in a sequence of lines for spelling errors
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/db
  ipoe/private/parse
  ipoe/private/ui
  (only-in racket/string string-split)
  (only-in racket/file file->value))

;; =============================================================================

;; Always succeeds.
;; As of 2015-08-06, just makes an alert if words are mispelled
;; - doesn't raise an error
;; - doesn't offer suggestions
;; (: check-spelling (-> (Sequenceof String) Void))
(define (check-spelling line*)
  (with-ipoe-db (lambda ()
    (for ([line line*]
          [line-num (in-naturals)])
      (for ([word (in-list (string-split line))]
            [word-num (in-naturals)])
        (define normalized (parse-word word))
        ;; Do nothing for non-words (punctuation)
        (when normalized
          (unless (word-exists? normalized)
            (alert (format "Warning: mispelled word '~a' on line '~a'" word line-num)))))))))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- check-spelling
  (check-spelling (list "yes"))
)
