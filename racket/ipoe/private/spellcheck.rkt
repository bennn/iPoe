#lang racket/base

;; A simple spellchecker

(provide
  check-spelling
  ;; (-> (Sequenceof String) Either)
  ;; Check all words in a sequence of lines for spelling errors
  ;; Assumes DB context.
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/db
  ipoe/private/either
  ipoe/private/parse
  ipoe/private/suggest
  ipoe/private/ui
  (only-in racket/string string-split)
  (only-in racket/file file->value))

;; =============================================================================

;; Always succeeds.
;; As of 2015-08-06, just makes an alert if words are mispelled
;; - doesn't raise an error
;; - only gives 1 suggestion (be nicer to display in a dropdown)
;; (: check-spelling (-> (Sequenceof String) Void))
(define (check-spelling line*)
  (define misspelled*
    (for/list
              ([line line*]
               [line-num (in-naturals)])
      (for/list
                ([w (in-list (string->word* line))]
                 [word-num (in-naturals)]
                 #:when (not (word-exists? w)))
        (define suggestions (suggest-spelling w #:limit 7))
        (define suggest-str (if (null? suggestions) "" (format " Maybe you meant '~a'?" (car suggestions))))
        (alert (format "Misspelled word '~a' on line '~a'.~a" w line-num suggest-str))
        (cons w suggestions))))
  (if (null? (car misspelled*))
      (success 'check-spelling #t)
      (failure 'check-spelling (apply append misspelled*))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  ;; -- check-spelling
  (with-ipoe-db #:commit? #f (lambda ()
    (check-true* (lambda line* (success? (check-spelling line*)))
      ["yes" "why"]
      ["all" "these words are" "spelled correctly! I promise"])

    (let ([bad1 "asdvhuhewdv"]
          [bad2 "uhnojfyondvwhbonvwf"]
          [bad3 "hjvndkwcxs"]
          [bad4 "xz"])
      (check-apply* (lambda (w*)
                      (check-print
                        (for/list ([w (in-list w*)])
                          #rx"^Misspelled word")
                        (lambda () (check-spelling w*))))
        [(list bad1) == (failure 'check-spelling (list (list bad1)))]
        [(list bad1 bad2 bad3) == (failure 'check-spelling (list (list bad1) (list bad2) (list bad3)))]
        [(list bad4) == (failure 'check-spelling (list (list bad4 "be" "of" "to" "a" "in" "I" "it")))]
      ))))

)
