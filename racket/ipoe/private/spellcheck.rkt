#lang racket/base

;; A very simple spellchecker

(provide
  check-spelling
  ;; (-> (Sequenceof String) Either)
  ;; Check all words in a sequence of lines for spelling errors
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
    (with-ipoe-db (lambda ()
      (for/list
                ([line line*]
                 [line-num (in-naturals)])
        (for/list
                  ([w (in-list (string->word* line))]
                   [word-num (in-naturals)]
                   #:when (not (word-exists? w)))
          (define suggestions (suggest-spelling w #:epsilon 1 #:limit 1))
          (define suggest-str (if (null? suggestions) "" (format " Maybe you meant '~a'?" (car suggestions))))
          (alert (format "Warning: mispelled word '~a' on line '~a'.~a" w line-num suggest-str))
          w)))))
  (if (null? (car misspelled*))
      (success 'check-spelling #t)
      (failure 'check-spelling (apply append misspelled*))))

;; =============================================================================

(module+ test
  (require rackunit "rackunit-abbrevs.rkt")

  ;; -- check-spelling
  (check-true* (lambda line* (success? (check-spelling line*)))
    ["yes" "why"]
    ["all" "these words are" "spelled correctly! I promise"]
  )

  (let ([bad1 "asdvhuhewdv"]
        [bad2 "uhnojfyondvwhbonvwf"]
        [bad3 "hjvndkwcxs"])
    (check-apply* check-spelling
      [(list bad1) == (failure 'check-spelling (list bad1))]
      [(list bad1 bad2 bad3) == (failure 'check-spelling (list bad1 bad2 bad3))]
    ))

)
