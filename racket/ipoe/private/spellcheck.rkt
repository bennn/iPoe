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
  (only-in racket/list remove-duplicates)
  (only-in racket/set mutable-set set-add! set-member? set->list)
  (only-in racket/string string-split)
  (only-in racket/file file->value))

;; =============================================================================

;; Always succeeds.
;; As of 2015-08-06, just makes an alert if words are mispelled
;; - doesn't raise an error
;; - only gives 1 suggestion (be nicer to display in a dropdown)
;; (: check-spelling (-> (Sequenceof String) Void))
(define (check-spelling line*)
  (define seen (mutable-set))
  (define misspelled*
    (for/list ([line line*]
               [line-num (in-naturals)])
      (for/list ([w (in-list (string->word* line))]
                 [word-num (in-naturals)]
                 #:when (and (not (word-exists? w))
                             (not (set-member? seen w))))
          (define suggestions (suggest-spelling w #:limit 7))
          (define s-str
            (if (null? suggestions)
                ""
                (format " Maybe you meant '~a'?" (car suggestions))))
          (alert (format "Misspelled word '~a' on line ~a.~a" w line-num s-str))
          (set-add! seen w)
          (cons w suggestions))))
  (if (null? (car misspelled*))
      (success 'check-spelling #t)
      (failure 'check-spelling
        (remove-duplicates #:key car
          (apply append misspelled*)))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  (define (check-misspelled w* [len #f])
    (check-print
      (for/list ([i (in-range (or len (length w*)))])
        #rx"^Misspelled word")
      (lambda () (check-spelling w*))))

  ;; -- check-spelling
  (with-ipoe-db #:commit? #f (lambda ()
    (check-true* (lambda line* (success? (check-spelling line*)))
      ["yes" "why"]
      ["all" "these words are" "spelled correctly! I promise"])

    (let ([bad1 "asdvhuhewdv"]
          [bad2 "uhnojfyondvwhbonvwf"]
          [bad3 "hjvndkwcxs"]
          [bad4 "xz"])
      (check-apply* check-misspelled
        [(list bad1) == (failure 'check-spelling (list (list bad1)))]
        [(list bad1 bad2 bad3) == (failure 'check-spelling (list (list bad1) (list bad2) (list bad3)))]
        [(list bad4) == (failure 'check-spelling (list (list bad4 "be" "of" "to" "a" "in" "I" "it")))]
      ))))

  ;; -- check-spelling, remove duplicates
  (with-ipoe-db #:commit? #f (lambda ()
    (let* ([bw "jipaengva"]
           [bw2 (string-append bw " " bw)])
      (check-apply* (lambda (w*) (check-misspelled w* 1))
       [(list bw2 bw2 bw2) == (failure 'check-spelling (list (list bw)))]))))

)
