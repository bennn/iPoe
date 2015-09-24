#lang racket/base

;; A simple spellchecker

(provide
  check-spelling
  ;; (-> (Sequenceof Word/Loc) (Listof Quirk))
  ;; Check all words in a sequence of lines for spelling errors
  ;; Assumes DB context.
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/poem
  ipoe/private/poem/poetic-license
  (only-in ipoe/private/db
    word-exists?)
  (only-in ipoe/private/parameters
   *spelling-error-penalty*)
  ipoe/private/suggest
  ;; --
  (only-in racket/set
    mutable-set
    set-add!
    set-member?)
  (only-in racket/match
    match-define)
)

;; =============================================================================

(define (spelling-error w/loc)
  (match-define (word/loc w w-num l-num s-num) w/loc)
  (define suggest* (suggest-spelling w #:limit 1))
  (define suggest-str
    (if (null? suggest*)
        ""
        (format " Maybe you meant '~a'?" (car suggest*))))
  (quirk (*spelling-error-penalty*)
         (format "Misspelled word '~a'; position ~a on line ~a of stanza ~a." w w-num l-num s-num suggest-str)))

;; Note: only gives 1 suggestion (be nicer to display in a dropdown)
;; (: check-spelling (-> (Sequenceof Word/Loc) Void))
(define (check-spelling word/loc*)
  (define seen (mutable-set))
  (for/list ([w/l (in-list word/loc*)]
             #:when (let ([w (word/loc-word w/l)])
                      (not (not (set-member? seen w))
                           (set-add! seen w) ;; Always add to `seen`
                           (word-exists? w))))
    (spelling-error w/l)))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  ;; TODO database-independent tests

;  (define (check-misspelled w* [len #f])
;    (check-print
;      (for/list ([i (in-range (or len (length w*)))])
;        #rx"^Misspelled word")
;      (lambda () (check-spelling w*))))

;  ;; -- check-spelling
;  (with-ipoe-db #:commit? #f (lambda ()
;    (check-true* (lambda line* (success? (check-spelling line*)))
;      ["yes" "why"]
;      ["all" "these words are" "spelled correctly! I promise"])
;
;    (let ([bad1 "asdvhuhewdv"]
;          [bad2 "uhnojfyondvwhbonvwf"]
;          [bad3 "hjvndkwcxs"]
;          [bad4 "xz"])
;      (check-apply* check-misspelled
;        [(list bad1) == (failure 'check-spelling (list (list bad1)))]
;        [(list bad1 bad2 bad3) == (failure 'check-spelling (list (list bad1) (list bad2) (list bad3)))]
;        [(list bad4) == (failure 'check-spelling (list (list bad4 "be" "of" "to" "a" "in" "I" "it")))]
;      ))))
;
;  ;; -- check-spelling, remove duplicates
;  (with-ipoe-db #:commit? #f (lambda ()
;    (let* ([bw "jipaengva"]
;           [bw2 (string-append bw " " bw)])
;      (check-apply* (lambda (w*) (check-misspelled w* 1))
;       [(list bw2 bw2 bw2) == (failure 'check-spelling (list (list bw)))]))))

)
