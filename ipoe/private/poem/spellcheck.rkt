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
         (format "Misspelled word '~a'; position ~a on line ~a of stanza ~a.~a" w w-num l-num s-num suggest-str)))

;; Note: only gives 1 suggestion (be nicer to display in a dropdown)
;; (: check-spelling (-> (Sequenceof Word/Loc) Void))
(define (check-spelling word/loc*)
  (define seen (mutable-set))
  (define (known-word? w)
    (unless (set-member? seen w)
      (set-add! seen w)
      (word-exists? w)))
  (for/list ([w/l (in-list word/loc*)]
             #:when (not (known-word? (word/loc-word w/l))))
    (spelling-error w/l)))

;; =============================================================================

(module+ test
  (require
    rackunit
    rackunit-abbrevs
    (only-in ipoe/private/db
      add-word
      with-ipoe-db)
    ipoe/private/parameters
  )

  (define o* (options-init-for-test))

  (define-syntax-rule (with-db-test e)
    (when o*
      (parameterize-from-hash o*
        (lambda ()
         (parameterize ([*interactive?* #f])
          (with-ipoe-db #:user (*user*)
                        #:dbname (*dbname*)
                        #:commit? #f
            (lambda () e)))))))

  (define-syntax-rule (add-word/nothing w)
    (add-word w #:syllables 1))

  (define-syntax-rule (check-pass? e)
    (check-true (null? e)))

  (define-syntax-rule (check-fail? N e)
    (begin (check-true (list? e))
           (check-equal? (length e) N)
           (check-true (for/and ([x (in-list e)]) (quirk? x)))))

  ;; -- check-spelling
  (with-db-test
    (let* ([w1 "sadwefscasdweda"]
           [w1/loc (word/loc w1 0 0 1)]
           [w2 "oweryqtqrerqere"]
           [w2/loc (word/loc w2 4 1 9)]
           [w3 "qeytqueorqerwer"]
           [w3/loc (word/loc w3 6 6 66)])
      ;; -- existing words are spelled correctly
      (check-fail? 1 (check-spelling (list w1/loc)))
      (add-word/nothing w1)
      (check-pass? (check-spelling (list w1/loc)))
      ;; -- returns a list of all misspelled words
      (check-fail? 2 (check-spelling (list w1/loc w2/loc w3/loc)))
      ;; -- ignore duplicate errors
      (check-fail? 1 (check-spelling (list w3/loc w3/loc w3/loc)))))

)
