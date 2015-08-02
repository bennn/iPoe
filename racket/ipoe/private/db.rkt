#lang racket/base

(provide
  add-word
  ;; (->* (connection? string?) (#:syllables (U Natural #f) #:rhymes (U (Listof String) #f) #:almost-rhymes (U (Listof String) #f)) Void)
  ;; Add a new word to the database.
  ;; Optional arguments supply information for the word's syllables, rhymes,
  ;;  and almost-rhymes.
  ;; If given, these arguments are validated against the internet anyway.

  ; add-rhyme
  ; add-almost-rhyme

  db-init
  ;; (-> connection?)
  ;; Connect to the ipoe database, return the psql connection

  syllables->word*
  ;; (-> connection? natural? (sequence/c string?))
  ;; Return a sequence of words with the supplied number of syllables

  word->id
  ;; (-> connection? string? natural?)
  ;; Get the id of the word, if it exists

  word->syllables
  ;; (-> connection? string? natural?)
  ;; Get the number of syllables in a word, if it exists

  word-exists?
  ;; (-> connection? string? boolean?)
  ;; True if the second argument is in the database
)

(require
  db/base
  db/postgresql
  racket/match
  racket/sequence
  (only-in ipoe/private/scrape
    resolve-syllables
    resolve-rhyme*
    resolve-almost-rhyme*)
)

;; =============================================================================

(define-syntax-rule (word-error loc msg arg* ...)
  (error (string->symbol (string-append "ipoe:db:" (symbol->string loc)))
         (format msg arg* ...)))

(define-syntax-rule (duplicate-word-error w)
  ;; Should never happen, so raise an error
  (error 'ipoe:db (format "WARNING: word '~a' is not unique in the database" w)))

;; -----------------------------------------------------------------------------

;; Add a new word to the database
(define (add-word pgc word #:syllables [syllables-param #f]
                           #:rhymes [rhyme-param '()]
                           #:almost-rhymes [almost-rhyme-param '()])
  (cond
   [(word-exists? pgc word)
    (error 'db:add-word (format "Cannot add word '~a', already in database" word))]
   [else
    (define syllables (resolve-syllables word syllables-param))
    (define rhyme*    (resolve-rhyme* word rhyme-param))
    (define almost-rhyme* (resolve-almost-rhyme* word almost-rhyme-param))
    (add-word/unsafe pgc word syllables rhyme* almost-rhyme*)]))

(define (add-word/unsafe pgc word syllables rhyme* almost-rhyme*)
  ;; -- WARNING: the rhyme functions do not check that the rhyme is a known word
  (query-exec pgc "INSERT INTO word (word, num_syllables) VALUES ($1, $2);" word syllables)
  (define wid (word->id pgc word))
  (add-rhyme*/unsafe pgc wid rhyme*)
  (add-almost-rhyme*/unsafe pgc wid almost-rhyme*))

(define (add-rhyme*/unsafe pgc wid rhyme*)
  ;; It'd be better to build one large insert statement, but I'm not sure how to format
  (for ([r (in-list rhyme*)])
    (define rid (word->id r))
    (query-exec pgc "INSERT INTO word_rhymes (word, rhyme) VALUES ($1, $2);" wid rid)))

(define (add-almost-rhyme*/unsafe pgc wid almost-rhyme*)
  (for ([r (in-list almost-rhyme*)])
    (define rid (word->id r))
    (query-exec pgc "INSERT INTO word_almost_rhymes (word, almost_rhyme) VALUES ($1, $2);" wid rid)))

(define (db-init)
  (postgresql-connect #:user "ben" #:database "ipoe"))

(define (find-word pgc word)
  (query-maybe-row pgc "SELECT * FROM word WHERE word.word=$1" word))

(define (syllables->word* pgc num-syllables)
  (sequence-map (lambda (id w syll) w)
                (in-query pgc "SELECT * FROM word WHERE word.num_syllables=$1" num-syllables)))

(define (word->id pgc word)
  (match (find-word pgc word)
    [#f #f]
    [(vector id word syllables) id]))

(define (word->syllables pgc word)
  (match (find-word pgc word)
    [#f #f]
    [(vector id word syllables) syllables]))

;; True if `word` is already in the database
(define (word-exists? pgc word)
  (and (find-word pgc word) #t))

;; =============================================================================

(module+ test
  (require rackunit racket/sequence)

  (define pgc (db-init))

  ;; ---- Start a transaction, so nothing in the tests gets committed
  (start-transaction pgc)

  ;; -- find-word
  (define-syntax-rule (check-find-word [word result] ...)
    (begin (check-equal? (find-word pgc word) result) ...))
  (check-find-word
    ["yoga" '#(679928 "yoga" 2)]
    ["pal"  '#(614060 "pal" 1)]
    ["sweet" '#(653512 "sweet" 1)]
    ["blimp" '#(526807 "blimp" 1)]
    ["anonymous" '#(517406 "anonymous" 4)])

  ;; -- syllables->word*
  (define-syntax-rule (check-syllables->word* [syllables word-expected*] ...)
    ;; Accept a number of syllables and list of words.
    ;; Check that the supplied words are a subset of the result sequence
    (begin (let* ([word* (syllables->word* pgc syllables)]
                  [word*-filtered (sequence->list (sequence-filter (lambda (w) (member w word-expected*)) word*))])
             (check-true (not (eq? '() word*-filtered)))
             (check-equal? (sort word-expected* string<?) (sort word*-filtered string<?))) ...))
  (check-syllables->word*
    [11 '("antidisestablishmentarianism" "dichlorodiphenyltrichloroethane"
          "electroencephalographically" "overindividualistically")]
    [9  '("antimilitaristically")])

  ;; -- word->id
  ;; Should return #f for unknown words
  (check-false (word->id pgc ""))
  (check-false (word->id pgc "afsv2qpewudvnjszd"))

  (define-syntax-rule (check-word->id/pass [word id] ...)
    (begin (check-equal? (word->id pgc word) id) ...))
  (check-word->id/pass
    ["cat" 532756]
    ["aardvark" 511396]
    ["holy" 573983]
    ["chalk" 534003]
    ["soldier" 645783])

  (define-syntax-rule (check-word->id/fail [word id] ...)
    (begin (check-not-equal? (word->id pgc word) id) ...))
  (check-word->id/fail
    ["baseball" 420]
    ["yolo" 1]
    ["demon" 12])

  ;; -- word->syllables
  (define-syntax-rule (check-word->syllables/pass [word syll] ...)
    (begin (check-equal? (word->syllables pgc word) syll) ...))
  (check-word->syllables/pass
    ["" #f]
    ["once" 1]
    ["never" 2]
    ["adamant" 3]
    ["stallion" 2]
    ["chrysanthemum" 4]
    ["metallurgy" 4]
    ["antihistamine" 5]
    ["academically" 6])

  (define-syntax-rule (check-word->syllables/fail [word syll] ...)
    (begin (check-not-equal? (word->syllables pgc word) syll) ...))
  (check-word->syllables/fail
    ["" 11]
    ["mary" 4]
    ["element" 1]
    ["balloon" 3]
    ["hour" 2])

  ;; -- word-exists?
  (define-syntax-rule (check-word-exists?/true [word ...])
    (begin (check-true (word-exists? pgc word)) ...))
  (check-word-exists?/true
   ["cat" "dog" "volcano" "element" "wood"])

  (define-syntax-rule (check-word-exists?/false [word ...])
    (begin (check-false (word-exists? pgc word)) ...))
  (check-word-exists?/false
   ["tagyscv7" "4axz" ""])

  ;; ---- End the transaction, do not commit
  (rollback-transaction pgc)
)
