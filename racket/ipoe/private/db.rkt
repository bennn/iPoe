#lang racket/base

(provide
  add-word
  ;; (->* (connection? string?) (#:syllables (U Natural #f) #:rhymes (U (Listof String) #f) #:almost-rhymes (U (Listof String) #f) #:interactive? boolean? #:offline? boolean?) Void)
  ;; Add a new word to the database.
  ;; First three optional arguments supply information for the word's syllables, rhymes,
  ;;  and almost-rhymes.
  ;; Other optional arguments decide whether to search the internet and prompt
  ;;  the user when validating rhymes and syllables.

  add-rhyme add-rhyme*
  ;; (-> connection? string? string? void?)
  ;; Calling `(add-rhyme pgc word rhyme)` adds the pair (word, rhyme)
  ;;  to the rhyme database.
  ;; The iterated version `add-rhyme*` accepts a list of rhymes for a single word

  add-almost-rhyme add-almost-rhyme*
  ;; (-> connection? string? string? void?)
  ;; Similar to `add-rhyme` and `add-rhyme*`, but adds to the almost-rhyme database

  almost-rhymes-with?
  ;; (-> connection? string? string? boolean?)
  ;; Calling `(almost-rhymes-with? pgc w r)` returns true if `w` almost rhymes with `r`.
  ;; Results should be symmetric, but this is not guaranteed.

  id->word
  ;; (-> connection? natural/c string?)
  ;; Convert a primary key to its matching word string.

  rhymes-with?
  ;; (-> connection? string? string? boolean?)
  ;; Calling `(rhymes-with? pgc w r)` returns true if `w` rhymes with `r`.
  ;; Results should be symmetric, but this is not guaranteed.

  syllables->word*
  ;; (-> connection? natural? (sequence/c string?))
  ;; Return a sequence of words with the supplied number of syllables

  with-ipoe-db
  ;; (-> (-> Any) Any)
  ;; Execute the thunk in the context of a fresh database connection
  ;; TODO Ensures the connection is closed, even when the thunk raises an exception

  word->almost-rhyme*
  ;; (-> connection? string? (sequence/c string?)
  ;; Return the sequence of words that almost rhyme with the argument.

  word->id
  ;; (-> connection? string? natural?)
  ;; Get the id of the word, if it exists

  word->rhyme*
  ;; (-> connection? string? (sequence/c string?)
  ;; Return the sequence of words that rhyme with the argument

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
    rhyme-result-rhyme*
    rhyme-result-almost-rhyme*)
)

;; =============================================================================

;; TODO these constants should be in an external config file
(define DB-USER "ben")
(define DB-NAME "ipoe")
(define DB-LOG  "./ipoe.log")

(define current-ipoe-db  (make-parameter #f))
(define current-ipoe-log (make-parameter #f))

;; -----------------------------------------------------------------------------

;; Open a database connection & set parameters
(define (db-init)
  (current-ipoe-log (open-output-file DB-LOG #:exists? 'error))
  (current-ipoe-db  (postgresql-connect #:user DB-USER #:database DB-NAME))
  (start-transaction (current-ipoe-db))
  (current-ipoe-db))

;; Close a database connection & unset parameters
(define (db-close [pgc (current-ipoe-db)]
                  #:commit? [commit? #t])
  (close-output-port (current-ipoe-log))
  (if commit?
    (begin (commit-transaction pgc)
           (logfile->sql DB-LOG))
    (begin (rollback-transaction pgc)
           (delete-file DB-LOG)))
  (disconnect pgc)
  (current-ipoe-db  #f)
  (current-ipoe-log #f)
  (void))

(define (with-ipoe-db thunk #:commit? [commit? #t])
  ;; Open a database connection
  (let* ([pgc (db-init)])
    (call-with-exception-handler
      ;; On error, close the DB connection
      (lambda (exn)
        (begin (db-close pgc #:commit? commit?) exn))
      ;; Execute thunk with parameter set, close DB when finished
      (lambda ()
        (let ([result (thunk)])
          (begin (db-close pgc #:commit? commit?) result))))))

;; Compile a logfile to a new SQL migration
(define (logfile->sql filename)
  TODO)

;; -----------------------------------------------------------------------------

(define-syntax-rule (db-error loc msg arg* ...)
  (error (string->symbol (string-append "ipoe:db:" (symbol->string loc)))
         (format msg arg* ...)))

(define-syntax-rule (db-warning loc msg arg* ...)
  ;; Alternatively, pipe these to a logfile
  (displayln (format "[WARNING ipoe:db:~a] : ~a" loc (format msg arg* ...))))

(define-syntax-rule (duplicate-word-error w)
  ;; Should never happen, so raise an error
  (error 'ipoe:db (format "WARNING: word '~a' is not unique in the database" w)))

(define-syntax-rule (table-error sym loc)
  (db-error sym "Cannot infer table from symbol '~a'" loc))

;; -----------------------------------------------------------------------------
;; 2015-08-05... should these all return booleans, or make an Either type?

;; Add a new word to the database
(define (add-word pgc word #:syllables [syllables-param #f]
                           #:rhymes [rhyme-param '()]
                           #:almost-rhymes [almost-rhyme-param '()]
                           #:interactive? [interactive? #f]
                           #:offline? [offline? #f])
  (cond
   [(word-exists? pgc word)
    (error 'db:add-word (format "Cannot add word '~a', already in database" word))]
   [else
    (define syllables (resolve-syllables word syllables-param #:interactive? interactive? #:offline? offline?))
    (unless syllables (db-error 'add-word "Cannot add word '~a', failed to infer syllables. Try again with explicit #:syllables argument." word))
    (define rr (resolve-rhyme* word rhyme-param almost-rhyme-param #:interactive? interactive? #:offline? offline?))
    (define rhyme* (rhyme-result-rhyme* rr))
    (define almost-rhyme* (rhyme-result-almost-rhyme* rr))
    (add-word/unsafe pgc word syllables rhyme* almost-rhyme*)]))

(define (add-rhyme pgc word r)
  (add-rhyme* pgc word (list r)))

(define (add-rhyme* pgc word r*)
  (define wid (or (word->id pgc word)
                  (db-error "Cannot add rhyme for unknown word '~a'" word)))
  (add-rhyme*/unsafe pgc wid r*))

(define (add-almost-rhyme pgc word a)
  (add-almost-rhyme* pgc word (list a)))

(define (add-almost-rhyme* pgc word a*)
  (define wid (or (word->id pgc word)
                  (db-error "Cannot add almost-rhyme for unknown word '~a'" word)))
  (add-almost-rhyme*/unsafe pgc wid a*))

(define (add-word/unsafe pgc word syllables rhyme* almost-rhyme*)
  (query-exec pgc "INSERT INTO word (word, num_syllables) VALUES ($1, $2);" word syllables)
  (define wid (word->id pgc word))
  (unless wid (db-error 'add-word "Cannot find ID for newly-added word '~a'" word))
  (add-rhyme*/unsafe pgc wid rhyme*)
  (add-almost-rhyme*/unsafe pgc wid almost-rhyme*))

(define (add-rhyme*/unsafe pgc wid rhyme*)
  (add-r*/unsafe pgc wid rhyme* #:table 'rhyme))

(define (add-almost-rhyme*/unsafe pgc wid almost-rhyme*)
  (add-r*/unsafe pgc wid almost-rhyme* #:table 'almost_rhyme))

;; Abstraction for adding rhymes or almost-rhymes
(define (add-r*/unsafe pgc wid r* #:table loc)
  ;; It'd be better to build one large insert statement, but I'm not sure how to format.
  (assert-rhyme-table? loc #:src 'add-rhyme)
  (define query-str
    (format "INSERT INTO word_~as (word, ~a) VALUES ($1, $2);" loc loc))
  (for ([r (in-list r*)])
    (cond
     [(word->id pgc r)
      => (lambda (rid) (query-exec pgc query-str wid rid))]
     [else
      (db-warning loc "Could not find ID for word '~a'" r)])))

;; (: find-word (->* [PGC (U String Integer)] [#:column (U 'id 'word 'num_syllables #f)] (U #f (Vector Integer String Integer))))
(define (find-word pgc word-property #:column [col-param #f])
  (case (or (validate-word-column col-param) (infer-word-column word-property))
    [(id)
     (query-maybe-row pgc "SELECT * FROM word WHERE word.id=$1" word-property)]
    [(num_syllables)
     (query-maybe-row pgc "SELECT * FROM word WHERE word.num_syllables=$1" word-property)]
    [(word)
     (query-maybe-row pgc "SELECT * FROM word WHERE word.word=$1" word-property)]))

;; A finder for rhymes and almost-rhymes.
;; Both `wid` and `rid` are nullable, but not at the same time.
;; If both parameters are present, returns a row vector.
;; If one parameter is present, returns a sequence of matches.
;; (: find-r (-> PGC (U Natural #f) (U Natural #f) #:table (U 'rhyme 'almost_rhyme) (U (Vector Integer Integer) (Sequenceof (Values Integer Integer)))))
(define (find-r pgc [wid #f] [rid #f] #:table loc)
  (assert-rhyme-table? loc #:src 'find-rhyme)
  (cond
   [(and wid rid)
    (define query-str
      (format "SELECT * FROM word_~as WHERE word=$1 AND ~a=$2" loc loc))
    (query-maybe-row pgc query-str wid rid)]
   [wid
    (define query-str
      (format "SELECT * FROM word_~as WHERE word=$1" loc))
    (in-query pgc query-str wid)]
   [rid
    (define query-str
      (format "SELECT * FROM word_~as WHERE ~a=$1" loc loc))
    (in-query pgc query-str rid)]
   [else
    (db-error 'find-rhyme "Cannot execute find with two #f arguments. Need to supply word id or rhyme id.")]))

(define (id->word pgc wid)
  (match (find-word pgc wid #:column 'id)
    [#f #f]
    [(vector id word syllables) word]))

(define (syllables->word* pgc num-syllables)
  (in-query pgc "SELECT word FROM word WHERE word.num_syllables=$1" num-syllables))

(define (word->id pgc word)
  (match (find-word pgc word #:column 'word)
    [#f #f]
    [(vector id word syllables) id]))

(define (word->almost-rhyme* pgc word)
  (word->r* pgc word #:table 'almost_rhyme))

(define (word->r* pgc word #:table loc)
  (define wid (word->id pgc word))
  (unless wid (db-error loc "Cannot find ~a for unknown word '~a'" loc word))
  (define rid* (find-r pgc wid #:table loc))
  (sequence-map (lambda (wid rid) (id->word pgc rid)) rid*))

(define (word->rhyme* pgc word)
  (word->r* pgc word #:table 'rhyme))

(define (word->syllables pgc word)
  (match (find-word pgc word #:column 'word)
    [#f #f]
    [(vector id word syllables) syllables]))

;; -----------------------------------------------------------------------------
;; --- queries

;; True if `word` is already in the database
(define (word-exists? pgc word)
  (and (find-word pgc word #:column 'word) #t))

(define (almost-rhymes-with? pgc w r)
  (r-with? pgc w r #:table 'almost_rhyme))

(define (rhymes-with? pgc w r)
  (r-with? pgc w r #:table 'rhyme))

(define (r-with? pgc w r #:table loc)
  (define wid (word->id pgc w))
  (unless wid (db-error 'r-with "Cannot check ~a-with? for word '~a'" loc w))
  (define rid (word->id pgc r))
  (unless rid (db-error 'r-with "Cannot check ~a-with? for rhyme '~a'" loc r))
  (if (find-r pgc wid rid #:table loc) #t #f))

;; -----------------------------------------------------------------------------
;; --- misc validation

(define (rhyme-table? sym)
  (or (eq? sym 'rhyme)
      (eq? sym 'almost_rhyme)))

(define (assert-rhyme-table? sym #:src loc)
  (unless (rhyme-table? sym)
    (table-error loc sym)))

(define (infer-word-column param)
  (cond
   [(string? param)
    ;; 'word' is the only string column
    'word]
   [(and (integer? param) (< 0 param 50))
    ;; Small integers are probably syllables
    'num_syllables]
   [(integer? param)
    ;; Other integers are hopefully word ids
    'id]
   [else
    (db-error 'infer-word-column "Cannot infer column in word database from parameter '~a'" param)]))

(define (validate-word-column col)
  (cond
   [(or (eq? col 'id) (eq? col 'word) (eq? col 'num_syllables))
    col]
   [else
    #f]))

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

  ;; -- id->word
  (define-syntax-rule (check-id-word-bijection [w ...])
    ;; Spot-check id->word = word->id
    (begin (check-equal? (id->word pgc (word->id pgc w)) w) ...))
  (check-id-word-bijection
   ["yes" "under" "africa" "polymorphism" "a" "heating"])

  ;; -- infer-word-column
  (define-syntax-rule (check-infer-word-column [in out] ...)
    (begin (check-equal? (infer-word-column in) out) ...))
  (check-infer-word-column
   ["yolo" 'word]
   ["a13t" 'word]
   [""     'word]
   ["word" 'word]
   ;; --
   [1      'num_syllables]
   [3      'num_syllables]
   [7      'num_syllables]
   [40     'num_syllables]
   ;; --
   [0      'id]
   [-51    'id]
   [55     'id]
   [8675309 'id])

  ;; -- validate-word-column
  (define-syntax-rule (check-validate-word-column? [in out] ...)
    (begin (check-equal? (validate-word-column in) out) ...))
  (check-validate-word-column?
   ['id 'id]
   ['word 'word]
   ['num_syllables 'num_syllables]
   ;; --
   ['part_of_speech #f]
   ['nothing #f]
   ['genre #f]
   ["id" #f]
   ["word" #f]
   ['() #f]
   [5 #f]
  )

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

  ;; -- add-word/unsafe
  (let ([new-word "ycvgpadfwd"])
    ;; Add a word without rhymes
    (begin
      (add-word/unsafe pgc new-word 1 '() '())
      ;; New word should be defined
      (check-true (word-exists? pgc new-word))
      (check-equal? (word->syllables pgc new-word) 1)))

  ;; Add a word with known rhymes
  (let* ([new-word "nasdofiz"]
         [r*   '("yes")]
         [a*   '("later")])
    (begin
      ;; -- assert rhymes exist
      (for ([r (in-list r*)])
        (check-true (word-exists? pgc r)))
      (for ([a (in-list a*)])
        (check-true (word-exists? pgc a)))
      ;; -- add word
      (add-word/unsafe pgc new-word 11 r* a*)
      ;; -- assert word added
      (check-true (word-exists? pgc new-word))
      (check-equal? (word->syllables pgc new-word) 11)
      (check-equal? (id->word pgc (word->id pgc new-word)) new-word)
      ;; -- check rhymes
      (check-equal? (sequence->list (word->rhyme* pgc new-word)) r*)
      (check-equal? (sequence->list (word->almost-rhyme* pgc new-word)) a*)))

  ;; Add a word with unknown rhymes
  (let* ([new-word "hvzx8da"]
         [r* '("asvhcuivhw")]
         [a* '()])
    (begin
      ;; -- assert rhymes do NOT exist
      (for ([r (in-list r*)])
        (check-false (word-exists? pgc r)))
      (for ([a (in-list a*)])
        (check-true (word-exists? pgc a)))
      ;; -- add word
      (add-word/unsafe pgc new-word 5 r* a*) ;; Should trigger a printout
      (check-true (word-exists? pgc new-word))
      ;; -- should have no rhymes
      (check-equal? (sequence->list (word->rhyme* pgc new-word)) '())))

  ;; -- add-word
  (let ([nonsense1 "asdv8uazxcvasd_1"]
        [nonsense2 "asdv8uazxcvasd_2"]
        [nonsense3 "asdv8uazxcvasd_3"])
    (begin
      ;; Fails in online mode, cannot tell number of syllables
      (check-exn exn:fail? (lambda () (add-word pgc nonsense1 #:interactive? #f #:offline? #f)))
      ;; Succeeds with an explicit number of syllables
      (with-output-to-file "/dev/null" #:exists 'append
        (lambda ()
          (check-true (and (add-word pgc nonsense2 #:syllables 32 #:interactive? #f #:offline? #f) #t))))
      ;; Succeeds offline because the naive test never fails
      (check-true (and (add-word pgc nonsense3 #:interactive? #f #:offline? #t) #t))))

  (let ([new-word "yolo"])
    (begin
      (check-true (and (add-word pgc "yolo" #:interactive? #f #:offline? #f) #t))))

  ;; -- add-rhyme / add-almost-rhyme
  ;; Doesn't commit for unknown rhymes/almost
  (let ([word "word"]
        [unknown "aiopsuvhz"])
    (check-true (word-exists? pgc word))
    (check-false (word-exists? pgc unknown))
    (add-rhyme pgc word unknown)
    (check-false (word-exists? pgc unknown))
    (add-almost-rhyme pgc word unknown)
    (check-false (word-exists? pgc unknown)))

  ;; Succeeds for known words, doesn't matter if they actually rhyme
  (let ([w "cat"]
        [r "dog"])
    (check-true (word-exists? pgc w))
    (check-true (word-exists? pgc r))
    (check-false (rhymes-with? pgc w r))
    (check-false (almost-rhymes-with? pgc w r))
    ;; --
    (add-rhyme pgc w r)
    (check-true (rhymes-with? pgc w r))
    (check-false (almost-rhymes-with? pgc w r))
    ;; --
    (add-almost-rhyme pgc w r)
    (check-true (rhymes-with? pgc w r))
    (check-true (and (almost-rhymes-with? pgc w r) #t)))

  ;; -- (assert-)rhyme-table?
  (check-true (rhyme-table? 'rhyme))
  (check-true (rhyme-table? 'almost_rhyme))

  (check-false (rhyme-table? 'r))
  (check-false (rhyme-table? 'am))

  (check-true (and (assert-rhyme-table? 'rhyme #:src 'test) #t))
  (check-true (and (assert-rhyme-table? 'almost_rhyme #:src 'test) #t))

  (check-exn exn:fail? (lambda () (assert-rhyme-table? 'foo #:src 'test)))
  (check-exn exn:fail? (lambda () (assert-rhyme-table? 'invalid #:src 'test)))

  ;; -- word-exists?
  (define-syntax-rule (check-word-exists?/true [word ...])
    (begin (check-true (word-exists? pgc word)) ...))
  (check-word-exists?/true
   ["cat" "dog" "volcano" "element" "wood"])

  (define-syntax-rule (check-word-exists?/false [word ...])
    (begin (check-false (word-exists? pgc word)) ...))
  (check-word-exists?/false
   ["tagyscv7" "4axz" ""])

  ;; -- (almost-)rhymes-with?
  (define-syntax-rule (check-rhymes-with?/true [w r] ...)
    (begin (check-true (rhymes-with? pgc w r)) ...))
  (check-rhymes-with?/true
   ["plate" "mate"]
   ["inherit" "parrot"]
   ["car" "far"]
   ["criticism" "dualism"])
  (define-syntax-rule (check-rhymes-with?/false [w r] ...)
    (begin (check-false (rhymes-with? pgc w r)) ...))
  (check-rhymes-with?/false
   ["out" "book"]
   ["waiter" "chase"]
   ["chalkboard" "goat"]
  )

  (define-syntax-rule (check-almost-rhymes-with?/true [w r] ...)
    (begin (check-true (almost-rhymes-with? pgc w r)) ...))
  (check-almost-rhymes-with?/true
   ["criticism" "proposition"]
   ["lyric" "lithic"]
   ["bulimic" "pinprick"]
   ["pinprick" "lipstick"])
  (define-syntax-rule (check-almost-rhymes-with?/false [w r] ...)
    (begin (check-false (almost-rhymes-with? pgc w r)) ...))
  (check-almost-rhymes-with?/false
   ["watermelon" "orange"]
   ["cat" "cat"]
   ["yoyo" "wolf"]
  )

  ;; ---- End the transaction, do not commit
  (rollback-transaction pgc)
  (db-close pgc)
  (purge-log)
)
