#lang racket/base

(provide
  add-word
  ;; (->* (string?) (#:db connection? #:syllables (U Natural #f) #:rhymes (U (Listof String) #f) #:almost-rhymes (U (Listof String) #f) #:interactive? boolean? #:offline? boolean?) Void)
  ;; Add a new word to the database.
  ;; First three optional arguments supply information for the word's syllables, rhymes,
  ;;  and almost-rhymes.
  ;; Other optional arguments decide whether to search the internet and prompt
  ;;  the user when validating rhymes and syllables.

  add-rhyme add-rhyme*
  ;; (->* [string? string?] [#:db connection?] void?)
  ;; Calling `(add-rhyme word rhyme)` adds the pair (word, rhyme)
  ;;  to the rhyme table of the current database.
  ;; The iterated version `add-rhyme*` accepts a list of rhymes for a single word

  add-almost-rhyme add-almost-rhyme*
  ;; (->* [string? string?] [#:db connection] void?)
  ;; Similar to `add-rhyme` and `add-rhyme*`, but adds to the almost-rhyme database

  almost-rhymes-with?
  ;; (->* [string? string?] [#:db connection?] boolean?)
  ;; Calling `(almost-rhymes-with? w r)` returns true if `w` almost rhymes with `r`.
  ;; Results should be symmetric, but this is not guaranteed.

  id->word
  ;; (->* [natural/c] [#:db connection?] string?)
  ;; Convert a primary key to its matching word string.

  rhymes-with?
  ;; (->* [string? string?] [#:db connection?] boolean?)
  ;; Calling `(rhymes-with? w r)` returns true if `w` rhymes with `r`.
  ;; Results should be symmetric, but this is not guaranteed.

  syllables->word*
  ;; (->* [natural?] [#:db connection?] (sequence/c string?))
  ;; Return a sequence of words with the supplied number of syllables

  with-ipoe-db
  ;; (-> (-> Any) Any)
  ;; Execute the thunk in the context of a fresh database connection

  word->almost-rhyme*
  ;; (->* [string?] [#:db connection?] (sequence/c string?)
  ;; Return the sequence of words that almost rhyme with the argument.

  word->id
  ;; (->* [string?] [#:db connection?] natural?)
  ;; Get the id of the word, if it exists

  word->rhyme*
  ;; (->* [string?] [connection?] (sequence/c string?)
  ;; Return the sequence of words that rhyme with the argument

  word->syllables
  ;; (->* [string?] [#:db connection?] natural?)
  ;; Get the number of syllables in a word, if it exists

  word-exists?
  ;; (->* [string?] [#:db connection?] boolean?)
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
;;(define DB-LOG  "./ipoe.log")

(define current-ipoe-db  (make-parameter #f))
;;(define current-ipoe-log (make-parameter #f))

;; -----------------------------------------------------------------------------

;; Open a database connection & set parameters
(define (db-init)
  ;;(current-ipoe-log (open-output-file DB-LOG #:exists? 'error))
  (current-ipoe-db  (postgresql-connect #:user DB-USER #:database DB-NAME))
  (start-transaction (current-ipoe-db))
  (current-ipoe-db))

;; Close a database connection & unset parameters
(define (db-close #:db [pgc (current-ipoe-db)]
                  #:commit? [commit? #t])
  ;;(close-output-port (current-ipoe-log))
  (if commit?
    (begin (commit-transaction pgc)
           #;(logfile->sql DB-LOG))
    (begin (rollback-transaction pgc)
           #;(delete-file DB-LOG)))
  (disconnect pgc)
  (current-ipoe-db  #f)
  ;;(current-ipoe-log #f)
  (void))

(define (with-ipoe-db thunk #:commit? [commit? #t])
  ;; Open a database connection
  (let* ([pgc (db-init)])
    (call-with-exception-handler
      ;; On error, close the DB connection
      (lambda (exn)
        (begin (db-close #:db pgc #:commit? commit?) exn))
      ;; Execute thunk with parameter set, close DB when finished
      (lambda ()
        (let ([result (thunk)])
          (begin (db-close #:db pgc #:commit? commit?) result))))))

;; -----------------------------------------------------------------------------
;; --- Logfile operations
;;     - need to get the queries executed by the db library
;
; ;; Compile a logfile to a new SQL migration
; (define (logfile->sql filename)
;   ;; Write the other ops first, this comes last
;   TODO)
;
;; -----------------------------------------------------------------------------
;; --- syntax & exceptions

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
;; 2015-08-10 definitiely an Either type

;; Add a new word to the database
(define (add-word word
                  #:db [pgc (current-ipoe-db)]
                  #:syllables [syllables-param #f]
                  #:rhymes [rhyme-param '()]
                  #:almost-rhymes [almost-rhyme-param '()]
                  #:interactive? [interactive? #f]
                  #:offline? [offline? #f])
  (cond
   [(word-exists? word #:db pgc)
    (duplicate-word-error (format "Cannot add word '~a', already in database" word))]
   [else
    (define syllables (resolve-syllables word syllables-param #:interactive? interactive? #:offline? offline?))
    (unless syllables (db-error 'add-word "Cannot add word '~a', failed to infer syllables. Try again with explicit #:syllables argument." word))
    (define rr (resolve-rhyme* word rhyme-param almost-rhyme-param #:interactive? interactive? #:offline? offline?))
    (define rhyme* (rhyme-result-rhyme* rr))
    (define almost-rhyme* (rhyme-result-almost-rhyme* rr))
    (add-word/unsafe word syllables rhyme* almost-rhyme* #:db pgc)]))

(define (add-rhyme word r #:db [pgc (current-ipoe-db)])
  (add-rhyme* word (list r) #:db pgc))

(define (add-rhyme* word r* #:db [pgc (current-ipoe-db)])
  (define wid (or (word->id word #:db pgc)
                  (db-error "Cannot add rhyme for unknown word '~a'" word)))
  (add-rhyme*/unsafe wid r* #:db pgc))

(define (add-almost-rhyme word a #:db [pgc (current-ipoe-db)])
  (add-almost-rhyme* word (list a) #:db pgc))

(define (add-almost-rhyme* word a* #:db [pgc (current-ipoe-db)])
  (define wid (or (word->id word #:db pgc)
                  (db-error "Cannot add almost-rhyme for unknown word '~a'" word)))
  (add-almost-rhyme*/unsafe wid a* #:db pgc))

(define (add-word/unsafe word syllables rhyme* almost-rhyme* #:db [pgc (current-ipoe-db)])
  (query-exec pgc "INSERT INTO word (word, num_syllables) VALUES ($1, $2);" word syllables)
  (define wid (word->id word #:db pgc))
  (unless wid (db-error 'add-word "Cannot find ID for newly-added word '~a'" word))
  (add-rhyme*/unsafe wid rhyme* #:db pgc)
  (add-almost-rhyme*/unsafe wid almost-rhyme* #:db pgc))

(define (add-rhyme*/unsafe wid rhyme* #:db [pgc (current-ipoe-db)])
  (add-r*/unsafe wid rhyme* #:db pgc #:table 'rhyme))

(define (add-almost-rhyme*/unsafe wid almost-rhyme* #:db [pgc (current-ipoe-db)])
  (add-r*/unsafe wid almost-rhyme* #:db pgc #:table 'almost_rhyme))

;; Abstraction for adding rhymes or almost-rhymes
(define (add-r*/unsafe wid r* #:db [pgc (current-ipoe-db)] #:table loc)
  ;; It'd be better to build one large insert statement, but I'm not sure how to format.
  (assert-rhyme-table? loc #:src 'add-rhyme)
  (define query-str
    (format "INSERT INTO word_~as (word, ~a) VALUES ($1, $2);" loc loc))
  (for ([r (in-list r*)])
    (cond
     [(word->id r #:db pgc)
      => (lambda (rid) (query-exec pgc query-str wid rid))]
     [else
      (db-warning loc "Could not find ID for word '~a'" r)])))

;; (: find-word (->* [PGC (U String Integer)] [#:column (U 'id 'word 'num_syllables #f)] (U #f (Vector Integer String Integer))))
(define (find-word word-property #:db [pgc (current-ipoe-db)] #:column [col-param #f])
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
(define (find-r [wid #f] [rid #f] #:db [pgc (current-ipoe-db)] #:table loc)
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

(define (id->word wid #:db [pgc (current-ipoe-db)])
  (match (find-word wid #:db pgc #:column 'id)
    [#f #f]
    [(vector id word syllables) word]))

(define (syllables->word* num-syllables #:db [pgc (current-ipoe-db)])
  (in-query pgc "SELECT word FROM word WHERE word.num_syllables=$1" num-syllables))

(define (word->id word #:db [pgc (current-ipoe-db)])
  (match (find-word word #:column 'word #:db pgc)
    [#f #f]
    [(vector id word syllables) id]))

(define (word->almost-rhyme* word #:db [pgc (current-ipoe-db)])
  (word->r* word #:db pgc #:table 'almost_rhyme))

(define (word->r* word #:db pgc #:table loc)
  (define wid (word->id word #:db pgc))
  (unless wid (db-error loc "Cannot find ~a for unknown word '~a'" loc word))
  (define rid* (find-r wid #:db pgc #:table loc))
  (sequence-map (lambda (wid rid) (id->word rid #:db pgc)) rid*))

(define (word->rhyme* word #:db [pgc (current-ipoe-db)])
  (word->r* word #:db pgc #:table 'rhyme))

(define (word->syllables word #:db [pgc (current-ipoe-db)])
  (match (find-word word #:db pgc #:column 'word)
    [#f #f]
    [(vector id word syllables) syllables]))

;; -----------------------------------------------------------------------------
;; --- queries

;; True if `word` is already in the database
(define (word-exists? word #:db [pgc (current-ipoe-db)])
  (and (find-word word #:column 'word #:db pgc) #t))

(define (almost-rhymes-with? w r #:db [pgc (current-ipoe-db)])
  (r-with? w r #:db pgc #:table 'almost_rhyme))

(define (rhymes-with? w r #:db [pgc (current-ipoe-db)])
  (r-with? w r #:db pgc #:table 'rhyme))

(define (r-with? w r #:db [pgc (current-ipoe-db)] #:table loc)
  (define wid (word->id w #:db pgc))
  (unless wid (db-error 'r-with "Cannot check ~a-with? for word '~a'" loc w))
  (define rid (word->id r #:db pgc))
  (unless rid (db-error 'r-with "Cannot check ~a-with? for rhyme '~a'" loc r))
  (if (find-r wid rid #:db pgc #:table loc) #t #f))

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

  (define-syntax-rule (with-db-test e)
    (with-ipoe-db (lambda () e) #:commit? #f))

  ;; -- find-word
  (define-syntax-rule (check-find-word [word result] ...)
    (with-db-test
      (begin (check-equal? (find-word word) result) ...)))
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
    (with-db-test
      (begin (let* ([word* (syllables->word* syllables)]
                    [word*-filtered (sequence->list (sequence-filter (lambda (w) (member w word-expected*)) word*))])
               (check-true (not (eq? '() word*-filtered)))
               (check-equal? (sort word-expected* string<?) (sort word*-filtered string<?))) ...)))
  (check-syllables->word*
    [11 '("antidisestablishmentarianism" "dichlorodiphenyltrichloroethane"
          "electroencephalographically" "overindividualistically")]
    [9  '("antimilitaristically")])

  ;; -- id->word
  (define-syntax-rule (check-id-word-bijection [w ...])
    ;; Spot-check id->word = word->id
    (with-db-test
      (begin (check-equal? (id->word (word->id w)) w) ...)))
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
  (with-db-test
    (begin
     (check-false (word->id ""))
     (check-false (word->id "afsv2qpewudvnjszd"))

     (define-syntax-rule (check-word->id/pass [word id] ...)
       (begin (check-equal? (word->id word) id) ...))
     (check-word->id/pass
       ["cat" 532756]
       ["aardvark" 511396]
       ["holy" 573983]
       ["chalk" 534003]
       ["soldier" 645783])
     (define-syntax-rule (check-word->id/fail [word id] ...)
       (begin (check-not-equal? (word->id word) id) ...))
     (check-word->id/fail
       ["baseball" 420]
       ["yolo" 1]
       ["demon" 12])))

  ;; -- word->syllables
  (with-db-test
    (begin
      (define-syntax-rule (check-word->syllables/pass [word syll] ...)
        (begin (check-equal? (word->syllables word) syll) ...))
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
        (begin (check-not-equal? (word->syllables word) syll) ...))
      (check-word->syllables/fail
        ["" 11]
        ["mary" 4]
        ["element" 1]
        ["balloon" 3]
        ["hour" 2])))

  ;; -- add-word/unsafe
  (with-db-test
    (begin
      (let ([new-word "ycvgpadfwd"])
        ;; Add a word without rhymes
        (begin
          (add-word/unsafe new-word 1 '() '())
          ;; New word should be defined
          (check-true (word-exists? new-word))
          (check-equal? (word->syllables new-word) 1)))

      ;; Add a word with known rhymes
      (let* ([new-word "nasdofiz"]
             [r*   '("yes")]
             [a*   '("later")])
        (begin
          ;; -- assert rhymes exist
          (for ([r (in-list r*)])
            (check-true (word-exists? r)))
          (for ([a (in-list a*)])
            (check-true (word-exists? a)))
          ;; -- add word
          (add-word/unsafe new-word 11 r* a*)
          ;; -- assert word added
          (check-true (word-exists? new-word))
          (check-equal? (word->syllables new-word) 11)
          (check-equal? (id->word (word->id new-word)) new-word)
          ;; -- check rhymes
          (check-equal? (sequence->list (word->rhyme* new-word)) r*)
          (check-equal? (sequence->list (word->almost-rhyme* new-word)) a*)))

      ;; Add a word with unknown rhymes
      (let* ([new-word "hvzx8da"]
             [r* '("asvhcuivhw")]
             [a* '()])
        (begin
          ;; -- assert rhymes do NOT exist
          (for ([r (in-list r*)])
            (check-false (word-exists? r)))
          (for ([a (in-list a*)])
            (check-true (word-exists? a)))
          ;; -- add word
          (add-word/unsafe new-word 5 r* a*) ;; Should trigger a printout
          (check-true (word-exists? new-word))
          ;; -- should have no rhymes
          (check-equal? (sequence->list (word->rhyme* new-word)) '())))

      ;; -- add-word
      (let ([nonsense1 "asdv8uazxcvasd_1"]
            [nonsense2 "asdv8uazxcvasd_2"]
            [nonsense3 "asdv8uazxcvasd_3"])
        (begin
          ;; Fails in online mode, cannot tell number of syllables
          (check-exn exn:fail? (lambda () (add-word nonsense1 #:interactive? #f #:offline? #f)))
          ;; Succeeds with an explicit number of syllables
          (with-output-to-file "/dev/null" #:exists 'append
            (lambda ()
              (check-true (and (add-word nonsense2 #:syllables 32 #:interactive? #f #:offline? #f) #t))))
          ;; Succeeds offline because the naive test never fails
          (check-true (and (add-word nonsense3 #:interactive? #f #:offline? #t) #t))))

      (let ([new-word "yolo"])
        (begin
          (check-true (and (add-word "yolo" #:interactive? #f #:offline? #f) #t))))))

  ;; -- add-rhyme / add-almost-rhyme
  (with-db-test
    ;; Doesn't commit for unknown rhymes/almost
    (let ([word "word"]
          [unknown "aiopsuvhz"])
      (check-true (word-exists? word))
      (check-false (word-exists? unknown))
      (add-rhyme word unknown)
      (check-false (word-exists? unknown))
      (add-almost-rhyme word unknown)
      (check-false (word-exists? unknown))))

  (with-db-test
    ;; Succeeds for known words, doesn't matter if they actually rhyme
    (let ([w "cat"]
          [r "dog"])
      (check-true (word-exists? w))
      (check-true (word-exists? r))
      (check-false (rhymes-with? w r))
      (check-false (almost-rhymes-with? w r))
      ;; --
      (add-rhyme w r)
      (check-true (rhymes-with? w r))
      (check-false (almost-rhymes-with? w r))
      ;; --
      (add-almost-rhyme w r)
      (check-true (rhymes-with? w r))
      (check-true (and (almost-rhymes-with? w r) #t))))

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
  (with-db-test
    (begin
      (define-syntax-rule (check-word-exists?/true [word ...])
        (begin (check-true (word-exists? word)) ...))
      (check-word-exists?/true
       ["cat" "dog" "volcano" "element" "wood"])

      (define-syntax-rule (check-word-exists?/false [word ...])
        (begin (check-false (word-exists? word)) ...))
      (check-word-exists?/false
       ["tagyscv7" "4axz" ""])))

  ;; -- (almost-)rhymes-with?
  (with-db-test
    (begin
      (define-syntax-rule (check-rhymes-with?/true [w r] ...)
        (begin (check-true (rhymes-with? w r)) ...))
      (check-rhymes-with?/true
       ["plate" "mate"]
       ["inherit" "parrot"]
       ["car" "far"]
       ["criticism" "dualism"])
      (define-syntax-rule (check-rhymes-with?/false [w r] ...)
        (begin (check-false (rhymes-with? w r)) ...))
      (check-rhymes-with?/false
       ["out" "book"]
       ["waiter" "chase"]
       ["chalkboard" "goat"]
      )

      (define-syntax-rule (check-almost-rhymes-with?/true [w r] ...)
        (begin (check-true (almost-rhymes-with? w r)) ...))
      (check-almost-rhymes-with?/true
       ["criticism" "proposition"]
       ["lyric" "lithic"]
       ["bulimic" "pinprick"]
       ["pinprick" "lipstick"])
      (define-syntax-rule (check-almost-rhymes-with?/false [w r] ...)
        (begin (check-false (almost-rhymes-with? w r)) ...))
      (check-almost-rhymes-with?/false
       ["watermelon" "orange"]
       ["cat" "cat"]
       ["yoyo" "wolf"]
      )))

)
