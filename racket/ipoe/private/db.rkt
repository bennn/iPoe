#lang racket/base

(provide
  add-word add-word*
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

  ipoe-db-connected?
  ;; (-> Boolean)
  ;; True if currently connected to a database

  rhymes-with?
  ;; (->* [string? string?] [#:db connection?] boolean?)
  ;; Calling `(rhymes-with? w r)` returns true if `w` rhymes with `r`.
  ;; Results should be symmetric, but this is not guaranteed.

  syllables->word*
  ;; (->* [natural?] [#:db connection?] (sequence/c string?))
  ;; Return a sequence of words with the supplied number of syllables

  with-ipoe-db
  ;; (->* [(-> Any)] [#:commit? Boolean] Any)
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
  ;; (->* [string?] [#:db connection?] (U #f natural?))
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
  ipoe/private/parameters
  (only-in ipoe/private/ui
    alert get-user-input read-yes-or-no)
  (only-in ipoe/private/scrape
    almost-rhymes?
    resolve-syllables
    resolve-rhyme*
    rhyme-result-rhyme*
    rhyme-result-almost-rhyme*
    rhymes?
    scrape-rhyme
    scrape-word)
  (only-in ipoe/private/string
    string-empty?)
)

;; =============================================================================

;;(define DB-LOG  "./ipoe.log")

;; (: *connection* (Parameterof (U #f connection?
;;                              (HashTable String (Pairof (U #f WordResult)
;;                                                        (U #f RhymeResult)))))
(define *connection*  (make-parameter #f))
;;(define *logfile* (make-parameter #f))

(define DBNAME-PROMPT "Enter the name of your local ipoe database.")
(define DBNAME-DESCRIPTION
  (string-append "Missing run-time parameter for ipoe database name." " "
                 "Please enter the database name."))

(define USER-PROMPT "Enter your database username")
(define USER-DESCRIPTION
  (string-append "Missing run-time parameter for database username." " "
                 "Please enter a username to connect to the ipoe database."))

;; -----------------------------------------------------------------------------

;; Open a database connection & set parameters
(define (db-init #:user [u-param #f]
                 #:dbname [db-param #f]
                 #:interactive? [interactive? #f])
  ;; -- Resolve username and dbname
  (define u (or u-param
                (and interactive?
                     (get-user-input string?
                                     #:prompt USER-PROMPT
                                     #:description USER-DESCRIPTION))))
  (define db (or db-param
                 (and interactive?
                      (get-user-input string?
                                      #:prompt DBNAME-PROMPT
                                      #:description DBNAME-DESCRIPTION))))
  (cond
   [(and u db (not (or (string-empty? u) (string-empty? db))))
    ;; -- Have all parameters, try connecting to the database
    (*connection* (postgresql-connect #:user u #:database db))
    ;;(current-ipoe-log (open-output-file DB-LOG #:exists? 'error))
    (start-transaction (*connection*))
    ;; -- Ask whether to save current user & db for future sessions
    (when (or (not u-param) (not db-param))
      (case (get-user-input read-yes-or-no
                            #:prompt "Save current database preferences? (Y/N)")
       [(Y)
        ;; Write the previously unset values to the global config
        (when (not  u-param)  (save-option 'user u))
        (when (not db-param)  (save-option 'dbname db))]
       [(N) (void)]))]
   [else
    (when interactive?
      (alert "Starting ipoe without a database connection (in online-only mode)"))
    (*connection* (make-hasheq))])
  ;; -- Return the new connection
  (*connection*))

;; Close a database connection & unset parameters
(define (db-close #:db [pgc (*connection*)]
                  #:commit? [commit? #t])
  ;;(close-output-port (current-ipoe-log))
  (when (connection? pgc)
    (if commit?
      (commit-transaction pgc)
      (rollback-transaction pgc))
    (disconnect pgc)
    (*connection* #f)
    ;;(current-ipoe-log #f)
    (void)))

(define (with-ipoe-db thunk #:user [u #f]
                            #:dbname [db #f]
                            #:commit? [commit? #t]
                            #:interactive? [i #f])
  ;; Open a database connection
  (let* ([pgc (db-init #:user u #:dbname db #:interactive? i)])
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

(define-syntax-rule (query-error loc query)
  (db-error loc "Cannot search for ~a, currently disconnected from database and internet." query))

;; -----------------------------------------------------------------------------

;; Add a new word to the database
(define (add-word word
                  #:db [pgc (*connection*)]
                  #:syllables [syllables-param #f]
                  #:rhymes [rhyme-param '()]
                  #:almost-rhymes [almost-rhyme-param '()]
                  #:interactive? [interactive? #f]
                  #:offline? [offline? #f])
  (when interactive?
    (alert (format "Adding new word '~a' to the database" word)))
  (cond
   [(online-mode? pgc)
    (when interactive?
      (alert (format "Cannot add word '~a', currently in online-only mode")))
    #f]
   [(not (connection? pgc))
    (when interactive?
      (alert (format "Cannot add word '~a', not connected to a database.")))
    #f]
   [(word-exists? word #:db pgc)
    (duplicate-word-error (format "Cannot add word '~a', already in database" word))]
   [else
    (define syllables (resolve-syllables word syllables-param #:interactive? interactive? #:offline? offline?))
    (unless syllables (db-error 'add-word "Cannot add word '~a', failed to infer syllables. Try again with explicit #:syllables argument." word))
    (define rr (resolve-rhyme* word rhyme-param almost-rhyme-param #:interactive? interactive? #:offline? offline?))
    (define rhyme* (rhyme-result-rhyme* rr))
    (define almost-rhyme* (rhyme-result-almost-rhyme* rr))
    (add-word/unsafe word syllables rhyme* almost-rhyme* #:db pgc)]))

(define (add-word* word*
                   #:db [pgc (*connection*)]
                   #:interactive? [interactive? #f]
                   #:offline? [offline? #f])
  (for ([w (in-list word*)])
    (add-word w #:db pgc #:interactive? interactive? #:offline? offline?)))

;; Add one new rhyme for a word
(define (add-rhyme word r #:db [pgc (*connection*)])
  (add-rhyme* word (list r) #:db pgc))

;; Add multiple new rhymes for a word
(define (add-rhyme* word r* #:db [pgc (*connection*)])
  (define wid (or (word->id word #:db pgc)
                  (db-error "Cannot add rhyme for unknown word '~a'" word)))
  (add-rhyme*/unsafe wid r* #:db pgc))

(define (add-almost-rhyme word a #:db [pgc (*connection*)])
  (add-almost-rhyme* word (list a) #:db pgc))

(define (add-almost-rhyme* word a* #:db [pgc (*connection*)])
  (define wid (or (word->id word #:db pgc)
                  (db-error "Cannot add almost-rhyme for unknown word '~a'" word)))
  (add-almost-rhyme*/unsafe wid a* #:db pgc))

(define (add-word/unsafe word syllables rhyme* almost-rhyme* #:db [pgc (*connection*)])
  (query-exec pgc "INSERT INTO word (word, num_syllables) VALUES ($1, $2);" word syllables)
  (define wid (word->id word #:db pgc))
  (unless wid (db-error 'add-word "Cannot find ID for newly-added word '~a'" word))
  (add-rhyme*/unsafe wid rhyme* #:db pgc)
  (add-almost-rhyme*/unsafe wid almost-rhyme* #:db pgc))

(define (add-rhyme*/unsafe wid rhyme* #:db [pgc (*connection*)])
  (add-r*/unsafe wid rhyme* #:db pgc #:table 'rhyme))

(define (add-almost-rhyme*/unsafe wid almost-rhyme* #:db [pgc (*connection*)])
  (add-r*/unsafe wid almost-rhyme* #:db pgc #:table 'almost_rhyme))

;; Abstraction for adding rhymes or almost-rhymes
(define (add-r*/unsafe wid r* #:db [pgc (*connection*)] #:table loc)
  ;; It's maybe better to build one large insert statement,
  ;;  but I'm not sure how to format that for racket/db
  ;; (At least, this way should give better errors & commit partial successes)
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
(define (find-word word-property #:db [pgc (*connection*)] #:column [col-param #f])
  (assert-connected pgc #:src 'find-word)
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
(define (find-r [wid #f] [rid #f] #:db [pgc (*connection*)] #:table loc)
  (assert-connected pgc #:src 'find-rhyme)
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

(define (id->word wid #:db [pgc (*connection*)])
  (match (find-word wid #:db pgc #:column 'id)
    [#f #f]
    [(vector id word syllables) word]))

(define (syllables->word* num-syllables #:db [pgc (*connection*)])
  (assert-connected pgc #:src 'syllables->word*)
  (in-query pgc "SELECT word FROM word WHERE word.num_syllables=$1" num-syllables))

(define (word->id word #:db [pgc (*connection*)])
  (match (find-word word #:column 'word #:db pgc)
    [#f #f]
    [(vector id word syllables) id]))

(define (word->almost-rhyme* word #:db [pgc (*connection*)])
  (word->r* word #:db pgc #:table 'almost_rhyme))

(define (word->r* word #:db pgc #:table loc)
  (define wid (word->id word #:db pgc))
  (unless wid (db-error loc "Cannot find ~a for unknown word '~a'" loc word))
  (define rid* (find-r wid #:db pgc #:table loc))
  (sequence-map (lambda (wid rid) (id->word rid #:db pgc)) rid*))

(define (word->rhyme* word #:db [pgc (*connection*)])
  (word->r* word #:db pgc #:table 'rhyme))

(define (word->syllables word #:db [pgc (*connection*)])
  (match (find-word word #:db pgc #:column 'word)
    [#f #f]
    [(vector id word syllables) syllables]))

;; -----------------------------------------------------------------------------
;; --- 

(define (scrape/cache tag w #:cache c
                            #:scrape f-scrape)
  (cond
   [(online-mode? c)
    ;; If word is unbound, scrape.
    ;; Also initialize an placeholder rhyme result.
    (define-values [sel inj]
      (case tag
       [(word)  (values car (lambda (x) (cons x #f)))]
       [(rhyme) (values cdr (lambda (x) (cons #f x)))]
       [else (error 'scrape/cache
                    (format "Undefined tag '~e'" tag))]))
    (define (on-failure)
      (define R (f-scrape w))
      (and R (let ([wr+rr (inj R)])
               (hash-set! c w wr+rr)
               wr+rr)))
    (define res (hash-ref c w on-failure))
    (cond
     [(eq? #f res)
      ;; Nothing cached & scraping (in `on-failure`) failed.
      #f]
     [(not (pair? res))
      (error 'scrape/cache (format "Expected a pair, got '~e'" res))]
     [(eq? #f (sel res))
      ;; Have other result, need to apply `f-scrape` to get this result
      (define R (f-scrape w))
      (and R
           (hash-set! c w (inj R))
           R)]
     [else
      (sel res)])]
   [else
    ;; Just lookup, don't bother with the cache
    (scrape-word w)]))

(define (scrape-word/cache w)
  (scrape/cache 'word w #:cache (*connection*)
                        #:scrape scrape-word))

(define (scrape-rhyme/cache w)
  (scrape/cache 'rhyme w #:cache (*connection*)
                         #:scrape scrape-rhyme))

;; -----------------------------------------------------------------------------
;; --- queries
;;     These are permitted in online mode

;; True if `word` is already in the database
(define (word-exists? word #:db [pgc (*connection*)])
  (cond
   [(connection? pgc)
    (and (find-word word #:column 'word #:db pgc) #t)]
   [(online-mode? pgc)
    (and (scrape-word/cache word) #t)]
   [else
    (query-error 'word-exists? (format "word '~a'" word))]))

(define (almost-rhymes-with? w r #:db [pgc (*connection*)])
  (cond
   [(connection? pgc)
    (r-with? w r #:db pgc #:table 'almost_rhyme)]
   [(online-mode? pgc)
    (almost-rhymes? (scrape-rhyme/cache w) r)]
   [else
    (query-error 'almost-rhymes-with? (format "rhymes of '~a'" w))]))

(define (rhymes-with? w r #:db [pgc (*connection*)])
  (cond
   [(connection? pgc)
    (r-with? w r #:db pgc #:table 'rhyme)]
   [(online-mode? pgc)
    (rhymes? (scrape-rhyme/cache w) r)]
   [else
    (query-error 'rhymes-with? (format "rhymes of '~a'" w))]))

(define (r-with? w r #:db [pgc (*connection*)] #:table loc)
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

(define (assert-connected pgc #:src loc)
  (unless (connection? pgc)
    (error (string->symbol (format "ipoe:db:~a" loc))
           (format "Expected a database connection, got '~e'" pgc))))

(define (assert-rhyme-table? sym #:src loc)
  (unless (rhyme-table? sym)
    (table-error loc sym)))

(define (ipoe-db-connected?)
  (connection? (*connection*)))

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

(define (online-mode? pgc)
  (hash? pgc))

(define (validate-word-column col)
  (cond
   [(or (eq? col 'id) (eq? col 'word) (eq? col 'num_syllables))
    col]
   [else
    #f]))

;; =============================================================================

(module+ main
  (require racket/cmdline xrepl racket/string)
  ;; -- parameters
  (define commit? (make-parameter #f))
  (define output-file (make-parameter #f))
  (define natural? exact-nonnegative-integer?)
  (define (exit? s)
    (memq s '(exit q quit)))
  (define (skip n seq)
    (for ([x seq] [m (in-range n)]) (void)))
  (define (take n seq)
    (for/list ([x seq] [m (in-range n)]) x))
  ;; -- repl
  (command-line
   #:program "db-repl"
   #:once-each
    [("-c" "--commit") "Commit to database" (commit? #t)]
    [("-o" "--output") o-p "Save interactions to file" (output-file o-p)]
   #:args ()
   (begin
     (printf "Initializing DB connection & starting REPL ...\n")
     (with-ipoe-db #:commit? (commit?)
       (lambda ()
         (let loop ()
           (display "ipoe> ")
           (match (read)
            [(or (? eof-object?)
                 (? exit?)
                 (and (? list?) (? (lambda (x) (exit? (car x))))))
             (printf "Goodbye\n")]
            [(list 'id->word (? natural? n))
             (displayln (id->word n))
             (loop)]
            [(list 'rhymes-with? (? string? sym1) (? string? sym2))
             (displayln (rhymes-with? sym1 sym2))
             (loop)]
            [(list 'syllables->word* (? natural? n1) '#:limit (? natural? n2))
             (displayln (take n2 (syllables->word* n1)))
             (loop)]
            [(list 'syllables->word* (? natural? n1) '#:limit (? natural? n2) '#:skip (? natural? n3))
             (define s (syllables->word* n1))
             (skip n3 s)
             (displayln (take n2 s))
             (loop)]
            [(list 'word->almost-rhyme* (? string? w) '#:limit (? natural? n))
             (displayln (take n (word->almost-rhyme* w)))
             (loop)]
            [(list 'word->almost-rhyme* (? string? w) '#:limit (? natural? n) '#:skip (? natural? n2))
             (define s (word->almost-rhyme* w))
             (skip n2 s)
             (displayln (take n s))
             (loop)]
            [(list 'word->id (? string? w))
             (displayln (word->id w))
             (loop)]
            [(list 'word->rhyme* (? string? w) '#:limit (? natural? n1))
             (displayln (take n1 (word->rhyme* w)))
             (loop)]
            [(list 'word->rhyme* (? string? w) '#:limit (? natural? n1) '#:skip (? natural? n2))
             (define s (word->rhyme* w))
             (skip n2 s)
             (displayln (take n1 s))
             (loop)]
            [(list 'word->syllables (? string? w))
             (displayln (word->syllables w))
             (loop)]
            [(list 'word-exists? (? string? w))
             (displayln (word-exists? w))]
            [x
             (printf "Unknown command '~a'\n" x)
             (loop)])))))))

;; =============================================================================

(module+ test
  (require rackunit racket/sequence "rackunit-abbrevs.rkt")

  (define o*
    (if (file-exists? IPOE-CONFIG)
        (error 'ipoe:db:test "Detected local config file '~a', please delete or change directories before running tests.")
        (options-init)))

  (define-syntax-rule (with-db-test e)
    (parameterize-from-hash o* (lambda ()
      (with-ipoe-db #:commit? #f
                    #:interactive? #f
                    #:user (*user*)
                    #:dbname (*dbname*)
        (lambda () e)))))

  ;; -- find-word
  (with-db-test
    (check-apply* find-word
      ["yoga" == '#(679928 "yoga" 2)]
      ["pal"  == '#(614060 "pal" 1)]
      ["sweet" == '#(653512 "sweet" 1)]
      ["blimp" == '#(526807 "blimp" 1)]
      ["anonymous" == '#(517406 "anonymous" 4)]))

  ;; --- without a DB, should get a "useful" error message
  (check-exn (regexp "ipoe:db:find-word")
             (lambda () (find-word "yolo")))
  ;; --- online-mode
  (check-exn (regexp "ipoe:db:find-word")
             (lambda ()
               (with-ipoe-db #:commit? #f (lambda () (find-word "apple")))))

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

   (check-exn (regexp "ipoe:db:syllables->word*")
     (lambda () (syllables->word* 3)))
   (check-exn (regexp "ipoe:db:syllables->word*")
     (lambda () (with-ipoe-db #:commit? #f (lambda () (syllables->word* 3)))))

  ;; -- id->word
  (define-syntax-rule (check-id-word-bijection [w ...])
    ;; Spot-check id->word = word->id
    (with-db-test
      (begin (check-equal? (id->word (word->id w)) w) ...)))
  (check-id-word-bijection
   ["yes" "under" "africa" "polymorphism" "a" "heating"])

  ;; -- ipoe-db-connected?
  (check-false (ipoe-db-connected?))
  (with-ipoe-db #:commit? #f
    (lambda () (check-false (ipoe-db-connected?))))
  (with-db-test
    (check-true (ipoe-db-connected?)))

  ;; -- infer-word-column
  (check-apply* infer-word-column
   ["yolo" == 'word]
   ["a13t" == 'word]
   [""     == 'word]
   ["word" == 'word]
   ;; --
   [1      == 'num_syllables]
   [3      == 'num_syllables]
   [7      == 'num_syllables]
   [40     == 'num_syllables]
   ;; --
   [0       == 'id]
   [-51     == 'id]
   [55      == 'id]
   [8675309 == 'id])

  ;; -- online-mode?
  (check-false (online-mode? (*connection*)))
  (check-false (online-mode? #f))
  (check-false (online-mode? #t))
  (with-db-test
    (check-false (online-mode? (*connection*))))
  (parameterize ([*connection* (make-hasheq)])
    (check-true (online-mode? (*connection*))))

  ;; -- validate-word-column
  (check-apply* validate-word-column
   ['id == 'id]
   ['word == 'word]
   ['num_syllables == 'num_syllables]
   ;; --
   ['part_of_speech == #f]
   ['nothing == #f]
   ['genre == #f]
   ["id" == #f]
   ["word" == #f]
   ['() == #f]
   [5 == #f]
  )

  ;; -- word->id
  ;; Should return #f for unknown words
  (with-db-test
    (begin
     (check-false* word->id
       [""]
       ["aasdfqehudv"]
     )
     (check-apply* word->id
       ["cat" == 532756]
       ["aardvark" == 511396]
       ["holy" == 573983]
       ["chalk" == 534003]
       ["soldier" == 645783])
     (check-apply* word->id
       ["baseball" != 420]
       ["yolo" != 1]
       ["demon" != 12])))

   (check-exn (regexp "ipoe:db:find-word")
     (lambda () (word->id "sea")))
   (check-exn (regexp "ipoe:db:find-word")
     (lambda () (with-ipoe-db #:commit? #f (lambda () (word->id "car")))))

  ;; -- word->syllables
  (with-db-test
    (begin
      (check-apply* word->syllables
        ["" == #f]
        ["once" == 1]
        ["never" == 2]
        ["adamant" == 3]
        ["stallion" == 2]
        ["chrysanthemum" == 4]
        ["metallurgy" == 4]
        ["antihistamine" == 5]
        ["academically" == 6])
      (check-apply* word->syllables
        ["" != 11]
        ["mary" != 4]
        ["element" != 1]
        ["balloon" != 3]
        ["hour" != 2])))

  (check-exn (regexp "ipoe:db:find-word")
    (lambda () (word->syllables "yes")))
  (check-exn (regexp "ipoe:db:find-word")
    (lambda ()
      (with-ipoe-db #:commit? #f (lambda () (word->syllables "yes")))))

  ;; -- add-word/unsafe
  (let ([new-word "ycvgpadfwd"])
    ;; Add a word without rhymes
    (with-db-test
      (begin
        (add-word/unsafe new-word 1 '() '())
        ;; New word should be defined
        (check-true (word-exists? new-word))
        (check-equal? (word->syllables new-word) 1))))

  ;; Add a word with known rhymes
  (let* ([new-word "nasdofiz"]
         [r*   '("yes")]
         [a*   '("later")])
   (with-db-test
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
       (check-equal? (sequence->list (word->almost-rhyme* new-word)) a*))))

  ;; Add a word with unknown rhymes
  (let* ([new-word "hvzx8da"]
         [r* '("asvhcuivhw")]
         [a* '()])
    (with-db-test
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
        (check-equal? (sequence->list (word->rhyme* new-word)) '()))))

  ;; -- add-word
  (let ([nonsense1 "asdv8uazxcvasd_1"]
        [nonsense2 "asdv8uazxcvasd_2"]
        [nonsense3 "asdv8uazxcvasd_3"])
    (with-db-test
      (begin
        ;; Fails in online mode, cannot tell number of syllables
        (check-exn exn:fail? (lambda () (add-word nonsense1 #:interactive? #f #:offline? #f)))
        ;; Succeeds with an explicit number of syllables
        (with-output-to-file "/dev/null" #:exists 'append
          (lambda ()
            (check-true (and (add-word nonsense2 #:syllables 32 #:interactive? #f #:offline? #f) #t))))
        ;; Succeeds offline because the naive test never fails
        (check-true (and (add-word nonsense3 #:interactive? #f #:offline? #t) #t)))))

  ;; --- add-word failures
  (let ([new-word "asdhvuianjsdkvasd"])
    (check-false (add-word new-word))
    (check-false
      (with-ipoe-db #:commit? #f (lambda () (add-word new-word)))))


  ;; -- add-word* (when offline?, never fails)
  (with-db-test
    (let ([new1 "onething"]
          [new2 "anotherthing"])
      (with-output-to-file "/dev/null" #:exists 'append
        (lambda ()
          (check-true (and (add-word* (list new1 new2) #:interactive? #f #:offline? #t) #t))))
      (check-true (word-exists? new1))
      (check-true (word-exists? new2))))

  (let ([w* '("rafflehsnarcuvjawe" "vpoawetz")])
    (with-output-to-file "/dev/null" #:exists 'append
      (lambda ()
        (check-true (and (add-word* w* #:interactive? #f #:offline? #t) #t))))
    (with-db-test
      (check-false (word-exists? (car w*)))))

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

   (let ([w "asdfasv"]
         [v "avhoiuswvp"])
     ;; Fails when disconnected from DB
     (check-exn (regexp "ipoe:db:find-word")
       (lambda () (add-rhyme w v)))
     (check-exn (regexp "ipoe:db:find-word")
       (lambda ()
         (with-ipoe-db #:commit? #f (lambda () (add-rhyme w v))))))

  ;; -- (assert-)rhyme-table?
  (check-true* rhyme-table?
    ['rhyme]
    ['almost_rhyme]
  )

  (check-false* rhyme-table?
    ['r]
    ['am]
  )

  (check-true (and (assert-rhyme-table? 'rhyme #:src 'test) #t))
  (check-true (and (assert-rhyme-table? 'almost_rhyme #:src 'test) #t))

  (check-exn exn:fail? (lambda () (assert-rhyme-table? 'foo #:src 'test)))
  (check-exn exn:fail? (lambda () (assert-rhyme-table? 'invalid #:src 'test)))

  ;; -- word-exists?
  (with-db-test
    (begin
      (check-true* word-exists?
        ["cat"]
        ["dog"]
        ["volcano"]
        ["element"]
        ["wood"])

      (check-false* word-exists?
       ["tagyscv7"]
       ["4axz"]
       [""])))

  ;; Fails if no DB and offline
  (check-exn (regexp "ipoe:db:word-exists?")
    (lambda () (word-exists? "yes")))

  ;; Succeeds in online mode (for real words)
  (with-ipoe-db #:commit? #f (lambda ()
    (check-true (word-exists? "paper"))))

  ;; -- (almost-)rhymes-with?
  (with-db-test
    (begin
      (check-true* rhymes-with?
       ["plate" "mate"]
       ["inherit" "parrot"]
       ["car" "far"]
       ["criticism" "dualism"])
      (check-false* rhymes-with?
       ["out" "book"]
       ["waiter" "chase"]
       ["chalkboard" "goat"]
      )

      (check-true* almost-rhymes-with?
       ["criticism" "proposition"]
       ["lyric" "lithic"]
       ["bulimic" "pinprick"]
       ["pinprick" "lipstick"])
      (check-false* almost-rhymes-with?
       ["watermelon" "orange"]
       ["cat" "cat"]
       ["yoyo" "wolf"]
      )))

  ;; Fails if no DB and offline
  (check-exn (regexp "ipoe:db:rhymes-with?")
    (lambda () (rhymes-with? "yes" "yes")))
  (check-exn (regexp "ipoe:db:almost-rhymes-with?")
    (lambda () (almost-rhymes-with? "yes" "yes")))

  ;; Succeeds in online mode (for real words)
  (with-ipoe-db #:commit? #f (lambda ()
    (check-true (rhymes-with? "paper" "draper"))
    (check-true (almost-rhymes-with? "paper" "pager"))))

  ;; -- TODO test cache
  ;; -- TODO test init prompt for username
  ;; -- TODO test init prompt for dbname
  ;; -- TODO test init, save preferences
  ;; -- TODO test online-only queries


)
