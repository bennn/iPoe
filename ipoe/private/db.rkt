#lang racket/base

;; TODO watch for adding duplicate rhyme / almost-rhyme

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

  create-ipoe-tables
  ;; (-> boolean?)
  ;; Create all iPoe tables

  id->word
  ;; (->* [natural/c] [#:db connection?] string?)
  ;; Convert a primary key to its matching word string.

  ipoe-db-connected?
  ;; (-> Boolean)
  ;; True if currently connected to a database

  remove-word
  ;; (->* string? [#:db connection?] boolean?)
  ;; Delete a word from the database and all its connections to other words.

  rhymes-with?
  ;; (->* [string? string?] [#:db connection?] boolean?)
  ;; Calling `(rhymes-with? w r)` returns true if `w` rhymes with `r`.
  ;; Results should be symmetric, but this is not guaranteed.

  syllables->word*
  ;; (->* [natural?] [#:db connection?] (sequence/c string?))
  ;; Return a sequence of words with the supplied number of syllables

  with-ipoe-db
  ;; (->* [(-> Any)] [TODO] Any)
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
  (only-in ipoe/private/db/migrate
    TABLE*)
  (only-in ipoe/private/ui
    alert
    get-user-input
    read-yes-or-no
    read-string)
  (only-in ipoe/private/scrape
    almost-rhymes?
    resolve-syllables
    resolve-rhyme*
    rhyme-result-rhyme*
    rhyme-result-almost-rhyme*
    rhymes?
    scrape-rhyme
    scrape-word
    word-result-num-syllables) ;; ick, could also make a scrape-syllables
  (only-in ipoe/private/string
    string-empty?)
  (only-in racket/serialize
    deserialize
    serialize)
  (only-in racket/file
    file->string
    file->value)
)

;; =============================================================================

;;(define DB-LOG  "./ipoe.log")

;; (: *connection* (Parameterof (U #f connection?
;;                              (HashTable String
;;                                         (Pairof (U #f WordResult)
;;                                                 (U #f RhymeResult)))))
(define *connection*  (make-parameter #f))

;;(define *logfile* (make-parameter #f))

(define DBNAME-PROMPT "Enter the name of your local ipoe database (Leave blank to skip):")
(define DBNAME-DESCRIPTION
  (string-append "Missing run-time parameter for ipoe database name. "
                 "Please enter the database name."))

(define USER-PROMPT "Enter your database username (Leave blank to skip):")
(define USER-DESCRIPTION
  (string-append "Missing run-time parameter for database username. "
                 "Please enter a username to connect to the ipoe database."))

;; -----------------------------------------------------------------------------

;; Boxes detect if user has saved DB preferences during this execution.
;; Saves us from re-compiling the options (and re-reading the poem)
;; or passing flags back up to the caller's stack frame to bind *user*
(define adhoc-user (box #f))
(define adhoc-dbname (box #f))

;; Open a database connection & set parameters
(define (db-init #:user [u-param #f]
                 #:dbname [db-param #f]
                 #:online? [online? #f] ;; 2015-09-19: should maybe redesign
                 #:interactive? [interactive? #f])
  ;; -- Resolve username and dbname
  (define u (or u-param
                (unbox adhoc-user)
                (and interactive?
                     (not online?)
                     (get-user-input read-string
                                     #:prompt USER-PROMPT
                                     #:description USER-DESCRIPTION))))
  (define db (or db-param
                 (unbox adhoc-dbname)
                 (and interactive?
                      (not online?)
                      (get-user-input read-string
                                      #:prompt DBNAME-PROMPT
                                      #:description DBNAME-DESCRIPTION))))
  (cond
   [(and (not online?) u db (not (or (string-empty? u) (string-empty? db))))
    ;; -- Have all parameters, try connecting to the database
    (*connection*
      (let ([on-err (lambda (e)
                      (raise-user-error (format "Failed to connect to database '~a' as user '~a'. Shutting down." db u)))])
        (with-handlers ([exn:fail:sql? on-err])
          (postgresql-connect #:user u #:database db))))
    ;;(current-ipoe-log (open-output-file DB-LOG #:exists? 'error))
    (start-transaction (*connection*))
    ;; -- Ask whether to save current user & db for future sessions
    (define got-new-user? (and (not u-param) (not (unbox adhoc-user))))
    (define got-new-dbname? (and (not db-param) (not (unbox adhoc-dbname))))
    (when (or got-new-user? got-new-dbname?)
      (case (get-user-input read-yes-or-no
                            #:prompt "Save current database preferences? (Y/N)")
       [(Y)
        ;; Write the previously unset values to the global config
        (when got-new-user?
          (set-box! adhoc-user u)
          (save-option 'user u))
        (when got-new-dbname?
          (set-box! adhoc-dbname db)
          (save-option 'dbname db))]
       [(N) (void)]))]
   [else
    (when interactive?
      (alert "Starting ipoe without a database connection (in online-only mode)"))
    (*connection* (read-cache))])
  ;; -- Return the new connection
  (*connection*))

;; Close a database connection & unset parameters
(define (db-close #:db [pgc (*connection*)]
                  #:commit? [commit? #t])
  ;;(close-output-port (current-ipoe-log))
  (cond
   [(connection? pgc)
    ;; -- Close transaction & connection
    (if commit?
      (commit-transaction pgc)
      (rollback-transaction pgc))
    (disconnect pgc)]
   [(online-mode? pgc)
    (write-cache (*connection*))])
  (*connection* #f)
  (void))

(define (with-ipoe-db thunk #:user [u #f]
                            #:dbname [db #f]
                            #:commit? [commit? #t]
                            #:online? [o #f]
                            #:interactive? [i #f])
  ;; Open a database connection
  (let* ([pgc (db-init #:user u #:dbname db #:online? o #:interactive? i)])
    (call-with-exception-handler
      ;; On error, close the DB connection
      (lambda (exn)
        (db-close #:db pgc #:commit? commit?)
        exn)
      ;; Execute thunk with parameter set, close DB when finished
      (lambda ()
        (let ([result (thunk)])
          (db-close #:db pgc #:commit? commit?)
          result)))))

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
  (when (*verbose*)
    (displayln (format "[WARNING ipoe:db:~a] : ~a" loc (format msg arg* ...)))))

(define-syntax-rule (duplicate-word-error w)
  ;; Should never happen, so raise an error
  (error 'ipoe:db (format "WARNING: word '~a' is not unique in the database" w)))

(define-syntax-rule (table-error sym loc)
  (db-error sym "Cannot infer table from symbol '~a'" loc))

(define-syntax-rule (query-error loc query)
  (db-error loc "Cannot search for ~a, currently disconnected from database and internet." query))

;; -----------------------------------------------------------------------------
;; --- misc validation

(define (assert-connected pgc #:src loc)
  (unless (connection? pgc)
    (db-error loc "Expected a database connection, got '~e'" pgc)))

(define (assert-rhyme-table? sym #:src loc)
  (unless (rhyme-table? sym)
    (table-error loc sym)))

(define (ipoe-db-connected?)
  (connection? (*connection*)))

(define (infer-word-column param)
  (cond
   [(string? param)
    'word]
   [(exact-nonnegative-integer? param)
    'id]
   [else
    (db-error 'infer-word-column "Cannot infer column in word database from parameter '~a'" param)]))

(define (online-mode? pgc)
  (hash? pgc))

(define (rhyme-table? sym)
  (and (memq sym '(rhyme almost_rhyme))
       #t))

;; -----------------------------------------------------------------------------
;; --- DB-only queries

(define (find-word word-property #:db [pgc (*connection*)]
                                 #:column [col-param #f])
  (assert-connected pgc #:src 'find-word)
  (define col (or col-param (infer-word-column word-property)))
  (case col
    [(id)
     (query-maybe-row pgc "SELECT * FROM word WHERE word.id=$1" word-property)]
    [(word)
     (query-maybe-row pgc "SELECT * FROM word WHERE word.word=$1" word-property)]
    [else
     (db-error 'find-word (format "Invalid column for word table '~a'" col))]))

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

(define (almost-rhymes-with?/id wid aid #:db [pgc (*connection*)])
  (and (find-r wid aid #:db pgc #:table 'almost_rhyme) #t))

(define (rhymes-with?/id wid rid #:db [pgc (*connection*)])
  (and (find-r wid rid #:db pgc #:table 'rhyme) #t))

(define (has-syllables? word syll #:db [pgc (*connection*)])
  (assert-connected pgc #:src 'has-syllables)
  (define wid (word->id/fail word #:db pgc #:src 'has-syllables?))
  (has-syllables?/id wid syll #:db pgc))

(define (has-syllables?/id wid syll #:db [pgc (*connection*)])
  (assert-connected pgc #:src 'has-syllables/id)
  (and (query-maybe-row pgc "SELECT word FROM word_syllables WHERE word=$1 AND syllables=$2;" wid syll)
       #t))

(define (id->word wid #:db [pgc (*connection*)])
  (match (find-word wid #:db pgc #:column 'id)
    [#f #f]
    [(vector id word) word]))

(define (syllables->word* syll #:db [pgc (*connection*)])
  (assert-connected pgc #:src 'syllables->word*)
  (define (id->word/pgc wid)
    (or (id->word wid #:db pgc)
        (db-error "syllables->word returned invalid ID ~a" wid)))
  (sequence-map id->word/pgc
    (in-query pgc "SELECT word FROM word_syllables WHERE syllables=$1" syll)))

(define (word->id word #:db [pgc (*connection*)])
  (match (find-word word #:column 'word #:db pgc)
    [#f #f]
    [(vector id word) id]))

(define (word->id/fail word #:src src #:db [pgc (*connection*)])
  (or (word->id word #:db pgc)
      (db-error 'word->id "Word '~a' does not exist, cannot '~a'" word src)))

;; -----------------------------------------------------------------------------
;; --- online or DB queries

;; Abbreviation for almost-rhymes-with? and rhymes-with?
;; 'rhymes-with or fail'
(define-syntax-rule (rw/fail f w r pgc src)
  (let* ([wid (word->id/fail w #:db pgc #:src src)]
         [rid (word->id/fail r #:db pgc #:src src)])
    (f wid rid #:db pgc)))

(define (almost-rhymes-with? w r #:db [pgc (*connection*)])
  (cond
   [(connection? pgc)
    (rw/fail almost-rhymes-with?/id w r pgc 'almost_rhyme)]
   [(online-mode? pgc)
    (almost-rhymes? (scrape-rhyme/cache w) r)]
   [else
    (query-error 'almost-rhymes-with? (format "almost-rhymes of '~a'" w))]))

(define (rhymes-with? w r #:db [pgc (*connection*)])
  (cond
   [(connection? pgc)
    (rw/fail rhymes-with?/id w r pgc 'rhyme)]
   [(online-mode? pgc)
    (rhymes? (scrape-rhyme/cache w) r)]
   [else
    (query-error 'rhymes-with? (format "rhymes of '~a'" w))]))

;; True if `word` is already in the database
(define (word-exists? word #:db [pgc (*connection*)])
  (cond
   [(connection? pgc)
    (and (find-word word #:column 'word #:db pgc) #t)]
   [(online-mode? pgc)
    (and (scrape-word/cache word) #t)]
   [else
    (query-error 'word-exists? (format "word '~a'" word))]))

(define (word->syllables word #:db [pgc (*connection*)])
  (cond
   [(connection? pgc)
    (define wid (word->id/fail word #:db pgc #:src 'word->syllables))
    (query-maybe-value pgc "SELECT syllables FROM word_syllables WHERE word=$1;" wid)]
   [(online-mode? pgc)
    (let ([r (scrape-word/cache word)])
      (and r (word-result-num-syllables r)))]
   [else
    (query-error 'word->syllables (format "syllables of '~a'" word))]))

(define (word->almost-rhyme* word #:db [pgc (*connection*)])
  (word->r* word #:db pgc #:table 'almost_rhyme))

(define (word->rhyme* word #:db [pgc (*connection*)])
  (word->r* word #:db pgc #:table 'rhyme))

(define (word->r* word #:db pgc #:table loc)
  (cond
   [(connection? pgc)
    (define wid (word->id/fail word #:db pgc #:src loc))
    (define rid* (find-r wid #:db pgc #:table loc))
    (sequence-map (lambda (wid rid) (id->word rid #:db pgc)) rid*)]
   [(online-mode? pgc)
    (define rr (scrape-rhyme/cache word))
    (case loc
     [(almost_rhyme)
       (rhyme-result-almost-rhyme* rr)]
     [(rhyme)
      (rhyme-result-rhyme* rr)])]
   [else
    (query-error loc (format "~a of ~a" loc word))]))

;; -----------------------------------------------------------------------------
;; --- add-syllables

(define (add-syllables word syll #:db [pgc (*connection*)])
  (define wid (word->id/fail word #:db pgc #:src 'add-syllables))
  (add-syllables/id wid syll #:db pgc))

(define (add-syllables/id wid syll #:db [pgc (*connection*)])
  (assert-connected pgc #:src 'add-syllables/id)
  (if (has-syllables?/id wid syll #:db pgc)
      (db-warning "Word '~a' already has ~a syllables" (id->word wid #:db pgc) syll)
      (add-syllables/unsafe wid syll #:db pgc)))

;; Directly add a (word_id,syllables) pair to the database
;; Please don't call this directly!
(define (add-syllables/unsafe wid syll #:db [pgc (*connection*)])
  (query-exec pgc "INSERT INTO word_syllables (word, syllables) VALUES ($1, $2);" wid syll))

;; -----------------------------------------------------------------------------
;; --- add-rhyme

;; Add multiple new rhymes for a word
(define (add-rhyme* w r* #:db [pgc (*connection*)])
  (for ([r (in-list r*)])
    (add-rhyme w r #:db pgc)))

(define (add-rhyme*/id wid r* #:db [pgc (*connection*)])
  (for ([r (in-list r*)])
    (define rid (word->id/fail r #:db pgc #:src 'add-rhyme*/id))
    (add-rhyme/id wid rid #:db pgc)))

;; Add one new rhyme for a word
(define (add-rhyme w r #:db [pgc (*connection*)])
  (define wid (word->id/fail w #:db pgc #:src 'add-rhyme))
  (define rid (word->id/fail r #:db pgc #:src 'add-rhyme))
  (add-rhyme/id wid rid #:db pgc))

(define (add-rhyme/id wid rid #:db [pgc (*connection*)])
  (unless (rhymes-with?/id wid rid #:db pgc)
    (add-rhyme/unsafe wid rid #:db pgc)))

(define (add-rhyme/unsafe wid rid #:db [pgc (*connection*)])
  (query-exec pgc
    "INSERT INTO word_rhymes (word, rhyme) VALUES ($1, $2);"
    wid rid))

;; --- add-almost-rhyme

(define (add-almost-rhyme* w a* #:db [pgc (*connection*)])
  (for ([a (in-list a*)])
    (add-almost-rhyme w a #:db pgc)))

(define (add-almost-rhyme*/id wid a* #:db [pgc (*connection*)])
  (for ([a (in-list a*)])
    (define aid (word->id/fail a #:db pgc #:src 'add-almost-rhyme*/id))
    (add-almost-rhyme/id wid aid #:db pgc)))

(define (add-almost-rhyme w a #:db [pgc (*connection*)])
  (define wid (word->id/fail w #:db pgc #:src 'add-almost-rhyme))
  (define aid (word->id/fail a #:db pgc #:src 'add-almost-rhyme))
  (add-almost-rhyme/id wid aid #:db pgc))

(define (add-almost-rhyme/id wid aid #:db [pgc (*connection*)])
  (unless (almost-rhymes-with?/id wid aid #:db pgc)
    (add-almost-rhyme/unsafe wid aid #:db pgc)))

(define (add-almost-rhyme/unsafe wid aid #:db [pgc (*connection*)])
  (query-exec pgc
    "INSERT INTO word_almost_rhymes (word, almost_rhyme) VALUES ($1, $2);"
    wid aid))

;; -----------------------------------------------------------------------------
;; --- add-word

;; Iteratively add words to the database
(define (add-word* word*
                   #:db [pgc (*connection*)]
                   #:interactive? [interactive? #f]
                   #:offline? [offline? #f])
  (for ([w (in-list word*)])
    (add-word w #:db pgc #:interactive? interactive? #:offline? offline?)))

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
      (alert (format "Cannot add word '~a', currently in online-only mode" word)))
    #f]
   [(not (connection? pgc))
    (when interactive?
      (alert (format "Cannot add word '~a', not connected to a database." word)))
    #f]
   [(word-exists? word #:db pgc)
    (when interactive?
      (alert (format "Word '~a' is already in the database" word)))
    #f]
   [else
    (define syllables (resolve-syllables word syllables-param #:interactive? interactive? #:offline? offline?))
    (unless syllables (db-error 'add-word "Cannot add word '~a', failed to infer syllables. Try again with an explicit #:syllables argument." word))
    (define rr (resolve-rhyme* word rhyme-param almost-rhyme-param #:interactive? interactive? #:offline? offline?))
    (define rhyme* (rhyme-result-rhyme* rr))
    (define almost-rhyme* (rhyme-result-almost-rhyme* rr))
    ;; -- got everything, time to add word
    (define wid (add-word/unsafe word))
    (add-syllables/id wid syllables #:db pgc)
    (add-rhyme*/id wid rhyme* #:db pgc)
    (add-almost-rhyme*/id wid almost-rhyme* #:db pgc)]))

;; Directly add a word to the database.
;; Please call `add-word` instead!
;; (: add-word/unsafe (->* [String] [#:db PGC] Natural))
(define (add-word/unsafe word #:db [pgc (*connection*)])
  (query-exec pgc "INSERT INTO word (word) VALUES ($1);" word)
  (word->id/fail word #:db pgc #:src 'add-word/unsafe))

;; -----------------------------------------------------------------------------
;; --- Danger zone!

(define-syntax-rule (path->filename p)
  (let-values (([base name mbd?] (split-path p)))
    (let ([fname (path->string name)])
      (substring fname 0 (- (string-length fname) 4)))))

(define (create-ipoe-tables #:db [pgc (*connection*)])
  (assert-connected pgc #:src 'create-tables)
  (for ([p (in-list TABLE*)])
    (alert (format "Creating table '~a' ..." (path->filename p)))
    (query-exec pgc (file->string (path->string (path->complete-path p))))))

(define (remove-word w #:db [pgc (*connection*)])
  (assert-connected pgc #:src 'remove-word)
  (define wid (word->id w))
  (and wid
       (query-exec pgc "DELETE FROM word WHERE id = $1" wid)))

;; -----------------------------------------------------------------------------
;; --- Caching
;;     Save word results, so we minimize the number of web queries

(define *ipoe-cache-dir* (make-parameter "./compiled"))
(define (*ipoe-cache*) (string-append (*ipoe-cache-dir*) "/ipoe.cache"))

;; Keys are strings, cannot use eq?
(define make-cache make-hash)

;; Either make an empty hash, or parse an existing hash from a file
;; TODO add warning if cache is LARGE or VERY-LARGE, assist in DB creation
(define (read-cache)
  (define ipoe-cache (*ipoe-cache*))
  (define cached
    (and (file-exists? ipoe-cache)
         (with-handlers ([exn:fail? (lambda (e)
             (alert (format "Error reading cache file '~a'" ipoe-cache))
             #f)])
           (deserialize (file->value ipoe-cache)))))
  (cond
   [(hash? cached)
    cached]
   [else
    (make-cache)]))

;; Save the dictionary `d` to be loaded later
(define (write-cache d)
  (define cache-dir (*ipoe-cache-dir*))
  (define cache (*ipoe-cache*))
  (cond
   [(hash? d)
    (unless (directory-exists? cache-dir)
      (make-directory cache-dir))
    (with-output-to-file cache #:exists 'replace
      (lambda ()
        (write (serialize d))))]
   [else
    (alert (format "Failed to save malformed cache '~a'" d))]))

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

;; =============================================================================

(module+ test
  (require rackunit racket/sequence ipoe/private/rackunit-abbrevs)

  ;; -------------------------------------------------------------------

  ;; TODO Warn & ignore local config
  (define o*
    (if (file-exists? IPOE-CONFIG)
        (error 'ipoe:db:test "Detected local config file '~a', please delete or change directories before running tests.")
        (options-init)))

  (define-syntax-rule (with-db-test e)
    (parameterize-from-hash o* (lambda ()
      (parameterize ([*verbose* #t])
        (with-ipoe-db #:commit? #f
                      #:interactive? #t
                      #:user (*user*)
                      #:dbname (*dbname*)
          (lambda () e))))))

  (define-syntax-rule (with-online-test e)
    (parameterize-from-hash o* (lambda ()
      (with-ipoe-db #:commit? #f
                    #:online? #t
                    #:interactive? #f
        (lambda () e)))))

  ;; Clear the cache first, then do the rest of the user's expression
  (define-syntax-rule (with-config/cache [global local] e)
    (with-config #:global global #:local local
      (lambda ()
        (define cache (*ipoe-cache*))
        (when (file-exists? cache)
          (delete-file cache))
        e)))

  ;; -- TODO test init prompt for username
  ;; -- TODO test init prompt for dbname
  ;; (with-config/cache ["#:online? #t\n#:interactive? #t" ""]
  ;;   (begin
  ;;     (define-values [in0 out0] (make-pipe))
  ;;     (define-values [in1 out1] (make-pipe))
  ;;     (define c (make-custodian))
  ;;     (parameterize ([current-custodian c])
  ;;       (define prompt-thread
  ;;         (parameterize ([current-input-port in1]
  ;;                        [current-output-port out0])
  ;;           (thread (lambda () (with-ipoe-db #:commit? #f #:user "any"
  ;;             (lambda () (displayln "success")))))))
  ;;       (parameterize ([current-input-port in0]
  ;;                      [current-output-port out1])
  ;;         (check-equal? (read-line) DBNAME-PROMPT)
  ;;         (displayln "anything")
  ;;         (check-equal? (read-line) "success"))
  ;;       (thread-wait prompt-thread))))

  ;; -- with-ipoe-db, invalid user
  (check-exn #rx"^Failed to connect"
    (lambda ()
      (with-ipoe-db #:user "ANONSTEIN" #:dbname "MISSING-TABLE"
        (lambda () (void)))))

  ;; -- with-ipoe-db, online-mode, check that preferences are saved
  (with-config/cache [#f #f]
    (begin
      ;; Log in to the database, make some queries
      ;; Use `with-ipoe-db` to test config/cache
      (with-online-test
        (begin
          (check-true (word-exists? "hello"))
          (check-false (word-exists? "asjdlviuahnzcijvaeafawjsdzidgw")
          (check-true (rhymes-with? "cat" "bat")))))
      ;; Check that cache was created alright
      (check-true (directory-exists? (*ipoe-cache-dir*)))
      (check-true (file-exists? (*ipoe-cache*)))
      (define c (read-cache))
      (check-true (hash? c))
      (check-equal? (hash-count c) 2)
      (check-true (hash-has-key? c "hello"))
      (check-true (hash-has-key? c "cat"))))

  ;; ------------------------------------------------------------------
  ;; -- misc validation

  ;; -- assert-connected
  (let* ([sym 'assert-connected-test]
         [rx  (regexp (symbol->string sym))])
    (check-exn rx
      (lambda () (assert-connected #f #:src sym)))
    (with-online-test
      (check-exn rx
        (lambda () (assert-connected (*connection*) #:src sym))))
    (with-db-test
      (check-equal?
        (assert-connected (*connection*) #:src sym)
        (void))))

  ;; -- assert-rhyme-table
  (check-equal?
    (assert-rhyme-table? 'rhyme #:src 'test)
    (void))

  (check-equal?
    (assert-rhyme-table? 'almost_rhyme #:src 'test)
    (void))

  (let* ([sym 'assert-rt-test]
         [rx  (regexp (symbol->string sym))])
    (check-exn rx
      (lambda () (assert-rhyme-table? 'foo #:src sym)))
    (check-exn rx
      (lambda () (assert-rhyme-table? 'invalid #:src sym))))

  ;; -- ipoe-db-connected?
  (check-false (ipoe-db-connected?))
  (with-online-test
    (check-false (ipoe-db-connected?)))
  (with-db-test
    (check-true (ipoe-db-connected?)))

  ;; -- infer-word-column
  (check-apply* infer-word-column
   ["yolo" == 'word]
   ["a13t" == 'word]
   [""     == 'word]
   ["word" == 'word]
   ;; --
   [1      == 'id]
   [3      == 'id]
   [7      == 'id]
   [40     == 'id]
   [0       == 'id]
   [55      == 'id]
   [8675309 == 'id])

  (check-exn #rx"infer-word-column"
    (lambda () (infer-word-column -1)))

  ;; -- online-mode?
  (check-false* online-mode?
   [(*connection*)]
   [#f]
   [#t]
   ['(1 3 15)])

  (with-db-test
    (check-false (online-mode? (*connection*))))

  (with-online-test
    (check-true (online-mode? (*connection*))))

  ;; -- rhyme-table?
  (check-true* rhyme-table?
    ['rhyme]
    ['almost_rhyme])

  (check-false* rhyme-table?
    ['r]
    ['am]
    ['anything]
    [131])

  ;; ------------------------------------------------------------------

  ;; -- find-word
  (with-db-test
    (let* ([w1 "uhnehviuwvbnsidvbwe"]
           [s1 3]
           [r1 '("vanjfnviw")]
           [a1 '("vniwajnv")]
           ;; --
           [w2 "iuwihpqihrpqhwrqp"]
           [s2 66]
           [r2 (list w1)]
           [a2 (list w1)])
    (define wid1 (add-word/unsafe w1))
    (define wid2 (add-word/unsafe w2))
    ;; --
    (check-apply* find-word
      [w1 == (vector wid1 w1)]
      [wid1 == (vector wid1 w1)]
      [w2 == (vector wid2 w2)]
      [wid2 == (vector wid2 w2)])))

  ;; --- without a DB, should get a "useful" error message
  (let ([rx #rx"ipoe:db:find-word"])
    (check-exn rx
      (lambda () (find-word "yolo")))
    (check-exn rx
      (lambda ()
        (with-online-test
          (find-word "yolo")))))

  ;; -- find-r
  (let ([rx #rx"find-rhyme"])
    (check-exn rx
      (lambda () (find-r #:table 'rhyme)))
    (check-exn rx
      (lambda () (with-online-test (find-r #:table 'rhyme))))
    (check-exn rx
      (lambda () (with-db-test (find-r #:table 'rhyme)))))

  (with-db-test
    (begin
      ;; --- Fake ids
      (let ([w1 0] ;; TODO need invalid ID
            [w2 0])
        (check-false (find-r w1 w2 #:table 'rhyme))
        (check-false (find-r w1 w2 #:table 'almost_rhyme)))
      ;; --- Success
      (let* ([w1 "asdgasdfadgasdgasd"]
             [w2 "askdjghasdfasdgjav"]
             [wid1 (add-word/unsafe w1)]
             [wid2 (add-word/unsafe w2)]
             [r (vector wid1 wid2)])
        (add-rhyme/unsafe wid1 wid2)
        ;; ---- wid+rid
        (check-equal?
          (find-r wid1 wid2 #:table 'rhyme)
          r)
        (check-false
          (find-r wid1 wid2 #:table 'almost_rhyme))
        (check-exn #rx"find-r"
          (lambda () (find-r wid1 wid2 #:table 'foobar)))
        ;; --- wid only
        (let-values ([(v1 v2) (sequence-ref (find-r wid1 #f #:table 'rhyme) 0)])
          (check-equal? v1 wid1)
          (check-equal? v2 wid2))
        ;; --- rid only
        (let-values ([(v1 v2) (sequence-ref (find-r #f wid2 #:table 'rhyme) 0)])
          (check-equal? v1 wid1)
          (check-equal? v2 wid2)))))

  ;; -- almost-rhymes-with?/id
  (with-db-test
    (let* ([w "asdgadshaf"]
           [a "asdgasddas"]
           [wid (add-word/unsafe w)]
           [aid (add-word/unsafe a)])
      (add-almost-rhyme/unsafe wid aid)
      (check-true (almost-rhymes-with?/id wid aid))
      (check-false (almost-rhymes-with?/id aid wid))))

  (let ([rx #rx"find-r"])
    (check-exn rx
      (lambda () (almost-rhymes-with?/id 1 2)))
    (check-exn rx
      (lambda () (with-online-test (almost-rhymes-with?/id 1 2)))))

  ;; -- rhymes-with?/id
  (with-db-test
    (let* ([w "asdgadshaf"]
           [r "asdgasddas"]
           [wid (add-word/unsafe w)]
           [rid (add-word/unsafe r)])
      (add-rhyme/unsafe wid rid)
      (check-true (rhymes-with?/id wid rid))
      (check-false (rhymes-with?/id rid wid))))

  (let ([rx #rx"find-r"])
    (check-exn rx
      (lambda () (rhymes-with?/id 1 2)))
    (check-exn rx
      (lambda () (with-online-test (rhymes-with?/id 1 2)))))

  ;; -- has-syllables? /id
  (with-db-test
    (let* ([w "vwapuidsvcnjw"]
           [wid (add-word/unsafe w)]
           [s 62])
      (add-syllables/unsafe wid s)
      ;; --- has-syllables?
      (check-true (has-syllables? w s))
      (check-false (has-syllables? w (add1 s)))
      ;; --- has-syllables/id
      (check-true (has-syllables?/id wid s))
      (check-false (has-syllables?/id wid (add1 s)))))

  ;; -- has-syllables? /id both fail when disconnected
  (let ([rx #rx"has-syllables"]
        [w  "wejoosdvslvndaqg"]
        [wid 134141] ;; Technically, should match w. Actually doesn't matter
        [s  51])
    (check-exn rx
      (lambda () (has-syllables? w s)))
    (check-exn rx
      (lambda () (has-syllables?/id wid s)))
    (check-exn rx
      (lambda () (with-online-test (has-syllables? w s))))
    (check-exn rx
      (lambda () (with-online-test (has-syllables?/id wid s)))))

  ;; -- id->word
  (with-db-test
    (let* ([w "asdvndzsrwgfvd"]
           [wid (add-word/unsafe w)]
           [wid2 0])
      (check-equal? (id->word wid) w)
      (check-false (id->word wid2))))

  (let ([rx #rx"find-word"])
    (check-exn rx
      (lambda () (id->word 69)))
    (check-exn rx
      (lambda () (with-online-test (id->word 623)))))

  ;; -- syllables->word*
  (with-db-test
    (let* ([w1 "avniwngadsvaet"]
           [w2 "asdgareabfbseg"]
           [w3 "dafwrbafezeafd"]
           [wid1 (add-word/unsafe w1)]
           [wid2 (add-word/unsafe w2)]
           [wid3 (add-word/unsafe w3)]
           [s1 3]
           [s2 1]
           [s3 83])
      (add-syllables/unsafe wid1 s1)
      (for ([wid (in-list (list wid1 wid2 wid3))])
        (add-syllables/unsafe wid s2))
      ;; -- Successful query, 1 result
      (check-equal?
        (sequence->list (syllables->word* s1))
        (list w1))
      ;; -- Success, multiple results
      (check-equal?
        (sequence->list (syllables->word* s2))
        (list w1 w2 w3))
      ;; -- Failed query
      (check-equal?
        (sequence->list (syllables->word* s3))
        '())))

  ;; -- syllables->word fails when disconnected
  (let ([rx #rx"syllables->word*"])
    (check-exn rx
      (lambda () (syllables->word* 8)))
    (check-exn rx
      (lambda () (with-online-test (syllables->word* 7)))))

  ;; -- word->id /fail
  (with-db-test
    (let* ([w "asdgasfwedvsv"]
           [w2 "aongwrfpisnvs"]
           [wid (add-word/unsafe w)])
      (check-equal? (word->id w) wid)
      (check-false (word->id w2))
      ;; -- word->id/fail
      (check-equal? (word->id/fail w #:src 'TEST) wid)
      (check-exn #rx"word->id"
        (lambda () (word->id/fail w2 #:src 'TEST)))))

  (let ([rx #rx"find-word"])
    (check-exn rx
      (lambda () (word->id "foobar")))
    (check-exn rx
      (lambda () (with-online-test (word->id "foobar"))))
    (check-exn rx
      (lambda () (word->id/fail "foobar" #:src 'TEST)))
    (check-exn rx
      (lambda () (with-online-test (word->id/fail "foobar" #:src 'TEST)))))

  ;; ------------------------------------------------------------------
  ;; -- online-or-db queries

  ;; -- (almost-)rhymes-with?
  (with-db-test
    (let* ([w "sdviwnednvse"]
           [wid (add-word/unsafe w)]
           [a1 "asdvniwedav"]
           [a2 "asdgwerfbse"]
           [aid1 (add-word/unsafe a1)]
           [aid2 (add-word/unsafe a2)]
           [r1 "vjrowjnrfsw"]
           [r2 "qqereqqrfsw"]
           [rid1 (add-word/unsafe r1)]
           [rid2 (add-word/unsafe r2)])
     (add-almost-rhyme/unsafe wid aid1)
     (add-almost-rhyme/unsafe wid aid2)
     (add-rhyme/unsafe wid rid1)
     (add-rhyme/unsafe wid rid2)
     ;; --
     (check-true (almost-rhymes-with? w a1))
     (check-true (almost-rhymes-with? w a2))
     ;; --
     (check-true (rhymes-with? w r1))
     (check-true (rhymes-with? w r2))
     ;; --
     (check-false (almost-rhymes-with? w w))
     (check-false (almost-rhymes-with? r1 a1))
     (check-false (rhymes-with? w w))
     (check-false (rhymes-with? r1 a1))))

  ;; -- (almost-)rhymes-with?, fails when offline + nodb
  (let ([w "yes"]
        [rx #rx"ipoe:db:(almost-)?rhymes-with"])
    (check-exn rx
      (lambda () (rhymes-with? w w)))
    (check-exn rx
      (lambda () (almost-rhymes-with? w w))))

  ;; -- (almost-)rhymes-with?, succeeds when online
  (with-online-test
    (begin
      (check-true (rhymes-with? "paper" "draper"))
      (check-true (almost-rhymes-with? "paper" "pager"))))

  ;; -- word-exists?
  (with-db-test
    (let ([w "asdlkgassdfasg"])
      (check-false (word-exists? w))
      (add-word/unsafe w)
      (check-true (word-exists? w))))

  ;; Fails if no DB and offline
  (check-exn (regexp "ipoe:db:word-exists?")
    (lambda () (word-exists? "yes")))

  ;; Succeeds in online mode (for real words)
  (with-online-test
    (check-true (word-exists? "paper")))

  ;; -- word->syllables
  (with-db-test
    (let* ([w "jphiwuhwrgw"]
           [wid (add-word/unsafe w)]
           [s 666])
      (check-false (word->syllables w))
      (add-syllables/unsafe wid s)
      (check-equal? (word->syllables w) s)))

  (check-exn #rx"word->syllables"
    (lambda () (word->syllables "yolo")))

  (with-online-test
    (begin
      (check-equal? (word->syllables "hour") 1)
      (let ([fake "uuuuuuuuuuuu"])
        (check-false (word->syllables fake)))))

  ;; -- word->almost-rhymes* word->rhyme* (secretly tests word->r)
  (with-db-test
    (let* ([w "asdgajsdfweds"]
           [wid (add-word/unsafe w)]
           [a1 "grwfvwrvfswg"]
           [a2 "asdvwfnosjwo"]
           [aid1 (add-word/unsafe a1)]
           [aid2 (add-word/unsafe a2)]
           [r1 "wrfsasrvfswg"]
           [r2 "asdvwtttttwo"]
           [rid1 (add-word/unsafe r1)]
           [rid2 (add-word/unsafe r2)])
      (check-equal?
        (sequence->list (word->almost-rhyme* w))
        '())
      (check-equal?
        (sequence->list (word->rhyme* w))
        '())
      ;; --
      (add-almost-rhyme/unsafe wid aid1)
      (add-almost-rhyme/unsafe wid aid2)
      (add-rhyme/unsafe wid rid1)
      (add-rhyme/unsafe wid rid2)
      ;; --
      (check-equal?
        (sequence->list (word->almost-rhyme* w))
        (list a1 a2))
      (check-equal?
        (sequence->list (word->rhyme* w))
        (list r1 r2))))

  ;; -- word->almost-rhymes* word->rhyme*, failure
  (let ([w "hahah"]
        [rx #rx"rhyme"])
    (check-exn rx
      (lambda () (word->almost-rhyme* w)))
    (check-exn rx
      (lambda () (word->rhyme* w))))

  ;; -- word->almost-rhymes* word->rhyme*, failure
  (with-online-test
    (let ([w "superman"])
      (check-equal?
        (word->rhyme* w)
        '("supermen"))
      (check-equal?
        (word->almost-rhyme* w)
        '("fueron" "uterine"))))

  ;; ------------------------------------------------------------------
  ;; -- add-syllables /id /unsafe + has-syllables
  (with-db-test
    (let* ([w "asivnascase"]
           [wid (add-word/unsafe w)]
           [s1 1]
           [s2 2]
           [s3 3])
      (for ([f (in-list (list add-syllables add-syllables/id add-syllables/unsafe))]
            [s (in-list (list s1 s2 s3))]
            [a (in-list (list w wid wid))])
        (check-false (has-syllables? w s))
        (f a s)
        (check-true (has-syllables? w s)))))

  (let ([rx1 #rx"find-word"]
        [rx2 #rx"add-syllables"]
        [w "rwwrworugw"]
        [wid 0]
        [s 4])
    (check-exn rx1
      (lambda () (add-syllables w s)))
    (check-exn rx1
      (lambda () (with-online-test (add-syllables w s))))
    (check-exn rx2
      (lambda () (add-syllables/id wid s)))
    (check-exn rx2
      (lambda () (with-online-test (add-syllables/id wid s)))))

  ;; -- add-rhyme, etc
  (let ([r-fun* (list add-rhyme* add-rhyme*/id add-rhyme add-rhyme/id add-rhyme/unsafe)]
        [a-fun* (list add-almost-rhyme* add-almost-rhyme*/id add-almost-rhyme add-almost-rhyme/id add-almost-rhyme/unsafe)])
    (with-db-test
     (let* ([w "asdgadsfa"]
            [wid (add-word/unsafe w)]
            [r1 "gwirfsnwds"]
            [r2 "gwirfasdxx"]
            [r3 "gwirxxxxds"]
            [r4 "apwugwrfswds"]
            [r5 "xxxxfsnwds"]
            [rid1 (add-word/unsafe r1)]
            [rid2 (add-word/unsafe r2)]
            [rid3 (add-word/unsafe r3)]
            [rid4 (add-word/unsafe r4)]
            [rid5 (add-word/unsafe r5)]
            [r* (list r1 r2 r3 r4 r5)]
            [arg1* (list w wid w wid wid)]
            [arg2* (list (list r1) (list r2) r3 rid4 rid5)])
       (for ([q (in-list (list rhymes-with? almost-rhymes-with?))]
             [f* (in-list (list r-fun* a-fun*))])
         (for ([f (in-list f*)]
               [r (in-list r*)]
               [arg1 (in-list arg1*)]
               [arg2 (in-list arg2*)])
           (check-false (q w r))
           (f arg1 arg2)
           (check-true (q w r))))))
    ;; -- add duplicate rhyme TODO
    ;; (with-db-test
    ;;  (let* ([w "asdgasdfas"]
    ;;         [wid (add-word/unsafe w)]
    ;;         [r "sdgaheudvs"]
    ;;         [rid (add-word/unsafe r)])
    ;;    (add-rhyme w r)))
    ;; -- add-rhyme / almost fails when online or disconnected
    (let* ([rx #rx"ipoe:db"] ;; Very generic
           [w "say"]
           [r "what"]
           [id 0]
           [arg1* (list w id w id)]
           [arg2* (list (list r r) (list r r) r id)])
      (for ([r-fun (in-list r-fun*)]
            [a-fun (in-list a-fun*)]
            [arg1 (in-list arg1*)]
            [arg2 (in-list arg2*)])
        (check-exn rx (lambda () (r-fun arg1 arg2)))
        (check-exn rx (lambda () (with-online-test (r-fun arg1 arg2))))
        (check-exn rx (lambda () (a-fun arg1 arg2)))
        (check-exn rx (lambda () (with-online-test (a-fun arg1 arg2)))))))

  ;; -- add-word
;  ;; -- add-word/unsafe
;  (let ([new-word "ycvgpadfwd"])
;    ;; Add a word without rhymes
;    (with-db-test
;      (begin
;        (add-word/unsafe new-word 1 '() '())
;        ;; New word should be defined
;        (check-true (word-exists? new-word))
;        (check-equal? (word->syllables new-word) 1))))
;
;  ;; Add a word with known rhymes
;  (let* ([new-word "nasdofiz"]
;         [r*   '("yes")]
;         [a*   '("later")])
;   (with-db-test
;     (begin
;       ;; -- assert rhymes exist
;       (for ([r (in-list r*)])
;         (check-true (word-exists? r)))
;       (for ([a (in-list a*)])
;         (check-true (word-exists? a)))
;       ;; -- add word
;       (add-word/unsafe new-word 11 r* a*)
;       ;; -- assert word added
;       (check-true (word-exists? new-word))
;       (check-equal? (word->syllables new-word) 11)
;       (check-equal? (id->word (word->id new-word)) new-word)
;       ;; -- check rhymes
;       (check-equal? (sequence->list (word->rhyme* new-word)) r*)
;       (check-equal? (sequence->list (word->almost-rhyme* new-word)) a*))))
;
;  ;; Add a word with unknown rhymes
;  (let* ([new-word "hvzx8da"]
;         [r* '("asvhcuivhw")]
;         [a* '()])
;    (with-db-test
;      (begin
;        ;; -- assert rhymes do NOT exist
;        (for ([r (in-list r*)])
;          (check-false (word-exists? r)))
;        (for ([a (in-list a*)])
;          (check-true (word-exists? a)))
;        ;; -- add word
;        (check-print
;          (format "[WARNING ipoe:db:rhyme] : Could not find ID for word '~a'\n" (car r*))
;          (lambda () (add-word/unsafe new-word 5 r* a*)))
;        (check-true (word-exists? new-word))
;        ;; -- should have no rhymes
;        (check-equal? (sequence->list (word->rhyme* new-word)) '()))))
;
;  ;; -- add-word
;  (let ([nonsense1 "asdv8uazxcvasd_1"]
;        [nonsense2 "asdv8uazxcvasd_2"]
;        [nonsense3 "asdv8uazxcvasd_3"])
;    (with-db-test
;      (begin
;        ;; Fails in online mode, cannot tell number of syllables
;        (check-exn exn:fail? (lambda () (add-word nonsense1 #:interactive? #f #:offline? #f)))
;        ;; Succeeds with an explicit number of syllables
;        (with-output-to-file "/dev/null" #:exists 'append
;          (lambda ()
;            (check-true (and (add-word nonsense2 #:syllables 32 #:interactive? #f #:offline? #f) #t))))
;        ;; Succeeds offline because the naive test never fails
;        (check-true (and (add-word nonsense3 #:interactive? #f #:offline? #t) #t)))))
;
;  ;; --- add-word failures
;  (let ([new-word "asdhvuianjsdkvasd"])
;    (check-false (add-word new-word))
;    (check-false
;      (with-online-test (add-word new-word))))
;
;
;  ;; -- add-word* (when offline?, never fails)
;  (with-db-test
;    (let ([new1 "onething"]
;          [new2 "anotherthing"])
;      (with-output-to-file "/dev/null" #:exists 'append
;        (lambda ()
;          (check-true (and (add-word* (list new1 new2) #:interactive? #f #:offline? #t) #t))))
;      (check-true (word-exists? new1))
;      (check-true (word-exists? new2))))
;
;  (let ([w* '("rafflehsnarcuvjawe" "vpoawetz")])
;    (with-output-to-file "/dev/null" #:exists 'append
;      (lambda ()
;        (check-true (and (add-word* w* #:interactive? #f #:offline? #t) #t))))
;    (with-db-test
;      (check-false (word-exists? (car w*)))))
;
;  ;; -- add-rhyme / add-almost-rhyme
;  (with-db-test
;    ;; Doesn't commit for unknown rhymes/almost
;    (let* ([word "word"]
;           [unknown "aiopsuvhz"]
;           [warn-str (format " : Could not find ID for word '~a'\n" unknown)])
;      (check-true (word-exists? word))
;      (check-false (word-exists? unknown))
;      (check-print
;        (string-append "[WARNING ipoe:db:rhyme]" warn-str)
;        (lambda () (add-rhyme word unknown)))
;      (check-false (word-exists? unknown))
;      (check-print
;        (string-append "[WARNING ipoe:db:almost_rhyme]" warn-str)
;        (lambda () (add-almost-rhyme word unknown)))
;      (check-false (word-exists? unknown))))
;
;  (with-db-test
;    ;; Succeeds for known words, doesn't matter if they actually rhyme
;    (let ([w "cat"]
;          [r "dog"])
;      (check-true (word-exists? w))
;      (check-true (word-exists? r))
;      (check-false (rhymes-with? w r))
;      (check-false (almost-rhymes-with? w r))
;      ;; --
;      (add-rhyme w r)
;      (check-true (rhymes-with? w r))
;      (check-false (almost-rhymes-with? w r))
;      ;; --
;      (add-almost-rhyme w r)
;      (check-true (rhymes-with? w r))
;      (check-true (and (almost-rhymes-with? w r) #t))))
;
;   (let ([w "asdfasv"]
;         [v "avhoiuswvp"])
;     ;; Fails when disconnected from DB
;     (check-exn (regexp "ipoe:db:find-word")
;       (lambda () (add-rhyme w v))))
;
;  ;; -- remove-word
;  (with-db-test
;    (let ([new-word "coalman"]
;          [new-syll 2]
;          [new-r* '()]
;          [new-a* '()])
;      (unless (word-exists? new-word)
;        (add-word/unsafe new-word new-syll new-r* new-a*))
;      (check-true (and (remove-word new-word) #t))
;      (check-false (word-exists? new-word))))
;
;  ;; -- remove-word, on a fake word
;  (with-db-test
;    (let ([fake-word "asovijwanrsbfhvs"])
;      (check-false (word-exists? fake-word))
;      (check-false (remove-word fake-word))
;      (check-false (word-exists? fake-word))))
;  ;; -- read-cache
;  (parameterize ([*ipoe-cache-dir* (path->string (find-system-path 'temp-dir))])
;    ;; --- Normal use
;    (let* ([C (make-cache)]
;           [w "yogurt"])
;      (define r (cons (scrape/cache 'word w #:cache C #:scrape scrape-word)
;                      #f))
;      (check-equal? (hash-ref C w (lambda () #f)) r)
;      (write-cache C)
;      (define C+ (read-cache))
;      (check-equal? (hash-ref C+ w (lambda () #f)) r))
;    ;; --- Garbage in the cache file
;    (define-syntax-rule (check-garbage thunk)
;      (let ()
;        (with-output-to-file (*ipoe-cache*) #:exists 'replace
;          thunk)
;        (define C (check-print
;                    (list #rx"^Error reading cache")
;                    read-cache))
;        (check-equal? C (make-cache))))
;    (check-garbage newline)
;    (check-garbage (lambda () (displayln "helloworld")))
;    (check-garbage (lambda () (write 8235)))
;    ;; --- Cache file missing
;    (let ()
;      (delete-file (*ipoe-cache*))
;      (check-equal? (read-cache)
;                    (make-cache))))
;
;  ;; -- write-cache
;  (parameterize ([*ipoe-cache-dir* (path->string (find-system-path 'temp-dir))])
;    (define-syntax-rule (test-bad-write val)
;      (check-equal? (check-print (list #rx"^Failed to save")
;                                 (lambda () (write-cache val)))
;                    (void)))
;    (test-bad-write 'blah)
;    (test-bad-write '())
;    (test-bad-write '(98 2))
;    (test-bad-write "bad"))
;
;  ;; -- scrape-word/cache & scrape-rhyme/cache
;  (with-config/cache [#f #f]
;    (with-online-test
;      (begin
;        ;; --- word
;        (check-true (online-mode? (*connection*)))
;        (check-true (word-exists? "car"))
;        (check-true (word-exists? "car"))
;        ;; Beyond the abstraction barrier...
;        (check-equal? (hash-count (*connection*)) 1)
;        (check-true (word-exists? "rake"))
;        (check-equal? (hash-count (*connection*)) 2)
;        ;; --- rhyme-scheme
;        (check-true (rhymes-with? "car" "far"))
;        (check-true (rhymes-with? "far" "car"))
;        ;; False because we never search for the word
;        (check-false (car (hash-ref (*connection*) "far")))
;        ;; False because we never searched for the rhyme
;        (check-false (cdr (hash-ref (*connection*) "rake")))
;        (check-equal? (hash-count (*connection*)) 3))))

)