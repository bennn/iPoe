#lang racket/base

;; Command-line interface to the DB repl

;; TODO it's time to get serious here and break this into helper functions

(provide
  db
  ;; (-> '() Void)
  ;; Start a fresh REPL for the database
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  ipoe/private/db
  ipoe/private/parameters
  ipoe/private/ui
  racket/match
  (only-in racket/string string-join)
  readline ;; For a much-improved REPL experience
  readline/pread
)
;; =============================================================================

(define-syntax-rule (arg-error id expected received)
  (format "~a: expected ~a, got ~a" id expected received))

(define-syntax-rule (query f arg rest)
  (let-values ([(l s) (parse-db-options rest)])
    (take l
      (skip s (f arg)))))

(define-syntax-rule (unknown-word w)
  (format "Unknown word '~a'" w))

(define-syntax-rule (warning msg arg* ...)
  (begin
    (display "Warning: ")
    (displayln (format msg arg* ...))))

;; -----------------------------------------------------------------------------
;; --- Commands

;; Available commands are a SUBSET of the db.rkt interface.
;; You can always get more done by directly requiring that file & programming.

(struct command (
  id ;; Symbol
  exec ;; (-> Any (U 'EXIT #f Any))
  descr ;; String
) #:transparent
  #:property prop:procedure
  (struct-field-index exec))

;; TODO replace symbols with identifiers, in match pattern?
(define COMMAND* (list
  (command
    'exit
    (lambda (v)
      (and
        (or (eof-object? v)
            (exit? v)
            (and (list? v) (exit? (car v))))
        'EXIT))
    "End the REPL session"
  )
  (command
    'help
    (lambda (v)
      (cond
       [(help? v)
        (show-help)]
       [(and (list? v) (help? (car v)))
        (show-help (cdr v))]
       [else
        #f]))
    "Display a generic help message, or give specific information about a command"
  )
  (command
    'id->word
    (lambda (v)
      (match v
       [(list 'id->word (? natural? n))
        (or (id->word n)
            (format "Unbound ID '~a'" n))]
       [(cons 'id->word x)
        (arg-error 'id->word "natural number" x)]
       [_ #f]))
    "Return the word associated with a database ID"
  )
  (command
    'rhymes-with?
    (lambda (v)
      (match v
       [(list 'rhymes-with? (? string? w1) (? string? w2))
        (cond
         [(not (word-exists? w1))
          (unknown-word w1)]
         [(not (word-exists? w2))
          (unknown-word w2)]
         [else ;; Very important that result is a string; #f is not printed.
          (format "~a" (rhymes-with? w1 w2))])]
       [(cons 'rhymes-with? x)
        (arg-error 'rhymes-with? "2 strings" x)]
       [_ #f]))
     "Test if two words rhyme"
  )
  (command
    'syllables->word*
    (lambda (v)
      (match v
       [(cons 'syllables->word* (cons (? natural? n) rest))
        (query syllables->word* n rest)]
       [(cons 'syllables->word* rest)
        (arg-error 'syllables->word* "natural number (+ options)" rest)]
       [_ #f]))
    "Return words with the given number of syllables"
  )
  (command
    'word->almost-rhyme*
    (lambda (v)
      (match v
       [(cons 'word->almost-rhyme* (cons (? string? w) rest))
        (query word->almost-rhyme* w rest)]
       [(cons 'word->almost-rhyme* rest)
        (arg-error 'word->almost-rhyme* "string (+ options)" rest)]
       [_ #f]))
    "Return words that almost rhyme with the argument"
  )
  (command
    'word->id
    (lambda (v)
      (match v
       [(cons 'word->id (? string? w))
        (or (word->id w)
            (unknown-word w))]
       [(cons 'word->id x)
        (arg-error 'word->id "string" x)]
       [_ #f]))
    "Return the ID associated with a word"
  )
  (command
    'word->rhyme*
    (lambda (v)
      (match v
       [(cons 'word->rhyme* (cons (? string? w) rest))
        (query word->rhyme* w rest)]
       [(cons 'word->rhyme* rest)
        (arg-error 'word->rhyme* "string (+ options)" rest)]
       [_ #f]))
    "Return words that rhyme with the argument"
  )
  (command
    'word->syllables
    (lambda (v)
      (match v
       [(list 'word->syllables (? string? w))
        (or (word->syllables w)
            (unknown-word w))]
       [(cons 'word->syllables rest)
        (arg-error 'word->syllables "string" rest)]
       [_ #f]))
    "Return the number of syllables in a word"
  )
  (command
    'word-exists?
    (lambda (v)
      (match v
       [(list 'word-exists? (? string? w))
        (format "~a" (word-exists? w))]
       [(cons 'word-exists? rest)
        (arg-error 'word-exists? "string" rest)]
       [_ #f]))
    "Check if a word exists in the database"
  )
  ;(command
  ;  'define
  ;  (
  ;)
))

(define HELP-STR
  (string-join
    (for/list ([c (in-list COMMAND*)])
      (format "    ~a : ~a" (command-id c) (command-descr c)))
    "\n"
    #:before-first "Available commands:\n"))

;; -----------------------------------------------------------------------------
;; --- Parameters

(define *cmd-dbname* (make-parameter #f))
(define *cmd-user* (make-parameter #f))
(define *commit?* (make-parameter #f))
(define *take* (make-parameter 20))
(define *skip* (make-parameter 0))
(define *output-file* (make-parameter #f))

(define natural? exact-nonnegative-integer?)

;; -----------------------------------------------------------------------------
;; --- REPL

(define (db arg*)
  ;; -- repl
  (command-line
   #:argv arg*
   #:once-each
    [("-c" "--commit") "Commit session to database" (*commit?* #t)]
    [("-d" "--dbname") d-p "Database name" (*cmd-dbname* d-p)]
    [("-o" "--output") o-p "Save interactions to file" (*output-file* o-p)]
    [("-t" "--take") t-p "Default number of query results to show" (*take* t-p)]
    [("-s" "--skip") s-p "Default number of query results to skip" (*skip* s-p)]
    [("-u" "--user") u-p "Username for database" (*cmd-user* u-p)]
   #:args ()
   (begin
     (alert "Initializing DB connection & starting REPL ...")
     (parameterize-from-hash (options-init)
       (lambda ()
         (define u (or (*cmd-user*) (*user*)))
         (define d (or (*cmd-dbname*) (*dbname*)))
         (with-ipoe-db #:commit? (*commit?*)
                       #:user u
                       #:dbname d
                       #:interactive? #t
                       #:online? #f
           (lambda ()
             (printf "Connected to database '~a' as user '~a'.\n" d u)
             (if (*output-file*)
                 (call-with-output-file* (*output-file*) #:exists 'replace
                   init-repl)
                 (init-repl)))))))))

(define PROMPT #"ipoe> ")

(define (init-repl [port #f])
  ;; -- Factor all REPL interactions through `respond`
  (define respond
    (if port
        (lambda (in out)
          (display PROMPT port)
          (displayln in port)
          (displayln out port)
          (newline port)
          (displayln out))
        (lambda (in out)
          (displayln out))))
  ;; -- REPL
  (let loop ()
    (define input
      (parameterize ([readline-prompt #"ipoe> "])
        (read)))
    (match (for/or ([c (in-list COMMAND*)]) (c input))
     ['EXIT
      (displayln "Goodbye")]
     [#f
      (printf "Unrecognized command '~a'\n" input)
      (loop)]
     [r
      (respond input r)
      (loop)])))

;     [(list 'word-exists? (? string? w))
;      (displayln (word-exists? w))
;      (loop)]
;     [x
;      (printf "Unknown command '~a'\n" x)
;      (loop)])))

;; -----------------------------------------------------------------------------

(define (exit? s)
  (memq s '(exit q quit)))

(define (help? s)
  (memq s '(help h ? ?? ??? --help -help wtf)))

(define (skip n seq)
  (for ([x seq] [m (in-range (or n (*skip*)))]) (void))
  seq)

(define (take n seq)
  (string-join
    (for/list ([x seq] [m (in-range (or n (*take*)))])
      (format "~a" x))
    "\n"
    #:after-last (if n "" "... (truncated)")))

(define (parse-db-options x*)
  (let loop ([lim #f]
             [skp #f]
             [x*  x*])
    (match x*
     [(or '() (cons _ '()))
      (values lim skp)]
     [(cons '#:limit (cons (? natural? n) rest))
      (when lim (warning "Option #:limit set twice, ignoring first binding"))
      (loop n skp rest)]
     [(cons '#:skip (cons (? natural? n) rest))
      (when skp (warning "Option #:skip set twice, ignoring first binding"))
      (loop lim n rest)]
     [(cons (or '#:limit '#:skip) (cons x rest))
      (warning "Ignoring invalid option '~a'" x)
      (loop lim skp rest)]
     [(cons _ (cons _ rest))
      (loop lim skp rest)])))

(define (show-help [v #f])
  (cond
   [(not v)
    HELP-STR]
   [else
    (format "'help ~a' not implemented\n" v)]))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  ;; TODO

)
