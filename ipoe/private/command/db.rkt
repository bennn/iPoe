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
;; === Commands

;; Available commands are a SUBSET of the db.rkt interface.
;; You can always get more done by directly requiring that file & programming.

(struct command (
  id ;; Symbol
  exec ;; (-> Any (U 'EXIT #f Any))
  descr ;; String
) #:transparent
  #:property prop:procedure
  (struct-field-index exec))

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
  ;(command
  ;  'id->word
  ;)
  ;(command
  ;  'rhymes-with?
  ;)
  ;(command
  ;  'syllables->word*
  ;)
  ;(command
  ;  'word->almost-rhyme*
  ;)
  ;(command
  ;  'word->id
  ;)
  ;(command
  ;  'word->rhyme*
  ;)
  ;(command
  ;  'word->syllables
  ;)
  ;(command
  ;  'word-exists?
  ;)
  ;(command
  ;  'define
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
             (init-repl (*output-file*)))))))))

(define (init-repl output-file)
  ;; -- Factor all REPL interactions through `respond`
  (define respond
    (if output-file
        (call-with-output-file* output-file #:exists 'replace
          (lambda (port)
            (lambda (tmp . arg*)
              (let ([str (apply format tmp arg*)])
                (displayln str port)
                (displayln str)
                (newline)))))
        (lambda (tmp . arg*)
          (displayln (apply format tmp arg*))
          (newline))))
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
      (respond r)
      (loop)])))

;     [(list 'id->word (? natural? n))
;      (displayln (id->word n))
;      (loop)]
;     [(list 'rhymes-with? (? string? sym1) (? string? sym2))
;      (displayln (rhymes-with? sym1 sym2))
;      (loop)]
;     [(list 'syllables->word* (? natural? n1) '#:limit (? natural? n2))
;      (displayln (take n2 (syllables->word* n1)))
;      (loop)]
;     [(list 'syllables->word* (? natural? n1) '#:limit (? natural? n2) '#:skip (? natural? n3))
;      (define s (syllables->word* n1))
;      (skip n3 s)
;      (displayln (take n2 s))
;      (loop)]
;     [(list 'word->almost-rhyme* (? string? w) '#:limit (? natural? n))
;      (displayln (take n (word->almost-rhyme* w)))
;      (loop)]
;     [(list 'word->almost-rhyme* (? string? w) '#:limit (? natural? n) '#:skip (? natural? n2))
;      (define s (word->almost-rhyme* w))
;      (skip n2 s)
;      (displayln (take n s))
;      (loop)]
;     [(list 'word->id (? string? w))
;      (displayln (word->id w))
;      (loop)]
;     [(list 'word->rhyme* (? string? w) '#:limit (? natural? n1))
;      (displayln (take n1 (word->rhyme* w)))
;      (loop)]
;     [(list 'word->rhyme* (? string? w) '#:limit (? natural? n1) '#:skip (? natural? n2))
;      (define s (word->rhyme* w))
;      (skip n2 s)
;      (displayln (take n1 s))
;      (loop)]
;     [(list 'word->syllables (? string? w))
;      (displayln (word->syllables w))
;      (loop)]
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
  (for ([x seq] [m (in-range n)]) (void)))

(define (take n seq)
  (for/list ([x seq] [m (in-range n)]) x))

(define (show-help [v #f])
  (cond
   [(not v)
    HELP-STR]
   [else
    (format "'help ~a' not implemented\n" v)]))

;; =============================================================================

(module+ test

)
