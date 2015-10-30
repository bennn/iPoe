#lang racket/base

;; Command-line interface to the DB repl

(provide
  dbshell
  ;; (->* [(Vectorof String)] Void)
  ;; Start a fresh REPL for the database
  ;; Argument should be command line arguments
)

;; -----------------------------------------------------------------------------

(define PROMPT #"ipoe:db> ")

(require
  racket/cmdline
  ipoe/private/db
  ipoe/private/parameters
  ipoe/private/suggest
  ipoe/private/ui
  racket/match
  (only-in ipoe/private/util/sequence
    sequence-empty?
    sequence-skip)
  ;; --
  (only-in racket/format ~a)
  (only-in racket/string string-join string-split)
  (only-in racket/sequence
    sequence->list)
  ipoe/private/util/check-os
  ;; --
  (for-syntax racket/base)
)

(if-windows
  (require ipoe/private/util/windows-readline)
  (require readline readline/pread))

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
    'add-word
    (lambda (v)
      (match v
       [(list 'add-word (? string? s))
        ;; TODO options correct?
        (define wid (add-word s))
        (if wid
            (format "Successfully added word '~a' (ID ~a)" s wid)
            (format "Failed to add word '~a'" s))]
       [(cons 'add-word x)
        (arg-error 'add-word "1 string" x)]
       [_ #f]))
     "Add a word to the database"
  )
  (command
    'id->word
    (lambda (v)
      (match v
       [(list 'id->word (? natural? n))
        (or (id->word n)
            (format "Unbound ID '~a'" n))]
       [(cons 'id->word x)
        (arg-error 'id->word "1 natural number" x)]
       [_ #f]))
    "Return the word associated with a database ID"
  )
  (command ;; TODO test
    'remove-word
    (lambda (v)
      (match v
       [(list 'remove-word (? string? w))
        (cond
         [(not (word-exists? w))
          (unknown-word w)]
         [(remove-word w)
          (format "Successfully removed word '~a'" w)]
         [else
          (format "Failed to remove word '~a'" w)])]
       [(cons 'remove-word x)
        (arg-error 'id->word "1 string" x)]
       [_ #f]))
    "Remove a word from the database"
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
        (cond
         [(not (word-exists? w))
          (unknown-word w)]
         [else
          (query word->almost-rhyme* w rest)])]
       [(cons 'word->almost-rhyme* rest)
        (arg-error 'word->almost-rhyme* "string (+ options)" rest)]
       [_ #f]))
    "Return words that almost rhyme with the argument"
  )
  (command
    'word->id
    (lambda (v)
      (match v
       [(list 'word->id (? string? w))
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
        (cond
         [(not (word-exists? w))
          (unknown-word w)]
         [else
          (query word->rhyme* w rest)])]
       [(cons 'word->rhyme* rest)
        (arg-error 'word->rhyme* "string (+ options)" rest)]
       [_ #f]))
    "Return words that rhyme with the argument"
  )
  (command
    'word->syllables*
    (lambda (v)
      (match v
       [(cons 'word->syllables* (cons (? string? w) rest))
        (cond
         [(not (word-exists? w))
          (unknown-word w)]
         [else
          (query word->syllables* w rest)])]
       [(cons 'word->syllables* rest)
        (arg-error 'word->syllables* "string" rest)]
       [_ #f]))
    "Return the syllable counts associated with a word"
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
))

(define COMMAND-ID*
  (for/list ([cmd (in-list COMMAND*)])
    (symbol->string (command-id cmd))))

(define HELP-STR
  (let ([W (apply max (map (compose1 string-length symbol->string command-id) COMMAND*))])
    (string-join
     (for/list ([c (in-list COMMAND*)])
       (format "    ~a : ~a" (~a (command-id c)#:min-width W) (command-descr c)))
     "\n"
     #:before-first "Available commands:\n")))

;; -----------------------------------------------------------------------------
;; --- Parameters

;; TODO ask about committing at the END, if unset

(define *cmd-dbname* (make-parameter #f))
(define *cmd-user* (make-parameter #f))
(define *commit?* (make-parameter #t))
(define *take* (make-parameter 20))
(define *skip* (make-parameter 0))
(define *output-file* (make-parameter #f))

(define natural? exact-nonnegative-integer?)

;; -----------------------------------------------------------------------------
;; --- REPL

(define (dbshell [arg* '#()])
  ;; -- repl
  (command-line
   #:argv arg*
   #:once-each
    [("-c" "--commit") "Commit session to database" (*commit?* #f)]
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
         (parameterize ([*interactive?* #t] [*online?* #f])
          (with-ipoe-db #:commit? (*commit?*)
                        #:user u
                        #:dbname d
            (lambda ()
              (printf "Connected to database '~a' as user '~a'.\n" d u)
              (printf "Enter 'help' for information and 'quit' to quit.\n")
              (if (*output-file*)
                  (call-with-output-file* (*output-file*) #:exists 'replace
                    init-repl)
                  (init-repl))))))))))

(define (init-repl [port #f])
  ;; -- Factor all REPL interactions through `respond`,
  ;;    so they get saved to the logfile
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
      (parameterize ([readline-prompt PROMPT])
        (read)))
    (match (for/or ([c (in-list COMMAND*)]) (c input))
     ['EXIT
      (displayln "Goodbye")]
     [#f
      (printf "Unrecognized command '~a'.~a\n" input (suggest-cmd input))
      (loop)]
     [r
      (respond input r)
      (loop)])))

;; TODO test
(define (suggest-cmd cmd)
  (match cmd
   [(cons (? symbol? s) _)
    (define sim* (filter-similar (symbol->string s) COMMAND-ID*
                                 #:limit 1 #:max-distance 3))
    (if (null? sim*)
      ""
      (format " Maybe you meant '~a'?" (car sim*)))]
   [_
    ""]))

;; -----------------------------------------------------------------------------

(define (exit? s)
  (memq s '(exit q quit)))

(define (help? s)
  (memq s '(help h ? ?? ??? --help -help wtf)))

(define (find-command sym)
  (for/first ([c (in-list COMMAND*)]
              #:when (eq? sym (command-id c)))
    c))

(define skip sequence-skip)

;; TODO should move this into ipoe/sequence
(define (take n-param seq)
  (define n (or n-param (*take*)))
  (string-join
    (for/list ([x seq] [m (in-range n)])
      (format "~a" x))
    "\n"
    #:after-last (if (and (not n) (not (sequence-empty? (skip n seq))))
                     "\n... truncated"
                     "")))

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
     [(cons (or '#:limit '#:skip) (cons (or '#:limit '#:skip) rest))
      (warning "Missing value for option '~a'" (car x*))
      (loop lim skp (cdr x*))]
     [(cons (or '#:limit '#:skip) (cons x rest))
      (warning "Ignoring invalid option '~a'" x)
      (loop lim skp rest)]
     [(cons _ rest)
      (loop lim skp rest)])))

(define (show-help [v #f])
  (match v
   [#f
    HELP-STR]
   [(list (? symbol? s))
    (define c (find-command s))
    (if c
        (command-descr c)
        (format "Unknown command '~a'" s))]
   [x
     (format "Cannot help with '~a'" x)]))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/util/rackunit-abbrevs)

  ;; -- arg-error
  (check-equal?
    (arg-error 'FOO 'BAR 'BAZ)
    "FOO: expected BAR, got BAZ")

  ;; -- query TODO
  ;(parameterize ([*skip* 0] [*take* 0])
  ;  (check-equal?
  ;    (query cdr '(1 2 5) '())
  ;    '(2 5)))

  ;; -- unknown-word
  (check-equal?
    (unknown-word 'FOO)
    "Unknown word 'FOO'")

  ;; -- warning
  (check-print
    "Warning: "
    (lambda ()
      (check-exn
        exn:fail:contract?
        (lambda () (warning 'FOO)))))

  (check-print
    "Warning: FOO\n"
    (lambda () (warning "FOO")))

  (check-print
    "Warning: FOO BAR BAZ\n"
    (lambda () (warning "~a ~a ~a" 'FOO 'BAR 'BAZ)))

  ;; -- COMMAND
  (define-syntax-rule (get-exec sym)
    (command-exec (find-command sym)))

  (define-syntax-rule (add-word/nothing w)
     (add-word w #:rhymes '() #:almost-rhymes '()))

  (define o*
    (let-values ([(gc lc) (get-config-filenames)])
      (if (file-exists? lc)
          (error 'ipoe:db:test "Detected local config file '~a', please delete or change directories before running tests.")
          (options-init))))

  (define-syntax-rule (with-db-test e)
    (parameterize-from-hash o* (lambda ()
      (parameterize ([*verbose* #t]
                     [*interactive?* #f]
                     [*online?* #f])
        (with-ipoe-db #:commit? #f
                      #:user (*user*)
                      #:dbname (*dbname*)
          (lambda () e))))))

  ;; --- exit
  (let ([exec (get-exec 'exit)])
    (check-equal?
     (exec 'exit)
     'EXIT)
    (check-false
     (exec 'foo)))

  ;; --- help
  (let ([exec (get-exec 'help)])
    (check-equal?
     (exec 'help)
     HELP-STR)
    (check-false (exec 'edit)))

  ;; --- id->word
  (with-db-test
   (let* ([id 'id->word]
          [exec (get-exec id)]
          [w "iabpgasgasd"])
     (add-word w)
     (define wid (word->id w))
     (check-equal? (exec (list id wid))  w)
     (remove-word w)
     (check-regexp-match #rx"^Unbound ID" (exec (list id wid)))
     (check-regexp-match #rx"^id->word: expected" (exec (list id "foo")))
     (check-false (exec 'blah))))

  ;; --- remove-word
  (with-db-test
   (let* ([id 'remove-word]
          [exec (get-exec id)]
          [w "gyehqurghba"])
     (add-word w)
     (check-true (word-exists? w))
     (check-regexp-match #rx"^Success"
       (exec (list id w)))
     (check-false (word-exists? w))))

  ;; --- rhymes-with?
  (with-db-test
   (let* ([exec (get-exec 'rhymes-with?)]
          [w "ahasdgasdf"]
          [r "asdgasdga"]
          [cmd (list 'rhymes-with? w r)])
     (check-regexp-match #rx"^Unknown word" (exec cmd))
     (add-word w)
     (check-regexp-match #rx"^Unknown word" (exec cmd))
     (add-word r)
     (check-equal? (exec cmd) "#f")
     (add-rhyme w r)
     (check-equal? (exec cmd) "#t")
     ;; --
     (check-regexp-match #rx"^rhymes-with?" (exec (list 'rhymes-with? "foo")))
     (check-regexp-match #rx"^rhymes-with?" (exec (list 'rhymes-with? 'blah 'blew)))
     (check-false (exec 'else))))

  ;; --- syllables->word (relies on naive syllable algorithm)
  (with-db-test
   (let* ([id 'syllables->word*]
          [exec (get-exec id)]
          [w "eieieieieieieieieeioeieieieieieieieieeioeieieieieieieieieeioeieieieieieieieieeioeieieieieieieieieeio"]
          [s (string-length w)]
          [cmd (list 'syllables->word* s)])
     (check-equal? (exec cmd) "")
     (add-word w)
     (check-equal? (exec cmd) w)
     ;; --
     (check-equal? (exec (list id s '#:limit 5)) w)
     (check-equal? (exec (list id s '#:limit 5 '#:skip 0)) w)
     (check-equal? (exec (list id s '#:limit 0)) "")
     (check-equal? (exec (list id s '#:skip 1)) "")
     (check-equal? (exec (list id s '#:skip 1 '#:limit 4 '#:garbage 'foo)) "")
     ;; --
     (check-regexp-match #rx"^syllables->word\\*: expected" (exec (list id 'foo)))
     (check-false (exec 'blah))))

  ;; --- word->almost-rhyme*
  (with-db-test
   (let* ([id 'word->almost-rhyme*]
          [exec (get-exec id)]
          [w "aspjngasdfvg"]
          [a "avsibgawgvs"]
          [cmd (list id w a)])
     (check-regexp-match #rx"^Unknown word" (exec cmd))
     ;; --
     (add-word w)
     (check-equal? (exec cmd) "")
     (add-word a)
     (add-rhyme w a)
     (check-equal? (exec cmd) "")
     (add-almost-rhyme w a)
     (check-equal? (exec cmd) a)
     ;; --
     (check-equal? (exec (list id w '#:limit 5)) a)
     (check-equal? (exec (list id w '#:limit 5 '#:skip 0)) a)
     (check-equal? (exec (list id w '#:limit 0)) "")
     (check-equal? (exec (list id w '#:skip 1)) "")
     (check-equal? (exec (list id w  '#:garbage 'foo '#:skip 0  '#:garbage 'foo '#:limit 4)) a)
     ;; --
     (check-regexp-match #rx"^word->almost-rhyme\\*: expected" (exec (list id 'asdfasd)))
     (check-false (exec 'basdf))))

  ;; --- word->id
  (with-db-test
   (let* ([id 'word->id]
          [exec (get-exec id)]
          [w "iabpgasgasd"]
          [cmd (list id w)])
     (add-word w)
     (define wid (word->id w))
     (check-equal? (exec cmd) wid)
     (remove-word w)
     (check-regexp-match #rx"^Unknown word" (exec cmd))
     (check-regexp-match #rx"^word->id: expected" (exec (list id 236)))
     (check-false (exec 'blah))))

  ;; --- word->rhyme* (very similar to word->rhyme*)
  (with-db-test
   (let* ([id 'word->rhyme*]
          [exec (get-exec id)]
          [w "aspjngasdfvg"]
          [a "avsibgawgvs"]
          [cmd (list id w a)])
     (check-regexp-match #rx"^Unknown word" (exec cmd))
     ;; --
     (add-word w)
     (check-equal? (exec cmd) "")
     (add-word a)
     (add-almost-rhyme w a)
     (check-equal? (exec cmd) "")
     (add-rhyme w a)
     (check-equal? (exec cmd) a)
     ;; --
     (check-equal? (exec (list id w '#:limit 5)) a)
     (check-equal? (exec (list id w '#:limit 5 '#:skip 0)) a)
     (check-equal? (exec (list id w '#:limit 0)) "")
     (check-equal? (exec (list id w '#:skip 1)) "")
     (check-equal? (exec (list id w  '#:garbage 'foo '#:skip 0  '#:garbage 'foo '#:limit 4)) a)
     ;; --
     (check-regexp-match #rx"^word->rhyme\\*: expected" (exec (list id 'asdfasd)))
     (check-false (exec 'basdf))))

  ;; --- word->syllables*
  (with-db-test
   (let* ([id 'word->syllables*]
          [exec (get-exec id)]
          [w "eieieieieieieieieeioeieieieieieieieieeioeieieieieieieieieeioeieieieieieieieieeioeieieieieieieieieeio"]
          [s (string-length w)]
          [r (number->string s)]
          [cmd (list id w)])
     (check-regexp-match #rx"^Unknown word" (exec cmd))
     (add-word w)
     (check-equal? (exec cmd) r)
     ;; --
     (check-equal? (exec (list id w '#:limit 5)) r)
     (check-equal? (exec (list id w '#:limit 5 '#:skip 0)) r)
     (check-equal? (exec (list id w '#:limit 0)) "")
     (check-equal? (exec (list id w '#:skip 1)) "")
     (check-equal? (exec (list id w '#:skip 1 '#:limit 4 '#:garbage 'foo)) "")
     ;; --
     (check-regexp-match #rx"^word->syllables\\*: expected" (exec (list id 'foo)))
     (check-false (exec 'blah))))

  ;; --- word->exists?
  (with-db-test
   (let* ([id 'word-exists?]
          [exec (get-exec id)]
          [w "ahvgiwdvweqwetwetwe"]
          [cmd (list id w)])
     (check-equal? (exec cmd) "#f")
     (add-word w)
     (check-equal? (exec cmd) "#t")
     ;; --
     (check-regexp-match #rx"^word-exists\\?: expected" (exec (list id 'fasodg)))
     (check-false (exec 'dasdg))))


  ;; -- HELP-STR
  (check-equal?
    (length (string-split HELP-STR "\n"))
    (add1 (length COMMAND*)))

  ;; -- REPL TODO

  ;; -- exit?
  (check-true* (lambda (x) (and (exit? x) #t))
   ['exit]
   ['q])

  (check-false* exit?
   ['a]
   ["asdf"])

  ;; -- help?
  (check-true* (lambda (x) (and (help? x) #t))
   ['help]
   ['h])

  (check-false* help?
   ['x]
   ['(31)]
   ["yolo"])

  ;; -- find-command
  (for ([s (in-list '(exit id->word word->id word->syllables* word->rhyme*))])
    (check-equal?
      (command-id (find-command s))
      s))

  (for ([s (in-list '(a blah foo bar))])
    (check-false (find-command s)))

  ;; -- take
  (check-equal?
    (take 3 '())
    "")

  (check-equal?
    (take 1 '(1 2 3))
    "1")

  (check-equal?
    (take 3 '(1 2 3))
    "1\n2\n3")

  (check-equal?
    (take 5 '(1 2 3))
    "1\n2\n3")

  (check-equal?
    (take #f '(1 2 3))
    "1\n2\n3")

  (parameterize ([*take* 2])
    (check-equal?
      (take #f '(1 2 3))
      "1\n2"))

  ;; -- parse-db-options, lim,skip
  (let*-values ([(l-val s-val) (values 1 2)]
                [(lm sk) (parse-db-options `(#:limit ,l-val #:skip ,s-val))])
    (check-equal? lm l-val)
    (check-equal? sk s-val))

  ;; -- parse-db-options, skip,lim
  (let*-values ([(l-val s-val) (values 1 2)]
                [(lm sk) (parse-db-options `(#:skip ,s-val #:limit ,l-val))])
    (check-equal? lm l-val)
    (check-equal? sk s-val))

  ;; -- parse-db-options, missing lim/skip
  (let*-values ([(s-val) 3]
                [(lm sk) (parse-db-options `(#:skip ,s-val))])
    (check-false lm)
    (check-equal? sk s-val))

  (let*-values ([(l-val) 3]
                [(lm sk) (parse-db-options `(#:limit ,l-val))])
    (check-equal? lm l-val)
    (check-false sk))

  ;; -- parse-db-options, bound twice
  (check-print (list #rx"^Warning: Option #:limit set twice")
    (lambda ()
      (let*-values ([(l-val-0 l-val-1) (values 1 2)]
                    [(lm sk) (parse-db-options `(#:limit ,l-val-0
                                                 #:limit ,l-val-1))])
        (check-equal? lm l-val-1)
        (check-false sk))))

  (check-print (list
                 #rx"^Warning: Option #:skip set twice"
                 #rx"^Warning: Option #:limit set twice")
    (lambda ()
      (let*-values ([(l-val-0 l-val-1) (values 1 2)]
                    [(s-val-0 s-val-1) (values 3 6)]
                    [(lm sk) (parse-db-options `(#:limit ,l-val-0
                                                 #:skip ,s-val-0
                                                 #:skip ,s-val-1
                                                 #:limit ,l-val-1))])
        (check-equal? lm l-val-1)
        (check-equal? sk s-val-1))))

  ;; -- parse-db-options, nonsense options
  (let*-values ([(l-val s-val) (values 1 2)]
                [(lm sk) (parse-db-options `(#:foo 1
                                             #:skip ,s-val
                                             #:limit ,l-val))])
    (check-equal? lm l-val)
    (check-equal? sk s-val))

  (let*-values ([(l-val s-val) (values 1 2)]
                [(lm sk) (parse-db-options `(#:foo 1
                                             #:skip ,s-val
                                             #:bar 8131
                                             #:limit ,l-val))])
    (check-equal? lm l-val)
    (check-equal? sk s-val))

  (let*-values ([(l-val s-val) (values 1 2)]
                [(lm sk) (parse-db-options `(#:foo
                                             #:skip ,s-val
                                             #:bar
                                             #:limit ,l-val
                                             #:baz))])
    (check-equal? lm l-val)
    (check-equal? sk s-val))

  ;; -- parse-db-options, bad type
  (check-print (list #rx"^Warning: Ignoring invalid")
    (lambda ()
      (let*-values ([(l-val s-val) (values 0 "pickles")]
                    [(lm sk) (parse-db-options `(#:skip ,s-val
                                                 #:limit ,l-val))])
        (check-false sk)
        (check-equal? lm l-val))))

  (check-print (list #rx"^Warning: Ignoring invalid")
    (lambda ()
      (let*-values ([(l-val s-val) (values "pickles" 2)]
                    [(lm sk) (parse-db-options `(#:skip ,s-val
                                                 #:limit ,l-val))])
        (check-false lm)
        (check-equal? sk s-val))))

  ;; -- parse-db-options, missing val
  (check-print (list #rx"^Warning: Missing value")
    (lambda ()
      (let*-values ([(l-val) 8]
                    [(lm sk) (parse-db-options `(#:skip #:limit ,l-val))])
        (check-equal? lm l-val)
        (check-false sk))))

  (check-print (list #rx"^Warning: Missing value")
    (lambda ()
      (let*-values ([(s-val) 8]
                    [(lm sk) (parse-db-options `(#:limit #:skip ,s-val))])
        (check-equal? sk s-val)
        (check-false lm))))

  ;; -- parse-db-options, empty list
  (let*-values ([(lm sk) (parse-db-options '())])
    (check-false lm)
    (check-false sk))

  ;; -- show-help
  (check-equal?
    (show-help)
    HELP-STR)

  (check-equal?
    (show-help #f)
    HELP-STR)

  ;; -- show-help, command
  (let* ([s '(exit)])
    (check-equal?
      (show-help s)
      (command-descr (find-command (car s)))))

  ;; -- show-help, invalid command
  (let* ([s 'foo])
    (check-equal?
      (show-help (list s))
      (format "Unknown command '~a'" s)))

  ;; -- show-help, bad type
  (let* ([a "whaaa"])
    (check-equal?
      (show-help a)
      (format "Cannot help with '~a'" a)))
)
