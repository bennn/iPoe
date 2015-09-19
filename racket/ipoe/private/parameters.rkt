#lang racket/base

;; Run-time parameters and their parsing and setting

(provide
  ;; NOTE: every 'define-parameter' is provided

  ;; -- Option parsing / binding

  IPOE-CONFIG
  ;; Path-String
  ;; Expected name of the local / global ipoe configuration file

  almost-option?
  ;; (-> String Boolean)
  ;; True if the line of text looks kind of like a configuration option.
  ;; Used to raise a warning message.

  get-config-filenames
  ;; (-> (Values Path-String Path-String))
  ;; Return the names of the global and local config files, respectively
  ;; For testing only!

  options-get
  ;; (-> OptionTbl Symbol Any)
  ;; Return the value associated with the symbol key in the table.

  options-init
  ;; (-> OptionTbl)
  ;; Initialize a table of run-time configuration options

  options-set
  ;; (-> OptionTbl Symbol Any Boolean)
  ;; Add the the (Symbol Any) pair to the OptionTbl

  option?
  ;; (->* [String] [#:parse-value (-> String Any)] (U #f Option))
  ;; If the string has the form "#:KEY VALUE" for any symbol `KEY`
  ;;  and Racket value `VALUE`, create and return an option structure.
  ;; @optional{parse-value} controls how Racket values are parsed
  ;;  from the string `VALUE`.

  parameterize-from-hash
  ;; (-> OptionTbl (-> Any) Any)
  ;; Update all parameters defined in this file with data from the OptionTbl,
  ;;  then execute the thunk in this updated context.

  save-option
  ;; (->* [Symbol Any] [#:location (U 'local 'global)] Boolean)
  ;; Write the `#:Symbol Any` pair to a configuration file.
  ;; By default, saves to the global config.

  with-config
  ;; (-> (-> Any) #:config String Any)
  ;; For testing,
  ;; Execute the thunk where the #:config argument is the only
  ;;  configuration file, no matter the filesystems global or local config.

)

(require
  ipoe/private/string
  ipoe/private/ui
  (only-in racket/set mutable-set set-member? set-add!)
  (for-syntax racket/base syntax/parse racket/syntax)
  (only-in racket/file file->string)
)

;; =============================================================================
;; Parameters

(define ALL-PARAMETERS (mutable-set))

(define-syntax (define-parameter stx)
  (syntax-parse stx
   [(_ name-stx:id val)
    (with-syntax ([param-id (format-id stx "*~a*" (syntax-e #'name-stx))])
      #`(begin
          (define param-id (make-parameter val))
          (define name-stx (quote #,(syntax-e #'name-stx)))
          (set-add! ALL-PARAMETERS name-stx)
          (provide param-id)))]))

;; -- general parameters
(define-parameter user #f)
(define-parameter dbname #f)
(define-parameter interactive? #t)
(define-parameter online? #t)
(define-parameter spellcheck? #t)
(define-parameter grammarcheck? #t) ;;bg; How should this work?
(define-parameter suggest-rhyme? #t) ;; unused
(define-parameter suggest-spelling? #t) ;; unused

;; -- poetic license / demerits
(define-parameter poetic-license 0)
(define-parameter almost-rhyme-penalty 0)
(define-parameter bad-rhyme-penalty 0)
(define-parameter bad-word-penalty 0)
(define-parameter bad-extra-penalty 0)
(define-parameter bad-syllable-penalty 0)
(define-parameter bad-stanza-penalty 0)
(define-parameter bad-lines-penalty 0)

;; -----------------------------------------------------------------------------

;; For internal use only.
;; Represents a configuration option parsed from a string
(struct option-match (
  key ;; String
  val ;; String
) #:transparent)

;; Name of the config file
(define IPOE-CONFIG ".ipoe")

(define almost-option-regexp (regexp "#:"))

;; (: almost-option? (-> String Boolean))
(define (almost-option? line)
  (regexp-match? almost-option-regexp line))

;; Generate the filenames for local & global configs
;; (: get-config-filenames (-> (Values String String)))
(define (get-config-filenames)
  (define global-config
    (string-append (path->string (find-system-path 'home-dir)) "/" IPOE-CONFIG))
  (define local-config
    (string-append "./" IPOE-CONFIG))
  (values global-config local-config))

;; Add output from `option?` to a table created by `options-init`.
;; (: options-set (-> Option* Option Void))
(define (options-set o* o)
  (and o (option-match? o)
       (hash-set! o* (option-match-key o) (option-match-val o))
       #t))

;; Parse options from each line of a file
;; Unless every line is a well-formed option, raise an exception
;; (: options-set-from-file (-> OptionTbl Path-String Void))
(define (options-set-from-file o* fname)
  (with-input-from-file fname
    (lambda ()
      (for ([ln (in-lines)] [i (in-naturals)]
            #:when (not (string-empty? ln)))
        (define o (option? ln))
        (unless o
          (raise-user-error 'ipoe:config "Error reading configuration file '~a', syntax error on line ~a\n    '~a'" fname i ln))
        (options-set o* o)))))

;; Count the number of bindings in a table of options
(define options-count hash-count)

(define (options-get o* k)
  (hash-ref o* k (lambda ()
                   (raise-user-error 'option-get (format "Unbound option ~e" k)))))

;; For testing
(define (options-new)
  (make-hasheq))

(define (options-init)
  (define o* (options-new))
  (define-values [global-config local-config] (get-config-filenames))
  (when (file-exists? global-config)
    (options-set-from-file o* global-config))
  (when (file-exists? local-config)
    (options-set-from-file o* local-config))
  o*)

;; Seach for "#:KEY VAL" on a line (for arbitrary text "KEY" and "VAL")
;; Ignore any extra whitespace before/after "#:KEY" or "VAL"
;; And there are no spaces allowed in keys or values
(define option-regexp #px"^[\\s]*#:([\\S]+)[\\s]+([\\S]+)[\\s]*$")

;; (: option? (->* [String] [#:parse-value (-> String Any)] Option))
(define (option? str #:parse-value [read-val read-from-string])
  ;; Maybe a bad idea to read here, but I don't think the parameters
  ;; will ever be too complicated
  (define m (regexp-match option-regexp str))
  (and m
      (option-match (string->symbol (cadr m))
                    (read-val (caddr m)))))

;; Reset all parameters, using new values from hash.
;; Warn if there are any unknown options in the hash.
(define (parameterize-from-hash o* thunk)
  ;; -- check for unknown keys
  (for ([k (in-hash-keys o*)])
    (unless (set-member? ALL-PARAMETERS k)
      (alert (format "Unknown key '~a'" k))))
  ;; -- update all parameters, use macro-defined identifiers to avoid typos
  (parameterize (
    [*user*  (hash-ref o* user *user*)]
    [*dbname*  (hash-ref o* dbname *dbname*)]
    [*interactive?* (hash-ref o* interactive? *interactive?*)]
    [*online?*      (hash-ref o* online? *online?*)]
    [*spellcheck?*  (hash-ref o* spellcheck? *spellcheck?*)]
    [*grammarcheck?* (hash-ref o* grammarcheck? *grammarcheck?*)]
    [*suggest-rhyme?* (hash-ref o* suggest-rhyme? *suggest-rhyme?*)]
    [*suggest-spelling?* (hash-ref o* suggest-spelling? *suggest-spelling?*)]
    [*poetic-license* (hash-ref o* poetic-license *poetic-license*)]
    [*almost-rhyme-penalty* (hash-ref o* almost-rhyme-penalty *almost-rhyme-penalty*)]
    [*bad-rhyme-penalty* (hash-ref o* bad-rhyme-penalty *bad-rhyme-penalty*)]
    [*bad-word-penalty* (hash-ref o* bad-word-penalty *bad-word-penalty*)]
    [*bad-extra-penalty* (hash-ref o* bad-extra-penalty *bad-extra-penalty*)]
    [*bad-syllable-penalty* (hash-ref o* bad-syllable-penalty *bad-syllable-penalty*)]
    [*bad-stanza-penalty* (hash-ref o* bad-stanza-penalty *bad-stanza-penalty*)]
    [*bad-lines-penalty* (hash-ref o* bad-lines-penalty *bad-lines-penalty*)])
    (thunk)))

(define (save-option k v #:location [loc 'global])
  (define-values [global-config local-config] (get-config-filenames))
  (define fname
    (case loc
     [(global) global-config]
     [(local)  local-config]
     [else     (error 'parameters:save-option
                      (format "Unknown config location '~e'" loc))]))
  (with-output-to-file fname #:exists 'append
    (lambda () (printf "#:~a ~s\n" k v))))

;; Execute the thunk with the given string as the only config file.
(define (with-config thunk #:local [local #f] #:global [global #f])
  ;; -- Change to a temp directory, to avoid the local config
  (parameterize ([current-directory (find-system-path 'temp-dir)])
    ;; -- Save existing global config, be prepared to restore it
    (define config-fname*
      (let-values ([(gc lc) (get-config-filenames)])
        (list gc lc)))
    (define old-config-data*
      (for/list ([fname (in-list config-fname*)])
        (and (file-exists? fname)
             (file->string fname))))
    (define (restore-config)
      (for/list ([d (in-list old-config-data*)]
                 [fname (in-list config-fname*)]
                 #:when d)
        (with-output-to-file fname #:exists 'replace
          (lambda () (display d)))))
    ;; -- Write the new config
    (for ([fname (in-list config-fname*)]
          [new-data (in-list (list global local))])
      (when new-data
        (with-output-to-file fname #:exists 'replace
          (lambda () (displayln new-data)))))
    ;; -- Call the user's thunk, watch for exceptions
    (call-with-exception-handler
      (lambda (exn)
        (begin (restore-config) exn))
      (lambda ()
        (let ([r (thunk)])
          (begin (restore-config) r))))))

;; =============================================================================

(module+ test
  (require
    rackunit
    ipoe/private/rackunit-abbrevs
    (only-in racket/string string-trim)
    (only-in racket/list last)
    (only-in racket/file file->lines))

  ;; -- almost-option
  (check-true* almost-option?
   ["#:key val"]
   ["key #: val"]
   ["#:key #:val"]
   ["   #:key    val:#   "]
   ["#:#:"]
   ["keyval#:"])

  (check-false* almost-option?
   ["key val"]
   ["# : key val"]
   [""]
   ["key val:#"])

  ;; -- get-config-filenames
  (let-values ([(f1 f2) (get-config-filenames)])
    (check-true (path-string? f1))
    (check-true (path-string? f2)))

  ;; -- options-init
  (let ([o* (options-init)])
    (define-values [g l] (get-config-filenames))
    (cond
     [(hash-empty? o*)
      (check-false (file-exists? g))
      (check-false (file-exists? l))
      ;; -- create a dummy .ipoe file, to make sure init works
      (parameterize ([current-directory (find-system-path 'temp-dir)])
        (unless (file-exists? IPOE-CONFIG)
          (with-output-to-file IPOE-CONFIG
            (lambda () (displayln "#:test output"))))
        (define ln* (file->lines IPOE-CONFIG))
        (define o*+ (options-init))
        (check-equal? (options-count o*+) (length ln*)))]
     [else
      ;; -- Number of default options should be at least the length of
      ;;    each config file. (It's not the sum because duplicates don't
      ;;    add to the count.)
      (define (L fname)
        (if (file-exists? fname) (length (file->lines fname)) 0))
      (check-true (<= (L g) (options-count o*)))
      (check-true (<= (L l) (options-count o*)))]))

  ;; -- options-set (options-count, options-get)
  (let* ([opt (options-init)]
         [o1 (option? "")]
         [o2 (option? "#:a b")]
         ;; TODO o3 tests depend on what's in the user's config files.
         ;;      if the option's already there, the number of values in the
         ;;      hash won't increase
         [o3 (option? "#:grammarcheck? #f")])
    (define N (options-count opt))
    (check-false (options-set opt o1))
    (check-equal? (options-count opt) N)
    (check-true (options-set opt o2))
    (check-equal? (options-count opt) (+ 1 N))
    (check-equal? (options-get opt 'a) 'b)
    (check-true (options-set opt o3))
    (check-equal? (options-count opt) (+ 2 N))
    (check-equal? (options-get opt 'grammarcheck?) #f))

  ;; -- options-set-from-file
  (define (gen-tmpfile fname)
    (define s (symbol->string (gensym)))
    (define full-path
      (string-append (path->string (find-system-path 'temp-dir))
                     "/"
                     (or fname s)))
    (if (file-exists? full-path)
        (gen-tmpfile (string-append fname s))
        full-path))

  (define-syntax-rule (refresh-file f str ...)
    (with-output-to-file f #:exists 'replace
      (lambda () (displayln str) ...)))

  (let ([fname (gen-tmpfile "ipoe.parameters.test")])
    (define-syntax-rule (test-from-file [str ...] count [kv ...])
      (let* ([o* (options-new)]
             [get (lambda (k) (hash-ref o* k (lambda () #f)))])
        (refresh-file fname str ...)
        (options-set-from-file o* fname)
        (check-equal? (options-count o*) count)
        (check-apply* get
          kv ...)))
    ;; --- blank config
    (test-from-file [""] 0 [['any == #f] ['keys == #f]])
    ;; -- normal config
    (test-from-file
      ["#:a a"
       "#:b b"]
      2
      [['a == 'a]
       ['b == 'b]
       ['c == #f]])
    ;; --- extra newlines
    (test-from-file
      ["#:first-option \"and-value\""
       ""
       "#:second 001/2"
       "#:third #t"]
      3
      [['first-option == "and-value"]
       ['second == 1/2]
       ['third ==  #t]])
    ;; --- error
    (check-exn (regexp "ipoe:config")
      (lambda () (test-from-file ["gibberish"] 0 [])))
    ;; --- whitespace in value
    (check-exn (regexp "Error reading configuration")
      (lambda () (test-from-file ["#:opt \"some value\""] 0 []))))

  ;; -- option?
  (check-false* option?
   ["key #: val"]
   ["#:#:"]
   ["nope"]
   [""]
   ["    \t   "]
   ["viet cong"]
   ["#w x"]
   ["a = b"]
   ["keyval#:"])
  (check-apply* option?
   ["#:key val" == (option-match 'key 'val)]
   ["   #:mr smith" == (option-match 'mr 'smith)]
   [" #:yes 411" == (option-match 'yes 411)]
   ["#:key #:val" == (option-match 'key '#:val)]
   ["   #:key    val:#   " == (option-match 'key 'val:#)])

  ;; -- option?, with custom parser
  (check-equal? (option? "#:xxx yyy" #:parse-value (lambda (x) x))
                (option-match 'xxx "yyy"))
  (check-equal? (option? "#:cat tac" #:parse-value (lambda (x) 42))
                (option-match 'cat 42))

  ;; -- parameterize-from-hash
  (let* ([opt (options-init)]
         [init-count (options-count opt)]
         [o1  (option? "#:online? #f")]
         [o2  (option? "  #:bad-lines-penalty -666")]
         [o3  (option? "nothin")]
         [o4  (option? "#:not real")])
    (for ([o (in-list (list o1 o2 o3 o4))])
      (options-set opt o))
    ;; -- pre-test
    (check-true (< init-count (options-count opt)))
    (check-true (*online?*))
    (check-true (<= 0 (*bad-lines-penalty*)))
    (check-print
      "Unknown key 'not'\n"
      (lambda () (parameterize-from-hash opt (lambda ()
        ;; -- mid-test
        (check-false (*online?*))
        (check-true (negative? (*bad-lines-penalty*)))))))
    ;; -- post-test
    (check-true (*online?*))
    (check-true (<= 0 (*bad-lines-penalty*))))

  ;; -- save-option
  (with-config #:local ""
    (lambda ()
      (define-values [gc lc] (get-config-filenames))
      ;; -- Write k/v pair to config file
      (define key 'hello)
      (define val 'world)
      (save-option key val #:location 'local)
      ;; -- Check new contents against the old
      (define local-data (string-trim (file->string lc)))
      (check-equal? local-data "#:hello world")
      (define global-lines (file->lines gc))
      (check-equal? (options-count (options-init))
                    (add1 (length global-lines)))))

  (with-config #:global ""
    (lambda ()
      (define-values [gc lc] (get-config-filenames))
      (define key 'hello)
      (define val 'world)
      (save-option key val #:location 'global)
      (check-equal? (string-trim (file->string gc))
                    (format "#:~a ~a" key val))))

)
