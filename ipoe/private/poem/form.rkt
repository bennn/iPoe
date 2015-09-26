#lang racket/base

;; Struct representing poem forms and a parser to read
;;  an input port into a form.

(provide
  (struct-out form)
  ;; Data structure representing a poem form (aka specification)

  make-form
  ;; (-> Input-Port Form)
  ;; Parse a poem form structure from an input stream

  form->validator
  ;; (-> Form #'(-> Input-Port Poem))
  ;; Return a function to read and validate input given a poem spec

  validator-requires
  ;; Syntax
  ;; Represents a require statement that should be evaluated before running
  ;;  a validator returned by `form->validator`
)

;; -----------------------------------------------------------------------------

(require
  ;;ipoe/private
  ipoe/private/poem
  ipoe/private/poem/check-rhyme-scheme
  ipoe/private/poem/check-spelling
  ipoe/private/poem/poetic-license
  ipoe/private/parameters
  racket/match
  (only-in ipoe/private/db
    add-word*
    with-ipoe-db
    ipoe-db-connected?)
  (only-in ipoe/private/ui
    user-error)
)

;; =============================================================================

(struct form (
  name ;; Symbol
  rhyme-scheme ;; RhymeScheme
  description ;; String
  extra-validator ;; (-> Poem Either)
) #:transparent )
;; (define-type Form form)

;; -----------------------------------------------------------------------------

;; Unless the first argument is #f, raise a helpful exception.
;; (: check-duplicate Any #:new-val Any #:src Symbol #:msg String Void)
(define (check-duplicate old-val #:new-val v #:src src #:msg msg)
  (when old-val
    (user-error src (format "Parsed two ~as: '~a' and '~a'" msg old-val v))))

;; Read an input file as a poem form
(define (make-form in)
  (define err-loc 'ipoe:parse)
  ;; Loop & manually parse the keywords and line
  ;; (There's so little to parse we might as well do a good job manually)
  (let loop ([name #f]
             [rhyme-scheme #f]
             [syllables #f]
             [description #f]
             [extra-validator #f])
    ;; Read one datum & dispatch on it
    (match (read in)
     [(? eof-object?)
      #:when (and name rhyme-scheme)
      ;; This is a GOOD end-of-file
      (define rs+s (if syllables
                       (replace-wildcard-syllables rhyme-scheme syllables)
                       rhyme-scheme))
      (form name rs+s description extra-validator)]
     [(? eof-object?)
      ;; A bad end of file. Missing some data.
      (user-error err-loc (format "Unexpected end-of-file, missing ~a"
        (cond [(and (not name) (not rhyme-scheme))
               "name & rhyme scheme"]
              [(not name)
               "name"]
              [else
              "rhyme-scheme"])))]
     ;; -- Keywords
     ['#:name
      ;; Keyword for poem name
      (define n (read-keyword-value in symbol?
                                    #:kw '#:name #:src err-loc))
      (check-duplicate name #:new-val n #:src err-loc #:msg "poem name")
      (loop n rhyme-scheme syllables description extra-validator)]
     ['#:rhyme-scheme
      ;; Keyword for rhyme scheme
      (define rs (read-keyword-value in rhyme-scheme?
                                     #:kw '#:rhyme-scheme #:src err-loc))
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs syllables description extra-validator)]
     ['#:syllables
      ;; Sets the number of syllables in the rhyme scheme
      (define s (read-keyword-value in exact-positive-integer?
                                    #:kw '#:syllables #:src err-loc))
      (check-duplicate syllables #:new-val s #:src err-loc #:msg "syllable count")
      (loop name rhyme-scheme s description extra-validator)]
     [(or '#:descr '#:description)
      ;; Keyword for descriptions
      (define d (read-keyword-value in string? #:kw '#:description #:src err-loc))
      (check-duplicate description #:new-val d #:src err-loc #:msg "description")
      (loop name rhyme-scheme syllables d extra-validator)]
     ['#:extra-validator
      ;; Keyword for validators, i.e. arbitrary boolean functions on stanzas
      (define ev (read-keyword-value in (lambda (x) #t)
                                     #:kw '#:extra-validator #:src err-loc))
      (define ev+ (validator? ev))
      (unless ev+ (user-error err-loc (format "Expected a function with type (-> (Listof (Listof String)) Boolean), but got '~a'" ev)))
      (check-duplicate extra-validator #:new-val ev+ #:src err-loc #:msg "extra validator")
      (loop name rhyme-scheme syllables description ev+)]
     ;; -- Infer data from predicates
     [(? symbol? n)
      (check-duplicate name #:new-val n #:src err-loc #:msg "poem name")
      (loop n rhyme-scheme syllables description extra-validator)]
     [(? rhyme-scheme? rs)
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs syllables description extra-validator)]
     [(? exact-positive-integer? s)
      (check-duplicate syllables #:new-val s #:src err-loc #:msg "syllable count")
      (loop name rhyme-scheme s description extra-validator)]
     [(? string? d)
      (check-duplicate description #:new-val d #:src err-loc #:msg "description")
      (loop name rhyme-scheme syllables d extra-validator)]
     [x
      ;; Try parsing `x` as a validator, otherwise fail because it's undefined data
      (define ev (validator? x))
      (cond
       [ev
        (check-duplicate extra-validator #:new-val ev #:src err-loc #:msg "extra validator")
        (loop name rhyme-scheme syllables description ev)]
       [else
        (user-error err-loc
                    (format "Unknown value ~a in poem specification" x))])])))

;; (: read-keyword-value (-> Input-Port (-> Any Boolean) #:kw Symbol Any))
(define (read-keyword-value in p? #:kw sym #:src err-loc)
  (when (eof-object? in)
    (user-error err-loc (format "Unmatched keyword '~a', expected to read another value" sym)))
  (define raw (read in))
  (if (p? raw)
      raw
      (user-error err-loc (format "Expected a '~a' to follow the keyword '~a', got ~a" (object-name p?) sym raw))))

;; Compile a poem specification into a validator function,
;;  which takes an input source and checks the input against the poem spec.
;; Result is a syntax object to be spliced into a new module.
;; (: form->validator (-> Form #'(-> Input-Port Poem)))
(define (form->validator F)
  (define name  (form-name F))
  (define rs    (form-rhyme-scheme F))
  (define descr (form-description F))
  (define ev    (form-extra-validator F))
  ;; (: check-extra #'(-> Poem Void))
  (define check-extra
    (or (form-extra-validator F) #'(lambda (x) #t)))
  ;; -- Define the poem-checker as syntax, because
  #`(lambda (in) ;; Input-Port
      ;; Read & process data from the input in-line.
      (define configuring? (box #t))
      (define option* (options-init))
      ;; TODO process the file as a stream, do not build list of lines
      ;; TODO stop abusing that poor #:when clause
      ;;      (it's abused for now because we need to keep the first non-configure line)
      (define line*
        (for/list ([raw-line (in-lines in)]
                   #:when (when (unbox configuring?)
                            ;; Try to read an option
                            (cond
                             [(string-empty? raw-line)
                              ;; Ignore blank lines, keep configuring
                              #f]
                             [(option? raw-line)
                              => (lambda (opt)
                              ;; Got an option, add to param. hash
                              (options-set option* opt)
                              #f)]
                             [else
                              ;; Non-blank, non-option => done configuring!
                              (when (almost-option? raw-line)
                                (alert (format "Treating line '~a' as part of the poem text." raw-line)))
                              (set-box! configuring? #f)
                              #t])))
          raw-line))
      ;; 2015-08-27: If we need punctuation some day, get it from line*
      (parameterize-from-hash option*
       (lambda ()
        (with-ipoe-db #:user (*user*)
                      #:dbname (*dbname*)
                      #:interactive? (*interactive?*)
         (lambda ()
          (define P (make-poem line*))
          (define L (poetic-license-init (*poetic-license*)))
          ;; -- Check for new words, optionally.
          (when (and (*online?*) (ipoe-db-connected?))
            (define unknown*
              (for/list ([w (poem->word/loc* P)]
                         #:when (not (word-exists? (word/loc-word w))))
                (word/loc-word w)))
            (add-word* unknown*
                       #:online? (*online?*)
                       #:interactive? (*interactive?*)))
          ;; -- Check spelling, optionally (and someday, check grammar)
          (when (*spellcheck?*)
            (define q* (check-spelling (poem->word/loc* P)))
            (poetic-license-apply L q*))
          ;; -- Check rhyme scheme. TODO poetic license option/param
          (let ([rs '#,rs])
            (when (not (null? rs))
              (define q* (check-rhyme-scheme P rs))
              (poetic-license-apply L q*)))
          ;; -- Check extra validator
          (let ([extra-q (#,check-extra P)])
            (when (quirk? extra-q)
              (poetic-license-apply L (list extra-q))))
          ;; --
          (when (*interactive?*)
            (poetic-license-report L))
          ;; -- Done! Return anything needed to make testing easy
          (list P L option*)))))))

;; Parse a syntax object as a function in a restricted namespace.
;; If ok, return the original syntax object.
;; (: validator? (-> Syntax (U #'(-> (Listof (Listof String)) Boolean) #f)))
(define (validator? expr)
  (define evaluated
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'ipoe/private/poem) ;;TODO
      (eval expr (current-namespace))))
  (and (procedure? evaluated)
       ;; 2015-08-19: Could create a syntax object protecting `expr` with a
       ;;   (-> (Listof (Listof String)) Boolean) contract
       expr))

(define validator-requires ;;TODO
  #'(require
      ipoe/private/poem
  (only-in ipoe/private/ui
    debug alert)
      ipoe/private/util/string))

;; =============================================================================

(module+ test
  (require
    rackunit
    ipoe/private/util/rackunit-abbrevs
    (only-in racket/port with-input-from-string)
    (only-in racket/string string-split)
  )

  ;; -- TODO redo tests

;  ;; -- helper function, convert a syntactic function into a lambda
;  (define (eval-extra-validator F)
;    (stx->validator (form-extra-validator F)))
;
;  (define (stx->validator stx)
;    (parameterize ([current-namespace (make-base-namespace)])
;      (namespace-require 'ipoe/sugar)
;      (eval stx (current-namespace))))
;
;  ;; -- check-duplicate
;  ;; Always void if first arg is #f
;  (check-equal? (check-duplicate #f #:new-val 'a #:src 'b #:msg 'c)
;                (void))
;
;  ;; Always an exception if first arg is non-false
;  (let* ([src 'cdtest]
;         [src-str (symbol->string src)])
;    (check-exn (regexp src-str)
;               (lambda () (check-duplicate #t #:new-val 'any #:src src #:msg 'any)))
;    (check-exn (regexp src-str)
;               (lambda () (check-duplicate 'A #:new-val 'any #:src src #:msg 'any)))
;    (check-exn (regexp src-str)
;               (lambda () (check-duplicate "hi" #:new-val 'any #:src src #:msg 'any))))
;
;  ;; -- input->form
;  (define (test-input->form str)
;    (with-input-from-string str
;      (lambda () (input->form (current-input-port)))))
;
;  (check-apply* test-input->form
;   ["#:name couplet #:rhyme-scheme ((A A)) #;syllables 10"
;    == (form 'couplet '(((A . 10) (A . 10))) #f #f)]
;   ["#:name couplet #:rhyme-scheme ((A A))"
;    == (form 'couplet '((A A)) #f #f)]
;   ["#:rhyme-scheme ((A) (B) (C)) #:name yes"
;    == (form 'yes '((A) (B) (C)) #f #f)]
;   ["#:rhyme-scheme ((A) (B) (C)) #:descr \"a short description\" #:name yes"
;    == (form 'yes '((A) (B) (C)) "a short description" #f)]
;   ["#:rhyme-scheme ((A) (B) (C)) #:description \"yo lo\" #:name yes"
;    == (form 'yes '((A) (B) (C)) "yo lo" #f)]
;   ;; It's okay to leave out the keywords
;   ["name (((Schema . 42)))"
;    == (form 'name '(((Schema . 42))) #f #f)]
;   ["name  10 (((Schema . 42)))"
;    == (form 'name '(((Schema . 42))) #f #f)]
;   ["name  10 (((Schema . 42) *))"
;    == (form 'name '(((Schema . 42) (* . 10))) #f #f)]
;   ["name  \"aha\" (((Schema . 42) *))"
;    == (form 'name '(((Schema . 42) *)) "aha" #f)])
;
;   ;; -- input->form, with #:extra-validator
;  (let* ([ps (test-input->form (string-append
;                                     "#:name has-extra "
;                                     "#:rhyme-scheme ((1 2 3) (A B (C . 3))) "
;                                     "#:extra-validator (lambda (x) #t)"))]
;         [ev (eval-extra-validator ps)])
;    (check-true (form? ps))
;    (check-equal? (form-name ps) 'has-extra)
;    (check-equal? (form-rhyme-scheme ps) '((1 2 3) (A B (C . 3))))
;    (check-true (ev '()))
;    (check-true (ev '(("hello" "world"))))
;    (check-true (ev '(()))))
;  (let* ([ps (test-input->form "name (((Schema . 42))) (lambda (x) #t)")]
;         [ev (eval-extra-validator ps)])
;    (check-true (form? ps))
;    (check-equal? (form-name ps) 'name)
;    (check-equal? (ev '()) #t))
;
;  ;; -- read-keyword-value
;  (let* ([src 'rkvtest]
;         [src-regexp (regexp (symbol->string src))])
;    ;; On EOF, raises an exception
;    (check-exn src-regexp
;               (lambda () (read-keyword-value eof boolean? #:kw 'yolo #:src src)))
;    (define (test-read-keyword-value str p?)
;      (with-input-from-string str
;        (lambda ()
;          (read-keyword-value (current-input-port) p? #:kw 'test-kw #:src src))))
;
;    ;; --- If value matches predicate, pass
;    (check-apply* test-read-keyword-value
;      ["hello" symbol? == 'hello]
;      ["(())" list? == '(())]
;      ["1" integer? == 1])
;
;    ;; -- If value doesn't match predicate, fail
;    (check-exn src-regexp
;               (lambda () (test-read-keyword-value "42" string?)))
;    (check-exn src-regexp
;               (lambda () (test-read-keyword-value "42" symbol?))))
;
;  ;; -- form->validator
;  (define (make-validator spec)
;    (parameterize ([current-namespace (make-base-namespace)])
;      (eval #`(begin #,validator-requires #,(form->validator spec)) (current-namespace))))
;
;  (let* ([couplet-validator (make-validator (form 'couplet '((A A)) #f #f))]
;         [pass-str "I was born\nhouse was worn\n"]
;         [fail-str "roses are red\nviolets are blue\n"])
;    (define (test-couplet str)
;      (define port (open-input-string str))
;      (define res (parameterize ([*interactive?* #f])
;        (couplet-validator port)))
;      (close-input-port port)
;      res)
;    (check-equal? (car (test-couplet pass-str)) (string-split pass-str "\n"))
;    (check-exn (regexp "ipoe")
;               (lambda () (test-couplet fail-str))))
;
;  ;; --- Testing options
;  (let* ([free-validator (make-validator (form 'free '() #f #f))]
;         [fake-options (string-append
;                         "#:one option\n"
;                         "#:another option\n\n\n"
;                         "#:third  thing    \n"
;                         "some text\n\nmore text\n"
;                         "#:not anoption\n")]
;         [real-options (string-append
;                         "#:online? #f\n"
;                         "#:interactive? #f\n"
;                         "#:spellcheck? #f\n"
;                         "#:poetic-license 9001\n\n"
;                         "Things are good these days.\n")]
;         [runon-options (string-append
;                          "#:spellcheck? #f\n"
;                          "#:yes #t #:no #f\n\n"
;                          "yoooolo\n")])
;    (define (test-free str)
;      (define port (open-input-string str))
;      (define res
;        (parameterize ([*interactive?* #f])
;          (free-validator port)))
;      (close-input-port port)
;      res)
;    ;; --- Test for unknown / invalid options (they do nothing)
;    (let ([fake-res (check-print
;                      (list #rx"^Unknown key"
;                            #rx"^Unknown key"
;                            #rx"^Unknown key"
;                            #rx"^Misspelled word")
;                      (lambda () (test-free fake-options)))])
;        (check-equal? (car fake-res)
;                      '("some text" "" "more text" "#:not anoption"))
;        (let* ([H (cdr fake-res)]
;               [get (lambda (k) (hash-ref H k (lambda () #f)))])
;          (check-apply* get
;            ['one == 'option]
;            ['another == 'option]
;            ['third == 'thing])))
;    ;; --- Test for known options (run-time config should change)
;    (let* ([real-res (test-free real-options)]
;           [H (cdr real-res)]
;           [get (lambda (k) (hash-ref H k (lambda () #f)))])
;      (check-equal? (car real-res) '("Things are good these days."))
;      (check-apply* get
;       ['online? == #f]
;       ['interactive? == #f]
;       ['spellcheck? == #f]
;       ['poetic-license == 9001]))
;    ;; -- Test options on one line
;    (let* ([runon-res (check-print
;                        (list #rx"as part of the poem text.$")
;                        (lambda () (test-free runon-options)))]
;           [str (car runon-res)]
;           [H (cdr runon-res)]
;           [get (lambda (k) (hash-ref H k (lambda () #f)))])
;      (check-equal? str '("#:yes #t #:no #f" "" "yoooolo"))
;      (check-apply* get
;       ['spellcheck? == #f]
;       ['yes == #f]
;       ['no == #f])))
;
;  ;; -- validator?
;  (check-false* validator?
;   ['#f]
;   [#f]
;   [''(1 2 3)]
;   ['(+ 1 1)]
;   ["hello"])
;
;  (check-true* (lambda (v) (and (validator? v) #t))
;   ['(lambda (x) #t)]
;   ['(lambda (x) #f)]
;   ['(lambda (x) (< 5 (length x)))])
;
;  ;; --- test a "good" validator function
;  ;;     2015-08-19: removed contract checks
;  (let* ([v-stx (validator? '(lambda (x) (null? x)))]
;         [v (stx->validator v-stx)])
;    (check-true (v '()))
;    (check-false (v '(())))
;    ; (check-exn exn:fail:contract? (lambda () (v 1)))
;  )
;  ;; 2015-08-27: Commented validator tests until we bring back the contracts
;  ; ;; --- invalid validator: wrong domain
;  ; (check-exn exn:fail:contract?
;  ;            (lambda () (validator? '(lambda (x y) x))))
;  ; ;; --- invalid validator: wrong codomain
;  ; (let ([v (validator? '(lambda (x) x))])
;  ;   (check-exn exn:fail:contract?
;  ;              (lambda () (v '()))))

)
