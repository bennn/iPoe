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
  ipoe/private/poem
  ipoe/private/poem/rhymecheck
  ipoe/private/poem/spellcheck
  ipoe/private/poem/poetic-license
  ipoe/private/parameters
  ipoe/private/db
  (only-in ipoe/private/ui
    user-error)
  ;; --
  racket/match
  (only-in racket/string string-prefix?)
)

;; =============================================================================

(struct form (
  name ;; Symbol
  rhyme-scheme ;; RhymeScheme
  description ;; String
  constraint* ;; (Listof Syntax)
) #:transparent )
;; (define-type Form form)

;; -----------------------------------------------------------------------------

;; Unless the first argument is #f, raise a helpful exception.
;; (: check-duplicate Any #:new-val Any #:src Symbol #:msg String Void)
(define (check-duplicate old-val #:new-val v #:src src #:msg msg)
  (when old-val
    (user-error src (format "Parsed two ~as: '~a' and '~a'" msg old-val v))))

;; TODO refactor into a command/db style parser. Heavily document options.
;; Read an input file as a poem form
(define (make-form in)
  (define err-loc 'ipoe:parse)
  ;; Loop & manually parse the keywords and line
  ;; (There's so little to parse we might as well do a good job manually)
  (let loop ([name #f]
             [rhyme-scheme #f]
             [syllables #f]
             [description #f]
             [constraint* '()])
    ;; Read one datum & dispatch on it
    (match (read in)
     [(? eof-object?)
      #:when name
      ;; This is a GOOD end-of-file
      ;; TODO what to do if syllables is set, but rs is #f?
      (define rs+s (if (and syllables rhyme-scheme)
                       (replace-wildcard-syllables rhyme-scheme syllables)
                       (or rhyme-scheme '()))) ;; 2015-09-26: Default to free-verse
      (form name rs+s description constraint*)]
     [(? eof-object?)
      ;; A bad end of file. Missing some data.
      (user-error err-loc "Unexpected end-of-file, missing #:name field")]
     ;; -- Keywords
     ['#:name
      ;; Keyword for poem name
      (define n (read-keyword-value in symbol?
                                    #:kw '#:name #:src err-loc))
      (check-duplicate name #:new-val n #:src err-loc #:msg "poem name")
      (loop n rhyme-scheme syllables description constraint*)]
     ['#:rhyme-scheme
      ;; Keyword for rhyme scheme
      (define rs (read-keyword-value in rhyme-scheme?
                                     #:kw '#:rhyme-scheme #:src err-loc))
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs syllables description constraint*)]
     ['#:syllables
      ;; Sets the number of syllables in the rhyme scheme
      (define s (read-keyword-value in exact-positive-integer?
                                    #:kw '#:syllables #:src err-loc))
      (check-duplicate syllables #:new-val s #:src err-loc #:msg "syllable count")
      (loop name rhyme-scheme s description constraint*)]
     [(or '#:descr '#:description)
      ;; Keyword for descriptions
      (define d (read-keyword-value in string? #:kw '#:description #:src err-loc))
      (check-duplicate description #:new-val d #:src err-loc #:msg "description")
      (loop name rhyme-scheme syllables d constraint*)]
     ['#:constraint
      ;; Keyword for extra constraints
      (define c (read-keyword-value in (lambda (x) #t)
                                    #:kw '#:constraint #:src err-loc))
      (define c+ (validator? c))
      (unless c+ (user-error err-loc (format "Expected a constraint expression, but got '~a'" c)))
      (loop name rhyme-scheme syllables description (cons c+ constraint*))]
     ;; -- Check for unknown symbols
     [(? keyword? x)
      (user-error err-loc (format "Unknown keyword '~a'" (keyword->string x)))]
     ;; -- Infer data from predicates
     [(? symbol? n)
      (check-duplicate name #:new-val n #:src err-loc #:msg "poem name")
      (loop n rhyme-scheme syllables description constraint*)]
     [(? rhyme-scheme? rs)
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs syllables description constraint*)]
     [(? exact-positive-integer? s)
      (check-duplicate syllables #:new-val s #:src err-loc #:msg "syllable count")
      (loop name rhyme-scheme s description constraint*)]
     [(? string? d)
      (check-duplicate description #:new-val d #:src err-loc #:msg "description")
      (loop name rhyme-scheme syllables d constraint*)]
     [x
      ;; Try parsing `x` as a validator, otherwise fail because it's undefined data
      (define c (validator? x))
      (cond
       [c
        (loop name rhyme-scheme syllables description (cons c constraint*))]
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
  (define constraint*
    ;; Implicitly thunk all the constraints
    (for/list ([stx (in-list (form-constraint* F))])
      #`(lambda () #,stx)))
  ;; -- Define the poem-checker as syntax
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
         (lambda ()
          (define P (make-poem line*))
          (define L (poetic-license-init (*poetic-license*)))
          ;; -- Check for new words, optionally.
          (when (and (*online?*) (ipoe-db-connected?))
            (define unknown*
              (for/list ([w (poem->word/loc* P)]
                         #:when (not (word-exists? (word/loc-word w))))
                (word/loc-word w)))
            (add-word* unknown*))
          ;; -- Check spelling, optionally (and someday, check grammar)
          (when (*spellcheck?*)
            (define q* (check-spelling (poem->word/loc* P)))
            (poetic-license-apply L q*))
          ;; -- Check rhyme scheme.
          (let ([rs '#,rs])
            (when (not (null? rs))
              (define q* (check-rhyme-scheme P rs))
              (poetic-license-apply L q*)))
          ;; -- Check extra constraints, with current poem bound
          (parameterize ([*poem* P])
            (for/list ([e (in-list (list #,@constraint*))])
              (poetic-license-apply L (e))))
          ;; --
          (poetic-license-report L)
          ;; -- Done! Return anything needed to make testing easy
          (list P L option*)))))))

;; Parse a syntax object as a function in a restricted namespace.
;; If ok, return the original syntax object.
;; TODO make sure these are good error messages
;; TODO raise error if compiles without *poem* set?
;; TODO rename, should be constraint? or something
;; (: validator? (-> Syntax Syntax))
(define (validator? expr)
  ;; -- Just compile, name sure doesn't error
  ;(define evaluated
  ;  (with-constraint-namespace
  ;    (eval expr (current-namespace))))
  expr)

;; TODO namespace should be very restricted
(define-syntax-rule (with-constraint-namespace e)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'ipoe/private/poem)
    e))

(define validator-requires
  #'(require ;; TODO really need all this?
      ipoe/private/poem
      ipoe/private/poem
      ipoe/private/poem/rhymecheck
      ipoe/private/poem/spellcheck
      ipoe/private/poem/poetic-license
      ipoe/private/parameters
      ipoe/private/ui
      (only-in ipoe/private/db
        with-ipoe-db
        add-word*
        ipoe-db-connected?
        word-exists?)
      (only-in syntax/strip-context strip-context)
      ipoe/private/util/string))

;; =============================================================================

(module+ test
  (require
    rackunit
    ipoe/private/util/rackunit-abbrevs
    (only-in racket/port with-input-from-string)
    (only-in racket/string string-split)
  )

;  ;; -- helper function, convert a syntactic function into a lambda
;  (define (eval-extra-validator F)
;    (stx->validator (form-extra-validator F)))
;
;  (define (stx->validator stx)
;    (parameterize ([current-namespace (make-base-namespace)])
;      (namespace-require 'ipoe/sugar)
;      (eval stx (current-namespace))))

  ;; -- check-duplicate
  ;; Always void if first arg is #f
  (check-equal? (check-duplicate #f #:new-val 'a #:src 'b #:msg 'c)
                (void))

  ;; Always an exception if first arg is non-false
  (let* ([src 'cdtest]
         [src-str (symbol->string src)])
    (check-exn (regexp src-str)
               (lambda () (check-duplicate #t #:new-val 'any #:src src #:msg 'any)))
    (check-exn (regexp src-str)
               (lambda () (check-duplicate 'A #:new-val 'any #:src src #:msg 'any)))
    (check-exn (regexp src-str)
               (lambda () (check-duplicate "hi" #:new-val 'any #:src src #:msg 'any))))

  ;; -- input->form
  (define (test-make-form str)
    (with-input-from-string str
      (lambda () (make-form (current-input-port)))))

  (check-apply* test-make-form
   ["#:name couplet #:rhyme-scheme ((A A)) #:syllables 10"
    == (form 'couplet '(((A . 10) (A . 10))) #f '())]
   ["#:name couplet #:rhyme-scheme ((A A))"
    == (form 'couplet '((A A)) #f '())]
   ["#:rhyme-scheme ((A) (B) (C)) #:name yes"
    == (form 'yes '((A) (B) (C)) #f '())]
   ["#:rhyme-scheme ((A) (B) (C)) #:descr \"a short description\" #:name yes"
    == (form 'yes '((A) (B) (C)) "a short description" '())]
   ["#:rhyme-scheme ((A) (B) (C)) #:description \"yo lo\" #:name yes"
    == (form 'yes '((A) (B) (C)) "yo lo" '())]
   ;; It's okay to leave out the keywords
   ["name (((Schema . 42)))"
    == (form 'name '(((Schema . 42))) #f '())]
   ["name  10 (((Schema . 42)))"
    == (form 'name '(((Schema . 42))) #f '())]
   ["name  10 (((Schema . 42) *))"
    == (form 'name '(((Schema . 42) (* . 10))) #f '())]
   ["name  \"aha\" (((Schema . 42) *))"
    == (form 'name '(((Schema . 42) *)) "aha" '())])

   ;; -- make-form, with #:constraint
  (let* ([ps (test-make-form (string-append
                                     "#:name has-extra "
                                     "#:rhyme-scheme ((1 2 3) (A B (C . 3))) "
                                     "#:constraint #t"))]
         [c* (form-constraint* ps)])
    (check-true (form? ps))
    (check-equal? (form-name ps) 'has-extra)
    (check-equal? (form-rhyme-scheme ps) '((1 2 3) (A B (C . 3))))
    (check-true (list? c*))
    (check-equal? (length c*) 1)
    (check-true (with-constraint-namespace (eval (car c*)))))

  (let* ([ps (test-make-form "name (((Schema . 42))) #t")]
         [c* (form-constraint* ps)])
    (check-true (form? ps))
    (check-equal? (form-name ps) 'name)
    (check-true (with-constraint-namespace (eval (car c*)))))

  ;; -- read-keyword-value
  (let* ([src 'rkvtest]
         [src-regexp (regexp (symbol->string src))])
    ;; On EOF, raises an exception
    (check-exn src-regexp
               (lambda () (read-keyword-value eof boolean? #:kw 'yolo #:src src)))
    (define (test-read-keyword-value str p?)
      (with-input-from-string str
        (lambda ()
          (read-keyword-value (current-input-port) p? #:kw 'test-kw #:src src))))

    ;; --- If value matches predicate, pass
    (check-apply* test-read-keyword-value
      ["hello" symbol? == 'hello]
      ["(())" list? == '(())]
      ["1" integer? == 1])

    ;; -- If value doesn't match predicate, fail
    (check-exn src-regexp
               (lambda () (test-read-keyword-value "42" string?)))
    (check-exn src-regexp
               (lambda () (test-read-keyword-value "42" symbol?))))

  ;; -- form->validator
  (define (make-validator spec)
    (parameterize ([current-namespace (make-base-namespace)])
      (eval #`(begin #,validator-requires #,(form->validator spec))
            (current-namespace))))

;  (define-syntax-rule (with-db-test e ...)
;    (parameterize-from-hash o*
;      (lambda ()
;        (parameterize ([*interactive?* #f] [*online?* #f])
;          (with-ipoe-db #:commit? #f
;                        #:user (*user*)
;                        #:dbname (*dbname*)
;            (lambda () e ...))))))

    (let* ([couplet-form (form 'couplet '((A A)) #f '())]
           [w1 "hello"]
           [w2 "hello"]
           [w3 "green"]
           [w4 "red"]
           [pass-str (string-append w1 "\n" w2 "\n")]
           [fail-str (string-append w3 "\n" w4 "\n")])
        (define couplet-validator (make-validator couplet-form))
        (define (test-couplet str)
          (define port (open-input-string str))
          (define res
            (with-config #:local "#:interactive? #f\n#:online? #t\n"
              (lambda () (couplet-validator port))))
          (close-input-port port)
          res)
        (define r
          (check-print
            (list #rx"Finished" #rx"")
            (lambda () (test-couplet pass-str))))
        (check-true (poem? (car r)))
        (check-exn
          (regexp "ipoe")
          (lambda () (test-couplet fail-str))))

  ;; --- Testing options
  (let* ([free-validator (make-validator (form 'free '() #f '()))]
         [fake-options (string-append
                         "#:one option\n"
                         "#:another option\n\n\n"
                         "#:third  thing    \n"
                         "some text\n\nmore text\n"
                         "#:not anoption\n")]
         [real-options (string-append
                         "#:online? #f\n"
                         "#:interactive? #f\n"
                         "#:spellcheck? #f\n"
                         "#:poetic-license 9001\n\n"
                         "Things are good these days.\n")]
         [runon-options (string-append
                          "#:spellcheck? #f\n"
                          "#:yes #t #:no #f\n\n"
                          "yoooolo\n")])
    (define (test-free str)
      (define port (open-input-string str))
      (define res
        (with-config #:local "#:interactive? #f\n#:online? #f\n"
          (lambda () (free-validator port))))
      (close-input-port port)
      res)
    ;; --- Test for unknown / invalid options (they do nothing)
    (let ([fake-res (check-print
                      (list #rx"^Unknown key"
                            #rx"^Unknown key"
                            #rx"^Unknown key"
                            #rx"Finished"
                            #rx"Misspelled"
                            #rx"Remaining")
                      (lambda () (test-free fake-options)))])
        (define P (car fake-res))
        (define H (caddr fake-res))
        (check-true (poem? P))
        (let* ([get (lambda (k) (hash-ref H k (lambda () #f)))])
          (check-apply* get
            ['one == 'option]
            ['another == 'option]
            ['third == 'thing])))
    ;; --- Test for known options (run-time config should change)
    (let* ([real-res (check-print
                       (list #rx"" #rx"")
                       (lambda () (test-free real-options)))]
           [H (caddr real-res)]
           [get (lambda (k) (hash-ref H k (lambda () #f)))])
      (check-true (poem? (car real-res)))
      (check-apply* get
       ['online? == #f]
       ['interactive? == #f]
       ['spellcheck? == #f]
       ['poetic-license == 9001]))
    ;; -- Test options on one line
    (let* ([runon-res (check-print
                        (list #rx"as part of the poem text.$"
                              #rx"Finished"
                              #rx"")
                        (lambda () (test-free runon-options)))]
           [P (car runon-res)]
           [H (caddr runon-res)]
           [get (lambda (k) (hash-ref H k (lambda () #f)))])
      (check-true (poem? P))
      (check-apply* get
       ['spellcheck? == #f]
       ['yes == #f]
       ['no == #f])))

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

