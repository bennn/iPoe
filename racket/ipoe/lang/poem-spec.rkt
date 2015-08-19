#lang racket/base

;; Struct representing poem forms and a parser to read
;;  an input port into a form.

;; 2015-08-19: This file lives in the lang/ directory
;;  because, until today, it was part of the reader file.
;;  If we move it to private/ I think I'd want it separate from private.rkt's
;;  exports.

(provide
  (struct-out poem-spec)
  ;; Data structure representing a poem form (aka specification)

  input->poem-spec
  ;; (-> Input-Port PoemSpec)
  ;; Parse a poem-spec structure from an input stream

  poem-spec->validator
  ;; (-> PoemSpec #'(-> Input-Port (Listof (Listof String))))
  ;; Return a function to read and validate input given a poem spec

  validator-requires
  ;; Syntax
  ;; Represents a require statement that should be evaluated before running
  ;;  a validator returned by `poem-spec->validator`
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
  racket/match
  (only-in racket/sequence sequence->list)
)

;; =============================================================================

(struct poem-spec (
  name ;; Symbol
  rhyme-scheme ;; RhymeScheme
  description ;; String
  extra-validator ;; (-> (Listof (Listof String)) Either)
) #:transparent )
;; (define-type PoemSpec poem-spec)

;; -----------------------------------------------------------------------------

;; Unless the first argument is #f, raise a helpful exception.
;; (: check-duplicate Any #:new-val Any #:src Symbol #:msg String Void)
(define (check-duplicate old-val #:new-val v #:src src #:msg msg)
  (when old-val
    (user-error src (format "Parsed two ~as: '~a' and '~a'" msg old-val v))))

;; Read an input file as a poem specification
;; (: ipoe-input->data (-> Input-Port PoemSpec))
(define (input->poem-spec in)
  (define err-loc 'ipoe:parse)
  ;; Loop & manually parse the keywords and line
  ;; (There's so little to parse we might as well do a good job manually)
  (let loop ([name #f] [rhyme-scheme #f] [syllables #f] [description #f] [extra-validator #f])
    ;; Read one datum & dispatch on it
    (match (read in)
     [(? eof-object?)
      #:when (and name rhyme-scheme)
      ;; This is a GOOD end-of-file
      (define rs+s (if syllables (replace-wildcard-syllables rhyme-scheme syllables) rhyme-scheme))
      (poem-spec name rs+s description extra-validator)]
     [(? eof-object?)
      ;; A bad end of file. Missing some data.
      (user-error err-loc (format "Unexpected end-of-file, missing ~a"
        (cond [(and (not name) (not rhyme-scheme)) "name & rhyme scheme"]
              [(not name) "name"]
              [else "rhyme-scheme"])))]
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
      (cond
       [(validator? x)
        => (lambda (ev)
        (check-duplicate extra-validator #:new-val ev #:src err-loc #:msg "extra validator")
        (loop name rhyme-scheme syllables description ev))]
       [else
        (user-error err-loc (format "Unknown value ~a in poem specification" x))])])))

;; (: read-keyword-value (-> Input-Port (-> Any Boolean) #:kw Symbol Any))
(define (read-keyword-value in p? #:kw sym #:src err-loc)
  (when (eof-object? in)
    (user-error err-loc (format "Unmatched keyword '~a'" sym)))
  (define raw (read in))
  (if (p? raw)
      raw
      (user-error err-loc (format "Expected a '~a' to follow the keyword '~a', got ~a" (object-name p?) sym raw))))

;; Compile a poem specification into a validator function,
;;  which takes an input source and checks the input against the poem spec.
;; Result is a syntax object to be spliced into a new module.
;; (: poem-spec->validator (-> PoemSpec #'(-> Input-Port (Listof (Listof String)))))
(define (poem-spec->validator ps)
  (define name (poem-spec-name ps))
  (define rs (poem-spec-rhyme-scheme ps))
  (define descr (poem-spec-description ps))
  (define ev (poem-spec-extra-validator ps))
  ;; (: check-rhyme #'(-> Poem Void))
  (define check-rhyme
    ;; If rhyme scheme is empty, do not check
    ;; (Special case for "free verse")
    (if (null? rs)
        #'(lambda (stanza*)
            (void))
        #`(lambda (stanza*)
            (assert-success #:src '#,name
              (check-rhyme-scheme stanza* #:rhyme-scheme '#,rs)))))
  ;; (: check-extra #'(-> Poem Void))
  (define check-extra
    (or (poem-spec-extra-validator ps)
        #'(lambda (x) #t)))
  #`(lambda (in) ;; Input-Port
    (define line* (to-line* in))
    (define stanza* (sequence->list (to-stanza* line*)))
    (#,check-rhyme stanza*)
    (unless (#,check-extra stanza*)
      (define d-str (if #,descr (string-append "\n  " #,descr) ""))
      (user-error '#,name (format "Rhyme scheme OK, but failed extra constraint.~a" d-str)))
    (check-spelling line*)
    line*))

;; Parse a syntax object as a function in a restricted namespace.
;; If ok, return the original syntax object.
;; (: validator? (-> Syntax (U #'(-> (Listof (Listof String)) Boolean) #f)))
(define (validator? expr)
  (define evaluated
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'ipoe/sugar)
      (eval expr (current-namespace))))
  (cond
   [(procedure? evaluated)
    ;; 2015-08-19: Could create a syntax object protecting `expr` with a
    ;;   (-> (Listof (Listof String)) Boolean) contract
    expr]
   [else #f]))

(define validator-requires
  #'(require
      ipoe/private
      ipoe/sugar
      (only-in racket/sequence sequence->list)))

;; =============================================================================

(module+ test
  (require
    rackunit
    (only-in racket/string string-split)
  )

  ;; -- helper function, convert a syntactic function into a lambda
  (define (eval-extra-validator ps)
    (stx->validator (poem-spec-extra-validator ps)))

  (define (stx->validator stx)
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'ipoe/sugar)
      (eval stx (current-namespace))))

  ;; -- check-duplicate
  ;; Always void if first arg is #f
  (check-equal? (check-duplicate #f #:new-val 'a #:src 'b #:msg 'c)
                (void))

  ;; Always an exception if first arg is non-false
  (let ([src 'cdtest])
    (check-exn (regexp (symbol->string src))
               (lambda () (check-duplicate #t #:new-val 'any #:src src #:msg 'any)))
    (check-exn (regexp (symbol->string src))
               (lambda () (check-duplicate 'A #:new-val 'any #:src src #:msg 'any)))
    (check-exn (regexp (symbol->string src))
               (lambda () (check-duplicate "hi" #:new-val 'any #:src src #:msg 'any))))

  ;; -- input->poem-spec
  (define (test-input->poem-spec str)
    (define p (open-input-string str))
    (define r (input->poem-spec p))
    (close-input-port p)
    r)
  (check-equal? (test-input->poem-spec "#:name couplet #:rhyme-scheme ((A A)) #;syllables 10")
                (poem-spec 'couplet '(((A . 10) (A . 10))) #f #f))
  (check-equal? (test-input->poem-spec "#:name couplet #:rhyme-scheme ((A A))")
                (poem-spec 'couplet '((A A)) #f #f))
  (check-equal? (test-input->poem-spec "#:rhyme-scheme ((A) (B) (C)) #:name yes")
                (poem-spec 'yes '((A) (B) (C)) #f #f))
  (check-equal? (test-input->poem-spec "#:rhyme-scheme ((A) (B) (C)) #:descr \"a short description\" #:name yes")
                (poem-spec 'yes '((A) (B) (C)) "a short description" #f))
  (check-equal? (test-input->poem-spec "#:rhyme-scheme ((A) (B) (C)) #:description \"yo lo\" #:name yes")
                (poem-spec 'yes '((A) (B) (C)) "yo lo" #f))
  ;; It's okay to leave out the keywords
  (check-equal? (test-input->poem-spec "name (((Schema . 42)))")
                (poem-spec 'name '(((Schema . 42))) #f #f))
  (check-equal? (test-input->poem-spec "name  10 (((Schema . 42)))")
                (poem-spec 'name '(((Schema . 42))) #f #f))
  (check-equal? (test-input->poem-spec "name  10 (((Schema . 42) *))")
                (poem-spec 'name '(((Schema . 42) (* . 10))) #f #f))
  (check-equal? (test-input->poem-spec "name  \"aha\" (((Schema . 42) *))")
                (poem-spec 'name '(((Schema . 42) *)) "aha" #f))
 ;; --- with #:extra-validator
  (let* ([ps (test-input->poem-spec "#:name has-extra #:rhyme-scheme ((1 2 3) (A B (C . 3))) #:extra-validator (lambda (x) #t)")]
         [ev (eval-extra-validator ps)])
    (check-true (poem-spec? ps))
    (check-equal? (poem-spec-name ps) 'has-extra)
    (check-equal? (poem-spec-rhyme-scheme ps) '((1 2 3) (A B (C . 3))))
    (check-true (ev '()))
    (check-true (ev '(("hello" "world"))))
    (check-true (ev '(()))))
  (let* ([ps (test-input->poem-spec "name (((Schema . 42))) (lambda (x) #t)")]
         [ev (eval-extra-validator ps)])
    (check-true (poem-spec? ps))
    (check-equal? (poem-spec-name ps) 'name)
    (check-equal? (ev '()) #t))

  ;; -- read-keyword-value
  (let* ([src 'rkvtest]
         [src-regexp (regexp (symbol->string src))])
    ;; On EOF, raises an exception
    (check-exn src-regexp
               (lambda () (read-keyword-value eof boolean? #:kw 'yolo #:src src)))
    (define (test-read-keyword-value str p?)
      (define port (open-input-string str))
      (define res  (read-keyword-value port p? #:kw 'test-kw #:src src))
      (close-input-port port)
      res)
    ;; If value matches predicate, pass
    (check-equal? (test-read-keyword-value "hello" symbol?)
                  'hello)
    (check-equal? (test-read-keyword-value "1" integer?)
                  1)
    ;; If value doesn't match predicate, fail
    (check-exn src-regexp
               (lambda () (test-read-keyword-value "42" string?)))
    (check-exn src-regexp
               (lambda () (test-read-keyword-value "42" symbol?))))

  ;; -- poem-spec->validator
  (let* ([couplet-validator-stx (poem-spec->validator (poem-spec 'couplet '((A A)) #f #f))]
         [couplet-validator (parameterize ([current-namespace (make-base-namespace)])
           (eval #`(begin #,validator-requires #,couplet-validator-stx) (current-namespace)))]
         [pass-str "I was born\nhouse was worn\n"]
         [fail-str "roses are red\nviolets are blue\n"])
    (define (test-couplet str)
      (define port (open-input-string str))
      (define res (couplet-validator port))
      (close-input-port port)
      res)
    (check-equal? (test-couplet pass-str) (string-split pass-str "\n"))
    (check-exn (regexp "ipoe")
               (lambda () (test-couplet fail-str))))

  ;; -- validator?
  (check-false (validator? '#f))
  (check-false (validator? #f))
  (check-false (validator? ''(1 2 3)))
  (check-false (validator? '(+ 1 1)))
  (check-false (validator? "hello"))

  (check-pred validator? '(lambda (x) #t))
  (check-pred validator? '(lambda (x) #f))
  (check-pred validator? '(lambda (x) (< 5 (length x))))
  ;; --- test a "good" validator function
  ;;     2015-08-19: removed contract checks
  (let* ([v-stx (validator? '(lambda (x) (null? x)))]
         [v (stx->validator v-stx)])
    (check-true (v '()))
    (check-false (v '(())))
    ; (check-exn exn:fail:contract? (lambda () (v 1)))
  )
  ; ;; --- invalid validator: wrong domain
  ; (check-exn exn:fail:contract?
  ;            (lambda () (validator? '(lambda (x y) x))))
  ; ;; --- invalid validator: wrong codomain
  ; (let ([v (validator? '(lambda (x) x))])
  ;   (check-exn exn:fail:contract?
  ;              (lambda () (v '()))))

)
