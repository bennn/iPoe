#lang racket/base

;; Look for #syllables and rhyme-scheme
;; Expand into a module reader that checks the poetry

(provide
  (rename-out
    [ipoe-read read]
    [ipoe-read-syntax read-syntax])
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
  racket/match
  racket/syntax
  syntax/strip-context
  (only-in racket/contract -> define/contract listof)
  (only-in racket/sequence sequence->list)
)

;; =============================================================================

(struct poem-spec (
  name ;; Symbol
  rhyme-scheme ;; RhymeScheme
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
  (let loop ([name #f] [rhyme-scheme #f] [syllables #f] [extra-validator #f])
    ;; Read one datum & dispatch on it
    (match (read in)
     [(? eof-object?)
      #:when (and name rhyme-scheme)
      ;; This is a GOOD end-of-file
      (define rs+s (if syllables (replace-wildcard-syllables rhyme-scheme syllables) rhyme-scheme))
      (poem-spec name rs+s extra-validator)]
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
      (loop n rhyme-scheme syllables extra-validator)]
     ['#:rhyme-scheme
      ;; Keyword for rhyme scheme
      (define rs (read-keyword-value in rhyme-scheme?
                                     #:kw '#:rhyme-scheme #:src err-loc))
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs syllables extra-validator)]
     ['#:syllables
      ;; Sets the number of syllables in the rhyme scheme
      (define s (read-keyword-value in exact-positive-integer?
                                    #:kw '#:syllables #:src err-loc))
      (check-duplicate syllables #:new-val s #:src err-loc #:msg "syllable count")
      (loop name rhyme-scheme s extra-validator)]
     ['#:extra-validator
      ;; Keyword for validators, i.e. arbitrary boolean functions on stanzas
      (define ev (read-keyword-value in (lambda (x) #t)
                                     #:kw '#:extra-validator #:src err-loc))
      (define ev+ (validator? ev))
      (unless ev+ (user-error err-loc (format "Expected a function with type (-> (Listof (Listof String)) Boolean), but got '~a'" ev)))
      (check-duplicate extra-validator #:new-val ev+ #:src err-loc #:msg "extra validator")
      (loop name rhyme-scheme syllables ev+)]
     ;; -- Infer data from predicates
     [(? symbol? n)
      (check-duplicate name #:new-val n #:src err-loc #:msg "poem name")
      (loop n rhyme-scheme syllables extra-validator)]
     [(? rhyme-scheme? rs)
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs syllables extra-validator)]
     [(? exact-positive-integer? s)
      (check-duplicate syllables #:new-val s #:src err-loc #:msg "syllable count")
      (loop name rhyme-scheme s extra-validator)]
     [x
      ;; Try parsing `x` as a validator, otherwise fail because it's undefined data
      (cond
       [(validator? x)
        => (lambda (ev)
        (check-duplicate extra-validator #:new-val ev #:src err-loc #:msg "extra validator")
        (loop name rhyme-scheme syllables ev))]
       [else
        (user-error err-loc (format "Unknown value ~a in poem specification" x))])])))

(define (ipoe-read in)
  (syntax->datum (ipoe-read-syntax #f in)))

(define (ipoe-read-syntax src-path in)
  (define ps (input->poem-spec in))
  (with-syntax ([name     (format-id #f "~a" (poem-spec-name ps))]
                [validate (poem-spec->validator ps)])
    (strip-context
      #'(module name racket/base
          (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]))
          (require syntax/strip-context) ;; 2015-08-15: Really necessary?
          (define (custom-read in) (syntax->datum (custom-read-syntax #f in)))
          (define (custom-read-syntax src-path in)
            (with-syntax ([str (validate in)])
              (strip-context #'(module anything racket (provide data) (define data 'str)))))))))

;; (: read-keyword-value (-> Input-Port (-> Any Boolean) #:kw Symbol Any))
(define (read-keyword-value in p? #:kw sym #:src err-loc)
  (when (eof-object? in)
    (user-error err-loc (format "Unmatched keyword '~a'" sym)))
  (define raw (read in))
  (if (p? raw)
      raw
      (user-error err-loc (format "Expected a '~a' to follow the keyword '~a', got ~a" (object-name p?) sym raw))))

(define (poem-spec->validator ps)
  ;; If rhyme scheme is empty, do not check
  ;; (Special case for "free verse")
  (define name (poem-spec-name ps))
  (define rs (poem-spec-rhyme-scheme ps))
  (define ev (poem-spec-extra-validator ps))
  (define check-rhyme
    (if (null? rs)
        (lambda (stanza*) (void))
        (lambda (stanza*) (assert-success #:src name
                          (check-rhyme-scheme stanza* #:rhyme-scheme rs)))))
  (define check-extra
    (or (poem-spec-extra-validator ps)
        (lambda (x) #t)))
  (lambda (in) ;; Input-Port
    (define line* (to-line* in))
    (define stanza* (sequence->list (to-stanza* line*)))
    (check-rhyme stanza*)
    (unless (check-extra stanza*) (user-error name (format "Rhyme scheme OK, but failed extra ~a constraint" name)))
    (check-spelling line*)
    line*))

;; Parse an expression as a contracted function
;; (: validator? (-> Any (U (-> (Listof (Listof String)) Boolean) #f)))
(define (validator? expr)
  (define raw-f
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'ipoe/sugar)
      (eval expr (current-namespace))))
  (cond
   [(procedure? raw-f)
    (define/contract ev (-> (listof (listof string?)) boolean?) raw-f)
    ev]
   [else #f]))

;; =============================================================================

(module+ test
  (require
    rackunit
    (only-in racket/string string-split)
  )

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
                (poem-spec 'couplet '(((A . 10) (A . 10))) #f))
  (check-equal? (test-input->poem-spec "#:name couplet #:rhyme-scheme ((A A))")
                (poem-spec 'couplet '((A A)) #f))
  (check-equal? (test-input->poem-spec "#:rhyme-scheme ((A) (B) (C)) #:name yes")
                (poem-spec 'yes '((A) (B) (C)) #f))
  ;; It's okay to leave out the keywords
  (check-equal? (test-input->poem-spec "name (((Schema . 42)))")
                (poem-spec 'name '(((Schema . 42))) #f))
  (check-equal? (test-input->poem-spec "name  10 (((Schema . 42)))")
                (poem-spec 'name '(((Schema . 42))) #f))
  (check-equal? (test-input->poem-spec "name  10 (((Schema . 42) *))")
                (poem-spec 'name '(((Schema . 42) (* . 10))) #f))
  ;; --- with #:extra-validator
  (let ([ps (test-input->poem-spec "#:name has-extra #:rhyme-scheme ((1 2 3) (A B (C . 3))) #:extra-validator (lambda (x) #t)")])
    (check-true (poem-spec? ps))
    (check-equal? (poem-spec-name ps) 'has-extra)
    (check-equal? (poem-spec-rhyme-scheme ps) '((1 2 3) (A B (C . 3))))
    (check-true ((poem-spec-extra-validator ps) '()))
    (check-true ((poem-spec-extra-validator ps) '(("hello" "world"))))
    (check-true ((poem-spec-extra-validator ps) '(()))))
  (let ([ps (test-input->poem-spec "name (((Schema . 42))) (lambda (x) #t)")])
    (check-true (poem-spec? ps))
    (check-equal? (poem-spec-name ps) 'name)
    (check-equal? ((poem-spec-extra-validator ps) '()) #t))

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
  (let ([couplet-validator (poem-spec->validator (poem-spec 'couplet '((A A)) #f))]
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
  (let ([v (validator? '(lambda (x) (null? x)))])
    (check-true (v '()))
    (check-false (v '(())))
    (check-exn exn:fail:contract? (lambda () (v 1))))
  ;; --- invalid validator: wrong domain
  (check-exn exn:fail:contract?
             (lambda () (validator? '(lambda (x y) x))))
  ;; --- invalid validator: wrong codomain
  (let ([v (validator? '(lambda (x) x))])
    (check-exn exn:fail:contract?
               (lambda () (v '()))))

)
