#lang racket/base

;; User-end library, helpers for specifying #:extra-validator functions
;;  in a poem spec.

(provide
  (rename-out [and/either and])
  ;; The and macro, lifted to accept Either types as well as Booleans

  contains-word?
  ;; (-> Line String Boolean)
  ;; True if the line contains the word

  last-word
  ;; (-> Line Word)
  ;; Return the final word in the line

  last-stanza
  ;; (-> Poem Stanza)
  ;; Return the final stanza in the poem

  line
  ;; (-> Natural Stanza Line)
  ;; Get a line from a stanza.
  ;; (Basically, a synonym for `list-ref`)

  line=?
  ;; (-> Line Line Either)
  ;; Succeeds if the two lines contain the same words.
  ;; i.e., are equal after removing punctuation.

  stanza
  ;; (-> Natural Poem Stanza)
  ;; Get a stanza from a poem.

  stanza->line*
  ;; (-> Stanza (Listof Line))
  ;; Get the lines from a stanza

  word
  ;; (-> Natural Line Word)
  ;; Get a word from a line

  word=?
  ;; (-> Word Word Either)
  ;; Success if two words are equal
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
  ipoe/private/ui
  (for-syntax racket/base syntax/parse)
  (only-in racket/match match-define)
)

;; =============================================================================
;; === API

(struct stanza/loc (line* s-num) #:transparent)
(struct line/loc   (word* l-num s-num) #:transparent)
(struct word/loc   (word  w-num l-num s-num) #:transparent)

(define-syntax (and/either stx)
  (syntax-parse stx
   [(_ v)
    (syntax/loc stx v)]
   [(_ v v* ...)
    (syntax/loc stx
      (if (or (not v) (failure? v))
          v
          (and/either v* ...)))]))

;; (: contains-word? (-> Line (U Word String) Boolean))
(define (contains-word? line w-param)
  (define w1 (if (word/loc? w-param)
                 (word/loc-word w-param) ;; Keep the locations?
                 (parse-word w-param)))
  (cond
   [(not w1)
    (failure 'contains-word? (format "Could not parse word from string '~a'" w1))]
   [(for/or ([w2 (in-list (line/loc-word* line))]) (string=? w1 w2))
    (success 'contains-word? #t)]
   [else
    (failure 'contains-word? (format "~a does not contain word '~a'" (line/loc->string line) w1))]))

;; (: last-word (-> Line Word))
(define (last-word line)
  (match-define (line/loc w* line-num stanza-num) line)
  (when (null? w*)
    (internal-error 'last-word (format "Invariant error: got empty line ~a" line)))
  (let loop ([w* w*] [word-num 0])
    (if (null? (cdr w*))
        (word/loc (car w*) word-num line-num stanza-num)
        (loop (cdr w*) (add1 word-num)))))

;; (: last-stanza (-> Poem Stanza))
(define (last-stanza p)
  (stanza (sub1 (length p)) p))

;; (: line (-> Natural Stanza Line))
(define (line line-num s)
  (match-define (stanza/loc l* stanza-num) s)
  (define l (safe-list-ref line-num l* 'line))
  (line/loc (string->word* l)
            line-num
            stanza-num))

;; (: line=? (-> String String * Boolean))
(define (line=? line . line*)
  (let loop ([w1* (line/loc-word* line)] [w2** (map line/loc-word* line*)])
    (cond
      [(and (null? w1*) (andmap null? w2**))
       ;; Base case: empty lines are equal
       (success 'line=? #t)]
      [(or (and (null? w1*)
                (for/first ([w2* (in-list w2**)] [n (in-naturals)] #:when (not (null? w2*))) n))
           (for/first ([w2* (in-list w2**)] [n (in-naturals)] #:when (null? w2*)) n))
       ;; False because lines have different numbers of words
       => (lambda (bad-index)
       (failure 'line=?
         (format "~a and ~a must have the same number of words."
           (line/loc->string line)
           (line/loc->string (list-ref line* bad-index)))))]
       [(for/first ([w2* (in-list w2**)] [n (in-naturals)]
                   #:when (not (string=? (car w1*) (car w2*)))) n)
       ;; Words don't match
       => (lambda (bad-index)
       (failure 'line=?
         (format
           "~a and ~a must contain the same words."
           (line/loc->string line)
           (line/loc->string (list-ref line* bad-index)))))]
      [else
       (loop (cdr w1*) (map cdr w2**))])))

;; (: stanza (-> Natural Poem Stanza))
(define (stanza stanza-num s*)
  (stanza/loc (safe-list-ref stanza-num s* 'stanza) stanza-num))

;; (: stanza->line* (-> Stanza (Listof Line)))
(define (stanza->line* s)
  (match-define (stanza/loc l* stanza-num) s)
  (for/list ([l (in-list l*)] [line-num (in-naturals)])
    (line/loc (string->word* l) line-num stanza-num)))

;; (: word (-> Natural Line Word))
(define (word word-num ln)
  (match-define (line/loc w* line-num stanza-num) ln)
  (word/loc (safe-list-ref word-num w* 'word) word-num line-num stanza-num))

;; (: word=? (-> Word Word * Either))
(define (word=? w1/loc . w*/loc)
  (define w1 (word/loc-word w1/loc))
  (cond
   [(for/first ([w2/loc (in-list w*/loc)]
                #:when (not (string=? w1 (word/loc-word w2/loc)))) w2/loc)
    => (lambda (w2/loc)
    (failure 'word=? (format "~a and ~a must match" (word->string w1/loc) (word->string w2/loc))))]
   [else
    (success 'word=? #t)]))

;; -----------------------------------------------------------------------------
;; --- private

;; (: line/loc->string (-> Line String))
(define (line/loc->string ln)
  (match-define (line/loc w* line-num stanza-num) ln)
  (format "Line ~a of Stanza ~a" line-num stanza-num))

(define (safe-list-ref goal-index x* [elem-type 'element] #:curr-index [ci 0])
  (cond
   [(null? x*)
    (user-error 'ipoe:safe-list-ref (format "Cannot access ~a ~a, sequence only has ~a ~as" elem-type goal-index ci elem-type))]
   [(= goal-index ci)
    (car x*)]
   [else
    (safe-list-ref goal-index (cdr x*) elem-type #:curr-index (add1 ci))]))

;; (: word->string (-> Word String))
(define (word->string w/loc)
  (match-define (word/loc w wn ln sn) w/loc)
  (format "Word ~a on line ~a of stanza ~a ('~a')" wn ln sn w))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  ;; -- contains-word?
  (let ([ln (line/loc '("i" "could" "not" "stop" "for" "death") 1 2)])
    (check-true* (lambda (w) (success? (contains-word? ln w)))
      ["I"]
      ["stop"]
      ["death"]))

  (check-true* (lambda (ln w) (failure? (contains-word? ln w)))
    [(line/loc '("the" "raging" "milkman") 2 3) "a"]
    [(line/loc '("") 9 0) "hey"]
    [(line/loc '() 8 3) ""]
  )

  ;; -- last-word
  (check-apply* last-word
    [(line/loc '("winnie" "the" "boo") 8 3) == (word/loc "boo" 2 8 3)]
    [(line/loc '("heya") 8 113) == (word/loc "heya" 0 8 113)])

  (check-exn (regexp "last-word")
             (lambda () (last-word (line/loc '() 0 0))))

  ;; -- last-stanza
  (check-apply* last-stanza
    ['(()) == (stanza/loc '() 0)]
    ['(("asdf" "bad" "lkmnue") ("x" "y" "z")) == (stanza/loc '("x" "y" "z") 1)]
  )

  ;; -- line
  (check-equal? (line 0 (stanza/loc '("a") 3)) (line/loc '("a") 0 3))
  (check-equal? (line 1 (stanza/loc '("a" "b" "c") 7)) (line/loc '("b") 1 7))
  (check-equal? (line 5 (stanza/loc '("a" "b" "c" "d" "e" "f") 1)) (line/loc '("f") 5 1))

  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (line 0 (stanza/loc '() 4))))
  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (line -1 (stanza/loc '(a b c) 11))))

  ;; -- line=?
  (check-true* (lambda (l1 l2) (success? (line=? l1 l2)))
    [(line/loc '("") 1 2)
     (line/loc '("") 4 6)]
    [(line/loc '("shall" "i" "compare" "thee") 4 4)
     (line/loc '("shall" "i" "compare" "thee") 4 4)]
    [(line/loc '("yes") 9 999)
     (line/loc '("yes") 124 0)]
    [(line 1 (stanza/loc '("" "It's nOt a .typo") 5))
     (line/loc '("its" "not" "a" "typo") 12 6)]
   )
  ;; --- line=? accepts variable-arity
  (check-true (success? (line=? (line/loc '("a") 0 1)
                                (line/loc '("a") 0 1)
                                (line 0 (stanza 0 '(("A"))))
                                (line/loc '("a") 7 2))))

  (check-true* (lambda (l1 l2) (failure? (line=? l1 l2)))
   [(line/loc '("A") 4 5)
    (line/loc '("B") 82 1)]
   [(line/loc '("just" "a" "minute") 1 1)
    (line/loc '("justaminute") 8 32)]
   [(line 0 (stanza/loc '("12 black birds") 5))
    (line 0 (stanza/loc '("13 black birds") 6))])

  ;; -- stanza
  (check-apply* stanza
   [0 '(a) == (stanza/loc 'a 0)]
   [1 '(a b c) == (stanza/loc 'b 1)]
   [5 '(a b c d e f) == (stanza/loc 'f 5)]
  )

  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (stanza 0 '())))
  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (stanza -1 '(a b c))))

  ;; -- stanza->line*
  (check-apply* stanza->line*
   [(stanza/loc '() 0) == '()]
   [(stanza/loc '("a" "b") 1) == (list (line/loc '("a") 0 1)
                                       (line/loc '("b") 1 1))]
   [(stanza/loc '("yes sir" "you've" "got IT") 3)
    == (list (line/loc '("yes" "sir") 0 3)
             (line/loc '("youve")     1 3)
             (line/loc '("got" "it")  2 3))]
  )

  ;; -- word
  (check-apply* word
   [3 (line/loc '("hel" "lo" "w" "orld") 1 2) == (word/loc "orld" 3 1 2)]
   [0 (line 1 (stanza 2 '(() () ("what is" "going on")))) == (word/loc "going" 0 1 2)]
  )

  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (word 0 (line/loc '() 3 8))))

  ;; -- word=?
  (check-true* (lambda (w1 w2) (success? (word=? w1 w2)))
   [(word/loc "yes" 1 2 3)
    (word/loc "yes" 3 1 5)]
   [(word 0 (line 0 (stanza 0 '(("yes")))))
    (word 0 (line 0 (stanza 0 '(("YES")))))]
   [(word 0 (line 0 (stanza 0 '(("cant")))))
    (word 0 (line 0 (stanza 0 '(("can't")))))]
  )
  (check-true (success? (word=? (word/loc "ab" 3 1 2)
                                (word/loc "ab" 3 3 6)
                                (word/loc "ab" 3 33 6)
                                (word/loc "ab" 3 3 96))))

  (check-true* (lambda (w1 w2) (failure? (word=? w1 w2)))
   [(word/loc "a" 4 6 8)
    (word/loc "b" 8 1 51)]
  )
  (check-true (failure? (word=? (word/loc "a" 1 1 1)
                                (word/loc "b" 1 1 1)
                                (word/loc "c" 1 1 1))))

)
