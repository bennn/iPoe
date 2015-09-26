#lang racket/base

;; Note: all vectors in this file are immutable

;; TODO enforce immutable vectors (overwrite for/vector?)
;; TODO standardize #t or #f for success casex
;;  (plase just remove the #f and use and/no-quirks)
;; TODO refactor into multiple files
;;  (separate validators interface from core structs from library code?)
(require racket/contract/base)
(provide
  ;; TODO rename to remove /loc?
  stanza/loc->string
  line/loc->string
  word/loc->string

  ;; TODO stop with the struct-out
  (struct-out stanza/loc)
  (struct-out line/loc)
  (struct-out word/loc)
  (struct-out poem)

  and/no-quirks for/no-quirks
  ;; The `and` macro, lifted to fail if it gets a quirk? struct
  ;; (and succeed on #f)

  (contract-out
  [contains-word? (-> line/loc? string? boolean?)]
  ;; (-> Line/Loc String Boolean)
  ;; True if the line contains the word

  [last-word (-> line/loc? word/loc?)]
  ;; (-> Line Word)
  ;; Return the final word in the line

  [last-stanza (-> poem? stanza/loc?)]
  ;; (-> Poem Stanza)
  ;; Return the final stanza in the poem

  [line (-> natural-number/c stanza/loc? line/loc?)]
  ;; (-> Natural Stanza Line)
  ;; Get a line from a stanza.
  ;; (Basically, a synonym for `list-ref`)

  [line=? (-> line/loc? line/loc? boolean?)]
  ;; (-> Line Line Either)
  ;; Succeeds if the two lines contain the same words.
  ;; i.e., are equal after removing punctuation.

  [line->word* (-> line/loc? sequence?)]
  ;; (-> Line/Loc (Sequenceof Word/Loc))
  ;; Return a sequence of words in the line

  [make-poem (-> (listof string?) poem?)]
  ;; (-> String Poem)
  ;; Parse a poem from an input source
  ;; TODO add more sources (input-port, path-string)

  [poem-count-stanza* (-> poem? natural-number/c)]
  ;; (-> Poem Natural)
  ;; Count the number of stanzas in the poem

  [poem->stanza* (-> poem? sequence?)]
  ;; (-> Poem (Sequenceof Stanza/Loc))
  ;; Return all stanzas in a poem

  [poem->word/loc* (-> poem? sequence?)]
  ;; (-> Poem (Sequenceof Word))
  ;; Return a sequence of all words in the poem

  [stanza (-> natural-number/c poem? stanza/loc?)]
  ;; (-> Natural Poem Stanza)
  ;; Get a stanza from a poem.

  [stanza-count-lines (-> stanza/loc? natural-number/c)]
  ;; (-> Stanza/Loc Natural)
  ;; Count the number of lines in a stanza

  [stanza->line* (-> stanza/loc? sequence?)]
  ;; (-> Stanza (Sequenceof Line))
  ;; Get the lines from a stanza

  [word (-> natural-number/c line/loc? word/loc?)]
  ;; (-> Natural Line Word)
  ;; Get a word from a line

  [word=? (-> word/loc? word/loc? boolean?)]
  ;; (-> Word Word Either)
  ;; Success if two words are equal
))

;; -----------------------------------------------------------------------------

(require
  ipoe/private/nlp/parse
  ipoe/private/poem/poetic-license
  ipoe/private/parameters
  ipoe/private/ui
  ipoe/private/util/string
  ;; --
  racket/generator
  racket/match
  (for-syntax racket/base syntax/parse)
)

;; =============================================================================

(struct stanza/loc (
  line* ;; (Vectorof (Vectorof Word))
  s-num ;; Natural
) #:transparent)

(struct line/loc (
  word* ;; (Vectorof Word)
  l-num ;; Natural
  s-num ;; Natural
) #:transparent)

(struct word/loc (
  word ;; Word
  w-num ;; Natural
  l-num ;; Natural
  s-num ;; Natural
) #:transparent)

(struct poem (
  src
  ;; (Listof (Listof String))
  ;; The raw source from which we built the poem.
  ;; Not sure we'll ever need this.
  ;; Note that the String contains multiple words

  stanza*
  ;; (Vectorof (Vectorof (Vectorof Word)))
  ;; Normalized stanzas. This is where the magic happens.
) #:transparent )

;; -----------------------------------------------------------------------------

(define-syntax (and/no-quirks stx)
 (syntax-parse stx
  [(_ v)
   (syntax/loc stx v)]
  [(_ v v* ...)
   (syntax/loc stx
    (if (quirk? v)
     v
     (and/no-quirks v* ...)))]))

(define-syntax (for/no-quirks stx)
  (syntax-parse stx
   [(_ iterator body)
    (syntax/loc stx
      (for/fold ([tmp #f]) iterator
        (and/no-quirks tmp body)))]))

;; TODO move to helpers
(define (parse-word str)
  (define w* (string->word* str))
  (unless (= 1 (length w*))
    (user-error 'parse-word
                (format "Expected to get 1 word from string, got '~a'" w*)))
  (car w*))

;; TODO allow multi-word matchings. (So that w-param can be an integer, say)
;; (: contains-word? (-> Line/Loc Word+ Boolean))
(define (contains-word? line w-param)
  (define w1 (if (word/loc? w-param)
                 (word/loc-word w-param) ;; Keep the locations?
                 (parse-word w-param)))
  (cond
   [(not w1)
    ;; Should never happen, unless validator is flawed
    (user-error 'contains-word?
                (format "Could not parse word from string '~a'" w-param))]
   [(for/or ([w2 (in-vector (line/loc-word* line))]) (string=? w1 w2))
    #t]
   [else
    (quirk (*bad-extra-penalty*)
           (format "~a does not contain word ~a" (line/loc->string line) w1))]))

;; (: last-line (-> Stanza/Loc Line/Loc))
(define (last-line stanza)
  (match-define (stanza/loc l* s-num) stanza)
  (define l-num (sub1 (vector-length l*)))
  (line/loc (vector-ref l* l-num) l-num s-num))

;; (: last-stanza (-> Poem Stanza/Loc))
(define (last-stanza P)
  (define s* (poem-stanza* P))
  (define s-num (sub1 (vector-length s*)))
  (stanza/loc (vector-ref s* s-num) s-num))

;; (: last-word (-> Line/Loc Word/Loc))
(define (last-word line)
  (match-define (line/loc w* l-num s-num) line)
  (define w-num (sub1 (vector-length w*)))
  (define w (vector-ref w* w-num))
  (word/loc w w-num l-num s-num))

;; (: line (-> Natural Stanza/Loc Line/Loc))
(define (line l-num s)
  (match-define (stanza/loc l* s-num) s)
  (define l (safe-vector-ref l-num l* 'line))
  (line/loc l l-num s-num))

;; TODO give word positions on error
;; (: line=? (-> Line/Loc Line/Loc * Boolean))
(define (line=? l1/loc . line*)
  (define w1* (line/loc-word* l1/loc))
  (define r
    (for/first ([l2/loc (in-list line*)]
                #:when (not (for/and ([w1 (in-vector w1*)]
                                      [w2 (in-vector (line/loc-word* l2/loc))])
                              (string=? w1 w2))))
      (quirk (*bad-extra-penalty*)
             (format "~a and ~a must have the same words"
               (line/loc->string l1/loc)
               (line/loc->string l2/loc)))))
  (or (eq? #f r)
      r))

(define-syntax-rule (maybe-yield-stanza st)
  (when (not (null? st))
    (yield (list->vector (reverse st)))))

(define (line*->stanza* line*)
  (in-generator (let loop ([line* line*] [curr-stanza '()])
    (cond
     [(null? line*)
      ;; End of input. Yield current stanza if anything's left.
      (maybe-yield-stanza curr-stanza)]
     [(string-empty? (car line*))
      ;; Hit an empty line. Yield the current stanza, if any.
      (maybe-yield-stanza curr-stanza)
      (loop (cdr line*) '())]
     [else
      ;; Advance `line*` and add to `curr-stanza`
      (loop (cdr line*)
            (cons (list->vector (string->word* (car line*)))
                  curr-stanza))]))))

;; (: line->word* TODO)
(define (line->word* ln)
  (match-define (line/loc w* l-num s-num) ln)
  (for/vector ([w (in-vector w*)]
               [w-num (in-naturals)])
    (word/loc w w-num l-num s-num)))

(define (make-poem line*)
  (define stanza*
    (for/vector ([s (line*->stanza* line*)])
      s))
  (poem line* stanza*))

(define (poem-count-stanza* P)
  (vector-length (poem-stanza* P)))

(define (poem->stanza* P)
  (define s* (poem-stanza* P))
  (for/vector ([i (in-range (vector-length s*))])
    (stanza/loc (vector-ref s* i) i)))

;; TODO streaming algorithm
(define (poem->word/loc* P)
  (for*/list ([st (poem->stanza* P)]
              [ln (stanza->line* st)]
              [w  (line->word* ln)])
    w))

;  (define stanza* (poem-stanza* P))
;  (define S (vector-length stanza*))
;  (in-generator (let loop ([s-num 0])
;    (cond
;     [(= s-num S)
;      (void)]
;     [else
;      (define line* (stanza/loc-line* (vector-ref stanza* s-num)))
;      (define L (vector-length line*))
;      (in-generator (let loop ([l-num 0])
;        (cond
;         [(= l-num L)
;          (void)]
;         [else
;          (define word* (line/loc-line
;          (for ([w-num (in-
;  (for*/list ([s-num (in-range (vector-length stanza*))]
;              [l-num (in-range (in-vector s))]
;              [w-num (in-range (in-vector l))])
;    w))

;; (: stanza (-> Natural Poem Stanza))
(define (stanza s-num s*)
  (stanza/loc (safe-vector-ref s-num s* 'stanza) s-num))

;; TODO test
(define (stanza-count-lines st)
  (match-define (stanza/loc l* s-num) st)
  (vector-length l*))

;; (: stanza->line* (-> Stanza/Loc (Vectorof Line/Loc)))
(define (stanza->line* s)
  (match-define (stanza/loc l* s-num) s)
  (for/vector ([l (in-vector l*)]
               [l-num (in-naturals)])
    (line/loc l l-num s-num)))

;; (: word (-> Natural Line Word))
(define (word w-num ln)
  (match-define (line/loc w* l-num s-num) ln)
  (word/loc (safe-vector-ref w-num w* 'word) w-num l-num s-num))

;; (: word=? (-> Word Word * Either))
(define (word=? w1/loc . w*/loc)
  (define w1 (word/loc-word w1/loc))
  (define r
    (for/first ([w2/loc (in-list w*/loc)]
                #:when (not (string=? w1 (word/loc-word w2/loc))))
      (quirk (*bad-extra-penalty*)
             (format "~a and ~a must match"
               (word/loc->string w1/loc)
               (word/loc->string w2/loc)))))
  (or (eq? #f r)
      r))

;; -----------------------------------------------------------------------------
;; --- private

;; (: stanza/loc->string (-> Stanza String))
(define (stanza/loc->string st)
  (match-define (stanza/loc l* s-num) st)
  (format "Stanza ~a" s-num))

;; (: line/loc->string (-> Line String))
(define (line/loc->string ln)
  (match-define (line/loc w* line-num stanza-num) ln)
  (format "Line ~a of Stanza ~a" line-num stanza-num))

(define (safe-vector-ref i x* [elem-type 'element])
  (define N (vector-length x*))
  (cond
   [(or (< i 0) (<= N i))
    (user-error 'ipoe:safe-vector-ref
                (format "Cannot access ~a ~a, sequence only has ~a ~as" elem-type i N elem-type))]
   [else
    (vector-ref x* i)]))

;; (: word/loc->string (-> Word String))
(define (word/loc->string w/loc)
  (match-define (word/loc w wn ln sn) w/loc)
  (format "Word ~a on line ~a of stanza ~a ('~a')" wn ln sn w))

;; =============================================================================

(module+ test
  (require
    (only-in racket/string string-split)
    rackunit
    ipoe/private/util/rackunit-abbrevs)

  ;; -- and/no-quirks / for/no-quirks
  (let ([q (quirk 1 "")])
    (define-syntax-rule (check-q? e)
      (check-equal? e q))
    ;; --
    (check-true (and/no-quirks #t))
    (check-true (and/no-quirks #f #f #f #t))
    (check-true (and/no-quirks 1 2 3 4 #t))
    ;; --
    (check-q? (and/no-quirks q))
    (check-q? (and/no-quirks q #f #t #f))
    (check-q? (and/no-quirks #f q #t))
    (check-q? (and/no-quirks #t q #f))
    ;; -- for/no-quirks
    (check-true
      (for/no-quirks ([x '(1 2 3 4)]) #t))
    (check-true
      (for/no-quirks ([x (list q q q)]) (quirk? x)))
    (check-q?
      (for/no-quirks ([x (list 1 2 3)]) q))
    (check-q?
      (for/no-quirks ([x (list 1 2 3)]) (if (= 1 x) q x))))

  ;; -- contains-word?
  (let ([ln (line/loc '#("i" "could" "not" "stop" "for" "death") 1 2)])
    (check-true* (lambda (w) (contains-word? ln w))
      ["I"]
      ["stop"]
      ["death"]))

  (check-true* (lambda (ln w) (quirk? (contains-word? ln w)))
    [(line/loc '#("the" "raging" "milkman") 2 3) "a"]
    [(line/loc '#("") 9 0) "hey"])

  (check-exn #rx"parse-word"
    (lambda () (contains-word? (line/loc '#() 8 3) "")))

  ;; -- last-line (technically these are contract errors, but whatever)
  (check-apply* last-line
    [(stanza/loc '#("winnie" "the" "boo") 8)
     == (line/loc "boo" 2 8)]
    [(stanza/loc '#("heya") 8)
     == (line/loc "heya" 0 8)])

  ;; -- last-stanza
  (check-apply* last-stanza
    [(poem "" '#(#()))
     == (stanza/loc '#() 0)]
    [(poem "" '#(#("asdf" "bad" "lkmnue") #("x" "y" "z")))
     == (stanza/loc '#("x" "y" "z") 1)])

  ;; -- last-word
  (check-apply* last-word
    [(line/loc '#("winnie" "the" "boo") 8 3)
     == (word/loc "boo" 2 8 3)]
    [(line/loc '#("heya") 8 113)
     == (word/loc "heya" 0 8 113)])

  ;; 2015-09-24: we assume the poem is never empty!
  ;(check-exn #rx"last-word"
  ;  (lambda () (last-word (line/loc '#() 0 0))))

  ;; -- line
  (check-equal?
    (line 0 (stanza/loc '#("a") 3))
    (line/loc "a" 0 3))
  (check-equal?
    (line 1 (stanza/loc '#("a" "b" "c") 7))
    (line/loc "b" 1 7))
  (check-equal?
    (line 5 (stanza/loc '#("a" "b" "c" "d" "e" "f") 1))
    (line/loc "f" 5 1))

  (check-exn #rx"ipoe:safe-vector-ref"
    (lambda () (line 0 (stanza/loc '#() 4))))
  (check-exn #rx"ipoe:safe-vector-ref"
    (lambda () (line -1 (stanza/loc '#(a b c) 11))))

  ;; -- line=?
  (check-true* line=?
    [(line/loc '#("") 1 2)
     (line/loc '#("") 4 6)]
    [(line/loc '#("shall" "i" "compare" "thee") 4 4)
     (line/loc '#("shall" "i" "compare" "thee") 4 4)]
    [(line/loc '#("yes") 9 999)
     (line/loc '#("yes") 124 0)]
    [(line 1 (stanza/loc '#(#("") #("its" "working" "ok")) 5))
     (line/loc '#("its" "working" "ok") 12 6)])

  ;; --- line=? accepts variable-arity
  (check-true (line=? (line/loc '#("a") 0 1)
                      (line/loc '#("a") 0 1)
                      (line 0 (stanza 0 '#(#(#("a")))))
                      (line/loc '#("a") 7 2)))

  (check-true* (lambda (l1 l2) (quirk? (line=? l1 l2)))
   [(line/loc '#("A") 4 5)
    (line/loc '#("B") 82 1)]
   [(line/loc '#("just" "a" "minute") 1 1)
    (line/loc '#("justaminute") 8 32)]
   [(line 0 (stanza/loc '#(#("12" "black" "birds")) 5))
    (line 0 (stanza/loc '#(#("13" "black" "birds")) 6))])

  ;; -- line*->to-stanza*
  (define-syntax-rule (check-to-stanza* [x stanza*] ...)
    (begin (for ([stanza-line* (line*->stanza* x)]
                 [line* (in-vector stanza*)])
             (check-equal? stanza-line* line*)) ...))
  (check-to-stanza*
    ['("a" "a" "a" "" "b" "b" "b")
     '#(#(#("a") #("a") #("a")) #(#("b") #("b") #("b")))]
    ['("")
     '#()]
    ['("asdf" "asdf" "asdf")
     '#(#(#("asdf") #("asdf") #("asdf")))]
    ['("what" "a WONDERFUL compliment!")
     '#(#(#("what") #("a" "wonderful" "compliment")))]
    ['("line .one" "line .two")
     '#(#(#("line" "one") #("line" "two")))])

  ;; -- poem-count-stanza*
  (check-apply* poem-count-stanza*
   [(poem "" #()) == 0]
   [(poem "" #(#(#()) #(#()) #(#("a" "b" "c")))) == 3])

  ;; -- poem->stanza*
  (check-apply* poem->stanza*
   [(poem "" #()) == '#()]
   [(poem "hi" #(#(#("a")) #() #("c")))
    == (vector (stanza/loc '#(#("a")) 0)
               (stanza/loc '#() 1)
               (stanza/loc '#("c") 2))])

  ;; -- poem->word/loc*
  (check-apply* poem->word/loc*
   [(poem "" #(#(#("a" "b" "c") #("d" "e")) #(#("f") #("g"))))
    == (list
         (word/loc "a" 0 0 0)
         (word/loc "b" 1 0 0)
         (word/loc "c" 2 0 0)
         (word/loc "d" 0 1 0)
         (word/loc "e" 1 1 0)
         (word/loc "f" 0 0 1)
         (word/loc "g" 0 1 1))])

  ;; -- stanza
  (check-apply* stanza
   [0 '#(a)
    == (stanza/loc 'a 0)]
   [1 '#(a b c)
    == (stanza/loc 'b 1)]
   [5 '#(a b c d e f)
    == (stanza/loc 'f 5)])

  (check-exn (regexp "ipoe:safe-vector-ref")
             (lambda () (stanza 0 '#())))
  (check-exn (regexp "ipoe:safe-vector-ref")
             (lambda () (stanza -1 '#(a b c))))

  ;; -- stanza-count-lines
  (check-apply* stanza-count-lines
   [(stanza/loc '#() 0) == 0]
   [(stanza/loc '#(#("a") #("b")) 1)
    == 2]
   [(stanza/loc '#(#("yes" "sir") #("youve") #("got" "it")) 3)
    == 3])

  ;; -- stanza->line*
  (check-apply* stanza->line*
   [(stanza/loc '#() 0) == '#()]
   [(stanza/loc '#(#("a") #("b")) 1)
    == (vector (line/loc '#("a") 0 1)
               (line/loc '#("b") 1 1))]
   [(stanza/loc '#(#("yes" "sir") #("youve") #("got" "it")) 3)
    == (vector (line/loc '#("yes" "sir") 0 3)
             (line/loc '#("youve")     1 3)
             (line/loc '#("got" "it")  2 3))])

  ;; -- word
  (check-apply* word
   [3 (line/loc '#("hel" "lo" "w" "orld") 1 2)
    == (word/loc "orld" 3 1 2)]
   [0 (line 1 (stanza 2 '#(#() #() #(#("what" "is") #("going" "on")))))
    == (word/loc "going" 0 1 2)])

  (check-exn (regexp "ipoe:safe-vector-ref")
             (lambda () (word 0 (line/loc '#() 3 8))))

  ;; -- word=?
  (check-true* (lambda (w1 w2) (word=? w1 w2))
   [(word/loc "yes" 1 2 3)
    (word/loc "yes" 3 1 5)]
   [(word 0 (line 0 (stanza 0 '#(#(#("yes"))))))
    (word 0 (line 0 (stanza 0 '#(#(#("yes"))))))]
   [(word 0 (line 0 (stanza 0 '#(#(#("cant"))))))
    (word 0 (line 0 (stanza 0 '#(#(#("cant"))))))]
  )
  (check-true (word=? (word/loc "ab" 3 1 2)
                      (word/loc "ab" 3 3 6)
                      (word/loc "ab" 3 33 6)
                      (word/loc "ab" 3 3 96)))

  (check-true* (lambda (w1 w2) (quirk? (word=? w1 w2)))
   [(word/loc "a" 4 6 8)
    (word/loc "b" 8 1 51)])

  (check-true (quirk? (word=? (word/loc "a" 1 1 1)
                      (word/loc "b" 1 1 1)
                      (word/loc "c" 1 1 1))))

  ;; -- safe-vector-ref
  (let ([v (vector #t #t #t)])
    (define-syntax-rule (check-bad-ref* i ...)
      (begin (check-exn #rx"ipoe:safe-vector-ref"
                        (lambda () (safe-vector-ref i v))) ...))
    (check-true* safe-vector-ref
     [0 v]
     [1 v]
     [2 v])
    (check-bad-ref* -1 -10 5 3 9000))

)
