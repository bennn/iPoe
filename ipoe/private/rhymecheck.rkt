#lang racket/base

;; Functions for checking rhyme schemes

(provide
  check-rhyme-scheme
  ;; (-> (Sequenceof (Listof String)) #:rhyme-scheme RhymeScheme #:src Symbol Either)
  ;; Check that the input text matches the rhyme scheme
  ;; Assumes DB context

  replace-wildcard-syllables
  ;; (-> RhymeScheme Natural RhymeScheme)
  ;; Overwrite all wildcard syllables in the rhyme scheme

  rhyme-scheme?
  ;; (-> Any Boolean)
  ;; Predicate defining rhyme schemes
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/db
  ipoe/private/either
  ipoe/private/parse
  ipoe/private/parameters
  ipoe/private/suggest
  ipoe/private/ui
  ;; --
  racket/match
  (only-in racket/string string-join)
)

;; =============================================================================
;; == API functions

;; 2015-08-06: May want to return the VarMap some day
;; (: check-rhyme-scheme (-> (Sequenceof (Listof String)) #:rhyme-scheme RhymeScheme #:src Symbol Either))
(define (check-rhyme-scheme stanza* #:rhyme-scheme rs*)
  ;; TODO do everything in one pass
  ;; -- preconditions
  (either-monad
    (if (rhyme-scheme? rs*)
      (success 'type-check (void))
      (failure 'rhyme-scheme (format "Expected a RhymeScheme, got ~a" rs*)))
    (check-num-stanzas stanza* rs*)
    ;; -- check rhyme (requires DB connection)
    (check-stanza* '() stanza* rs* #:stanza-number 0)))

;; -----------------------------------------------------------------------------
;; -- dynamic typechecking predicates

;; (define-type RhymeScheme (Listof StanzaScheme))
;; (define-type StanzaScheme (Listof LineScheme))
;; (define-type LineScheme (U Rhyme Syllable (Pairof Rhyme Syllable)))
;; (define-type Rhyme (U Wildcard Symbol))
;; (define-type Syllable (U Wildcard Natural))
;; (define-type Wildcard (U '*))

;; A rhyme scheme is a (Listof (Listof (Pairof Symbol (U Wildcard Natural))))
;; (: rhyme-scheme? (-> Any Boolean))
(define (rhyme-scheme? x**)
  (and (list? x**)
       (for/and ([x* (in-list x**)])
         (stanza-scheme? x*))))

(define (stanza-scheme? x*)
  (and (list? x*)
       (for/and ([x (in-list x*)])
         (line-scheme? x))))

(define (line-scheme? x)
  (or (and (pair? x)
        (rhyme? (car x))
        (syllable? (cdr x)))
      (rhyme? x)
      (syllable? x)))

(define (rhyme? x)
  (or (wildcard? x)
      (symbol? x)))

(define (syllable? x)
  (or (wildcard? x)
      (exact-nonnegative-integer? x)))

(define (wildcard? x)
  (eq? '* x))

;; Future-proofing.
;; A line spec could be done as a struct, but I really want it easily read-able

(define (line-scheme->rhyme ls)
  (cond
   [(pair? ls) (car ls)]
   [(syllable? ls) '*]
   [(rhyme? ls) ls]
   [else (internal-error 'rhymecheck (format "Expected a LineScheme, got ~a" ls))]))

(define (line-scheme->syllable ls)
  (cond
   [(pair? ls) (cdr ls)]
   [(syllable? ls) ls]
   [(rhyme? ls) '*]
   [else (internal-error 'rhymecheck (format "Expected a LineScheme, got ~a" ls))]))

(define (split-stanza-scheme* ls)
  (values (map line-scheme->rhyme ls)
          (map line-scheme->syllable ls)))

;; -----------------------------------------------------------------------------
;; -- varmap functions

;; (define-type VarMap (Listof (Pairof Symbol String)))

;; (: varmap-lookup (-> VarMap Symbol (U #f String)))
(define (varmap-lookup sym+word* sym)
  (for/first ([sym+word (in-list sym+word*)]
              #:when (eq? sym (car sym+word)))
    (cdr sym+word)))

;; (: varmap-add (-> VarMap Symbol String VarMap))
(define (varmap-add sym+word* sym word)
  (cons (cons sym word) sym+word*))

;; -----------------------------------------------------------------------------

;; (: check-num-stanzas (-> (Sequenceof (Listof String)) RhymeScheme #:src Symbol Void))
(define (check-num-stanzas stanza* rs*)
  (check-length stanza* rs* "stanzas" #:src 'num-stanzas))

;; (: check-num-lines (-> (Sequenceof Any) (Sequenceof Any) #:stanza-number Natural #:src Symbol Void))
(define (check-num-lines stanza rs #:stanza-number n)
  (check-length stanza rs (format "lines in stanza ~a" n) #:src 'num-lines))

;; (: check-length (-> (Sequenceof Any) (Sequenceof Any) String #:src Symbol Void))
(define (check-length seq-test seq-ref descr-str #:src src)
  (define len-test (for/sum ([s seq-test]) 1))
  (define len-ref  (for/sum ([r seq-ref]) 1))
  (if (= len-test len-ref)
    (success src (void))
    (failure src (format "Expected ~a ~a, got ~a" len-ref descr-str len-test))))

;; Check the number of lines, rhyme scheme, and syllables of each line in
;;  all stanzas.
;; Preconditions:
;; - rs* is a rhyme scheme
;; - stanza* has the same number of stanzas as rs*
;; - there is an open connection to the ipoe database
(define (check-stanza* varmap stanza* rhyme-scheme #:stanza-number n)
  (match (cons stanza* rhyme-scheme)
    [(cons (cons stanza st*) (cons s+r* rs*))
     ;; Match `stanza` with syllables & rhymes for lines.
     ;; Either continue the recursion or return a failure result
     (define-values (r* s*) (split-stanza-scheme* s+r*))
     (define result (either-monad
       (check-num-lines stanza s+r* #:stanza-number n)
       (check-syllables stanza s*   #:stanza-number n)
       (unify-rhyme-scheme varmap stanza r* #:stanza-number n)))
     (if (success? result)
         ;; Continue, with the new varmap
         (check-stanza* (success-value result) st* rs* #:stanza-number (add1 n))
         ;; Stop iteration, return the failure
         result)]
    [(cons '() '())
     ;; End of recursion (could return the varmap, I guess)
     (success 'rhyme-scheme (void))]
    [_
     (internal-error 'check-stanza* (format "invariant broken: arguments stanza* and rhyme-scheme have different lengths.\n  stanza* = ~a\n  rhyme-scheme = ~a" stanza* rhyme-scheme))]))

;; Preconditions:
;; - len(stanza) == len(syll*)
;; - must be called with an active database connection
(define (check-syllables stanza syll*
                         #:stanza-number sn
                         #:line-number [ln 0])
  (define src 'check-syllables)
  (match (cons stanza syll*)
    [(cons (cons line st) (cons s s*))
     (cond
       [(wildcard? s)
        ;; Ignore wildcards
        (check-syllables st s*
                         #:stanza-number sn #:line-number (add1 ln))]
       [(line->syllables line)
        ;; 2015-08-13: Never fails, but that may be a bad idea.
        => (lambda (actual-syll)
        (if (= s actual-syll)
            (check-syllables st s*
                             #:stanza-number sn #:line-number (add1 ln))
            (failure src (format "Expected ~a syllables in line ~a of stanza ~a, got ~a syllables" s ln sn actual-syll))))])]
    [(cons '() '())
     ;; All done
     (success src (void))]
    [_
     ;; Should never happen
     (internal-error src (format "Invariant error: expected arguments `stanza` and `syll*` to be the same length.\n  stanza = ~a\n  syll* = ~a" stanza syll*))]))

;; Counts the number of syllables in a line.
;; NEVER fails, even with unknown words.
;; Preconditions:
;; - must be called with an active database connection
(define (line->syllables line)
  (for/sum ([w (in-list (string->word* line))])
    (or (word->syllables w)
        (begin
          (alert (format "Could not determine syllables for word '~a'" w))
          0))))

(define (replace-wildcard-syllables rs s)
  (for/list ([stanza-spec (in-list rs)])
    (for/list ([line-spec (in-list stanza-spec)])
      (cond
       [(wildcard? line-spec)
        ;; Plain wildcard => wildcard rhyme, known syllables
        (cons '* s)]
       [(rhyme? line-spec)
        ;; Add syllables to plain rhymes
        (cons line-spec s)]
       [(and (pair? line-spec) (wildcard? (cdr line-spec)))
        (cons (car line-spec) s)]
       [else ;; (or (syllable? line-spec) (pair? line-spec))
        line-spec]))))

;; True if two words rhyme in the context of this poem
;; For now, allowing both rhyme and almost-rhyme
;; (: rhyme=? (-> String String Boolean))
(define (rhyme=? w1 w2)
  ;; Precondition: must be called with an active database connection
  (or (string=? w1 w2) ;; TODO rhymes-with? is not necessarily an equivalence
      (rhymes-with? w1 w2)
      (almost-rhymes-with? w1 w2)))

;; Unify a stanza of poetry with a rhyme scheme
;; (: unify-rhyme-scheme (-> VarMap (Listof String) (Listof Symbol) #:src Symbol #:stanza-number Natural VarMap))
(define (unify-rhyme-scheme vm stanza rs #:stanza-number sn #:line-number [ln 0])
  (define src 'unify-rhyme-scheme)
  (match (cons stanza rs)
    [(cons (cons line st*) (cons var rs*))
     ;; Match the last word of the line with a rhyme,
     ;;  either the rhyme stored in the varmap OR bind the rhyme scheme variable
     (define lw (string-last line))
     (cond
      [(wildcard? var)
       ;; Ignore wildcards
       (unify-rhyme-scheme vm st* rs*
                           #:stanza-number sn
                           #:line-number (add1 ln))]
      [(varmap-lookup vm var)
       => (lambda (rhyme)
       (cond
        [(rhyme=? rhyme lw)
         ;; Success!
         (unify-rhyme-scheme vm st* rs*
                             #:stanza-number sn
                             #:line-number (add1 ln))]
        [else
         (define r* (filter-similar lw (word->rhyme* rhyme) #:limit 3))
         (define a* (filter-similar lw (word->almost-rhyme* rhyme) #:limit 2))
         (define suggest-str
           (if (and (null? r*) (null? a*))
               ""
               (string-append " Suggestions: '"
                              (string-join (append r* a*) "', '")
                              "'")))
         (failure src (format "Line ~a of stanza ~a does not match rhyme scheme. Expected a word to rhyme with '~a' but got '~a'.~a" (add1 ln) (add1 sn) rhyme lw suggest-str))]))]
      [else
       (unify-rhyme-scheme (varmap-add vm var lw) st* rs*
                           #:stanza-number sn
                           #:line-number (add1 ln))])]
    [(cons '() '())
     ;; All finished
     (success src vm)]
    [_ ;; Should never happen
     (internal-error src (format "Invariant error: expected stanza and rhyme-scheme to have the same number of elements.\n  stanza = ~a\n  rhyme-scheme = ~a" stanza rs))]))

;; =============================================================================

(module+ test
  (require rackunit "rackunit-abbrevs.rkt")

  (define o* (options-init))

  (define-syntax-rule (with-db/test e ...)
    (parameterize-from-hash o* (lambda ()
      (with-ipoe-db #:user (*user*)
                    #:dbname (*dbname*)
                    #:interactive? #t
                    #:commit? #f
        (lambda () e ...)))))

  ;; -- check-rhyme-scheme
  (with-db/test
    (check-true* (lambda (s r) (success? (check-rhyme-scheme s #:rhyme-scheme r)))
      ['() '()]
      ['(("the quick brown fox" "Jumped over the lazy dog")) '(((A . *) (B . *)))]
      ['(("anything")) '(((A . *)))]
      ['(("cat") ("rat") ("mat") ("sat")) '(((X . *)) ((X . *)) ((X . *)) ((X . *)))]
      ['(("cat") ("rat") ("mat") ("sat")) '(((X . 1)) ((X . 1)) ((X . 1)) ((X . 1)))]
      ['(("cat") ("rat") ("mat") ("sat")) '((X) (X) (X) (X))]
      ['(("cat") ("rat") ("mat") ("sat")) '((1) (1) (1) (1))]
      ['(("willful and raisin" "under the weather" "jet set go")
         ("domino effect" "very motivation")
         ("beside our goal" "the free bird" "gory category"))
       '(((A . 5) (B . 5) (C . 3)) ((D . 5) (E . 6)) ((F . 4) (G . 3) (H . 6)))]
      ['(("once" "upon" "a" "time" "a" "long" "slime" "ago")
         ("in" "a" "land" "full" "of" "snow")
         ("the" "end"))
       '(((A . 1) (B . 2) (C . 1) (D . 1) (C . 1) (E . 1) (D . 1) (F . 2))
         ((G . *) (C . *) (I . *) (J . *) (K . *) (F . *))
         ((L . *) (M . *)))])

    (check-true* (lambda (s r) (failure? (check-rhyme-scheme s #:rhyme-scheme r)))
      ['() '((A))]
      ['(("never" "land")) '()]
      ['(("never" "land")) '(())]
      ['(("never" "land")) '(((X . *)))]
      ['(("never" "land")) '(((A . *) (A . *)))]
      ['(("never" "land")) '(((F . *) (U . *) (N . *)))]
      ['(("never" "land")) '((F U N))]
      ['(("never" "land")) '(((A . *) (B . 2)))]
      ['(("never" "land")) '((* 2))]
      ['(("once" "upon" "a" "time" "a" "long" "hour" "ago"))
       '(((A . 1) (B . *) (C . *) (D . *) (C . *) (E . *) (D . *) (F . *)))]
      ['(("once" "upon" "a" "time" "aye" "long" "slime" "ago")
         ("in" "a" "land" "full" "of" "snow")
         ("the" "end"))
       '(((A . 1) (B . 2) (C . 1) (D . 1) (C . 1) (E . 1) (D . 1) (F . 2))
         ((G . *) (C . 77) (I . *) (J . *) (K . *) (F . *))
         ((L . *) (M . *)))]
    ))

  ;; ---------------------------------------------------------------------------
  ;; -- rhyme-scheme?
  (check-true* rhyme-scheme?
    ['()]
    ['(())]
    ['(((A . 1)))]
    ['(((A . 1) (B . 1) (A . 0) (C . 0))
       ((B . 99) (C . 6) (DA . 1) (e . 3) (f . 1)))]
    ['((A B A  C )
       (B C DA e  f ))]
    ['((1 1 0 0)
       (99 6 1 3 1))]
  )
  (check-false* rhyme-scheme?
    ['("yolo")]
    ['(4)]
    ['(("A"))]
    [68]
    ['free]
  )

  ;; -- stanza-scheme?
  (check-true* stanza-scheme?
    ['()]
    ['((A . 1) (B . 1) (C . 1) (D . 1))]
    ['((* . *))]
    ['((* . 0) (* . 14) (A . *))]
    ;; -- non-pairs
    ['(A B C D)]
    ['(1 1 3)]
  )

  (check-false* stanza-scheme?
    ['((1 . A))]
    ['((* . *) (* . *) (* . A))]
  )

  ;; -- line-scheme?
  (check-true* line-scheme?
    ['(A . 1)]
    ['(B . 0)]
    ['(* . *)]
    ['(hello . *)]
    ['(* . 0)]
    ;; -- non-pairs
    ['hello]
    ['*]
    [0]
    [83]
  )

  (check-false* line-scheme?
    ['()]
    ["not a pair"]
    ['(1 . A)]
    ['(A A)]
    ['(A 1)]
    ['("yes" . "no")]
    ['(2 . *)] ;; Reversed order
    [-2]
  )

  ;; -- rhyme?
  (check-true* rhyme?
    ['A]
    ['AAA]
    ['*]
    ['yolo]
    ['n1234]
  )

  (check-false* rhyme?
    ["hi"]
    [123]
    [(lambda (x) x)]
  )

  ;; -- syllable?
  (check-true* syllable?
    [1]
    [2]
    [3]
    ['*]
    [0]
    [9001]
  )

  (check-false* syllable?
    ['a]
    ['b]
    ['n0]
    ['doorknob]
    ["nope"]
    ['(yo lo)]
  )

  ;; -- wildcard?
  (check-true* wildcard?
    ['*]
  )

  (check-false* wildcard?
    ["nope"]
    ['Z]
    [42]
    ['(((())))]
    [(lambda (x) x)]
  )

  ;; -- line-scheme->rhyme
  (check-apply* line-scheme->rhyme
    ['(A . 2) == 'A]
    ['(B . 0) == 'B]
    ['(c . 11) == 'c]
    ['(D . *) == 'D]
    ['("yes" . "no") == "yes"]
    ;; -- syllables => rhyme is a wildcard
    [41 == '*]
    [0 == '*]
    [6 == '*]
    ;; -- rhyme => rhyme
    ['* == '*]
    ['A == 'A]
    ['hello-world == 'hello-world]
  )
  (check-exn (regexp "rhymecheck")
             (lambda () (line-scheme->rhyme "invalid!")))

  ;; -- line-scheme->syllable
  (check-apply* line-scheme->syllable
    ['(A . 2) == 2]
    ['(B . 0) == 0]
    ['(c . 11) == 11]
    ['(D . *) == '*]
    ;; -- rhyme => syllables are wildcard
    ['A == '*]
    ['yessir == '*]
    ;; -- syllables => syllables
    ['* == '*]
    [0 == 0]
    [2531 == 2531]
  )
  (check-exn (regexp "rhymecheck")
             (lambda () (line-scheme->syllable "invalid!")))

  ;; -- split-stanza-scheme*
  (let-values ([(r* s*) (split-stanza-scheme* '((A . 1) (B . 2) (* . 0)))])
    (check-equal? r* '(A B *))
    (check-equal? s* '(1 2 0)))
  ;; Rhymes only
  (let-values ([(r* s*) (split-stanza-scheme* '(A B *))])
    (check-equal? r* '(A B *))
    (check-equal? s* '(* * *)))
  ;; Syllables only
  (let-values ([(r* s*) (split-stanza-scheme* '(1 2 0))])
    (check-equal? r* '(* * *))
    (check-equal? s* '(1 2 0)))

  ;; ---------------------------------------------------------------------------
  ;; -- varmap-lookup
  (check-apply* varmap-lookup
    ['() 'a == #f]
    ['((a . "b")) 'b == #f]
    ['((a . "b") (c . "d") (e . "f")) 'A == #f]
    ['((a . "b") (c . "d") (e . "f")) 'f == #f]
    ;; --
    ['((a . "b")) 'a == "b"]
    ['((a . "b") (car . "dee") (eee . "f")) 'eee == "f"]
    ['((lolcats . "rofl") (win . "win")) 'win == "win"]
  )

  ;; -- varmap-add
  (define-syntax-rule (check-varmap-add [vm new-sym new-str] ...)
    (begin (begin (check-false (varmap-lookup vm new-sym))
                  (let ([vm2 (varmap-add vm new-sym new-str)])
                    (check-equal? (varmap-lookup vm2 new-sym) new-str))) ...))
  (check-varmap-add
    ['() 'a "b"]
    ['((A1 "B1")) 'A2 "B2"]
  )

  ;; ---------------------------------------------------------------------------
  ;; -- check-num-stanzas
  (check-true* (lambda (a b) (success? (check-num-stanzas a b)))
    ['() '()]
    ['(("a")) '((X))]
    ['(() () () ()) '(() () () ())]
    ['(("yo" "lo") ("wepa")) '((X X X) (Y Z A))])

  (check-true* (lambda (a b) (failure? (check-num-stanzas a b)))
    ['() '((a))]
    ['(("word")) '()]
    ['(("a") ("b")) '((a))]
    ['(("a") ("b")) '((a) (b) (c) (d))])

  ;; -- check-num-lines
  (check-true* (lambda (a b) (success? (check-num-lines a b #:stanza-number 1)))
    ['() '()]
    ['("a") '(b)]
    ['("yes I say" "yes" "Yes") '(one two three)])

  (check-true* (lambda (a b) (failure? (check-num-lines a b #:stanza-number 66)))
    ['() '(a)]
    ['("a") '()]
    ['("asdf asdf" "bbb") '(q w e r)])

  ;; -- check-length
  (check-true* (lambda (a b) (success? (check-length a b "hi" #:src 'pass)))
    ['() '()]
    ['(1 2 3) '(1 2 3)]
    ["blah" "blah"]
    ['(() () 3) '(() 513423123 ("yes" "Yes"))])

  (check-true* (lambda (a b) (failure? (check-length a b "ih" #:src 'fail)))
    ['(a b c) '()]
    ['() '(a b c)]
    ["blah" "b"])

  ;; -- check-stanza*
  (with-db/test
    (define (test-check-stanza*/pass vm stanza rs)
      (success? (check-stanza* vm stanza rs #:stanza-number 0)))
    (check-true* test-check-stanza*/pass
      ['() '() '()]
      ['() '(("yes")) '(((* . *)))]
      ['() '(("yes") ("and" "yes")) '(((* . *)) ((* . *) (* . *)))]
      ['() '(("tomato") ("paste")) '(((* . 3)) ((* . 1)))]
      ['() '(("pogo" "stripe") ("kite" "sight" "night")) '(((* . 2) (A . 1)) ((B . 1) (B . 1) (B . 1)))]
      ['() '(("yes I do" "you do not") ("sorrow") ("tomorrow")) '(((* . 3) (* . 3)) ((A . 2)) ((A . 3)))]
      ['((A . "word") (B . "mouse"))
       '(("bird" "house") ("furred" "spouse"))
       '(((A . 1) (B . 1)) ((A . 1) (B . 1)))]
    )
    (define (test-check-stanza*/fail vm stanza rs)
      (failure? (check-stanza* vm stanza rs #:stanza-number 0)))
    (check-true* test-check-stanza*/fail
      ['() '(("hello" "goodbye")) '(((A . 2) (A . 2)))]
      ['() '(("tomato") ("paste")) '(((* . 0)) ((* . 1)))]
      ['() '(("pogo" "stripe") ("kite" "sight" "night")) '(((* . 1) (A . 1)) ((B . 1) (B . 1) (B . 1)))]
      ['() '(("yes I do" "you do not") ("sorrow") ("tomorrow")) '(((X . 3) (* . 3)) ((A . 2)) ((X . 3)))]
      ['((A . "word") (B . "mouse"))
       '(("bird" "house") ("worried" "spouse"))
       '(((A . 1) (B . 1)) ((A . 1) (B . 1)))]
    ))

  ;; -- check-syllables
  (with-db/test
    (define (test-check-syllables/pass stanza syll*)
      (success? (check-syllables stanza syll* #:stanza-number 4 #:line-number 5)))
    (check-true* test-check-syllables/pass
      ['() '()]
      ['("yes" "yes" "yes.") '(1 1 1)]
      ['("waffle" "fries" "served" "hourly") '(2 1 * *)]
    )
    (define (test-check-syllables/fail stanza syll*)
      (failure? (check-syllables stanza syll* #:stanza-number 4 #:line-number 5)))
    (check-true* test-check-syllables/fail
      ['("apple") '(3)]
      ['("walnut" "flavored" "goat" "cheese") '(* * * 3)]
    )
    ;; -- invariant error
    (check-exn (regexp "ipoe:check-syllables:internal-error")
      (lambda () (check-syllables '("a" "a") '(*) #:stanza-number 3)))
    (check-exn (regexp "ipoe:check-syllables:internal-error")
      (lambda () (check-syllables '() '(1 2 3) #:stanza-number 3))))

  ;; -- line->syllables
  (with-db/test
    (check-apply* line->syllables
     ["" == 0]
     ["a" == 1]
     ["yes yes yes" == 3]
     ["hello, world!" == 3]
     ["volcanic antidisestablishmentarianism you know" == 16]
     ["." == 0])
    (check-apply* (lambda (ln)
                    (check-print (for/list ([i (in-range 2)])
                                   #rx"^Could not determine syllables")
                                 (lambda () (line->syllables ln))))
     ;; -- unknown words have 0 syllables
     ["madeupwordnotarealword bladlaksdczjiewdscz" == 0]))

  ;; -- replace-wildcard-syllables
  (check-apply* replace-wildcard-syllables
   ['() 3 == '()]
   ['((A A A) (B B C)) 1 == '(((A . 1) (A . 1) (A . 1)) ((B . 1) (B . 1) (C . 1)))]
   ['((1 2 3)) 7 == '((1 2 3))]
   ['(((A . 42) (B . *)) (*)) 2 == '(((A . 42) (B . 2)) ((* . 2)))]
  )

  ;; -- rhyme=?
  (with-db/test
    (check-true* rhyme=?
     ;; -- rhyme
     ["car" "far"]
     ;; -- almost-rhyme
     ["lettuce" "conscientious"]
    )
    (check-false* rhyme=?
     ["cat" "dog"]
     ["second" "masterpiece"]
    )
    ;; --
    (check-exn (regexp "ipoe:db")
      (lambda () (rhyme=? "cat" "blahblahblahblahblah"))))

  ;; -- unify-rhyme-scheme
  (define-syntax-rule (check-unify/pass [vm st rs == vm2] ...)
    (begin
      (let ([result (unify-rhyme-scheme vm st rs #:stanza-number 0)])
        (check-pred success? result)
        (check-equal? (success-value result) vm2)) ...))
  (with-db/test
    (let ([vm1 '((V1 . "val1") (Var2 . "val2") (M . "m"))])
      (check-unify/pass
        ['() '() '() == '()]
        [vm1 '() '()
         == vm1]
        ['() '("cat" "dog" "rat") '(A B A)
         == '((B . "dog") (A . "cat"))]
        ['((X . "role")) '("mole" "sole") '(X X)
         == '((X . "role"))]
        [vm1 '("car" "file" "scientist") '(* * *)
         == vm1]
      )))

  (define-syntax-rule (check-unify/fail [vm st rs] ...)
    (begin (check-pred failure? (unify-rhyme-scheme vm st rs #:stanza-number 2)) ...))
  (with-db/test
    (check-unify/fail
      ['() '("a" "bacon") '(A A)]
      ['() '("harpoon" "moon" "will") '(A B A)]
      ['((A . "worm")) '("cat" "dog" "rat") '(A B A)]))

  ;; Invariant: unify-rhyme-scheme expects 2nd and 3rd args to have same length
  (check-exn (regexp "ipoe:.*:internal-error")
    (lambda () (unify-rhyme-scheme '() '() '(A) #:stanza-number 2)))
  (check-exn (regexp "ipoe:.*:internal-error")
    (lambda () (unify-rhyme-scheme '() '("hello") '() #:stanza-number 2)))

  (with-db/test
    ;; Nothing rhymes with a fake word
    (check-exn (regexp "ipoe:db")
      (lambda () (unify-rhyme-scheme '() '("asdgwrfscbad" "hello" "bye") '(A A A) #:stanza-number 81))))

)