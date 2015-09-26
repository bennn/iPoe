#lang racket/base

;; TODO testing tools, for making stanzas etc

;; Functions for checking rhyme schemes
(require racket/contract/base)
(provide
  (contract-out
    [check-rhyme-scheme (-> poem? rhyme-scheme? (listof quirk?))])
  ;; (-> Poem RhymeScheme (Listof Quirk))
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
  ipoe/private/parameters
  ipoe/private/poem
  ipoe/private/poem/poetic-license
  ipoe/private/ui
  ipoe/private/suggest
  ipoe/private/util/sequence
)

;; =============================================================================
;; == API functions

;; 2015-08-06: May want to return the VarMap some day
;; (-> poem rhyme-scheme (listof quirk))
(define (check-rhyme-scheme P rs*)
  (define q0 (check-num-stanzas P rs*))
  (if (quirk? q0)
      (list q0)
      (check-stanza* (poem->stanza* P) rs*)))

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

(struct varmap (
  vm ;; (HashTable Symbol String)
) #:transparent)

;; (: varmap-lookup (-> VarMap Symbol (U #f Word wildcard?)))
(define (varmap-lookup vm sym)
  (if (wildcard? sym)
      sym
      (hash-ref (varmap-vm vm) sym (lambda () #f))))

;; (: varmap-add! (-> VarMap Symbol Word Void))
(define (varmap-add! vm sym w)
  (hash-set! (varmap-vm vm) sym w))

;; Aka, non-functional `list->varmap`
(define (varmap-add!* vm kv*)
  (for ([k+v (in-list kv*)])
    (varmap-add! vm (car k+v) (cdr k+v))))

(define (varmap-init)
  (varmap (make-hasheq)))

;; -----------------------------------------------------------------------------

;; (: check-num-stanzas (-> Poem RhymeScheme (U quirk? #t)))
(define (check-num-stanzas P rs*)
  (define L0 (length rs*))
  (define L1 (poem-count-stanza* P))
  (or (= L0 L1)
      (quirk (*bad-stanza-penalty*)
        (format "Expected ~a stanzas, got ~a stanzas" L0 L1))))

;; TODO type
(define (check-num-lines st rs)
  (define L0 (length rs))
  (define L1 (stanza-count-lines st))
  (or (= L0 L1)
      (quirk (*bad-lines-penalty*)
        (format "Expected ~a lines in ~a, got ~a lines" L0 (stanza/loc->string st) L1))))

;; (: check-stanza* (-> (Sequenceof Stanza) RhymeScheme (Listof Quirk)))
(define (check-stanza* stanza* rs*)
  ;; Iterate over stanzas, count quirks and update the varmap (functionally)
  (define vm (varmap-init))
  (apply append
    (for/list ([st stanza*]
               [rs (in-list rs*)])
      (check-stanza st rs #:varmap vm))))

;; Check the number of lines, rhyme scheme, and syllables
;;  of each line in a stanza
;; Preconditions:
;; - DB connection
;; (: check-stanza (->* [Stanza/Loc Rhyme-Scheme] [#:varmap VarMap] (U quirk varmap)))
(define (check-stanza st rs #:varmap vm)
  ;; Match `stanza` with syllables & rhymes for lines.
  ;; Either continue the recursion or return a failure result
  (define-values (r* s*) (split-stanza-scheme* rs))
  (define line-q (check-num-lines st rs))
  (if (quirk? line-q)
      (list line-q)
      (append (check-syllables st s*)
              (check-rhyme st r* #:varmap vm))))

;; Preconditions:
;; - len(stanza) == len(syll*)
;; - must be called with an active database connection
;; TODO type
(define (check-syllables st syll*)
  ;; TODO clean filtering
  (filter quirk?
    (for/list ([ln (stanza->line* st)]
               [S0  (in-list syll*)])
      (define S1 (line->syllables ln))
      (unless (or (wildcard? S0)
                  (= S0 S1))
        (quirk (* (*bad-syllable-penalty*) (abs (- S0 S1)))
          (format "Expected ~a syllables on ~a, got ~a syllables" S0 (line/loc->string ln) S1))))))

;; Counts the number of syllables in a line.
;; NEVER fails, even with unknown words.
;; Preconditions:
;; - must be called with an active database connection
;; TODO type
(define (line->syllables ln)
  (for/sum ([w (line->word* ln)]
            #:when (word-exists?/alert w))
    ;; TODO don't just use the first result from ->syllables*
    (sequence-first (word->syllables* (word/loc-word w)))))

(define (word-exists?/alert wl)
  (define w (word/loc-word wl))
  (unless (word-exists? w)
    (alert (format "~a is undefined. Consider adding it to the dictionary."
      (word/loc->string wl)))
    #f))

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
;; Precondition: Expects DB
;; (: rhyme=? (-> String String Boolean))
(define (rhyme=? wl0 wl1)
  (define w0 (word/loc-word wl0))
  (define w1 (word/loc-word wl1))
  (or (string=? w0 w1)
      (rhymes-with? w0 w1)))

(define (almost-rhyme=? w1 w2)
  (almost-rhymes-with? (word/loc-word w1)
                       (word/loc-word w2)))

;; Unify a stanza of poetry with a rhyme scheme
;; Silently updates the varmap!
;; TODO type
(define (check-rhyme st rs #:varmap vm)
  (filter quirk?
  (for/list ([ln (stanza->line* st)]
             [v (in-list rs)])
    (define W0 (varmap-lookup vm v))
    (define W1 (sequence-last (line->word* ln)))
    (cond
     [(wildcard? v)
      #t]
     [(word/loc? W0)
      (or (rhyme=? W0 W1)
          (and (almost-rhyme=? W0 W1)
               (quirk (*almost-rhyme-penalty*)
                 (format "Word '~a' almost rhymes with '~a'."
                   (word/loc-word W1)
                   (word/loc-word W0))))
          (quirk (*bad-rhyme-penalty*)
            ;; TODO cleaner error message
            (format "Expected a word to rhyme with ~a, got ~a (position ~a, line ~a, stanza ~a)\n"
              (word/loc-word W0)
              (word/loc-word W1)
              (word/loc-w-num W1)
              (word/loc-l-num W1)
              (word/loc-s-num W1))))]
     [else
      (varmap-add! vm v W1)]))))

;; =============================================================================

(module+ test
  (require
    rackunit
    ipoe/private/util/rackunit-abbrevs
    (only-in racket/string string-join))

  (define o* (options-init))

  (define-syntax-rule (with-db-test e ...)
    (parameterize-from-hash o* (lambda ()
      (with-ipoe-db #:user (*user*)
                    #:dbname (*dbname*)
                    #:interactive? #t
                    #:commit? #f
        (lambda () e ...)))))

  (define-syntax-rule (add-word/nothing w s)
     (add-word w #:syllables s
                 #:rhymes '()
                 #:almost-rhymes '()
                 #:online? #f
                 #:interactive? #f))

  (define-syntax-rule (sentence w ...)
    (string-join (list w ...) " "))

  ;; -- check-rhyme-scheme
  (with-db-test
    ;; Add a bunch of new words, so tests pass regardless of DB setup
    (let* ([wA (make-string 10 #\a)]
           [wB (make-string 10 #\b)]
           [wC (make-string 10 #\c)]
           [wD (make-string 10 #\d)]
           [wE (make-string 10 #\e)]
           [wF (make-string 10 #\f)]
           [wG (make-string 10 #\g)]
           [wH (make-string 10 #\h)]
           [wI (make-string 10 #\i)]
           [wJ (make-string 10 #\j)]
           [wK (make-string 10 #\k)]
           [wL (make-string 10 #\l)]
           [w* (list wA wB wC wD wE wF wG wH wI wJ wK wL)])
      (for ([w (in-list w*)]
            [s (in-naturals)])
        (add-word/nothing w s)
        (add-rhyme w w))
      (check-true* (lambda (s r)
                     (let ([r (check-rhyme-scheme (poem "" s) r)])
                     (if (null? r)
                         #t
                         r)))
       ['#() '()]
       [(vector (vector (vector wC) (vector wC wC wC)))
        '(((A . *) (B . *)))]
;       [`((,(sentence wD wE wF) ,(sentence wC wC wC)))
;        '(((A . *) (B . *)))]
;       [`((,wA))
;        '(((A . *)))]
;       [`((,wC) (,wC) (,wC) (,wC))
;        '(((X . *)) ((X . *)) ((X . *)) ((X . *)))]
;       [`((,wB) (,wB) (,wB) (,wB))
;        '(((X . 1)) ((X . 1)) ((X . 1)) ((X . 1)))]
;       [`((,wB) (,wB) (,wB) (,wB))
;         '((X) (X) (X) (X))]
;       [`((,wF) (,wF) (,wF) (,wF))
;         '((X) (X) (X) (X))]
;       [`((,wB) (,wB) (,wB) (,wB))
;        '((1) (1) (1) (1))]
;       [`((,(sentence wB wB wA) ,(sentence wC wB) ,(sentence wC))
;          (,wD ,wE)
;          (,wF ,wG ,wH))
;        '(((A . 2) (B . 3) (C . 2))
;          ((D . 3) (E . 4))
;          ((F . 5) (G . 6) (H . 7)))]
;       [`((,(sentence wB wA) ,(sentence wB wB) ,(sentence wA wC) ,(sentence wB wD) ,(sentence wA wC) ,wE ,wD ,wF)
;          (,(sentence wD wD wD) ,wC ,wI ,wJ ,wK ,wF)
;          (,wE ,(sentence wA wB wC wD wE)))
;        '(((A . 1) (B . 2) (C . 2) (D . 4) (C . 2) (E . 4) (D . 3) (F . 5))
;          ((G . *) (C . *) (I . *) (J . *) (K . *) (F . *))
;          ((L . *) (M . *)))])
)
      ;; -- failures
      (check-true* (lambda (s r) (for/and ([q (check-rhyme-scheme (poem "" s) r)])
                                   (quirk? q)))
       [(vector (vector wB) (vector wB) (vector wB))
        '(((X . 1)) ((X . 1)) ((X . 1)) ((X . 1)))]
;       [`((,wA) (,wB) (,wB) (,wB))
;        '(((X . 1)) ((X . 1)) ((X . 1)) ((X . 1)))]
;       [`((,wF) (,wF) (,wF) (,wF))
;        '(((X . 1)) ((X . 1)) ((X . 1)) ((X . 1)))]
;      ['()
;       '((A))]
;      [`((,wB ,(sentence wD wE))) '()]
;      [`((,wB ,(sentence wD wE))) '(())]
;      [`((,wB ,(sentence wD wE))) '(((X . *)))]
;      [`((,wB ,(sentence wD wE))) '(((A . *) (A . *)))]
;      [`((,wB ,(sentence wD wE))) '(((F . *) (U . *) (N . *)))]
;      [`((,wB ,(sentence wD wE))) '((F U N))]
;      [`((,wB ,(sentence wD wE))) '(((A . *) (B . 2)))]
;      [`((,wB ,(sentence wD wE))) '((* 2))]
;      [`((,(sentence wB wA) ,wB ,wC ,wD ,wE ,wF ,wG ,wH))
;       '(((A . 1) (B . *) (C . *) (D . *) (C . *) (E . *) (D . *) (F . *)))]
;       [`((,(sentence wB wA) ,(sentence wB wB) ,(sentence wA wC) ,(sentence wB wD) ,(sentence wA wC) ,wE ,wD ,wF)
;          (,(sentence wD wD wD) ,wC ,wI ,wJ ,wK ,wF)
;          (,wE ,(sentence wA wB wC wD wE)))
;        '(((A . 1) (B . 2) (C . 2) (D . 4) (C . 2) (E . 4) (D . 3) (F . 5))
;          ((G . *) (C . 8) (I . *) (J . *) (K . *) (F . *))
;          ((L . *) (M . *)))]
;       ;; -- wD vs. 5 syllables
;       [`((,(sentence wB wB wA) ,(sentence wC wB) ,(sentence wC))
;          (,wD ,wE)
;          (,wF ,wG ,wH))
;        '(((A . 2) (B . 3) (C . 2))
;          ((D . 5) (E . 4))
;          ((F . 5) (G . 6) (H . 7)))]
;       ;; -- wD vs. rhyme B
;       [`((,(sentence wB wB wA) ,(sentence wC wB) ,(sentence wC))
;          (,wD ,wE)
;          (,wF ,wG ,wH))
;        '(((A . 2) (B . 3) (C . 2))
;          ((B . 3) (E . 4))
;          ((F . 5) (G . 6) (H . 7)))]
;       ;; -- A vs. 2 syllables
;       [`((,wA ,(sentence wC wB) ,(sentence wC))
;          (,wD ,wE)
;          (,wF ,wG ,wH))
;        '(((A . 2) (B . 3) (C . 2))
;          ((D . 3) (E . 4))
;          ((F . 5) (G . 6) (H . 7)))])))
)))
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
  (define (test-varmap-lookup kv* k)
    (define vm (varmap-init))
    (varmap-add!* vm kv*)
    (varmap-lookup vm k))

  (check-apply* test-varmap-lookup
    ['() 'a == #f]
    ['((a . "b")) 'b == #f]
    ['((a . "b") (c . "d") (e . "f")) 'A == #f]
    ['((a . "b") (c . "d") (e . "f")) 'f == #f]
    ;; --
    ['((a . "b")) 'a == "b"]
    ['((a . "b") (car . "dee") (eee . "f")) 'eee == "f"]
    ['((lolcats . "rofl") (win . "win")) 'win == "win"]
  )

  ;; ---------------------------------------------------------------------------
  ;; -- check-num-stanzas
  (check-true* check-num-stanzas
    [(poem "" '#()) '()]
    [(poem "" '#(#("a"))) '((X))]
    [(poem "" '#(#() #() #() #())) '(() () () ())]
    [(poem "" '#(#("yo" "lo") #("wepa"))) '((X X X) (Y Z A))])

  (check-true* (lambda (a b) (quirk? (check-num-stanzas a b)))
    [(poem "" '#()) '((a))]
    [(poem "" '#(#("word"))) '()]
    [(poem "" '#(#("a") #("b"))) '((a))]
    [(poem "" '#(#("a") #("b"))) '((a) (b) (c) (d))])

  ;; -- check-num-lines
  (check-true* (lambda (a b) (check-num-lines (stanza/loc (list->vector a) 0) b))
    ['() '()]
    ['("a") '(b)]
    ['("yes I say" "yes" "Yes") '(one two three)])

  (check-true* (lambda (a b) (quirk? (check-num-lines (stanza/loc (list->vector a) 0) b)))
    ['() '(a)]
    ['("a") '()]
    ['("asdf asdf" "bbb") '(q w e r)])

  ;; -- check-stanza*
  (with-db-test
    (let ([wA5 "yeysyeysyeyseysy"]
          [wB8 "asdnandndnanadnnn"])
      (add-word/nothing wA5 5)
      (add-word/nothing wB8 8)
      (add-rhyme wA5 wA5)
      (add-rhyme wB8 wB8)
      (define (test-check-stanza*/pass stanza* rs)
        (null? (check-stanza* stanza* rs)))
      (check-true* test-check-stanza*/pass
        ['#() '()]
        [
         (vector (stanza/loc (vector (vector wA5)) 0))
         '(((* . *)))]
        [
         (vector (stanza/loc (vector (vector wA5)) 3)
                 (stanza/loc (vector (vector wA5) (vector wB8)) 4))
         '(((* . *)) ((* . *) (* . *)))]
        [
         (vector (stanza/loc (vector (vector wB8)) 5)
                 (stanza/loc (vector (vector wA5 wA5)) 6))
         '(((* . 8)) ((* . 10)))]
;        [
;         `((,(sentence wA5 wA5) ,(sentence wB8 wA5))
;           (,wB8 ,(sentence wB8 wB8 wB8) ,(sentence wA5 wB8)))
;         '(((* . 10) (A . 13))
;           ((B . 8) (B . 24) (B . 13)))]
;        [
;         `((,wB8 ,(sentence wB8 wB8 wB8 wA5))
;           (,wA5)
;           (,(sentence wA5 wA5 wA5)))
;         '(((* . 8) (* . 29))
;           ((A . 5))
;           ((A . 15)))]
;        [
;         `((,wA5 ,(sentence wA5 wA5 wB8))
;           (,(sentence wB8 wA5) ,(sentence wB8 wB8 wA5 wB8)))
;         '(((A . 5) (B . 18))
;           ((A . 13) (B . 29)))]
      )
    (define (test-check-stanza*/fail stanza rs)
      (for/and ([q (check-stanza* stanza rs)])
        (quirk? q)))
    (check-true* test-check-stanza*/fail
      ;; -- wrong syllables
      [
       (vector (stanza/loc (vector (vector wA5)) 9)
               (stanza/loc (vector (vector wA5 wB8)) 10))
       '(((* . 2)) ((* . *) (* . *)))]
;      ;; -- wrong num lines
;      [
;       `((,(sentence wA5 wA5) ,(sentence wB8 wA5))
;         (,wB8 ,(sentence wB8 wB8 wB8) ,(sentence wA5 wB8)))
;       '(((* . 10) (A . 13))
;         ((B . 8)))]
;      ;; -- wrong rhymes, A=/B
;      [
;       `((,wA5 ,(sentence wA5 wA5 wB8))
;         (,(sentence wB8 wA5) ,(sentence wB8 wB8 wA5 wB8)))
;       '(((A . 5)  (A . 18))
;         ((A . 13) (A . 29)))]
;      ;; -- wrong syllables, second line
;      [
;       `((,wA5 ,(sentence wA5 wA5 wB8))
;         (,(sentence wB8 wA5) ,(sentence wB8 wB8 wA5 wB8)))
;       '(((A . 5) (B . 18))
;         ((A . 1) (B . 9)))]
)
       ))

  ;; -- check-syllables
  (with-db-test
    (let ([w1 "avnrwfadger"]
          [w2 "qopadvzsda"]
          [w3 "avsfojnwger"])
      (for ([w (in-list (list w1 w2 w3))]
            [i (in-naturals 1)])
        (add-word/nothing w i))
      (define (test-check-syllables/pass stanza syll*)
        (null? (check-syllables stanza syll*)))
      (check-true* test-check-syllables/pass
        [(stanza/loc '#() 0) '()]
        [(stanza/loc (vector (vector w1) (vector w1) (vector w1)) 1)
         '(1 1 1)]
        [(stanza/loc (vector (vector w2) (vector w1 w1) (vector w3)) 2)
         '(2 2 *)])
      (define (test-check-syllables/fail stanza syll*)
        (for/and ([c (check-syllables stanza syll*)])
          (quirk? c)))
      (check-true* test-check-syllables/fail
        [(stanza/loc (vector (vector w2)) 4)
         '(3)]
        [(stanza/loc (vector (vector w1) (vector w2) (vector w3) (vector w2)) 5)
         '(* * * 3)])
      ;; -- invariant error
      ;(check-exn (regexp "ipoe:check-syllables:internal-error")
      ;  (lambda () (check-syllables (list w1 w1) '(*) #:stanza-number 3)))
      ;(check-exn (regexp "ipoe:check-syllables:internal-error")
      ;  (lambda () (check-syllables '() '(1 2 3) #:stanza-number 3)))
        ))

  ;; TODO test prints
  ;; -- line->syllables
  (with-db-test
    (let ([w1 "avnrwfadger"]
          [w2 "qopadvzsda"]
          [w3 "avsfojnwger"])
      (for ([w (in-list (list w1 w2 w3))]
            [i (in-naturals 1)])
        (add-word/nothing w i))
      (check-apply* line->syllables
       [(line/loc '#("") 5 1)
         == 0]
       [(line/loc (vector w1) 4 2)
         == 1]
       [(line/loc (vector w1 w1 w1) 0 1)
         == 3]
       [(line/loc (vector w1 w2) 1 2)
         == 3]
       [(line/loc (vector w3 w3 w1 w2 w2 w1 w3 w3 w3) 2 3)
         == (+ 3 3 1 2 2 1 3 3 3)]
       [(line/loc (vector ".") 4 5)
        == 0]
       [(line/loc (vector ".---.;--?!?!?!?" "5" "6") 3 3)
        == 0])
      (check-apply* line->syllables
       ;; -- unknown words have 0 syllables
       [(line/loc (vector "madeupwordnotarealword" "bladlaksdczjiewdscz") 9 9)
        == 0])))

  ;; -- replace-wildcard-syllables
  (check-apply* replace-wildcard-syllables
   ['() 3 == '()]
   ['((A A A) (B B C)) 1 == '(((A . 1) (A . 1) (A . 1)) ((B . 1) (B . 1) (C . 1)))]
   ['((1 2 3)) 7 == '((1 2 3))]
   ['(((A . 42) (B . *)) (*)) 2 == '(((A . 42) (B . 2)) ((* . 2)))]
  )

  ;; -- rhyme=?
  (with-db-test
    (let ([A1 "adsvoasdvnag"]
          [A2 "asdgawrfscvg"]
          [A* "agiruwbfae"]
          [B1 "bsviabaerarh"])
      (for ([w (in-list (list A1 A2 A* B1))])
        (add-word/nothing w 1))
      (add-rhyme A1 A2)
      (add-almost-rhyme A1 A*)
      (define (rhyme=?+ a b)
        (rhyme=? (word/loc a 1 1 1) (word/loc b 2 2 2)))
      (define (almost-rhyme=?+ a b)
        (almost-rhyme=? (word/loc a 1 1 1) (word/loc b 2 2 2)))
      (check-true* rhyme=?+
       ;; -- rhyme
       [A1 A2])
      (check-true* almost-rhyme=?+
       ;; -- almost-rhyme
       [A1 A*])
      (check-false* rhyme=?+
       [A2 A1]
       [A1 A*]
       [A1 B1])
      ;; --
      (check-exn (regexp "ipoe:db")
        (lambda () (rhyme=?+ A1 "blahblahblahblahblah")))))

  ;; -- check-rhyme
  (define-syntax-rule (check-rhyme/pass [st rs tmp] ...)
    (begin
      (let* ([vm (varmap-init)]
             [x (varmap-add!* vm tmp)]
             [result (check-rhyme st rs #:varmap vm)])
        (check-true (null? result))) ...))
  (define-syntax-rule (check-rhyme/fail [st rs kv*] ...)
    (begin
      (let* ([vm (varmap-init)]
             [x (varmap-add!* vm kv*)])
      (check-true (for/and ([x (in-list (check-rhyme st rs #:varmap vm))])
                         (quirk? x)))) ...))
  (with-db-test
    (let ([wA "asdgasfewfda"]
          [wB "gwrfbnadv"]
          [wC "qvdapiuvb"])
      (for ([w (in-list (list wA wB wC))])
        (add-word/nothing w 1)
        (add-rhyme w w))
      (let ([vm1 '((V1 . "val1") (Var2 . "val2") (M . "m"))])
        (check-rhyme/pass
          [(stanza/loc '#() 2) '() '()]
          [(stanza/loc '#() 3) '() vm1]
          [(stanza/loc (vector (vector wC) (vector wC) (vector wB)) 6)
           '(* * *) vm1]
          [(stanza/loc (vector (vector wA) (vector wB) (vector wA)) 4)
           '(A B A) '()]
          [(stanza/loc (vector (vector wC) (vector wC)) 5)
           '(X X) `((X . ,wC))])
        ;; --
        (check-rhyme/fail
          [(stanza/loc (vector (vector wA) (vector wB)) 8) '(A A) '()]
          [(stanza/loc (vector (vector wA) (vector wA) (vector wC)) 7)
           '(A B A) '()]
          [(stanza/loc (vector (vector wC) (vector wB) (vector wC)) 22)
           '(A B A) `((A . ,wA))]))))

   ;; 2015-09-26: no longer checking invariant
;  ;; Invariant: unify-rhyme-scheme expects 2nd and 3rd args to have same length
;  (check-exn (regexp "ipoe:.*:internal-error")
;    (lambda () (check-rhyme (stanza/loc '#(#()) 0) '(A) #:varmap (varmap-init))))
;  (check-exn (regexp "ipoe:.*:internal-error")
;    (lambda () (check-rhyme (stanza/loc '#(#("hello")) 8) '() #:varmap (varmap-init))))

  (with-db-test
    ;; Nothing rhymes with a fake word
    (check-exn (regexp "ipoe:db")
      (lambda ()
        (check-rhyme
          (stanza/loc '#(#("asdgwrfscbad") #("hello") #("bye")) 8)
          '(A A A)
          #:varmap (varmap-init)))))

)
