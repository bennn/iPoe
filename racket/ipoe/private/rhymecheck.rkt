#lang racket/base

;; 

;; -----------------------------------------------------------------------------

(provide
  assert-rhyme-scheme
  ;; (-> (Sequenceof (Listof String)) #:rhyme-scheme RhymeScheme #:src Symbol Void)
  ;; Assert that the input text matches the rhyme scheme
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/db
  ipoe/private/parse
  ipoe/private/ui
)

;; =============================================================================
;; == API functions

;; 2015-08-06: May want to return the VarMap some day
;; (: assert-rhyme-scheme (-> (Sequenceof (Listof String)) #:rhyme-scheme RhymeScheme #:src Symbol Void))
(define (assert-rhyme-scheme stanza* #:rhyme-scheme rs* #:src src)
  (define err-loc (string->symbol (format "~a:rhyme-scheme" src)))
  ;; -- preconditions
  (unless (rhyme-scheme? rs*) (raise-argument-error err-loc "Expected (Listof (Listof Symbol)) for #:rhyme-scheme" rs*))
  (assert-num-stanzas stanza* rs* #:src err-loc)
  ;; -- assert rhyme
  (with-ipoe-db (lambda ()
    (for/fold ([sym+word '()])
              ([stanza stanza*] [s+r* (in-list rs*)] [n (in-naturals)])
      (define s* (map line-scheme->syllable s+r*))
      (define r* (map line-scheme->rhyme s+r*))
      (assert-num-lines stanza s+r* #:src err-loc #:stanza-number n)
      (assert-syllables stanza s* #:src err-loc #:stanza-number n)
      (unify-rhyme-scheme sym+word stanza r* #:src err-loc #:stanza-number n)))))

;; -----------------------------------------------------------------------------
;; -- dynamic typechecking predicates

;; (define-type RhymeScheme (Listof StanzaScheme))
;; (define-type StanzaScheme (Listof LineScheme))
;; (define-type LineScheme (Pairof Rhyme Syllable))
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
  (and (pair? x)
       (rhyme? (car x))
       (syllable? (cdr x))))

(define (rhyme? x)
  (or (wildcard? x)
      (symbol? x)))

(define (syllable? x)
  (or (wildcard? x)
      (exact-nonnegative-integer? x)))

(define (wildcard? x)
  (eq? '* x))

;; Future-proofing.
;; A line spec could be done as a struct, but I really want a _convenient_
;;  read-able representation.

(define (line-scheme->rhyme ls)
  (car ls))

(define (line-scheme->syllable ls)
  (cdr ls))

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

;; (: assert-num-stanzas (-> (Sequenceof (Listof String)) RhymeScheme #:src Symbol Void))
(define (assert-num-stanzas stanza* rs* #:src src)
  (assert-length stanza* rs* "stanzas" #:src src))

;; (: assert-num-lines (-> (Sequenceof Any) (Sequenceof Any) #:stanza-number Natural #:src Symbol Void))
(define (assert-num-lines stanza rs #:stanza-number n #:src src)
  (assert-length stanza rs (format "lines in stanza ~a" n) #:src src))

;; (: assert-length (-> (Sequenceof Any) (Sequenceof Any) String #:src Symbol Void))
(define (assert-length seq-test seq-ref descr-str #:src src)
  (define len-test (for/sum ([s seq-test]) 1))
  (define len-ref  (for/sum ([r seq-ref]) 1))
  (unless (= len-test len-ref)
    (raise-user-error src
      (format "Expected ~a ~a, got ~a" len-ref descr-str len-test))))

(define (assert-syllables stanza syll* #:src src #:stanza-number n)
  ;; Precondition: len(stanza) == len(syll*)
  ;; Precondition: must be called with an active database connection
  (for ([line stanza]
        [line-no (in-naturals)]
        [s    (in-list syll*)]
        #:when (not (wildcard? s)))
    (define a (line->syllables line))
    (unless (= s a)
      (raise-user-error src (format "Expected ~a syllables in line ~a of stanza ~a, got ~a syllables" s line-no n a)))))

(define (line->syllables line)
  ;; Precondition: must be called with an active database connection
  (for/sum ([w (in-list (string->word* line))])
    (cond
      [(word->syllables w) => (lambda (x) x)]
      [else
       (alert (format "Warning: could not determine syllables for word '~a'" w))
       0])))

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
(define (unify-rhyme-scheme sym+word* stanza rs #:src src #:stanza-number n)
  (for/fold ([sym+word* sym+word*])
            ([line (in-list stanza)]
             [var  (in-list rs)]
             [line-num (in-naturals)])
    (define l (last-word line))
    (cond
     [(varmap-lookup sym+word* var)
      => (lambda (rhyme)
      ;; Match current line with existing word
      ;; Do not update varmap
      (unless (rhyme=? rhyme l)
        (raise-user-error src (format "Line ~a of stanza ~a does not match rhyme scheme. Expected a word to rhyme with '~a' but got '~a'" (add1 line-num) (add1 n) rhyme l)))
      sym+word*)]
     [else
      ;; Bind the current word to the variable
      (varmap-add sym+word* var l)])))

;; =============================================================================

(module+ test
  (require rackunit "rackunit-abbrevs.rkt")

  ;; -- rhyme-scheme?
  (check-true* rhyme-scheme?
    ['()]
    ['(())]
    ['(((A . 1)))]
    ['(((A . 1) (B . 1) (A . 0) (C . 0))
       ((B . 99) (C . 6) (DA . 1) (e . 3) (f . 1)))]
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
  )

  (check-false* line-scheme?
    ['()]
    ["not a pair"]
    ['(1 . A)]
    ['(A A)]
    ['(A 1)]
    ['("yes" . "no")]
    ['(2 . *)]
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
  )

  ;; -- line-scheme->syllable
  (check-apply* line-scheme->syllable
    ['(A . 2) == 2]
    ['(B . 0) == 0]
    ['(c . 11) == 11]
    ['(D . *) == '*]
  )

  ;; -- assert-rhyme-scheme
  (define-syntax-rule (check-rhyme-scheme/pass [st* rs*] ...)
    (begin (check-not-exn (lambda () (assert-rhyme-scheme st* #:rhyme-scheme rs* #:src 'test))) ...))
  (check-rhyme-scheme/pass
    ['() '()]
    ['(("the quick brown fox" "Jumped over the lazy dog")) '(((A . *) (B . *)))]
    ['(("anything")) '(((A . *)))]
    ['(("cat") ("rat") ("mat") ("sat")) '(((X . *)) ((X . *)) ((X . *)) ((X . *)))]
    ['(("cat") ("rat") ("mat") ("sat")) '(((X . 1)) ((X . 1)) ((X . 1)) ((X . 1)))]
    ['(("willful and raisin" "under the weather" "jet set go")
       ("domino effect" "very motivation")
       ("beside our goal" "the free bird" "gory category"))
     '(((A . 5) (B . 5) (C . 3)) ((D . 5) (E . 6)) ((F . 4) (G . 3) (H . 6)))]
    ['(("once" "upon" "a" "time" "a" "long" "slime" "ago")
       ("in" "a" "land" "full" "of" "snow")
       ("the" "end"))
     '(((A . 1) (B . 2) (C . 1) (D . 1) (C . 1) (E . 1) (D . 1) (F . 2))
       ((G . *) (C . *) (I . *) (J . *) (K . *) (F . *))
       ((L . *) (M . *)))]
  )

  (define-syntax-rule (check-rhyme-scheme/fail [st* rs*] ...)
    (begin (check-exn exn:fail? (lambda () (assert-rhyme-scheme st* #:rhyme-scheme rs* #:src 'test))) ...))
  (check-rhyme-scheme/fail
    ['() '((A))]
    ['(("never" "land")) '()]
    ['(("never" "land")) '(())]
    ['(("never" "land")) '(((X . *)))]
    ['(("never" "land")) '(((A . *) (A . *)))]
    ['(("never" "land")) '(((F . *) (U . *) (N . *)))]
    ['(("never" "land")) '(((A . *) (B . 2)))]
    ['(("once" "upon" "a" "time" "a" "long" "hour" "ago"))
     '(((A . 1) (B . *) (C . *) (D . *) (C . *) (E . *) (D . *) (F . *)))]
    ['(("once" "upon" "a" "time" "uh" "long" "slime" "ago")
       ("in" "a" "land" "full" "of" "snow")
       ("the" "end"))
     '(((A . 1) (B . 2) (C . 1) (D . 1) (C . 1) (E . 1) (D . 1) (F . 2))
       ((G . *) (C . 77) (I . *) (J . *) (K . *) (F . *))
       ((L . *) (M . *)))]
  )

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

  ;; -- assert-num-lines
  (define-syntax-rule (check-num-lines/fail [st rs] ...)
    (begin (check-exn exn:fail? (lambda () (assert-num-lines st rs #:stanza-number -1 #:src 'test))) ...))
  (check-num-lines/fail
    ['() '(a)]
    ['("a") '()]
    ['("asdf asdf" "bbb") '(q w e r)])

  (define-syntax-rule (check-num-lines/pass [st rs] ...)
    (begin (check-not-exn (lambda () (assert-num-lines st rs #:stanza-number -1 #:src 'test))) ...))
  (check-num-lines/pass
    ['() '()]
    ['("a") '(b)]
    ['("yes I say" "yes" "Yes") '(one two three)])

  ;; -- assert-num-stanzas
  (define-syntax-rule (check-num-stanzas/fail [st rs] ...)
    (begin (check-exn exn:fail? (lambda () (assert-num-stanzas st rs #:src 'test))) ...))
  (check-num-stanzas/fail
    ['() '((a))]
    ['(("word")) '()]
    ['(("a") ("b")) '((a))]
    ['(("a") ("b")) '((a) (b) (c) (d))])

  (define-syntax-rule (check-num-stanzas/pass [st rs] ...)
    (begin (check-not-exn (lambda () (assert-num-stanzas st rs #:src 'test))) ...))
  (check-num-stanzas/pass
    ['() '()]
    ['(("a")) '((X))]
    ['(() () () ()) '(() () () ())]
    ['(("yo" "lo") ("wepa")) '((X X X) (Y Z A))])

  ;; -- assert-length
  (define-syntax-rule (check-length/fail [x* y*] ...)
    (begin (check-exn exn:fail? (lambda () (assert-length x* y* "test" #:src 'test))) ...))
  (check-length/fail
    ['(a b c) '()]
    ['() '(a b c)]
    ["blah" "b"])

  (define-syntax-rule (check-length/pass [x* y*] ...)
    (begin (check-not-exn (lambda () (assert-length x* y* "test" #:src 'test))) ...))
  (check-length/pass
    ['() '()]
    ['(1 2 3) '(1 2 3)]
    ["blah" "blah"]
    ['(() () 3) '(() 513423123 ("yes" "Yes"))])

  ;; -- unify
  (define-syntax-rule (check-unify/pass [vm st rs == vm2] ...)
    (begin (check-equal? (unify-rhyme-scheme vm st rs #:src 'test #:stanza-number 0) vm2) ...))
  (check-unify/pass
    ['() '() '() == '()]
    ['((V1 . "val1") (Var2 . "val2") (M . "m")) '() '() == '((V1 . "val1") (Var2 . "val2") (M . "m"))]
    ;; TODO fix contract error
;    ['() '("cat" "dog" "rat") '(A B A) == '((B . "dog") (A . "cat"))]
;    ['((X . "role")) '("mole" "sole") '(X X) == '((X . "role"))]
    ;; TODO more
  )

  (define-syntax-rule (check-unify/fail [vm st rs] ...)
    (begin (check-exn exn:fail? (lambda () (unify-rhyme-scheme vm st rs #:src 'test #:stanza-number 2))) ...))
  (check-unify/fail
;    ['() '() '(A)] ;; TODO no exception raised
    ['((A . "worm")) '("cat" "dog" "rat") '(A B A)]
    ['() '("a" "bacon") '(A A)]
    ['() '("harpoon" "moon" "will") '(A B A)]
    ;; TODO
  )

;; -- assert-syllables
;; -- line->syllables

)