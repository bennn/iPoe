#lang racket/base

;; 

(provide
  assert-rhyme-scheme
  ;; (-> (Sequenceof (Listof String)) #:rhyme-scheme RhymeScheme #:src Symbol Void)
  ;; Assert that the input text matches the rhyme scheme
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/parse
  ipoe/private/db
)

;; =============================================================================
;; --- Types and Parameters
;; (Parameters should probably be in a new file)

;; Current database connection
(define *pgc* (make-parameter #f))

;; (define-type VarMap (Listof (Pairof Symbol String)))
;; (define-type RhymeScheme (Listof (Listof Symbol)))

;; -----------------------------------------------------------------------------

;; 2015-08-06: May want to return the VarMap some day
;; (: assert-rhyme-scheme (-> (Sequenceof (Listof String)) #:rhyme-scheme RhymeScheme #:src Symbol Void))
(define (assert-rhyme-scheme stanza* #:rhyme-scheme rs* #:src src)
  (define err-loc (string->symbol (format "~a:rhyme-scheme" src)))
  ;; -- preconditions
  (unless (rhyme-scheme? rs*) (raise-argument-error err-loc "Expected (Listof (Listof Symbol)) for #:rhyme-scheme" rs*))
  (assert-num-stanzas stanza* rs* #:src err-loc)
  ;; -- assert rhyme
  (for/fold ([sym+word '()])
            ([stanza stanza*] [rs (in-list rs*)] [n (in-naturals)])
    (unify-rhyme-scheme sym+word stanza rs #:stanza-number n #:src err-loc)))

;; A rhyme scheme is a (Listof (Listof Symbol))
;; (: rhyme-scheme? (-> Any Boolean))
(define (rhyme-scheme? x**)
  (and (list? x**)
       (for/and ([x* (in-list x**)])
         (and (list? x*)
              (for/and ([x (in-list x*)])
                (symbol? x))))))

;; (: assert-num-stanzas (-> (Sequenceof (Listof String)) RhymeScheme #:src Symbol Void))
(define (assert-num-stanzas stanza* rs* #:src src)
  (assert-length stanza* rs* "stanzas" #:src src))

;; (: assert-num-lines (-> (Sequenceof String) (Sequenceof Symbol) #:stanza-number Natural #:src Symbol Void))
(define (assert-num-lines stanza rs #:stanza-number n #:src src)
  (assert-length stanza rs (format "lines in stanza ~a" n) #:src src))

;; (: assert-length (-> (Sequenceof Any) (Sequenceof Any) String #:src Symbol Void))
(define (assert-length seq-test seq-ref message #:src src)
  (define len-test (for/sum ([s seq-test]) 1))
  (define len-ref  (for/sum ([r seq-ref]) 1))
  (unless (= len-test len-ref)
    (raise-user-error src
      (format "Expected ~a ~a, got ~a" len-ref message len-test))))

;; True if two words rhyme in the context of this poem
;; For now, allowing both rhyme and almost-rhyme
;; (: rhyme=? (-> String String Boolean))
(define (rhyme=? w1 w2)
  (unless (*pgc*)
    (*pgc* (db-init)))
  (or (rhymes-with? (*pgc*) w1 w2)
      (almost-rhymes-with? (*pgc*) w1 w2)))

;; Unify a stanza of poetry with a rhyme scheme
;; (: unify-rhyme-scheme (-> VarMap (Listof String) (Listof Symbol) #:src Symbol #:stanza-number Natural VarMap))
(define (unify-rhyme-scheme sym+word* stanza rs #:src src #:stanza-number n)
  (assert-num-lines stanza rs #:stanza-number n #:src src)
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
        (raise-user-error src (format "Line ~a of stanza ~a does not match rhyme scheme. Expected a word to rhyme with '~a' but got '~a'" line-num n rhyme l)))
      sym+word*)]
     [else
      ;; Bind the current word to the variable
      (varmap-add sym+word* var l)])))

;; (: varmap-lookup (-> VarMap Symbol (U #f String)))
(define (varmap-lookup sym+word* sym)
  (for/first ([sym+word (in-list sym+word*)]
              #:when (eq? sym (car sym+word)))
    (cdr sym+word)))

;; (: varmap-add (-> VarMap Symbol String VarMap))
(define (varmap-add sym+word* sym word)
  (cons (cons sym word) sym+word*))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- assert-rhyme-scheme
  (define-syntax-rule (check-rhyme-scheme/pass [st* rs*] ...)
    (begin (check-not-exn (lambda () (assert-rhyme-scheme st* #:rhyme-scheme rs* #:src 'test))) ...))
  (check-rhyme-scheme/pass
    ['() '()]
    ['(("the quick brown fox" "Jumped over the lazy dog")) '((A B))]
    ;; TODO more tests
  )

  (define-syntax-rule (check-rhyme-scheme/fail [st* rs*] ...)
    (begin (check-exn exn:fail? (lambda () (assert-rhyme-scheme st* #:rhyme-scheme rs* #:src 'test))) ...))
  (check-rhyme-scheme/fail
    ['() '((A))]
    ['(("never" "land")) '((A A))]
    ;; TODO more tests
  )

  ;; -- rhyme-scheme?
  (define-syntax-rule (check-rhyme-scheme?/true [rs ...])
    (begin (check-true (rhyme-scheme? rs)) ...))
  (check-rhyme-scheme?/true
    ['()
     '(())
     '((A))
     '((A B A C) (B C DA e f))])

  (define-syntax-rule (check-rhyme-scheme?/false [rs ...])
    (begin (check-false (rhyme-scheme? rs)) ...))
  (check-rhyme-scheme?/false
    ['("yolo")
     '(4)
     '(("A"))
     68
     'free])

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
    ['() '("cat" "dog" "rat") '(A B A) == '((B . "dog") (A . "cat"))]
    ['((X . "role")) '("mole" "sole") '(X X) == '((X . "role"))]
    ;; TODO more
  )

  (define-syntax-rule (check-unify/fail [vm st rs] ...)
    (begin (check-exn exn:fail? (lambda () (unify-rhyme-scheme vm st rs #:src 'test #:stanza-number 2))) ...))
  (check-unify/fail
    ['() '() '(A)]
    ['((A . "worm")) '("cat" "dog" "rat") '(A B A)]
    ['() '("a" "bacon") '(A A)]
    ['() '("harpoon" "moon" "will") '(A B A)]
    ;; TODO
  )

  ;; -- varmap-lookup
  (define-syntax-rule (check-varmap-lookup [vm sym == val] ...)
    (begin (check-equal? (varmap-lookup vm sym) val) ...))
  (check-varmap-lookup
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
)
