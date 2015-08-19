#lang racket/base

;; User-end library, helpers for specifying #:extra-validator functions
;;  in a poem spec.

(provide
  contains-word?
  ;; (-> (Listof String) String Boolean)
  ;; True if the line contains the word

  last-word
  ;; (-> (Listof String) String)
  ;; Return the final word in the line

  line
  ;; (-> Natural (Listof String) String)
  ;; Get a line from a stanza.
  ;; (Basically, a synonym for `list-ref`)

  line=?
  ;; (-> String String Boolean)
  ;; Returns #t if the two lines contain the same words.
  ;; i.e., are equal after removing punctuation.

  stanza
  ;; (-> Natural (Listof (Listof String)) String)
  ;; Get a stanza from a poem.
  ;; (Synonym for `list-ref`)

  word=?
  ;; (-> String String Boolean)
  ;; True if two words are equal
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
)

;; =============================================================================
;; === API

;; (: contains-word? (-> (Listof String) String Boolean))
(define (contains-word? line w-param)
  (define w1 (parse-word w-param))
  (and w1
      (for/or ([w2 (in-list (string->word* line))] #:when w2)
        (string=? w1 w2))))

;; (: line (-> Natural (Listof String) String))
(define (line n l*)
  (safe-list-ref n l* 'line))

;; (: stanza (-> Natural (Listof (Listof String)) String))
(define (stanza n s*)
  (safe-list-ref n s* 'stanza))

;; (: line=? (-> String String * Boolean))
(define (line=? line . line*)
  (let loop ([w1* (string->word* line)] [w2** (map string->word* line*)])
    (cond
      [(and (null? w1*) (andmap null? w2**))
       ;; Base case: empty lines are equal
       #t]
      [(or (null? w1*) (ormap null? w2**))
       ;; False because lines have different numbers of words
       #f]
      [else
       (and (apply string=? (cons (car w1*) (map car w2**)))
            (loop (cdr w1*) (map cdr w2**)))])))

(define (word=? w1-param . w*-param)
  (define w1 (parse-word w1-param))
  (and w1
    (for/and ([w2-param (in-list w*-param)])
      (define w2 (parse-word w2-param))
      (and w2 (string=? w1 w2)))))

;; -----------------------------------------------------------------------------
;; --- private

(define (safe-list-ref goal-index x* [elem-type 'element] #:curr-index [ci 0])
  (cond
   [(null? x*)
    (user-error 'ipoe:safe-list-ref (format "Cannot access ~a ~a, sequence only has ~a ~as" elem-type goal-index ci elem-type))]
   [(= goal-index ci)
    (car x*)]
   [else
    (safe-list-ref goal-index (cdr x*) elem-type #:curr-index (add1 ci))]))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- contains-word?
  (check-true (contains-word? "I could not stop for death." "i"))
  (check-true (contains-word? "I could not stop for death." "stop"))
  (check-true (contains-word? "I could not stop for death." "death!"))

  (check-false (contains-word? "the raging: milkman" "a"))
  (check-false (contains-word? "" "hey"))
  (check-false (contains-word? "" ""))

  ;; -- line
  (check-equal? (line 0 '(a)) 'a)
  (check-equal? (line 1 '(a b c)) 'b)
  (check-equal? (line 5 '(a b c d e f)) 'f)

  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (line 0 '())))
  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (line -1 '(a b c))))

  ;; -- line=?
  (check-true (line=? ""
                      ""))
  (check-true (line=? "shall I compare thee"
                      "shall, I ComparE Thee!"))
  (check-true (line=? "YES"
                      "yes."))
  (check-true (line=? "it's not a typo"
                      "its not a typo"))
  ;; --- should do multiple arguments
  (check-true (line=? "a" "a" "A" "a"))

  (check-false (line=? "A"
                       "B"))
  (check-false (line=? "just a minute"
                       "justaminute"))
  (check-false (line=? "12 black birds"
                       "13 black birds"))

  ;; -- stanza
  (check-equal? (stanza 0 '(a)) 'a)
  (check-equal? (stanza 1 '(a b c)) 'b)
  (check-equal? (stanza 5 '(a b c d e f)) 'f)

  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (stanza 0 '())))
  (check-exn (regexp "ipoe:safe-list-ref")
             (lambda () (stanza -1 '(a b c))))

  ;; -- word=?
  (check-true (word=? "YES" "yes"))
  (check-true (word=? "a" "a"))
  (check-true (word=? "can't" "cant"))
  (check-true (word=? "ab" "Ab" "aB," "A.B."))

  (check-false (word=? "a" "B"))
  (check-false (word=? "" ""))
  (check-false (word=? "a" "b" "c" "d"))
)
