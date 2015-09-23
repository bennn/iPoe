#lang racket/base

(require racket/contract/base)

;; (: SCALE (Vectorof String))
(define SCALE '#(END "thousand" "million" "billion" "trillion"))

(define UPPER-BOUND (sub1 (expt 10 (* (vector-length SCALE) 3))))

(provide (contract-out
  [integer->word* (-> (integer-in (- UPPER-BOUND) UPPER-BOUND) (listof string?))]
  ;; Convert a number to an English word
))

;; -----------------------------------------------------------------------------

(require
  (only-in racket/match
    match-define)
)

;; =============================================================================

;; Serves as a map from small naturals to their string representations
;; (: N<20 (Vectorof String))
(define N<20
  '#("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
     "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
     "eighteen" "nineteen"))

;; (: TENS>10 (Vectorof String))
(define TENS>10
  '#("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

;; Convert a two-digit number to a list of lowercase words
(define (digit2->word* n)
  (cond
   [(< n 20)
    (list (vector-ref N<20 n))]
   [else
    (define q (quotient n 10))
    (define r (modulo n 10))
    (define ten-str (vector-ref TENS>10 (- q 2)))
    (define one-str (and (not (zero? r)) (vector-ref N<20 r)))
    (if one-str
        (list ten-str one-str)
        (list ten-str))]))

;; Split a natural number into 3-digit chunks
(define (natural->natural* N)
  (let loop ([acc '()]
             [n N]  ;; Starts as original & we remove 3 digits each step.
             [i 0]) ;; Index used to pick a scale
    (define q (quotient n 1000))
    (define r (modulo n 1000))
    (cond
     [(= n r)
      ;; Reached fixpoint, stop iteration
      (cons r acc)]
     [else
      ;; Repeat using the quotient
      (loop (cons r acc) q (add1 i))])))

;; Break a natural into chunks, attach a scale to each chunk
(define (natural->scaled* n)
  (define (add-scale n acc+i)
    (match-define (cons acc i) acc+i)
    (define s (vector-ref SCALE i))
    (define n+s (cons n s))
    (cons (cons n+s acc) (add1 i)))
  (car (foldr add-scale (cons '() 0) (natural->natural* n))))

(define (integer->word* N)
  ;; Break N into chunks, convert each chunk+scale to a string
  (define str*
    (for/list ([n+s (in-list (natural->scaled* (abs N)))])
      (match-define (cons n s) n+s)
      (define q (quotient n 100))
      (define r (modulo n 100))
      (define n-str*
        (cond
         [(zero? n)
          '()]
         [(= 100 n)
          (list "one hundred")]
         [(< 100 n)
          (define hd (vector-ref N<20 q))
          (define tl (digit2->word* r))
          (list* hd "hundred" tl)]
         [else
          (digit2->word* r)]))
      ;; Don't print a scale for zeros or the last chunk
      (if (or (eq? s 'END) (zero? n))
          n-str*
          (append n-str* (list s)))))
  (cond ;; Check for special cases
   [(zero? N)
    (list "zero")]
   [(negative? N)
    (cons "negative" (apply append str*))]
   [else
    (apply append str*)]))

;; =============================================================================

(module+ test

  (require rackunit ipoe/private/util/rackunit-abbrevs)

  (check-true
    (and (member "trillion" (integer->word* UPPER-BOUND))
         #t))

  (check-true
    (and (member "trillion" (integer->word* (- UPPER-BOUND)))
         #t))

  ;; -- integer->word*
  (check-apply* integer->word*
   [10 == '("ten")]
   [100 == '("one hundred")]
   [10000 == '("ten" "thousand")]
   [10000000 == '("ten" "million")]
   [10000000000 == '("ten" "billion")]
   [10000000000000 == '("ten" "trillion")]
   [999000000000000 == '("nine" "hundred" "ninety" "nine" "trillion")]
   [0 == '("zero")]
   [16 == '("sixteen")]
   [999 == '("nine" "hundred" "ninety" "nine")]
   [-1 == '("negative" "one")]
   [22 == '("twenty" "two")]
   [123 == '("one" "hundred" "twenty" "three")]
   [22 == '("twenty" "two")]
   [14 == '("fourteen")]
   [50 == '("fifty")]
   [98 == '("ninety" "eight")]
   [12345 == '("twelve" "thousand" "three" "hundred" "forty" "five")])
;    [10 == '("ten")]
;    [10000 == '("ten" "thousand")]
;    [10000000 == '("ten" "million")]
;    [10000000000 == '("ten" "billion")]
;    [10000000000000 == '("ten" "trillion")]
;    [999000000000000 == '("nine" "hundred" "ninety" "nine" "trillion")]
;    [999000000000000 == '("nine" "hundred" "ninety" "nine" "trillion")]
;    [0 == '("zero")]
;    [16 == '("sixteen")]
;    [-1 == '("negative" "one")]
;    [123 == '("one" "hundred" "twenty" "three")]
;    [22 == '("twenty" "two")]
;    [14 == '("fourteen")]
;    [50 == '("fifty")]
;    [98 == '("ninety" "eight")]
;    [100 == '("one" "hundred")]
;    [120 == '("one" "hundred" "twenty")]
;    [1002 == '("one" "thousand" "two")]
;    [1323 == '("one" "thousand" "three" "hundred" "twenty" "three")]
;    [8675309 == '("eight" "million" "six" "hundred" "seventy" "five" "thousand" "three" "hundred" "nine")])

  ;; -- natural->scaled*
  (check-apply* natural->scaled*
    [3222 == '((3 . "thousand") (222 . END))]
    [901003004111 == '((901 . "billion") (3 . "million")
                       (4 . "thousand") (111 . END))]
    [999 == '((999 . END))]
    [21 == '((21 . END))]
    [19 == '((19 . END))]
    [100 == '((100 . END))]
    [123 == '((123 . END))]
    [1234567890 == '((1 . "billion") (234 . "million")
                     (567 . "thousand") (890 . END))])

  ;; -- natural->natural*
  (check-apply* natural->natural*
    [1234567890 == '(1 234 567 890)]
    [1000000890 == '(1   0   0 890)]
    [22 == '(22)]
    [3222 == '(3 222)]
    [1000231 == '(1 0 231)]
    [0 == '(0)]
    [1 == '(1)]
    [22 == '(22)]
    [3222 == '(3 222)]
    [1000231 == '(1 0 231)]
    [18201661181166 == '(18 201 661 181 166)])

  ;; -- digit2->word*
  (check-apply* digit2->word*
    [2 == '("two")]
    [99 == '("ninety" "nine")]
    [71 == '("seventy" "one")]
    [10 == '("ten")]
    [33 == '("thirty" "three")]
    [40 == '("forty")]
    [0 == '("zero")]
    [2 == '("two")]
    [14 == '("fourteen")]
    [50 == '("fifty")]
    [98 == '("ninety" "eight")]
    [99 == '("ninety" "nine")])

)
