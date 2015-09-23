#lang racket/base

(require racket/contract/base)

;; (: SCALE (Vectorof String))
(define SCALE
  '#(#f "thousand" "million" "billion" "trillion"))

(define UPPER-BOUND (sub1 (expt 10 (* (vector-length SCALE) 3))))

(provide (contract-out
  [integer->word* (-> (integer-in (- UPPER-BOUND) UPPER-BOUND) (listof string?))]
  ;; Convert a number to an English word
))

;; -----------------------------------------------------------------------------

;; =============================================================================

;; Serves as a map from small naturals to their string representations
;; (: N<20 (Vectorof String))
(define N<20
  '#("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
     "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
     "eighteen" "nineteen"))

;; (: TENS<100 (Vectorof String))
(define TENS<100
  '#("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))


;; Convert an integer to an English word
;; (: integer->word* (-> Integer (Listof String)))
(define (integer->word* n)
  (if (negative? n)
      (cons "negative" (natural->word* (- 0 n)))
      (natural->word* n)))

;; (: natural->word* (-> Natural (Listof String)))
(define (natural->word* n [suffix ""])
  (apply append (for/list ([d3+s (in-list (split-number n))])
                  (append (digit3->word* (car d3+s))
                          (if (cdr d3+s) (list (cdr d3+s)) '())))))

;; Divide a number into chunks of at most 3 digits.
;; Each chunk comes with a descriptor, for example
;; (split-number 1001) => '((1 . "thousand") (1 . ""))
;; (: split-number (-> Natural (Listof (Pairof Natural String))))
(define (split-number n)
  ;; -- Recurse & build up list of 3-digit chunks
  (let loop ([d+s* '()] ;; List of 3-digit numbers and suffixes
             [n n]      ;; Current number. Starts as original, remove 3 digits from right at each iteration.
             [i 0])     ;; Index, used to choose suffixes
    (define q (quotient n 1000))
    (define r (modulo n 1000))
    (define d+s (cons r (vector-ref SCALE i)))
    (cond
     [(= n r)
      ;; Reached fixpoint, return number with current suffix
      (cons d+s d+s*)]
     [else
      ;; Loop. Ignore the new tuple if it represents a 0.
      (define new-d+s* (if (zero? r) d+s* (cons d+s d+s*)))
      (loop new-d+s* q (add1 i))])))

;; Convert a number with fewer than 3 digits to a word
;; (: digit3->word* (-> Natural (Listof String)))
(define (digit3->word* d3)
  (define rest (digit2->word* (modulo d3 100)))
  (cond
   [(= 100 d3)
    '("one" "hundred")]
   [(< 99 d3)
    (cons (digit->word (quotient d3 100))
          (cons "hundred"
                rest))]
   [else
    rest]))

;; (: digit2->word* (-> Natural (Listof String)))
(define (digit2->word* n)
  (cond
   [(< n 20)
    (list (vector-ref N<20 n))]
   [else
    (define r (modulo n 10))
    (cons (vector-ref TENS<100 (- (quotient n 10) 2))
          (if (zero? r) '() (list (digit->word r))))]))

;; (: digit->word (-> Natural String))
(define (digit->word n)
  (vector-ref N<20 n))

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
    [10000 == '("ten" "thousand")]
    [10000000 == '("ten" "million")]
    [10000000000 == '("ten" "billion")]
    [10000000000000 == '("ten" "trillion")]
    [999000000000000 == '("nine" "hundred" "ninety" "nine" "trillion")]
    [999000000000000 == '("nine" "hundred" "ninety" "nine" "trillion")]
    [0 == '("zero")]
    [16 == '("sixteen")]
    [-1 == '("negative" "one")]
    [123 == '("one" "hundred" "twenty" "three")]
    [22 == '("twenty" "two")]
    [14 == '("fourteen")]
    [50 == '("fifty")]
    [98 == '("ninety" "eight")]
    [100 == '("one" "hundred")]
    [120 == '("one" "hundred" "twenty")]
    [1002 == '("one" "thousand" "two")]
    [1323 == '("one" "thousand" "three" "hundred" "twenty" "three")]
    [8675309 == '("eight" "million" "six" "hundred" "seventy" "five" "thousand" "three" "hundred" "nine")])

  ;; -- natural->word
  (check-apply* natural->word*
    [901003004111 == '("nine" "hundred" "one" "billion" "three" "million" "four" "thousand" "one" "hundred" "eleven")]
    [121314 == '("one" "hundred" "twenty" "one" "thousand" "three" "hundred" "fourteen")]
    [0 == '("zero")]
    [860 == '("eight" "hundred" "sixty")]
    [19 == '("nineteen")])

  ;; -- split-number
  (check-apply* split-number
    [0 == '((0 . #f))]
    [1 == '((1 . #f))]
    [22 == '((22 . #f))]
    [3222 == '((3 . "thousand") (222 . #f))]
    [1000231 == '((1 . "million") (231 . #f))]
    [18201661181166 == '((18 . "trillion") (201 . "billion") (661 . "million") (181 . "thousand") (166 . #f))])

  ;; -- digit3->word*
  (check-apply* digit3->word*
    [0 == '("zero")]
    [999 == '("nine" "hundred" "ninety" "nine")]
    [21 == '("twenty" "one")]
    [19 == '("nineteen")]
    [100 == '("one" "hundred")]
    [123 == '("one" "hundred" "twenty" "three")]
    [666 == '("six" "hundred" "sixty" "six")])

  ;; -- digit2->word*
  (check-apply* digit2->word*
    [2 == '("two")]
    [99 == '("ninety" "nine")]
    [71 == '("seventy" "one")]
    [10 == '("ten")]
    [33 == '("thirty" "three")]
    [40 == '("forty")])

  ;; -- digit->word
  (check-apply* digit->word
    [0 == "zero"]
    [1 == "one"]
    [2 == "two"]
    [3 == "three"]
    [4 == "four"]
    [5 == "five"]
    [6 == "six"]
    [7 == "seven"]
    [8 == "eight"]
    [9 == "nine"])
)
