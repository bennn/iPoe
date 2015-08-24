#lang racket/base

;; Generic word/line parsing tools

(provide
  last-word
  ;; (-> String (U #f String))
  ;; Get the last parseable word from a string of text

  integer->word*
  ;; (-> Integer String)
  ;; Convert a number to an English word
  ;; Current limit is 999 trillion

  string->word*
  ;; (-> String (Listof String))
  ;; Split a string by whitespace, ensure that each result is a normalized word

  parse-word
  ;; (-> String String)
  ;; Filter unimportant things from some text.
  ;; i.e. Remove punctuation, convert to lowercase.

  to-line*
  ;; (-> (U Input-Port String (Listof String)) (Listof String))
  ;; Convert an input source to a list of newline-separated strings

  to-stanza*
  ;; (-> (Sequenceof String) (Sequenceof (Sequenceof String)))
  ;; Convert a sequence of lines to a sequence of stanzas
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/list last)
  (only-in racket/port port->lines)
  (only-in racket/string string-split string-trim)
  (only-in racket/generator in-generator generator yield)
  ipoe/private/ui
)

;; =============================================================================
;; TODO library should be lazy enough to handle Dickens

(define (last-word str)
  ;; Collect a reverse-order list of parsed words
  ;; (Basically, rev-map)
  (define word* (string->word* str))
  (and (not (null? word*))
       (last word*)))

;; Serves as a map from small naturals to their string representations
;; (: digit1-cache (Vectorof String))
(define digit1-cache
  '#("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
     "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
     "eighteen" "nineteen"))

;; (: digit2-cache (Vectorof String))
(define digit2-cache
  '#("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

;; (: digit3-cache (Vectorof String))
(define digit3-cache
  '#(#f "thousand" "million" "billion" "trillion"))

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
  ;; -- Check precondition: number can't be too big
  (define max-digits (* (vector-length digit3-cache) 3))
  (when (<= (expt 10 max-digits) n)
    (user-error 'integer->word* (format "Failed to parse ~a, cannot translate numbers with more than ~a digits" n max-digits)))
  ;; -- Recurse & build up list of 3-digit chunks
  (let loop ([d+s* '()] ;; List of 3-digit numbers and suffixes
             [n n]      ;; Current number. Starts as original, remove 3 digits from right at each iteration.
             [i 0])     ;; Index, used to choose suffixes
    (define q (quotient n 1000))
    (define r (modulo n 1000))
    (define d+s (cons r (vector-ref digit3-cache i)))
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
  (if (< 99 d3)
      (cons (digit->word (quotient d3 100))
            (cons "hundred"
                  rest))
      rest))

;; (: digit2->word* (-> Natural (Listof String)))
(define (digit2->word* n)
  (cond
   [(< n 20)
    (list (vector-ref digit1-cache n))]
   [else
    (define r (modulo n 10))
    (cons (vector-ref digit2-cache (- (quotient n 10) 2))
          (if (zero? r) '() (list (digit->word (modulo n 10)))))]))

;; (: digit->word (-> Natural String))
(define (digit->word n)
  (vector-ref digit1-cache n))

;; Convert a string to an "equivalent" string that might be in the database.
;; i.e., remove things like '?' and '!'.
;; (: parse-word (-> String String))
(define (parse-word word)
  (define str
    (apply string
           (for/list ([c (in-string word)]
                      #:when (or (char-alphabetic? c)
                                 (char-numeric? c)))
             (char-downcase c))))
  (and (not (string-empty? str)) str))

(define (string->word* str)
  (let loop ([str* (string-split str)])
    (if (null? str*)
        '()
        (let ([normalized (parse-word (car str*))]
              [rest       (lambda () (loop (cdr str*)))])
          (cond [(not normalized)
                 (rest)]
                [(string->number normalized)
                 => (lambda (n) (append (integer->word* n) (rest)))]
                [else
                 (cons normalized (rest))])))))

(define (to-line* arg)
  (cond
    [(input-port? arg)
     (port->lines arg)]
    [(string? arg)
     (string->lines arg)]
    [(list? arg)
     (list->lines arg)]
    [else
     (error 'parse:to-line* (format "Cannot coerce argument '~a' to newline-separated strings."))]))

(define-syntax-rule (maybe-yield-stanza st)
  (when (not (null? st)) (yield (reverse st))))

(define (to-stanza* line*)
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
      (loop (cdr line*) (cons (car line*) curr-stanza))]))))

;; (: string-empty? (-> String Boolean))
(define (string-empty? str)
  (zero? (string-length (string-trim str))))

(define (string->lines arg)
  (string-split arg "\n" #:trim? #f))

;; 2015-07-30: Defined only for (Listof String)
(define (list->lines arg)
  (apply append
         (for/list ([x (in-list arg)])
           (string->lines x))))

;; =============================================================================

(module+ test
  (require rackunit "rackunit-abbrevs.rkt")

  ;; -- last-word
  (check-apply* last-word
    ["a red fox" == "fox"]
    ["a" == "a"]
    ;; --
    ["A" == "a"]
    ["word." == "word"]
    ["a few words and some ..." == "some"]
    ["don't do it!" == "it"]
    ["\tdiffn't\nspaces\n" == "spaces"]
    ["521351" == "one"]
    ;; --
    ["" == #f]
    [",!--?" == #f]
    ["   \t   \t   \n" == #f]
  )

  ;; -- integer->word*
  (check-apply* integer->word*
    [10 == '("ten")]
    [10000 == '("ten" "thousand")]
    [10000000 == '("ten" "million")]
    [10000000000 == '("ten" "billion")]
    [10000000000000 == '("ten" "trillion")]
    [999000000000000 == '("nine" "hundred" "ninety" "nine" "trillion")]
    [0 == '("zero")]
    [16 == '("sixteen")]
    [-1 == '("negative" "one")]
    [123 == '("one" "hundred" "twenty" "three")]
    [8675309 == '("eight" "million" "six" "hundred" "seventy" "five" "thousand" "three" "hundred" "nine")])

  ;; --- Doesn't support words over 1 quadrillion
  (check-exn (regexp "integer->word")
             (lambda () (integer->word* 1000000000000000)))

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

  ;; -- string->word*
  (check-apply* string->word*
    ["a" == '("a")]
    ["" == '()]
    ["." == '()]
    ["hello, world" == '("hello" "world")]
    ["WHAT IS THIS" == '("what" "is" "this")]
    ["161 things!" == '("one" "hundred" "sixty" "one" "things")]
    ["non-word cruft ... gets filtered !" == '("nonword" "cruft" "gets" "filtered")]
  )

  ;; -- to-line*
  (check-apply* to-line*
    ["" == '()] ;; Racket special case.. not sure how I feel about this
    ["a\nb" == '("a" "b")]
    [" \n " == '(" " " ")]

    ['("yes") == '("yes")]
    ['("yo\nlo" "we\npa") == '("yo" "lo" "we" "pa")]
  )

  ;; -- to-stanza*
  (define-syntax-rule (check-to-stanza* [text stanza*] ...)
    (begin (check-true
             (for/and ([stanza-line* (to-stanza* (to-line* text))]
                       [line* (in-list stanza*)])
               (equal? stanza-line* line*))) ...))
  (check-to-stanza*
    ["a\na\na\n\nb\nb\nb\n"
     '(("a" "a" "a") ("b" "b" "b"))]
    [""
     '()]
    ["asdf asdf asdf"
     '(("asdf asdf asdf"))]
    ["what\n\ta great poem"
     '(("what" "\ta great poem"))]
    ["line one\n\n line two"
     '(("line one") (" line two"))]
  )

  ;; -- string-empty?
  (check-true* string-empty?
    [""]
    [" "]
    ["\t"]
    ["\n"]
    ["\r"]
    ["\t\t\t"]
    ["    "]
    ["\n\r\t"]
  )

  (check-false* string-empty?
    ["a"]
    [" hello "]
    ["why\nnot\n"]
    ["\ta\tb"]
  )

  ;; -- parse-word
  (check-apply* parse-word
    ["asdf" == "asdf"]
    ["cat61" == "cat61"]
    ["ARGH"  == "argh"]
    ["waiT?" == "wait"]
    ["don't" == "dont"]
    ["hel,p" == "help"]
    ["123" == "123"]
    ;; --
    ["" == #f]
    ["..,." == #f]
  )
)
