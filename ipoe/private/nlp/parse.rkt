#lang racket/base

;; Generic word/line parsing tools
;; Design goal: all functions from this module have type (-> String (U #f Word*))

;; The type 'Word' represents normalized strings
;; i.e. strings that might possibly be in the word database
;;
;; 2015-09-23: It's currently an undefined concept, but
;;  roughly only [a-z] strings are words.
;;  We may eventually include:
;;  - capitalization (unlikely, see https://github.com/bennn/iPoe/issues/6)
;;  - hyphens (likely, see https://github.com/bennn/iPoe/issues/5)
;;  - other punctuation (like the apostrope in "farfetch'd")

;; (define-type Word* (Listof Word))
(provide
  number->word*
  ;; (-> String (U Word* #f))
  ;; Parse

  ;parse-phone-number ;; TODO and so on...
  ;;; (-> String (U Word* #f))

  string->word*
  ;; (-> String Word*)
  ;; Filter unimportant things from some text.
  ;; i.e. Remove punctuation, convert to lowercase.
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/nlp/number
  ipoe/private/util/string
  ipoe/private/ui
  ;; --
  racket/match
  (only-in racket/string
    string-split
    string-trim)
)

;; =============================================================================

(define (number->word* str)
  ;; TODO catch contract-error, return a failure struct
  (let ([n (string->number (string-trim str))])
    (and n
         (integer? n)
         (with-handlers ([exn:fail:contract? (lambda (e) #f)])
           (integer->word* n)))))

(define (string->word* str)
  (let loop ([str* (string->token* str)])
    (match str*
     ['()  '()]
     [(cons hd tl)
      (define w (parse-word* hd))
      (append w (loop tl))])))

;; -----------------------------------------------------------------------------
;; --- helpers

;; 2015-09-23: Eventually, will want to recognize punctuation here
(define string->token* string-split)

;; Get the words from a string
(define (parse-word* str)
  (or (number->word* str)
      ;; 2015-10-04: register new parsers here
      (let ([w (string-normalize-downcase str)])
        (cond
         [(string-empty? w)
          ;(alert (format "Cannot parse string '~a', ignoring it." w))
          '()]
         [else (list w)]))))

;; =============================================================================

(module+ test
  (require
    rackunit
    ipoe/private/util/rackunit-abbrevs)

  ;; -- number->word*
  (check-apply* number->word*
   ["0" == '("zero")]
   ["-432600" == '("negative" "four" "hundred" "thirty" "two" "thousand" "six" "hundred")]
   ["999001002001089" == '("nine" "hundred" "ninety" "nine" "trillion" "one" "billion" "two" "million" "one" "thousand" "eighty" "nine")])

  ;; -- number->word* failures, we hide the contract error
  (check-false* number->word*
   ["yolo"]
   ["1/3"]
   ["5.00"]
   ["456789456789345678345678"]
   ["-98765432345678908765444"])

  ;; -- string->word*
  (check-apply* string->word*
   ["I counted 30 sheep" == '("i" "counted" "thirty" "sheep")]
   ["don't look at HIM that way!" == '("dont" "look" "at" "him" "that" "way")]
   ["The b42 is hot." == '("the" "b" "is" "hot")])

  ;; -- string->token*
  (check-apply* string->token*
   ["" == '()]
   ["      \t   \n   " == '()]
   ["foo bar baz" == '("foo" "bar" "baz")]
   ["what're we gonn'a do?" == '("what're" "we" "gonn'a" "do?")]
   ["HEY you!" == '("HEY" "you!")])

  ;; -- parse-word*
  (check-apply* parse-word*
   ["asdf" == '("asdf")]
   ["cat61" == '("cat")]
   ["ARGH"  == '("argh")]
   ["waiT?" == '("wait")]
   ["don't" == '("dont")]
   ["hel,p" == '("help")]
   ["61" == '("sixty" "one")]
   ["123400" == '("one" "hundred" "twenty" "three" "thousand" "four" "hundred")])
  (define-syntax-rule (check-parse-error* str ...)
    (begin
      (check-equal?
        (parse-word* str)
        '()) ...))

  (check-parse-error*
    ""
    "...,."
    "!@!")
)
