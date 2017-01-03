#lang racket/base

;; English-language inference algorithms

(provide
  infer-rhyme=?
  ;; (-> String String Boolean)
  ;; Guess whether two words rhyme

  infer-syllables
  ;; (-> String Natural)
  ;; Guess the number of syllables in a word
)

;; -----------------------------------------------------------------------------

(require
  racket/string
  ipoe/private/util/string
)

;; =============================================================================

;; Naively count syllables (doesn't require an internet connection)
(define (naive-syllables word)
  (define vowels '(#\a #\e #\i #\o #\u #\y))
  (string-count-chars (lambda (c) (member c vowels)) word))

(define (naive-rhyme=? w1 w2)
  (define L1 (string-length w1))
  (define L2 (string-length w2))
  (and (positive? L1)
       (positive? L2)
       (char=? (string-ref w1 (sub1 L1))
               (string-ref w2 (sub1 L2)))))

;; -----------------------------------------------------------------------------

(define infer-rhyme=? naive-rhyme=?)

(define infer-syllables naive-syllables)

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  ;; -- naive-syllables
  (check-apply* naive-syllables
    ["foobar" == 3]
    ["" == 0]
    ["eek" == 2]
    ["hour" == 2]
    ["what" == 1]
    ["balloon" == 3]
    ["wombat" == 2]
    ["arcade" == 3]
    ["apvuhinjets" == 4]
    ["bat" == 1]
    ["computer" == 3])

  ;; -- naive-rhyme=?
  (check-true*  naive-rhyme=?
   ["yes" "stress"]
   ["yes" "squirrels"]
   ["man" "can"]
   ["dance" "suave"]
   ["a" "a"]
   ["gee" "e"])

  (check-false*  naive-rhyme=?
   ["a" "b"]
   ["cowa" "bungoo"]
   ["month" ""]
   ["" "foo"]
   ["" ""]
   ["iewvwdaeg" "vzfvaeweq"])


)
