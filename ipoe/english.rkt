#lang racket/base

(provide
  infer-rhyme=?
  infer-syllables
  integer->word*
  integer->word/space
  suggest-spelling
)

(require
  (only-in racket/string string-join)
  ipoe/private/nlp/infer
  ipoe/private/nlp/number
  ipoe/private/suggest/spelling)

;; =============================================================================

(define (integer->word/space i)
  (string-join (integer->word* i) " "))
