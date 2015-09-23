#lang racket/base

;; For validating new words, rhymes, and syllables via the internet.

;; TODO I do not like these struct-out
(provide
  scrape-word
  ;; --
  (struct-out rhyme-result)
  rhymes?
  almost-rhymes?
  scrape-rhyme
  ;; --
  (struct-out word-result)
)

(require
  ipoe/private/scrape/scrape-rhymes
  ipoe/private/scrape/scrape-words
)
