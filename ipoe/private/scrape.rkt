#lang racket/base

;; For validating new words, rhymes, and syllables via the internet.
;; TODO should this file be typed/racket/base, instead of contract/base ?

(require racket/contract/base)
(provide ;; TODO these are not great, but necessary for the cache
  (struct-out rhyme-result)
  (struct-out word-result)
)
(provide (contract-out
  [rhymes? (-> rhyme-result? string? boolean?)]
  ;; True if the string is one rhyme in the struct

  [almost-rhymes? (-> rhyme-result? string? boolean?)]
  ;; True if the string is one almost-rhyme in the struct

  [scrape-rhyme (-> string? rhyme-result?)]
  ;; Collect rhymes and almost-rhymes for a word

  [scrape-word (-> string? (or/c #f word-result?))]
  ;; Collect word data from the internet
))

(require
  ipoe/private/scrape/scrape-rhymes
  ipoe/private/scrape/scrape-words
)
