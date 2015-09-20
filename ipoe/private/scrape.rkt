#lang racket/base

;; For validating new words, rhymes, and syllables via the internet.

(provide
  scrape-word
  resolve-rhyme*
  resolve-syllables
  ;; --
  rhyme-result
  rhyme-result?
  rhyme-result-rhyme*
  rhyme-result-almost-rhyme*
  rhymes?
  almost-rhymes?
  scrape-rhyme
  ;; --
  word-result
  word-result?
  word-result-num-syllables
)

(require
  "scrape/scrape-rhymes.rkt"
  "scrape/scrape-syllables.rkt"
  "scrape/scrape-words.rkt"
)
