#lang racket/base

(provide
  resolve-rhyme*
  resolve-syllables
  ;; --
  rhyme-result
  rhyme-result?
  rhyme-result-rhyme*
  rhyme-result-almost-rhyme*
)

(require
  "scrape/scrape-rhymes.rkt"
  "scrape/scrape-syllables.rkt"
)
