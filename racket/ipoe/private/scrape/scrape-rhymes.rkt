#lang racket/base

(provide
  (struct-out rhyme-result)

  resolve-rhyme*
  ;; (-> String (U #f (Listof String)) (Listof String))
  ;; Validate a suggested list of rhymes for a word

  resolve-almost-rhyme*
  ;; (-> String (U #f (Listof String)) (Listof String))
  ;; Validate a suggested list of almost-rhymes for a word

  ;scrape-rhymes
  ;; (-> String rhyme-result)
  ;; Search 
)

;; =============================================================================

(struct rhyme-result (
  rhyme*        ; (Listof String)
  almost-rhyme* ; (Listof String)
) #:transparent)

(define (resolve-rhyme* word rhyme*-param)
  (error 'scrape-rhymes:rhyme "Not implemented"))

(define (resolve-almost-rhyme* word almost-rhyme*-param)
  (error 'scrape-rhymes:almost-rhyme "Not implemented"))

