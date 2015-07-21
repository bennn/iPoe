#lang racket/base

;; A very simple spellchecker

(provide
  ;; (-> String Boolean)
  spellcheck)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/file file->value))

;; =============================================================================

(define (spellcheck word)
  (member word WORDS))

(define WORDS (file->value "/home/ben/code/ipoe/racket/ipoe/data/common-words.rktd"))
