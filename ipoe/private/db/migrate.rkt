#lang racket/base

(provide
  TABLE*
  ;; Runtime paths to all .sql files containing tables
)

(require
  (for-syntax racket/base)
  (only-in racket/file file->string)
  (only-in racket/runtime-path define-runtime-path))

;; =============================================================================

(define-runtime-path WORD.sql "./migrate/word.sql")
(define-runtime-path WORD_SYLLABLES.sql "./migrate/word_syllables.sql")
(define-runtime-path WORD_RHYMES.sql "./migrate/word_rhymes.sql")
(define-runtime-path WORD_ALMOST_RHYMES.sql "./migrate/word_almost_rhymes.sql")

(define TABLE*
  (list WORD.sql WORD_SYLLABLES.sql WORD_RHYMES.sql WORD_ALMOST_RHYMES.sql))

