#lang racket/base

(provide
  db-init
  ;; (-> connection?)
  ;; Connect to the ipoe database, return the psql connection

  find-word
  ;; (-> connection? string? TODO)
  ;; Search the database for the supplied word

  word->id
  ;; (-> connection? string? natural?)
  ;; Get the id of the word, if it exists
)

(require
  db/base
  db/postgresql
  racket/match)

;; =============================================================================

(define-syntax-rule (word-error loc msg arg* ...)
  (error (string->symbol (string-append "ipoe:db:" (symbol->string loc)))
         (format msg arg* ...)))

(define-syntax-rule (duplicate-word w)
  ;; Should never happen, so raise an error
  (error 'ipoe:db "WARNING: word '~a' is not unique in the database"))

;; -----------------------------------------------------------------------------

(define (db-init)
  (postgresql-connect #:user "ben" #:database "ipoe"))

(define (find-word pgc word)
  (rows-result-rows (query pgc "SELECT * FROM word WHERE word.word=$1" word)))

(define (word->id pgc word)
  (define rows (find-word pgc word))
  (match rows
    ['()
     #f]
     ;(word-error 'word->id "Word '~a' does not exist" word)]
    [(cons v rest)
     (unless (eq? '() rest)
       (duplicate-word word))
     (vector-ref v 0)]))
