#lang racket/base

(provide
  db-init
  ;; (-> connection?)
  ;; Connect to the ipoe database, return the psql connection

  find-word
  ;; (-> connection? string? TODO)
  ;; Search the database for the supplied word
)

(require
  db/base
  db/postgresql)

;; =============================================================================

(define (db-init)
  (postgresql-connect #:user "ben" #:database "ipoe"))

(define (find-word pgc word)
  (rows-result-rows (query pgc "SELECT * FROM word WHERE word.word=$1" word)))

