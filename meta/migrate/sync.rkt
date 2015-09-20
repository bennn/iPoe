#lang racket

;; Perform database migrations in this folder (i.e., XXXX.sql files)
;; Use the file `VERSION` to remember the current version number.
;; Raise an error if version (i+1) is missing.

(require db glob)

;; =============================================================================


;; Execute a query on the database.
;; TODO query-exec gives poor error messages
;; TODO this is very inefficient for large files, I had better luck running
;;      `psql -U <user> -d <database> -a -f <file.sql>` manually.
(define (migrate pgc fname)
  (query-exec pgc (file->string fname)))

(define (main)

;; =============================================================================

(module+ main
  ;; -- connect to database
  (define pgc (postgresql-connect #:user "ben" #:database "ipoe"))
  ;; -- fold over all version files, execute new _successors_ only
  (for/and ([fname (in-list "word.sql"
                            "word_syllables.sql"
                            "word_rhymes.sql"
                            "word_almost_rhymes.sql")])
     (printf "[SYNC] executing '~a'\n" fname)
     (migrate pgc fname))
  (printf "[SYNC] successfully finished.\n"))
