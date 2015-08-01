#lang racket

;; Perform database migrations in this folder (i.e., XXXX.sql files)
;; Use the file `VERSION` to remember the current version number.
;; Raise an error if version (i+1) is missing.

(require db glob)

;; =============================================================================

(define VERSION "./version.rktd")

;; Read current version number from the file
(define (get-current-version)
  (file->value VERSION))

;; Update the current version number
(define (set-current-version n)
  (with-output-to-file VERSION #:exists 'replace
    (lambda () (displayln n))))

;; Convert an `XXXX.sql` filename to a number.
;; By convention, these filenames are 4-digit integers padded with zeros to the left.
(define (filename->number fname)
  (call-with-exception-handler
    (lambda (err) (raise-user-error 'sync (format "Could not parse number from filename '~a'" fname)))
    (lambda () (string->number (car (string-split (cadr (string-split fname "/")) "."))))))

;; Execute a query on the database.
;; TODO query-exec gives poor error messages
;; TODO this is very inefficient for large files, I had better luck running
;;      `psql -U <user> -d <database> -a -f <file.sql>` manually.
(define (migrate pgc fname)
  (query-exec pgc (file->string fname)))

(define (main)
  ;; -- get current version, connect to database
  (define init-version (get-current-version))
  (define pgc (postgresql-connect #:user "ben" #:database "ipoe"))
  ;; -- fold over all version files, execute new _successors_ only
  (define new-version
    (for/fold ([version init-version])
              ([fname (in-glob "./*.sql")])
      (define i (filename->number fname))
      (define v+1 (add1 version))
      (cond
        [(<= i version)
         ;(printf "[SYNC] skipping old file '~a'\n" fname)
         version]
        [(= i v+1)
         (printf "[SYNC] executing '~a'\n" fname)
         (migrate pgc fname)
         (set-current-version v+1)
         v+1]
        [else
         (raise-user-error 'sync (format "missing version '~a'" (add1 version)))])))
  (printf "[SYNC] successfully performed ~a updates\n" (- new-version init-version))
  (set-current-version new-version))

;; =============================================================================

(module+ main
  (main))

;; =============================================================================

(module+ test
  (require rackunit)

  (check-equal? (filename->number "./001.sql") 1)
  (check-equal? (filename->number "./912") 912)
  (check-equal? (filename->number "./0200.sql") 200)
)
