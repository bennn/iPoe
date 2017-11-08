#lang racket/base

(provide
  log-ipoe-db-fatal
  log-ipoe-db-error
  log-ipoe-db-warning
  log-ipoe-db-info
  log-ipoe-db-debug

  log-ipoe-scrape-fatal
  log-ipoe-scrape-error
  log-ipoe-scrape-warning
  log-ipoe-scrape-info
  log-ipoe-scrape-debug
)

(define-logger ipoe-db)
(define-logger ipoe-scrape)

