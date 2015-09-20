#lang racket/base

;; Command-line interface to the DB repl

(provide
  db
  ;; (-> '() Void)
  ;; Start a fresh REPL for the database
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  (only-in racket/system system)
)

;; =============================================================================

;; TODO this is AWFUL. Should:
;; 1. require the main submodule from db.rkt
;; 2. run that submodule
;; NO RELATIVE PATHS
(define (db arg*)
  (command-line
   #:argv arg*
   #:args ()
   (system "racket ../db.rkt")))

