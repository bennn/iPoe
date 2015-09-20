#lang racket/base

(provide
  init
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  ;ipoe/private/parameters
)

;; =============================================================================

(define (init arg*)
  ;; - Check if postgres is installed & server running
  ;; - Check for a init files, an existing database
  ;;   (use params.rkt to scrape)
  ;; - Communicate lots
  (command-line
   #:argv arg*
   #:once-each
   ;; [("-u" "--username")
   ;;  u-param "Username for new database" (*new-user* u-param)]
   ;; [("-d" "--database")
   ;;  d-param "Name of new database" (*new-dbname* d-param)]
   #:args ()
   (begin
     (displayln "INIT not implemented"))))

;; =============================================================================

(module+ test
  (require rackunit)

)
