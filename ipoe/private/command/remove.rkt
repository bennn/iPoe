#lang racket/base

;; Uniform interface for
;; - Removing words
;; - (TODO) Removing poem forms

;; TODO accept input from file?

(provide
  remove
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  ipoe/private/command/common
  ipoe/private/db
  (only-in ipoe/private/ui alert)
  ipoe/private/parameters
)

;; =============================================================================
;; === API

(define (remove arg*)
  (command-line
   #:argv arg*
   #:args (word-or-form)
   (cond
    [(rkt-file? word-or-form)
     (displayln "REMOVE FORM not implemented")]
    [else
     (define w word-or-form)
     (parameterize-from-hash (options-init)
       (lambda ()
         (parameterize ([*interactive?* #t])
           (with-ipoe-db #:commit? #t
                         #:user (*user*)
                         #:dbname (*dbname*)
             (lambda ()
               (cond
                [(not (word-exists? w))
                 (alert (format "Word '~a' does not exist, cannot remove." w))]
                [(remove-word w)
                 (alert (format "Successfully removed word '~a'" w))]
                [else
                 (alert (format "Failed to remove word '~a'" w))]))))))])))

;; -----------------------------------------------------------------------------

