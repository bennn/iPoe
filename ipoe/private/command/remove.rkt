#lang racket/base

;; TBA
;; Uniform interface for
;; - Removing poem forms
;; - Removing words

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
     (remove-form word-or-form)]
    [else
     (remove-word word-or-form)])))

;; -----------------------------------------------------------------------------

;; Assumes that `fname` is a valid (existing) Racket file
(define (remove-form fname)
  (displayln "REMOVE FORM not implemented"))

(define (remove-word w)
  ;; Init database
  ;; Check if word exists
  ;; Interactively add word (get syllables, get rhymes)
  (parameterize-from-hash (options-init)
    (lambda ()
      (with-ipoe-db #:commit? #t
                    #:user (*user*)
                    #:dbname (*dbname*)
                    #:interactive? #t
        (lambda ()
          (cond
           [(not (word-exists? w))
            (alert (format "Word '~a' does not exist, cannot remove." w))]
           [(remove-word w)
            (alert (format "Successfully removed word '~a'" w))]
           [else
            (alert (format "Failed to remove word '~a'" w))]))))))

;; -----------------------------------------------------------------------------

