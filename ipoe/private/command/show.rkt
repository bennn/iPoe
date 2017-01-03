#lang racket/base

;; List information about installed poems

(provide
  show
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
)

;; =============================================================================

(define (show arg*)
  (command-line
   #:argv arg*
   #:args ()
   (printf "Built-in poetic forms:~n")
   (for ([pf (in-list (get-forms))])
     (display "  ipoe/")
     (display pf)
     (newline))
   (void)))

(define (get-forms)
  (define-values [base _main _dir?] (split-path (collection-file-path "main.rkt" "ipoe")))
  (for/list ([form (in-list (directory-list base))]
             #:when (directory-exists? (build-path base form "lang")))
    form))

