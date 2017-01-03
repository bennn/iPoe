#lang racket/base

;; List information about installed poems

;; Will probably work by reading the folder names in the cloned directory
;;  and filtering those without a ./lang/reader file.
;; See https://github.com/bennn/iPoe/issues/54

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

