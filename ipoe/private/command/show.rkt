#lang racket/base

;; List information about installed poems

;; Will probably work by reading the folder names in the cloned directory
;;  and filtering those without a ./lang/reader file.

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
   (displayln "SHOW not implemented")))

