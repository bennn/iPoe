#lang racket/base

;; TBA
;; Maybe, tools for language builders. We'll see what we can abstract.

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "ipoe"
   #:args ANY*
   (displayln "UNDER CONSTRUCTION"))
)

;; =============================================================================

(module+ test
  (require rackunit)

)
