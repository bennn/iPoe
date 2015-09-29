#lang at-exp racket

(provide (all-defined-out))

(require scriblib/autobib)

;; =============================================================================

(define-cite ~cite citet generate-bibliography)

(define essay-on-man
  (make-bib
   #:author "Alexander Pope"
   #:title "An Essay on Man: Epistle I"
   #:location "http://www.poetryfoundation.org/poem/174165"
   #:is-book? #t
   #:date "1734"
   #:note "Accessed 2015-09-29"))


