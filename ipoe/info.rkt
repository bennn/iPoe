#lang info

(define collection 'use-pkg-name)
(define deps
  '("base" ;; v6.3
    "rackunit-lib"
    "sxml"
    "levenshtein"))
(define build-deps
  '("scribble-lib"
    "racket-doc"))
(define pkg-desc "Family of languages for editing poetry")
(define pkg-authors '(ben))
(define raco-commands '(("ipoe" (submod ipoe/main main) "console UI" #f)))
;(define scribblings '(("scribblings/ipoe.scrbl")))
(define version "0.1")
