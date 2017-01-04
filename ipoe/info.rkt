#lang info

(define collection "ipoe")
(define deps
  '("base"
    "db-lib"
    "html-parsing"
    "levenshtein"
    "html-lib"
    "rackunit-lib"
    "readline-lib"
    "reprovide-lang"
    "sxml"
   ))
(define build-deps
  '("scribble-lib"
    "net-doc"
    "scribble-doc"
    "rackunit-lib"
    "rackunit-abbrevs"
    "racket-doc"))
(define pkg-desc "Interactive Poetry Editor")
(define pkg-authors '(ben))
(define raco-commands '(("ipoe" (submod ipoe/main main) "interactive poetry editor" #f)))
(define scribblings '(("docs/ipoe.scrbl" () (experimental))))
(define version "0.2")
