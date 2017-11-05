#lang info

(define collection "ipoe")
(define deps '(
  "base"
  "basedir"
  "db-lib"
  "html-lib"
  "html-parsing"
  "levenshtein"
  "rackunit-lib"
  "readline-lib"
  "reprovide-lang"
  "sxml"
))
(define build-deps '(
  "basedir"
  "net-doc"
  "racket-doc"
  "rackunit-abbrevs"
  "rackunit-lib"
  "scribble-doc"
  "scribble-lib"
))
(define pkg-desc "Interactive Poetry Editor")
(define pkg-authors '(ben))
(define raco-commands '(("ipoe" (submod ipoe/main main) "interactive poetry editor" #f)))
(define scribblings '(("docs/ipoe.scrbl" () (experimental))))
(define version "0.2")
