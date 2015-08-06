#lang racket/base

(provide
  spellchecker
  ;; (-> (-> String Boolean))
  ;; Thunk yielding a spellcheck function.
  ;; The spellcheck function queries the ipoe database to see if
  ;;  its input is a "real" word.

  to-lines
  ;; (-> (U Input-Port String (Listof String)) (Listof String))
  ;; Convert a data source to a list of strings where no string in
  ;;  the output contains a newline character.
  ;; Strings containing newlines are broken into multiple substrings.
)

;; -----------------------------------------------------------------------------

(require
  "private/parse.rkt"
  "private/spellcheck.rkt"
)

