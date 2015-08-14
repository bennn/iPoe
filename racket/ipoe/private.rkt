#lang racket/base

(provide
  (struct-out success)
  (struct-out failure)
  ;; TODO doc

  alert
  ;; (-> String Void)
  ;; Send an alert to the user

  assert-success
  ;; (-> Either #:src Symbol Any)
  ;; Raise an exception if the argument is not a `success?`

  check-rhyme-scheme
  ;; (-> (Sequenceof (Sequenceof String)) #:rhyme-scheme (Listof (Listof Symbol)) #:src Symbol Void)
  ;; Check the rhyme scheme of the stanzas.
  ;; Use the source for error reporting.

  check-spelling
  ;; (-> (Sequenceof String) Either)
  ;; Check spelling of words in the plain-text source of a file.

  on-failure
  ;; (-> Either (-> Failure Void) Void)
  ;; Call the thunk if the argument is not a `success?` struct

  to-line*
  ;; (-> (U Input-Port String (Listof String)) (Listof String))
  ;; Convert a data source to a list of strings where no string in
  ;;  the output contains a newline character.
  ;; Strings containing newlines are broken into multiple substrings.

  to-stanza*
  ;; (-> (Sequenceof String) (Sequenceof (Sequenceof String)))
  ;; Convert a sequence of source-file lines to a grouped sequence of stanzas
  ;; A stanza is a group of consecutive lines (no double-newlines between them)
)

;; -----------------------------------------------------------------------------

(require
  "private/either.rkt"
  "private/parse.rkt"
  "private/rhymecheck.rkt"
  "private/spellcheck.rkt"
  "private/ui.rkt"
)

