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

  replace-wildcard-syllables
  ;; (-> RhymeScheme Natural RhymeScheme)
  ;; Overwrite all wildcard syllables in the rhyme scheme

  rhyme-scheme?
  ;; (-> Any Boolean)
  ;; Predicate for rhyme schemes

  string->word*
  ;; (-> String (Listof String))
  ;; Parse a line of text into a list of normalized words.

  to-line*
  ;; (-> (U Input-Port String (Listof String)) (Listof String))
  ;; Convert a data source to a list of strings where no string in
  ;;  the output contains a newline character.
  ;; Strings containing newlines are broken into multiple substrings.

  to-stanza*
  ;; (-> (Sequenceof String) (Sequenceof (Sequenceof String)))
  ;; Convert a sequence of source-file lines to a grouped sequence of stanzas
  ;; A stanza is a group of consecutive lines (no double-newlines between them)

  user-error
  ;; (-> Symbol String Any)
  ;; Raise a user-level exception
)

;; -----------------------------------------------------------------------------

(require
  "private/either.rkt"
  "private/parse.rkt"
  "private/rhymecheck.rkt"
  "private/spellcheck.rkt"
  "private/ui.rkt"
)

