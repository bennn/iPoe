#lang racket/base

;; Command-line tools. For use with raco:
;;   raco ipoe COMMAND ARG ...
;; Will execute each tool with the list `ARG ...`
;;
;; Tools verify that `ARG ...` is well-formed

(provide
  check
  dbshell
  init
  new
  remove
  show
  update

  command-descriptions
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/command/check
  ipoe/private/command/dbshell
  ipoe/private/command/init
  ipoe/private/command/new
  ipoe/private/command/remove
  ipoe/private/command/show
  ipoe/private/command/update
)

;; =============================================================================

(define command-descriptions '(
  ("check" . "Check a poem for syntax errors (same as `raco make)")
  ("db" . "Open a REPL to your local iPoe database")
  ("init" . "Initialize a new database")
  ("new" . "Add a word to the database")
  ("remove" . "Remove a word from the database")
  ("show" . "List all poetic forms")
  ("update" . "Edit a word")))

