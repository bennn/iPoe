#lang racket/base

;; Command-line tools. For use with raco:
;;   raco ipoe COMMAND ARG ...
;; Will execute each tool with the list `ARG ...`
;; (Tools will verify that `ARG ...` is well-formed for their purposes)

(provide
  check
  ;; Check a sequence of poems.
  ;; This is a shortcut for calling `racket FILE.rkt` on each poem individually

  db
  ;; Open a REPL session with the database

  init
  ;; Initialize a new database.
  ;; A guided setup.

  new
  ;; Create a new poem form, or interactively add a new word to the database

  remove
  ;; Delete poem forms or words

  show
  ;; Display information on installed poem forms
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/command/check
  ipoe/private/command/db
  ipoe/private/command/init
  ipoe/private/command/new
  ipoe/private/command/remove
  ipoe/private/command/show
)