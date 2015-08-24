#lang racket/base

;; Utilities for spelling / rhyming suggestions.

(provide
  suggest-spelling
  ;; (-> String (Listof String))
  ;; Return a list of similar, correctly-spelled words
)

(require
  ipoe/private/suggest/spelling
)
