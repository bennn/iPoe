#lang racket/base

;; Utilities for spelling / rhyming suggestions.

(provide
  filter-similar
  ;; (->* [String (Sequenceof String)] [#:limit Natural] (Listof String))
  ;; (filter-similar w w* #:limit N)
  ;; Return the top `N` words from the sequence `w*` with respect to their
  ;;  levenshtein distance from `w`.

  suggest-spelling
  ;; (-> String (Listof String))
  ;; Return a list of similar, correctly-spelled words
)

(require
  ipoe/private/suggest/spelling
)
