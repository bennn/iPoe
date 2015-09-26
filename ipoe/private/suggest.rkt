#lang racket/base

;; Utilities for spelling / rhyming suggestions.

(provide
  filter-similar
  ;; (->* [String (Sequenceof String)] [#:limit Natural #:max-distance Natural] (Listof String))
  ;; (filter-similar w w* #:limit N #:max-distance M)
  ;; Return the top `N` words from the sequence `w*` with respect to their
  ;;  levenshtein distance from `w`.
  ;; No results will have distance greater than M.

  suggest-spelling
  ;; (-> String (Listof String))
  ;; Return a list of similar, correctly-spelled words
)

(require
  ipoe/private/suggest/spelling
)
