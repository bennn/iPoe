#lang racket/base

;; Simulate readline
;; Provide the same bindings that ipoe files expect readline to give,
;;  but use only windows-compliant tools.

(provide
  readline-prompt
  ;; (Parameterof String)
  ;; Print this before reading anything

  (rename-out [winread read])
  ;; (-> Any)
  ;; Show the current `readline-prompt`, then read one value from
  ;;  the `current-input-port`.
)

;; =============================================================================

(define readline-prompt (make-parameter #"> "))

(define (winread)
  (display (readline-prompt))
  (read))
