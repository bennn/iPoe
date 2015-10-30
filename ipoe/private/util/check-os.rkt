#lang racket/base

;; Reflect on the current operating system.
;; This is all because the `readline` library does not work on Windows.
;; (But may be useful later, anyway)

(provide
  if-windows
  ;; (if-windows yes no)
  ;; Conditional macro.
  ;; If the current operating system is windows, execute `yes`.
  ;; Otherwise, execute `no`.
)

(require
  (for-syntax
    racket/base
    syntax/parse
))

;; =============================================================================

(define-syntax (if-windows stx)
  (syntax-parse stx
   [(_ yes no)
    (case (system-type 'os)
      [(windows) (syntax/loc stx yes)]
      [(unix macosx) (syntax/loc stx no)])]
   [_ (error 'if-windows
        (format "Expected (if-windows YES NO), got '~a'" (syntax->datum stx)))]))
