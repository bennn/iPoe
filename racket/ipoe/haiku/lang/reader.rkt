#lang racket/base

(provide
  (rename-out
    [read/haiku read]
    [read-syntax/haiku read-syntax])
  haiku)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
  syntax/strip-context
  (only-in racket/string string-join)
  (only-in racket/sequence sequence->list)
)

;; =============================================================================

(define (haiku arg)
  (define line* (to-line* arg))
  (define stanza* (sequence->list (to-stanza* line*)))
  ;; Fail if rhyme scheme doesn't match
  (assert-success #:src 'haiku
    (check-rhyme-scheme stanza* #:rhyme-scheme '(((* . 5) (* . 7) (* . 5)))))
  ;; Post a warning if spelling fails
  (on-failure (check-spelling line*)
    (lambda (fl) (alert (failure-reason fl))))
  (string-join line* "\n"))

(define (read/haiku in)
  (syntax->datum (read-syntax/haiku #f in)))

(define (read-syntax/haiku src-path in)
  (with-syntax ([str (haiku in)])
    (strip-context
      #`(module anything racket
          (provide data)
          (define data 'str)))))

(define (make-info key default use-default)
  (use-default key default))

