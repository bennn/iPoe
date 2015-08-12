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
  (assert-rhyme-scheme stanza* #:rhyme-scheme '(((* . 5) (* . 7) (* . 5)))
                               #:src 'haiku)
  (check-spelling line*)
  (string-join line* "\n"))

(define (read/couplet in)
  (syntax->datum (read-syntax/haiku #f in)))

(define (read-syntax/haiku src-path in)
  (with-syntax ([str (couplet in)])
    (strip-context
      #`(module anything racket
          (provide data)
          (define data 'str)))))

(define (make-info key default use-default)
  (use-default key default))

