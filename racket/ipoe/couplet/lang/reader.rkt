#lang racket/base

(provide
  ;; -- for the #lang users
  (rename-out
    [read/couplet read]
    [read-syntax/couplet read-syntax])
  ;; -- for library users
  couplet
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
  syntax/strip-context
  (only-in racket/string string-join)
  (only-in racket/sequence sequence->list)
)

;; =============================================================================

(define (couplet arg)
  (define line* (to-line* arg))
  (define stanza* (sequence->list (to-stanza* line*)))
  ;; -- Expecting 1 stanza of two rhyming lines
  (assert-success #:src 'couplet
    (check-rhyme-scheme stanza* #:rhyme-scheme '(((A . *) (A . *)))))
  ;; -- Spellcheck the original lines
  (on-failure (check-spelling line*)
    (lambda (fl) (alert (failure-reason fl))))
  (string-join line* "\n"))

(define (read/couplet in)
  (syntax->datum (read-syntax/couplet #f in)))

(define (read-syntax/couplet src-path in)
  (with-syntax ([str (couplet in)])
    (strip-context
       #`(module anything racket
          (provide data)
          (define data 'str)))))

(define (make-info key default use-default)
  ;; Can dispatch on symbol `key` to do things like loading Dr.Racket extensions
  (use-default key default))
