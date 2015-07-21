#lang racket/base

(provide
  (rename-out
    [read/free-verse read]
    [read-syntax/free-verse read-syntax])
)

;; =============================================================================

(require
  ipoe ;;bg TODO prefer to say 'ipoe/private'
  syntax/strip-context
  (only-in racket/port port->lines)
  (only-in racket/string string-split string-join)
)

;; =============================================================================
;; TODO read vs. read-syntax

(define (read/free-verse in)
  (syntax->datum (read-syntax/free-verse #f in)))

(define (read-syntax/free-verse src in)
  (define line* (port->lines in))
  ;; spellcheck lines
  (for* ([line (in-list line*)]
        [word (in-list (string-split line))])
    (unless (spellcheck word) (printf "Warning: misspelled word '~a'\n" word)))
  (with-syntax ([str (string-join line* "\n")])
    (strip-context
      (syntax/loc src (module anything racket
          (provide data)
          (define data 'str))))))

(define (make-info key default use-default)
  ;; Can dispatch on symbol `key` to do things like loading Dr.Racket extensions.
  (use-default key default))
