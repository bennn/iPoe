#lang racket/base

(provide
  ;; -- for the #lang users
  (rename-out
    [read/free-verse read]
    [read-syntax/free-verse read-syntax])
  ;; -- for library users
  free-verse
)

;; =============================================================================

(require
  ipoe ;;bg TODO prefer to say 'ipoe/private'
  syntax/strip-context
  (only-in racket/string string-split string-join)
)

;; =============================================================================

(define spellcheck (spellchecker))

;; Good name convention?
;; I like the idea of calling `(free-verse "text")` after importing the same
;;  library, but it doesn't make it clear that this is a validator.
;; Maybe `free-verse?` would be better;
;;  maybe this'll be easier to figure out after I know all the things to provide
(define (free-verse arg)
  (define line* (to-lines arg))
  (for* ([line (in-list line*)]
         [word (in-list (string-split line))])
    (unless (spellcheck word) (printf "Warning: misspelled word '~a'\n" word)))
  (string-join line* "\n"))

(define (read/free-verse in)
  (syntax->datum (read-syntax/free-verse #f in)))

(define (read-syntax/free-verse src-path in)
  ;; spellcheck lines
  (with-syntax ([str (free-verse in)])
    (strip-context
       #`(module anything racket
          (provide data)
          (define data 'str)))))

(define (make-info key default use-default)
  ;; Can dispatch on symbol `key` to do things like loading Dr.Racket extensions.
  (use-default key default))
