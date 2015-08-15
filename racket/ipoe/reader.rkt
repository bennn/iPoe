#lang racket/base

;; Look for #syllables and rhyme-scheme
;; Expand into a module reader that checks the poetry

(provide
  #%app #%datum quote
  ;; Enough from racket/base to create lists

  (rename-out [ipoe-begin #%module-begin])
  ;; The reader.
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
  syntax/strip-context
  (for-syntax
    racket/base
    syntax/parse)
)

;; =============================================================================

(define-syntax-rule (ipoe-begin rhyme-scheme)
  (#%module-begin
    (provide
      (rename-out [custom-read read] [custom-read-syntax read-syntax]))

    (define (custom-read in)
      (syntax->datum (custom-read-syntax #f in)))

    (define (custom-read-syntax src-path in)
      (with-syntax ([str (validate in)])
        (strip-context
          #'(module anything racket
              (provide data)
              (define data 'str)))))

    ;; Parse data from the input port as a poem
    ;; (-> Input-Port String)
    (define validate (rhyme-scheme->validator rhyme-scheme))
  ))

(define (rhyme-scheme->validator rs)
  (unless (rhyme-scheme? rs)
    (user-error 'ipoe (format "ipoe s-exp reader expects a rhyme scheme, got '~a'" rs)))
  ;; If rhyme scheme is empty, do not check
  ;; (Special case for "free verse")
  (define check-rhyme
    (if (null? rs)
        (lambda (line*) (void))
        (lambda (line*) (assert-success #:src 'idk
                          (check-rhyme-scheme (to-stanza* line*) #:rhyme-scheme rs)))))
  (lambda (in)
    (define line* (to-line* in))
    (check-rhyme line*)
    (check-spelling line*)
    line*))
