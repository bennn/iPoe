#lang racket/base

;; Look for #syllables and rhyme-scheme
;; Expand into a module reader that checks the poetry

(provide
  (rename-out
    [ipoe-read read]
    [ipoe-read-syntax read-syntax])
)

;; -----------------------------------------------------------------------------

(require
  syntax/strip-context
  racket/splicing
  ipoe/lang/poem-spec
  (only-in racket/syntax format-id)
)

;; =============================================================================

(define (ipoe-read in)
  (syntax->datum (ipoe-read-syntax #f in)))

(define (ipoe-read-syntax src-path in)
   (define ps (input->poem-spec in))
   (with-syntax ([mod-id   (format-id #f "~a" (poem-spec-name ps))]
                 [descr    (poem-spec-description ps)]
                 [validate (poem-spec->validator ps)]
                 [def-req  validator-requires])
     (strip-context
       #'(module mod-id racket/base
           (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]))
           def-req
           (require
             ipoe/private/parameters
             ipoe/private/either
             (only-in ipoe/private/ui alert)
             (only-in ipoe/private/db with-ipoe-db add-word* ipoe-db-connected?)
             (only-in ipoe/private/spellcheck check-spelling)
             (only-in syntax/strip-context strip-context))
           (define (custom-read in) (syntax->datum (custom-read-syntax #f in)))
           (define (custom-read-syntax src-path in)
             (with-syntax ([str (validate in)])
               (strip-context #'(module anything racket
                                  (provide text description)
                                  (define description descr)
                                  (define text 'str)))))))))

;; =============================================================================

