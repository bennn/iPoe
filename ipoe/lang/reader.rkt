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
  ipoe/private/poem/form
  (only-in racket/syntax format-id)
)

;; =============================================================================

(define (ipoe-read in)
  (syntax->datum (ipoe-read-syntax #f in)))

(define (ipoe-read-syntax src-path in)
   (define ps (make-form in))
   (with-syntax ([mod-id   (format-id #f "~a" (form-name ps))]
                 [descr    (form-description ps)]
                 [validate (form->validator ps)]
                 [default-requires  validator-requires])
     (strip-context
       #'(module mod-id racket/base
           (provide (rename-out [custom-read read]
                                [custom-read-syntax read-syntax]))
           default-requires
           (define (custom-read in) (syntax->datum (custom-read-syntax #f in)))
           (define (custom-read-syntax src-path in)
             (with-syntax ([str (validate in)])
               (strip-context #'(module anything racket
                                  (provide text description)
                                  (define description descr)
                                  (define text 'str)))))))))

;; =============================================================================

