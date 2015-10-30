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
   (define F (make-form in))
   (with-syntax ([mod-id   (format-id #f "~a" (form-name F))]
                 [descr    (form-description F)]
                 [validate (form->validator F)]
                 [default-requires  validator-requires])
     (strip-context
       #'(module mod-id racket/base
           (provide (rename-out [custom-read read]
                                [custom-read-syntax read-syntax]))
           default-requires
           (define (custom-read in)
             (syntax->datum (custom-read-syntax #f in)))
           (define (custom-read-syntax src-path in)
             (with-syntax ([(P L O*) (validate in)]
                           [mod-id2 (gensym 'mod-id)])
               (strip-context
                 #'(module mod-id2 racket
                     ;; Set up a basic interactions environment
                     (require
                       (only-in ipoe/private/parameters parameterize-from-hash)
                       (only-in ipoe/private/command/dbshell dbshell))
                     (define (connect)
                       (parameterize-from-hash O* dbshell))
                     (define help
                      "Type '(connect)' to open a connection to the iPoe database")
                     (define description descr)
                     ;; Print a message in Dr.Racket
                     (module+ test (displayln help))))))))))


