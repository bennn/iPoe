#lang racket/base

;; Look for #syllables and rhyme-scheme
;; Expand into a module reader that checks the poetry

(provide
  ipoe-read
  ipoe-read-syntax
  ;#%app #%datum quote
  ;;; Enough from racket/base to create lists

  ;(rename-out [ipoe-begin #%module-begin])
  ;;; The reader.
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

(struct poem-spec (
  name ;; String
  rhyme-scheme ;; RhymeScheme
) #:transparent )
;; (define-type PoemSpec poem-spec)

;; -----------------------------------------------------------------------------

;; Read an input file as a poem specification
;; TODO think of a nice, user-friendly format
;; (: ipoe-input->data (-> Input-Port PoemSpec))
(define (ipoe-input->data in)
  (define raw (read in))
  (unless (pair? raw)
    (user-error 'ipoe (format "Expected a pair of (name . rhyme-scheme), got ~a." raw)))
  (define name (car raw))
  (define rs   (cdr raw))
  (unless (string? name)
    (user-error 'ipoe (format "Expected a string poem name, got '~a'" name)))
  (unless (rhyme-scheme? rs)
    (user-error 'ipoe (format "Expected a rhyme scheme, got ~a" rs)))
  (poem-spec name rs))

(define (ipoe-read in)
  (syntax->datum (ipoe-read-syntax #f in)))

(define (ipoe-read-syntax in)
  (define ps (ipoe-input->data in)) ;; TODO get fancier
  (with-syntax (
                [name (format-id #f "~a" TODO)]
                [validate ()])
    (strip-context
      #'(module name racket/base
          (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]))
          (define (custom-read in) (syntax->datum (custom-read-syntax #f in)))
          (define (custom-read-syntax src-path in)
            (with-syntax ([str (validate in)])
              (strip-context #'(module anything racket (provide data) (define data 'str))))))))

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
