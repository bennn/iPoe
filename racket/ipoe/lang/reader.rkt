#lang racket/base

;; Look for #syllables and rhyme-scheme
;; Expand into a module reader that checks the poetry

(provide
  (rename-out
    [ipoe-read read]
    [ipoe-read-syntax read-syntax])
  ;#%app #%datum quote
  ;;; Enough from racket/base to create lists

  ;(rename-out [ipoe-begin #%module-begin])
  ;;; The reader.
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private
  racket/match
  racket/syntax
  syntax/strip-context
)

;; =============================================================================

(struct poem-spec (
  name ;; Symbol
  rhyme-scheme ;; RhymeScheme
  extra-validator ;; (-> (Listof (Listof String)) Either)
) #:transparent )
;; (define-type PoemSpec poem-spec)

;; -----------------------------------------------------------------------------

;; Unless the first argument is #f, raise a helpful exception.
;; (: check-duplicate Any #:new-val Any #:src Symbol #:msg String Void)
(define (check-duplicate old-val #:new-val v #:src src #:msg msg)
  (when old-val
    (user-error src (format "Parsed two ~as: '~a' and '~a'" old-val v))))

;; Read an input file as a poem specification
;; (: ipoe-input->data (-> Input-Port PoemSpec))
(define (input->poem-spec in)
  (define err-loc 'ipoe:parse)
  ;; Loop & manually parse the keywords and line
  ;; (There's so little to parse we might as well do a good job manually)
  (let loop ([name #f] [rhyme-scheme #f] [extra-validator #f])
    ;; Read one datum & dispatch on it
    (match (read in)
     [(? eof-object?)
      #:when (and name rhyme-scheme)
      ;; This is a GOOD end-of-file
      (poem-spec name rhyme-scheme extra-validator)]
     [(? eof-object?)
      ;; A bad end of file. Missing some data.
      (user-error err-loc (format "Unexpected end-of-file, missing ~a"
        (cond [(and (not name) (not rhyme-scheme)) "name & rhyme scheme"]
              [(not name) "name"]
              [else "rhyme-scheme"])))]
     ;; -- Keywords
     ['#:name
      ;; Keyword for poem name
      (define s (read-keyword-value in symbol?
                                    #:kw '#:name #:src err-loc))
      (check-duplicate name #:new-val s #:src err-loc #:msg "poem name")
      (loop s rhyme-scheme extra-validator)]
     ['#:rhyme-scheme
      ;; Keyword for rhyme scheme
      (define rs (read-keyword-value in rhyme-scheme?
                                     #:kw '#:rhyme-scheme #:src err-loc))
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs extra-validator)]
     ['#:extra-validator
      ;;
      (define ev (read-keyword-value in procedure?
                                     #:kw '#:extra-validator #:src err-loc))
      (check-duplicate extra-validator #:new-val ev #:src err-loc #:msg "extra validator")
      (loop name rhyme-scheme ev)]
     ;; -- Infer data from predicates
     [(? symbol? s)
      (check-duplicate name #:new-val s #:src err-loc #:msg "poem name")
      (loop s rhyme-scheme extra-validator)]
     [(? rhyme-scheme? rs)
      (check-duplicate rhyme-scheme #:new-val rs #:src err-loc #:msg "rhyme scheme")
      (loop name rs extra-validator)]
     [(? procedure? ev)
      (check-duplicate extra-validator #:new-val ev #:src err-loc #:msg "extra validator")
      (loop name rhyme-scheme ev)]
     ;; --
     [x (user-error err-loc (format "Unknown value ~a in poem specification" x))]
     )))

(define (ipoe-read in)
  (syntax->datum (ipoe-read-syntax #f in)))

(define (ipoe-read-syntax src-path in)
  (define ps (input->poem-spec in))
  (with-syntax ([name     (format-id #f "~a" (poem-spec-name ps))]
                [validate (poem-spec->validator ps)])
    (strip-context
      #'(module name racket/base
          (provide (rename-out [custom-read read] [custom-read-syntax read-syntax]))
          (require syntax/strip-context) ;; 2015-08-15: Really necessary?
          (define (custom-read in) (syntax->datum (custom-read-syntax #f in)))
          (define (custom-read-syntax src-path in)
            (with-syntax ([str (validate in)])
              (strip-context #'(module anything racket (provide data) (define data 'str)))))))))

;; (: read-keyword-value (-> Input-Port (-> Any Boolean) #:kw Symbol Any))
(define (read-keyword-value in p? #:kw sym #:src err-loc)
  (when (eof-object? in)
    (user-error err-loc (format "Unmatched keyword '~a'" sym)))
  (define raw (read in))
  (if (p? raw)
      raw
      (user-error err-loc (format "Expected a '~a' to follow the keyword '~a', got ~a" (object-name p?) sym raw))))

(define (poem-spec->validator ps)
  ;; If rhyme scheme is empty, do not check
  ;; (Special case for "free verse")
  (define rs (poem-spec-rhyme-scheme ps))
  (define check-rhyme
    (if (null? rs)
        (lambda (line*) (void))
        (lambda (line*) (assert-success #:src (poem-spec-name ps)
                          (check-rhyme-scheme (to-stanza* line*) #:rhyme-scheme rs)))))
  (lambda (in)
    (define line* (to-line* in))
    (check-rhyme line*)
    (check-spelling line*)
    line*))
