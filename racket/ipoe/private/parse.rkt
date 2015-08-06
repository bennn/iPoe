#lang racket/base

;; Generic parsing tools

(provide
  to-line*
  ;; (-> (U Input-Port String (Listof String)) (Listof String))
  ;; Convert an input source to a list of newline-separated strings

  to-stanza*
  ;; (-> (Sequenceof String) (Sequenceof (Sequenceof String)))
  ;; Convert a sequence of lines to a sequence of stanzas
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/port port->lines)
  (only-in racket/string string-split string-trim)
  (only-in racket/generator in-generator generator yield)
)

;; =============================================================================
;; TODO library should be lazy enough to handle Dickens

(define (to-line* arg)
  (cond
    [(input-port? arg)
     (port->lines arg)]
    [(string? arg)
     (string->lines arg)]
    [(list? arg)
     (list->lines arg)]
    [else
     (error 'parse:to-line* (format "Cannot coerce argument '~a' to newline-separated strings."))]))

(define-syntax-rule (maybe-yield-stanza st)
  (when (not (null? st)) (yield (reverse st))))

;; TODO test
(define (to-stanza* line*)
  (in-generator (let loop ([line* line*] [curr-stanza '()])
    (cond
     [(null? line*)
      ;; End of input. Yield current stanza if anything's left.
      (maybe-yield-stanza curr-stanza)]
     [(string-empty? (car line*))
      ;; Hit an empty line. Yield the current stanza, if any.
      (maybe-yield-stanza curr-stanza)
      (loop (cdr line*) '())]
     [else
      ;; Advance `line*` and add to `curr-stanza`
      (loop (cdr line*) (cons (car line*) curr-stanza))]))))

;; TODO test
(define (string-empty? str)
  (zero? (string-length (string-trim str))))

(define (string->lines arg)
  (string-split arg "\n" #:trim? #f))

;; 2015-07-30: Defined only for (Listof String)
(define (list->lines arg)
  (apply append
         (for/list ([x (in-list arg)])
           (string->lines x))))

;; =============================================================================

(module+ test
  (require rackunit)

  ;; -- to-line*
  (define-syntax-rule (check-to-line* [in out] ...)
    (begin (check-equal? (to-line* in) out) ...))
  (check-to-line*
    ["" '()] ;; Racket special case.. not sure how I feel about this
    ["a\nb" '("a" "b")]
    [" \n " '(" " " ")]

    ['("yes") '("yes")]
    ['("yo\nlo" "we\npa") '("yo" "lo" "we" "pa")]
  )

  ;; -- to-stanza*
  (define-syntax-rule (check-to-stanza* [text stanza*] ...)
    (begin (check-true
             (for/and ([stanza-line* (to-stanza* (to-line* text))]
                       [line* (in-list stanza*)])
               (equal? stanza-line* line*))) ...))
  (check-to-stanza*
    ["a\na\na\n\nb\nb\nb\n"
     '(("a" "a" "a") ("b" "b" "b"))]
    [""
     '()]
    ["asdf asdf asdf"
     '(("asdf asdf asdf"))]
    ["what\n\ta great poem"
     '(("what" "\ta great poem"))]
    ["line one\n\n line two"
     '(("line one") (" line two"))]
  )

  ;; -- string-empty?
  (define-syntax-rule (check-string-empty?/true [s ...])
    (begin (check-true (string-empty? s)) ...))
  (check-string-empty?/true
    [""
     " "
     "\t"
     "\n"
     "\r"
     "\t\t\t"
     "    "
     "\n\r\t"])

  (define-syntax-rule (check-string-empty?/false [s ...])
    (begin (check-false (string-empty? s)) ...))
  (check-string-empty?/false
    ["a"
     " hello "
     "why\nnot\n"
     "\ta\tb"])
)
