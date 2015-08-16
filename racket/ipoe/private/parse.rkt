#lang racket/base

;; Generic word/line parsing tools

(provide
  last-word
  ;; (-> String (U #f String))
  ;; Get the last parseable word from a string of text

  ;number->word  ;; TODO implement + test
  ;; (-> Integer String)
  ;; Convert a number to an English word

  string->word*
  ;; (-> String (Listof String))
  ;; Split a string by whitespace, ensure that each result is a normalized word

  parse-word
  ;; (-> String String)
  ;; Filter unimportant things from some text.
  ;; i.e. Remove punctuation, convert to lowercase.

  to-line*
  ;; (-> (U Input-Port String (Listof String)) (Listof String))
  ;; Convert an input source to a list of newline-separated strings

  to-stanza*
  ;; (-> (Sequenceof String) (Sequenceof (Sequenceof String)))
  ;; Convert a sequence of lines to a sequence of stanzas
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/list last)
  (only-in racket/port port->lines)
  (only-in racket/string string-split string-trim)
  (only-in racket/generator in-generator generator yield)
)

;; =============================================================================
;; TODO library should be lazy enough to handle Dickens

(define (last-word str)
  ;; Collect a reverse-order list of parsed words
  ;; (Basically, rev-map)
  (define word* (string->word* str))
  (and (not (null? word*))
       (last word*)))

;; Convert a string to an "equivalent" string that might be in the database.
;; i.e., remove things like '?' and '!'.
;; (: parse-word (-> String String))
(define (parse-word word)
  (define str
    (apply string
           (for/list ([c (in-string word)]
                      #:when (or (char-alphabetic? c)
                                 (char-numeric? c)))
             (char-downcase c))))
  (and (not (string-empty? str)) str))

(define (string->word* str)
  (let loop ([str* (string-split str)])
    (if (null? str*)
        '()
        (let ([normalized (parse-word (car str*))]
              [rest       (lambda () (loop (cdr str*)))])
          (if normalized
              (cons normalized (rest))
              (rest))))))

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

;; (: string-empty? (-> String Boolean))
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
  (require rackunit "rackunit-abbrevs.rkt")

  ;; -- string->word*
  (check-apply* string->word*
    ["a" == '("a")]
    ["" == '()]
    ["." == '()]
    ["hello, world" == '("hello" "world")]
    ["WHAT IS THIS" == '("what" "is" "this")]
  )

  ;; -- to-line*
  (check-apply* to-line*
    ["" == '()] ;; Racket special case.. not sure how I feel about this
    ["a\nb" == '("a" "b")]
    [" \n " == '(" " " ")]

    ['("yes") == '("yes")]
    ['("yo\nlo" "we\npa") == '("yo" "lo" "we" "pa")]
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
  (check-true* string-empty?
    [""]
    [" "]
    ["\t"]
    ["\n"]
    ["\r"]
    ["\t\t\t"]
    ["    "]
    ["\n\r\t"]
  )

  (check-false* string-empty?
    ["a"]
    [" hello "]
    ["why\nnot\n"]
    ["\ta\tb"]
  )

  ;; -- parse-word
  (check-apply* parse-word
    ["asdf" == "asdf"]
    ["cat61" == "cat61"]
    ["ARGH"  == "argh"]
    ["waiT?" == "wait"]
    ["don't" == "dont"]
    ["hel,p" == "help"]
    ["123" == "123"]
    ;; --
    ["" == #f]
    ["..,." == #f]
  )

  ;; -- last-word
  (check-apply* last-word
    ["a red fox" == "fox"]
    ["a" == "a"]
    ;; --
    ["A" == "a"]
    ["word." == "word"]
    ["a few words and some ..." == "some"]
    ["don't do it!" == "it"]
    ["\tdiffn't\nspaces\n" == "spaces"]
    ["521351" == "521351"]
    ;; --
    ["" == #f]
    [",!--?" == #f]
    ["   \t   \t   \n" == #f]
  )
)
