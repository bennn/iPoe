#lang racket/base

(provide
  string-count-chars
  ;; (-> (U Char (-> Char Boolean)) String Natural)
  ;; Count the number of characters satisfying the predicate

  string-empty?
  ;; (->* [String] [#:trim? Boolean #f] Boolean)
  ;; True if the argument is an empty string
  ;; Optional argument #:trim decides whether to ignore whitespace.

  string-normalize-downcase
  ;; (-> String String)
  ;; Filter non-alphabetic characters from the string & downcase what's left

  read-from-string
  ;; (-> String Any)
  ;; Use `read` to pull a single value from the string

  string-ascii
  ;; (-> String String)
  ;; Filter non-ascii characters from a string
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/port
    with-input-from-string)
  (only-in racket/string
    non-empty-string?
    string-trim)
)

;; =============================================================================

(define (string-count-chars f str)
  (define p (if (char? f) (lambda (c) (char=? c f)) f))
  (for/sum ([c (in-string str)]
            #:when (p c))
    1))

(define (string-empty? s #:trim? [trim? #f])
  (let ([s/trim (if trim? (string-trim s) s)])
    (not (non-empty-string? s/trim))))

(define (string-normalize-downcase str)
  (define char* (for/list ([c (in-string str)]
                           #:when (char-alphabetic? c))
                  (char-downcase c)))
  (apply string char*))

;; (: read-from-string (-> String Any))
(define (read-from-string str)
  (with-input-from-string str read))

(define (string-ascii str)
  (apply string
    (for/list ([c (in-string str)]
               #:when (<= (char->integer c) 127))
      c)))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (test-case "string-count-chars"
    (check-apply* string-count-chars
     [#\4 "42" == 1]
     [#\a "foo" == 0]
     [#\x "" == 0]
     [(lambda (c) (or (eq? c #\c) (eq? c #\d)))
      "cadddr" == 4]
    ))

  (test-case "string-empty?"
    (check-true* string-empty?
     [""]
     [(string)]
     [(make-string 0)]
     [(string-append "" "")]
     [""]
     [" " #:trim? #t]
     ["\t" #:trim? #t]
     ["\n" #:trim? #t]
     ["\r" #:trim? #t]
     ["\t\t\t" #:trim? #t]
     ["    " #:trim? #t]
     ["\n\r\t" #:trim? #t])

    (check-false* string-empty?
     ["a"]
     [" hello "]
     ["why\nnot\n"]
     ["\ta\tb"]
     ["a"]
     ["b c d"]
     ["\r"]
     ["\t\t\t"]
     ["    "]))

  (test-case "string-normalize-downcase"
    (check-apply* string-normalize-downcase
      ["a" == "a"]
      ["" == ""]
      ["." == ""]
      ["hello," == "hello"]
      ["WHAT'S-THIS" == "whatsthis"]
      ["161" == ""]
      ["+_d!@#$%^&*(angi)<>?,t." == "dangit"]
      ["asdf" == "asdf"]
      ["cat61" == "cat"]
      ["ARGH"  == "argh"]
      ["waiT?" == "wait"]
      ["don't" == "dont"]
      ["hel,p" == "help"]
      ["..,." == ""]
      ["\t    \t " == ""]))

  (test-case "read-from-string"
    (check-apply* read-from-string
     ["42" == 42]
     ["yolo" == 'yolo]
     ["(x = y * 3 )" == '(x = y * 3)]))

  (test-case "string-ascii"
    (check-apply* string-ascii
     ["asdf" ==> "asdf"]
     ["" ==> ""]
     ["λ x . x" ==> " x . x"]))
)
