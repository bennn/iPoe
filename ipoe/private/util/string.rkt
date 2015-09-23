#lang racket/base

(provide
  string-contains?
  ;; (-> String String Boolean)
  ;; True if the second string is contained within the first

  string-count-chars
  ;; (-> (U Char (-> Char Boolean)) String Natural)
  ;; Count the number of characters satisfying the predicate

  string-empty?
  ;; (-> String Boolean)
  ;; True if the argument is an empty string

  string-prefix?
  ;; (-> String String Boolean)
  ;; True if the second argument exactly matches the first characters of
  ;;  the first argument.

  string-suffix?
  ;; (-> String String Boolean)
  ;; True if the second argument exactly matches the last characters of
  ;;  the first argument

  read-from-string
  ;; (-> String Any)
  ;; Use `read` to pull a single value from the string
)

(require
  (only-in racket/port with-input-from-string)
)

;; =============================================================================

(define (string-contains? str sub)
  (define L1 (string-length str))
  (define L2 (string-length sub))
  (for/or ([start (in-range (add1 (- L1 L2)))])
    (for/and ([i (in-range L2)])
      (char=? (string-ref str (+ i start))
              (string-ref sub i)))))

(define (string-count-chars f str)
  (define p (if (char? f) (lambda (c) (char=? c f)) f))
  (for/sum ([c (in-string str)]
            #:when (p c))
    1))

(define (string-empty? s)
  (zero? (string-length s)))

(define (string-prefix? str prefix)
  (define L1 (string-length str))
  (define L2 (string-length prefix))
  (let loop ([i 0])
    (cond
     [(= L2 i)  #t] ;; Finished reading all chars in prefix
     [(= L1 i)  #f] ;; Prefix is longer than string
     [else      (and (char=? (string-ref str i) (string-ref prefix i))
                     (loop (add1 i)))])))

(define (string-suffix? str suffix)
  (define L2 (string-length suffix))
  (define offset (- (string-length str) L2))
  (and (not (negative? offset)) ;; Suffix isn't longer than string
       (let loop ([i+o offset] [i 0])
         (or (= i L2)
             (and (char=? (string-ref str i+o) (string-ref suffix i))
                  (loop (add1 i+o) (add1 i)))))))

;; (: read-from-string (-> String Any))
(define (read-from-string str)
  (with-input-from-string str read))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  ;; -- string-contains?
  (check-true* string-contains?
    ["racket" "racket"]
    ["racket" "cket"]
    ["racket" "acke"]
    ["racket" ""]
    ["racket" "t"])

  (check-false* string-contains?
   ["racket" "b"]
   ["racket" "R"]
   ["racket" "kc"]
   ["racket" "racketr"])

  ;; -- string-count-chars
  (check-apply* string-count-chars
   [#\4 "42" == 1]
   [#\a "foo" == 0]
   [#\x "" == 0]
   [(lambda (c) (or (eq? c #\c) (eq? c #\d)))
    "cadddr" == 4]
  )

  ;; -- string-empty?
  (check-true* string-empty?
   [""]
   [(string)]
   [(make-string 0)]
   [(string-append "" "")])

  (check-false* string-empty?
   ["a"]
   ["b c d"])

  ;; -- string-prefix?
  (check-true* string-prefix?
   ["racket" ""]
   ["racket" "r"]
   ["racket" "rack"]
   ["racket" "racket"])

  (check-false* string-prefix?
   [""       "racket"]
   ["racket" "R"]
   ["racket" "rak"]
   ["racket" "racket2"])

  ;; -- string-suffix?
  (check-true* string-suffix?
    ["racket" ""]
    ["racket" "t"]
    ["racket" "cket"]
    ["racket" "racket"])

  (check-false* string-suffix?
   [""       "racket"]
   ["racket" "T"]
   ["racket" "r"]
   ["racket" "kat"])

  ;; -- read-from-string
  (check-apply* read-from-string
   ["42" == 42]
   ["yolo" == 'yolo]
   ["(x = y * 3 )" == '(x = y * 3)])
)
