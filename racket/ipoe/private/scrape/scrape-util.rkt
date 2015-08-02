#lang racket/base

(provide
  count-chars
  ;; (-> (-> Char Boolean) String Natural)
  ;; Count the number of characters satisfying the predicate

  url->html

  url->sxml
)

(require
  net/url
  (only-in html read-html)
  (only-in "html.rkt" html->xexp)
)

;; =============================================================================

(define (count-chars f str)
  (for/sum ([c (in-string str)] #:when (f c)) 1))

(define (url->html arg)
  ;; Sure about `get-impure-port`?
  (call/input-url (->url arg) get-impure-port read-html))

(define (url->sxml arg)
  (call/input-url (->url arg) get-impure-port html->xexp))

;; Coerce a variety of argument types to a `url` struct
(define (->url arg)
  (cond
   [(url? arg) arg]
   [(string? arg) (string->url arg)]
   [(path? arg)   (path->url arg)]
   [(symbol? arg) (string->url (symbol->string arg))]))
