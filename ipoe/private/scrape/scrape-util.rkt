#lang racket/base

;; Helper functions for scrapers

(provide
  url->html
  ;; (-> (U url String) html)
  ;; Get the html data that lives at location URL

  url->sxml
  ;; (-> (U url String) sxml)
  ;; Get the html data that lives at location URL and parse it to sxml

  ;; -- filters
  id?
  ;; (-> String (-> (Listof sxml) ??? (Listof sxml)))
  ;; Given a string `str`, create an sxml filter that accepts all elements
  ;;  whose id attribute matches `str` exactly.

  class?
  ;; (-> String (-> (Listof sxml) ??? (Listof sxml)))
  ;; Given a string `str`, create an sxml filter that accepts all elements
  ;;  whose class attribute matches `str` exactly.

  contains-text?
  ;; (-> String (-> (Listof sxml) ??? (Listof sxml)))
  ;; Given a string `str`, create an sxml filter that accepts all elements
  ;;  whose text attribute matches the regular expression pattern `str`.

)

(require
  net/url
  (only-in html read-html)
  (only-in "html.rkt" html->xexp)
  sxml
  (only-in ipoe/private/util/string
    string-count-chars)
)

;; =============================================================================

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

;; Filter elements that do not have an id attribute equal to `str`
(define ((id? str) elem* ???)
  (define (id=str? e)
     (define id-text ((if-car-sxpath '( @ id *text*)) e))
     (and id-text (string=? str id-text)))
  (filter id=str? elem*))

(define ((class? str) elem* ???)
  (define (class=str? e)
    (define class-text ((if-car-sxpath '(@ class *text*)) e))
    (and class-text (string=? str class-text)))
  (filter class=str? elem*))

;; Filter elements that do not contain text matching the pattern
(define ((contains-text? pat) elem* ???)
  (define (contains? e)
    (define txt ((if-car-sxpath '(*text*)) e))
    (and txt (regexp-match pat txt)))
  (filter contains? elem*))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

)
