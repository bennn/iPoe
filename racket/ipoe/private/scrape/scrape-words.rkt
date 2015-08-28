#lang racket/base

(provide
  scrape-word
  ;; (-> String Boolean)
  ;; Search a poem for new words.

  (struct-out word-result)
)

(require
  racket/string
  (only-in racket/match match-define)
  (only-in sxml if-car-sxpath)
  "scrape-util.rkt"
)

;; =============================================================================

(struct word-result (
  word ;; String, the result word
  definition ;; String, a definition of the word
  num-syllables ;; Natural, number of syllables in the word
  src ;; Symbol, describes where this information came from
) #:transparent )


(define (scrape-word w)
  (or (dictionary.com w)))
  ;urban-dictionary
  ;merriam-webster
  ;the-free-dictionary

;; -----------------------------------------------------------------------------
;; Data Definition: Word Scraper

(struct word-scraper (
  word->url
  ;; (-> String String)
  ;; Build a URL query string from a word.

  sxml->definition
  ;; (-> SXML (U #f String))
  ;; Parse the definition from a word's web page.

  sxml->num-syllables
  ;; (-> SXML (U #f Natural))
  ;; Parse the number of syllables from a word's web page.

  src
  ;; Symbol
  ;; The name of this scraper

) #:property prop:procedure
  ;; Search the web for information on a word.
  ;; An instance of this struct defines the search protocol for 1 website
  ;; (-> Word-Scraper String Word-Result)
  (lambda (self word)
    (match-define (word-scraper word->url
                                sxml->definition
                                sxml->num-syllables
                                src) self)
    (define url (word->url word))
    (define sxml (url->sxml url))
    (define def (sxml->definition sxml))
    (define syl (sxml->num-syllables sxml))
    (and def syl (word-result word def syl src)))
)

;; -----------------------------------------------------------------------------
;; dictionary.com

(define dictionary.com
  (word-scraper
    ;; word->url
    (lambda (w) (string-append "http://dictionary.reference.com/browse/" w))

    ;; sxml->definition
    (lambda (sxml)
      (let ([div ((if-car-sxpath `(// div ,(class? "def-content"))) sxml)])
        ;; cadddr is scary, but *text* seems to be caddr
        (and div (string-trim (cadddr div)))))

    ;; sxml->num-syllables
    (lambda (sxml)
      (let ([h1 ((if-car-sxpath '(// h1 span @ data-syllable *text*)) sxml)])
        (and h1
             (add1 (count-chars (lambda (c) (char=? c #\·)) h1)))))

    ;; src
    'dictionary.com))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "scrape-word"
   #:args WORD*
   (for ([w (in-list WORD*)])
     (printf "Searching for '~a'... " w)
     (displayln (scrape-word w)))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  (check-apply* (lambda (w) (word-result? (scrape-word w)))
   ["yes" == #t]
   ["hguwisdvzodxv" == #f]
  )

  (check-apply* dictionary.com
   ["penguin" == (word-result "penguin" "any of several flightless, aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having webbed feet and wings reduced to flippers." 2 'dictionary.com)]
   ["flower" == (word-result "flower" "the blossom of a plant." 2 'dictionary.com)]
  )
)
