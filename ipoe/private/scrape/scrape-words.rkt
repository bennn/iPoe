#lang racket/base

;; Search the internet for data about a word

(provide
  scrape-word
  ;; (-> String Word-Result)
  ;; Search a poem for new words.

  (struct-out word-result)
)

(require
  racket/string
  (only-in racket/match match-define)
  (only-in sxml if-car-sxpath)
  ipoe/private/scrape/scrape-util
  (only-in ipoe/private/util/string
    string-count-chars)
)

;; =============================================================================

(struct word-result (
  word ;; String, the result word
  definition ;; String, a definition of the word
  num-syllables ;; Natural, number of syllables in the word
  src ;; Symbol, describes where this information came from
) #:prefab ) ;; prefab to easily serialize the db cache


(define (scrape-word w)
  (or ;(american-heritage w)
      (dictionary.com w)
      (merriam-webster w)
      (the-free-dictionary w)))
      ;urban-dictionary (they don't have syllables)

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
             (add1 (string-count-chars #\· h1)))))

    ;; src
    'dictionary.com))

;; 2015-09-23: The word 'coalman' doesn't have syllables, but it does
;;  have a page on this site. Should we accept that?
(define the-free-dictionary
  (word-scraper
    ;; word->url
    (lambda (w) (string-append "http://www.thefreedictionary.com/" w))

    ;; sxml->definition
    (lambda (sxml)
     (let ([div ((if-car-sxpath `(// div ,(class? "ds-list") *text*)) sxml)])
       (and div (string-trim div))))

    ;; sxml->num-syllables
    (lambda (sxml)
     (let ([h2 ((if-car-sxpath `(// div ,(id? "Definition") section h2 *text*)) sxml)])
       (and h2 (add1 (string-count-chars #\· h2)))))

    ;; src
    'the-free-dictionary))

(define merriam-webster
  (word-scraper
    ;; word->url
    (lambda (w) (string-append "http://www.merriam-webster.com/dictionary/" w))

    ;; sxml->definition
    (lambda (sxml)
      (let ([p ((if-car-sxpath `(// p ,(class? "bottom_entry") *text*)) sxml)])
        (and p (substring p 2))))

    ;; sxml->num-syllables
    (lambda (sxml)
      ;; The <span> we get has a list of children:
      ;; - a string for each syllable part
      ;; - a separator between each syllable part
      ;; - 2 metadata children, one to say "span" and the other for attributes
      (let* ([span ((if-car-sxpath `(// span ,(class? "hw-syllables"))) sxml)]
             [len  (length (or span '(1 2)))]
             ;; Add 1 to quotient if len is odd
             [q    (quotient len 2)])
        (and (positive? q) q)))

    ;; src
    'merriam-webster))

(define american-heritage
  (word-scraper
    ;; word->url
    (lambda (w) (string-append "https://www.ahdictionary.com/word/search.html?q=" w))

    ;; sxml->definition
    (lambda (sxml)
      (let ([d (or
                 ((if-car-sxpath `(// div ,(class? "ds-list") *text*)) sxml)
                 ((if-car-sxpath `(// div ,(class? "ds-single") *text*)) sxml))])
        d))

    ;; sxml->num-syllables
    (lambda (sxml)
      (let* ([div ((if-car-sxpath `(// div ,(class? "rtseg"))) sxml)]
             [res ((if-car-sxpath `(// *text*)) div)])
        (and res (add1 (string-count-chars #\· res)))))

    ;; src
    'american-heritage))

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
  (require rackunit ipoe/private/util/rackunit-abbrevs)

  (define not-a-word "hguwisdvzodxv")

  (check-apply* (lambda (w) (word-result? (scrape-word w)))
   ["yes" == #t]
   [not-a-word == #f]
  )

  (check-apply* dictionary.com
   ["penguin" == (word-result "penguin" "any of several flightless, aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having webbed feet and wings reduced to flippers." 2 'dictionary.com)]
   ["flower" == (word-result "flower" "the blossom of a plant." 2 'dictionary.com)]
    [not-a-word == #f]
  )

  (check-apply* the-free-dictionary
   ["penguin" == (word-result "penguin" "Any of various stout, flightless aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having flipperlike wings and webbed feet adapted for swimming and diving, short scalelike feathers, and white underparts with a dark back." 2 'the-free-dictionary)]
   ["boilerplate" == (word-result "boilerplate" "Steel in the form of flat plates used in making steam boilers." 3 'the-free-dictionary)]
   ["coalman" == #f]
   [not-a-word == #f]
  )

  (check-apply* merriam-webster
    ["day" == (word-result "day" "the part of the day when people are usually most active and when most businesses are open" 1 'merriam-webster)]
    ["penguin" == (word-result "penguin" "a black-and-white bird that cannot fly, that uses its wings for swimming, and that lives in or near the Antarctic" 2 'merriam-webster)]
    ["umbrella" == (word-result "umbrella" "something that includes several or many different things" 3 'merriam-webster)]
    ["candelabra" == (word-result "candelabra" "an object with several branches for holding candles or lights" 4 'merriam-webster)]
    [not-a-word == #f]
   )

  (check-apply* american-heritage
   ["trip" == (word-result "trip" "A going from one place to another; a journey." 1 'american-heritage)]
   ["penguin" == (word-result "penguin" "Any of various stout, flightless aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having flipperlike wings and webbed feet adapted for swimming and diving, short scalelike feathers, and white underparts with a dark back." 2 'american-heritage)]
   ["hardcover" == (word-result "hardcover" "Having a rigid binding, as of cardboard covered with cloth or with leather. Used of books." 3 'american-heritage)]
   [not-a-word == #f]
  )
)
