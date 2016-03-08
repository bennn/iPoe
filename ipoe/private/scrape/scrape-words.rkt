#lang racket/base

;; Search the internet for data about a word

(provide
  scrape-word
  ;; (-> String Word-Result)
  ;; Search the internet for a new word.

  (struct-out word-result)
)

(require
  (only-in racket/match match-define)
  ipoe/private/scrape/scrape-util
  racket/string
  sxml
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
  (or (dictionary.com w)
      (american-heritage w)
      (merriam-webster w)
      (the-free-dictionary w)
      #;(urban-dictionary w)))

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

  sxml->word
  ;; (-> SXML (U #f String))
  ;; Parse the word from a word's web page.
  ;; Use to validate a site's response against the input query.

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
                                sxml->word
                                src) self)
    (define url (word->url word))
    (define sxml (url->sxml url))
    (define w-res (sxml->word sxml))
    (define d-res (sxml->definition sxml))
    (define s-res (sxml->num-syllables sxml))
    (and
      w-res d-res s-res
      (string-ci=? word w-res)
      (word-result word d-res s-res src)))
)

;; -----------------------------------------------------------------------------

(define dictionary.com
  (word-scraper
    ;; word->url
    (lambda (w) (string-append "http://dictionary.reference.com/browse/" w))

    ;; sxml->definition
    (lambda (sxml)
      ;"<no-definition>"
      (let ([div ((if-car-sxpath `(// div ,(class? "def-content"))) sxml)])
        ;; cadddr is scary, but *text* seems to be caddr
        (and div (string-trim (cadddr div)))))

    ;; sxml->num-syllables
    (lambda (sxml)
      (let ([h1 ((if-car-sxpath '(// h1 span @ data-syllable *text*)) sxml)])
        (and h1
             (add1 (string-count-chars #\· h1)))))

    ;; sxml->word
    (if-car-sxpath '(// h1 span *text*)) ;; Brittle! But I could not get class? working

    ;; src
    'dictionary.com))

;; 2015-09-23: The word 'coalman' doesn't have syllables, but it does
;;  have a page on this site. Should we accept that?
(define the-free-dictionary
  (let ([sxml->word+syll
         (if-car-sxpath `(// div ,(id? "Definition") section h2 *text*))])
  (word-scraper
    ;; word->url
    (lambda (w) (string-append "http://www.thefreedictionary.com/" w))

    ;; sxml->definition
    (lambda (sxml)
     (let ([div ((if-car-sxpath `(// div ,(class? "ds-list") *text*)) sxml)])
       (and div (string-trim div))))

    ;; sxml->num-syllables
    (lambda (sxml)
     (let ([w (sxml->word+syll sxml)])
       (and w (add1 (string-count-chars #\· w)))))

    ;; sxml->word
    (lambda (sxml)
      (let ([w (sxml->word+syll sxml)])
        (and w (string-replace w (string #\·) ""))))

    ;; src
    'the-free-dictionary)))

(define merriam-webster
  (word-scraper
    ;; word->url
    (lambda (w) (string-append "http://www.merriam-webster.com/dictionary/" w))

    ;; sxml->definition
    (lambda (sxml)
      (let ([p ((if-car-sxpath `(// p ,(class? "definition-inner-item") span *text*)) sxml)])
        (and p (string-trim (car (string-split p ":"))))))

    ;; sxml->num-syllables
    (lambda (sxml)
      ;; The <span> we get has a list of children:
      ;; - a string for each syllable part
      ;; - a separator between each syllable part
      ;; - 2 metadata children, one to say "span" and the other for attributes
      (let* ([w ((if-car-sxpath `(// span ,(class? "word-syllables"))) sxml)])
        (if w (add1 (string-count-chars #\· (cadddr w))) 1)))

    ;; sxml->word
    (if-car-sxpath '(// h1 *text*))

    ;; src
    'merriam-webster))

(define american-heritage
  (let ([sxml->word+syll
         (lambda (sxml)
           (let* ([div ((if-car-sxpath `(// div ,(class? "rtseg"))) sxml)]
                  [res ((if-car-sxpath `(// *text*)) div)])
             res))])
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
      (let ([w (sxml->word+syll sxml)])
        (and w (add1 (string-count-chars #\· w)))))

    ;; sxml->word
    (lambda (sxml)
      (let ([w (sxml->word+syll sxml)])
        (and w (string-replace w (string #\·) ""))))

    ;; src
    'american-heritage)))

;(define urban-dictionary
;  (word-scraper
;    ;; word->url
;    (lambda (w) (string-append "http://www.urbandictionary.com/define.php?term=" w))
;
;    ;; sxml->definition
;    (if-car-sxpath `(// div ,(class? "meaning") *text*))
;
;    ;; sxml->num-syllables (not stored on urbandict!)
;    (lambda (sxml) #f)
;
;    ;; sxml->word
;    (if-car-sxpath `(// div ,(class? "def-header") span *text*))
;
;    ;; src
;    'urban-dictionary))

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
   ["mississippi" == #t]
   [not-a-word == #f]
  )

  (check-apply* dictionary.com
   ["penguin" == (word-result "penguin" "any of several flightless, aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having webbed feet and wings reduced to flippers." 2 'dictionary.com)]
   ["flower" == (word-result "flower" "the blossom of a plant." 2 'dictionary.com)]
   [not-a-word == #f]
   ["cats" == #f]
  )

  (check-apply* the-free-dictionary
   ["penguin" == (word-result "penguin" "Any of various stout, flightless aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having flipperlike wings and webbed feet adapted for swimming and diving, short scalelike feathers, and white underparts with a dark back." 2 'the-free-dictionary)]
   ["boilerplate" == (word-result "boilerplate" "Steel in the form of flat plates used in making steam boilers." 3 'the-free-dictionary)]
   ["coalman" == (word-result "coalman" "a person who sells or delivers coal" 1 'the-free-dictionary)]
   [not-a-word == #f]
   ["leagues" == #f]
  )

  (check-apply* merriam-webster
    ["day" == (word-result "day" "a period of 24 hours beginning at midnight" 1 'merriam-webster)]
    ["penguin" == (word-result "penguin" "a black-and-white bird that cannot fly, that uses its wings for swimming, and that lives in or near the Antarctic" 2 'merriam-webster)]
    ["umbrella" == (word-result "umbrella" "a device that is used for protection from the rain and sun" 3 'merriam-webster)]
    ["candelabra" == (word-result "candelabra" "an object with several branches for holding candles or lights" 4 'merriam-webster)]
    [not-a-word == #f]
    ["stopping" == #f]
   )

  (check-apply* american-heritage
   ["trip" == (word-result "trip" "A going from one place to another; a journey." 1 'american-heritage)]
   ["penguin" == (word-result "penguin" "Any of various stout, flightless aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having flipperlike wings and webbed feet adapted for swimming and diving, short scalelike feathers, and white underparts with a dark back." 2 'american-heritage)]
   ["hardcover" == (word-result "hardcover" "Having a rigid binding, as of cardboard covered with cloth or with leather. Used of books." 3 'american-heritage)]
   [not-a-word == #f]
    ["stopping" == #f] ;; Word converted to root -- scraping should fail
  )

;  (check-apply* urban-dictionary
;   ["match" == #t]
;   [not-a-word == #f]
;   ["leagues" == #f]
;  )

  (define (check-syllables scraper w)
    (word-result-num-syllables (scraper w)))

  (check-apply* (lambda (scraper w) (word-result-num-syllables (scraper w)))
   [dictionary.com "each" == 1]
   [the-free-dictionary "each" == 1]
   [merriam-webster "each" == 1]
   [american-heritage "each" == 1]
   [american-heritage "every" == 2]
   [american-heritage "day" == 1])
   ;[urban-dictionary "magnificent" == #f]
   ;[urban-dictionary "cat" == #f]

)
