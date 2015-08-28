#lang racket/base

(provide
  scrape-word
  ;; (-> String Boolean)
  ;; Search a poem for new words.

  (struct-out word-result)
)

(require
  racket/string
  (only-in sxml if-car-sxpath)
  "scrape-util.rkt"
)

;; =============================================================================

(struct word-result (
  word
  definition
  num-syllables
) #:transparent )

(define DICTIONARY.COM "http://dictionary.reference.com")

(define (scrape-word w)
  (scrape-dictionary.com w))

;; Parse results from dictionary.com
;; (: scrape-dictionary.com (-> String (U #f Word-Result)))
(define (scrape-dictionary.com word)
  (define url-str (string-append DICTIONARY.COM "/browse/" word))
  (define sxml (url->sxml url-str))
  (define def (dictionary.com-description sxml))
  (define syl (dictionary.com-syllable sxml))
  ;; If word is not found, just return #f
  (and def syl (word-result word def syl)))

;; (-> SXML String)
(define (dictionary.com-description sxml)
  (define div ((if-car-sxpath `(// div ,(class? "def-content"))) sxml))
  ;; caddr is scary, but *text* seems to do caddr
  (and div (string-trim (cadddr div))))

;; Dictionary.com keeps a syllable-ized version of each word
;; (-> SXML (U #f Natural))
(define (dictionary.com-syllable sxml)
  (define elem ((if-car-sxpath '(// h1 span @ data-syllable *text*)) sxml))
  (and elem
       (add1 (count-chars (lambda (c) (char=? c #\·)) elem))))


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

  (check-apply* scrape-dictionary.com
   ["penguin" == (word-result "penguin" "any of several flightless, aquatic birds of the family Spheniscidae, of the Southern Hemisphere, having webbed feet and wings reduced to flippers." 2)]
   ["flower" == (word-result "flower" "the blossom of a plant." 2)]
  )
)
