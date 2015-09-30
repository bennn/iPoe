#lang at-exp racket

(provide (all-defined-out))

(require scriblib/autobib)

;; =============================================================================

(define-cite ~cite citet generate-bibliography)

(define essay-on-man
  (make-bib
   #:author "Alexander Pope"
   #:title "An Essay on Man: Epistle I"
   #:url "http://www.poetryfoundation.org/poem/174165"
   #:is-book? #t
   #:date "1734"
   #:note "Accessed 2015-09-29"))

(define ct-limerick
  (make-bib
   #:author "Mike Scholtes"
   #:title "Church-Turing thesis"
   #:location "The Omnificent English Dictionary in Limerick Form"
   #:url "http://www.oedilf.com/db/Lim.php?Word=Church-Turing%20thesis"
   #:date "2007"
   #:note "Accessed 2015-09-27"))

(define shakes
  (make-bib
   #:author "Shakespeare"
   #:title "Sonnet #18"
   #:url "https://en.wikipedia.org/wiki/Sonnet_18"
   #:date "1608"
   #:note "Accessed 2015-08-16"))

(define plath
  (make-bib
   #:author "Sylvia Plath"
   #:title "Mad Girl's Love Song"
   #:url "https://en.wikipedia.org/wiki/Mad_Girl's_Love_Song"
   #:date "1953"
   #:note "Accessed 2015-08-16"))

(define heaney
  (make-bib
   #:author "Seamus Heaney"
   #:title "Two Lorries"
   #:url "http://www.ppu.org.uk/learn/poetry/poetry_otherwars3.html"
   #:date "1996"
   #:note "Accessed 2015-08-25"))

(define joyce
  (make-bib
   #:author "James Joyce"
   #:title "Portrait of the Artist as a Young Man"
   #:is-book? #t
   #:location "B.W. Huebsch"
   #:date "1916"))
