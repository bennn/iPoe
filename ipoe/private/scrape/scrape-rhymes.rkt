#lang racket/base

;; Find rhymes for a word

(provide
  (struct-out rhyme-result)

  almost-rhymes?
  ;; (-> Rhyme-Result String Boolean)
  ;; True if the string is one of the Result's almost-rhymes

  rhymes?
  ;; (-> Rhyme-Result String Boolean)
  ;; True if the string is one of the Result's rhymes

  scrape-rhyme
  ;; (-> String Rhyme-Result)
  ;; Search online for words that rhyme and almost rhyme with the argument.
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/ui
  ipoe/private/scrape/scrape-util
  ipoe/private/util/logging
  (only-in sxml sxpath if-car-sxpath)
  (only-in racket/string string-trim string-join)
  (only-in racket/list take)
)

;; =============================================================================

(struct rhyme-result (
  rhyme*        ; (Listof String)
  almost-rhyme* ; (Listof String)
) #:constructor-name make-rhyme-result
  #:prefab ) ;; prefab to easily serialize the db cache

(define REFERENCE "http://rhymebrain.com/en")

(define (almost-rhymes? rr w)
  (and (member w (rhyme-result-almost-rhyme* rr)) #t))

(define (rhymes? rr w)
  (and (member w (rhyme-result-rhyme* rr)) #t))

(define (naive-rhyme word)
  ;; TODO I'm sure we can do better
  (make-rhyme-result '() '()))

(define (scrape-rhyme word)
  (define url-str (word->rhyme-url word))
  (define sxml (url->sxml url-str))
  (define all-results ((sxpath `(// div ,(id? "results") *)) sxml))
  (split-rhymes all-results))

;; Input is a list of <span> results, split these where the html changes to
;;  "almost rhymes" and return only the text for each word.
(define (split-rhymes elem*)
  (define h4? (if-car-sxpath (list (contains-text? "Words that almost rhyme"))))
  (let loop ([r* '()] [elem* elem*])
    (cond
      [(eq? '() elem*)
       ;; Should never happen
       (make-rhyme-result r* '())]
      [(h4? (car elem*))
       ;; Reached end of rhymes, extract text from the almost-rhymes
       (define a* (map (if-car-sxpath `(,(class? "wordpanel") *text*)) (cdr elem*)))
       (make-rhyme-result r* (for/list ([a (in-list a*)] #:when a) (string-trim a)))]
      [else
       ;; Extract text from one rhyme
       (define r ((if-car-sxpath `(,(class? "wordpanel") *text*)) (car elem*)))
       (define new-r* (if r (cons (string-trim r) r*) r*))
       (loop new-r* (cdr elem*))])))

;; Create a rhymebrain url from an english word
(define (word->rhyme-url word)
  (string-append REFERENCE "/What_rhymes_with_" word ".html"))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  ;; --
  (define out-file (make-parameter #f))
  (define u-rhyme  (make-parameter #f))
  ;; --
  (command-line
   #:program "scrape-syllables"
   #:once-each
   [("-o" "--output") o-param "Filename to save results to" (out-file o-param)]
   #:multi
   #:args WORDS
   (begin
     (define out-port (if (out-file)
                          (open-output-file (out-file))
                          (current-output-port)))
     (fprintf out-port "WORD\tRHYMES\tALMOST-RHYMES\n")
     (for ([w (in-list WORDS)])
       (define rr (scrape-rhyme w))
       (fprintf out-port "~a\t~a\t~a\n" w (string-join (rhyme-result-rhyme* rr) ",") (string-join (rhyme-result-almost-rhyme* rr) ",")))
     (define saved-to
       (if (out-file)
         (and (close-output-port out-port)
              (format " Saved results to '~a'" (out-file)))
         ""))
     (printf "Finished finding rhymes for ~a words.~a\n" (length WORDS) saved-to))))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (define CI? (getenv "CI"))

  (test-case "rhyme?:almost-rhyme?"
    (when (not CI?)
      (with-handlers ((exn:fail:network? (lambda (ex) (log-ipoe-scrape-warning "network error, skipping test"))))
        (define rr (scrape-rhyme "parent"))
        (check-true (rhymes? rr "aberrant"))
        (check-true (almost-rhymes? rr "embarrassed"))
        (check-false (rhymes? rr "child"))
        (check-false (almost-rhymes? rr "cat")))))

  (test-case "scrape-rhyme"
    (when (not CI?)
      (with-handlers ((exn:fail:network? (lambda (ex) (log-ipoe-scrape-warning "network error, skipping test"))))
        (define r (scrape-rhyme "mouse"))
        (check-true (rhyme-result? r))

        (check-equal? (take (rhyme-result-rhyme* r) 3)
                      '("porterhouse" "slaughterhouse" "clearinghouse"))

        (check-equal? (take (rhyme-result-almost-rhyme* r) 3)
                      '("mouth" "aus" "mows"))))
  )
)
