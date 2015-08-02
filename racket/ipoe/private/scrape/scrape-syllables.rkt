#lang racket/base

(provide
  resolve-syllables
)

(require
  "scrape-util.rkt"
  (only-in sxml car-sxpath)
)

;; =============================================================================

;; Validate the suggested number of syllables for a word
;; (-> String (U Natural #f) Natural)
(define (resolve-syllables word syllables)
  (error 'scrape-syllables "not implemented"))

;; Parse the internet to count syllables for a word.
;; (: scrape-syllables (-> String (Listof String)))
(define (scrape-syllables word)
  (scrape-dictionary.com word))

;; Parse results from dictionary.com
(define (scrape-dictionary.com word)
  (define url-str (string-append "http://dictionary.reference.com/browse/" word))
  (define sxml (url->sxml url-str))
  (define data ((car-sxpath '(// h1 span @ data-syllable *text*)) sxml))
  (add1 (count-chars (lambda (c) (char=? c #\·)) data)))

;; Naively count syllables
(define (naive-syllables word)
  (define vowels '(#\a #\e #\i #\o #\u #\y))
  (count-chars (lambda (c) (member c vowels)) word))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  ;; --
  (define out-file (make-parameter #f))
  (define offline? (make-parameter #f))
  ;; --
  (command-line
   #:program "scrape-syllables"
   #:once-each
   [("-o" "--output") o-param "Filename to save results to" (out-file o-param)]
   [("-x" "--offline") "When set, run in offline mode" (offline? #t)]
   #:args WORDS
   (begin
     (define get-syllables (if (offline?)
                               naive-syllables
                               scrape-syllables))
     (define out-port (if (out-file)
                          (open-output-file (out-file))
                          (current-output-port)))
     (parameterize ([current-output-port out-port])
       (printf "WORD\tNUM-SYLLABLES\n")
       (for ([w (in-list WORDS)])
         (define s (get-syllables w))
         (printf "~a\t~a\n" w s)))
     (define saved-to
       (if (out-file)
         (and (close-output-port out-port)
              (format " Saved results to '~a'" (out-file)))
         ""))
     (printf "Finished counting syllables for ~a words.~a\n" (length WORDS) saved-to))))

;; =============================================================================

(module+ test
  (require rackunit)

  (define-syntax-rule (check-naive-syllables [word syll] ...)
    (begin (check-equal? (naive-syllables word) syll) ...))
  (check-naive-syllables
    ["foobar" 3]
    ["" 0]
    ["eek" 2]
    ["hour" 2]
    ["what" 1]
    ["balloon" 3]
    ["wombat" 2]
    ["arcade" 3]
    ["bat" 1]
    ["computer" 3])
)
