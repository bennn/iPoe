#lang racket/base

;; Look up / compute the number of syllables in a word.
;; On the command line, resolve syllables for a list of argument words
;;   `racket scrape-syllables.rkt WORD1 WORD2 WORD3 ...`

(provide
  resolve-syllables
  ;; (->* [String (U Natural #f)] [#:offline? Boolean #:interactive? Boolean] Natural)
  ;; Given a word and a suggested number of syllables,
  ;;  double-check against a 'trusted' source.
  ;; Optional argument `#:offline?` decides whether to query the internet for
  ;;  a reference number of syllables.
  ;; Optional argument `#:interactive?` decides whether to interactively resolve
  ;;  conflicts, or assume the function's argument is correct.
  ;; (To take the reference answer no matter what, use `#f` as input.)
)

(require
  "scrape-util.rkt"
  ipoe/private/ui
  (only-in sxml if-car-sxpath)
)

;; =============================================================================

(define REFERENCE "http://dictionary.reference.com")

;; Validate the suggested number of syllables for a word
(define (resolve-syllables word
                           syllables
                           #:offline? [offline? #f]
                           #:interactive? [interactive? #t])
  (define ref-syllables (if offline? (naive-syllables word)
                                     (scrape-syllables word)))
  (cond
   [(not syllables)
    ref-syllables]
   [(or (not ref-syllables) (= syllables ref-syllables))
    ;; Good, validated user input against trusted source
    syllables]
   [interactive?
    (get-user-input read-natural
                    #:prompt (format "Please enter the correct number of syllables for '~a'." word)
                    #:description (format "Data mismatch: word '~a' expected to have ~a syllables, but '~a' says it has ~a syllables." word syllables REFERENCE ref-syllables))]
   [else
    (printf "WARNING: reference '~a' claims word '~a' has ~a syllables (instead of the given ~a syllables).\n" REFERENCE ref-syllables word syllables)
    syllables]))

;; Parse the internet to count syllables for a word.
;; (: scrape-syllables (-> String (Listof String)))
(define (scrape-syllables word)
  (scrape-dictionary.com word))

;; Parse results from dictionary.com
(define (scrape-dictionary.com word)
  (define url-str (string-append REFERENCE "/browse/" word))
  (define sxml (url->sxml url-str))
  ;; Dictionary.com keeps a syllable-ized version of each word
  (define data ((if-car-sxpath '(// h1 span @ data-syllable *text*)) sxml))
  ;; If word is not found, just return #f
  (and data
       (add1 (count-chars (lambda (c) (char=? c #\·)) data))))

;; Naively count syllables (doesn't require an internet connection)
(define (naive-syllables word)
  (define vowels '(#\a #\e #\i #\o #\u #\y))
  (count-chars (lambda (c) (member c vowels)) word))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  ;; --
  (define out-file (make-parameter #f))
  (define offline? (make-parameter #f))
  (define interac? (make-parameter #t))
  (define u-syll   (make-parameter #f))
  ;; --
  (command-line
   #:program "scrape-syllables"
   #:once-each
   [("-o" "--output") o-param "Filename to save results to" (out-file o-param)]
   [("-x" "--offline") "When set, run in offline mode" (offline? #t)]
   [("-s" "--syllables") s-param "Suggested number of syllables" (u-syll (string->number s-param))]
   [("-n" "--non-interactive") "Always override -s suggestions with trusted source" (interac? #f)]
   #:args WORDS
   (begin
     (define out-port (if (out-file)
                          (open-output-file (out-file))
                          (current-output-port)))
     (fprintf out-port "WORD\tNUM-SYLLABLES\n")
     (for ([w (in-list WORDS)])
       (define s (resolve-syllables w (u-syll) #:offline? (offline?) #:interactive? (interac?)))
       (fprintf out-port "~a\t~a\n" w s))
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

  (define-syntax-rule (check-dictionary-syllables [word syll] ...)
    (begin (check-equal? (scrape-dictionary.com word) syll) ...))
  (check-dictionary-syllables
    ["hour" 1]
    ["never" 2]
    ["mississippi" 4]
    ["continuity" 5]
    ["asbferufvzfjuvfds" #f]
  )
)
