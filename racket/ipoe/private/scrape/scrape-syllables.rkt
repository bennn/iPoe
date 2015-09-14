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
  "scrape-words.rkt"
  ipoe/private/ui
)

;; =============================================================================

;; Validate the suggested number of syllables for a word
(define (resolve-syllables word
                           syllables
                           #:offline? [offline? #f]
                           #:interactive? [interactive? #t])
  (define-values (ref-syllables src)
    (if offline?
        (values (naive-syllables word) "our-heuristic")
        (let ([wr (scrape-word word)])
          (if (word-result? wr)
              (values (word-result-num-syllables wr) (word-result-src wr))
              (values #f #f)))))
  (cond
   [(not syllables)
    ref-syllables]
   [(or (not ref-syllables) (= syllables ref-syllables))
    ;; Good, validated user input against trusted source
    syllables]
   [interactive?
    (get-user-input read-natural
                    #:prompt (format "Please enter the correct number of syllables for '~a'." word)
                    #:description (format "Data mismatch: word '~a' expected to have ~a syllables, but ~a says it has ~a syllables." word syllables src ref-syllables))]
   [else
    (alert (format "Source '~a' claims that word '~a' has ~a syllables (instead of the given ~a syllables).\n" src word ref-syllables syllables))
    syllables]))

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
  (require rackunit ipoe/private/rackunit-abbrevs)

  (check-apply* naive-syllables
    ["foobar" == 3]
    ["" == 0]
    ["eek" == 2]
    ["hour" == 2]
    ["what" == 1]
    ["balloon" == 3]
    ["wombat" == 2]
    ["arcade" == 3]
    ["apvuhinjets" == 4]
    ["bat" == 1]
    ["computer" == 3])

  ;; Should scrape internet for syllables
  (check-apply* (lambda (w) (resolve-syllables w #f #:interactive? #f #:offline? #f))
    ["hour" == 1]
    ["never" == 2]
    ["mississippi" == 4]
    ["continuity" == 5]
    ["asbferufvzfjuvfds" == #f]
  )

  ;; Should run a local algorithm (and get the wrong answer for "hour"
  (check-apply* (lambda (w) (resolve-syllables w #f #:interactive? #f #:offline? #t))
    ["hour" == 2]
  )

  ;; Should trust the user input
  (check-apply* (lambda (w) (resolve-syllables w 99 #:interactive? #f #:offline? #t))
    ["hour" == 99]
  )
)
