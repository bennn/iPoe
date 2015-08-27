#lang racket/base

(provide
  scrape-word
  ;; (-> String Boolean)
  ;; Search a poem for new words.
)

(require

)

;; =============================================================================

;; TODO parameters?
(define (scrape-word w)
  TODO-implement)

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "scrape-word"
   #:args WORD*
   (for ([w (in-list WORD*)])
     (printf "Searching for '~a'...\n" w)
     (displayln (scrape-word w)))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  (check-apply* scrape-word
   ["yes" == #t]
   ["hguwisdvzodxv" == #f]
  )
)
