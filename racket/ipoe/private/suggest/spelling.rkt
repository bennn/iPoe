#lang racket/base

(provide
  suggest-spelling
)

(require
  (only-in ipoe/private/suggest/levenshtein string-levenshtein)
  racket/runtime-path
)

;; =============================================================================

(define default-epsilon 2)
(define-runtime-path common-words "./common-words.rktd")

(define (suggest-spelling str-param
                          #:epsilon [eps default-epsilon]
                          #:limit   [lim 10])
  (define num-matches (box 0))
  (define str (string-downcase str-param))
  (with-input-from-file common-words
    (lambda ()
      (for/list ([w (in-lines)]
                  #:when (and (< (unbox num-matches) lim)
                              (<= (string-levenshtein str (string-downcase w)) eps)))
        (set-box! num-matches (add1 (unbox num-matches)))
        w))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  (check-apply* suggest-spelling
    ["the" #:limit 1 == '("the")]
    ["we"  #:limit 1 == '("the")]
    ["I"   #:limit 0 == '()]
    ["I"   #:limit 3 == '("be" "of" "to")]
    ["I" #:epsilon 1 #:limit 3 == '("a" "in" "I")]
  )
)
