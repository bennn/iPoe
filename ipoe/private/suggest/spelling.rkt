#lang racket/base

;; Offer spelling suggestions

(provide
  filter-similar
  ;; (-> [String (Listof String)] [#:limit Natural] (Listof String))
  ;; (filter-similar w w* #:limit N)
  ;; Return a list of at most N words from the list w* that are similar to w.
  ;; (Similar = Levenshtein distance)

  suggest-spelling
  ;; (->* [String] [#:epsilon Natural #:limit Natural] (Listof String))
  ;; (suggest-spelling w #:epsilon e #:limit N)
  ;; Return a list of at most N words that are within `e` Levenshtein distance from `w`.
)

(require
  (only-in ipoe/private/suggest/levenshtein string-levenshtein)
  racket/runtime-path
)

;; =============================================================================

(define default-epsilon 2)
(define default-limit 10)
(define-runtime-path common-words "./common-words.rktd")

(define (filter-similar w w* #:limit [lim default-limit])
  (define top-N (make-vector lim #f))
  (for ([w2 w*])
    (define score (string-levenshtein w w2))
    (define new-pos
      (for/first ([old+score (in-vector top-N)]
                  [n         (in-naturals)]
                  #:when (or (not old+score)
                             (< score (cdr old+score))))
        n))
   (when new-pos
     (vector-set! top-N new-pos (cons w2 score))))
   (for/list ([w+s (in-vector top-N)]
              #:when w+s)
     (car w+s)))

(define (suggest-spelling str-param
                          #:epsilon [eps default-epsilon]
                          #:limit   [lim default-limit])
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
  (require rackunit ipoe/private/util/rackunit-abbrevs)

  (check-apply* filter-similar
    ["car" '("cat" "boat" "wolf" "march") #:limit 1 == '("cat")]
    ["car" '("cat" "boat" "wolf" "march") #:limit 3 == '("cat" "boat" "march")]
    ["car" '("cat" "boat" "wolf" "march") #:limit 5 == '("cat" "boat" "march")]
    ["ace" '("ace" "ace" "ace") #:limit 2 == '("ace" "ace")]
  )

  (check-apply* suggest-spelling
    ["the" #:limit 1 == '("the")]
    ["we"  #:limit 1 == '("the")]
    ["I"   #:limit 0 == '()]
    ["I"   #:limit 3 == '("be" "of" "to")]
    ["I" #:epsilon 1 #:limit 3 == '("a" "in" "I")]
  )
)
