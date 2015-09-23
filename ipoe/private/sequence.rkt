#lang racket/base

(provide
  sequence-empty?
  ;; (-> Sequence Boolean)
  ;; True if the sequence is empty. Does not advance the sequence.
  ;; (How? idk, it's magic to me)

  sequence-first
  ;; (-> (Sequenceof Any) (U Any #f))
  ;; Return the first element in the sequence, or #f if the sequence is empty.

  sequence-skip
  ;; (-> Natural (Sequenceof Any) (Sequenceof Any))
  ;; Return a new sequence missing the first N elements of the argument.
  ;; Unlike `sequence-tail` does not throw an exception if the sequence has
  ;;  fewer than N element; instead returns an empty sequence.
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/sequence
    sequence-tail)
)

;; =============================================================================

(define (sequence-empty? seq)
  (for/fold ([e? #t])
            ([s seq]
             [i (in-range 1)])
    #f))

(define (sequence-first seq)
  (for/fold ([acc #f])
            ([s seq]
             [i (in-range 1)])
    s))

(define (sequence-skip n seq)
  (for/fold ([s seq])
            ([i (in-range (or n 0))])
    (if (sequence-empty? s)
        s
        (sequence-tail s 1))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  ;; -- sequence-empty?
  (check-true* sequence-empty?
   ['()]
   [(in-range 0)]
   [(in-string "")]
   [(in-range 2 0)])

  (check-false* sequence-empty?
   ['(1)]
   ['(a b c)]
   [(in-range 3)]
   [(in-string "asdfA")]
   [(in-naturals)])

  (let ([s (in-range 2)])
    (check-false (sequence-empty? s))
    (check-equal?
     (for/list ([x s]) x)
     '(0 1)))

  ;; -- sequence-first
  (check-apply* sequence-first
   ['() == #f]
   ['(1 2 3) == 1]
   ['(a b) == 'a]
   [(in-range 5) == 0]
   [(in-range 2 1) == #f]
   [(in-naturals 3) == 3])

  ;; -- sequence-skip
  (for ([n1 (in-range 5 10)]
        [n2 (sequence-skip 5 (in-range 0 10))])
    (check-equal? n1 n2))

  (for ([n1 (in-range 0 10)]
        [n2 (sequence-skip #f (in-range 0 10))])
    (check-equal? n1 n2))

  (check-true
   (sequence-empty? (sequence-skip 10 (in-range 0 5))))

)
