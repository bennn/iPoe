#lang racket/base

;; Tools for interacting with users

(provide
  get-user-input
  ;;

  ;; -- built-in readers

  read-natural
  ;; (-> String (U Natural #f))
  ;; Try reading natural number from input, return #f on failure.

)

(require
)

;; =============================================================================

;; Prompt the user until he/she/it returns someting `valid?`
(define (get-user-input valid?
                        #:prompt prompt-str
                        #:description [desc-str #f])
  ;; TODO use something more general/gui-friendly than printf
  (when desc-str (displayln desc-str))
  (define (show-prompt)
    (displayln prompt-str)
    (display "> "))
  (show-prompt)
  (let loop ([response (read-line)])
    (define result (valid? response))
    (cond
      [result
       ;; Successfully parsed result, we're done
       result]
      [else
       ;; Optimistically send a help message
       (when (and desc-str (or (regexp-match "help" response)
                               (regexp-match "\\?"    response)))
         (displayln desc-str))
       ;; Re-show the prompt and loop
       (show-prompt)
       (loop (read-line))])))

(define (read-natural str)
  (define n (string->number str))
  (and n (exact-nonnegative-integer? n) n))

;; =============================================================================

(module+ test
  (require rackunit)

  (define-syntax-rule (check-read-natural [str expect] ...)
    (begin (check-equal? (read-natural str) expect) ...))
  (check-read-natural
    ["hello"    #f]
    ["o"        #f]
    ["(-> a b)" #f]
    ["'1"       #f]
    ["'(1 2 3)" #f]
    ["-1"       #f]
    ["1/2"      #f]
    ;; --
    ["1351"           1351]
    ["0"              0]
    ["1"              1]
    ["83423113513513" 83423113513513]
  )
)

