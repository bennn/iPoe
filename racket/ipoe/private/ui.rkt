#lang racket/base

;; Tools for interacting with users

(provide
  alert
  ;; (-> String Void)
  ;; Display a message to the user

  get-user-input
  ;;

  user-error
  ;; (-> String Any)
  ;; Raise an error message

  ;; -- built-in readers

  read-natural
  ;; (-> String (U Natural #f))
  ;; Try reading natural number from input, return #f on failure.

  read-yes-or-no
  ;; (-> String (U 'Y 'N #f))
  ;; Read a yes-or-no input

)

(require
)

;; =============================================================================

;; For external clients
(define alert displayln)

;; Prompt the user until he/she/it returns someting `valid?`
(define (get-user-input valid?
                        #:prompt prompt-str
                        #:description [desc-str #f])
  ;; TODO use something more general/gui-friendly than printf
  (when desc-str (alert desc-str))
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
         (alert desc-str))
       ;; Re-show the prompt and loop
       (show-prompt)
       (loop (read-line))])))

(define (user-error src str)
  ;; Will probably change after Dr.Racket
  (raise-user-error src str))

;; -----------------------------------------------------------------------------

(define (read-natural str)
  (define n (string->number str))
  (and n (exact-nonnegative-integer? n) n))

(define (read-yes-or-no str)
  (case (string-downcase str)
   [("y" "yes" "yolo") 'Y]
   [("n" "no") 'N]
   [else #f]))

;; =============================================================================

(module+ test
  (require rackunit)

  (define-syntax-rule (check-fun f [str expect] ...)
    (begin (check-equal? (f str) expect) ...))

  (check-fun read-natural
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

  (check-fun read-yes-or-no
    ["hello"    #f]
    ["" #f]
    ["word" #f]
    ["yes please" #f]
    ["never" #f]
    ["no no no" #f]
    ;; --
    ["y" 'Y]
    ["Y" 'Y]
    ["yes" 'Y]
    ["yEs" 'Y]
    ["yolo" 'Y]
    ;; --
    ["n" 'N]
    ["no" 'N]
    ["NO" 'N]
  )
)

