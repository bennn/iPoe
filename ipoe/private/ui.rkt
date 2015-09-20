#lang racket/base

;; Tools for interacting with users

(provide
  alert
  ;; (-> String Void)
  ;; Display a message to the user

  get-user-input
  ;; (->* [(-> String A) #:prompt String] [#:descr (U #f String)] A)
  ;; Run a simple interactive loop to get input from (current-input-port).
  ;; The first argument is used to accept/reject input at each step.
  ;; @optional{descr} Detailed information about what the user should enter 

  internal-error
  ;; (-> Symbol String Any)
  ;; Raise a developer-centric error message

  user-error
  ;; (-> Symbol String Any)
  ;; Raise a user-level error message

  ;; -- built-in readers

  read-natural
  ;; (-> String (U Natural #f))
  ;; Try reading natural number from input, return #f on failure.

  read-string
  ;; (-> String (U #f String))
  ;; Identity function, but asserts its argument is a string

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
    (or (valid? response)
        (begin
         ;; Optimistically send a help message
         (when (and desc-str (or (regexp-match "help" response)
                                 (regexp-match "\\?"  response)))
           (alert desc-str))
         ;; Re-show the prompt and loop
         (show-prompt)
         (loop (read-line))))))

(define (internal-error src str)
  (define err-loc (string->symbol (format "ipoe:~a:internal-error" src)))
  (error err-loc str))

(define (user-error src str)
  ;; Will probably change after Dr.Racket
  (raise-user-error src str))

;; -----------------------------------------------------------------------------

(define (read-natural str)
  (define n (string->number str))
  (and n (exact-nonnegative-integer? n) n))

(define (read-string str)
  (and (string? str)
       str))

(define (read-yes-or-no str)
  (case (string-downcase str)
   [("y" "yes" "yolo") 'Y]
   [("n" "no") 'N]
   [else #f]))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/rackunit-abbrevs)

  (check-false* read-natural
    ["hello"]
    ["o"]
    ["(-> a b)"]
    ["'1"]
    ["'(1 2 3)"]
    ["-1"]
    ["1/2"])
  (check-apply* read-natural
    ["1351"           == 1351]
    ["0"              == 0]
    ["1"              == 1]
    ["83423113513513" == 83423113513513])

  (check-false* read-string
   [1]
   ['asdf])

  (check-apply* read-string
   ["yes" == "yes"]
   ["A" == "A"]
   ["" == ""])

  (check-false* read-yes-or-no
    ["hello"]
    [""]
    ["word"]
    ["yes please"]
    ["never"]
    ["no no no"])

  (check-apply* read-yes-or-no
    ;; --
    ["y" == 'Y]
    ["Y" == 'Y]
    ["yes" == 'Y]
    ["yEs" == 'Y]
    ["yolo" == 'Y]
    ;; --
    ["n" == 'N]
    ["no" == 'N]
    ["NO" == 'N]
  )
)

