#lang racket/base

;; Tools for interacting with users

(provide
  alert
  ;; (-> String Void)
  ;; Display a message to the user

  debug
  ;; (-> String Void)
  ;; Display a debugging message to the user

  get-user-input
  ;; (->* [(-> String A) #:prompt String] [#:nullable? Boolean #:descr (U #f String)] A)
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

  read-sql-id
  ;; (-> String (U #f String))
  ;; Identity if argument is a SQL identifier

  read-yes-or-no
  ;; (-> String (U 'Y 'N #f))
  ;; Read a yes-or-no input
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/util/check-os
  (for-syntax racket/base)
)
(if-windows
  (require ipoe/private/util/windows-readline)
  (require readline readline/pread))

;; =============================================================================

;; For external clients
(define alert displayln)

(define (debug str)
  (display "[DEBUG] ")
  (displayln str))

;; Prompt the user until he/she/it returns someting `valid?`
(define (get-user-input valid?
                        #:prompt prompt-str
                        #:nullable? [nullable? #f]
                        #:description [desc-str #f])
  ;; TODO use something more general/gui-friendly than printf
  (when desc-str (alert desc-str))
  (define (read/prompt)
    (displayln prompt-str)
    (parameterize ([readline-prompt #"ipoe> "])
      (read)))
  (let loop ([response (read/prompt)])
    (cond
     [(or (eof-object? response)
          (and nullable? (eq? #f response)))
      #f]
     [(valid? response)
      => (lambda (x) x)]
     [else
      ;; Optimistically send a help message
      (when (and desc-str
                 (string? response)
                 (or (regexp-match "help" response)
                     (regexp-match "\\?"  response)))
        (alert desc-str))
      ;; Re-show the prompt and loop
      (loop (read/prompt))])))

(define (internal-error src str)
  (define err-loc (string->symbol (format "ipoe:~a:internal-error" src)))
  (error err-loc str))

(define (user-error src str)
  ;; Will probably change after Dr.Racket
  (raise-user-error src str))

;; -----------------------------------------------------------------------------

(define (read-natural n)
  (and (exact-nonnegative-integer? n)
       n))

(define (read-string str)
  (and (string? str)
       str))

(define rSQL #rx"^[a-z_]+[a-z_0-9]*$")
(define (read-sql-id str-or-sym)
  (define str (if (symbol? str-or-sym) (symbol->string str-or-sym) str-or-sym))
  (and (string? str)
       (regexp-match? rSQL str)
       str))

(define YES* '(Y y yes))
(define NO*  '(N n no))

(define (read-yes-or-no v)
  (cond
   [(symbol? v)
    (cond
     [(memq v YES*)
      'Y]
     [(memq v NO*)
      'N]
     [else #f])]
   [(string? v)
    (case (string-downcase v)
     [("y" "yes" "yolo") 'Y]
     [("n" "no") 'N]
     [else #f])]
   [else #f]))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs ipoe/private/util/check-print)

  ;; TODO test interactions

  (check-false* read-natural
    ["hello"]
    ["o"]
    ["(-> a b)"]
    ["'1"]
    ["'(1 2 3)"]
    ["-1"]
    ["1/2"])
  (check-apply* read-natural
    [1351           == 1351]
    [0              == 0]
    [1              == 1]
    [83423113513513 == 83423113513513])

  (check-false* read-string
   [1]
   ['asdf])

  (check-true* (lambda (w) (string=? (read-string w) w))
   ["yes"]
   ["A"]
   ["$cash$"]
   ["P@$$-W0RD__"]
   ["さ"]
   [""])

  (check-true* (lambda (w) (string=? (read-sql-id w) w))
   ["a"]
   ["_"]
   ["b42"]
   ["jake6969brakes"]
   ["hello_world"])

  (check-true* (lambda (w) (string=? (read-sql-id w) (symbol->string w)))
   ['yolo]
   ['db23]
   ['happy_go_lucky])

  (check-false* read-sql-id
   [""]
   ["-"] ;; No - allowed
   ["name$"] ;; No $ allowed
   ["777_lucky_tables"]  ;; can't start with a number
   ["WWW"] ;; No caps allowed
   ["Table 1"]) ;; No spaces, no caps

  (check-false* read-yes-or-no
    ["hello"]
    [""]
    ["word"]
    ["yes please"]
    ["never"]
    ["no no no"])

  (check-apply* read-yes-or-no
    ['Y == 'Y]
    ['y == 'Y]
    ['N == 'N]
    ['no == 'N]
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

