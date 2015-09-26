#lang racket/base

;; Uniform endpoint for
;; - Registering new poem forms
;; - Adding word

;; TODO accept input from file? (lots of words to submit)

(provide
  new
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  ipoe/private/command/common
  ipoe/private/db
  ipoe/private/parameters
  (only-in ipoe/private/ui read-natural get-user-input alert)
)

;; =============================================================================
;; === API

(define (new arg*)
  (command-line
   #:argv arg*
   #:args (word-or-form)
   (cond
    [(rkt-file? word-or-form)
     (new-form word-or-form)]
    [else
     (new-word word-or-form)])))

;; -----------------------------------------------------------------------------

;; Assumes that `fname` is a valid (existing) Racket file
(define (new-form fname)
  ;; Still unsure how to implement this,
  ;; See the discussion on github:
  ;;   https://github.com/bennn/iPoe/issues/10
  (displayln "NEW FORM not implemented"))

(define (new-word w)
  ;; Init database
  ;; Check if word exists
  ;; Interactively add word (get syllables, get rhymes)
  (parameterize-from-hash (options-init)
    (lambda ()
      (with-ipoe-db #:commit? #t
                    #:user (*user*)
                    #:dbname (*dbname*)
                    #:interactive? #t
        (lambda ()
          (cond
           [(word-exists? w)
            (alert (format "Word '~a' already exists, not adding." w))]
           [(add-word w #:syllables (get-syllables w)
                        #:interactive? #t
                        #:online? (*online?*))
            (alert "Success!")]
           [else
            (alert (format "Failed to add word '~a'" w))]))))))

;; -----------------------------------------------------------------------------
;; --- util

(define (get-syllables w)
  ;; No description
  (get-user-input read-natural
                  #:prompt (format "How many syllables does '~a' have?" w)))

