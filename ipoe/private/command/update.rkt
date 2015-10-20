#lang racket/base

;; TODO update all words (audit)

(provide
  update
)

;; -----------------------------------------------------------------------------

(require
  ipoe/private/parameters
  ipoe/private/ui
  ipoe/private/db
  racket/cmdline
)

;; =============================================================================

;; These names chosen to avoid conflicts with existing parameters
(define *almost-rhymes* (make-parameter '()))
(define *cmd-dbname* (make-parameter #f))
(define *cmd-user* (make-parameter #f))
(define *offline* (make-parameter #f))
(define *quiet* (make-parameter #f))
(define *rhymes* (make-parameter '()))
(define *syllables* (make-parameter #f))

(define (update arg*)
  (command-line
   #:argv arg*
   #:once-each
   [("-x" "--offline")
    "Run in offline mode" (*offline* #t)]
   [("-q" "--quiet")
    "Run quietly, aka non-interactively" (*quiet* #t)]
   [("-u" "--user") u-param
    "Set database user" (*cmd-user* u-param)]
   [("-d" "--dbname") d-param
    "Set database" (*cmd-dbname* d-param)]
   #:multi
   [("-a" "--almost-rhyme") a-param
    "Declare an almost-rhyme for the word" (*almost-rhymes*
                                             (cons a-param (*almost-rhymes*)))]
   [("-r" "--rhyme") r-param
    "Declare a rhyme for the word" (*rhymes* (cons r-param (*rhymes*)))]
   [("-s" "--syllables") s-param
    "Set number of syllables" (*syllables* (string->number s-param))]
   #:args (w)
   (let ([wid (parameterize-from-hash (options-init)
                (lambda ()
                  (define i (if (*quiet*) #f (*interactive?*)))
                  (define o (if (*offline*) #f (*online?*)))
                  (parameterize ([*interactive?* i])
                    (with-ipoe-db #:commit? #t
                                  #:user (or (*cmd-user*) (*user*))
                                  #:dbname (or (*cmd-dbname*) (*dbname*))
                      (lambda ()
                        (update-word w #:syllables (*syllables*)))))))])
     (when wid
         (printf "Successfully updated word '~a' (ID = ~a)\n" w wid)))))
