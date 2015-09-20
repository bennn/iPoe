#lang racket/base

;; Command-line interface to the DB repl

;; TODO it's time to get serious here and break this into helper functions

(provide
  db
  ;; (-> '() Void)
  ;; Start a fresh REPL for the database
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  ipoe/private/db
  racket/match
)

;; =============================================================================

(define (db arg*)
  ;; -- parameters
  (define commit? (make-parameter #f))
  (define output-file (make-parameter #f))
  ;; --
  (define natural? exact-nonnegative-integer?)
  (define (exit? s)
    (memq s '(exit q quit)))
  (define (skip n seq)
    (for ([x seq] [m (in-range n)]) (void)))
  (define (take n seq)
    (for/list ([x seq] [m (in-range n)]) x))
  ;; -- repl
  (command-line
   #:argv arg*
   ;#:once-each
   ; [("-c" "--commit") "Commit to database" (commit? #f)]
   ; [("-o" "--output") o-p "Save interactions to file" (output-file o-p)]
   #:args ()
   (begin
     (printf "Initializing DB connection & starting REPL ...\n")
     (with-ipoe-db #:commit? #f ;;(commit?)
       (lambda ()
         (let loop ()
           (display "ipoe> ")
           (match (read)
            [(or (? eof-object?)
                 (? exit?)
                 (and (? list?) (? (lambda (x) (exit? (car x))))))
             (printf "Goodbye\n")]
            ['help
             (displayln "Available commands:")
             (for ([cmd (in-list '(id->word rhymes-with syllables->word*
                                   word-exists? word->syllables word->rhyme*
                                   word->almost-rhyme*))])
               (printf "- ~a\n" cmd))
             (loop)]
            [(list 'id->word (? natural? n))
             (displayln (id->word n))
             (loop)]
            [(list 'rhymes-with? (? string? sym1) (? string? sym2))
             (displayln (rhymes-with? sym1 sym2))
             (loop)]
            [(list 'syllables->word* (? natural? n1) '#:limit (? natural? n2))
             (displayln (take n2 (syllables->word* n1)))
             (loop)]
            [(list 'syllables->word* (? natural? n1) '#:limit (? natural? n2) '#:skip (? natural? n3))
             (define s (syllables->word* n1))
             (skip n3 s)
             (displayln (take n2 s))
             (loop)]
            [(list 'word->almost-rhyme* (? string? w) '#:limit (? natural? n))
             (displayln (take n (word->almost-rhyme* w)))
             (loop)]
            [(list 'word->almost-rhyme* (? string? w) '#:limit (? natural? n) '#:skip (? natural? n2))
             (define s (word->almost-rhyme* w))
             (skip n2 s)
             (displayln (take n s))
             (loop)]
            [(list 'word->id (? string? w))
             (displayln (word->id w))
             (loop)]
            [(list 'word->rhyme* (? string? w) '#:limit (? natural? n1))
             (displayln (take n1 (word->rhyme* w)))
             (loop)]
            [(list 'word->rhyme* (? string? w) '#:limit (? natural? n1) '#:skip (? natural? n2))
             (define s (word->rhyme* w))
             (skip n2 s)
             (displayln (take n1 s))
             (loop)]
            [(list 'word->syllables (? string? w))
             (displayln (word->syllables w))
             (loop)]
            [(list 'word-exists? (? string? w))
             (displayln (word-exists? w))
             (loop)]
            [x
             (printf "Unknown command '~a'\n" x)
             (loop)])))))))

