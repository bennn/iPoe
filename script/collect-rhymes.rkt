#lang racket/base

;; Split a tabfile into two large files: word rhymes & word almost-rhymes
;; The input format is:
;;   WORD\tRHYME,RHYME,...\tALMOST,ALMOST,...
;; - Each "WORD" is a string
;; - Each "RHYME" is a string, these are comma-separated
;; - Each "ALMOST" is a string, these are also comma-separated
;; For a line with N rhymes and M almost-rhymes, we produce N+M lines of SQL
;;
;; The output is a large SQL insert statement, where each data line is
;;  the ID of the word and the ID of the rhyme or almost-rhyme.
;; All rhyme inserts go in one file.
;; All almost-rhyme inserts go into another.
;;
;; If we encounter a word/rhyme/almost that does not have an ID,
;;  it goes into an 'extra.rktd' file for later processing.

(require glob
         (only-in racket/match match-define)
         (only-in racket/string string-split)
         (only-in racket/format ~r)
         "../ipoe/private/db.rkt")

;; =============================================================================

;; Log a missing word `w` to the output port `f`.
;; The tag `t` communicates whether `w` is a missing word, rhyme, or almost-rhyme
;; (: missing (-> String #:tag Symbol #:out Output-Port Void))
(define (missing w #:tag t #:out f)
  (fprintf f "(~a ~a)\n" t w))

;; Print a bunch of tuples to the output port `f`.
;; - `word*` is a list of words to print (as the second component of the tuples)
;; - `#:tag t` is the type of insert we're doing. This is used for error reporting.
;; - `#:conn c` is the database connection, used to retrive word IDs
;; - `#:output f` file to output tuples to
;; - `#:wid id` first component of every printed tuple
;; - `#:missing f-miss` output port for error messages
(define (print-pairs word* #:tag t #:conn pgc #:output f #:wid id #:missing f-miss)
  (for ([r (in-list word*)])
    (define rid (word->id pgc r))
    (cond
     [rid
      (fprintf f ",\n")
      (fprintf f "(~a, ~a)" id rid)]
     [else
      (missing (format "~a ~a" id r) #:out f-miss #:tag t)])))

;; Convert a natural number to a sql filename
(define (nat->filename n)
  (format "../migrate/~a.sql" (~r n #:min-width 4 #:pad-string "0")))

;; Increment a box
(define-syntax-rule (incr b)
  (set-box! b (add1 (unbox b))))

;; A large, gross script for starting the database connection, opening I/O,
;; and iterting over the rhymes & almost rhymes while recording errors.
(define (main arg*)
  ;; -- init files and connection
  (define pgc (db-init))
  (define f-extras (open-output-file "extras.rktd" #:exists 'replace))
  (fprintf f-extras "(\n")
  (define counter (box 5))
  ;; -- process files
  (for ([fname (in-vector arg*)])
    (printf "Processing '~a'...\n" fname)
    ;; -- headers
    (define f-rhyme (open-output-file (nat->filename (unbox counter)) #:exists 'replace))
    (incr counter)
    (fprintf f-rhyme "INSERT INTO word_rhymes (word, rhyme) VALUES\n")
    (define f-almost-rhyme (open-output-file (nat->filename (unbox counter)) #:exists 'replace))
    (incr counter)
    (fprintf f-almost-rhyme "INSERT INTO word_almost_rhymes (word, almost_rhyme) VALUES\n")
    ;; -- process input
    (with-input-from-file fname
      (lambda ()
        (for ([line (in-lines)])
          (match-define (list word rhyme-str almost-rhyme-str) (string-split line "\t" #:trim? #f))
          (define wid (word->id pgc word))
          (cond
           [wid
            (define rhyme* (string-split rhyme-str ","))
            (define almost-rhyme* (string-split almost-rhyme-str ","))
            (print-pairs rhyme*
                         #:tag 'rhyme
                         #:conn pgc
                         #:output f-rhyme
                         #:wid wid
                         #:missing f-extras)
            (print-pairs almost-rhyme*
                         #:tag 'almost-rhyme
                         #:conn pgc
                         #:output f-almost-rhyme
                         #:wid wid
                         #:missing f-extras)]
           [else
            (missing word #:tag 'word #:out f-extras)]))))
    (fprintf f-rhyme ";\n")
    (fprintf f-almost-rhyme ";\n")
    (close-output-port f-rhyme)
    (close-output-port f-almost-rhyme))
  ;; -- footers
  (fprintf f-extras ")\n")
  (close-output-port f-extras))

;; =============================================================================

(module+ main
  (main (current-command-line-arguments)))
