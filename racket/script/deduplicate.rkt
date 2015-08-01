#lang racket/base

;; De-duplicate lines in a file.
;; This is a poor substiture for the INSERT IGNORE statement.
;;
;; Walks over each line of a file, applies a filter, and checks if the filtered
;;  string has already been seen.
;; If not, the line gets saved to a temporary file.
;; At the end, the contents of the temporary file replace the original.

(provide
  deduplicate
  ;; (->* [String] [#:filter (-> String String)] Void)
  ;; Destructively remove duplicate lines from the file
  ;; Use filter to select parts of a line, by default is identity.

  but-last
  ;; Filter, all but last character
  first-word
  ;; Filter, everything up to first blank space
  ;; (Blank defined by string-split)
)

(require
  racket/set
  (only-in racket/string string-split)
  (only-in racket/system system))

;; =============================================================================

(define TMP "/tmp/deduplicate.rkt.tmp")

;; Copy input `from` to output `to`, omitting lines that appear twice.
;; The filter `f` is used to extract a "key" from each line.
;; (: copy-without-duplicates (-> Input-Port Output-Port #:filter (-> String String) Void))
(define (copy-without-duplicates from to #:filter f)
  (with-output-to-file to #:exists 'replace (lambda ()
    (with-input-from-file from (lambda ()
      (for/fold ([seen (set)])
                ([line (in-lines)]
                 #:when (not (set-member? seen (f line))))
        (displayln line)
        (set-add seen (f line)))
      (void))))))

;; A filter.
;; Get the first word of a line.
;; (: first-word (-> String String))
(define (first-word line)
  (car (string-split line)))

;; A filter, does nothing.
;; (: id (-> String String))
(define (id x) x)

;; A filter, gets all but the last character of a line.
;; (: but-last (-> String String))
(define (but-last x)
  (substring x 0 (sub1 (string-length x))))

;; Replace the contents of `file` with the same data minus duplicate lines.
(define (deduplicate file #:filter [f id])
  (copy-without-duplicates file TMP #:filter but-last)
  (system (format "mv ~a ~a" TMP file)))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "deduplicate"
    #:args (FILE)
    (when (file-exists? FILE)
      (deduplicate FILE #:filter but-last))))
