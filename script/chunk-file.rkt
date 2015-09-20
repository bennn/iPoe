#lang racket/base

;; Split one large SQL insert statement into smaller ones.
;; Command-line arguments are:
;; - Required <filename>
;; - Optional [-i N], the natural-number index to start naming new files with.
;; - Optional [-c N], the number of lines in each new file

(require
  "deduplicate.rkt"
  (only-in racket/format ~r))

;; =============================================================================

;; Convert a number N to a filename `N.sql`,
;; padded with 0 if `N` has fewer than 4 digits.
;; (: number->filename (-> Natural Path-String))
(define (number->filename n)
  (format "~a.sql" (~r n #:min-width 4 #:pad-string "0")))

;; Convert an `N.sql` filename to a number
;; (: filename->number (-> Path-String Natural))
(define (filename->number fname)
  (string->number (substring fname 0 4)))

;; Break a large SQL file into smaller chunks.
;; Each chunk has the same first line as the original file,
;;  and we try (often unsuccessfully) to end the last line with a semicolon.
;; (: chunk-file (->* [Path-String #:index Natural] [#:chunk-size Natural] Void))
(define (chunk-file fname #:index i
                          #:chunk-size [csize 500000])
  (with-input-from-file fname
    (lambda ()
      (define first-line (read-line))
      ;; -- loop, creating one new chunk file at each iteration
      ;; (: loop (-> Natural Void))
      (let loop ([index i])
        (define outfile (number->filename index))
        (printf "Creating file '~a'...\n" outfile)
        (define loop? (box #f))
        (with-output-to-file outfile
          (lambda ()
            (displayln first-line)
            ;; -- Take the next `chunk-size` lines, or the rest of the file
            (for ([x (in-range csize)]
                  [line (in-lines)])
              (displayln line))
            (define last-line (read-line))
            ;; -- continue looping unless we hit EOF
            (unless (eof-object? last-line)
              (displayln (string-append (substring last-line 0 (sub1 (string-length last-line))) ";"))
              (set-box! loop? #t))))
        (deduplicate outfile #:filter but-last)
        (when (unbox loop?) (loop (add1 index)))))))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (define index (make-parameter 31))
  (define chunk-size (make-parameter 1000000))
  (command-line
    #:once-each
    [("-i" "--index") i-p "Start index" (index (string->number i-p))]
    [("-c" "--chunk-size") c-p "Chunk size" (chunk-size (string->number c-p))]
    #:args (FILE)
    (begin
     (chunk-file FILE #:chunk-size (chunk-size) #:index (index)))))
