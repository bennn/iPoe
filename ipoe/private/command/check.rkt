#lang racket/base

;; Command-line alternative to calling
;;     racket FNAME.rkt
;; Explicitly on an ipoe poem

(provide
  check
  ;; (-> (Listof String) Void)
  ;; Compile a list of poems
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  ipoe/private/parameters
  ipoe/private/ui
  (only-in racket/string string-suffix?)
  (only-in racket/system system)
)

;; =============================================================================

(define-syntax-rule (file-exists?/alert fname)
  (or (file-exists? fname)
      (and (alert (format "File '~a' does not exist, cannot check." fname))
           #f)))

(define-syntax-rule (rkt?/alert fname)
  (or (string-suffix? fname ".rkt")
      (and (alert (format "File '~a' is not a Racket module, cannot check." fname))
           #f)))

;; -----------------------------------------------------------------------------

(define (check arg*)
  (command-line
   #:argv arg*
   ;#:once-each
   ;[("-l" "--license")
   ; l-param "Set poetic license" (*license* l-param)]
   ;[("-s" "--spellcheck")
   ; s-param "Toggle spellchecking" (*spellcheck?* s-param)]
   ;[("-r" "--rhymecheck")
   ; r-param "Toggle rhyme & syllable checking" (*rhymecheck?* r-param)]
   #:args ARG*
   (begin
     (for ([fname (in-list ARG*)]
           #:when (and (file-exists?/alert fname)
                       (rkt?/alert fname)))
       (alert (format "** Checking '~a'" fname))
       (system (format "racket ~a" fname))
       (newline)))))

;; =============================================================================

(module+ test
  (require rackunit)

)

