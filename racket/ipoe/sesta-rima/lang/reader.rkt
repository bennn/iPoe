#lang ipoe

#:name sesta-rima
#:description "The Sesta Rima (song of sixes) has six stanzas of six lines,
  followed by a 3-line ending. There is no rhyme scheme, but the
  words ending each line of the first stanza must appear as the
  end words of lines in successive stanza following a rotating
  pattern. Furthermore the 3-line end stanza must contain 2 of
  these end words on each line."
#:rhyme-scheme ((* * * * * *)
                (* * * * * *)
                (* * * * * *)
                (* * * * * *)
                (* * * * * *)
                (* * * * * *)
                (* * *))
#:extra-validator (lambda (s*)
                    (and
                      ;; Word constraints (after position 0, want +1, +3, +4, +1, +2
                      (for/and ([l+s* (in-list '(((0 . 0) (1 . 1) (3 . 2) (4 . 3) (2 . 4) (5 . 5))
                                                 ((1 . 0) (3 . 1) (4 . 2) (2 . 3) (5 . 4) (0 . 5))
                                                 ((2 . 0) (5 . 1) (0 . 2) (1 . 3) (3 . 4) (4 . 5))
                                                 ((3 . 0) (4 . 1) (2 . 2) (5 . 3) (0 . 4) (1 . 5))
                                                 ((4 . 0) (2 . 1) (5 . 2) (0 . 3) (1 . 4) (3 . 5))
                                                 ((5 . 0) (0 . 1) (1 . 2) (3 . 3) (4 . 4) (2 . 5))))])
                        ;;(printf "Checking ~a\n" l+s*)
                        (apply word=? (map (lambda (l+s) (last-word (line (car l+s) (stanza (cdr l+s) s*)))) l+s*)))
                      ;; Tercet constraints.
                      ;; A specific pair of words must appear in each line the second
                      ;;  in each pair must be the last word in the line.
                      (for/and ([ln (in-list (stanza->line* (last-stanza s*)))]
                                [fw (in-list '(1 3 5))]
                                [lw (in-list '(4 2 0))])
                        (and (contains-word? ln (last-word (line fw (stanza 0 s*))))
                             (word=? (last-word ln) (last-word (line lw (stanza 0 s*))))))))

;; The examples I've seen only use the 6 words in the last line.
;; There's not as strict about the order, but
;; - gotta be unique
;; - each line must end with one
;; http://www.thehypertexts.com/Best%20Sestinas.htm

