#lang ipoe

#:name villanelle
#:rhyme-scheme {[R1 B R2]
                [A  B R1]
                [A  B R2]
                [A  B R1]
                [A  B R2]
                [A  B R1 R2]}
#:syllables 10
#:extra-validator (lambda (P)
                    ;; All R1 lines must be equal,
                    ;; and all R2 lines must be equal
                    (append
                      (line=? (line 0 (stanza 0 P))
                              (line 2 (stanza 1 P))
                              (line 2 (stanza 3 P))
                              (line 2 (stanza 5 P)))
                      (line=? (line 2 (stanza 0 P))
                              (line 2 (stanza 2 P))
                              (line 2 (stanza 4 P))
                              (line 3 (stanza 5 P)))))
