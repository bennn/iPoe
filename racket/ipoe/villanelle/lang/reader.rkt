#lang ipoe

#:name villanelle
#:rhyme-scheme ((R1 B R2)
                (A  B R1)
                (A  B R2)
                (A  B R1)
                (A  B R2)
                (A  B R1 R2))
#:syllables 10
#:extra-validator (lambda (s*)
                    ;; All R1 lines must be equal,
                    ;; and all R2 lines must be equal
                    (and (line=? (line 0 (stanza 0 s*))
                                 (line 2 (stanza 1 s*))
                                 (line 2 (stanza 3 s*))
                                 (line 2 (stanza 5 s*)))
                         (line=? (line 2 (stanza 0 s*))
                                 (line 2 (stanza 2 s*))
                                 (line 2 (stanza 4 s*))
                                 (line 3 (stanza 5 s*)))))
