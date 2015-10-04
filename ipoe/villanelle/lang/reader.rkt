#lang ipoe

#:name villanelle
#:rhyme-scheme {[R1 B R2]
                [A  B R1]
                [A  B R2]
                [A  B R1]
                [A  B R2]
                [A  B R1 R2]}
#:syllables 10
;; All R1 lines must be equal,
;; and all R2 lines must be equal
#:constraint
  (line=? (line 0 (stanza 0))
          (line 2 (stanza 1))
          (line 2 (stanza 3))
          (line 2 (stanza 5)))
#:constraint
(line=? (line 2 (stanza 0))
        (line 2 (stanza 2))
        (line 2 (stanza 4))
        (line 3 (stanza 5)))
