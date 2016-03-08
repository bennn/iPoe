#lang ipoe

#:name rondelet
#:rhyme-scheme (((A . 4)
                 (B . 8)
                 (A . 4)
                 (A . 8)
                 (B . 8)
                 (B . 8)
                 (A . 4)))
#:constraint
  (let ([S (stanza 0)])
    (line=? (line 0 S)
            (line 2 S)
            (line 6 S)))

