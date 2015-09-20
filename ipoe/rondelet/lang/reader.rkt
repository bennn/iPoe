#lang ipoe

#:name rondelet
#:rhyme-scheme (((A . 4)
                 (B . 8)
                 (A . 4)
                 (A . 8)
                 (B . 8)
                 (B . 8)
                 (A . 4)))
#:extra-validator (lambda (s*)
                    (let ([s (stanza 0 s*)])
                      (line=? (line 0 s)
                              (line 2 s)
                              (line 6 s))))

