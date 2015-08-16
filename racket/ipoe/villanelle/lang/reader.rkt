#lang ipoe

#:name villanelle
#:rhyme-scheme ((R1 B R2)
                (A  B R1)
                (A  B R2)
                (A  B R1)
                (A  B R2)
                (A  B R1 R2))
#:syllables 10
#:extra-validator (lambda (stanza*)
                     (and (apply string=?
                            (for/list ([pair (in-list '((0 . 0) (1 . 2) (3 . 2) (5 . 2)))])
                              (apply string (for/list ([c (in-string (list-ref (list-ref stanza* (car pair)) (cdr pair)))] #:when (char-alphabetic? c))
                              (char-downcase c)))))
                          (apply string=?
                            (for/list ([pair (in-list '((0 . 2) (2 . 2) (4 . 2) (5 . 3)))])
                              (apply string (for/list ([c (in-string (list-ref (list-ref stanza* (car pair)) (cdr pair)))] #:when (char-alphabetic? c)) (char-downcase c)))))))
