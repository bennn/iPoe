#lang racket

(require db)

(define pgc (postgresql-connect #:user "ben" #:database "ipoe"))

(query-exec pgc "CREATE TABLE word (
                   id serial PRIMARY KEY,
                   word text UNIQUE NOT NULL,
                   num_syllables integer NOT NULL);")

(query-exec pgc "CREATE TABLE word_rhymes (
                   word integer REFERENCES word(id),
                   rhyme integer REFERENCES word(id),
                   PRIMARY KEY (word, rhyme));")

(query-exec pgc "CREATE TABLE word_almost_rhymes (
                   word integer REFERENCES word(id),
                   almost_rhyme integer REFERENCES word(id),
                   PRIMARY KEY (word, almost_rhyme));")

