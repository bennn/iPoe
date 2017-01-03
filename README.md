iPoe
====
[![Build Status](https://travis-ci.org/bennn/iPoe.svg)](https://travis-ci.org/bennn/iPoe)
[![Coverage Status](https://coveralls.io/repos/bennn/iPoe/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/iPoe?branch=master)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/ipoe/index.html)

Interactive POetry Editor

It's a ~~text editor~~ compiler for poetry!

```
$ cat test.txt
#lang ipoe/couplet

Humpty Dumpty sat on the wall
Humpty Dumpty had a great big head

$ raco make test.txt
ipoe: Expected a word to rhyme with 'wall', got 'head' (word 6, line 1, stanza 0)
```

#### Features:
- Checking for rhyme, syllables, stanza counts, and line equality
- Specification language (`#lang ipoe`) for defining new poetic forms

Here is the specification for `#lang ipoe/couplet`.

```
#lang ipoe
#:name couplet
#:rhyme-scheme {[A A]}
```

That's 1 stanza with 2 rhyming lines with any number of syllables.


## Install

0. Install Racket v6.3 or later ([download](http://download.racket-lang.org/))
1. Install this package
  - from pkgs.racket-lang.org : `$ raco pkg install ipoe`
  - or from GitHub : `$ git clone https://github.com/bennn/ipoe; raco pkg install ./ipoe`
3. (optional) Install PostgreSQL ([download](https://www.postgresql.org/download))
4. Connect to the internet


## Getting Started

1. (if `postgresql` is running) Run `raco ipoe init` to initialize a database
2. Browse the built-in poetic forms (`raco ipoe show`)
3. Write a poem, e.g. `hello.rkt`
4. Run `racket hello.rkt`


## Future Work

Check the [issue tracker](https://github.com/bennn/iPoe/issues) for more.

#### Sharing & Collaboration
- Submit poems, poetic forms, and words to a central repository

#### Metre Checking
- Real iambic pentameter is more than "10 syllables per line"

#### Semantics
- Include word meaning in the analysis, especially when giving suggestions
- Try [WordNet](https://wordnet.princeton.edu/)

#### Algorithms for Rhyme and Syllables
- Especially a problem for rhymes (syllable information is more accurate)
- Use the dictionary pronunciations / phonics

#### Learning
- Instead of using the internet / the user, parse Shakespeare's works and infer what words rhyme etc.
