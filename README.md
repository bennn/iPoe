iPoe
====
[![Build Status](https://travis-ci.org/bennn/ipoe.svg)](https://travis-ci.org/bennn/ipoe)
[![Coverage Status](https://coveralls.io/repos/bennn/ipoe/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/ipoe?branch=master)

Interactive POetry Editor

It's a text editor for poetry!

[[ Insert screenshot here ]]

Currently provides:
- Validation for rhyme, syllable counts, line counts, and stanza counts
- A specification language for poetic forms

Scroll to the bottom to see future plans.


Requirements
------------

1. Install a recent development release of Racket (v6.2.900.17)
2. Clone and install this repository:
  ```git clone https://github.com/bennn/ipoe; raco pkg install ./ipoe```
3. (Optional, but recommended) Install postgres.

If you skip step 3 that's fine, we just save data on words locally, in your current directory.


Getting Started
---------------
1. Run `raco ipoe init` to initialize a database
2. Check out the built-in poetic forms [[TODO how?]]
3. Write a poem: `hello.rkt`
4. Run `racket hello.rkt`


Future Work
-----------
High-concern issues to deal with later.
These are also on the [issues](https://github.com/bennn/iPoe/issues) page.

#### Sharing & Collaboration
- Submit poems, poetic forms, and words to a central repository

#### Metre Checking
- Real iambic pentameter is more than "10 syllables per line"

#### Semantics
- Include word meaning in the analysis, especially when giving suggestions
- Use [WordNet](https://wordnet.princeton.edu/)

#### Algorithms for Rhyme and Syllables
- Especially a problem for rhymes (syllable information is more accurate)
- Use the dictionary pronunciations / phonics

