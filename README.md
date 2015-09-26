iPoe
====

Interactive POetry Editor

It's a text editor for poetry!

[[ Insert screenshot here ]]


Requirements
------------

1. Install a recent development release of Racket (v6.2.900.17)
2. Clone and install this repository: `git clone https://github.com/bennn/ipoe; raco pkg install ./ipoe`
3. (Optional, but recommended) Install postgres.

If you skip step 3 that's fine, we just save data on words locally, in a hashtable.


Getting Started
---------------
1. Run `raco ipoe init` to initialize a database
2. Check out the built-in poetic forms [[TODO how?]]
3. Write a poem: `hello.rkt`
4. Run `racket hello.rkt`

