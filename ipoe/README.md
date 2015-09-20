ipoe
====

Library & languages.

(2015-09-19) This badly needs an update



#lang s-exp ipoe
----------------
Module reader for definining poetic structure.

For now, expects a rhyme scheme, exactly.

TODO
- settle on module reader format
- add an example


Rhyme scheme
------------

- List of lists
- Inner lists represent stanzas
- Elements of inner lists represent lines
  - lines are pairs, first component is rhyme & second is syllables
  - rhymes are `([a-z]*[A-Z]*)+` symbols, or any other symbol as a wildcard
    (TODO will also want wildcard num lines, or stanzas,
     so probably want a general way to specify them
     instead of just "use '*, or '?, or mix whatever you feel like)

```
<rhyme-scheme>  ::= Listof <stanza-scheme>
<stanza-scheme> ::= Listof <line-scheme>
<line-scheme>   ::= Pairof <rhyme> <num_syllables>
<rhyme>         ::= <wildcard> | Symbol
<num_syllables> ::= <wildcard> | Natural
<wildcard>      ::= '*
```

Examples are in the folders.
TODO move examples here
