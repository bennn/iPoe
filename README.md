iPoe
====

Interactive POetry Editor

Overview
--------

Spellcheck is great.
Grammar check is great.

iPoe aims to provide rhythm, meter, rhyme, and form checks for common poetic formats like sonnets and villanelles.
It seems the only thing in the way of that is an accurate database of words and phonics.

This may take a while.


Poetry editor, implemented as a Racket language.

To start the database, use the command:
```
su postgres -c 'pg_ctl start -D /var/lib/postgres/data -l /tmp/PGSQL.log'
```
