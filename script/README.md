Script
======

Non-essential scripts, mostly for handling database imports.
Committed in case they can be re-used later.

- `chunk-file.rkt` Split a large SQL insert statement into a series of
  smaller ones in different files.
- `collect-rhymes.rkt` Process a plaintext .tab file into SQL insert statements
- `collecy_syllables.py` Process a plaintext .tab file, this time for syllables
  rather than rhymes.
- `deduplicate.rkt` Remove duplicate lines in a file.
