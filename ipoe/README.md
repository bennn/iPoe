ipoe
====

Implementation of `#lang ipoe`
and some example poetic forms.

Boring Files:
- `info.rkt` package metadata
- `main.rkt` front-end for `raco ipoe`

API Files
- `english.rkt` English-language utilities
- `poetic-form.rkt` actions on the internal representation of a poem
- `prompt.rkt` for command-line interaction
- `scrape.rkt` for collecting words, definitions, syllables, and rhymes from the internet

Directories:
- `docs/` holds documentation (Scribblings)
- `lang/` the reader for `#lang ipoe`
- `private/` iPoe implementation
- other directories implement poetic forms, e.g. `haiku/` implements `#lang ipoe/haiku`
