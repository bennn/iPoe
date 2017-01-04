#lang reprovide

(only-in ipoe/private/poem/rhymecheck
  rhyme-scheme?)

(only-in ipoe/private/poem/poetic-license
  quirk?)

(only-in ipoe/private/poem
  [*poem* *current-poem*]

  poem?
  [stanza/loc? stanza?]
  [line/loc? line?]
  [word/loc? word?]

  contains-word?
  last-word
  last-stanza
  line
  line=?
  line->word*
  poem-count-stanza*
  poem->stanza*
  [poem->word/loc* poem->word*]
  stanza
  [stanza-count-lines stanza-count-line*]
  stanza->line*
  word
  word=?)
