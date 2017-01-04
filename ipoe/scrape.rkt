#lang reprovide

(only-in ipoe/private/scrape/scrape-rhymes
  rhyme-result?
  almost-rhymes?
  rhymes?
  scrape-rhyme)

(only-in ipoe/private/scrape/scrape-util
  url->sxml
  sxml-200?
  id?
  class?
  contains-text?
  scrape-logger)

(only-in ipoe/private/scrape/scrape-words
  word-result?
  word-scraper?
  make-word-scraper
  scrape-word
  dictionary.com
  american-heritage
  merriam-webster
  the-free-dictionary)
