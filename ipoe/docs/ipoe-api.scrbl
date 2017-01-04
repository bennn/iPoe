#lang scribble/manual
@require[
  scribble/example
  (for-label racket/base racket/contract racket/string ipoe/poetic-form sxml net/url)]

@title[#:tag "ipoe-api"]{iPoe API}

The following modules have been useful internally to @racketmodname[ipoe] and are stable enough to be used in (or ported to) other projects.
Hopefully, these will eventually re-appear in a more general context (@seclink["top" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{scribble}, @racketmodname[sxml], @url{pkgs.racket-lang.org}, @|etc|).


@section{English-Language Tools}
@(define english-eval (make-base-eval '(require ipoe/english)))

@defmodule[ipoe/english]{
  The @racketmodname[ipoe/english] module provides simple tools for working with the English language.
}

@defproc[(infer-rhyme=? [str1 string?] [str2 string?]) boolean?]{
  Guess whether the given words rhyme with one another.

  @examples[#:eval english-eval
    (infer-rhyme=? "cat" "hat")
    (infer-rhyme=? "sauce" "boss")
    (infer-rhyme=? "books" "carrots")]
}

@defproc[(infer-syllables [str string?]) exact-nonnegative-integer?]{
  Guess the number of syllables in the given word.

  Technically, the number of syllables in some words depends on their pronunciation (does ``flower'' have 1 or 2 syllables?), but this function just returns a natural number without thinking very hard.

  @examples[#:eval english-eval
    (infer-syllables "month")
    (infer-syllables "hour")
    (infer-syllables "munificent")]
}

@defproc[(integer->word* [i exact-integer?]) (listof string?)]{
  Converts an integer to a list of English words.
  Raises an exception (specifically, an @racket[exn:fail:contract?]) if @racket[(abs i)] is greater than an arbitrary (but pretty big) limit.

  @examples[#:eval english-eval
    (integer->word* 2)
    (integer->word* -21)]
}

@defproc[(integer->word/space [i exact-integer?]) string?]{
  Returns the same result as:
  @racketblock[
    (string-join (integer->word* i) " ")
  ]
}

@defproc[(suggest-spelling [str string?]
                           [#:max-distance d exact-nonnegative-integer? 2]
                           [#:limit n exact-nonnegative-integer? 10]) (listof string?)]{
  Return a list of common English words with Levenshtein distance (see the @racketmodname[levenshtein] package) at most @racket[d] from the given string.
  The result list has no more than @racket[n] elements.

  @examples[#:eval english-eval
    (suggest-spelling "tpyo")
    (suggest-spelling "balon" #:limit 3)
  ]
}


@section[#:tag "ipoe-api:form"]{Poetic Form API}

@defmodule[ipoe/poetic-form]{
  The @racketmodname[ipoe/poetic-form] module provides functions over the internal representation of an @tech{iPoe poem}.
}

@defproc[(poem? [x any/c]) boolean?]{
  A predicate for @tech{iPoe poems}.
}

@defproc[(stanza? [x any/c]) boolean?]{
  A predicate for @tech{iPoe stanzas}.
}

@defproc[(line? [x any/c]) boolean?]{
  A predicate for @tech{iPoe lines}.
}

@defproc[(word? [x any/c]) boolean?]{
  A predicate for @tech{iPoe words}.
}

@defproc[(stanza [i integer?] [p poem? (*current-poem*)]) stanza?]{
  Returns the stanza in poem @racket[p] at position @racket[i], where the first stanza is at position @racket[0].
}

@defproc[(line [i exact-nonnegative-integer?] [st stanza?]) line?]{
  Returns the line in stanza @racket[st] at position @racket[i].
}

@defproc[(word [i exact-nonnegative-integer?] [ln line?]) word?]{
  Returns the word in line @racket[ln] at position @racket[i].
}

@defproc[(line=? [ln1 line?] [ln2 line?]) boolean?]{
  Returns @racket[#true] if the given lines contain the same words.
}

@defproc[(word=? [w1 word?] [w2 word?]) boolean?]{
  Return @racket[#true] if the given words are equal.
}

@defproc[(line->word* [ln line?]) (sequence/c word?)]{
  Splits a line into words.
}

@defproc[(poem->stanza* [p poem?]) (sequence/c stanza?)]{
  Splits a poem into stanzas.
}

@defproc[(poem->word* [p poem?]) (sequence/c word?)]{
  Return a sequence of all words in the given poem.
}

@defproc[(stanza->line* [st stanza?]) (sequence/c line?)]{
  Return a sequence of lines in the given stanza.
}

@defproc[(poem-count-stanza* [p poem?]) exact-nonnegative-integer?]{
  Count the number of stanzas in the given poem.
}

@defproc[(stanza-count-line* [st stanza?]) exact-nonnegative-integer?]{
  Count the number of lines in the given stanza.
}

@defproc[(last-word [ln line?]) word?]{
  Returns the last word on the given line.
}

@defproc[(last-stanza [poem poem? *current-poem*]) stanza?]{
  Returns the last stanza of the given poem.
}

@defproc[(contains-word? [ln line?] [w word?]) boolean?]{
  Returns @racket[#true] if the given line contains the given word.
}

@defparam[*current-poem* p (or/c poem? #f) #:value #f]{
  Poetic forms defined using @hash-lang[] @racketmodname[ipoe] can assume this parameter holds the poem that is currently being checked.
}


@section[#:tag "ipoe-api:scrape"]{Web Scraping}
@(define scrape-eval (make-base-eval '(require ipoe/scrape racket/string sxml)))

@defmodule[ipoe/scrape]{
  The @racketmodname[ipoe/scrape] module provides tools for scraping word and rhyme data from the internet.
}

@defproc[(scrape-word [str string?]) (or/c word-result? #f)]{
  Search the internet for information about the given word.
}

@defproc[(word-result? [x any/c]) boolean?]{
  Predicate for the results of a successful word scrape.
}

@defproc[(word-scraper? [x any/c]) boolean?]{
  Predicate for a @tech{word scraper}.
}

@defproc[(make-word-scraper [sym symbol?]
                            [#:word->url word->url (-> string? string?)]
                            [#:sxml->definition sxml->defn (-> sxml (or/c string? #f))]
                            [#:sxml->num-syllables sxml->syll (-> sxml (or/c exact-nonnegative-integer? #f))]
                            [#:sxml->word sxml->word (-> sxml (or/c string? #f))])
         (-> string? (or/c word-result? #f))]{
  Builds a new @deftech{word scraper}, that is, an opaque structure that scrapes a fixed web resource for information on words.
  @itemlist[
  @item{
    @racket[sym] briefly describes the resource being scraped.
  }
  @item{
    @racket[word->url] converts a word to a URL for a web page that may have information about the word.
  }
  @item{
    @racket[sxml->defn] parses an @racket[sxml] page for the definition of a word.
  }
  @item{
    @racket[sxml->syll] parses an @racket[sxml] page for the number of syllables in a word.
  }
  @item{
    @racket[sxml->word] parses an @racket[sxml] page for the word the page describes.
  }
  ]
  If any of the above procedures fail, they should return @racket[#false].
  When any one procedure returns @racket[#false] during a call @racket[(scraper w)], then the overall result is @racket[#false].
}

@deftogether[(
  @defthing[dictionary.com word-scraper?]{}
  @defthing[american-heritage word-scraper?]{}
  @defthing[merriam-webster word-scraper?]{}
  @defthing[the-free-dictionary word-scraper?]{}
)]{
  Built-in @tech{word scrapers}.
}

@defproc[(scrape-rhyme [str string?]) rhyme-result?]{
  Search the internet for words that rhyme and @tech{almost rhyme} with the given word.
}

@defproc[(rhyme-result? [x any/c]) boolean?]{
  Prefab structure representing the results of scraping for rhymes.
}

@defproc[(almost-rhymes? [rr rhyme-result?] [str string?]) boolean?]{
  Return @racket[#true] if @racket[str] is listed in @racket[rr] as an ``almost rhyme''.
}

@defproc[(rhymes? [rr rhyme-result?] [str string?]) boolean?]{
  Return @racket[#true] if @racket[str] is listed in @racket[rr] as a rhyming word.
}

@defproc[(url->sxml [url (or/c url? string?)]) sxml:top?]{
  Send a GET request to the given @racket[url] and parse the result to SXML.
  Return the parsed response.
}

@defproc[(sxml-200? [sx sxml:top?]) boolean?]{
  Returns @racket[#true] if the given @racket[sxml] object represents a response to a successful HTML request.
  Use this on the result of @racket[url->sxml] to see if the request was successful.
}

@defproc[(id? [id-str string?]) (-> (listof sxml) any/c (listof sxml))]{
  Returns a procedure that filters @racket[sxml] elements, keeping only those elements with an @litchar{id} attribute with value @racket[id-str].

  I do not know the purpose of the second argument to the procedure returned by @racket[id?].

  @examples[#:eval scrape-eval
    ((sxpath `(// div ,(id? "foo") *text*))
     '(div
        (div (#,(string->symbol @"@") id "foo") "foo-text")
        (div (#,(string->symbol @"@") id "bar") "bar-text")))]
}

@defproc[(class? [class-str string?] [<? (-> string? string? boolean?) string=?]) (-> (listof sxml) any/c (listof sxml))]{
  Similar to @racket[id?], but filters based on the @litchar{class} attribute.
  Override the @racket[<?] argument to treat @racket[class-str] as a substring in the value of @racket[sxml] elements' class attribute.

  @examples[#:eval scrape-eval
    ((sxpath `(// div ,(class? "a" string-prefix?) *text*))
     '(div
        (div (#,(string->symbol @"@") class "abc") "success")
        (div (#,(string->symbol @"@") class "def") "failure")))]
}

@defproc[(contains-text? [pattern regexp?]) (-> (listof sxml) any/c (listof sxml))]{
  Similar to @racket[id?], but filters @racket[sxml] elements to keep only those whose @litchar{*text*} contents match the given @racket[pattern] (in the sense of @racket[regexp-match?]).
}

@defthing[scrape-logger logger?]{
  A logger that receives iPoe scraping events, such as rejected GET requests.
}

