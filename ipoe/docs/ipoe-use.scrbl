#lang scribble/manual
@require[scribble/example (for-label racket/base racket/contract/base)]

@title[#:tag "ipoe-use"]{Using an iPoe Language}

@section[#:tag "ipoe-tour"]{A Guided Tour}

Let's write a haiku.
According to @hyperlink["https://en.wikipedia.org/wiki/Haiku_in_English"]{Wikipedia}:

@centered{@emph{
  A typical haiku is a three-line observation about a fleeting moment involving nature.
  .... with 17 syllables arranged in a 5--7--5 pattern.
}}

Here's a first shot:
@verbatim[#:indent 4]{
#lang ipoe/haiku

pit pat pit pat, rain?
or clicking on a keyboard?
it's 3 am I don't care anymore
}

Save the file as @litchar{./first.haiku} and run @exec{raco make first.haiku} to compile the poem.
Compiling will check the text of the poem against the @racketmodname[ipoe/haiku] @tech{poetic form}.

If this is your first time using @racketmodname[ipoe], you will see the following message:
@verbatim[#:indent 4]{
Missing run-time parameter for database username. Please enter a username to connect to the ipoe database.
Enter your database username (or #f to skip):
ipoe>
}

Just type @litchar{#f} and skip for now.
(If you're feeling brave, try running @exec{raco ipoe init} or browsing @secref{ipoe-use:pragmatics}.)
The next thing you will see is:

@verbatim[#:indent 4]{
Starting ipoe without a database connection (in online-only mode)
Word 1 on line 1 of stanza 0 ('clicking') is undefined. Consider adding it to the dictionary.
Word 4 on line 2 of stanza 0 ('dont') is undefined. Consider adding it to the dictionary.
ipoe: Expected 7 syllables on Line 1 of Stanza 0, got 5 syllables
  compilation context...:
     /Users/ben/code/racket/my-pkgs/ipoe/ipoe/first.rkt
}

Okay!
This means @racketmodname[ipoe] checked our poem and found a problem: the second line has 5 syllables but was expected to have 7 syllables.
The trouble is that @racketmodname[ipoe] couldn't figure out how many syllables are in the word ``clicking'' (by asking the internet, see the @racketmodname[ipoe/scrape] module).

We can't add ``clicking'' to the dictionary because we haven't set up a database.
But we can bypass the issue by claiming our @tech{poetic license}.
Change the poem to read:

@verbatim[#:indent 4]{
#lang ipoe/haiku
#:poetic-license 10

pit pat pit pat, rain?
or clicking on a keyboard?
it's 3 am I don't care no more
}

Now compile the poem; you should see:

@verbatim[#:indent 4]{
Missing run-time parameter for database username. Please enter a username to connect to the ipoe database.
Enter your database username (or #f to skip):
ipoe> #f
Starting ipoe without a database connection (in online-only mode)
Word 1 on line 1 of stanza 0 ('clicking') is undefined. Consider adding it to the dictionary.
Word 4 on line 2 of stanza 0 ('dont') is undefined. Consider adding it to the dictionary.
Finished checking poem.
- Misspelled word 'clicking'; position 1 on line 1 of stanza 0.
- Misspelled word 'dont'; position 4 on line 2 of stanza 0. Maybe you meant 'on'?
- Expected 7 syllables on Line 1 of Stanza 0, got 5 syllables
- Expected 5 syllables on Line 2 of Stanza 0, got 7 syllables
Remaining poetic license: 6
}

Success!
And now you've seen the basics of @racketmodname[ipoe]:
@itemlist[
@item{
  The @hash-lang[] line declares a @tech{poetic form}.
  Poetic forms can specify the number of lines, number of stanzas, rhyme scheme, and number of syllables in the following text.
  See @secref{ipoe-define} for more details.
}
@item{
  @racketmodname[ipoe] checks the internet to get the spelling, number of syllables, and rhymes for a given word.
  If you have an @tech{ipoe database}, it stores this information.
  Otherwise, the data for words is cached locally (in @litchar{./compiled/ipoe.cache}).
}
@item{
  Poems can start with a few keyword/value pairs to alter the poem-checker.
}
]


@section[#:tag "ipoe-tech"]{iPoe Concepts}

An @deftech{iPoe poem} is a text file with @litchar{#lang <spec>} as its first line, where @litchar{<spec>} refers to an iPoe @tech{poetic form}.

A @deftech{poetic form} is a grammar; it specifies how a class of poems should look and sound.

A @deftech{rhyme scheme} is an important part of a poetic form; it declares the number of stanzas, number of lines in each stanza, number of syllables in each line, and constrains certain lines to rhyme with one another.

A @deftech{iPoe stanza} is a group of lines.
Different stanzas are separated by at least 2 consecutive newline characters.

A @deftech{iPoe line} is a sequence of words.
Different lines are separated by a single newline character.

A @deftech{iPoe word} is basically a sequence of non-whitespace characters.
Different words are separated by at least one whitespace character.
(This definition is intentionally vague.)

@deftech{syllables} are ``units of pronunciation'' in a word (thanks Google).

Two words @deftech{rhyme} if they end with the same sound.

Two words @deftech{almost rhyme} if they kind-of-but-not-really end with the same sound.

The rules of a @tech{poetic form} are more like guidelines.
Good poetry doesn't always follow the rules, and poets have a @deftech{poetic license} to break the rules.
@margin-note{
How much license?
I don't know.
How much does it cost to break a rule?
I don't know, but I tried to set good defaults.
Have fun with this until we have something better.
}

An @deftech{iPoe database} stores known words, their properties, and the relation between them.
Once you have an iPoe database, it will update itself.
But it tends to ask for too much help, you can disable the help by putting @racket[#:interactive? #f] at the top of your poems or configuration file.


@subsection[#:tag "ipoe-forms"]{Built-in Poetic Forms}

@defmodulelang[ipoe/cinquain]
@defmodulelang[ipoe/clerihew]
@defmodulelang[ipoe/couplet]
@defmodulelang[ipoe/english-sonnet]
@defmodulelang[ipoe/free-verse]
@defmodulelang[ipoe/haiku]
@defmodulelang[ipoe/italian-sonnet]
@defmodulelang[ipoe/limerick]
@defmodulelang[ipoe/quaternion]
@defmodulelang[ipoe/rondelet]
@defmodulelang[ipoe/sestina]
@defmodulelang[ipoe/sextilla]
@defmodulelang[ipoe/tanaga]
@defmodulelang[ipoe/tercet]
@defmodulelang[ipoe/villanelle]

@section[#:tag "ipoe-use:pragmatics"]{Pragmatics}

@subsection[#:tag "ipoe-db"]{Connecting to an @tt{ipoe} Database}

If you install PostgreSQL and start a server, running @exec{raco ipoe init} should start an @tech{iPoe database}.

@subsection{Configuration}

Global configuration file: @racket[(build-path (find-system-path 'home-dir) ".ipoe")]

Local configuration file: @racket{./.ipoe}

There are many configuration options.
Put these in a configuration file or at the top of a poem.
Global configurations have the least precedence, in-file configurations have the most.

@itemlist[
@item{
  @racket[#:user string?] username for the @tech{iPoe database}
}
@item{
  @racket[#:dbname string?] database name for the @tech{iPoe database}
}
@item{
  @racket[#:interactive? boolean?] when @racket[#true], ask permission before peforming most actions.
  Otherwise, never ask for user input.
}
@item{
  @racket[#:online? boolean?] when @racket[#true], use the internet to look up information about words.
  Otherwise, never use the internet.
}
@item{
  @racket[#:spellcheck? boolean?] when @racket[#true], warn the user about spelling errors.
}
@item{
  @racket[#:grammarcheck? boolean?] when @racket[#true], warn the user about grammar errors (currently does nothing).
}
@item{
  @racket[#:suggest-rhyme? boolean?] when @racket[#true], offer suggestions to replace a word that doesn't rhyme.
}
@item{
  @racket[#:suggest-spelling? boolean?] when @racket[#true], offer suggestions to replace a mis-spelled word.
}
@item{
  @racket[#:poetic-license exact-nonnegative-integer?] set value of initial poetic license
}
@item{
  @racket[#:almost-rhyme-penalty (or/c exact-nonnegative-integer? #f)] set penalty for using an almost-rhyme.
  By default, @racket[1].
}
@item{
  @racket[#:bad-extra-penalty (or/c exact-nonnegative-integer? #f)] set penalty for failing a @racket[#:constraint].
  By default, @racket[10].
}
@item{
  @racket[#:bad-lines-penalty (or/c exact-nonnegative-integer? #f)] set penalty for using the wrong number of lines.
  By default, @racket[#false]; this immediately rejects poems with the wrong number of lines.
}
@item{
  @racket[#:bad-rhyme-penalty (or/c exact-nonnegative-integer? #f)] set the penalty for not rhyming.
  By default, @racket[3].
}
@item{
  @racket[#:bad-stanza-penalty (or/c exact-nonnegative-integer? #f)] set the penalty for using the wrong number of stanzas.
  By default, @racket[#false].
}
@item{
  @racket[#:bad-syllable-penalty (or/c exact-nonnegative-integer? #f)] set the penalty for using the wrong number of syllables.
  By default, @racket[1].
}
@item{
  @racket[#:repeat-rhyme-penalty (or/c exact-nonnegative-integer? #f)] set the penalty for using rhyming a word with itself.
  By default, @racket[3].
}
@item{
  @racket[#:spelling-error-penalty (or/c exact-nonnegative-integer? #f)] set the penalty for misspelling a word.
  By default, @racket[0].
}
]
