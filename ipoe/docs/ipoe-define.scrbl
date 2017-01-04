#lang scribble/manual
@require[
  scribble/example
  scribble/bnf
  (for-label racket/base racket/contract (only-in syntax/parse expr))
]

@title[#:tag "ipoe-define"]{Defining a new iPoe Language}

@defmodulelang[ipoe]{
  The @racketmodname[ipoe] language is a specification language for @tech{poetic forms}.
}

The syntax accepted by the @hash-lang[] @racketmodname[ipoe] reader is keyword-based:
@(let ([lp @litchar{(}] [rp @litchar{)}])
@BNF[
  (list @nonterm{stmt}
    @BNF-seq[@racket[#:name] @racket[identifier]]
    @BNF-seq[@racket[#:description] @racket[string?]]
    @BNF-seq[@racket[#:syllables] @racket[exact-positive-integer?]]
    @BNF-seq[@racket[#:rhyme-scheme] @nonterm{rhyme-scheme}]
    @BNF-seq[@racket[#:constraint] @nonterm{constraint-expr}])
  (list @nonterm{rhyme-scheme} @BNF-seq[lp @kleenestar[@nonterm{stanza-scheme}] rp])
  (list @nonterm{stanza-scheme} @BNF-seq[lp @kleenestar[@nonterm{line-scheme}] rp])
  (list @nonterm{line-scheme} @BNF-seq[lp @nonterm{rhyme} @litchar{.} @nonterm{syllable} rp]
                              @nonterm{syllable}
                              @nonterm{rhyme})
  (list @nonterm{rhyme} @racket[symbol?] @nonterm{wildcard})
  (list @nonterm{syllable} @racket[exact-positive-integer?] @nonterm{wildcard})
  (list @nonterm{wildcard} @litchar{*})
  (list @nonterm{constraint-expr} @racket[expr])])

In summary, a @hash-lang[] @racketmodname[ipoe] program is a sequence of statements.
@itemlist[
@item{
  The @racket[#:name] statement @emph{must} appear once.
  This statement declares the name of the poetic form (currently only used for debugging).
}
@item{
  The @racket[#:description] statement may appear at most once; it gives a brief description of the poetic form (for debugging).
}
@item{
  The @racket[#:syllables] statement may appear at most once.
  When @racket[#:syllables N] is given, any wildcard syllables in the @nonterm{rhyme-scheme} (implicit or explicit) are replaced with @racket[N].
}
@item{
  The @racket[#:rhyme-scheme] statement may appear at most once.
  A @nonterm{rhyme-scheme} specifies:
  @itemlist[
  @item{
    the number of stanzas in the poetic form,
  }
  @item{
    the number of lines in each stanza,
  }
  @item{
    the number of syllables in each line, and
  }
  @item{
    which lines must rhyme with one another.
  }
  ]
  An empty rhyme scheme allows any number of stanzas.
  A @nonterm{line-scheme} that omits the @nonterm{rhyme} or @nonterm{syllable} component has an implicit wildcard in that component.
  A @nonterm{rhyme} can be any symbol; lines that must rhyme must have equal symbols (in the sense of @racket[eq?]).
}
@item{
  The @racket[#:constraint] statement may appear any number of times.
  Each statement may contain arbitrary expressions using (most!) identifiers from the @racketmodname[racket/base] and @racketmodname[ipoe/poetic-form] namespaces.
}
]

@bold{NOTE:} The syntax of @nonterm{constraint-expr} nonterminals is bad IMO.
Ideally it should be a simple, Racket-like domain-specific-language.
Allowing anything in @racketmodname[racket/base] is probably too much power, but @racket[ipoe/poetic-form] by itself isn't enough to express the constraints in certain poems (@racketmodname[ipoe/sestina]).

@section[#:tag "ipoe-define:install"]{Installing a Language}

@subsection{The Fast Way}
Suppose you have a @hash-lang[] @racketmodname[ipoe] program with name @litchar{my-form}.
The easiest way to start writing @litchar{my-form} poems is:
@itemlist[#:style 'ordered
@item{
  Install the @hyperlink["https://pkgd.racket-lang.org/pkgn/package/gnal-lang"]{gnal-lang} package.
}
@item{
  Create a directory @litchar{my-form/} and a sub-directory @litchar{my-form/lang/}.
}
@item{
  Save your program as @litchar{my-form/lang/reader.rkt}.
}
]

Now you can write programs using the new poetic form:

@verbatim[#:indent 4]|{
#lang gnal "my-form"

Racket is fun because #:fun is a keyword!
}|

Compiling the program will check the text against the @litchar{my-form} specification.


@subsection{The Franchised Way}
A longer but prettier method is:
@itemlist[#:style 'ordered
@item{
  Create the @litchar{my-form/lang/} directory and save the @racketmodname[ipoe] program as @litchar{my-form/lang/reader.rkt}.
}
@item{
  Run @exec{raco pkg install ./my-form}.
}
@item{
  Write programs that start with @tt{#lang my-form}.
}
]

@subsection{The Directory-Free Way}

@bold{TODO} should be possible to defined a @racket[reader] submodule, but this doesn't work right now!



@section[#:tag "ipoe-define:examples"]{Example Languages}

Browse the source code for more examples.

@hash-lang[] @racket[ipoe/haiku]
@codeblock|{
#lang ipoe

#:name haiku
#:rhyme-scheme {[5 7 5]}
#:description
"1 stanza, 3 lines
No rhyme constraints
5 syllables in first line
7 in second
5 in last"
}|

@hash-lang[] @racket[ipoe/couplet]
@codeblock|{
#lang ipoe

#:name couplet
#:rhyme-scheme {[A A]}
}|

@hash-lang[] @racket[ipoe/english-sonnet]
@codeblock|{
#lang ipoe

#:name english-sonnet
#:syllables 10
#:rhyme-scheme {[A B A B]
                [C D C D]
                [E F E F]
                [G G]}
}|

@hash-lang[] @racket[ipoe/villanelle]
@codeblock|{
#lang ipoe

#:name villanelle
#:rhyme-scheme {[R1 B R2]
                [A  B R1]
                [A  B R2]
                [A  B R1]
                [A  B R2]
                [A  B R1 R2]}
#:syllables 10
#:constraint
  (line=? (line 0 (stanza 0))
          (line 2 (stanza 1))
          (line 2 (stanza 3))
          (line 2 (stanza 5)))
#:constraint
  (line=? (line 2 (stanza 0))
          (line 2 (stanza 2))
          (line 2 (stanza 4))
          (line 3 (stanza 5)))
}|
