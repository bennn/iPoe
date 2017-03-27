#lang scribble/manual

@title[#:tag "top"]{iPoe: interactive poetry editor}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodulelang[ipoe]{
  The @racketmodname[ipoe] package is an extensible compiler for poetry.
}

You can use @racket[ipoe] to:
@itemlist[
@item{
  Check a poem against one of the built-in @tech{poetic forms} (@secref{ipoe-use}).
}
@item{
  Define a new poetic form (@secref{ipoe-define}).
}
@item{
  Add basic English-language processing to a Racket program (@secref{ipoe-api}).
}
]

Each poetic form is implemented as a Racket @hash-lang[].
The syntax of these languages is plain text; their semantics is to check the text for spelling, grammar, and other errors.

@bold{NOTE:} this package is unstable.
The purpose of this documentation is just to describe how @racketmodname[ipoe] fits together as of January 2017.

@include-section{ipoe-use.scrbl}
@include-section{ipoe-define.scrbl}
@include-section{ipoe-api.scrbl}
@include-section{ipoe-externals.scrbl}
