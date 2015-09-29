mlang scribble/sigplan


A #lang For All Seasons


@section{Introduction}

Racket is a programming-language programming language.
It gives developers the tools to express and solve problems at the @emph{language} level, rather than as formulas in a fixed logic, type system, or operational semantics.
These language-building tools enable direct, concise solutions.

We demonstrate by example with the @emph{iPoe} programming language: a system for @underline{i}nteractive @underline{po}etry @underline{e}diting.
On the surface, iPoe is a convenient tool for writing poems in a known style, such as sonnets, limericks, and haikus.
One level deeper, iPoe provides a specificiation language for defining new poetic styles.
Our thesis is that students and practicing poets alike can benefit from this tool @emph{despite having no knowledge of programming languages}.
This is precisely because Racket lets us express the system at a high level of abstraction.


@section{Example: Couplet}

The following couplet is by Alexander Pope [CITE].
Prepending a language declaration @racket{#lang ipoe/couplet} above the couplet makes a complete @ipoe program.

    Hope spings eternal in the human breast,
    Man never is, but always to be blest.

Like all couplets of Pope's day, it is composed of two rhyming lines of iambic pentameter.
The specification languge @racket{ipoe/couplet} captures the essence of this poetic form:

    #lang ipoe

    #:name couplet
    #:description "Two rhyming lines, 10 syllables each."
    #:rhyme-scheme {[ (A . 10)
                      (A . 10) ]}

As the in-program description states, a couplet is composed of:
@itemlist[
  @item{One stanza, delimited by square brackets}
  @item{Two lines, each delimited by parentheses}
  @item{A constraint on the rhyme and meter of each line, expressed as a symbol (A) and natural number (10).}
]
The symbol @racket{A} is a @emph{rhyme variable}.
When we match this specification against two lines of text, this rhyme variable is bound by the last word @math{w} of the first line.
The second line is then constrained to end with a word that rhymes with @math{w}.
In our example, the rhyme constraints translate to the proposition: ``@emph{breast} rhymes with @emph{blest}''.

The integer 10 used to encode syllables is more straightforward.
It simply requires that the sum total of syllables in all words of the specified line is exactly 10.
We compute syllables just as we compute rhymes: by asking a trusted internet authority@cite{rhymebrain} and caching the result in a local database.

Unfortunately, we cannot express the finer constraint that Pope's couplets were written in iambic pentameter (meaning that 5 of the 10 syllables of each line are stressed, and the other 5 relatively silent).
Nor can we express the opinion that this is a particularly good couplet.
We leave these ideas to future work.


@section{Example: Sonnet}
While couplets are poems in their own right, they often appear as part of a larger poem.
The English (or Shakespearian) Sonnet, for example, always ends with a couplet:

    Shall I compare thee

We can specify English Sonnets using the same techniques from the couplet specification.
For brevity, however, we use the new directive @racket{#:syllables} to declare that all lines, unless otherwise specified, should contain 10 syllables.

    #lang ipoe

    #:name english-sonnet
    #:description "Five stanzas of iambic pentameter, followed by a couplet."
    #:syllables 10
    #:rhyme-scheme {
      TODO
      TODO
    }

Note that we did not use the specification of couplets directly in our specification of sonnets.
This is another opportunity for future work.

These @racket{rhyme-scheme} patterns are exactly the specification for a variety of common poetic forms.
Below we list some classic forms, but of course past and future poets can imagine many more varieties.
@itemlist[
  @item{haiku : {[5 7 5]}}
  @item{limerick : {[A A (B . 6) (B . 6) A]}
  @item{cinquain : TODO}
  OTHER THINGS WITHOUT #:extra
]


@section{Extra Constraints}
Here is a famous villanelle by Sylvia Plath.

    I close my eyes and all the world drops dead

Using the tools developed so far, we can give a partial specification for this and other villanelles:

    #lang ipoe

    #:name villanelle
    #:description "Six passionate triplets with a haunting refrain"
    #:syllables 10
    #:rhyme-scheme {
      [R1 B  R2]
      [A  B  R1]
      [A  B  R2]
      [A  B  R1]
      [A  B  R2]
      [R1 B  R2]
    }

This specification misses, however, the constraint that all @racket{R1} lines contain exactly the same words (``I close my eyes and all the world drops dead'').
Same for all @racket{R2} lines.
We can express these and other positional equality constraints by calls to an API of zero-indexed selector functions.

    #:constraint
      (line=? (line 0 (stanza 0))
              (line 2 (stanza 1))
              (line 2 (stanza 3))
              (line 0 (stanza 5)))
    #:constraint
      (line=? (line 2 (stanza 0))
              (line 2 (stanza 2))
              (line 2 (stanza 4))
              (line 2 (stanza 5)))

It is a limited API, designed for readability.
But the real charm of these constraints is that, once encoded, they make it very easy to check new poems against complicated specifications.
Villanelles are fairly easy to check, but other forms like the sestina are quite complicated.
For example, the first two stanzas of Seamus Heaney's @emph{Two Lorries} demonstrate the sestina's word-cycling structure:

    TODO


@; Belongs in the Scribble docs, not in the extended abstract
@; @section{Writing a New Poem}


@section{Family of Languages}
All our poetic forms have been written in the language @racket{#lang ipoe}.
Our poems, in contrast, have been written in the more specific @racket{#lang ipoe/couplet}, @racket{#lang ipoe/english-sonnet}, and @racket{ipoe/villanelle}.
These specific languages are actually generated by programs written in @racket{#lang ipoe}, which is a language for generating a family of little languages.

In full detail, programs written in @racket{#lang ipoe} are interpreted as poem specifications.
These specifications list critical data and metadata about a poetic form.
Nothing more.
The language @racket{ipoe} then elaborates a specification into a new Racket language that implements the mechanical task of reading new text from an input source and checking the new text against this spec.
Additionally, elaborated programs include a pass to check spelling and support a variety of run-time flags to control details of the poem-checking process.

Leveraging @racket{#lang ipoe} as a language-generator makes it easy to define new poetic forms.
The process takes 3 steps.
First create the folders @racket{portrait/lang} anywhere on the filesystem.
Second, create a file @racket{portrait/lang/reader.rkt} and fill it with:

    #lang ipoe

    #:name portrait
    #:description "4-line biographical poems"
    #:rhyme-scheme {[A B C B]}
    #:syllables 8

Finally, run the command @racket{raco pkg install ./portrait} to install the language.
Now we can check and approve poems like the following:

    #lang portrait

    Stephen Daedelus is my name
    TODO
    Clongowes is my dwelling place
    And heaven my expectation

Wonderful!
Now, the reason it is @racket{#lang portrait} and not @lang{#lang ipoe/portrait} is that we created and installed a new Racket package.
If we prefer to use @racket{#lang ipoe/portrait}, we must:
@itemlist[
  @item[Find the @racket{ipoe/} folder on our filesystem.]
  @item[Move the folder @racket{./portrait} into the @racket{ipoe/} folder.]
  @item[Run @racket{raco pkg update ipoe/} to compile the @racket{portrait} reader.]
]
Either way, we hope to have shown it is easy to define new kinds of poems.


@section{A Type System for Poetry}
A rhyme scheme really is a form of type specification for poems.
Here we formalize this analogy and give a typed operational semantics for poetry.
Using this semantics, it is possible to give a poem soundness theorem guaranteeing the correctness of our generated langauges.

The syntax of a rhyme scheme is:

    TODO

Similarly, the syntax of a poem is:

    TODO

Given a poem @math{\mathcal{P}}, a rhyme scheme @math{\Sigma}, and a mapping @math{Gamma} from rhyme variables to strings, we have the semantics:


@section{Discussion & Future Work}
Our inspiration for this work dates back to the third grade, with MS Word.

First solution done in Java.

Still, we do not deny that an elegant peotry editor @emph{could}, in theory, be made in Java.
Indeed,

    < Church Turning >

Point taken, but in closing we would like to remind readers that:

    The gap between theory and practice
    is, quite frequently, drastic.
    And the programming language
    can help you or hang you --
    that's why I prefer hacking in Racket

