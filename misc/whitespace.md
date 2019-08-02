# About this document

This document is intended to describe the use of whitespace by
Hoon to the level and to the degree of precision
necessary for `hoonfmt` and `hoonlint`.
This document therefore only deals with conventions
for Hoon expressions that contain gaps.
These are

* tall Hoon expressions; and

* SELGAP, a special case.

Whitespace in other Hoon expressions is always an "ace" -- a single space.
For these Hoon expressions in which all whitespace is an ace,
there is no latitude in the use of whitespace,
and no need for conventions of the kind described
in this document.

# Terminology

## Deontology

In this document, we say "`X` should be `Y`",
if `X` must be `Y` in order to meet the standard described
in this document.
Code or practices that do not meet this standard we
will call **non-standard**.
One pragmatic consequence of non-standard code
is that
it may draw warnings from `hoonlint`.
It is expected that, even in carefully written Hoon code,
non-standard practices will occur.

In this document, we say "`X` must be `Y`",
if `X` must be `Y` in both standard and non-standard code.
This would be the case, for example,
if X is Y as a matter of definition,
or if X is Y due to a logical or a mathematical equivalence.

## Hoon source files

This document applies to Hoon source files.
A Hoon source file is divided into newline-terminated lines
each of which contains 1 or more characters.
The 1-based offset of a character in its line is its **column location**.
The 1-based line number of a character is its **line location**.
Line location is often abbreviated to **line**,
and column location is often abbreviated to **column**.

To avoid confusion,
in this document
"column" is always used to mean the
column location of a character within a line,
as defined above.
When we discuss other 2-dimensional grids,
we use the terms "row" for position on the vertical axis,
and "silo" for position on the horizontal axis.

For orthogonality with columns and lines,
row and silo numbers are 1-based.
There will be several definitions
of "row" and "silo", and we will give
specifics in the context
where they are used.

When we say that "sequence `S[0], S[1] ... S[n]` is **siloed** beginning at silo `x`",
we mean that, for `0 <= i <= n`, `S[i]` goes into silo `i+x`.
When we say that "sequence `S[0], S[1] ... S[n]` is **siloed**",
we mean that element `S[i]` goes into silo `i+1`.

A **whitespace character** is either an ASCII space
or a newline.
A **whitespace boundary** is one of the following:

* The start of the Hoon source file.

* The end of the Hoon source file.

* A location between a whitespace character and a
  non-whitespace character.

Note that a whitespace boundary is not a column
location.
Instead it is a location immediately before
a column location;
immediately after a column location;
or both.

A character block is a lexically contiguous sequence of one or more
characters.
A whitespace block is a character block
that contains only whitespace characters
and occurs between two whitespace boundaries.
An **ace** is a whitespace block that contains a single ASCII space.
A **gap** is any whitespace block that is not an ace.

A **horizontal gap** is a gap that contains only ASCII spaces.
A **vertical gap** is a gap that contains at least one newline.
All gaps are either horizontal or vertical.

A **text block** is a character block

* that begins at a whitespace boundary;

* whose first character is a non-whitespace character;

* that ends at a whitespace boundary; and

* whose last character is a non-whitespace character.

Note that text blocks may contain whitespace characters.
We often refer to a text block simply as a **text**.

The column location at which `x` starts
will sometimes be written as `Column(x)`,
where `x` designates some text in a Hoon source file.
The line location at which `x` starts
will sometimes be written as `Line(x)`.
When we say "the column location of `x`",
we mean `Column(x)`.

We say "column `C` is aligned `N` characters after column `D`"
if and only if `C = D + N`.
We say "column `C` is aligned at column `D`"
if and only if `C = D`.
We say "column `C` is aligned `N` characters before column `D`"
if and only if `C = D - N`.

When
we speak of text blocks being aligned relative to
columns or relative to other text blocks,
we are refering to the column location
of the text blocks.
For example, if we say
"Text X is aligned N characters after text Y",
we mean that `Column(X) = Column(Y) + N`.

Often we express distance between columns
in stops instead of characters.
A **stop** is two characters.

Many alignments are with respect to an **anchor column**.
Usually, the anchor column is the rune column,
but there are important exceptions, which
will be described below.

## Hoon statements

A hoon source file contains one or more hoon **statements**.
The hoon statements are separated by whitespace.
Optionally, the first hoon statement may be preceded by whitespace.
Also optionally, the last hoon statement may be followed by whitespace.

A hoon **statement** is a text block which compiles to a
single AST node.
We sometimes refer to a hoon statement as a "hoon",
but strictly speaking, the "hoon statement" is the text block
before parsing, and the "hoon" is the AST produced by parsing.

## Runes

In this document, **rune** is used in a somewhat different
sense than in most Hoon documents.
In most Hoon documents, "rune"
means a digraph which acts as the keyword at
the beginning of a hoon statement.
In this document, we will refer to those as the
"statement runes".

In this document,
"rune" will mean either

* a statement rune; or

* an arm marker digraph:
  one of LUSLUS (`++`), LUSHEP (`+-`), or
  LUSTIS (`+=`).

Note that not all special-character digraphs are runes.
In this document,
HEPHEP (`--`) or TISTIS (`==`) will be called **terminators**,
and they are not runes.
The comment-marking digraph `::`,
is also not a rune.
Comments, including the digraphs that begin them,
are considered whitespace,
and nothing in whitespace is considered a rune.

## Hoon expressions

A tall **rune expression** is a tall Hoon expression whose "keyword"
is a rune.

Let `r` be the rune digraph of a rune expression.
The **rune line** of `r` is `Line(r)`.
The **rune column** of `r` is `Column(r)`.

A tall rune expression consists of the initial rune
followed by zero or more subexpressions,
called **runechildren**.
The **arity** of a rune expression is the number of
its runechildren.
In the tall Hoon expressions being considered in this document,
the rune is separated from its first runechild,
and consecutive
runechildren are separated
from each other, by gaps.

Let `r` be a rune.
We sometimes write the Hoon expression that begins with `r`
as `Hoon(r)`.
If `h` is a Hoon, we can write its rune as `Rune(h)`.
Then

```
   Line(Hoon(r)) == Line(r)
   Column(Hoon(r)) == Column(r)
```
Let `h` be a rune expression.
We sometimes write the Hoon expression that is the syntactic parent
of `h` as `Parent(h)`.

A hoon properly contains a text block
if it includes the entire text block,
but is not identical to it.
A hoon
"directly contains" a text block if it
properly contains the text block,
and no other properly contained text block of the hoon
is a Hoon subexpression
that properly contains that text block.

## Horizontal Alignment

Three kinds of horizontal alignment
will be of special interest:

* **Tight**:  A text block is tightly aligned if is immediately preceded by,
and separated from,
another text block by a horizontal one-stop gap.
Note that, by this definition,
the first text block on a line can never be tightly aligned.

* **Backdented**:  Hoon's standard alignment, described
in detail below.

* **Inter-line**:  Inter-line alignment aligns the non-whitespace lexeme with
non-whitespace lexemes on associated lines.  Which non-whitespace lexeme is aligned with which,
and which lines are considered "associated" varies depending on
the syntactic context.  Inter-line alignment is described in detail
in the sections describing the syntaxes where it is allowed.

## Joined and split

For many Hoon expressions,
the pattern of the standard whitespace conventions depends
on whether a specific gap in that Hoon expression
is horizontal or vertical.
If the pattern-decisive gap is horizontal, the
conventions follow a "joined" pattern.
If the pattern-decisive gap is vertical, the
conventions follow a "split" pattern.
The specific conventions will be specified below,
with each syntax.

## Pseudo-joins

It is sometimes convenient to have a vertical
pattern-decisive gap,
but to otherwise follow
the "joined" syntax pattern.
For example, the programmer may find "joined"
syntax appropriate, but want to insert
a comment in the pattern-decisive gap.
In these situations,
it is useful to have a form of the vertical gap
that, for pattern-decisive purposes,
is treated as if it were a horizontal gap.
Such a gap is called a **pseudo-join**.

Let `J` be a **join column**.
A text is **pseudo-joined** at `J`
if and only if every line of it,
except the last line,
has a comment aligned at `J`.
Note that this implies that the first, partial, line of the gap
must contain a comment aligned at `J`.

Visually, the pseudo-join's comments look like "place holders" for
the text that follows the pseudo-join.

Intuitively, a pseudo-join is equivalent to a horizontal gap
if the text that follows both of them
is aligned at the same column location.
More formally,
Let `file1` be a Hoon source file.
Let `J` be the join column of a pseudo-join in `file1`.
Construct another Hoon source file, `file2`, by
lexically replacing the pseudo-join with a horizontal gap.
Call that horizontal gap, `G`.
Let `H` be the column location immediately after `G` in `file2`.
The pseudo-join and the horizontal gap are **equivalent**
if and only if `J == H`.

## Reanchoring

Reanchoring is a method of conserving indentation.
We call the original rune,
the one that is to be reanchored,
the **reanchored rune**.

Typically, a rune is its own
"anchor" rune for indentation purposes,
but Hoon takes advantage of some opportunities
to move the anchor column closer to the left margin.
Not all runes participate in reanchoring.
Which do, and which do not, are specified in the
individual cases.

The anchor column depends
on two things: the column location of anchor rune,
and the "reanchor offset".
The **anchor rune** is a rune on the same
line as the reanchored rune.
The anchor rune is either the same as the reanchored rune,
or is a rune closer to the left margin.

### Reanchor offset

The reanchor offset is an adjustment
necessary
to make reanchor indentation "look right" in
Hoon terms.
It makes the reanchored indentation look
and act like normal backdented indentation.
The need for it is not obvious from the definition but,
visually,
if the reanchor offset were not applied,
things would "look wrong".

Call the text block that starts at the
column location of the anchor rune,
and that ends immediately after the last character
of the reanchored rune,
the "reanchor block".
Reanchoring may be thought of treating the
"reanchor block"
as if it were a single rune.
This can be thought of a sort of "currying",
and the reanchor block can
be thought of as a curried rune.

Some runechildren of the curried runes are
included in the reanchor block,
and they therefore must be included in the currying.
Those runechildren not in the reanchor block
are "left over",
and are not included in the currying.

The "left over" runechildren
can be thought of as the runechildren of the
curried rune.
For it to look as if the children of
this curried rune were backdented
normally,
the whitespace conventions must account for
curried versus "left over" runechildren.

Intuitively, if not all the runechildren of a curried rune
are on the rune line,
not only are some of the runechildren "left over",
the appropriate amount of indentation is also "left over".
This "left over" indentation of each rune is the "per-rune offset"
of that rune.

The **reanchor offset** of `r` is the sum of all the
per-rune offsets between the `a`
and `r`, including `a` but not including `r`.
`r`'s per-rune offset is not included in the
calculation of the reanchor offset,
because the rune-children of `r` are not curried.
In effect, all of `r`'s children are "left over".

There will be examples below that show the calculation
of per-rune-offsets in a reanchoring context.
But, for a first example,
it will be easiest to see how
the definition of the per-rune-offset
applies to a simple backdented
hoon.
Call the following Hoon fragment, "Actual":

```
:^  a  b  c
d
```

What is the per-rune offset of the COLKET (`:^`) expression
in "Actual"?
Its last runechild on the same line is the text `c`.
Let us rewrite "Actual",
moving the `c` to the next line
and aligning according to the conventions
of this document.
Our rewritten fragment,
which we will call "What if?",
follows:

```
:^  a  b
  c
d
```

Standard alignment for the 3rd COLKET runechild placed it at column 3
in the "What if?" example.
Column 3 is two characters after the alignment of the COLKET rune, at column 1.
Therefore the per-rune-offset of the COLKET in "Actual" is two:
`3 - 1 == 2`.
A formal definition of "anchor column" is given in an appendix.

### First example


*From `arvo/sys/vane/ford.hoon`, lines 1916-1920:*
```
        :+  %depends  %|  :~
          definitions+[%& deh]
          listeners+[%& sup]
          waiting+[%& out]
        ==
```

The rune line is line 1916, and the rune is
COLSIG (`:~`).
The anchor rune is the COLLUS (`:+`).
COLLUS is 3-fixed, but all three of its runechildren are
on the rune line, so the per-rune offset is 0.
The anchor column is defined to be
the anchor rune column plus the reanchor
offset, so that in this case,
the anchor column is the same as the anchor rune column.

COLSIG is 0-running, and normal COLSIG indentation aligns the runsteps
one stop after the anchor column,
and puts the final TISTIS at the anchor column.
This is exactly what we see.

### Second example

*From `arvo/lib/hood/kiln.hoon`, lines 346-356:*
```
    =/  request-data  :~
        [0 [0 8.080] 0 'localhost' ~]
        ::  associate 0 as the anonymous ship, which is the ++add result.
        [[0 (scot %p (add our ^~((bex 64))))] ~ ~]
        'not-yet-implemented'
        `'en-US,en;q=0.9'
        `.127.0.0.1
      ==
    =/  monies/coin  [%many ~[[%blob request-data] [%$ ~.n 0]]]
    =/  request/silk:ford  [%bake %urb monies [our %home [%da now]] /web]
    (emit `card`[%exec /kiln/prime/cache our `[[our %home [%da now]] request]])
```

Here the rune line is line 346, the rune is again COLSIG,
and it reanchors at TISFAS (`=/`).
TISFAS is 3-fixed, and 2 of its runechildren are on the rune line,
so that the per-rune offset of the TISFAS is that of
its 2nd runechild -- one stop.
The anchor column is therefore one stop after the anchor rune
column.

COLSIG again is 0-running, so that its runsteps should be aligned
one stop after the anchor column,
which is two stops after the TISFAS column.
By the same logic, the TISTIS should be aligned at the anchor column --
one stop after the TISFAS.
This is what we see in the example.
The last three lines of the example are the last runechild of the
TISFAS.

## Header and inline comments

Comments count as whitespace.
A comment is a **header comment** if it is on a line
by itself.
If a comment is not a header
comment, it is a **rightside** comment.
A rightside comment is
a **margin comment** if it begins at or after column 57,
or immediately after a horizontal gap of 20 or more characters.
All margin comments should start at column 57.
A rightside comment is an **inline comment** if it is not a margin comment.

In standard code, header comments are

* pre-comments or inter-comments,
as determined by their hoon;

* part of a staircase; or

* meta-comments.

We say that a comment belongs to a hoon
if that hoon directly contains the gap
that lexically contains that comment.
We say a hoon is the hoon of a comment
if the comment belongs to that hoon.

Meta-comments start at column 1.
Since, depending on their hoon,
inter-comments may also start at column 1,
only the programmer's intent determines whether
a given header comment is an inter-comment or a meta-comment.

Pre-comments, inter-comments and staircases are structural comments.
The contents of structural comments should usually
be appropriate for the syntactic context of the code in which
the structural comment occurs.

Meta-comments are not structural comments,
and their content may be anything.
A frequent use for meta-comments is to
"comment out" Hoon code.

## Staircase comments

Informally, staircase comments take the form

```
::
::
::::
  ::
  ::
```

More formally, a staircase comment consists of an
**upper riser**,
a **tread**,
and a **lower riser**.
The upper riser is a sequence of normal comment lines,
aligned at the inter-comment column.
(The inter-comment column location varies,
depending on the syntax which
contains the comment.)
The tread is four colons, aligned at the inter-comment
column and followed by a whitespace character.
The lower riser is a sequence of normal comment lines,
aligned at the column
one stop greater than the inter-comment column.
A still more formal definition of a staircase is
given in an appendix.

## Vertical Gaps

When we state that a vertical gap
has comments at one location,
we mean
that the gap's inter-comment column location
is the stated column location,
and that the gap's pre-comment column location
is not defined.
When we state that a vertical gap
has comments at two locations,
we mean
that the gap's inter-comment column location
is the first location mentioned,
and that the gap's pre-comment column location
is the second location mentioned.

For example, when we write

> A vertical gap, with comments at the
> the anchor column and the jog base column

we mean that the inter-comment location of the vertical gap is
the anchor column,
and the pre-comment-location is the "jog base column".
If instead we write

> A vertical gap, with comments at the the anchor column.

then we mean that the inter-comment location of the vertical gap is
the anchor column,
and the pre-comment-location is not defined.

In describing a vertical gap's comment locations,
we may specify them explicitly as column locations,
or implicitly, as alignments with a text block.
If column location is expressed implicitly via a text block,
the implicit location is the start column
location of the text block.

A vertical gap contains

* A newline-terminated partial line preamble.
This preamble may be of length 1 -- that is,
it may be just the newline.

* A body of zero or more full newline-terminated lines,
all of them header comments or
(in the case of non-standard code) blank lines.

* A partial line postamble.
This is never newline-terminated,
and may be zero-length.

Informally, the body of a standard vertical gap
follows these conventions:

* A vertical gap may contain
an "inter-part", a "pre-part",
or both.

* If both an inter-part and a pre-part are present,
the inter-part must precede the pre-part.

* The inter-part is a non-empty sequence of inter-comments
and staircases.

* A pre-part is a non-empty sequence of pre-comments.

* Meta-comments may be inserted anywhere in either the pre-part
or the inter-part.

* A comment is not regarded as a meta-comment
if it can be parsed as structural comment.

A more formal description of a vertical gap body is
given in an appendix.

## Types of tall hoons

Every tall hoon falls into one of 5 disjoint classes:
backdented, running, jogging, battery or irregular.

* A tall hoon is a **battery hoon** if it contains an runechild that
uses the battery syntax.

* A tall hoon is a **running hoon** if it contains an runechild that
uses the running syntax.

* A tall hoon is a **jogging hoon** if it contains an runechild that
uses the jogging syntax.

* A tall hoon is a **backdented hoon** if it contains a fixed number
of gap-separated runechildren, and none of them follow the running, jogging or
battery syntax.

* SELGAP is a tall **irregular hoon**.
(Most irregular hoons do not contain a gap.)

# Top-level whitespace

We call a hoon statement not contained by another hoon statement,
a **top-level** hoon statement.
A Hoon source file is a sequence of top-level hoon statements.
All top-level hoon statements should start at column 1.
Successive top-level hoon statements should be separated by
a vertical gap with
comments at column location 1.

The first top-level hoon statement may be preceded by a vertical gap,
called the top-level leader.
Comments in the top-level leader should be at column location 1.
The top-level leader is a special-case of a vertical gap --
it contains a body and
a zero-length postamble, but no preamble.

The last top-level hoon statement may be follows by a vertical gap,
called the top-level trailer.
Comments in the top-level trailer should be at column location 1.
The top-level trailer is a special-case of a vertical gap --
it contains a body and
a preamble, but no postamble.

# Backdented hoons

The flagship Hoon whitespace strategy is backdenting.
Variations on the idea of backdenting appear throughout,
but the archetypal case of backdenting is its use in
backdented hoons.
Here is a example of a backdented 4-ary hoon:

*From `arvo/sys/hoon.hoon`, 6752-6755:*
```
        :^    %wtcl
            [%dtts [%rock %$ |] [%$ axe]]
          [%rock %f |]
        [%rock %f &]

```

A backdented hoon of arity 3 or more should be joined.
A backdented hoon of arity 2 or less should be split.

The first runechild of a joined backdented hoon should be
on the same line as the rune, separated by a horizontal gap.
Subsequent runechildren of a joined backdented hoon should be
separated by a vertical gap.

The first runechild of a split backdented hoon should be
separated from the rune by a vertical gap.
Subsequent runechildren of a joined backdented hoon should also be
separated by a vertical gap.

In an backdented `n`-arity hoon,
the `m`'th runechild should be aligned `n-m` stops after than the anchor column.
For example,
in an 3-runechild hoon,
the first runechild should be aligned 2 stops after than the anchor column;
a second runechild should be aligned 1 stop after than the anchor column;
and the third runechild be aligned at the anchor column.

This implies that, regardless of the number of runechildren in a
backdented hoon,

* the last runechild should be aligned at the anchor column;

* every runechild before the last should be aligned
one stop after
than the runechild that follows it.

* every runechild after the first should have be aligned
one stop before the runechild that precedes it.

In the vertical gaps that belong directly to backdented hoons,
comments should be aligned
with the runechild immediately following the gap.

## Chaining backdented hoons

As a special case, a runechild of an backdented hoon may
have an inter-line alignment, based on its **silo** and **chain**.
Intuitively,
if two runechildren are in the same silo of the same chain,
they should have the
same inter-line alignment.

*From `arvo/sys/zuse.hoon`, lines 2950-2959:*
```
     ::
     ::  make acru cores
     ::
     =/  ali      (pit:nu:crub 512 (shaz 'Alice'))
     =/  ali-pub  (com:nu:crub pub:ex.ali)
     =/  bob      (pit:nu:crub 512 (shaz 'Robert'))
     =/  bob-pub  (com:nu:crub pub:ex.bob)
     ::
     ::  alice signs and encrypts a symmetric key to bob
     ::
```

More formally,
a chain is a sequence of backdented hoons
that obeys the following rules:

* Every hoon except the first is the last
runechild of the previous hoon in the sequence.

* Every tall rune is either

    * a "row-initial" tall rune,
      which should be aligned at
      the first hoon in the sequence, or;

    * a "joined" tall rune, that is, another tall
       rune on the same line as
       an initial rune.

* The sequence is maximal.  That is,
no chain is a sub-sequence of a longer chain.

For the purposes of chain inter-line alignment,
we define row and silo as follows:
Each row-initial rune starts a new row.
For each row,
the tall runes and their runechildren on the
initial rune's line,
taken in lexical order and recursively, are siloed.

A runechild that itself is a tall rune expression is never
a silo element -- instead it is broken out into
its rune and runechildren,
and these become silo elements in that row.
A row may contain multiple lines,
but runes and
runechildren not on the first line of the row
are not added as silo elements,
and therefore are not used in determining inter-line alignment.

This implies that

* Every row-initial rune is in silo 0,

* Every tall rune is a separate silo element.

* Every runechild of a tall rune is a separate
  silo element, unless it is itself a tall rune expression.

* For chained inter-line alignment,
  the row and silo grid may be "ragged", so that some
  rows do not have elements in every silo.

* Every row in a chain starts with a rune, but not every
  rune starts a new row.

* A silo may contain both runes and runechildren.

*From `arvo/sys/hoon.hoon`, lines 1572-1575:*
```
    =|  b/(set _?>(?=(^ a) p.n.a))
    |-  ^+  b
    ?~  a   b
    $(a r.a, b $(a l.a, b (~(put in b) p.n.a)))
```

In the above example, the chain contains 3 rows.
(The 3rd and 4th lines are both in the last row.)
There are 3 silos, but the rows are "ragged", so that nothing
in the first row is in the 3rd silo.
In all 3 rows, the 2nd silo is tightly aligned,
as is the 3rd silo in the 2nd row.

The 3rd silo of the 3rd row is inter-line aligned.
The `b` in the 3rd row comes after a gap of 3 spaces,
but this follows the standard set forth
in this document because it aligns with the `b`
of the 2nd row, at column location 13.

Note that the 2nd silo includes both runechild and a rune.
This is an example of a "joined" hoon.
In a chained hoon sequence,
when the row starts with a unary rune,
joined hoons can be convenient.

## Split hints

*From `sys/vane/clay.hoon`, lines 4022-4228:*
```
    ~>  %slog.
        :^  0  %rose  [" " "[" "]"]
        :^    leaf+"initial merge failed"
            leaf+"my most sincere apologies"
          >p.p.p.+.q.hin<
        q.p.p.+.q.hin
    [~ ..^$]
```

The first runechild of
SIGGAL (`~<`) and
SIGGAR (`~>`) is a "hint".
Hints have a "split" form.

A split form hint begins with its "head":
a symbol name
(in `%name` form) followed
by a DOT.
The rest of the hint is its "tail".

The tail of a split form hint should be separated
from its head by a vertical gap,
with comments aligned with the head.
The tail should also be aligned with the head.

# Running hoons

A running runechild is more often called simply a **running**.
Currently, no hoon contains more than one running.

A running hoon may contain
a runechild before the running.
That runechild
is called the **head** of the running hoon.

There are current three kinds of running hoons:

* A **0-running** has no head.
The current 0-running rules are
BUCCEN (`$%`),
BUCCOL (`$:`),
BUCWUT (`$?`),
COLSIG (`:~`),
COLTAR (`:*`),
TISSIG (`=~`),
WUTBAR (`?|`),
and
WUTPAM (`?&`).

<!-- TISSIG is very problematic.
It's a 0-running implemented (why?) in hoon.hoon as a 1-running.
-->

* A **1-running** has a head.
The current 1-running rules are
CENCOL (`%:`),
DOTKET (`.^`),
SEMCOL (`;:`),
SEMSIG (`;~`).


## 0-running hoons

0-running hoons may be either split or joined.

### Joined 0-running hoons

*From `arvo/sys/zuse.hoon`, lines 2399-2402:*
```
              :~  [b er]
                  [b pk]
                  [(met 0 m) m]
              ==
```

A joined 0-running hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* A running where

    * runstep lines are aligned two stops
      after the anchor column;

    * the inter-column of the vertical gaps is
      aligned at the anchor column; and

    * the pre-column of the vertical gaps is
      aligned at the runstep lines.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned at the runstep lines.

* A TISTIS aligned at the anchor column.

### Split 0-running hoons

This example is the beginning and end of a long split 0-running hoon.

*From `avro/sur/twitter.hoon`, lines 84-194:*
```
    :~
      [  {$mentions $~}                %get   /statuses/mentions-timeline  ]
      [  {$posts-by sd $~}             %get   /statuses/user-timeline  ]
      [  {$timeline $~}                %get   /statuses/home-timeline  ]
      [  {$retweets-mine $~}           %get   /statuses/retweets-of-me  ]
:: Lines 89-189 are omitted
      [  {$help-langs $~}              %get   /help/languages  ]
      [  {$help-privacy $~}            %get   /help/privacy  ]
      [  {$help-tos $~}                %get   /help/tos  ]
      [  {$rate-limit-info $~}         %get   /application/rate-limit-status  ]
    ==
```

A split 0-running hoon should consist of,
in lexical order:

* Its rune.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned at the runstep lines.

* A running where

    * runstep lines are aligned one stop
      after the anchor column;

    * the inter-column of the vertical gaps is
      aligned at the anchor column; and

    * the pre-column of the vertical gaps is
      aligned at the runstep lines.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned at the runstep lines.

* A TISTIS aligned at the anchor column.

<!-- TODO: check all uses of runeColumn to be sure that
     anchor column is not what is intended -->

## 1-running hoons

1-running hoons can be joined or split.

### Joined 1-running hoons

*From `arvo/sys/hoon.hoon`, lines 4853-4855:*
```
             ;~  less  soz
               (ifix [soq soq] (boss 256 (more gon qit)))
             ==
```

A joined 1-running hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* Its head.

* A one-stop horizontal gap.

* The first runstep line.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned one stop after
  the anchor column.

* The remainder of the running, where

    - runstep lines are aligned one stop
      after the anchor column;

    - the inter-column of the vertical gaps is
      aligned at the anchor column; and

    - the pre-column of the vertical gaps is
      aligned one stop after the anchor column.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned one stop after the
  the anchor column.

* A TISTIS aligned at the anchor column.

### Split 1-running hoons

*From `arvo/sys/zuse.hoon`, lines 4048-4051:*
```
      ;~  pose
        (cold & (jest 'true'))
        (cold | (jest 'false'))
      ==
```

A split 1-running hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* Its head.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned one stop after
  the anchor column.

* A running where

    - runstep lines are aligned one stop
      after the anchor column;

    - the inter-column of the vertical gaps is
      aligned at the anchor column; and

    - the pre-column of the vertical gaps is
      aligned one stop after the anchor column.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned one stop after the
  the anchor column.

* A TISTIS aligned at the anchor column.

## Runnings

A running contains one or more **runstep lines**.
Runstep lines should be aligned at the **runstep base column**,
with exceptions.
The runstep base column location
for each running hoon,
and the exceptions, if any,
are stated in the specification of the running hoon
that directly contains the running.

Within a runstep line, the runsteps
should be tightly aligned,
or they should follow runstep inter-line alignment,
as described next.

### Runstep inter-line alignment

In a row of runsteps, runsteps after the first may have
inter-line alignment.
Within a running,
the inter-line alignment column of runsteps is
determined by their silo.

*From `sys/zuse.hoon`, lines 4892-4905:*
```
        :~  ~2015.6.30..23.59.59   ~2012.6.30..23.59.59
            ~2008.12.31..23.59.58  ~2005.12.31..23.59.57
            ~1998.12.31..23.59.56  ~1997.6.30..23.59.55
            ~1995.12.31..23.59.54  ~1994.6.30..23.59.53
            ~1993.6.30..23.59.52   ~1992.6.30..23.59.51
            ~1990.12.31..23.59.50  ~1989.12.31..23.59.49
            ~1987.12.31..23.59.48  ~1985.6.30..23.59.47
            ~1983.6.30..23.59.46   ~1982.6.30..23.59.45
            ~1981.6.30..23.59.44   ~1979.12.31..23.59.43
            ~1978.12.31..23.59.42  ~1977.12.31..23.59.41
            ~1976.12.31..23.59.40  ~1975.12.31..23.59.39
            ~1974.12.31..23.59.38  ~1973.12.31..23.59.37
            ~1972.12.31..23.59.36  ~1972.6.30..23.59.35
        ==
```

Each runstep line is a row.
For each row,
the runsteps, in lexical order, are siloed.

### Running-inherited inter-line alignment

If a runstep is a backdented hoon,
the runechildren of a backdented hoon
may have a **running-inherited** inter-line alignment.
Within a running,
the running-inherited inter-line alignment of a runechild
is determined by the silo of the runechild.
A running should not have both running-inherited and runestep
alignment.

*From `sys/hoon.hoon', lines 5303-5308:*
```
    :~  :-  ['a' 'z']  (cook |=(a/@ta [%$ %tas a]) sym)
        :-  ['0' '9']  (stag %$ bisk)
        :-  '-'        (stag %$ tash)
        :-  '.'        ;~(pfix dot perd)
        :-  '~'        ;~(pfix sig ;~(pose twid (easy [%$ %n 0])))
    ==
```

Each backdented hoon is a row.
For each row,
The sequence composed of the rune of the hoon,
followed by its runechildren in
lexical order,
is siloed.

Note that if a backdented hoon is a runestep,
it may have both a running-inherited inter-line alignment
and a chained inter-line alignment.
If a backdented hoon does have both inter-line alignments,
they should be consistent.
That is,
for every silo,
the column location of that silo according
to running-inherited alignment
should be identical to
the column location of that silo according
to chained alignment.

# Jogging hoons

A jogging runechild is more often called simply a **jogging**.
(Currently all hoons contains at most one jogging.)

In addition to the jogging, a jogging hoon may contain
other runechildren, either before or after the jogging.
An runechild after the jogging is called a **tail**.
If there is one runechild before the jogging, it
is called the **head** of the jogging hoon.
If there are two runechildren before the jogging, they
are called, in order, the **head** and **subhead** of
the jogging.

There are currently four kinds of jogging hoon:

* The current 1-jogging rules are CENTIS (`%=`), CENCAB (`%_`) and WUTHEP (`?-`).
  A 1-jogging has one head and no tail.

* The current 2-jogging rules are CENTAR (`%*`) and WUTLUS (`?+`).
  A 2-jogging has a head and a subhead but no tail.

* The current jogging-1 rule is TISCOL (`=:`).
  A jogging-1 has a tail and no head.

* The current 2-jogging-1 statement is SIGCEN (`~%`).
  A 2-jogging-1 statement has
  a head, a subhead, and a tail.

## Joggings

A jogging is a gap-separated sequence of one or more jogs.
Every **jog** contains a **jog head**, followed by a gap and a **jog body**.
Note that it is important to distinguish between the head of a jogging
hoon, defined above, and the head of a jog.

### Chess-sidedness

Jogs, joggings and jogging hoons have **chess-sidedness**.
Chess-sidedness is always either kingside and queenside.
Informally, **kingside** means the indentation has a left-side bias;
and **queenside** means that the indentation has a right-side bias.
The effect of sidedness on
indentation will be specified more precisely in the descriptions of
jogs and of the individual types of jogging hoon.
The general idea is that chess-sidedness stays consistent within
a hoon.

## Jogs

The column location of a jog is that of its head.
A jog is **joined** if its head is on the same
line as its body.
Otherwise, a jog is **multiline**.
As explained below,
a multiline jog may be either **pseudo-joined**
or **split**.

Jogs are either kingside or queenside.
In standard code,
the sidedness of a jog determines the alignment
of split jogs.

The base column location of a jog depends on the jogging hoon
that it belongs to.
It is specified
in the description of the different kinds of jogging hoon.

### Joined jogs

A joined jog may be either
**inter-line aligned** or **ragged**.
A joined jog is ragged if its body is aligned one stop after
its head.
Otherwise, a joined jog is considered inter-line aligned.

All inter-line aligned jogging should be aligned at
the jogging body column.
With each jogging,
at most one column should be designated as
the **jogging body column** of the jogging.

A jog may also be pseudo-joined.
The pseudo-join of pseudo-joined jog should be
equivalent to the one of the horizontal gaps
that a joined jog is allowed according to this standard.

### Split jogs

The gap of split jog should be a vertical gap
with comments at the column location of the jog body.
The standard column location of the body of a split jog
varies according to the sidedness of the jog.

* The column location of the body of a split kingside jog
should be aligned one stop
**after** the jog's head.

* The column location of the body of a split queenside jog
should be aligned one stop
**before** the jog's head.

## Criss-cross lines

*From `arvo/sys/zuse.hoon`, lines 906-918 :*
```
          $:  $:  our/ship                              ::  host
                  src/ship                              ::  guest
                  dap/term                              ::  agent
              ==                                        ::
              $:  wex/boat                              ::  outgoing subs
                  sup/bitt                              ::  incoming subs
              ==                                        ::
              $:  ost/bone                              ::  opaque cause
                  act/@ud                               ::  change number
                  eny/@uvJ                              ::  entropy
                  now/@da                               ::  current time
                  byk/beak                              ::  load source
          ==  ==                                        ::
```

Recall that a terminator is
a HEPHEP (`--`) or
a TISTIS (`==`).
Successive terminators may occur on the same line,
as part of a **criss-cross terminator line**.
A terminator is in a criss-cross line does not have to
be aligned correctly,
if some other terminator with the required alignment
"stands in" for it.

Note, however, that since every terminator on the line requires some
terminator to be aligned according to this standard,
all of the terminators end up with an alignment requirement.
In the example above, the two terminators on line 918 stand in
for each other,
and each imposes an alignment requirement on the other.

Criss-cross lines are so called because
if you draw
a line from each terminator on the criss-cross line
to the rune that
it matches syntactically,
you end up with
a set of lines,
each of which crosses
all the others.

Only one kind of terminator should appear in a criss-cross line.
Every character in a criss-cross line should be
part of a terminator, or part of a gap.

## 1-jogging hoons

Every 1-jogging hoon is either kingside or queenside.

### Kingside 1-jogging hoons

*From `arvo/sys/hoon.hoon`, lines 6330-6334:*
```
  ?-  pex
    {$1 $0}  yom
    {$1 $1}  woq
    *        [%6 pex yom woq]
  ==
```

A kingside 1-jogging hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* Its head.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A kingside jogging.
  The jog base column
  should be
  one stop after the anchor column.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A TISTIS,
  starting at the anchor column.

### Queenside 1-jogging hoons

*From `arvo/sys/hoon.hoon`, lines 6305-6309:*
```
  ?-    nug
      {$0 *}   p.nug
      {$10 *}  $(nug q.nug)
      *        ~_(leaf+"cove" !!)
  ==
```

A queenside 1-jogging hoon should consist of,
in lexical order:

* Its rune.

* A 2-stop horizontal gap.

* Its head.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A queenside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A TISTIS,
  aligned at the anchor column.

## 2-jogging hoons

If the head and subhead of a 2-jogging hoon are on the
same line, the 2-jogging hoon is called **head-joined**.
If the head and subhead of a 2-jogging hoon are on
different lines, the 2-jogging hoon is called **head-split**.

### Kingside head-joined 2-jogging hoons

*From `arvo/sys/hoon.hoon`, lines 6583-6586:*
```
      ?+  p.mod  [%rock %$ 0]
        $cell  [[%rock %$ 0] [%rock %$ 0]]
        $void  [%zpzp ~]
      ==
```

A head-joined kingside 2-jogging hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* Its head.

* A one-stop horizontal gap.

* Its subhead.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A kingside jogging.
  The jog base column
  should be
  one stop after the anchor column.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A TISTIS,
  aligned at the anchor column.

### Kingside head-split 2-jogging hoons

*From `hoons/arvo/app/hall.hoon`, lines 1772-1781:*
```
    ?+  -.det
      =<  sa-done
      %.  det
      =+  (fall (~(get by stories) nom) *story)
      ~(sa-change sa nom -)
    ::
      $new      (da-create nom +.det)
      $bear     ~&(%unexpected-unsplit-bear +>)
      $remove   (da-delete nom)
    ==
```

A head-joined kingside 2-jogging hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* Its head.

* A vertical gap, with comments at the anchor column.

* Its subhead, aligned one stop before the head.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A kingside jogging.
  The jog base column
  should be
  one stop after the anchor column.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A TISTIS,
  aligned at the anchor column.

### Queenside head-joined 2-jogging hoons

*From `arvo/sys/vane/ames.hoon`, lines 1568-1575:*
```
      ?+    lot  ~
          [$$ %ud @]
        (perm p.why u.hun q.p.lot [syd t.tyl])
      ::
          [$$ %da @]
        ?.  =(now q.p.lot)  ~
        (temp p.why u.hun [syd t.tyl])
      ==
```

A head-joined queenside 2-jogging hoon should consist of,
in lexical order:

* Its rune.

* A 2-stop horizontal gap.

* Its head.

* A one-stop horizontal gap.

* Its subhead.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A queenside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A TISTIS,
  aligned at the anchor column.

### Queenside head-split 2-jogging hoons

*Adapted from `arvo/lib/hood/kiln.hoon`, starting at line 582:*
```
    ?+    gem
        (spam leaf+"strange auto" >gem< ~)
    ::
        $init
      =+  :-  "auto merge failed on strategy %init"
          "I'm out of ideas"
      lose:(spam leaf+-< leaf+-> [>p.p.are< q.p.are])
    ::
    ==
```

A head-split queenside 2-jogging hoon should consist of,
in lexical order:

* Its rune.

* A 2-stop horizontal gap.

* Its head.

* A vertical gap, with comments at the the anchor column.

* Its subhead, aligned one stop before the head.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A queenside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A TISTIS,
  aligned at the anchor column.

## Jogging-1 hoons

<!-- "Split" form addressed in Github issue #31" -->

Jogging-1 hoons do not reanchor.

*From `arvo/sys/hoon.hoon`, lines 9720-9727:*
```
          =:  hos  ~
              wec  [~ ~ ~]
            ==
          ::  descend into cell
          ::
          :+  %cell
            dext(sut p.sut, ref (peek(sut ref) %free 2))
          dext(sut q.sut, ref (peek(sut ref) %free 3))
```

Every jogging-1 hoon is considered kingside.
A jogging-1 hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* A kingside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at the
  the anchor column and the jog base column.

* A TISTIS,
  aligned one stop after the anchor column.

* A vertical gap, with comments at the
  the anchor column.

* A tail, aligned at the anchor column.

## SIGCEN

SIGCEN does not reanchor.

*From `sys/hoon.hoon`, lines 3684-6234:*
```
~%    %qua
    +
  ==
    %mute  mute
    %show  show
  ==
|%
:: Lines 3691-6233 are omitted
--
```

SIGCEN statements
can be treated either a special form of a jogging hoon,
or as a 4-fixed hoon with, optionally,
an unusual 3rd runechild.

SIGCEN, except for its 3rd runechild, is treated
as an ordinary 4-ary backdented hoon.
SIGCEN's 3rd runechild may be either `~` or a jogging
bounded by TISTIS lines.
The jogging is always kingside and elements are never
split.

The initial TISTIS should be backdented appropriately
for a third runechild.
The final TISTIS should be aligned with the initial TISTIS.

The initial TISTIS should be followed by a vertical gap
with comments aligned with the TISTIS and with the jogs.
The final TISTIS should be followed by a vertical gap
with comments aligned with the TISTIS and with the jogs.

The jog head should be aligned one stop after the initial TISTIS.
The jog body should be tightly aligned,
or inter-line aligned with other bodies in the formula
list.
Successive formulas should be separated by a vertical
gap with comments aligned with the jogs.

# Battery hoons

The battery hoons are BARCAB, BARCEN and BARKET.

## Batteries

The arms of a battery should all align at the
same column, called the **base column**
of the battery.
The arms should be separated by vertical gaps,
where the inter-column is the base column,
and the pre-column is two stops after the base column.
The base column of the battery is specified below,
in the description of each battery hoon.
Arms may be joined or split.

A battery arm reanchors at BARCEN.

*From sieve_k example, line 6:*
```
++  abet  (sort (~(tap in fed)) lth)
```

A joined arm consists of,
in lexical order:

* A arm marker.

* A one-stop horizontal gap.

* The arm head.

* A one-stop horizontal gap,
  or an equivalent pseudo-join.

* The arm body.

*From sieve_k example, lines 7-12:*
```
++  main
  =+  fac=2
  |-  ^+  ..main
  ?:  (gth (mul fac fac) top)
    ..main
  $(fac +(fac), ..main (reap fac))
```

A split arm consists of,
in lexical order:

* A arm marker.

* A one-stop horizontal gap.

* The arm head.

* A vertical gap,
  with comments at the base column,

* The arm body,
  aligned one stop after the anchor column.

## BARCEN

BARCEN may reanchor at KETBAR or KETWUT.
It may be joined or split.

### Split BARCEN

*From the sieve_k example:*
```
|%
++  abet  (sort (~(tap in fed)) lth)
++  main
  =+  fac=2
  |-  ^+  ..main
  ?:  (gth (mul fac fac) top)
    ..main
  $(fac +(fac), ..main (reap fac))
::
++  reap
  |=  fac=@
  =+  cot=(mul 2 fac)
  |-  ^+  ..reap
  ?:  (gth cot top)
    ..reap
  $(cot (add cot fac), fed (~(del in fed) cot))
--
```

A split BARCEN should consist of,
in lexical order:

* Its rune.

* A vertical gap,
  with comments at the anchor column.

* A battery whose base column is the anchor column.

* A vertical gap,
  with comments at the anchor column.

* A HEPHEP, aligned at the anchor column.


### Joined BARCEN

*From `arvo/sys/zuse.hoon`, lines 176-181:*
```
      |%  ++  seal  |~({a/pass b/@ c/@} *@)             ::  encrypt to a
          ++  sign  |~({a/@ b/@} *@)                    ::  certify as us
          ++  sure  |~({a/@ b/@} *(unit @))             ::  authenticate from us
          ++  tear  |~  {a/pass b/@}                    ::  accept from a
                    *(unit {p/@ q/@})                   ::
      --  ::as                                          ::
```

A joined BARCEN should consist of,
in lexical order:

* Its rune.

* A one stop horizontal gap.

* A battery whose base column is
  two stops after anchor column.

* A vertical gap,
  with comments at the anchor column.

* A HEPHEP, aligned at the anchor column.

## BARCAB

BARCAB runes do not reanchor.

*From `sys/vane/jael.hoon`, lines 697-827:*
```
  |_  pig/safe
  ::                                                    ::  ++delete:up
  ++  delete                                            ::  delete right
    |=  ryt/rite
    ^-  safe
:: Lines 701-820 omitted
  ::
  ++  update                                            ::  arbitrary change
    |=  del/bump
    ^-  safe
    (splice(pig (remove les.del)) mor.del)
  --
```

A BARCAB should consist of,
in lexical order:

* Its rune

* A one stop horizontal gap,
  or an equivalent pseudo-join.

* Its head.

* A vertical gap,
  with comments at the anchor column,
  and one stop after the anchor column.

* A battery, whose base column location should be the anchor column.

* A vertical gap,
  with comments at the anchor column,
  and one stop after the anchor column.

* A HEPHEP, aligned at the anchor column.

## BARKET

BARKET runes do not reanchor.

*From `arvo/sys/zuse.hoon`, lines 3975-4025:*
```
    |^  |=(val/json (apex val ""))
    ::                                                  ::  ++apex:en-json:html
    ++  apex
      |=  {val/json rez/tape}
      ^-  tape
:: Lines 3980-4015 omitted
    ::                                                  ::  ++jesc:en-json:html
    ++  jesc                                            ::  escaped
      =+  utf=|=(a/@ ['\\' 'u' ((x-co 4):co a)])
      |=  a/@  ^-  tape
      ?+  a  ?:((gth a 0x1f) [a ~] (utf a))
        $10  "\\n"
        $34  "\\\""
        $92  "\\\\"
      ==
    --  ::en-json
```

A BARKET should consist of,
in lexical order:

* Its rune

* A one stop horizontal gap,
  or an equivalent pseudo-join.

* Its head.

* A vertical gap,
  with comments at the anchor column,
  and one stop after the anchor column.

* A battery, whose base column location should be the anchor column.

* A vertical gap,
  with comments at the anchor column,
  and one stop after the anchor column.

* A HEPHEP, aligned at the anchor column.

# Ford hoons

## Ford-1 hoons

The ford-1 hoons are a set of unary ford hoons:
FASSIG (`/~`),
FASBUC (`/$`),
FASCAB (`/_`),
FASCEN (`/%`),
FASFAS (`//`),
FASHAX (`/#`) and
FASWUT (`/?`).

*From `arvo/app/hall.hoon`, line 17:*
```
          /~  |=({t/telegram:hall bowl:gall} t)
```

*From `hoons/arvo/mar/rss-xml.hoon`, line 6:*
```
//  /===/mar/xml  :: alias
```

A tall ford-1 hoon should consist of,
in lexical order:

* The rune.

* A one-stop horizontal gap.

* The runechild.

## Ford hoof hoons

The ford hoof hoons are FASHEP (`/-`) and FASLUS (`/+`).

*From `../hoons/arvo/app/dojo.hoon`, line 5:*
```
/-  sole, lens                                          ::  console structures
```

*From `hoons/arvo/mar/twit/feed.hoon`, line 6:*
```
/+  twitter, httr-to-json, old-zuse
```

A tall Ford hoof hoon should consist of,
in lexical order:

* The rune.

* A one-stop horizontal gap.

* The runechild.

* A vertical gap, with comments at the anchor column.

## 0-sequence Ford hoons

The 0-sequence Ford hoons are those whose only runechild
is a sequence of Forn hoons.
FASBAR (`/|`) and FASDOT (`/.`) are the 0-sequence Ford hoons.
The "0-" prefix is for orthogonality with "0-running"
hoons.

*From `hoons/arvo/app/hall.hoon`, lines 16-18:*
```
      /|  /:  /%/filter  /!noun/
          /~  |=({t/telegram:hall bowl:gall} t)
      ==
```

<!-- No FASDOT code in the current arvo corpus is standard,
do this is a revised example.
-->
*Adapted from
`hoons/arvo/web/pack/css/codemirror-fonts-bootstrap-tree.hoon`,
lines 6-10:*
```
  /.  /:/===/web/lib/css/codemirror:/css/
      /:/===/web/lib/css/fonts:/css/
      /:/===/web/lib/css/bootstrap:/css/
      /:/===/web/tree/main:/css/
  ==
```

A tall 0-sequence Ford hoon should consist of,
in lexical order:

* The rune.

* A one-stop horizontal gap.

* A sequence of ford hoons.

* A vertical gap, with comments at the anchor column.

* A TISTIS.

## Ford hoon sequence

A Ford hoon sequence is a sequence of ford hoons.
All of the ford hoons should be at the same
alignment.

The hoons in the sequence should be
separated by vertical gaps.
The comments in the vertical gaps should be
aligned with the rune of the hoon which
directly contains the Ford hoon sequence.

## Ford-2 hoons

The Ford-2 hoons are FASCOL (`/:`),
FASKET (`/^`),
FASPAM (`/&`),
and
FASSEM (`/;`).

<!-- Note FASPAM tall whitespacing in conjectured, based
on parallel with FASKET. --
no examples of tall FASPAM exist in the arvo corpus.
-->

*From `hoons/arvo/app/gmail.hoon`, line 21:*
```
/=  rfctext  /:  /%/rfc  /txt/
```

*From `hoons/arvo/app/hall.hoon`, lines 15-18:*
```
      /^  $-({telegram:hall bowl:gall} telegram:hall)
      /|  /:  /%/filter  /!noun/
          /~  |=({t/telegram:hall bowl:gall} t)
      ==
```

*From `hoons/arvo/ren/tree/index.hoon`, line 7:*
```
    /;  (getall:tree /h1/h2/h3/h4/h5/h6)  /tree-elem/
```

Whitespace conventions for the Ford-2 hoons are the
same as those for backdented 2-arity hoons.

## FASCOM

<!-- FASCOM is not well represented in the arvo
database, and a lot of these rules are extrapolations --
they are not actual examples in the corpus.
The remnants of the two-stop Ford convention in the
corpus make extrapolation from the examples next
to impossible.
I use indentation-conservation heavily as the guideline
for extrapolation.
-->

### Joined FASCOM

All joined FASCOM's are considered "queenside".

*From `hoons/arvo/ren/css.hoon`, lines 6-8:*
```
/,  /web/pack/css  /%   /!css/
    /              /~  !!
==
```

A tall joined FASCOM hoon should consist of,
in lexical order:

* The rune.

* A one-stop horizontal gap.

* A sequence of FASCOM elements, all of which are
  aligned with the first one.

* A vertical gap, with comments at the anchor column.

* A TISTIS.

### Kingside split FASCOM

<!-- The split FASCOM examples are based
on the only arvo example of a split FASCOM.
It was anchored and non-standard, and therefore
I have revised it.
-->

*Adapted from `hoons/arvo/ren/tree/elem.hoon`, lines 5-8:*
```
/,
  /web  /|(/!elem/ /elem/)
  /     /elem/
==
```

A tall kingside split FASCOM hoon should consist of,
in lexical order:

* The rune.

* A vertical gap, with comments at the anchor column.

* A sequence of FASCOM elements, all of which are
  aligned one stop after the anchor column.

* A vertical gap, with comments at the anchor column.

* A TISTIS.

### Queenside split FASCOM

<!-- The split FASCOM examples are based
on the only arvo example of a split FASCOM.
It was anchored and non-standard, and therefore
I have revised it.
-->

*Adapted from `hoons/arvo/ren/tree/elem.hoon`, lines 5-8:*
```
/,
    /web  /|(/!elem/ /elem/)
    /     /elem/
==
```

A tall queenside split FASCOM hoon should consist of,
in lexical order:

* The rune.

* A vertical gap, with comments at the anchor column.

* A sequence of FASCOM elements, all of which are
  aligned two stops after the anchor column.

* A vertical gap, with comments at the anchor column.

* A TISTIS.

### FASCOM element sequence

*From `hoons/arvo/ren/css.hoon`, lines 6-8:*
```
/,  /web/pack/css  /%   /!css/
    /              /~  !!
==
```

A FASCOM element sequence consists of a one
or more **FASCOM elements**.
The head of all the FASCOM elements should be
aligned at the same column.
The FASCOM elements
should be separated by vertical gaps.
Comments in the vertical gaps
should be at the anchor column
or aligned with the heads of the FASCOM elements.

A FASCOM element is considered joined or split,
depending on the gap between the head and the body.
If joined, the body of the FASCOM element
separated from the head by a one-stop gap,
or an equivalent pseudo-join.

If the FASCOM element is split, the
body of the FASCOM element should be separated by
a vertical gap with comments at the body column.
The body of a kingside FASCOM element should be
aligned one stop **after** the head of the FASCOM element.
The body of a queenside FASCOM element should be
aligned one stop **before** the head of the FASCOM element.

## FASTIS

```
/=  rfctext  /:  /%/rfc  /txt/
```

A tall FASTIS hoon should consist of,
in lexical order:

* The FASTIS rune.

* A one-stop horizontal gap.

* A runechild.

# Sail

Sail statements start with a semicolon.
Sail statements never participate in reanchoring.

## Sail 1-fixed runes

*From `ren/tree/head.hoon`, lines 35-43:*
```
    ;*  ?.  nopack.dbg
          :_  ~
          ;link(type "text/css", rel "stylesheet", href "/===/web/pack/css/codemirror-fonts-bootstrap-tree.css");
        ;=
          ;link(type "text/css", rel "stylesheet", href "/===/web/lib/css/fonts.css");
          ;link(type "text/css", rel "stylesheet", href "/===/web/lib/css/bootstrap.css");
          ;link(type "text/css", rel "stylesheet", href "/===/web/lib/css/codemirror.css");
          ;link(type "text/css", rel "stylesheet", href "/===/web/tree/main.css");
        ==
```

The sail 1-fixed runes are
SEMHEP (;-), SEMLUS (;+), SEMTAR (;*), and SEMCEN (;%).
They have a single runechild.
The rune child should be on the same line as the rune,
and should be separated from it by a one-stop horizontal gap.

## SEMTIS

*From `ren/tree/head.hoon`, lines 59-64:*
```
        ;=
::           ;script(type "text/javascript", src "/===/web/lib/js/hoon.js");
          ;script(type "text/javascript", src "/===/web/tree/main.js");
          ;script(type "text/javascript", src "{?.(aut "" "/~~/~/at")}".
                                              "/===/web/lib/js/urb.js");
        ==
```

SEMTIS (;=)
takes a TISTIS-terminated list of sail statements.
It is split or joined, depending on whether the first element
of the list is on the same line as the SEMTIS.

If joined, the first element of the list should be tightly aligned.
If split, the first elements of the list should be aligned
one stop after the SEMTIS,
and the first element should be preceded by a vertical gap
with comments aligned at the SEMTIS and with the first element.

All subsequent elements should be aligned with the first element.
For a joined SEMTIS, this implies that
all elements after the first should be aligned two stops after the SEMTIS.
For a split SEMTIS, this implies that
all elements after the first should be aligned one stop after the SEMTIS.

All elements after the first should be preceded by a vertical gap.
Comments should be aligned at the SEMTIS and with the first element.

The TISTIS should be aligned with the SEMTIS.
it should be preceded by a vertical gap,
with comments aligned at the SEMTIS and with the first element.

## Tagged sail statements

*Adapted from `web/dojo.hoon`, lines 13-27:*
```
;module
    =nav_title    "Dojo"
    =nav_no-dpad  ""
    =nav_no-sibs  ""
  ;script(src "//cdnjs.cloudflare.com/ajax/libs/mousetrap/1.4.6/mousetrap.js");
  ;style:'''
         #term { width: 100%; }
         #term * { margin: 0px; }
         .module pre { margin-bottom: 0; }
         '''
  ;div#err;
  ;div#term:""
  ;script@"/lib/js/sole.js";
  ;sole(appl "dojo");
==
```

Tagged sail statement are always split.
The conventions for the sail attributes are described below.
All elements should be aligned one stop after the start of the statement.
The tagged statement is considered to start at the semicolon which
starts the tag that is its keyword.

All elements should be preceded by a vertical gap.
Comments in the vertical gaps
should be aligned at the start of the statement
and with the first element.

The TISTIS should be aligned with the tagged sail statement
that it terminates.
The TISTIS should be preceded by a vertical gap,
with comments aligned with the tagged sail statement,
and with the first element.

## Sail attributes

The sail attribute base column is two stops after start column location
of the sail text block.
Sail tall attributes should be separated from each other by a vertical
gap with comments aligned at the attribute base column.
Values in sail attributes should be on the same line
as the sail key.
The sail attribute values should be tightly aligned,
or aligned with each other.

# Udon

Currently,
this standard does not address
Udon (Unmarkdown) whitespace conventions.

# SELGAP

*From `arvo/sur/twitter.hoon`, line 85:*
```
     [  {$mentions $~}                %get   /statuses/mentions-timeline  ]
```

Whitespace in one-line SELGAP's is free-form -- `hoonlint` never generates
a warning for them.

Only one-line SELGAP's occur in the `arvo` corpus.
If `hoonlint` encounters a multi-line SELGAP,
it issues a "not yet implemented" warning.

# Appendix: Non-standard code

By non-standard code, we mean code that does not follow
the guidelines of this document.
In non-standard code,
`hoonlint` sometimes must decide the "intended" syntax,
in order to produce diagnostics that are as helpful as possible.
This appendix describes the methods used for deciding what was
"intended" in non-standard Hoon code.

## Chess-sidedness

A jog is considered queenside if it is aligned 2 stops or more
after the base column of the jogging.
Otherwise, the jog is considered kingside.

The chess-sidedness of a jogging is that of the majority
of its jogs.
In case of a tie, the jogging is considered to be queenside.
The chess-sidedness of a jogging hoon is that of its
jogging.

## Jogging body column

In non-standard code,
the jogging body column of a jogging is considered to be the most common start column
of the bodies of the jogging's inter-line aligned jogs.
If more than one column is "most common",
the tie is resolved in favor of the body column
that is lexically first.
If there are no inter-line aligned jogs in a jogging,
the jogging body column is undefined.

## Inter-line alignment

"Attached" text blocks are those with tight or backdented
alignment.
All other text blocks are "floating".
In non-standard code,
the column location of the inter-line alignment
of a silo of text blocks is
the column location most common in the "floating" text blocks.

If two column locations tie in the
count of floating text blocks,
the tie is broken using the count of total text blocks
for that column location.
If two column locations tie by total text block count,
the tie is broken in favor of the column location that
occurs first, lexically.

If there are no floating lexemes,
the inter-line alignment column location
is irrelevant and undefined.
Also,
the inter-line alignment column location is undefined unless it also
has a total text block count of at least 2 --
in other words
an inter-line alignment column location must be the column location
of at least two of the text blocks in the silo.

Each silo is examined separately.
For each silo, only rows with an element in that silo participate
in the calculation.

If a running could have both a runestep alignment,
and a running-inherited alignment,
the runestep alignment takes precedence.
In other words,
if a running has runestep alignment,
the running-inherited alignment is undefined.

# Appendix: Formal definition of "vertical gap body"

The format of a vertical gap body obeys the BNF

```
:start ::= gapComments
gapComments ::= OptExceptions Body
gapComments ::= OptExceptions
Body ::= InterPart PrePart
Body ::= InterPart
Body ::= PrePart
InterPart ::= InterComponent
InterPart ::= InterruptedInterComponents
InterPart ::= InterruptedInterComponents InterComponent

InterruptedInterComponents ::= InterruptedInterComponent+
InterruptedInterComponent ::= InterComponent Exceptions
InterComponent ::= Staircases
InterComponent ::= Staircases InterComments
InterComponent ::= InterComments

InterComments ::= InterComment+

Staircases ::= Staircase+
Staircase ::= UpperRisers Tread LowerRisers
UpperRisers ::= UpperRiser+
LowerRisers ::= LowerRiser+

PrePart ::= ProperPreComponent OptPreComponents
ProperPreComponent ::= PreComment
OptPreComponents ::= PreComponent*
PreComponent ::= ProperPreComponent
PreComponent ::= Exception

OptExceptions ::= Exception*
Exceptions ::= Exception+
Exception ::= MetaComment
Exception ::= BadComment
Exception ::= BlankLine
```

The terminals in this BNF are

* BadComment -- a comment with any alignment.

* BlankLine -- a line terminated with a newline and
otherwise containing only zero or more spaces.

* InterComment -- a comment that starts at the inter-comment column.

* LowerRiser -- a comment that starts one stop past the inter-comment
column.

* MetaComment -- a comment that starts at column 1.

* PreComment -- a comment that starts at the pre-comment column.

* Tread -- a comment that starts at the inter-comment column,
whose first four characters are colons,
and whose fifth character is either a space or a newline.

* UpperRiser -- a comment that starts at the inter-comment column.

Terminals are ambiguous -- a given line may match more than one terminal.
The lexer limits this ambiguity using "priorities".
Each terminal has a numerical priority: 1, 2, 3 or 4.
Comments are read as if looked for
in numerical order by priority.
This has the slightly counter-intuitive effect
that the highest priority is the lowest numbered one.
These definitions imply that

* No comment is read as a Priority 4 comment if it can be
  lexed as a Priority 1, 2 or 3 comment.

* No comment is read as a Priority 3 comment if it can be
  lexed as a Priority 1 or 2 comment.

* No comment is read as a Priority 2 comment if it can be
  lexed as a Priority 1 comment.

Comments with the same priority are treated
as if they were looked for simultaneously.
This means that it remains possible for terminals to be ambiguous --
that is, one line can be read as
two different terminals.
In these cases the BNF grammar ensures that only
one of the terminals will be used.

Priority 1 comments are InterComment, LowerRiser, Tread,
and UpperRiser.
The priority 2 comment is PreComment.
The priority 3 comment is MetaComment.
Priority 4 comments are BadComment and BlankLine.

# Appendix: Formal definition of "anchor column"

Let `r` be the reanchored rune.
Let `a` be the anchor rune of `r`.
The anchor column is
`Column(a) + Offset(r)`,
where `Offset(r)`
is the reanchor-offset of `r`.
The rest of this definition will be
devoted to defining `Offset(r)`.

We first define `PerRuneOff(r)`,
the per-rune-offset.
Let `Child(n, r1)`,
the `n`'th runechild of a `r1`,
be the last runechild of `r1` on `Line(r1)`.
Consider a rewrite of Hoon source that

* moves `Child(n, r1)` to `Line(r1)+1`,

* adjusts the whitespace as necessary to follow the standard whitespace conventions,
  and

* is minimal in all other respects.

Let `c2` be `Child(n, r1)` in this rewrite,
so that `Line(c2) == Line(r1)+1 == Line(Child(n, r1))+1`.
Then the per-rune offset of `r1`
is `PerRuneOff(r1) == Column(c2) - Column(r1)`.

Recall that `r` is the reanchored rune,
and that `a` is the anchor rune of `r`.
We now define a sequence of runes, `S`,
such that

* If `a == r`, then `S` is empty.

* Otherwise, `S = S[0], S[1] ... S[n]`, and we
  have the following:

    * `S[0] = a`.

    * `S[n] = (Rune(Parent(Hoon(r)))`.

    * `S[i] = (Rune(Parent(Hoon(S[i+1])))`,
      for all `i` such that `0 <= i < n`.

Note the following:

* By the above definition, `r` is never an
  element of `S`.

* In the trivial case,
  where a rune is its own anchor,
  `S` is always empty; the reanchor offset is always zero;
  and the anchor column is always the same
  as the rune column.

* If `S` is not empty, it includes `a`
  and all the proper syntactic parents
  of `r` that are descendants of `a`.

We can now finish our definition of anchor column,
by defining `Offset(r)`:
`Offset(r)` is the sum of `PerRuneOff(S[i])`
for all `i` such that `0 <= i <= n`.

# Appendix: Reanchoring futures

The reanchoring in this document is based, first on the examples,
and second, on the `arvo/` corpus.
The approach is conservative -- reanchoring is only in this
standard if it is exemplified in the `arvo/` corpus
or the examples.
Reanchorings which cause inconsistences with the `arvo/` corpus are accepted
as part of this standard,
if it seems reasonable to consider the exceptions to be aberrations.
But no inconsistencies with the examples are accepted as part of this standard.

In future,
it may be best to revise this document to 
treat more reanchorings as standard than
currently.

The `arvo/` corpus suggests that BARCEN reanchoring at TISGAR should
be considered standard,
but accepting BARCEN-to-TISGAR reanchoring as standard
would make the tic-tac-toe example non-standard.
