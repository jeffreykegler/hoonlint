# About this document

This document is intended to describe the use of whitespace by
Hoon to the level and to the degree of precision
necessary for `hoonfmt` and `hoonlint`.

This document therefore only deals with conventions
for Hoon expressions which contain gaps.
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
if `X` must be `Y`, in order to meet the standard described
in this document.
Code, or practices which do not meet this standard we
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
or due to a logical or a mathematical equivalence.

## Lines, columns and alignment

This document applies to Hoon source files.
A Hoon source file is organized in newline-terminated lines
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
When we say simply that "sequence `S[0], S[1] ... S[n]` is **siloed**",
we mean that element `S[i]` goes into silo `i+1`.
(We are assuming that `S` is 0-based.)

The column location at which `x` starts
will sometimes be written as `Column(x)`,
where `x` designates some text in a Hoon source file.
The line location at which `x` starts
will sometimes be written as `Line(x)`.
When we say "the column location of `x`",
we mean `Column(x)`.

A **whitespace character** is either an ASCII space
or a newline.
A **whitespace boundary** is one of the following:

* The start of the Hoon source file.

* The end of the Hoon source file.

* A location between a whitespace character and a
  non-whitespace character.

Pedantically, a whitespace boundary is not a column
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

A **horizontal gap** is a gap which contains only ASCII spaces.
A **vertical gap** is a gap which contains at least one newline.
All gaps are either horizontal or vertical.

A **text block** is a character block that

* occurs between two whitespace boundaries;

* begins with a non-whitespace character; and

* ends with a non-whitespace character.

Note that text blocks may contain whitespace characters.
We often refer to a text block simply as a **text**.

We say "column `C` is aligned `N` characters after column `D`"
if and only if `C = D + N`.
We say "column `C` is aligned at column `D`"
if and only if `C = D`.
We say "column `C` is aligned `N` characters before column `D`"
if and only if `C = D - N`.

When
we speak texts being aligned relative to
columns or relative to other text blocks,
we are refering to the column location of the texts.
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

## Hoon expressions

A tall **rune-ish expression** is a tall hoon expression whose "keyword"
is a rune-ish.
A **rune-ish** is one of the following:

* A rune.

* A **terminator**, either HEPHEP (`--`) or TISTIS (`==`).

* A cell keyword, one of LUSLUS (`++`), LUSHEP (`+-`), or
  LUSTIS (`+_`).

Currently rune-ishes are always represented in Hoon source
files as digraphs.

Note that not all special-character digraphs are rune-ishes.
Comments, including the digraphs which begin them
are considered whitespace,
and nothing in whitespace is considered a rune-ish.
Comment digraphs include `::`.

Let `r` be the rune-ish digraph of a rune-ish expression.
The **rune line** of a hoon is `Line(r)`.
The **rune column** of a hoon is `Column(r)`.

A tall rune-ish expression consists of the initial rune-ish
followed by zero or more subexpressions,
called **runechildren**.
The **arity** of a rune-ish expression is the number of
its runechildren.
In the tall hoon expressions being considered in this document,
runechildren are separated from the initial rune-ish,
and from each other, by gaps.

Let `r` be a rune-ish.
We sometimes write the hoon expression that begin with `r`
as `Hoon(r)`.
If `h` is a Hoon, we can write its rune-ish as `Rune(h)`.
Then

```
   Line(Hoon(r)) == Line(r)
   Column(Hoon(r)) == Column(r)
```
Let `h` be a rune-ish expression.
We sometimes write the hoon expression that is the syntactic parent
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

All non-whitespace lexemes in Hoon are aligned horizontally in one of 4 ways:

* **Tight**:  A text block is tightly aligned if is immediately preceded by,
and separated from another text block by, a horizontal one-stop gap.
Note that, by this definition,
the first text block on a line can never be tightly aligned.

* **Backdented**:  Hoon's standard alignment, described
in detail below.

* **Inter-line**:  Inter-line alignment aligns the non-whitespace lexeme with
non-whitespace lexemes on associated lines.  Which non-whitespace lexeme is aligned with which,
and which lines are considered "associated" varies depending on
the syntactic context.  Inter-line alignment is described in detail
in the sections describing the syntaxes where it is allowed.

* **Free-form**:  Within SELGAP hoons,
horizontal alignment in standard code can be free-form.

## Joined and split

For many Hoon expressions,
the pattern of the standard whitespace conventions depends
on whether a specific gap in that Hoon expression
is horizontal or vertical.
If the pattern-decisive gap is horizontal, the
conventions follow a "joined" pattern.
If the pattern-decisive gap is vertical, the
conventions follow a "split" pattern.
The specific conventions will be described below,
for each syntax.

## Pseudo-joins

In some cases,
it is sometimes convenient to have a vertical
pattern-decisive gap,
but to otherwise follow
the "joined" syntax pattern.
For example, the programmer may find "joined"
syntax appropriate, but want to insert
a comment in the pattern-decisive gap.
In these situations,
it is useful to have a form of the vertical gap
which, for pattern-decisive purposes,
is treated as if it were a horizontal gap.
Such a gap is called a **pseudo-join**.

Let `J` be a **join column**.
A text is **pseudo-joined** at `J`
if and only if every line of it,
except the last line,
has a comment aligned at `J`.
Note that this implies that the first, partial, line of the gap
must contain a comment aligned at `J`.

Visually, the pseudo-join's comments looks like "place holders" for
the text that follows the pseudo-join.

Intuitively, a pseudo-join is equivalent to a horizontal gap
if the text which follows both of them
is aligned at the same column location.
More formally,
a pseudo-join is **equivalent** to a horizontal gap,
if the join column of the pseudo-join is the column location
immediately after the last character of the horizontal gap.

## Reanchoring

Reanchoring is a method of conserving indentation.
We call the original rune-ish,
the one which is to be reanchored,
the **reanchored rune-ish*.

Typically, a rune-ish is its own
"anchor" rune-ish for indentation purposes,
but Hoon takes advantage of some opportunities
to move the anchor column closer to the left margin.
Not all rune-ishes participate in reanchoring.
Which do, and which do not, are described in the
individual cases.

The anchor column depends
on two things: the column location of anchor rune-ish,
and the "reanchor offset".
The **anchor rune-ish** is a rune-ish on the same
line as the reanchored rune-ish.
The anchor rune-ish is either the same as the reanchored rune-ish,
or is a rune-ish closer to the left margin.

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
things would look "wrong".

Call the text block which starts with the anchor rune-ish
and ends with the reanchored rune-ish,
the "reanchor block".
Reanchoring may be thought of treating the
"reanchor block"
as a single rune-ish.
This can be thought of a sort of "currying",
and the reanchor block can
be thought of as a curried rune-ish.

Some runechildren of the curried rune-ishes are
included in the reanchor block,
and they therefore must be included in the currying.
Those runechildren not in the reanchor block
are "left over",
and are not included in the currying.

The "left over" runechildren
can be thought of as the runechildren of the
curried rune-ish.
For it to look as if the children of
this curried rune-ish were backdented
normally,
the whitespace conventions must account for
curried versus "left over" runechildren.

Intuitively, if not all the runechildren of a curried rune-ish
are on the rune line,
not only are some of the runechildren "left over",
the appropriate amount of indentation is also "left over".
This "left over" indentation of each rune-ish is the "per-rune-ish offset"
of that rune.

The **reanchor offset** of `r` is the sum of all the
per-rune-ish offsets between the `a`
and `r`, including `a` but not including `r`.
`r`'s per-rune-ish offset is not included in the
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

What is the per-rune-ish offset of the COLKET (`:^`) expression
in "Actual"?
Its last runechild on the same line is the text `c`.
Let us rewrite "Actual",
moving the `c` to the next line
and aligning according to the conventions
of this document.
Our rewritten fragment
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

### Formal definition of the anchor column.

More formally,
let `r` be the reanchored rune-ish.
Let `a` be the anchor rune-ish of `r`.
The anchor column is
`Column(a) + Offset(r)`,
where `Offset(r)`
is the reanchor-offset of `r`.
It remains to define `Offset(r)`.

We now procede to define `Offset(r)`.
To do this, we first define `PerRuneOff(r)`,
the per-rune-offset.
Let `Child(n, r1)`,
the `n`'th runechild of a `r1`,
be the last runechild of `r1` on `Line(r1)`.
Consider a rewrite of Hoon source that

* moves `Child(n, r1)` to `Line(r1)+1`,

* adjust the whitespace as necessary to follow the standard whitespace conventions.

and which is, in other respects, minimal.

Let `c2` be `Child(n, r1)` in this rewrite,
so that `Line(c2) == Line(r1)+1 == Line(c1)+1`.
Then the per-rune-ish offset of (r1)
is `PerRuneOff(r) == Column(c2) - Column(r)`.

Recall that `r` is the reanchored rune-ish,
and that `a` is the anchor rune-ish of `r`.
We define now defined a sequence of rune-ishes,
`S[0], S[1] ... S[n]`,
such that all
of the following are true.

* `S` is empty if a rune-ish is its own anchor.
That is,
If `a == r`,
the `S` is empty and `n == 0`.

* `S[0] = a`.

* `S[n] = (Rune(Parent(Hoon(r)))`.

* `S[i] = (Rune(Parent(Hoon(S[i+1])))`,
  for all `i` such that `0 <= i < n`.

Note the following:

* By the above definition, `r` is never an
  element of `S`.

* Because of the way that we constructed `S`,
  the `c`'th runechild of `S[i]` must be a
  rune-ish.

* In the trivial case,
  where a rune-ish is its own anchor,
  `S` is always empty; the reanchor offset is always zero;
  and the anchor column is always the same
  as the rune column.

* If `S` is not empty, it includes `a`
  and all the proper syntactic parents
  of `r` which are descendants of `a`.

We now finish our definition of anchor column,
by defining `Offset(r)`.
`Offset(r)` is the sum of all `PerRuneOff(S[i])`
for all `i` such that `0 <= i <= n`.

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
The anchor rune-ish is the COLLUS (`:+`).
COLLUS is 3-fixed, but all three of its runechildren are
on the rune line, so the per-rune-ish offset is 0.
The anchor column is defined to be
the anchor rune-ish column plus the reanchor
offset, so that in this case,
the anchor column is the same as the anchor rune-ish column.

COLSIG is 0-running, and normal COLSIG indentation align the runsteps
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

Here the rune line is line 346, the rune-ish is again COLSIG,
and it reanchors at TISFAS (`=/`).
TISFAS is 3-fixed, and 2 of its runechildren are on the rune line,
so that the per-rune-ish offset of the TISFAS is that of
its 2nd runechild -- one stop.
The anchor column is therefore one stop after the anchor rune-ish
column.

COLSIG again is 0-running, so that its runsteps should be aligned
one stop after the anchor column,
which is two stops after the TISFAS column.
By the same logic, the TISTIS should be aligned at the anchor column.
(One stop after the TISFAS.)
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
be appropriate for the syntactic structure of the code in which
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
aligned at the anchor column.
The tread is four colons, aligned at the anchor
column and followed by a whitespace character.
The lower riser is a sequence of normal comment lines,
aligned at the column
one stop greater than the anchor column.
A even more formal definition of a staircase is
given in an appendix.

## Vertical Gaps

A **vertical gap** is a gap which contains

* A newline-terminated partial line preamble.
This preamble may be of length 1 -- that is,
it may be just the newline.

* A body of one or more full newline-terminated lines,
all of them header comments or
(in the case of non-standard code) blank lines.

* A partial line postamble.
This is never newline-terminated,
and may be zero-length.

A vertical gap should contain zero or more
**inter-comments**
followed by zero or more **pre-comments**.
Both inter-comments and pre-comments may contain any content,
but in concept,
inter-comments separate the sequence steps from other lexemes,
and from each other;
while pre-comments precede sequence steps.

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

* Comments which do not obey the above rules are
**bad comments**.
A **good comment** is any comment which is not a bad comment.

* A comment is not regarded as a meta-comment
if it can be parsed as structural comment.

A more formal description of a vertical gap body is
given in an appendix.

# Types of tall hoons

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
one stop more
than the runechild that follows it.

* every runechild after the first should have be aligned
one stop before the runechild that precedes it.

In the vertical gaps that belong to backdented hoons,
the inter-comment column location should be the column
location of the first text after the gap;
and the pre-comment column location should be undefined.

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
which obeys the following rules:

* Every hoon except the first is the last
runechild of the previous hoon in the sequence.

* Every tall rune-ish is either

    * a "row-initial" tall rune-ish,
      which should be at the same column location
      as the first hoon in the sequence, or;

    * a "joined" tall rune-ish, that is, another tall
       rune-ish on the same line as
       an initial rune-ish.

* The sequence is maximal.  That is,
no chain is a sub-sequence of a longer chain.

For the purposes of chain inter-line alignment,
we define row and silo as follows:
A row starts with a row-initial rune-ish.
For each row,
the tall rune-ishes and their runechildren on the
initial rune-ish's line,
taken in lexical order and recursively, are siloed.

A runechild which itself is a tall rune-ish expression is never
a silo element -- instead it is broken out into
its rune-ish and runechildren,
and these are become silo elements in that row.
A row may contain multiple lines,
but rune-ishes and
runechildren not on the first line of the row
and are not added as silo elements,
and therefore are not used in determining inter-line alignment.

This implies that

* Every row-initial rune-ish is in silo 0,

* Every tall rune-ish is a separate silo element.

* Every runechildr of a tall rune-ish is a separate
  silo element, unless it is itself a tall rune-ish.

* For chained inter-line alignment,
  the row and silo grid may be "ragged", so that some
  rows do not have elements in every silo.

Every row in a chain starts with a rune-ish, but not every
rune-ish need start a row,
and a given silo may contain both rune-ishes and runechildren.

*From `arvo/sys/hoon.hoon`, lines 1572-1575:*
```
    =|  b/(set _?>(?=(^ a) p.n.a))
    |-  ^+  b
    ?~  a   b
    $(a r.a, b $(a l.a, b (~(put in b) p.n.a)))
```

In the above example, the chain contains 3 rows.
(The 3rd and 4th lines are both in last row.)
There are 3 silos, but the rows are "ragged", so that nothing
in the first row is in the 3rd silo.
In all 3 rows, the 2nd silo is tightly aligned,
as is the 3rd silo in the 2nd row.

The 3rd silo of the 3rd row is inter-line aligned.
The `b` in the 3rd row comes after a gap of 3 spaces,
but this follows the standard set forth
in this document because it aligns with the `b`
of the 2nd row, at column location 13.

Note that the 2nd silo includes both runechild and a rune-ish.
This is an example of a "joined" hoon.
In a chained hoon sequence,
when the row starts with a unary rune,
joined hoons can be convenient.

# Running hoons

A running runechild is more often called simply a **running**.
Currently, no hoon contains more than one running.

A running hoon may contain
an runechild before the running.
That runechild
is called the **head** of the running hoon.

There are current three kinds of regular runnings.

* A **0-running** has no head.
The current 0-running rules are
BUCCEN (`$%`),
BUCCOL (`$:`),
BUCWUT (`$?`),
COLSIG (`:~`),
COLTAR (`:*`),
WUTBAR (`?|`),
and
WUTPAM (`?&`).

* TISSIG (`=~`) also has no head,
  but is a special case.

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

  - runstep lines are aligned two stops
    after the anchor column;

  - the inter-column of the vertical gaps is
    aligned at the anchor column; and

  - the pre-column of the vertical gaps is
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

<!-- TODO: check all uses of runeColumn to be sure that
     anchor column is not what is intended -->

## TISSIG

*From `arvo/sys/vane/ford.hoon`, lines 8-13:*
```
=>  =~
::  structures
|%
++  heel  path                                          ::  functional ending
++  move  {p/duct q/(wind note gift:able)}              ::  local move
:: Lines from 13 on omitted ]
```

Note that, in the example above, the anchor column is different
from the rune column.

A TISSIG hoon should consist of,
in lexical order:

* Its rune.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is undefined

* A running where

  - runstep lines are aligned
    at the anchor column;

  - the inter-column of the vertical gaps is
    aligned at the anchor column; and

  - the pre-column of the vertical gaps is
    undefined.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is undefined.

* A TISTIS aligned at the anchor column.

<!-- TODO: TISSIG is very problematic.
By the above definition of correctness, none
of the TISTIS occurrences in the corpus are correct.
Also, contrary to the above
 there may to be a joined form of TISSIG.
-->

## 1-running hoons

1-running hoons can be joined or split.

### Joined 1-running hoons

*From `arvo/sys/hoon.hoon`, lines 4853-4855:*
```
             ;~  less  soz
               (ifix [soq soq] (boss 256 (more gon qit)))
             ==
```

A joined 0-running hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* Its head.

* A one-stop horizontal gap.

* The first runstep line.

* A vertical gap, whose inter-column is the anchor column,
  and whose pre-column is aligned one stops after
  the anchor column.

* A running where

  - runstep lines are aligned two stops
    after the anchor column;

  - the inter-column of the vertical gaps is
    aligned at the anchor column; and

  - the pre-column of the vertical gaps is
    aligned one stops after the anchor column.

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

A joined 0-running hoon should consist of,
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

A running is considered to start at the start of its first run step.
A running contains one or more **runstep lines**.
The column location of the runstep lines should be as described
for the running hoon that directly contains the running.

Within a runstep line, the runsteps
should be tightly aligned,
or follow runstep inter-line alignment,
as described next.

### Runstep inter-line alignment

In a row of runsteps, runsteps after the first may have
inter-line alignment.
Within a running,
The inter-line alignment column of runsteps is
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

Note that if a backdented hoon is a runestep,
that it may have both a running-inherited inter-line alignment
and a chained inter-line alignment.
If a backdented hoon does have both inter-line alignments,
for every runechild silo,
their column locations should be identical.

Each backdented hoon is a row.
For each row,
The sequence composed of the rune-ish of the hoon,
followed by its runechildren in
lexical order
is siloed.

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

There are current three kinds of jogging.

* The current 1-jogging rules are CENTIS (`%=`), CENCAB (`%_`) and WUTHEP (`?-`).
  A 1-jogging has one head and no tail.

* The current 2-jogging rules are CENTAR (`%*`) and WUTLUS (`?+`).
  A 2-jogging has a head and a subhead and no tail.

* The current jogging-1 rule is TISCOL (`=:`).
  A jogging-1 has a tail and no head.

A jogging is a gap-separated sequence of one or more jogs.
Every **jog** contains a **jog head**, followed by a gap and a **jog body**.
Note that it is important to distinguish between the head of a jogging
hoon, defined above, and the head of a jog.

## Chess-sidedness

Jogs, joggings and jogging hoons have **chess-sidedness**.
Chess-sidedness is always either kingside and queenside.

Informally, **kingside** means the indentation has a left-side bias;
and **queenside** means that the indentation has a right-side bias.
The effect of sidedness on
indentation will be described more precisely in what follows.

## Jogs

The alignment of a jog is that of its head.
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
in the description for the different kinds of jogging hoon.

### Joined jogs

A joined jog may be either **aligned** or **ragged**.
A joined jog is ragged if its body is aligned one stop after
its head.
Otherwise, a joined jog is considered aligned.

All aligned jogs in a jogging should be aligned at the
same column.
This column is called the **jogging body column** of the jogging.
A jog body which is aligned at the jog body column
is said to be **jogging-body-aligned**.
A jog whose jog body is jogging-body-aligned
is also said to be **jogging-body-aligned**.

The pseudo-join of pseudo-joined jog should be
equivalent to the one of the horizontal gaps
that a joined jog is allowed according to this standard.

### Split jogs

The gap of split jog should be vertical.
The inter-comment column location of the gap
should be the column location of the jog body;
and there should be no pre-comment column location.
The standard column location of the body of a split job
varies according to the sidedness of the jog.

* The column location of the body of a split kingside jog
should be 1 stop **greater** than the column location of the jog's head.

* The column location of the body of a split queenside jog
should be 1 stop **less** than the column location of the jog's head.

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

Successive TISTIS's may occur on the same line,
as part of a **criss-cross TISTIS line**.
If a TISTIS is in a criss-cross line, it does not have to
be properly aligned, if some other TISTIS with the required alignment
"stands in" for it.
Such lines are called "criss-cross" because
if you drew
a line that pairs each TISTIS with the rune that it matches by alignment;
and another set of lines, each of which pairs its TISTIS with the rune that
it matches syntactically;
then the two sets of lines would cross each other.

HEPHEP's may also be joined into criss-cross lines.

## 1-jogging hoons

Every 1-jogging hoon is either kingside or queenside.
Specifics are given below,
but the general rule for sidedness is that it should be
consistend.

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

* A head.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A kingside jogging.
  Its base column should be one stop after the anchor column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

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

* A head.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A queenside jogging.
  Its base column should be 2 stops after the anchor column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A TISTIS,
  starting at the anchor column.

## 2-jogging hoons

Every 2-jogging hoon is either kingside or queenside.
Specifics are given below,
but the general rule is that
"sidedness" should be consistent.
The base column
for the jogs of a 2-jogging hoon is its anchor column.

If the head and subhead of a 2-jogging hoon are on the
same line, the 2-jogging hoon is called **head-joined**.
If the head and subhead of a 2-jogging hoon are on
different lines, the 2-jogging hoon is called **head-split**.

### Kingside 2-jogging hoons

All kingside 2-jogging hoons are head-joined.

*From `arvo/sys/hoon.hoon`, lines 6583-6586:*
```
      ?+  p.mod  [%rock %$ 0]
        $cell  [[%rock %$ 0] [%rock %$ 0]]
        $void  [%zpzp ~]
      ==
```

A kingside 2-jogging hoon should consist of,
in lexical order:

* Its rune.

* A one-stop horizontal gap.

* Its head.

* A one-stop horizontal gap.

* Its subhead.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A kingside jogging.
  The base column of its jogs should be one stop after the anchor
  column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A TISTIS,
  starting at the anchor column.

### Head-joined queenside 2-jogging hoons

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

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A queenside jogging.
  The base column of its jogs should be 2 stops after the anchor
  column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A TISTIS,
  starting at the anchor column.

### Head-split queenside 2-jogging hoons

*From `arvo/sys/hoon.hoon`, lines 10111-10116:*
```
      ?+    (rash p.q.ham ;~(sfix (cook crip (star low)) (star hig)))
          ~(rend co [%$ p.q.ham lum])
        $$    ~(rend co [%$ %ud lum])
        $t    (dash (rip 3 lum) '\'' ~)
        $tas  ['%' ?.(=(0 lum) (rip 3 lum) ['$' ~])]
      ==
```

A head-split queenside 2-jogging hoon should consist of,
in lexical order:

* Its rune.

* A 2-stop horizontal gap.

* Its head.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* Its subhead, outdented one stop from the head.
  This style is more indentation-conserving than backdenting.
  It is called the "pseudo-jog" style, because arrangement
  of the head
  and subhead resembles that of a queenside jog.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A queenside jogging.
  The base column of its jogs should be 2 stops after the anchor
  column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A TISTIS,
  starting at the anchor column.

## Jogging-1 hoons

<!-- "Split" form addressed in Github issue #31" -->

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
  The base column of its jogs
  should be 2 stops after the anchor column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A TISTIS,
  aligned one stop after than the anchor column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A tail, starting at the anchor column.

# Battery hoons

The battery hoons are BARCAB, BARCEN and BARKET.

## Batteries

The cells of a battery should all align at the
same column, called the **cell alignment column**.
The cells should be separated by vertical gaps,
where the base column for header comments is the cell
alignment column.
The "cell alignment column" is specified below, for each
battery hoon.

[ TODO: Describe intra-LUSLUS conventions. ]

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

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A battery whose base column is the anchor column.

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

A split BARCEN should consist of,
in lexical order:

* Its rune.

* A one stop horizontal gap.

* A battery whose base column is
  two stops after the rune column.

* A vertical gap.
  Its inter-comment column should be the anchor column.
  Its pre-comment column should be undefined.

* A HEPHEP, starting at the anchor column.

## BARCAB

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
  or a equivalent pseudo-join.

* Its head.

* A vertical gap.
The inter-comment column location of the vertical gap should
be the anchor column and the pre-column should be undefined.

* A battery, whose base column location should be the anchor column.

* A vertical gap.
The inter-comment column location of the vertical gap should
be the anchor column and the pre-column should be undefined.

* A HEPHEP, starting at the anchor column.

## BARKET

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

* A vertical gap.
The inter-comment column location of the vertical gap should
be the anchor column and the pre-column should be undefined.

* A battery, whose base column location should be the anchor column.

* A vertical gap.
The inter-comment column location of the vertical gap should
be the anchor column and the pre-column should be undefined.

* A HEPHEP, starting at the anchor column.

# Ford-1 hoons

The ford-1 hoons are a set of unary ford hoons:
FASSIG (`/~`),
FASBUC (`/$`),
FASCAB (`/_`),
FASCEN (`/%`) and
FASHAX (`/#`).

<!-- TODO: fordHoop? -->

*From `arvo/app/hall.hoon`, line 17:*
```
          /~  |=({t/telegram:hall bowl:gall} t)
```

A tall Ford-1 hoon should consist of,
in lexical order:

* The rune.

* A one-stop horizontal gap.

* The runechild.

# FASCOM

TODO

# FASHEP

TODO

# FASLUS

TODO

# FASTIS

TODO

# FASWUT

TODO

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

In non-standard code -- code which does not follow these guidelines,
`hoonlint` sometimes must decide the "intended" syntax,
in order to produce diagnostics that are as helpful as possible.
This section describes the methods used for deciding what was
"intended" in non-standard Hoon code.

## Chess-sidedness

A jog is considered queenside if it is aligned 2 stops or more
after the anchor column.
Otherwise, the jog is considered kingside.

The chess-sidedness of a jogging is that of the majority
of its jogs.
In case of a tie, the jogging is considered to be queenside.
The chess-sidedness of a jogging hoon is that of its
jogging.

## Jogging body column

In non-standard code,
the jogging body column of a jogging is considered to be the most common start column
of the bodies of the jogging's aligned jogs.
If more than one column is "most common",
so that there is a tie,
the jogging body column is
the body column of the lexically first jog to have its jog body
start at one of the most common columns.
If there are no aligned jogs in a jogging,
the jogging body column is the body column of the
lexically first jog in the jogging.

## Inter-line alignment

In non-standard code,
the inter-line alignment of a column of lexemes is
the alignment most common in the "wide" lexemes.
(Wide lexemes are those which do not have tight or backdented
alignment.)

If two alignments tie by wide lexeme count,
the tie is broken using the total lexeme count.
If two alignments tie by total lexeme count,
the tie is broken in favor of the alignment which
occurs first, lexically.

If there are no wide lexemes, the inter-line alignment is
irrelevant and left undefined.
Also,
the inter-line alignment is undefined unless it
has a total lexeme count of at least 2 --
in other words
an inter-line alignment must actually align with a corresponding lexeme
on another line.

## Inter-line alignment by silo

In the case of chained inter-line alignments,
runstep inter-line alignments,
and running-inheritied inter-line alignments,
multiple silos are involved.
The rules multi-silo inter-line alignments
in non-standard code are an extension of the basic
rules for inter-line alignments in non-standard code.
When only one silo is involved,
the rules are equivalent.

Each silo is examined separately.
For each silo, only rows with an element in that silo participate
in the calculation.

For runstep alignment and running-inherited alignment,
the "wide" elements at those which are not
tightly aligned.
For chains, the "wide" elements
are those which are tightly or backdented
aligned.

If a running could have both a runestep alignment,
and a running-inherited alignment,
the runestep alignment takes precedence.
In other words,
if a running has runestep alignment,
the running-inherited alignment is undefined.

# Appendix: Vertical gap body

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

* BadComment -- a comment with any alignment. Priority 3.

* BlankLine -- a line terminated with a newline and
otherwise containing only zero or more spaces. Priority 3.

* InterComment -- a comment that starts at the inter-comment column.
Priority 1.

* LowerRiser -- a comment that starts one stop past the inter-comment
column.
Priority 1.

* MetaComment -- a comment that starts at column zero.
Priority 2.

* PreComment -- a comment that starts at the pre-comment column.
Priority 1.

* Tread -- a comment that starts at the inter-comment column,
whose first four characters are colons,
and whose fifth character is either a space or a newline.
Priority 1.

* UpperRiser -- a comment that starts at the inter-comment column.
Priority 1.

Terminals are ambiguous -- a given line may match more than one terminal.
The lexer limits this ambiguity using the "priorities".
Comments are read as if tested for each
priority in numerical order.
This has the slightly counter-intuitive effect
that the highest priority is the lowest numbered one.
Comments with the same priority are treated as if tested
all at once, which allows for ambiguous terminals.

Specifically,

* No comment is read as a Priority 3 comment if it can be
lexed as a Priority 1 or 2 comment.

* No comment is read as a Priority 2 comment if it can be
lexed as a Priority 1.

