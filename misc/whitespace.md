# About this document

This document specifies a
convention for the use of whitespace by
Hoon to the level and to the degree of precision
necessary for `hoonfmt` and `hoonlint`.
This document only deals with conventions
for Hoon expressions that contain gaps.
These are

* tall Hoon expressions; and

* SELGAP, a special case.

Whitespace in other Hoon expressions is always an "ace" -- a single space.
For Hoon expressions in which all whitespace is an ace,
there is no latitude in the use of whitespace,
and therefore no need for any conventions.

# Terminology

## Deontology

In this document, we say "`X` should be `Y`",
if `X` must be `Y` in order to meet the standard described
in this document.
Code or practices that obey this standard we
will call **standard**.
Code or practices that are not standard we
will call **non-standard**.
One pragmatic consequence of non-standard code
is that
it may draw warnings from `hoonlint`.

In this document, we say "`X` must be `Y`",
if `X` must be `Y` in both standard and non-standard code.
This would be the case, for example,
if X is Y as a matter of definition;
if X is Y due to a logical or a mathematical equivalence;
or if the Hoon syntax requires that X be Y.

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

## Rows, silos and slots

A row-silo grid is a 2-dimensional grid of **slots**,
where a row is a sequence of slots which share
the same position on the vertical axis,
and a silo is a sequence of slots which share
the same position on the horizontal axis.
For orthogonality with columns and lines,
row and silo numbers are 1-based.
There will be several definitions
of "row" and "silo", and we will give
specifics in the context
where they are used.

When, for a given row,
we say that "sequence `S[0], S[1] ... S[n]` is **slotted** beginning at slot `x`",
we mean that, for `0 <= i <= n`, `S[i]` goes into slot `i+x`.
When, for a given row,
we say that "sequence `S[0], S[1] ... S[n]` is **slotted**",
we mean that
"sequence `S[0], S[1] ... S[n]` is **slotted** beginning at slot 1",

## Characters and character boundaries

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

## Blocks

A character block is a lexically contiguous sequence of one or more
characters.
A whitespace block is a character block
that contains only whitespace characters
and occurs between two whitespace boundaries.
An **ace** is a whitespace block that contains a single ASCII space.
A **gap** is any whitespace block that is not an ace.

A **flat gap** is a gap that contains only ASCII spaces.
A **multiline gap** is a gap that contains at least one newline.
All gaps are either flat or multiline.

A **text block** is a character block

* that begins at a whitespace boundary;

* whose first character is a non-whitespace character;

* that ends at a whitespace boundary; and

* whose last character is a non-whitespace character.

Note that text blocks may contain whitespace characters.
We often refer to a text block simply as a **text**.

## Location and alignment

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
"text X is aligned N characters after text Y",
we mean that `Column(X) = Column(Y) + N`.

Often we express distance between columns
in stops instead of characters.
A **stop** is two characters.

Let *T1* and *T2* be two text blocks on the same line.
We say that *T2* is **tightly separated**,
if and only if

* *T1* is separated from *T2* by a one-stop flat gap, and

* *T1* precedes *T2*.

If a text block is **tightly separated**,
we also say that it is **tightly aligned**.
Note that, by this definition,
the first text block on a line can never be tightly aligned.

Many alignments are with respect to an **anchor column**.
In the context of a specific Hoon statement, **h**,
the anchor column typically is `Column(Rune(h))`.
But if the rune of **h** is part
of a non-trivial curried text block,
as described below,
the anchor column of **h** may be aligned at a different rune.

## Hoon statements

A Hoon source file contains one or more Hoon **statements**.
The Hoon statements are separated by whitespace.
Optionally, the first Hoon statement may be preceded by whitespace.
Also optionally, the last Hoon statement may be followed by whitespace.

A Hoon **statement** is a text block which compiles to a
single AST node.
We sometimes refer to a Hoon statement as a "hoon",
but strictly speaking, the "Hoon statement" is the text block
before parsing, and the "hoon" is the AST produced by parsing.

## Runes

In this document, **rune** is used in a somewhat different
sense than in most Hoon documents.
In most Hoon documents, "rune"
means a digraph which acts as the keyword at
the beginning of a Hoon statement.
In this document, we will refer to those as
"statement runes".

In this document,
"rune" will mean either

* a statement rune; or

* an arm marker digraph:
  one of LUSLUS (`++`), LUSHEP (`+-`), or
  LUSTIS (`+=`).

Note that not all special-character digraphs are runes.
In this document,
HEPHEP (`--`) or TISTIS (`==`) will be called **boundary digraphs**,
and in this document they are not considered runes.
When context makes the meaning clear,
a boundary digraph is usually called simply a **boundary**.

Comment-marking digraphs,
such as `::`,
are also not considered runes.
Comments, including the digraphs that begin them,
are considered whitespace,
and nothing in whitespace is considered a rune.

## Hoon expressions

A tall **rune expression** is a tall Hoon expression whose "keyword"
is a rune.
A tall rune expression consists of the initial rune
followed by zero or more subexpressions,
called **runechildren**.

The **arity** of a rune expression is the number of
its runechildren.
In the tall Hoon expressions which are the focus in this document,
the rune is separated from its first runechild,
and consecutive
runechildren are separated
from each other, by gaps.

Let `r` be the rune digraph of a rune expression.
The **rune line** of `r` is `Line(r)`.
The **rune column** of `r` is `Column(r)`.

Let `r` be a rune.
We sometimes write the Hoon expression that begins with `r`
as `Hoon(r)`.
If `h` is a Hoon, we can write its rune as `Rune(h)`.
This implies that

```
   Line(Hoon(r)) == Line(r)
   Column(Hoon(r)) == Column(r)
```
Let `h` be a rune expression.
We sometimes write the Hoon expression that is the syntactic parent
of `h` as `Parent(h)`.

Let *H* be a text block which is a Hoon statement,
and let *T1* be a text block.
*H* **properly contains** *T1*
if *H* contains *T1*,
but *H* is not identical to *T1*.
*H*
**directly contains** *T1* if
*H* properly contains *T1*
and no other properly contained 
Hoon statement of *H*
properly contains *T1*.

## Joined and split

For many Hoon statements,
the pattern of the standard whitespace conventions depends
on whether a specific gap in that Hoon statement
is horizontal or vertical.
A flat gap is always considered horizontal,
and a vertical gap is always multiline.
But, because of pseudo-joins,
a multiline gap is not necessarily
a vertical gap.
Pseudo-joins are defined below.

If the pattern-decisive gap is horizontal, the
conventions for the hoon which directly contains
that gap follow a "joined" pattern.
If the pattern-decisive gap is vertical, the
conventions for the hoon which directly contains
that gap follow a "split" pattern.
The specific conventions will be specified below,
with each syntax.

## Pseudo-joins

It is sometimes convenient to have a multiline
pattern-decisive gap,
but to otherwise follow
the "joined" syntax pattern.
For this, it useful to have a form of multiline
gap which is treated as a horizontal gap.

This is useful,
for example,
when the programmer finds "joined"
syntax appropriate, but wants to include
a comment in the pattern-decisive gap.
A multiline gap that
is treated as if it were a horizontal gap
is called a **pseudo-join**.

*From `app/talk.hoon`, lines 24-33*:
```
=>  :>  #
    :>  #  %arch
    :>  #
    :>    data structures
    ::
    |%
    ++  state                                           :>  application state
      $:  ::  messaging state                           ::
          grams/(list telegram)                         :<  all history
          known/(map serial @ud)                        :<  messages heard
```

The above example contains two pseudo-joins.
The first pseudo-join runs from line 24 to line 29.
The second pseudo-join runs from line 31 to line 32.

Let `J` be a **join column**.
A multiline text is a **pseudo-join** at `J`
if and only if every line of it,
except the last line,
has a comment aligned at `J`.
This implies that the first, partial, line of the pseudo-join
must contain a comment aligned at `J`.

Visually, the pseudo-join's comments look like "place holders" for
the text that follows the pseudo-join.

Intuitively, a pseudo-join is
said to be
**equivalent**
to a flat gap
if the text that follows both of them
is aligned at the same column location.
More formally,
Let `file1` be a Hoon source file.
Let `J` be the join column of a pseudo-join in `file1`.
Construct another Hoon source file, `file2`, by
lexically replacing the pseudo-join with a flat gap.
Call that flat gap, `G`.
Let `H` be the column location immediately after `G` in `file2`.
The pseudo-join and the flat gap are **equivalent**
if and only if `J == H`.

## Horizontal, multiline and vertical separation

If a flat gap is allowed according to these conventions,
it is a **comformant** flat gap.
If the gap immediately preceding a text block is a pseudo-join
equivalent to a comformant flat gap,
the text block is **pseudo-joined**.

If the gap immediately preceding a text block is flat
or a pseudo-join,
the text block 
is **horizontally separated**.
If a text block is not 
horizontally separated,
then it is **vertically separated**.

## Inter-line Alignment

Inter-line alignment aligns a text block
with one or more text blocks on "associated" lines.
There are several different kinds of inter-line
alignment,
and each differs in its definition
of which text blocks should be aligned with which,
and which lines it "associates" with the current line.
The specific definitions of inter-line alignment
will be described in detail
in the sections to follow.

# Header and inline comments

Comments count as whitespace.
A comment is a **header comment** if it is on a line
by itself.
If a comment is not a header
comment, it is a **rightside** comment.
A rightside comment is
a **margin comment** if it begins at or after column 57,
or immediately after a sequence of 20 or more ASCII spaces.
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
Depending on their hoon,
inter-comments may also start at column 1.
When inter-comments start at column 1,
only the programmer's intent determines whether
a given header comment is an inter-comment or a meta-comment.

Pre-comments, inter-comments and staircases are structural comments.
The contents of structural comments wll typically
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

# Vertical Gaps

When we state that a vertical gap
has **comments at location A**,
we mean
that the gap's inter-comment column location
is the stated column location,
and that the gap's pre-comment column location
is not defined.
When we state that a vertical gap
has
**comments at location A as well as location B**,
we mean
that the gap's inter-comment column location
is location A,
and that the gap's pre-comment column location
is location B.

For example, when we write

> A vertical gap, with comments at
> the anchor column as well as the jog base column

we mean that the inter-comment location of the vertical gap is
the anchor column,
and the pre-comment location is the "jog base column".
If instead we write

> A vertical gap, with comments at the anchor column.

then we mean that the inter-comment location of the vertical gap is
the anchor column,
and the pre-comment location is not defined.

In describing a vertical gap's comment locations,
we may specify them explicitly as column locations,
or implicitly, as alignments with a text block.
If column location is expressed implicitly via a text block,
the implicit location is the start column
location of the text block.

A vertical gap contains, in lexical order,

* A newline-terminated partial line preamble.
This preamble may be of length 1 -- that is,
it may be just the newline character.

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

* Meta-comments may be inserted anywhere in, before, or after,
either the pre-part
or the inter-part.

* A comment is not regarded as a meta-comment
if it can be parsed as structural comment.

A formal description of the vertical gap body is
given in an appendix.

# Types of tall hoons

Every tall hoon falls into one of 8 disjoint classes:
running, jogging, battery, irregular,
Ford, Sail, Udon and basic syntax.

* A tall hoon is a **running hoon** if it contains a runechild that
uses the running syntax.

* A tall hoon is a **jogging hoon** if it contains a runechild that
uses the jogging syntax.

* A tall hoon is a **battery hoon** if it contains a runechild that
uses the battery syntax.

* SELGAP is the only tall **irregular hoon**.
(Most irregular hoons do not contain a gap.)

* Ford statements have a specialized syntax,
as described under "Details".

* Sail statements have a specialized syntax,
as described below.

* Udon (Unmarkdown) Hoon statements have a specialized syntax.
The whitespace convention of this document allows Udon statements,
but does impose any restrictions on their whitespace format.

* A tall hoon is a **basic syntax hoon** if it does not fall into
any of the above, more complex, classes.

# Top-level whitespace

We call a Hoon statement not contained by another Hoon statement,
a **top-level** Hoon statement.
A Hoon source file is a sequence of top-level Hoon statements.
All top-level Hoon statements should start at column 1.
Successive top-level Hoon statements should be separated by
a vertical gap with
comments at column location 1.

The first top-level Hoon statement may be preceded by a vertical gap,
called the **top-level leader**.
Comments in the top-level leader should be at column location 1.
The top-level leader is a special-case of a vertical gap --
it contains a body and
a zero-length postamble, but no preamble.

The last top-level Hoon statement may be followed by a vertical gap,
called the **top-level trailer**.
Comments in the top-level trailer should be at column location 1.
The top-level trailer is a special-case of a vertical gap --
it contains a body and
a preamble, but no postamble.

# Backdenting

The flagship Hoon whitespace strategy is backdenting.
Variations on the idea of backdenting appear throughout,
but the archetypal case of backdenting is its use in
basic syntax hoons.
Here is a example of a 4-ary hoon:

*From `arvo/sys/hoon.hoon`, 6752-6755:*
```
        :^    %wtcl
            [%dtts [%rock %$ |] [%$ axe]]
          [%rock %f |]
        [%rock %f &]

```

A basic syntax hoon of arity 3 or more should be joined.
A basic syntax hoon of arity 2 or less should be split.

The first runechild of a joined basic syntax hoon should be
on the same line as the rune, separated by a horizontal gap.
The first runechild may be tightly aligned,
or aligned at the backdent column.
Subsequent runechildren of a joined basic syntax hoon should be
tightly aligned with the previous runechild,
or separated from the previous runechild by a vertical gap.

The first runechild of a split basic syntax hoon should be
vertically separated.
Subsequent runechildren of a joined basic syntax hoon should be
tightly aligned with the previous runechild,
or vertically separated.

## Backdenting and vertical separation

In a basic syntax `n`-arity hoon,
a vertically separated `m`'th runechild should be aligned `n-m` stops after than the anchor column.
Here `m` is 1-based, so that the 1'th runechild is the first runechild,
the 2'th runechild is the second runechild, etc.

For example,
in an 3-runechild hoon,
a vertically separated first runechild should be aligned 2 stops after than the anchor column;
a vertically separated second runechild should be aligned 1 stop after than the anchor column;
and a vertically separated third runechild be aligned at the anchor column.

This implies that, regardless of the number of runechildren in a
backdented hoon,
a vertically separated last runechild should be aligned at the anchor column.
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
a sequence is not considered a chain
if it is a sub-sequence of a longer chain.

For the purposes of chain inter-line alignment,
we define row and silo as follows:
Each row-initial rune starts a new row.
For each row,
the tall runes and their runechildren on the
initial rune's line,
taken in lexical order and recursively, are slotted.

Unless it is a 1-ary rune,
a tall rune expression is never
a silo element.
Hoon statements of arity greater than 1 must be broken out into
its statement rune and its runechildren,
before being slotted.
A row may contain multiple lines,
but runes and
runechildren not on the first line of the row
are not slotted
and therefore are not used in determining inter-line alignment.

This implies that, in the first line of a row

* Every row-initial rune is in slot 0,

* Every statement rune is in its own slot.

* Every runechild of a tall rune is in its own slot,
  unless that runechild is itself a non-unary tall rune expression.

Further,

* For chained inter-line alignment,
  the row and silo grid may be "ragged", so that some
  rows do not have elements in every slot.

* A slot with no element is called "empty".  In a given
  row, a non-empty slot cannot follow an empty slot.
  In other words, all the empty slots must be at the
  end of a row.

* Every row in a chain starts with a rune, but not every
  rune starts a new row.

* A silo may be "mixes", containing both runes and runechildren.

*From `arvo/sys/hoon.hoon`, lines 1572-1575:*
```
    =|  b/(set _?>(?=(^ a) p.n.a))
    |-  ^+  b
    ?~  a   b
    $(a r.a, b $(a l.a, b (~(put in b) p.n.a)))
```

In the above example, the chain contains 3 rows.
(The 3rd row is multiline -- it contains
the 3rd and 4th lines.)
There are 3 silos and therefore 3 slots in every row.
The 2nd silo is mixed --
it contains a rune (at row 2)
as well as runechildren (at rows 1 and 3).

The 3rd slot of the 1st row is empty,
so the slotting is "ragged".
All 3 rows have non-empty 1st and 2nd slots.

In all 3 rows, the 2nd slot is tightly aligned,
The 3rd slot in the 2nd row is also tightly aligned.

Only one non-empty slot is not tightly aligned.
The 3rd slot of the 3rd row is inter-line aligned.
The `b` in the 3rd row comes after a gap of 3 spaces,
but this follows the standard set forth
in this document because it aligns with the `b`
of the 2nd row, at column location 13.

Note that the 2nd row
is an example of a "joined" hoon.
The 2nd row contains two runes --
both the 1st and 2nd slots of the 2nd row are
runes.
In a chained hoon sequence,
when a row starts with a unary rune,
joined hoons can be convenient.

# Curried backdenting

Curried backdenting is a method of conserving indentation.
Curried backdenting
treats a **curried text block**
as a single rune 
for backdenting purposes.
A curried text block must start
with a rune,
and must end with a rune on the same line.
This implies that a curried text block cannot
contain a multiline gap.

A curried text block is also called a **currying**.
When a rune is in a currying,
its anchor column is based on the first rune of the curried text block,
and this may involve changing the anchor column of some of the runes
in the currying.
This shift of the anchor column is called **reanchoring**.

We call the last rune in a currying,
the **source rune**
or the **reanchored rune** of the currying.
We call the first rune of
the curried text block,
its **target rune**,
or the **anchor rune** of the currying.

Pedantically, every rune is in a curried text block,
but most curryings are **trivial** --
they consist only of a single
rune.
In a trivial currying the reanchored rune
is the same as the anchor rune or,
to put the same thing in different words,
and the source rune is the same as the target rune.

Not all pairings of source and target rune are allowed.
If a currying whose source is rune `S` and
whose target is rune `T` is allowed,
we say the
**rune S curries with rune T**.
Trivial curryings are always allowed.
Which rune is allowed to pair with which other runes
in a non-trivial curryings is described in
"Details".

The runechildren of a currying are backdenting according
to the standard rules for backdenting.
The anchor column of a currying is the column location
of the anchor rune.
The arity of a currying is the sum of the arites of the
curried runes, less the number of runechildren in the currying.

Since backdenting follows the same rules for curried hoons
as for ordinary hoons,
backdenting a trivial currying is equivalent to
backdenting its rune.
However, non-trivial currying
will change the anchor column.

## First example


*From `arvo/sys/vane/ford.hoon`, lines 1916-1920:*
```
        :+  %depends  %|  :~
          definitions+[%& deh]
          listeners+[%& sup]
          waiting+[%& out]
        ==
```

The currying is on the line 1916, and contains two runes:
COLSIG (`:~`) is the source and COLLUS (`:+`) is the target.
The arity of COLSIG is 1 and the arity of COLLUS is 3.
The currying contains 3 runechildren,
so the currying is 1-arity: `(1 + 3) - 3 = 1`.

The anchor rune is the COLLUS (`:+`) at column 9,
so the anchor column of the currying is column 9.
COLSIG is 0-running, so the running is its first and only
child.
Normal backdenting aligns the 1st runechild
of a 1-arity hoon at the anchor column (column 9).
As we will see when we deal with runnings,
the runsteps should be aligned one stop after running,
which means they are at column 11 (`11 = 9 + 2`).
The final TISTIS of running should be aligned
at the anchor column (column 9).
These alignments are what we see in the display above.

## Second example

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

The currying is line 346, the source rune is again COLSIG,
and the target rune is TISFAS (`=/`).
TISFAS is 3-fixed, and 2 of its runechildren are on the rune line.
COLSIG is 1-arity, and its runechild is not on the rune line.
The currying therefore contains a total of 2 runechildren.

Calculating the arity of the currying we have 3 (the arity of TISFAS)
plus 1 (the arity of COLSIG)
less 2 (the count of runechildren in the currying).
Therefore the currying is 2-arity: `2 = (3+1)-(2)`.

The running of COLSIG is the first runechild of a 2-arity (curried) hoon,
and therefore by the standard backdenting rules should be
aligned at column 7,
one stop after the anchor column (column 5).
The runsteps are aligned one stop after the running,
which is column 9 (`9=7+2`).
The TISTIS of the running
is aligned with the running,
and therefore is at column 7.

The last child of the TISFAS is on line 354.
It is the 2nd child of the 2-arity currying and therefore,
by the standard backdenting rules,
is aligned at the anchor column (column 5).

# Split hints

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

The tail of a split form hint should be vertically separated,
with comments aligned with the head.
The tail should also be aligned with the head.

# Running hoons

A running runechild is more often called simply a **running**.
Currently, no hoon contains more than one running.

A running hoon may contain
a runechild before the running.
That runechild
is called the **head** of the running hoon.

## Runnings

A running contains one or more **runstep lines**.
Runstep lines should be aligned at the **runstep base column**,
with exceptions.
The runstep base column location
is usually one stop after the column location
of the running,
but there are exceptions, as described in "Details".

Within a runstep line, the runsteps
should be tightly aligned,
or they should follow runstep inter-line alignment,
as described next.

A running is terminated by a
TISTIS, which should be aligned with the running.
The TISTIS should be vertically separated,
with comments aligned with the running,
and with the runsteps.

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
Within each row,
the runsteps are slotted in lexical order.

### Running-inherited inter-line alignment

If a runstep is a basic syntax hoon,
the runechildren of the basic syntax hoon
may have a **running-inherited** inter-line alignment.
In running-inherited inter-line alignment,
every basic syntax hoon is a row.
Within each row,
first the rune of the hoon is slotted,
then its runechildren in
lexical order.

Within a running,
the running-inherited inter-line alignment of a runechild
is determined by the slot of the runechild.

*From `sys/hoon.hoon', lines 5303-5308:*
```
    :~  :-  ['a' 'z']  (cook |=(a/@ta [%$ %tas a]) sym)
        :-  ['0' '9']  (stag %$ bisk)
        :-  '-'        (stag %$ tash)
        :-  '.'        ;~(pfix dot perd)
        :-  '~'        ;~(pfix sig ;~(pose twid (easy [%$ %n 0])))
    ==
```

### Mixed inter-line alignments in a running

A running should not have both running-inherited and runstep
alignment.
But note that if a basic syntax hoon is a runstep,
it may have both a running-inherited inter-line alignment
and a chained inter-line alignment.
If a basic syntax hoon does have both inter-line alignments,
they should be consistent.
That is,
for every slot,
the column location of that slot according
to running-inherited alignment
should be identical to
the column location of that slot according
to chained alignment.

# Jogging hoons

A jogging runechild is more often called simply a **jogging**.
A Hoon statement contains at most one jogging.

In addition to the jogging, a jogging hoon may contain
other runechildren, either before or after the jogging.
An runechild after the jogging is called a **tail**.
If there is one runechild before the jogging, it
is called the **head** of the jogging hoon.
If there are two runechildren before the jogging, they
are called, in order, the **head** and **subhead** of
the jogging.

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
a Hoon statement.

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

The base column location of a jog depends
on the chess-sidedness of the jogging hoon.
The base column of kingside jogs is one stop after the anchor column.
The base column of queenside jogs is two stops after the anchor column.

### Joined jogs

A joined jog may be either
**inter-line aligned** or **ragged**.
A joined jog is ragged if its body is tightly aligned.
Otherwise, a joined jog is considered inter-line aligned.
A jog may also be pseudo-joined.

All inter-line aligned jogging should be aligned at
the jogging body column.
With each jogging,
at most one column is designated as
the **jogging body column** of the jogging.

### Split jogs

The gap of split jog should be a vertical gap
with comments at the column location of the jog body.
The standard column location of the body of a split jog
varies according to the sidedness of the jog:

* The column location of the body of a split kingside jog
should be aligned one stop
**after** the jog's head.

* The column location of the body of a split queenside jog
should be aligned one stop
**before** the jog's head.

## Jogging terminator

A jogging is terminated by a
TISTIS, which should be aligned with the jogging.
The TISTIS should be vertically separated,
with comments aligned with the jogging,
and at the jog base column.

# Batteries

The column at which a battery starts is
the **base column** of the battery.
The arms of a battery should all align at the
the base column
of the battery.
The arms should be separated by vertical gaps,
where the comments are at the base column,
as well as two stops after the base column.
The base column of the battery is specified below,
in the description of each battery hoon.
Arms may be joined or split.

Battery hoons are joined or split according to whether
the battery is horizontally or vertically separated.
A joined battery should be tightly aligned.
A split battery should be vertically separated,
with comments at the battery base column;
and with the battery itself
aligned at the battery base column.

A battery should be followed by a HEPHEP,
aligned at the battery base column.
The HEPHEP should be vertically separated,
with comments at the battery base column
as well as
one step after the battery base column.

*From sieve_k example, line 6:*
```
++  abet  (sort (~(tap in fed)) lth)
```

A joined arm consists of,
in lexical order:

* A arm marker.

* A one-stop flat gap.

* The arm head.

* A one-stop horizontal gap.

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

* A one-stop flat gap.

* The arm head.

* A vertical gap,
  with comments at the base column,

* The arm body,
  aligned one stop after the anchor column.

# Criss-cross lines

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

Recall that a boundary is
a HEPHEP (`--`) or
a TISTIS (`==`).
Successive boundaries may occur on the same line,
as part of a **criss-cross boundary line**.
A boundary is in a criss-cross line does not have to
be aligned correctly,
if some other boundary with the required alignment
"stands in" for it.

Note, however, that since every boundary on the line requires some
boundary to be aligned according to this standard,
all of the boundaries end up with an alignment requirement.
In the example above, the two boundaries on line 918 stand in
for each other,
and each imposes an alignment requirement on the other.

Criss-cross lines are so called because
if you draw
a line from each boundary on the criss-cross line
to the rune that
it matches syntactically,
you end up with
a set of lines,
each of which crosses
all the others.

Only one kind of boundary should appear in a criss-cross line.
Every character in a criss-cross line should be
part of a boundary, or part of a gap.

# Sail

Sail statements start with a semicolon.
Sail statements do not curry.

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
The rune child should be
separated from the rune by a one-stop flat gap.

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

All elements after the first should be preceded by a vertical gap,
with
comments aligned at the SEMTIS
as well as with the first element.

The TISTIS should be aligned with the SEMTIS.
The TISTIS should be vertically separated
with comments aligned at the SEMTIS
as well as with the first element.

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
as well as with the first element.

The TISTIS should be aligned with the tagged sail statement
that it terminates.
The TISTIS should be preceded by a vertical gap,
with comments aligned with the tagged sail statement,
as well as with the first element.

## Sail attributes

The sail attribute base column is two stops after start column location
of the sail text block.
Sail tall attributes should be separated from each other by a vertical
gap with comments aligned at the attribute base column.
Values in sail attributes should be on the same line
as the sail key.
The sail attribute values should be tightly aligned,
or aligned with each other.

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

# Details

This "Details" section contains specific and detailed
whitespace conventions.
Where they conflict with
the more general principles set forth
in the rest of this
document,
these "details" override.

## Currying basic syntax hoons

The basic syntax hoons curry as follows:

* BARDOT curries with
  CENHEP,
  CENLUS,
  KETTIS, and
  LUSLUS.

* BARHEP curries with
  KETTIS and
  TISDOT.

* BARTIS curries with
  COLHEP.

* CENDOT curries with
  BARTIS and
  CENHEP.

* CENHEP curries with
  CENHEP,
  COLHEP,
  TISLUS, and
  TISGAL.

* CENLUS curries with
  CENHEP and
  TISLUS.

* COLCAB curries with
  CENHEP,
  CENLUS, and
  COLCAB.

* COLHEP curries with
  CENLUS and
  COLCAB.

* KETHEP curries with
  BARDOT,
  BARHEP,
  BARSIG,
  BARTIS,
  CENHEP,
  CENLUS,
  COLHEP,
  KETHEP,
  KETSIG,
  KETTIS,
  TISFAS,
  TISGAL,
  TISHEP,
  TISTAR,
  TISDOT,
  SIGLUS, and
  ZAPGAR.

* KETLUS curries with
  BARDOT,
  BARHEP,
  BARTIS,
  BARTAR,
  CENHEP, and
  TISGAL.

* KETSIG curries with
  CENLUS.

* KETWUT curries with
  BUCCAB and
  LUSLUS.

* SIGCAB curries with
  BARTIS.

* SIGFAS curries with
  LUSLUS.

* SIGLUS curries with
    BARDOT,
    BARTIS, and
    CENLUS.

* TISBAR curries with
  BUCCAB and
  BARTIS.

* TISCOM curries with
  TISCOM.

* TISDOT curries with
  TISLUS.

* TISGAL curries with
  BARTIS,
  CENHEP,
  CENLUS,
  KETLUS,
  TISGAL, and
  TISGAR.

* TISGAR curries with
    CENLUS and
    TISGAR.

* WUTCOL curries with
  BARHEP,
  CENHEP, and
  WUTCOL.

* WUTDOT curries with
  BARHEP.

* WUTGAL curries with
  BARHEP.

* WUTGAR curries with
  SIGBAR.

* WUTSIG curries with
  TISLUS and
  WUTSIG.

* ZAPCOL curries with
  LUSLUS.

* ZAPDOT curries with
  LUSLUS.

* ZAPGAR curries with
  CENHEP.

## Running hoons

There are two kinds of running hoons:

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

* COLSIG (`:~`) runes curry
at
CENDOT (`%.`),
CENHEP (`%-`),
CENLUS (`%+`),
COLLUS (`:+`),
KETHEP (`^-`),
TISFAS (`~/`) and
TISGAR (`~>`).

* COLTAR (`:*`) runes curry
at CENHEP (`%-`).

* TISSIG (`=~`) runes curry at
TISGAR (`~>`) and
WUTLUS (`?+`).

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
  runstep lines are aligned two stops
  after the anchor column.
  Runstep lines should be separated by
  vertical gaps with comments
  at the anchor column
  as well as
  aligned at the runstep lines.

* A vertical gap with comments is the anchor column,
  as well as aligned at the runstep lines.

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

* A vertical gap, with comments at the anchor column
  as well as aligned at the runstep lines.

* A running where runstep lines are aligned one stop
  after the anchor column.
  The runstep lines are separated by
  vertical gaps with comments
  at the anchor column as well as
  aligned with the runstep lines.

* A vertical gap, with comments at the anchor column,
  as well as aligned at the runstep lines.

* A TISTIS aligned at the anchor column.

<!-- TODO: check all uses of runeColumn to be sure that
     anchor column is not what is intended -->

## 1-running hoons

1-running hoon statements do not curry.
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

* A one-stop flat gap.

* Its head.

* A one-stop flat gap.

* The first runstep line.

* A vertical gap with comments at the anchor column,
  as well as aligned one stop after
  the anchor column.

* The remainder of the running, where
  runstep lines are aligned one stop
  after the anchor column.
  The runsteps lines should be separated by
  vertical gaps with comments
  at the anchor column as well as
  aligned one stop after the anchor column.

* A vertical gap
  with comments
  at the anchor column as well as
  aligned one stop after the anchor column.

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

* A one-stop flat gap.

* Its head.

* A vertical gap, with comments at the anchor column,
  as well as aligned one stop after
  the anchor column.

* A running where runstep lines are aligned one stop
    after the anchor column.
    Comments in the running are
    aligned at the anchor column as well as
    aligned one stop after the anchor column.

* A vertical gap, with comments at the anchor column,
  as well as aligned one stop after the
  the anchor column.

* A TISTIS aligned at the anchor column.

## Jogging hoons

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

## 1-jogging hoons

Every 1-jogging hoon is either kingside or queenside.
1-jogging runes curry with KETLUS (`^+`).

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

* A one-stop flat gap.

* Its head.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A kingside jogging.
  The jog base column
  should be
  one stop after the anchor column.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

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

* A 2-stop flat gap.

* Its head.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A queenside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A TISTIS,
  aligned at the anchor column.

## 2-jogging hoons

If the head and subhead of a 2-jogging hoon are on the
same line, the 2-jogging hoon is called **head-joined**.
If the head and subhead of a 2-jogging hoon are on
different lines, the 2-jogging hoon is called **head-split**.

2-jogging Hoon runes do not curry.

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

* A one-stop flat gap.

* Its head.

* A one-stop flat gap.

* Its subhead.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A kingside jogging.
  The jog base column
  should be
  one stop after the anchor column.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

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

A head-split kingside 2-jogging hoon should consist of,
in lexical order:

* Its rune.

* A one-stop flat gap.

* Its head.

* A vertical gap, with comments at the anchor column.

* Its subhead, aligned one stop before the head.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A kingside jogging.
  The jog base column
  should be
  one stop after the anchor column.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

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

* A 2-stop flat gap.

* Its head.

* A one-stop flat gap.

* Its subhead.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A queenside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

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

* A 2-stop flat gap.

* Its head.

* A vertical gap, with comments at the anchor column.

* Its subhead, aligned one stop before the head.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A queenside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A TISTIS,
  aligned at the anchor column.

## Jogging-1 hoons

<!-- "Split" form addressed in Github issue #31" -->

Jogging-1 hoons do not curry.

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

* A one-stop flat gap.

* A kingside jogging.
  The jog base column
  should be
  two stops after the anchor column.

* A vertical gap, with comments at
  the anchor column as well as the jog base column.

* A TISTIS,
  aligned one stop after the anchor column.

* A vertical gap, with comments at
  the anchor column.

* A tail, aligned at the anchor column.

## SIGCEN

SIGCEN does not curry.

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
can be regarded either a special form of a jogging hoon,
or as a 4-fixed hoon with, optionally,
an unusual 3rd runechild.

SIGCEN, except for its 3rd runechild, is treated
as an ordinary 4-ary basic syntax hoon.
SIGCEN's 3rd runechild may be either `~` or a jogging
bounded by TISTIS lines.
The jogging is always kingside and elements are never
split.

The initial TISTIS should be backdented appropriately
for a third runechild.
The final TISTIS should be aligned with the initial TISTIS.

The initial TISTIS should be followed by a vertical gap
with comments aligned with the TISTIS as well as
with the jogs.
The final TISTIS should be followed by a vertical gap
with comments aligned with the TISTIS as well as
with the jogs.

The jog head should be aligned one stop after the initial TISTIS.
The jog body should be tightly aligned,
or inter-line aligned with other bodies in the formula
list.
Successive formulas should be separated by a vertical
gap with comments aligned with the jogs.

## Battery hoons

The battery hoons are BARCAB, BARCEN and BARKET.

### Battery arm currying

An arm marker can be curried,
but it must be the anchor rune,
or must curry with BARCEN.
As a special case, an arm marker has a per-rune offset of
one stop.

### BARCEN

BARCEN may curry with KETBAR or KETWUT.
It may be joined or split.

#### Split BARCEN

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


#### Joined BARCEN

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

* A one stop flat gap.

* A battery whose base column is
  two stops after anchor column.

* A vertical gap,
  with comments at the anchor column.

* A HEPHEP, aligned at the anchor column.

### BARCAB

BARCAB runes do not curry.

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

* A one stop horizontal gap.

* Its head.

* A vertical gap,
  with comments at the anchor column,
  as well as one stop after the anchor column.

* A battery, whose base column location should be the anchor column.

* A vertical gap,
  with comments at the anchor column,
  as well as one stop after the anchor column.

* A HEPHEP, aligned at the anchor column.

### BARKET

BARKET runes do not curry.

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

* A one stop horizontal gap.

* Its head.

* A vertical gap,
  with comments at the anchor column,
  as well as one stop after the anchor column.

* A battery, whose base column location should be the anchor column.

* A vertical gap,
  with comments at the anchor column,
  as well as one stop after the anchor column.

* A HEPHEP, aligned at the anchor column.

## Ford hoons

### Ford-1 hoons

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

* A one-stop flat gap.

* The runechild.

### Ford hoof hoons

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

* A one-stop flat gap.

* The runechild.

* A vertical gap, with comments at the anchor column.

### 0-sequence Ford hoons

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

* A one-stop flat gap.

* A sequence of ford hoons.

* A vertical gap, with comments at the anchor column.

* A TISTIS.

### Ford hoon sequence

A Ford hoon sequence is a sequence of ford hoons.
All of the ford hoons should be at the same
alignment.

The hoons in the sequence should be
separated by vertical gaps.
The comments in the vertical gaps should be
aligned with the rune of the hoon which
directly contains the Ford hoon sequence.

### Ford-2 hoons

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
same as those for basic syntax 2-arity hoons.

### FASCOM

<!-- FASCOM is not well represented in the arvo
database, and a lot of these rules are extrapolations --
they are not actual examples in the corpus.
The remnants of the two-stop Ford convention in the
corpus make extrapolation from the examples next
to impossible.
I use indentation-conservation heavily as the guideline
for extrapolation.
-->

#### Joined FASCOM

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

* A one-stop flat gap.

* A sequence of FASCOM elements, all of which are
  aligned with the first one.

* A vertical gap, with comments at the anchor column.

* A TISTIS.

#### Kingside split FASCOM

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

#### Queenside split FASCOM

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

#### FASCOM element sequence

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
should be separated by vertical gaps,
with comments
at the anchor column
as well as aligned with the heads of the FASCOM elements.

A FASCOM element is considered joined or split,
depending on the gap between the head and the body.
If joined, the body of the FASCOM element
is separated from the head by a one-stop gap,
or an equivalent pseudo-join.

If the FASCOM element is split, the
body of the FASCOM element should be separated by
a vertical gap with comments at the body column.
The body of a kingside FASCOM element should be
aligned one stop **after** the head of the FASCOM element.
The body of a queenside FASCOM element should be
aligned one stop **before** the head of the FASCOM element.

### FASTIS

```
/=  rfctext  /:  /%/rfc  /txt/
```

A tall FASTIS hoon should consist of,
in lexical order:

* The FASTIS rune.

* A one-stop flat gap.

* A runechild.

# Appendix: Non-standard code

Recall that, by non-standard code, in this document
we mean code that does not follow
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

"Attached" slots are those with tight or backdented
alignment.
All other slots are "floating".
In non-standard code,
the column location of the inter-line alignment
of a silo of slots is
the column location most common in the "floating" slots.

If there are no floating slots,
the inter-line alignment column location
is irrelevant and undefined.
If two column locations tie in the
count of floating slots,
the tie is broken using the overall count of slots,
including both floating and attached slots,
with that column location.
If two column locations remain tied,
even after taking into account the overall count of slots,
then the tie is broken in favor of the column location that
occurs first, lexically.

If no floating slot has an overall slot count
of at least 2,
the inter-line alignment column location is undefined.
The idea is that the inter-line alignment must actually be
an alignment shared by two different lines.

Each silo is examined separately.
For each silo, only rows with an element in that silo's slot participate
in the calculation.

If a running could have both a runstep alignment,
and a running-inherited alignment,
the runstep alignment takes precedence.
In other words,
if a running has runstep alignment,
the running-inherited alignment is undefined.

# Appendix: Formal definition of "vertical gap body"

The format of a vertical gap body obeys this BNF:

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
This is traditional,
but has the counter-intuitive effect
that the highest priority is the lowest numbered one.
These definitions imply that

* No comment is read as a Priority 4 comment if it can be
  lexed as a Priority 1, 2 or 3 comment.

* No comment is read as a Priority 3 comment if it can be
  lexed as a Priority 1 or 2 comment.

* No comment is read as a Priority 2 comment if it can be
  lexed as a Priority 1 comment.

Comments with the same priority are treated
as if they were read simultaneously.
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

# Appendix: Futures

## Eliminate the details

The scheme for currying in this document was derived by
attempting to minimize
discrepancies with the 
`arvo/` corpus,
while being conservative.
While this allows practices
actually exemplified in the 
`arvo/` corpus,
it bans many practices which fit elegantly with the
general principles of these conventions.

It would produce a simpler and more elegant standard if
the rules in the "Details" section were eliminated,
except for those describing the Sail and Udon statements.

## "Free" currying

Another disadvantage of
attempting to minimize
discrepancies with the 
`arvo/` corpus,
is that it introduced
a large number of "special cases"
into the currying conventions.

It might make more sense to adopt a "free"
convention for when currying is allowed.
Here "free" is used in the mathematical sense,
to mean "free of arbitrary assumptions".

Here is a "free" definition of when currying occurs;

* The source and target runes of a curried text block
  should be Hoon statement runes on the same line.

* A tall running should not start or end inside a text block.

* A tall jogging should not start or end inside a text block.

* A battery should not start or end inside a text block.

* An arm of a battery should not start or end inside a text block.

* A curried text block should be as long as possible, consistent
  with the above rules.

This "free" definition is both more liberal and more restrictive
than these conventions given in the "Details" section.
The "Details" conventions followed the `arvo/` corpus in allowing battery
arms to be curried, while the free definition bans this practice.
On the other hand,
the "Details" conventions restricted currying to the sources and targets
actually exemplified in the `arvo/` corpus,
while this free definition allows source-target pairs for which
no example exists in the corpus.

## Post-comments

```
 :~
  :: inter-comment
    :: pre-comment
    42
    :: post-comment
  :: inter-comment
    :: pre-comment
    1729
    :: post-comment
  :: inter-comment
  ==
```

Currently vertical gaps allow pre-comments and inter-comments.
In the `arvo/` corpus,
post-comments also seem to occur.
Further, in sequences,
the vertical gap before a terminating boundary digraph
seems to include post-comments and inter-comments,
but not pre-comments.
Symmetrically,
the vertical gap preceding a sequence 
seems never to include post-comments.
