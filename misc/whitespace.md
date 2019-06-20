# About this document

This document is intended to describe the use of whitespace by
Hoon to the level and to the degree of precision
necessary for `hoonfmt` and `hoonlint`.

This document therefore only deals with conventions
for Hoon expressions which contain gaps.
These are

* Tall Hoon expressions; and

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
if X is Y as matter of definition,
or due to a mathematical equivalence.

## Lines, columns and indentation

This document applies to Hoon source files.
A Hoon source file is organized in newline-terminated lines
each of which contains 1 or more characters.
The 1-based offset of a character in its line is its **column location**.
The 1-based line number of a character is its **line location**.
Line location is often abbreviated to **line**,
and column location is often abbreviated to **column**.

To avoid confusion,
in this document
column position is always used to mean the
location of a character within a line,
as defined above.
When we discuss other horizontal vs. vertical grids,
we use the terms "row" for position on the vertical axis,
and "silo" for position on the horizontal axis.
The definition of the specific meaning of
"row" and "silo" will be given with the context
where it is used.

The column location at which `x` starts
will sometimes be written as `Column(x)`,
where `x` designates some text in a Hoon source file.
The line location at which `x` starts
will sometimes be written as `Line(x)`.
When we say "the column location of `x`",
we mean `Column(x)`.

A **stop** is two spaces.
We say the text `Y` is indented `N` stops **after** text `X`,
if there are exactly `N` stops between the end of text `X`
and the beginning of text `Y`.

If the column location of text `X` is `N` stops more
than the column location of text `Y`,
then text `X` begins at column `Column(Y)+N*2`.
If the column location of text `X` is `N` stops less
than the column location of text `Y`,
then text `X` begins at column `Column(Y)-N*2`.

A **brick** is a subtree of the Hoon syntax tree with semantics.
A subtree without semantics is a **mortar** subtree.
All rune expressions are bricks, but not all bricks are rune expressions.
Mortar subtrees will not play a big role in this document --
they are usually invisible at the source file level.

Many alignments are with respect to an **anchor column**.
Often, the anchor column is the rune column,
but there are important exceptions, which
will be described below.

## Hoon expressions

A tall **rune-ish expression** is a tall hoon expression whose "keyword"
is a rune-ish.
A `rune-ish` is usually a rune,
but the `rune-ishes` also include some other keyword digraphs,
such as `++`, `+=`, `+-`, `--` and `==`.
Currently rune-ishes are always represented in Hoon source
files as digraphs.

Digraphs which introduce comments,
such as `::` are **not** considered rune-ishes.
Comments, including the digraphs which begin them
are considered whitespace,
and nothing in whitespace is considered a rune-ish.

Let `r` be the rune-ish digraph of a rune-ish expression.
The **rune line** of a hoon is `Line(r)`.
The **rune column** of a hoon is `Column(r)`.

A rune-ish expression consists of the initial rune-ish
followed by zero or more subexpressions,
called **runechildren**.
The **arity** of a rune-ish expression is the number of
its runechildren.
In the hoon expressions being considered in this document,
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

## Horizontal Alignment

All non-whitespace lexemes in Hoon are aligned horizontally in one of 4 ways:

* **Tight**:  Tightly aligned non-whitespace lexemes follow the preceding
non-whitespace lexeme by exactly one stop.  A non-whitespace lexeme cannot be tightly aligned
if it is the first non-whitespace lexeme on its line.

* **Backdented**:  Hoon's standard alignment, described
in detail below.

* **Inter-line**:  Inter-line alignment aligns the non-whitespace lexeme with
non-whitespace lexemes on associated lines.  Which non-whitespace lexeme is aligned with which,
and which lines are considered "associated" varies depending on
the syntactic context.  Inter-line alignment is described in detail
in the sections describing the syntaxes where it is allowed.

* **Free-form**:  Within SELGAP hoons,
horizontal alignment in standard code can be free-form.

## Reanchoring

Reanchoring is a method of conserving indentation.
Typically, a rune-ish is its own
"anchor" rune-ish for indentation purposes,
but Hoon takes advantage of some opportunities
to move the anchor column closer to the left margin.
Not all rune-ishes participate in reanchoring.
Which do, and which do not, is described in the
individual cases.

We now present a more formal
definition of reanchoring,
which will be followed by a number of examples.
Let `r` be a rune-ish.
If `Line(r)` contains other rune-ishes,
`r` may **reanchor**,
that is,
the anchor column of `r`
may be somewhere to the left of `Column(r)`.

For every rune-ish, there is always an anchor
rune-ish,
although usually this is trivially true --
a rune-ish is its own anchor rune-ish.
Let `a` be an anchor rune-ish.
The anchor column is the column of an
anchor rune-ish,
plus the **reanchor-offset**.

Again, let `r` 
Let `S` be a sequence of rune-ishes such that

* `S` is empty if a rune-ish is its own anchor, that is `S` is
empty if `a == r`.

* `S[0] = a`

* Where `S[n]` is the last element of `S`,
`S[n] = (Rune(Parent(Hoon(r)))`.

* For all `i` such that `0 <= i < n`,
S[i] = (Rune(Parent(Hoon(S[i+1])))`

Note that, by the above definition, `r` is never an
element of `S`.
Informally, if `S` is not empty, it includes `a`
and all the proper syntactic parents
of `r` which are descendants of `a`.

Intuitively, the per-rune-ish offsets are the
indentation that is "left over" when its rune-ish
child begins.
Note that, because of the way that we constructed `S`,
the `c`'th runechild of `S[i]` must be a
rune-ish.
More formally,
let `S[i]` be a rune-ish in S,
and let `c` be the number of runechildren
of `S[i]` that are on `Line(S[i])`.
The
per-rune-ish offset is the indentation from the anchor
column that
would apply to the `c`'th runechild
if it were on `Line(S[i]) + 1`.

The **reanchor offset** of `r` is the sum of all the
per-rune-ish offsets in `S`.
Note that in the trivial case,
where a rune-ish is its own anchor,
`S` is always empty; the reanchor offset is always zero;
and the anchor column is always the same
as the rune column.

### First example


```
        :+  %depends  %|  :~
          definitions+[%& deh]
          listeners+[%& sup]
          waiting+[%& out]
        ==
```

This is lines 1916-1920 of
`arvo/sys/vane/ford.hoon`.
The rune line is line 1916, and the rune is
COLSIG (`:~`).
The anchor rune-ish is the COLLUS (`:+`).
COLLUS is 3-fixed, but all three of its runechildren are
on the rune line, so the per-rune-ish offset is 0.
The anchor column is the anchor rune-ish column plus the reanchor
offset, so that in this case,
the anchor column is the same as the anchor rune-ish column.

COLSIG is 0-running, and normal COLSIG indentation indents the runsteps
one stop past the anchor column,
and puts the final TISTIS at the anchor column.
This is exactly what we see.

### Second example

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

This is 
lines 346-356 of
`arvo/lib/hood/kiln.hoon`:
Here the rune line is line 346, the rune-ish is again COLSIG,
and it reanchors at TISFAS (`=/`).
TISFAS is 3-fixed, and 2 of its runechildren are on the rune line,
so that the per-rune-ish offset of the TISFAS is that of
its 2nd runechild -- one stop.
The anchor column is therefore one stop after the anchor rune-ish
column.

COLSIG again is 0-running, so that its runsteps should be indented
one stop past the anchor column.
By the same logic, the TISTIS should be indented at the anchor column.
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
or immediately after a horizontal gap of 20 or more spaces.
All margin comments should start at column 57.
A rightside comment is an **inline comment** if it is not a margin comment.

In standard code, header comments are

* Pre-comments or inter-comments,
as determined by their hoon.

* Meta-comments.

We say a hoon is the hoon of a comment
if that hoon directly contains the gap
that that comment is part of.
A comment is "directly contained" by a hoon if the hoon
contains the comment,
but no proper subexpression of the hoon also
contains that comment.
This implies that
a vertical gap, and its comments,
are "contained" by a rune-ish expression if that vertical gap separates
the rune-ish from the first runechild;
or if that vertical gap separates
a consecutive pair of the rune-ish expression's runechildren.

Meta-comments start at column 1.
Since, depending on their hoon,
inter-comments may also start at column 1,
only the programmer's intent determines whether
a given header comment is an inter-comment or a meta-comment.
An inter-comment is structural, so that its content should
fit the syntactic structure of the code in which it occurs.
Meta-comments are not structural comments, and their content
may be anything.
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
A more formal definition of a staircase is
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

A vertical gap may contain zero or more
**inter-comments**
followed by zero or more **pre-comments**.
Both inter-comments and pre-comments can contain any content,
but in concept,
inter-comments separate the sequence steps from other lexemes,
and from each other;
while pre-comments preceed sequence steps.

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
An **structural comment** is any good comment which is
not a meta-comment.

A more formal description of a vertical gap body is
given in an appendix.

# Types of hoons

Every hoon falls into one of 5 disjoint classes:
backdented, running, jogging, battery or irregular.

* A hoon is a **battery hoon** if it contains an runechild that
uses the battery syntax.

* A hoon is a **running hoon** if it contains an runechild that
uses the running syntax.

* A hoon is a **jogging hoon** if it contains an runechild that
uses the jogging syntax.

* A hoon is a **backdented hoon** if it contains a fixed number
of gap-separated runechildren, and none of them follow the running, jogging or
battery syntax.

* A hoon is an **irregular hoon** if it is not a backdented,
running, jogging or backdented hoon.
Most irregular hoons do not contain a gap.

# Fixed arity hoons

The flagship Hoon whitespace strategy is backdenting.
Variations on the idea of backdenting appear throughout,
but the archetypal case of backdenting is its use in
fixed arity hoons.
Here is a example of a backdented 4-ary hoon:

```
        :^    %wtcl
            [%dtts [%rock %$ |] [%$ axe]]
          [%rock %f |]
        [%rock %f &]

```

A backdented hoon of arity 3 or more should be joined.
A backdented hoon of arity 2 or less should be split.

The first runechild of a joined backdented hoon should be
on the same line as the rune, separated by a gap.
Subsequent runechildren of a joined backdented hoon should be
separated by a vertical gap.

The first runechild of a joined backdented hoon should be
separated from the rune by a vertical gap.
Subsequent runechildren of a joined backdented hoon should also be
separated by a vertical gap.

In an fixed `n`-arity hoon,
the `m`'th runechild should be indented `n-m` stops more than the anchor column.
For example,
in an 3-runechild hoon,
the first runechild should be indented 2 stops more than the anchor column;
a second runechild should be indented 1 stop more than the anchor column;
and the third runechild be indented at the anchor column.

This implies that, regardless of the number of runechildren in a
backdented hoon,

* the last runechild should be aligned at the anchor column;

* every runechild before the last should have a column location one
stop greater
than the column location of the runechild that follows it.

* every runechild after the first should have a column location
one stop less than the runechild that precedes it.

In the vertical gaps belonging to backdented hoons,
the inter-comment column location should be the column
location of the first text after the gap;
and the pre-comment column location should be undefined.

## Chaining fixed arity hoons

As a special case, a runechild of an backdented hoon may
have an inter-line alignment, based on its **silo** and **chain**.
Here is an example of chained alignment
from `arvo/sys/zuse.hoon`, lines 2950-2959:

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

Intuitively,
if two runchildren are in the same silo of the same chain,
they should have the
same inter-line alignment.

More formally,
a chain is a sequence of fixed-arity hoons
which obeys the following rules:

* Every hoon except the first is the last
runchild of the previous hoon in the sequence

* Every tall rune-ish is either

    * a "row-initial" tall rune-ish,
      which should be at the same column location
      as the first hoon in the sequence, or;

    * a "joined" tall rune-ish, that is, another tall
       rune-ish on the same line as
       the initial rune-ish.

* The sequence is maximal.  That is,
no chain is a sub-sequence of a longer chain.

For the purposes of chain inter-line alignment,
a row starts with a row-initial rune-ish.
The silo elements for a row are its tall rune-ishes and their runechildren,
in lexical order,
recursively.
A runechild which itself is a tall rune-ish expressions is never
a silo elements -- instead it is broken out into
its rune-ish and runechildren,
and these are become silo elements in that row.
Each silo element goes into one silo,
so that the first silo element goes into silo 0,
the second goes into silo 1, etc.

A row may contain multiple lines,
but rune-ishes and
runechildren not on the first line of the row
are not used in determining inter-line alignment,
and therefore are not added as silo elements.

This implies that

* Every row-initial rune-ish is in silo 0,

* Every tall rune-ish is a separate silo element.

* Every runechildren of a tall rune-ish is a separate
  silo element, unless it is itself a tall rune-ish.

* For chained inter-line alignment,
  The row and silo grid may be "ragged", so that some
  rows do not have elements in every silo.

In the following example, there are 4 rows each of 3 silos.
Every row in a chain starts with a rune-ish, but not every
rune-ish need start a row,
and a given silo may contain both rune-ishes and runechildren.
For example, here is code
from `arvo/sys/hoon.hoon`, lines 1572-1575:

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
If there is one runechild before the running, it
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

* A **1-running** has a head.
The current 1-running rules are
CENCOL (`%:`),
DOTKET (`.^`),
SEMCOL (`;:`),
SEMSIG (`;~`).

* TISSIG (`=~`) is a special case.

## 0-running hoons

0-running hoons may be either split or joined.

### Joined 0-running hoons

This example of a joined 0-running hoon
is from `arvo/sys/zuse.hoon`, lines 2399-2402:

```
              :~  [b er]
                  [b pk]
                  [(met 0 m) m]
              ==
```

The running of a joined 0-running hoon should be tightly aligned
on the rune line.
All subsequent runstep lines should be at the same column
location as the first runstep line.

### Split 0-running hoons

This example is the beginning and end of a long split 0-running hoon,
which starts at line 84 of `avro/sur/twitter.hoon`.

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

In a split 0-running hoon,
the column location of the running and of the runstep lines should be one stop
more than the anchor column.
This implies that all runstep lines should be at the same column location.

In the vertical gap between the rune and the running of a split 0-running hoon,
the inter-comment column location should be the same as the rune column.
The pre-comment location should be the column location of the running.

## 1-running hoons

1-running hoons can be joined or split.

### Joined 1-running hoons

Lines 4853-4855 of `arvo/sys/hoon.hoon`:
```
             ;~  less  soz
               (ifix [soq soq] (boss 256 (more gon qit)))
             ==
```

The head of a joined 1-running hoon should occur on the rune line,
tightly aligned.
The running (and therefore the first runstep line)
of a joined 1-running hood should be tightly aligned.
Subsequent runstep lines should be one stop after the anchor column.

### Split 1-running hoons

Lines 4048-4051 of `arvo/sys/zuse.hoon`:
```
      ;~  pose
        (cold & (jest 'true'))
        (cold | (jest 'false'))
      ==
```

In a split 1-running hoon,
there should be a vertical gap
between the head and the running.
In the vertical gap,
the inter-comment column is the anchor column,
and the pre-column is the column location of the running.

The column location running of a split 1-running hoon
should occur one vertical gap after the
head, and its column location
should be one stop more than the anchor column.

## TISSIG

From `arvo/sys/vane/ford.hoon`, beginning at line 8:
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

In TISSIG, the running must be separated from the rune by a vertical gap.
The inter-comment location of this vertical gap is the anchor
column, and its pre-comment location is undefined.

The column location of the running must be the anchor column.
The column location of the runstep lines in TISSIG should be
the anchor column.

The TISTIS should be separated from the last runstep by a vertical gap,
whose inter-comment location is the anchor
column.
Since the runstep column location and the anchor column are the same,
the vertical gap before the TISTIS
is a special case.
The pre-comment column location of the vertical gap before the
TISTIS should be undefined.

<!-- TODO: TISSIG is very problematic.
By the above definition of correctness, none
of the TISTIS occurrences in the corpus are correct.
Also, contrary to the above
 there may to be a joined form of TISSIG.
-->

## Runnings

A running is considered to start at the start of its first run step.
A running contains one or more **runstep lines**.
The column location of the runstep lines should be as described
for the parent running hoon.

Every running ends in a TISTIS (`==`).
The TISTIS should occur on its own line, or as part of a criss-cross
TISTIS line.
The column location of the TISTIS should be the anchor column of
the hoon that contains it.

A vertical gap should occur between every line of runsteps.
A vertical gap should also occur between the last runstep line
and the TISTIS.
Inter-comments in a running should be at the anchor column
of the parent hoon.
Pre-comments in a running should be aligned with the following
runstep.

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
Here is an example of runstep inter-line alignment
from `sys/zuse.hoon`, lines 4892-4905:

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

### Running-inherited inter-line alignment

If a runstep is a hoon, the runechildren of a fixed-arity hoon
may have a **running-inherited** inter-line alignment.
[ Describe row-silo ]
Within a running,
the running-inherited inter-line alignment of a runechild
is determined by the silo of the runechild.

From `sys/hoon.hoon', lines 5303-5308.

```
    :~  :-  ['a' 'z']  (cook |=(a/@ta [%$ %tas a]) sym)
        :-  ['0' '9']  (stag %$ bisk)
        :-  '-'        (stag %$ tash)
        :-  '.'        ;~(pfix dot perd)
        :-  '~'        ;~(pfix sig ;~(pose twid (easy [%$ %n 0])))
    ==
```

Note that if a fixed-arity hoon is a runestep,
that it may have both a running-inherited inter-line alignment
and a chained inter-line alignment.
If a fixed-arity hoon does have both inter-line alignments,
for every runechild silo,
their column locations should be identical.

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

There are current four kinds of jogging.
A 1-jogging has one head and no tail.
A 2-jogging has a head and a subhead and no tail.
A jogging-1 has a tail and no head.

* The current 1-jogging rules are CENTIS (`%=`), CENCAB (`%_`) and WUTHEP (`?-`).

* The current 2-jogging rules are CENTAR (`%*`) and WUTLUS (`?+`).

* The current jogging-1 rule is TISCOL (`=:`).

A jogging is a gap-separated sequence of one or more jogs.
Every **jog** contains a **jog head**, followed by a gap and a **jog body**.
Note that it is important to distinguish between the head of a jogging
hoon, defined above, and the head of a jog.

## Chess-sidedness

Jogs, joggings and jogging hoons have **chess-sidedness**.
Chess-sidedness is always either kingside and queenside.

Informally, **kingside** means the indentation has a left-side bias;
and **queenside** means that the indentation has a right-side bias.
Indentation will be described more precisely in what follows.

## Jogs

A jog is **joined** if its head and its body are both on the same line.
Otherwise, the jog is said to be **split**.

The indentation of a jog is that of its head.
A jog is **joined** if its head is on the same
line as its body.
Otherwise, a jog is **multiline**.
As explained below,
a multiline jog may be either **pseudo-joined**
or **split**.

A joined jog may be either **aligned** or **ragged**.
A joined jog is ragged if its body is indented 1 stop after
its head.
Otherwise, a joined jog is considered aligned.

All aligned jogs in a jogging should be indented to the
same column.
This column is called the **jogging body column** of the jogging.
A jog body which is aligned at the jog body column
is said to be **jogging-body-aligned**.
A jog whose jog body is jogging-body-aligned
is also said to be **jogging-body-aligned**.

Jogs are either kingside or queenside.
The column location
of a kingside jog should be 1 stop greater than
the base column location of its jogging hoon.
The column location
of a queenside jog should be 2 stops greater than
the base column location of its jogging hoon.
The base column location of a jogging hoon was described above,
in the description for the different kinds of jogging hoon.

A multi-line kingside jog may either be pseudo-joined or split.
A jog is **pseudo-joined**

* if and only every line of it,
except the body line,
has a comment at the column location where the properly aligned body of
a ragged jog would start; or

* if and only every line of it,
except the body line,
has a comment at the column location where the properly aligned body of
a jogging-body-aligned jog would start.

Note that this implies that the line containing the head of the
jog must contain a comment.
Pseudo-joined jogs are so called because the comment on the
line of the head of the jog is a kind of "place holder" for
the join,
and the comments can be seen as "postponing" the join.

The gap of split jog should be vertical.
The inter-comment column location of the gap
should be the column location of the jog body;
and there should be no pre-comment column location.
The column location of the body of a split kingside jog
should be 1 stop **greater** than the column location of the jog's head.
The column location of the body of a split queenside jog
should be 1 stop **less** than the column location of the jog's head.

## Joggings

A jogging is considered to start at the start of jog head
of its first jog.
Every jogging ends in a TISTIS (`==`).
The TISTIS should occur on its own line, or a part of a criss-cross
TISTIS line.

The TISTIS of a kingside jogging should be outdented one stop
from the start of its jogging.
The TISTIS of a queenside jogging should be outdented two stops
from the start of its jogging.

## Criss-cross lines

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
The base column
for the jogs of a 1-jogging hoon is its anchor column.
Specifics are given below,
but the general rule for sidedness is that it should be
consistend.

### Kingside 1-jogging hoons

From `arvo/sys/hoon.hoon`, lines 6330-6334:
```
  ?-  pex
    {$1 $0}  yom
    {$1 $1}  woq
    *        [%6 pex yom woq]
  ==
```

* The head of a kingside 1-jogging hoon should be kingside.
It should be on the rune line,
indented 1 stop after the anchor column.

* The jogging of a kingside 1-jogging hoon should be kingside.
It should be on a line after the rune line,
and should consist entirely of kingside jogs.

### Queenside 1-jogging hoons

From `arvo/sys/hoon.hoon`, lines 6305-6309:
```
  ?-    nug
      {$0 *}   p.nug
      {$10 *}  $(nug q.nug)
      *        ~_(leaf+"cove" !!)
  ==
```


* The head of a queenside 1-jogging hoon should be queenside.
It should be on the rune line,
indented 2 stops after the anchor column.

* The jogging of a queenside 1-jogging hoon should be queenside.
It should be on a line after the rune line,
and should consist entirely of queenside jogs.

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

From `arvo/sys/hoon.hoon`, lines 6583-6586:
```
      ?+  p.mod  [%rock %$ 0]
        $cell  [[%rock %$ 0] [%rock %$ 0]]
        $void  [%zpzp ~]
      ==
```

* The head of a kingside 2-jogging head-joined hoon should be kingside.
It should be on the rune line, tightly aligned.

* The subhead of a head-joined 2-jogging hoon
should be on the rune line, tightly aligned.

* The jogging of a kingside 2-jogging hoon should be kingside.
It should be on a line after the rune line,
and should consist entirely of kingside jogs.

### Head-joined queenside 2-jogging hoons

From `arvo/sys/vane/ames.hoon`, lines 1568-1575:
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

* The head of a queenside 2-jogging head-joined hoon should be queenside.
It should be on the rune line,
separated by 2 stops from the rune.

* The subhead of a head-joined 2-jogging hoon
should be on the rune line, tightly aligned.

### Head-split queenside 2-jogging hoons

From `arvo/sys/hoon.hoon`, lines 10111-10116:
```
      ?+    (rash p.q.ham ;~(sfix (cook crip (star low)) (star hig)))
          ~(rend co [%$ p.q.ham lum])
        $$    ~(rend co [%$ %ud lum])
        $t    (dash (rip 3 lum) '\'' ~)
        $tas  ['%' ?.(=(0 lum) (rip 3 lum) ['$' ~])]
      ==
```


* The head of a head-split 2-jogging has no sidedness.
It should be on the rune line, separated by two stops
from the rune.

* The subhead of a head-split 2-jogging hoon
should be one vertical gap after the
the rune line, and
should be indented one stop less than the head.
This style is more indentation-conserving than backdenting.
It is called the "pseudo-jog" style, because arrangement
of the head
and subhead resembles that of a queenside jog.

* The jogging of a queenside 2-jogging hoon should be queenside.
It should be on a line after the rune line,
and should consist entirely of queenside jogs.

## Jogging-1 hoons

<!-- "Split" form addressed in Github issue #31" -->

From `arvo/sys/hoon.hoon`, lines 9720-9727:
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
The base column location
for the jogs of a jogging-1 hoon is one stop greater than its anchor column.

* The jogging of a jogging-1 hoon
should start on the rune line,
and be tightly joined.
The jogging should consist entirely of kingside jogs.
This implies that the TISTIS should be indented one stop
more than the anchor column.

* For the vertical gap before the TISTIS,
the inter-comment column location should be the anchor
column, and the pre-comment column location should be undefined.

* The tail of a jogging-1 hoon should be
should be one vertical gap after the TISTIS,
and its column location should be the anchor column.

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

## BARCEN

BARCEN may reanchor at KETBAR or KETWUT.
It may be joined or split.

### Split BARCEN

From the sieve_k example:
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

* A split BARCEN has
a vertical gap between the rune and the battery.
In this vertical gap, the inter-comment column location
should be the anchor column,
and the pre-comment column location should be undefined.

### Joined BARCEN

From `arvo/sys/zuse.hoon`, lines 176-181:
```
      |%  ++  seal  |~({a/pass b/@ c/@} *@)             ::  encrypt to a
          ++  sign  |~({a/@ b/@} *@)                    ::  certify as us
          ++  sure  |~({a/@ b/@} *(unit @))             ::  authenticate from us
          ++  tear  |~  {a/pass b/@}                    ::  accept from a
                    *(unit {p/@ q/@})                   ::
      --  ::as                                          ::
```

* The battery of a joined BARCEN must be tightly
aligned.

## BARCAB

From `sys/vane/jael.hoon`, lines 697-827
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

* The head of BARCAB may be tightly joined or pseudo-joined.

* There should be a vertical gap between the head and the battery.
The inter-comment column location of the vertical gap should
be the anchor column and the pre-column should be undefined.

* The battery column location should be the anchor column.

## BARKET

From `arvo/sys/zuse.hoon`, lines 3975-4025
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

* The head of BARKET should be tightly joined or pseudo-joined.

* There should be a vertical gap between the head and the battery.
The inter-comment column location of the vertical gap should
be the anchor column and the pre-column should be undefined.

* The battery column location should be the anchor column.

# SELGAP

From `arvo/sur/twitter.hoon`, line 85:
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

A jog is considered queenside if its indentation is 2 stops or more
greater than the anchor column.
Otherwise, the jog is considered kingside.

The chess-sidedness of a misindented jogging is that of the majority
of its jogs.
In case of a tie, the jogging is considered to be queenside.
The chess-sidedness of a misindented jogging hoon is that of its
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

* BadComment -- a comment with any indent. Priority 3.

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

# Appendix: Non-standard chains

In determining the "intended" inter-line alignment of chained hoons,
and of runsteps,
each silo is examined separately.
For each silo, only rows with an element in that silo participate.

In each silos,
some element are "nominators" and others are "tie-breakers".
Nominator elements "nominate" their column location --
an inter-line alignment must have been "nominated".

For runsteps, the the silo elements which are tightly aligned
are tie-breakers.
All others are nominators.

For chains, the silo elements which are tightly or backdented
aligned are tie-breakers.
All others are nominators.

The following steps are followed:

* The inter-line alignment are narrowed down to those which are most common by count of the nominators.

* If the previous step produces a tie,
it is broken by selecting the elements which is most common by count of all elements in
that silo, including both nominators and tie-breakers.

* If this still produces a tie,
it is broken by using the column location of the element which is first lexically.
This will result in a unique column location.

* The resulting column location must be the column location of at least 2 elements
in its silo.
If so, it is the result of this procedure.
Otherwise, the inter-line column location is considered to
be undefined.

The purpose of the last step is to ensure that
an inter-line column location does actually involve two
elements on two different rows -- in other words,
that it is really an alignment.
* A colum


