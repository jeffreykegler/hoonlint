# About this document

This document is intended to describe the use of whitespace by
Hoon to the level and degree of precision
necessary for `hoonfmt` and `hoonlint`.

# Terminology

## Deontology

In this document, we say "`X` should be `Y`",
if `X` must be `Y`, in order to meet the standard described
in this document.
Code, or practices which do not meet this standard we
will call **non-standard**.
One pragmatic consequence of non-standard code
is that,
by default, it may draw warnings from the hoon toolkit.

It is expected that, even in carefully written hoon code,
non-standard practices will occur.
For this reason, at points,
we will be careful to define
terms in such a way that they are meaningful even
when applied to non-standard code.

## Lines, columns and indentation

In this document, a **stop** is two spaces.
We say the text `Y` is indented `N` stops **after** text `X`,
if there are exactly `N` stops between the end of text `X`
and the beginning of text `Y`.

Let `Column(Y)` be column at which text `Y` begins.
We say the indentation of text `X` is `N` stops **greater than the indentation**
of text `Y` if text `X` begins at column `Column(Y)+N*2`.
We say the indentation of text `X` is `N` stops **less than the indentation**
of text `Y` if text `X` begins at column `Column(Y)-N*2`.

A **brick** is a Hoon lexeme with semantics.
A Hoon lexeme without semantics is a **mortar** lexeme.
All runes are bricks, but not all bricks are runes.

The **rune line** of a hoon is the line on which the hoon's rune occurs,
and the **rune column** of a hoon
is the column at which the hoon's rune begins.

Many alignments are with respect to an **anchor column**.
Usually, the anchor column is the rune column,
but there are many exceptions, and they
are important.
See below.

## Horizontal Alignment

All lexemes in hoon are aligned horizontally in one of 4 ways:

* **Tight**:  Tightly aligned lexemes follow the preceding
lexeme by exactly one stop.  A lexeme cannot be tightly aligned
if it is the first one on its line.

* **Backdented**:  Hoon's standard alignment, described
in detail below.

* **Inter-line**:  Inter-line alignment aligns the lexeme with
lexemes on associated lines.  Which lexeme is aligned with which,
and which lines are considered "associated" varies depending on
the syntactic context.  Inter-line alignment is described in detail
in the sections describing the syntaxes where it is allowed.

* **Free-form**:  Within SELACE hoons,
horizontal alignment can be free-form.

## Reanchoring

If the rune line contains other bricks,
it may **reanchor**, that is the anchor column
may be somewhere other than the rune column.
Not all runes participate in reanchoring.
Which do, and which do not, is described in the
individual cases.

We will first give a definition of reanchoring.
Because the definitions are difficult to follow without illustrations,
they are followed by a number of examples.

Informally, reanchoring is a method of conserving indentation.
Reanchoring moves the anchor column left
on the rune line, so that it becomes based at a parent brick earlier in the
rune line.
The column of the parent brick becomes the **anchor brick column**.
All parent bricks contribute to the backdenting, so that all the per-brick
offsets are added together to produce
an **reanchor offset**.

More formally, then,
the **anchor column** is the **anchor brick column**,
plus the **reanchor offset**.
The anchor brick column is the column of a brick on the rune line.
Which one will depend on the individual bricks involved, but often
it is the first brick on the rune line.

Each rune line has the rune itself and zero or more parent bricks.
Here parent means "proper parent", so that a rune is not considered
to be its own parent brick.

Depending on the number of elements required by the brick,
and the number of those elements joined on the rune line,
each brick on the rune line has a **per-brick anchor offset**.
If `n` elements of a parent brick are on the rune line,
the per-brick anchor offset is the indentation that would
apply to the `n`th element,
if the `n`th element were split onto the next line.
(Note that every proper parent brick must have at least one
element on the rune line.)

The total of all the per-brick anchor offsets of parent bricks
is the **reanchor offset** of the rune line.
The **anchor column** of the rune line is the anchor brick column,
plus the reanchor offset.

Note that when a rune is the only brick on a line,
the above definition of reanchoring is equivalent to the statement
that the rune column is the anchor column.

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
The reanchor base is the COLLUS (`:+`).
COLLUS is 3-fixed, but all three of its elements are
on the rune line, so the per-brick anchor offset is 0.
The anchor column is the anchor brick column plus the reanchor
offset, so that in this case,
the anchor column is the same as the anchor brick column.

COLSIG is 1-running, and normal COLSIG indentation indents the runsteps
one stop past the anchor column,
and the final TISTIS at the anchor column.
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
Here the rune line is line 346, the rune is again COLSIG,
and it reanchors at TISFAS (`=/`).
TISFAS is 3-fixed, and 2 of its elements are on the rune line,
so that the per-brick anchor offset of the TISFAS is that of
its 2nd element -- one stop.
The anchor column is therefore one stop after the anchor brick
column.

COLSIG again is 1-running, so that its runsteps should be indented
one stop past the anchor column,
which in this case means two stops past the anchor brick column.
By the same logic, the TISTIS should be indented at the anchor column,
whicb is one stop past the anchor brick column.
This is what we see in the example.
The last three lines of the example are the last element of the
TISFAS.

## Comments

Comments count as whitespace.
A comment is a **header comment** if it is on a line
by itself.
Header comments outside of a running or a jogging
should be immediately followed by a code line or
a line containing another header comment,
and should align with the code or header comment
which follows.
Header comments inside of a running or a jogging should
align at the anchor column.

If a comment is not a header
comment, it is a **rightside** comment.
A rightside comment is
a **margin comment** if it begins at or after column 57,
or immediately after a horizontal gap of 20 or more spaces.
All margin comments should start at column 57.
A rightside comment is an **inline comment** if it is not a margin comment.

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

A **vertical gap** is a gap containing which contains

* A newline-terminated partial line preamble.
This may be just the newline.

* A body of one or more full newline-terminated lines,
all of them comments or
(in the case of non-standard code) blank lines.

* A partial line postamble.
This is never newline-terminated,
and may be zero-length.

If it is before a step of a sequence,
a vertical gap may contain zero or more
**inter-comments**
followed by zero or more **pre-comments**.
Both inter-comments and pre-comments can contain any content,
but in concept,
inter-comments separate the sequence steps from other lexemes,
and from each other;
while pre-comments preceed sequence steps.

The inter-comment column and pre-comment column is specified,
for each type of sequence, below.

Informally, the body of a standard multi-line comment 
follows these conventions:

* A multi-line comment may contain
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

A more formal description of a multi-line comment body is
given in an appendix.

# Types of hoons

Hoons may be backdented, running, jogging, battery or irregular.

* A hoon is a **battery hoon** if it contains an element that
uses the battery syntax.

* A hoon is a **running hoon** if it contains an element that
uses the running syntax.

* A hoon is a **jogging hoon** if it contains an element that
uses the jogging syntax.

* A hoon is a **backdented hoon** if it contains a fixed number
of gap-separated elements, and none of them follow the running, jogging or
battery syntax.

* A hoon is an **irregular hoon** if it is not a backdented,
running, jogging or backdented hoon.
Most irregular hoons do not contain a gap.

TODO: Corner cases, including BARCAB,
SELACE and (possibly)
other tall irregular hoons.

Re BARCAB,
ohAitch asks,
"Is this not just a battery hoon + single "backdented" same-line child?"

# Backdented hoons

The archetypal Hoon whitespace pattern is backdenting.
A backdented hoon of 3 or more elements should be joined.
A backdented hoon of 2 or fewer elements should be split.

The first element of a joined backdented hoon should be
on the same line as the rune, separated by a gap.
Subsequent elements of a joined backdented hoon should be
separated by a vertical gap.

The first element of a joined backdented hoon should be
separated from the rune by a vertical gap.
Subsequent elements of a joined backdented hoon should also be
separated by a vertical gap.

The last element of a backdented hoon should be aligned at the
anchor column.
Every other elements of a backdented hoon should be aligned one
stop more than the element that follows it.

This means that, in an *n*-element hoon,
the first element should be indented `n-1` stops more than the anchor column;
a second element should be indented `n-2` stops more than the anchor column;
etc.

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

# Running hoons

A running element is more often called simply a **running**.
(Currently all hoons contains at most one running.)

In addition to the running, a running hoon may contain
an element before the running.
If there is one element before the running, it
is called the **head** of the running hoon.

There are current two kinds of regular runnings.
A 1-running has a head.
A 0-running has no head.

* The current 0-running rules are
BUCCEN (`$%`),
BUCCOL (`$:`),
BUCWUT (`$?`),
COLSIG (`:~`),
COLTAR (`:*`),
WUTBAR (`?|`),
and
WUTPAM (`?&`).

* The current 1-running rules are
CENCOL (`%:`),
DOTKET (`.^`),
SEMCOL (`;:`),
SEMSIG (`;~`).

* The current 0-as-1-running rule is
TISSIG (`=~`).

[ TODO: More on TISSIG. ]

## 0-running hoons

0-running hoons may be either split or joined.
Header comments in a 1-running hoon should be aligned at the anchor
column of the parent hoon.

The running of a **joined 0-running hoon** begins on the rune line.
The runsteps of a joined 0-running hoons should begin on the rune
line, indented two stops more than the anchor column.  This has the
effect of aligning all subsequent runesteps with the runstep on the
rune line.

The running of a **split 0-running hoon** begins after the rune line.
The runsteps of a split 0-running hoons should begin one newline equivalent
after the rune
line, indented one stop more than the anchor column.

For both joined and split 0-running hoons,
the TISTIS should align at the anchor column.

As a special case, the first line of the running of either a
joined or split 0-running hoon, may be a horizontal sub-running.
If a joined 0-running hoon has an initial horizontal sub-running,
the rune and the sub-running should be separated by one stop.

## 1-running hoons

The head of a 1-running hoon should occur on the rune line,
one stop after the anchor column.
The running should occur one vertical gap after the
head, and should be indented one stop after the anchor column.
Header comments in a 1-running hoon should be aligned at the anchor
column of the parent hoon.

As a special case, the running may start on the same line as the
head of the 1-running hoon.
In this case, the first line of the running may be
a horizontal sub-running.

## Runnings

A running is considered to start at the start of its first run step.
Every running ends in a TISTIS (`==`).
The TISTIS should occur on its own line, or as part of a criss-cross
TISTIS line.

A vertical gap should occur between every pair of runsteps.
A vertical gap should also occur between the last runstep
and the TISTIS.
Inter-comments in a running should be at the anchor column
of the parent hoon.
Pre-comments in a running should be aligned with the runstep.

A **horizontal sub-running** is a portion of a running which has
two or more runsteps on one line.
The runsteps in a horizontal subrunning
should be separated from each other by one stop.

From `sys/zuse.hoon`, lines 4892-4905.

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

From `sys/hoon.hoon', lines 5303-5308.

```
    :~  :-  ['a' 'z']  (cook |=(a/@ta [%$ %tas a]) sym)
        :-  ['0' '9']  (stag %$ bisk)
        :-  '-'        (stag %$ tash)
        :-  '.'        ;~(pfix dot perd)
        :-  '~'        ;~(pfix sig ;~(pose twid (easy [%$ %n 0])))
    ==
```

# Jogging hoons

A jogging element is more often called simply a **jogging**.
(Currently all hoons contains at most one jogging.)

In addition to the jogging, a jogging hoon may contain
other elements, either before or after the jogging.
An element after the jogging is called a **tail**.
If there is one element before the jogging, it
is called the **head** of the jogging hoon.
If there are two elements before the jogging, they
are called, in order, the **head** and **subhead** of
the jogging.

There are current four kinds of jogging.
A 1-jogging has one head and no tail.
A 2-jogging has a head and a subhead and no tail.
A jogging-1 has a tail and no head.

* The current 1-jogging rules are CENTIS (`%=`), CENCAB (`%_`) and WUTHEP (`?-`).

* The current 2-jogging rules are CENTAR (`%*`) and WUTLUS (`?+`).

* The current jogging-1 rule is TISCOL (`=:`).

## Jogs

A jogging is a gap-separated sequence of one or more jogs.
Every **jog** contains a **jog head**, followed by a gap and a **jog body**.
Note that it is important to distinguish between the head of a jogging
hoon, defined above, and the head of a jog.

A jog is **joined** if its head and its body are both on the same line.
Otherwise, the jog is said to be **split**.

## Chess-sidedness

Jogs, joggings and jogging hoons have **chess-sidedness**.
Chess-sidedness is always either kingside and queenside.

Informally, **kingside** means the indentation has a left-side bias;
and **queenside** means that the indentation has a right-side bias.
Indentation will be described more precisely in what follows.

## Jogs

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
A jog body which is a aligned at the jog body column
is said to be **jogging-body-aligned**.
A jog whose jog body is jogging-body-aligned
is also said to be **jogging-body-aligned**.

Jogs are either kingside or queenside.
A kingside jog should have an indentation 1 stop greater than
the base column of its jogging hoon.
A queenside jog should have an indentation 2 stops greater than
the base column of its jogging hoon.
The base column of a jogging hoon is described below,
in the description for the different kinds of jogging hoon.

A multi-line kingside jog may either be pseudo-joined or split.
A multi-line kingside jog must be a split jog.
A jog is **pseudo-joined**

* if and only every line of it,
except the body line,
has a comment at the column where the properly aligned body of
a ragged jog would start; or

* if and only every line of it,
except the body line,
has a comment at the column where the properly aligned body of
a jogging-body-aligned jog would start.

Note that this implies that the line containing the head of the
jog must contain a comment.
Pseudo-joined jogs are so called because the comment on the
line of the head of the jog is a kind of "place holder" for
the join,
and the comments can be seen as "postponing" the join.

The gap of split jog should be vertical,
with the gap's comments aligned with the jog body.
The indentation of the body of a split kingside jog
should be 1 stop **greater** than the indentation of the jog's head.
The indentation of the body of a split queenside jog
should be 1 stop **less** than the indentation of the jog's head.

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
lines connecting the TISTIS's that match each rune by alignment
with the TISTIS's that match the same rune syntactically,
the lines would form a "criss-cross" pattern.

HEPHEP's may also be joined into criss-cross lines.

## 1-jogging hoons

Every 1-jogging hoon is either kingside or queenside.
The base column
for the jogs of a 1-jogging hoon is its anchor column.

"Sidedness" should be consistent:

* The head of a kingside 1-jogging hoon should be kingside.
It should be on the rune line,
indented 1 stop after the anchor column.

* The jogging of a kingside 1-jogging hoon should be kingside.
It should be on a line after the rune line,
and should consist entirely of kingside jogs.

* The head of a queenside 1-jogging hoon should be queenside.
It should be on the rune line,
indented 2 stops after the anchor column.

* The jogging of a queenside 1-jogging hoon should be queenside.
It should be on a line after the rune line,
and should consist entirely of queenside jogs.

## 2-jogging hoons

Every 2-jogging hoon is either kingside or queenside.
The base column
for the jogs of a 2-jogging hoon is its anchor column.

If the head and subhead of a 2-jogging hoon are on the
same line, the 2-jogging hoon is called **head-joined**.
If the head and subhead of a 2-jogging hoon are on
different lines, the 2-jogging hoon is called **head-split**.

"Sidedness" should be consistent:

* The head of a kingside 2-jogging head-joined hoon should be kingside.
It should be on the rune line,
indented 1 stop after the anchor column.

* The head of a queenside 2-jogging head-joined hoon should be queenside.
It should be on the rune line,
indented 2 stops after the anchor column.

* The head of a head-split 2-jogging has no sidedness.
It should be on the rune line,
indented 1 stop after the anchor column.

* The subhead of a head-joined 2-jogging hoon
is on the rune line, and
should be indented one stop after the head.

* The subhead of a head-split 2-jogging hoon
should be one vertical gap after the
the rune line, and
should be indented one stop less than the head.
This style is more indentation-conserving than backdenting.
It is called the "pseudo-jog" style, because arrangement
of the head
and subhead resembles that of a queenside jog.

* The jogging of a kingside 2-jogging hoon should be kingside.
It should be on a line after the rune line,
and should consist entirely of kingside jogs.

* The jogging of a queenside 2-jogging hoon should be queenside.
It should be on a line after the rune line,
and should consist entirely of queenside jogs.

## Jogging-1 hoons

Every jogging-1 hoon is considered kingside.
The base column
for the jogs of a jogging-1 hoon is one stop greater than its anchor column.

* The jogging of a jogging-1 hoon
should start on the rune line,
1 stop after the rune line,
and should consist entirely of kingside jogs.
This implies that the TISTIS should be indented one stop
more than the anchor column.

* The tail of a jogging-1 hoon should be
should be one new-line equivalent after the TISTIS,
and aligned at the anchor column.

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

With one exception,
the **anchor column** of a BARCEN battery is the column
of the first rune on the same line as the rune of the hoon
of the battery that contains those cells.
The exception is that
the rune column of a parent TISGAL or TISGAR rune
should never be the anchor column of a BARCEN battery.

Note that BARCEN Cells
also should never use the column of a parent LUSLUS, LUSHEP or LUSTIS
as their anchor column.
Technically, this is not an exception because pedantically,
LUSLUS, LUSHEP and LUSTIS are not runes.

# SELACE

Only one-line SELACE's occur in the `arvo/` corpus.
One-line SELACE's are free-form -- `hoonlint` never generates
a warning for them.
If `hoonlint` encounters a multi-line SELACE,
it issues a "not yet implemented" warning.

# Special cases

## TISSIG

As a special case,
the runsteps in TISSIG should be aligned with the rune.

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

# Appendix: Multi-line comment body

The format of a multi-line comment body obeys the BNF

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
