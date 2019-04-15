# Hoon whitespace standards

## About this document

This document is intended to describe the use of whitespace by
Hoon to the level and degree of precision
necessary for `hoonfmt` and `hoonlint`.

## Terminology

### Deontology

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

### Lines, columns and indentation

In this document, a **stop** is two spaces.
We say the text `Y` is indented `N` stops **after** text `X`,
if there are exactly `N` stops between the end of text `X`
and the beginning of text `Y`.

Let `Column(Y)` be column at which text `Y` begins.
We say the indentation of text `X` is `N` stops **greater than the indentation**
of text `Y` if text `X` begins at column `Column(Y)+N*2`.
We say the indentation of text `X` is `N` stops **less than the indentation**
of text `Y` if text `X` begins at column `Column(Y)-N*2`.

The **rune line** of a hoon is the line on which the hoon's rune occurs,
and the **rune column** of a hoon
is the column at which the hoon's rune begins.

Many alignments are with respect to an **anchor column**.
By default, the anchor column is the rune column,
but there are many important exceptions.
Exceptions are described below.

### Comments

Comments count as whitespace.
A comment is a **header comment** if it is on a line
by itself.
Header comments outside of a running or a jogging
should be immediately followed by a code line or
a line containing another header comment,
and should align with the the code or header comment
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
Inline comments must be aligned according to a scheme yet to be
investigated.

### Staircase comments

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

For `hoonlint` purposes, it is useful to distinquish
between malintended staircase comments,
and malindented normal comments.
Informally, a comment is a staircase comment
if it contains a tread and the first line of the
lower riser.
More formally,
a sequence of comment lines is consider a staircase comment
if

* it contains a tread, properly aligned and followed by
a whitespace character; and

* the line immediately after the tread is 
a comment aligned at the column one stop greater
than the anchor column.

### Gaps

A **newline-equivalent gap** is a gap containing which contains only one
newline, not counting those newlines which termination properly aligned
header commments.

### Types of hoons

Hoons may be backdented, running, jogging, battery or irregular.

* A hoon is a **battery hoon** if it contains an element that
uses the battery syntax.

* A hoon is a **running hoon** if it contains an element that
uses the running syntax.

* A hoon is a **jogging hoon** if it contains an element that
uses the jogging syntax.

* A hoon is a **backdented hoon** if it and contains a fixed number
of gap-separated elements, and none of them follow the running, jogging or
battery syntax.

* A hoon is an **irregular hoon** if it is not a backdented,
running, jogging or backdented hoon.
Most irregular hoons do not contain a gap.

TODO: Corner cases, including BARCAB, lutes and (possibly)
other tall irregular hoons.

### Backdented hoons

The archetypal Hoon whitespace pattern is backdenting.
A backdented hoon of 3 or more elements should be joined.
A backdented hoon of 3 or more elements should be split.

The first element of a joined backdented hoon should be
on the same line as the rune, separated by a gap.
Subsequent elements of a joined backdented hoon should be
separated by a newline-equivalent.

The first element of a joined backdented hoon should be
separated from the rune by a newline-equivalent.
Subsequent elements of a joined backdented hoon should also be
separated by a newline-equivalents.

The last element of a backdented hoon should be aligned at the
anchor column.
All other elements of a backdented hoon should be aligned one
stop more than element that follow them.

This means that, in an *n*-element hoon,
the first element should be indented `n-1` stops more than the anchor column;
a second element should be indented `n-2` stops more than the anchor column;
etc.

### Running hoons

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

### Proper spacing of Runnings

A running is considered to start at the start of its first run step.
Every running ends in a TISTIS (`==`).
The TISTIS should occur on its own line, or a part of a criss-cross
TISTIS line.

A newline-equivalent gap should occur between every pair of runsteps.
A newline-equivalent gap should also occur between the last
and the TISTIS.
Header comments in a running should be aligned at the anchor column
of the parent hoon.

### Horizontal sub-running

A **horizontal sub-running** is a portion of a running which has
two or more runsteps on one line.
The runsteps in a horizontal subrunning
should be separated from each other by one stop.

### Proper spacing of 0-running hoons

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

### Proper spacing of 1-running hoons

The head of a 1-running hoon should occur on the rune line,
one stop after the anchor column.
The running should occur one newline-equivlent gap after the
head, and should be indented one stop after the anchor column.
Header comments in a 1-running hoon should be aligned at the anchor
column of the parent hoon.

As a special case, the running may start on the same line as the
head of the 1-running hoon.
In this case, the first line of the running may be
a horizontal sub-running.

### Jogging hoons

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

### Jogs

A jogging is a gap-separated sequence of one or more jogs.
Every **jog** contains a **jog head**, followed by a gap and a **jog body**.
Note that it is important to distinguish between the head of a jogging
hoon, defined above, and the head of a jog.

A jog is **joined** if its head and its body are both on the same line.
Otherwise, the jog is said to be **split**.

### Chess-sidedness

Jogs, joggings and jogging hoons have **chess-sidedness**.
Chess-sidedness is always either kingside and queenside.

Informally, **kingside** means the indentation has a left-side bias;
and **queenside** means that the indentation has a right-side bias.
Indentation will be described more precisely in what follows.

## Proper spacing of jogs

The indentation of a jog is that of its head.
A joined is **joined** if its head is on the same
line as its body.
Otherwise, a jog is **multiline**.
As explained below,
a multiline jog may be either **pseudo-joined**
or **split**.

A joined jog may be either **aligned** or **ragged**.
A joined jog is ragged is its body is indented 1 stop after
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

The gap of split jog should be newline-equivalent,
with the gap's comments aligned with the jog body.
The indentation of the body of a split kingside jog
should be 1 stop *greater* than the indentation of the jog's head.
The indentation of the body of a split queenside jog
should be 1 stop *less* than the indentation of the jog's head.

## Proper spacing of Joggings

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

Such lines are called "criss-cross" because,
it the boundaries of the hoons were drawn on a tree diagram
of the parse,
and the boundaries were drawn based on matching TISTIS's according
to alignment,
rather than following the logic of the syntax,
the boundaries at the criss-cross TISTIS line
would cross each other.

HEPHEP's may also be joined into criss-cross lines.

## Proper spacing of 1-jogging hoons

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

## Proper spacing of 2-jogging hoons

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
should be one newline-equivalent after the
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

## Proper spacing of jogging-1 hoons

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

### Batteries

The cells of a battery should all align at the
same column, called the **cell alignment column**.
The cells should be separated by newline-equivalents,
where the base column for header comments is the cell
alignment column.
The "cell alignment column" is specified below, for each
battery hoon.

### Battery hoons

The battery hoons are BARCAB, BARCEN and BARKET.

### BARCEN

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
Technically, this is not an exception because, pedantically,
LUSLUS, LUSHEP and LUSTIS are not runes.

### Lutes

Only one-line lutes occur in the `arvo/` corpus.
One-line lutes are free-form -- `hoonlint` never generates
a warning for them.
If `hoonlint` encounters a multi-line lute,
it issues a "not yet implemented" warning.

### Special cases

#### TISSIG

As a special case,
the runsteps in TISSIG should be aligned with the rune.

## Appendix: Non-standard code

For linting purposes, it is necessary to decide the intended
chess-sidedness of misindented jogging hoons, joggings and jogs
in non-standard code.
A jog is considered queenside if its indentation is 2 stops or more
greater than the anchor column.
Otherwise, the jog is considered kingside.

The chess-sidedness of a misindented jogging is that of the majority
of its jogs.
In case of a tie, the jogging is considered to be queenside.
The chess-sidedness of a misindented jogging hoon is that of its
jogging.

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
