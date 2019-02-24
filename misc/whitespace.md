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
align at the rune column.

If a comment is not a header
comment, it is a **rightside** comment.
A rightside comment is
a **margin comment** if it begins at or after column 57,
or immediately after a horizontal gap of 20 or more spaces.
All margin comments should start at column 57.

A rightside comment is an **inline comment** if it is not a margin comment.
Inline comments must be aligned according to a scheme yet to be
investigated.

### Gaps

A **newline-equivalent gap** is a gap containing which contains only one
newline, not counting those newlines which termination properly aligned
header commments.

### Running hoons

A hoon is a **running hoon** if it contains an element that
uses the running syntax.
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
SEMSIG (`;~`),
and
TISSIG (`=~`).

### Proper spacing of Runnings

A running is considered to start at the start of its first run step.
Every running ends in a TISTIS (`==`).
The TISTIS should occur on its own line, or a part of a criss-cross
TISTIS line.

A newline-equivalent gap should occur between every pair of runsteps.
A newline-equivalent gap should also occur between the last
and the TISTIS.
Header comments in a running should be aligned with the rune
of the parent hoon.

### Proper spacing of 1-running hoons

The head of a 1-running hoon should occur on the rune line,
one stop after the rune.
The running should occur one newline-equivlent gap after the
head, and should be indented one stop from the rune column.
Header comments in a running hoon should be aligned with the rune
of the parent hoon.

### Jogging hoons

A hoon is a **jogging hoon** if it contains an element that
uses the jogging syntax.
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

## Criss-cross TISTIS lines

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

## Proper spacing of 1-jogging hoons

Every 1-jogging hoon is either kingside or queenside.
The base column for jogs of a 1-jogging hoon is its rune column.

"Sidedness" should be consistent:

* The head of a kingside 1-jogging hoon should be kingside.
It should be on the rune line,
indented 1 stop after the rune.

* The jogging of a kingside 1-jogging hoon should be kingside.
It should be on a line after the rune line,
and should consist entirely of kingside jogs.

* The head of a queenside 1-jogging hoon should be queenside.
It should be on the rune line,
indented 2 stops after the rune.

* The jogging of a queenside 1-jogging hoon should be queenside.
It should be on a line after the rune line,
and should consist entirely of queenside jogs.

## Proper spacing of jogging-1 hoons

Every jogging-1 hoon is either kingside or queenside.
Queenside jogs do not actually occur in the `arvo/` corpus,
so their description here is speculative.
The base column for jogs of a jogging-1 hoon is one stop greater than its rune column.

"Sidedness" of a jogging-1 hoon should be consistent:

* The jogging of a kingside jogging-1 hoon should be kingside.
It should start on the rune line,
1 stop after the rune line,
and should consist entirely of kingside jogs.

* The jogging of a queenside jogging-1 hoon should be queenside.
It should start on the rune line,
2 stops after the rune line,
and should consist entirely of queenside jogs.

* The tail of a jogging-1 hoon should be
should be on its own line,
and aligned with the rune.

## Non-standard code

For linting purposes, it is necessary to decide the intended
chess-sidedness of misindented jogging hoons, joggings and jogs
in non-standard code.
A jog is considered queenside if its indentation is 2 stops or more
greater than the rune column.
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
