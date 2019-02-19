# Hoon jogging indentation

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
terms in such a what that they are meaningful even
when applied to non-standard code.

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

## Limitations of this current document

The rest of this document describes the indentation of the 1-jogging
rules only.
I expect to treat the other rules along similar lines,
and expect to revise this document accordingly.

## Proper spacing of Jogs

The indentation of a jog is that of its head.
A kingside jog should have an indentation 1 stop greater than
the rune column.
A queenside jog should have an indentation 2 stops greater than
the rune column.

A joined jog may be either **aligned** or **ragged**.
A joined jog is ragged is its body is indented 1 stop after
its head.
Otherwise, a joined jog is considered aligned.

All aligned jogs in a jogging should be indented to the
same column.
This column is called the **jog body column** of the jogging.

The indentation of the body of a split kingside jog
should be 1 stop *greater* than the indentation of the jog's head.
The indentation of the body of a split queenside jog
should be 1 stop *less* than the indentation of the jog's head.

## Proper spacing of 1-jogging hoons

Every 1-jogging hoon is either kingside or queenside.

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
the jog body column of a jogging is considered to be the most common start column
of the bodies of the jogging's aligned jogs.
In case of a tie, the body column of the first jog with one of the most common
body columns is the jog body column of the jogging.
If there are no aligned jogs in a jogging,
the jog body column is the body column of the first jog in the jogging.
