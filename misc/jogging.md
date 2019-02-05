# Hoon jogging indentation

## Terminology

A hoon is a *jogging hoon* if it contains an element that
uses the jogging syntax.
A jogging element is more often called simply a *jogging*.
(Currently all hoons contains at most one jogging.)

In addition to the jogging, a jogging hoon may contain
other elements, either before or after the jogging.
An element after the jogging is called a *tail*.
If there is one element before the jogging, it
is called the *head* of the jogging hoon.
If there are two elements before the jogging, they
are called, in order, the *head* and *subhead* of
the jogging.

There are current four kinds of jogging.
A 0-jogging has no head and no tail.
A 1-jogging has one head and no tail.
A 2-jogging has a head and a subhead and no tail.
A jogging-1 has a tail and no head.

The current 0-jogging rules are WUTBAR (?|) and WUTPAM (?&).
The current 1-jogging rules are CENTIS (%=), CENCAB (%_) and WUTHEP (?-).
The current 2-jogging rules are CENTAR (%*) and WUTLUS (?+).
The current jogging-1 rule is TISCOL (=:).

## Limitations of this current document

The rest of this document describes the indentation of the 1-jogging
rules only.
I expect to revise it to describe the other rules along similar lines.

