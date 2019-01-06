# Hoontidy as of 5 Jan 2019

This outlines the remaining hoontidy issues in the
official example hoon files.
Diffs and links to source are in an appendix.

## Extra spaces

Last week hoontidy found an small spacing error
in one of the official versions of `fizzbuzz`.
This week, it found one on `toe`.

## Parent versus conserving alignment.

The hoon examples use two different alignments for backdented
hoons.
In one, the "base" alignment of the twigs is that of their
parent.
I call this "parent" alignment.

In the other,
the "base" alignment is that of an ancestor of the the
parent on the same line as the parent.
I call this "conserving" alignment,
because it can be seen as conserving indentation.

In one case in the official examples,
the same file uses different alignments for the same
rune, kethep (^?).
In [the official toe example, starting on line 54](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/toe.hoon#L54
),
wutgal (?<),
the child of kethep, 
is aligned with kethep.
In [the same example, starting on line 4](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/toe.hoon#L4
),
wutsig (?~) is the child of kethep,
and it is aligned, not with kethep,
but with barhep (|-), the leftmost ancestor on the same line as the parent kethep.

### Discussion

To my eye, parent alignment looks nicer.
On the other hand,
as the name suggests, "conserving" alignment conserves indentation
better.

There are mixed solutions which may capture the usage
in the examples.
In the `toe` file, the inconsistency could be explained by the
ancestor.
Luslus (++), the ancestor in the parent-alignment case is not strictly a rune.
Barhep (|-), the ancestor in the conserving-alignment case, is a unary rune.

So the inconsistency might be resolved by special-casing luslus,
or by special-casing unary ancestors.

For consistency, in the appendices,
this week's version of `hoontidy` uses parent alignment
throughout.
As a side effect,
this causes the two sieve examples,
which `hoontidy` "round-tripped" without changes last week,
to undergo alignment changes in this week's version.

## Inline backdenting

I tried adding a feature to "normalize" non-vertical gaps to two spaces,
and discovered one of the backdenting corner cases.
When the twigs of a hoon are backdented, and one of them is on the same line
as the hoon's rune, following it,
it also follows the backdenting, adding extra spaces as necessary.
This case shows up on
[line 5 of the toe example](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/toe.hoon#L5
).


## Appendix

In these diff outputs the first, gal-prefixed, lines
are the official version.
The second, gar-prefixed, lines are the "tidied" versions.

### Links

The diffs which follow are between:

* For `fizzbuzz`, the
[an official version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/fizzbuzz.hoon
)
and
[the current "tidied" version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/util.d/fizzbuzz.tidied.hoon
).

* For `sieve_b`, the
[the official version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/sieve_b.hoon
)
and
[the current "tidied" version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/util.d/sieve_b.tidied.hoon
).

* For `sieve_k`, the
[the official version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/sieve_k.hoon
)
and
[the current "tidied" version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/util.d/sieve_k.tidied.hoon
).

* For `toe`, the
[the official version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/toe.hoon
)
and
[the current "tidied" version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/util.d/toe.tidied.hoon
).

### Extra spaces

#### fizzbuzz

There are two "official" fizzbuzz versions.
Only one of them has this issue.

```
6c6
<    ~
---
>   ~
```

#### toe

```
58c58
<                [[~ %win ?:(w.game %x %o)] nu]           ::  58
---
>               [[~ %win ?:(w.game %x %o)] nu]            ::  58
```

### Parent versus conserving alignment

#### sieve_b

```
12,14c12,14
<   ?:  (gth (mul factor factor) thru)                    ::  12
<     ..main                                              ::  13
<   $(factor +(factor), ..main (reap factor))             ::  14
---
>       ?:  (gth (mul factor factor) thru)                ::  12
>         ..main                                          ::  13
>       $(factor +(factor), ..main (reap factor))         ::  14
20,25c20,25
<   ?:  (gth count thru)                                  ::  20
<     ..reap                                              ::  21
<   %=  $                                                 ::  22
<     count  (add count factor)                           ::  23
<     field  (~(del in field) count)                      ::  24
<   ==                                                    ::  25
---
>       ?:  (gth count thru)                              ::  20
>         ..reap                                          ::  21
>       %=  $                                             ::  22
>         count  (add count factor)                       ::  23
>         field  (~(del in field) count)                  ::  24
>       ==                                                ::  25
```

#### sieve_k

```
10,12c10,12
<   ?:  (gth (mul fac fac) top)
<     ..main
<   $(fac +(fac), ..main (reap fac))
---
>       ?:  (gth (mul fac fac) top)
>         ..main
>       $(fac +(fac), ..main (reap fac))
18,20c18,20
<   ?:  (gth cot top)
<     ..reap
<   $(cot (add cot fac), fed (~(del in fed) cot))
---
>       ?:  (gth cot top)
>         ..reap
>       $(cot (add cot fac), fed (~(del in fed) cot))
```

#### toe

```
4,9c4,9
<     ?~  feed  ~                                         ::  4
<     =^    this/(unit fact)                              ::  5
<         game                                            ::  6
<       (~(do go game) i.feed)                            ::  7
<     =/  rest/(list fact)  $(feed t.feed)                ::  8
<     ?~(this rest [u.this rest])                         ::  9
---
>         ?~  feed  ~                                     ::  4
>         =^  this/(unit fact)                            ::  5
>             game                                        ::  6
>           (~(do go game) i.feed)                        ::  7
>         =/  rest/(list fact)  $(feed t.feed)            ::  8
>         ?~(this rest [u.this rest])                     ::  9
```

### Inline backdenting

Note spacing in line 5.

#### toe

```
4,9c4,9
<     ?~  feed  ~                                         ::  4
<     =^    this/(unit fact)                              ::  5
<         game                                            ::  6
<       (~(do go game) i.feed)                            ::  7
<     =/  rest/(list fact)  $(feed t.feed)                ::  8
<     ?~(this rest [u.this rest])                         ::  9
---
>         ?~  feed  ~                                     ::  4
>         =^  this/(unit fact)                            ::  5
>             game                                        ::  6
>           (~(do go game) i.feed)                        ::  7
>         =/  rest/(list fact)  $(feed t.feed)            ::  8
>         ?~(this rest [u.this rest])                     ::  9
```
