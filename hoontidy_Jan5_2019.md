# Hoontidy as of 5 Jan 2019

This outlines the remaining hoontidy issues in the
official example hoon files.
Diffs and links to source are in an appendix.

## Extra spaces



## Appendix

In these diff outputs the first, gal-prefixed, lines
are the official version.
The second, gar-prefixed, lines are the "tidied" versions.

These diffs are between:

* for `fizzbuzz`, the
[an official version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/fizzbuzz.hoon
)
and
[the current "tidied" version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/util.d/fizzbuzz.tidied.hoon
).

* for `sieve_b`, the
[the official version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/sieve_b.hoon
}
and
[the current "tidied" version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/util.d/sieve_b.tidied.hoon
).

* for `sieve_k`, the
[the official version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/ast.d/sieve_k.hoon
)
and
[the current "tidied" version](
https://github.com/jeffreykegler/yahc/blob/5115e5cbe0b49cabf9dfa2b22b4014283cf781d0/t/util.d/sieve_k.tidied.hoon
).

* for `toe`, the
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

Note spacing line in 5.

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
