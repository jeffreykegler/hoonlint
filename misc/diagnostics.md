# A long-felt need and three long-known solutions

In [his blog post](http://duriansoftware.com/joe/Constructing-human-grade-parsers.html),
Joe Groff laments, appropriately,
the state of the art in error-detection.
He suggests three techniques.

* "Synchronization" or "recoverability" from misformed input.

* "Partial output", that is, production of a parse output,
  even from misformed input.

* "Total grammars" or parsing that "always succeeds", even in the face
  of misformed input.

In fact all three have long been known,
and collectively are the subject of a very substantial literature.
In this document,
I'll often fall back to the terminology of the 2nd edition
of Grune and Jacobs,
which connects more easily with previous research.

* What Groff calls "synchronization" or "recoverability",
  Grune and Jacobs call **error recovery**.

* Grune and Jacob's term for parsing that produces a "partial output"
  is **substring parsing**.

* The techniques that Groff describes for
  creating "total grammars",
  Grune and Jacobs call **error productions**
  and **error tokens**,
  and Grune and Jacob's term for "always succeed" parsing
  is **ad hoc parsing**.

[ Grune and Jacobs summarize the research on error recovery,
in their 2nd edition, pp. 526-543.
[Their online bibliography](https://dickgrune.com/Books/PTAPG_2nd_Edition/CompleteList.pdf)
has 148 entries
under the topic of "Error handling"
(section 18.2.7, pp. 687-703).
Grune and Jacob's discussion of substring parsing is in their Chapter 12
(pp. 399-423) and is section 18.2.3 (pp. 656-657) in their online
bibliography.
]

# Why so little progress?

Why, if the need is widely felt and techniques that solve it are well
known, has there been so little progress?
As a start, let's focus on ad hoc parsing.
We will call the language actually targeted by a parser,
its "core" language.
We will call the language as extended
to allow "always succeed" parsing,
an "ad hoc" language.

In his post,
Groff's example is arithmetic expressions.
But his grammar does not fully represent the syntax of arithmetic expressions --
it ignores associativity.
Groff appears to have in mind Recursive Descent (RD) parsers,
and these are not powerful enough to deal directly with associativity in arithmetic
expressions.
Typically, RD parsers tackle arithmetic expressions using an over-simplified grammar,
like the one Groff presents,
and then fix the parse in post-processing.
Presumably this is Groff's intention.

Given that a parser that cannot even handle its "core" language,
modifying it to deal with an extended "ad hoc" language will be,
at a minimum, difficult.
If we were dealing with a parsing algorithm, in fact, it would
be impossible.
But RD is an extensible technique, not a specific algorithm.
In theory, RD can be hacked up to do anything that is Turing-computable.
An RD implementation could, for example, incorporate a Earley parser
with the latest improvements
as a coroutine and,
so equipped,
RD could be said to do anything an Earley parser can do.
Such an extension,
of course, would be pointless -- if you rely on the Earley parser,
why not use it directly?

But extensions
to a parser which is already overburdened by
its core language are likely to be awkward.
Existing RD implementations for
large languages, often,
are already severe maintenance challenges.
Incremental addition of error tokens and productions,
if they have to justify themselves in terms of added-functionality
versus added-complexity,
face an uphill battle and diminishing returns.

All of which explains why, despite ad hoc parsing being long known,
only limited examples of it are found "in the wild".

# Earley parsing and ad hoc parsing

Early in
my work in the Earley/Leo lineage,
I came to believe that it had
the power to break through the error-detection logjam.
I tackled both ad hoc parsing and error recovery,
implemented parsers to prove my ideas,
and wrote blog posts about them.
And partial parsing is an integral part of Marpa::R2.

The idea behind ad hoc parsing is simple.
We first define the
"core" language.
We then add productions and tokens
to implement a parser which succeeds on,
not just the core language,
but any input whatsoever.

If the input was not in the core language,
it will contain subparses which contain either error
productions or error tokens.
These errors can be identified,
and reported,
with their location,
to the writer of the misformed source.

My most ambitious "ad hoc" parser was a configurable ad hoc HTML parser.
(In my writings on it, I used  the term "liberal" instead of "ad hoc".)
This takes any input file whatsoever
and treats it as if it was HTML,
albeit perhaps severely misformed HTML.

Ad hoc HTML parsing is certainly
not new with Marpa.
Most browsers never reject any text that is presented to them under
the pretense of being HTML -- they display it as best they can.
And it is easy to imagine a browser being modified to
replace misformed text with an error message.

This is how my Earley-Leo-based
[`html_fmt` utility](https://metacpan.org/pod/distribution/Marpa-R2/html/pod/html_fmt.pod)
works.
`html_fmt` never rejects an input but,
where the HTML appears to be clearly defective,
`html_fmt` adds comments to the HTML to point out where `html_fmt` made its corrections.
These HTML comments do not appear when the HTML
is displayed,
but they are present in the HTML source that `html_fmt` outputs.
The user can use these to guide his corrections of the HTML.

[ I wrote
[a lot](http://jeffreykegler.github.io/Ocean-of-Awareness-blog/metapages/annotated.html#PARSE_HTML)
about my configurable parser,
and it is available as part of Marpa::R2.
`html_fmt` is the Marpa application that I personally use most.
]

# Error recovery

## The general problem

Error recovery in Marpa is a similar breakthrough.
With Marpa's LR-regular power,
precise error recovery for a sequence is straight-forward.
Recovery from a error in a block-structured language is much more
complicated.

The general problem of error recovery can be treated as
equivalent to that of recovering from
mismatched delimiters, that is, correcting strings like "`((([))`".
There is no perfect solution to these problems short of mind-reading.
But, given Marpa's power, workable solutions are possible.
I designed and implemented one of these and
[blogged it](http://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2014/11/delimiter.html).

## Combinators

Marpa allows combinator parsing.
This means that, for synchronization,
Marpa can go beyond even what an ad hoc grammar can do --
it can actually switch parsers and use one that is specialized
for synchronization.
The subparser can be another Marpa parser with the same
grammar; another Marpa parser with a different grammar;
or another parser entirely.

[ Two of my blog posts on Marpa's combinator parsing
are
[here](
http://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2018/05/combinator.html
)
and [here](
http://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2018/05/combinator2.html
). ]

## Ruby Slippers

Marpa also has a feature called the "Ruby Slippers" -- if a Marpa parse cannot continue,
for example due to a missing token,
it can stop and ask the application how to handle that.
The application can "invent" a token on the spot,
and retry the Marpa parse.
In effect, this "wishes" a token into being, hence the name "Ruby Slippers".

To use the Ruby Slippers in detection of missing tokens, the grammar can be changed
to accept two kinds of token:
"physical" (actually present in the input);
and "virtual" (invented on the fly for error detection).
Virtual tokens in the completed parse, then,
will represent tokens erroneously omitted in the source.

# Pin-point error detection and left-eideticism

Traditional parsing algorithms often
report failure too late.
Traditional deterministic algorithms are state-driven,
and their state tables assume correct input.
(In RD, the state table is implicit.)
These state tables do have error cells, but they are not necessarily
reached at the point where the error first occurs.

Marpa spots errors exactly when they occur.
Of course,
finding
the "real" point of failure requires knowing the programmer's intent,
and Marpa does not read minds.
But Marpa does know the exact point at which the input is no longer
a correct prefix for any string in the target language.

In other words, Marpa knows the point at which additional input cannot possibly
produce a correct parse.
This often corresponds to the user's intuitive idea of
"point at which the parse failed".
Even when Marpa's failure point is not the intuitive one, it is
straightforward for a user to determine why Marpa picked its
point-of-failure.
By contrast,
second-guessing the point-of-failure in a traditional algorithm
often requires examining the state table,
a task which even those comfortable with parsing theory avoid.

# Partial output

Marpa has another "pin-pointing" aid that traditional parsers lack --
left-eideticism.
At any given point, Marpa is fully aware of the parse so far,
including all possible parses.
This is Marpa's solution to the question of partial output,
and it is built in.

Left-eideticism is also helpful in error reporting.
In cases where the pin-point location is not enough.
Marpa can answer questions like

- "What was the last complete statement, etc. recognized?"

- "Are any expressions, etc., complete at the current location?"

- "If any statements are incomplete at the current location, where did they start?"

# Mixing production and ad hoc parsing

A production parser could use
a debugging "ad hoc" parser as fallback,
so that the debugging parser is called when the
first error is encountered.
This would mean that, on one hand,
production runs that are recompiling code,
if that code is expected to be error-free,
can run with a
stream-lined parser.

On the other hand,
when the confidence that the code is error-free proves
to be misplaced,
advanced error-detection is deployed.
Of course,
developers who expect trouble may want to save a little overhead,
and invoke the debugging compiler
directly.

# Hoon-specific points

Most of the techniques described above have straightforward application
to Hoon.
For example, in dealing with wide twigs,
and runes with battery and running syntax.
the techniques for mismatched delimiters have a clear application.

For tall, fixed-length twigs, other clues must be sought.
A "stray" twig may have been caused by a wrong rune,
and the error might have occurred at various levels.
Here indentation can be used as a clue -- a repair that makes the syntax
match the indentation is more likely to be correct.
For this reason,
tracking whitespace is probably essential in 
a Hoon debugging parser.
