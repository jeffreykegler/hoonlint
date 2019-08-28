# HOONLINT TASKS

This is a (subject to revision) plan for work on `hoonlint` and
`hoonfmt`.

## Classify unclassified warnings

As of this writing (Sun Apr  7 12:41:30 EDT 2019),
these "unclassified warnings" remain:

* 3779 in `Lint/arvo.lint.out`
and

* 25 in `Lint/later.suppressions`.

The total is 3804.
I plan, as the next phase, to "classify"
these into "spurious warinings"
and "anomalies".
Spurious warnings will be silenced.
Anomalies will be added to
`Lint/anomaly.suppressions`.

At the end of this phase, I hope to have under 2000 remaining
warnings, all of them in 
`Lint/anomaly.suppressions`.
`Lint/anomaly.suppressions` currently contains 338 warnings.
The corpus is 62205 lines, so this would be less than
one per printed page of code.

## Packaging: Make hoonlint a Perl module

At this point, `hoonlint` should be converted to
a Perl module,
with its own Github repo
and its own test suite.
It should also be documented.

## Packaging: Document whitespace conventions

I have been updating a document formally describing
the whitespace conventions enforced by `hoonlint`,
but I've allowed it to fall behind in some respects.
For example, the concepts behind re-anchoring have
yet to be fully discovered,
and therefore the definition of anchor column
is not complete.

## Prepare for PRs

Once packaging is ready,
we are ready to create PRs containing `hoonlint`
output
against the `arvo/`
corpus.
At this point, we want to settle which sections
of the corpus to target initially.
I think that the listings in the PR should,
for the sake of courtesy
among other reasons,
be manually reviewed before being submitted.

## Move on to hoonfmt

With what has been learned from the `hoonlint` effort,
creation of a spec for `hoonfmt` should be straightforward.
I hope the implementation of `hoonfmt` will,
if tedious at some points,
raise no conceptual issues.
