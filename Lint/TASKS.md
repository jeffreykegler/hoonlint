# HOONLINT TASKS

This is a (subject to revision) plan for work on `hoonlint` and
`hoonfmt`.

## Eliminate "not yet implemented" syntax.

Finish implementing all the hoon syntax represented in
the `arvo/` corpus.
At this point the unsettled issues are 
down to cases where the count of instances in the corpus is
in the single digits.

## Eliminate spurious warnings

With the full implementation of the hoon syntax,
remaining problems should be mostly spurious warnings,
and absent warnings should be few.
As of this writing there are about 4400 warnings.
We can perhaps reduce this to about 1000.
The corpus is 62205 lines, so this is
less than one per printed page of code.

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
and therefore the definitionn of anchor column
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
for the sake among other reasons,
be manually reviewed before being submitted.

## Move on to hoonfmt

With what has been learned from the `hoonlint` effort,
creation of a spec for `hoonfmt` should be straightforward.
I hope the implementation of `hoonfmt` will,
if tedious at some points,
raise no conceptual issues.
