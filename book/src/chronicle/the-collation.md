# The Collation

To collate, in textual scholarship, is to set two witnesses of a text side by
side and write down where they differ. The variants are the whole point. A
single manuscript, however carefully read, cannot tell you which of its
readings belong to the work and which belong to the copyist; it takes a second
witness to make the first one's peculiarities visible at all.

[The Repertoire](./the-repertoire.md) built a capability probe and read it
once, against Georges Polti's thirty-six dramatic situations. [Decision
0095](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0095-a-corpus-is-an-instrument-never-a-standard.md)
then ratified what that reading *was*: a corpus is an instrument carrying a
declared bias, never a standard, and its output is a **matrix over corpora**
rather than a score. The decision closed by naming its own debt — one corpus
supplies one column, and the first genuine matrix claim waits for corpus number
two. This campaign took the second reading and made the disagreement between
the two witnesses a generated artifact rather than something a reader has to
assemble by eye.

## The column that already existed

Corpus two was not built here and was not built for this. `tvtropes-2012` is
409 character tropes drawn from a May–June 2012 wiki rip, selected by the
wiki's own editorial judgment rather than a compiler's — entries the wiki both
dissected on a *PlayingWith* page and filed in one of seven character indexes —
with their requirements mapped onto the bundle vocabulary by a model reading
wiki prose blind. It was frozen before first measurement in a sibling
repository on 2026-08-03, and for three days nothing here read it. The matrix
claim 0095 deferred had been available and untaken.

The corpus now lives here, copied byte-for-byte, because a gate cannot depend
on a sibling checkout. Its situation count is asserted at 409 the same way
Polti's is asserted at 36 and for the same reason: the count is what every
figure downstream was scored against, so moving it has to be a deliberate act
somebody comes here to perform.

Two things had to change before a second column could exist at all. The
report's `check` mode read **one hardcoded artifact path**, so any corpus but
Polti compared its own render against Polti's committed file and could only
ever fail — a second column was not merely absent, it was unreachable. The path
is now derived from the corpus's own `corpus` field, which means a caller
cannot pair the wrong corpus with the wrong artifact; making the path merely
configurable would have preserved exactly that failure. Polti's artifact was
renamed to match the new shape rather than keeping the unsuffixed name, since
within a month an asymmetry reads as an accident and the next corpus inherits
the confusion.

Both columns resolve against **one registry, built once per run**. That is what
makes the collation a collation: a difference between the columns is a
difference between the catalogues, and never a difference between two worlds.

## Both columns read zero

`polti-1895`: **0 stageable of 36**, 1 inapplicable.
`tvtropes-2012`: **0 stageable of 409**, 62 inapplicable.

Nothing in the second reading disturbs the first. Zero was the expected
baseline — the probe measures a world whose person-level machinery is not built
— and a green reading from a 2012 fan taxonomy would have meant the corpus was
drawn wrong, exactly as it would have for Polti. So the matrix is not a
scoreboard, and the finding is not in the score.

**It is in the demand.** The two catalogues ask the world for different things,
and a single column could not have shown that no matter how carefully it was
read.

## What the second witness shows

Polti's 1895 taxonomy is about **will, blood and love**: what a person intends,
who they are related to, whom they desire, how they judge themselves. The 2012
fan taxonomy is about **knowledge, identity and standing**: what a character
knows, whether they are recognised, which norms they break, how they are
regarded. Neither is more complete. They are different instruments, which is
the whole content of 0095 and the reason one column was never enough to support
it.

Two consequences follow, and both are new information rather than a restatement
of either column.

**They agree, without exception, on what to build first.**
`bundle:individual-persons` is required by **every situation in both
catalogues** — 36 of 36 and 409 of 409, and by 100% of the blocked situations
in each. Two instruments built a century apart, for different media, by
different methods, converge without a single exception on the first thing this
world lacks. The Repertoire's backlog claim was that the next tier is
ontological rather than narrative; the second witness does not soften it.

**They fork immediately after.** Polti's second rank is `bundle:intent`, at 50%
of its situations against the fan taxonomy's 25%. The fan taxonomy's second
rank is `bundle:agent-knowledge`, at 53% against Polti's 19%. That is a
**choice** and not a ranking artefact: after the one thing both catalogues
demand, the question *what should exist next* has two defensible answers, and
which one a backlog takes depends on which instrument it means to satisfy. A
single column presents its own rank 2 as the answer, because it has nothing to
disagree with.

The third reading is about the instruments themselves. **Inapplicable runs
15.2% in the fan taxonomy against Polti's 2.8%** — sixty-two situations against
one. A taxonomy of modern screen media is substantially about cameras, casting,
adaptation and audience reception, none of which a world derived from a seed
has anything to attach to. These exclusions are written into the corpus at
freeze, each carrying its published reason, so the rate is a declared property
of the instrument rather than an outcome of the run — and the corpus's own
emission record says as much, calling the gap a finding and calling 15.2% a
**floor**, since only what a blind judge happened to flag was excluded at all.
Under 0095 that gap is a finding about the catalogue and not a deficiency in
the world, and the third verdict is the machinery that lets it be recorded as
one. The verdict earned its place exactly once in The Repertoire, which was
thin evidence for keeping it; it fires sixty-two times in the second column.

A fourth reading falls out of the collation and belongs to neither column
alone. Of the 52 bundles either catalogue requires, **12 are declared by no
catalogue at all** — time travel, machines, arcane power, healing, food. A
requirement naming an undeclared bundle expands to itself, matches no registry
token, and blocks its situation by construction, so the row exists because a
catalogue *asked* and asking is all it can do. That is default-deny working as
designed rather than a defect, and it is legible only in a table that lists
demand across catalogues instead of within one.

## Why the matrix is generated

`docs/audits/` is described in its own README as generated, drift-checked and
never hand-edited, and a hand-written matrix would have been the single
untrusted document in that directory. It would also have been wrong on the
first day either column moved, which is the specific failure a summary of two
artifacts is most prone to.

So
[`docs/audits/trope-matrix.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/trope-matrix.md)
is rendered by `hornvale tropes matrix` from the same resolution the columns
use, and ratcheted the same way — a whole-file byte comparison against the
committed copy. Its shares are counted over the corpora rather than read back
out of the rendered columns; its sort order is stated and re-derivable from the
counts printed in its own cells; and one test walks the other direction
entirely, parsing each committed column's headline and each matrix row and
requiring them to agree. The byte check alone pins the matrix to *itself*, and
would happily ratify a matrix whose figures had drifted from the columns it
summarises — which is the one failure a generated summary can still have, and
the reason the second check exists.

## What it still does not say

The wording The Repertoire was careful about holds, and the second column does
not license loosening it. This is **reach against these catalogues**, never
*coverage*. It scores representability only: whether the world can hold the
facts a situation needs, not whether any mind in it could plan or recognise
one. And it is not a ranking of the catalogues against each other — each column
is a reading taken through a declared bias, so a column is worth precisely what
its provenance says it is worth.

What has changed is that the disagreement is now a committed, re-derivable
document rather than an argument. Adding a third corpus adds a column; the
machinery is per-corpus rather than two-corpus, and nothing here proposes one.
Whether to build toward `intent` or `agent-knowledge` is a backlog decision the
matrix exists to inform and deliberately does not make.
