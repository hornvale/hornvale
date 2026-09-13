# 0987. A corpus is closed under its source's dependency relation

**Status:** Accepted (2026-09-12) · **Decider:** Nathan (autopilot) ·
**Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0095](0095-a-corpus-is-an-instrument-never-a-standard.md),
[0386](0386-a-corpus-declares-its-demands-or-derives-them-never-both.md),
[0986](0986-a-technology-corpus-is-a-sixth-family-and-a-capability-can-be-lost.md);
[The Kiln](../../book/src/chronicle/the-kiln.md), [The
Cadastre](../superpowers/ledgers/2026-09-12-the-cadastre.md)

In the context of a corpus family whose demands are **derived** rather than
declared (decision 0386), facing the fact that the derivation field
(`presupposes`, for `technologies/`) may name only items already inside the
corpus, we decided that **a corpus drawn from a source that carries its own
dependency relation must be closed under that relation, in the direction that
terminates**, and that **a corpus which cannot close states the truncation
and its size in `provenance`** — accepting that a corpus's item count is no
longer whatever a selection rule happens to admit, but whatever admission
plus closure produces.

**This rule binds every corpus family whose source is graph-structured, not
only `technologies/`.** It is stated here, not only in `technologies/CLAUDE.md`,
because the defect it repairs is structural to decision 0386 itself and will
recur wherever a future corpus derives its demands from a linked catalogue.

## The defect

Decision 0386 derives a corpus's demands by transitive closure over
`presupposes`, and states plainly that a corpus's demand set is *not* its
item list — it is the closure of its item list. But `presupposes` may name
only in-corpus items: the field has nowhere to point outside the file. So any
corpus authored from a **subset** of a source that itself carries a
dependency relation loses every edge crossing that subset's boundary, at
authoring time, silently, before any resolver could see the loss — the field
that would carry the missing edge simply does not exist to carry it.

The Kiln, which authored `technologies/asimov-1989` as a 41-item sample of a
1,484-item catalogue, documented the resulting damage rather than hiding it:

> `presupposes` NAMES ITEMS IN THIS CORPUS AND NOTHING ELSE, WHICH DROPS REAL
> PREREQUISITES ON PURPOSE. … The consequence is that the derived demand set
> UNDER-DESCRIBES every such item's real prerequisites.

`inv-printing-press` really is built on steel, in the source catalogue's own
terms; a corpus that admits the printing press without admitting steel says
it is built on nothing. **The corpus cannot compute what it already claims
about itself**, because the field that would let it is structurally blind to
anything outside the sample.

This is not an authoring slip fixable by a more careful selection. It is a
consequence of derivation (0386) meeting sampling: whenever the two combine,
the sample's boundary silently truncates the very relation the corpus exists
to expose.

## The rule

> A corpus drawn from a source that carries its own dependency relation is
> closed under that relation, in the direction that terminates. A corpus that
> cannot close states the truncation and its size in `provenance`.

**The direction matters, and it is not a free choice.** A dependency relation
generally has two directions — `technologies/`'s source names both `Built on`
(prerequisites) and `Led to` (consequences) — and only one of them is safe to
close over. Closing under `Built on` terminates: prerequisites bottom out,
because the population of things a given item depends on is finite and
(absent a cycle in the source, which authoring must check for and refuse to
paper over) acyclic. Closing under `Led to` does not: consequences propagate
forward across a connected catalogue without a natural stopping point, so
closing under it would not select a corpus, it would reconstruct the whole
graph one link at a time. **Downward (prerequisite) closure is the direction
that is both blind — no item is added or removed by hand — and finite**,
which is what makes it a rule rather than a preference between two equally
valid choices.

**A corpus that cannot close is not thereby forbidden** — closure may be
infeasible for a source with no acyclic finite direction, or deliberately
declined for a reason recorded elsewhere. What it may not do is stay silent
about the gap: `provenance` states that the corpus is truncated and the size
of what was cut, so a reader can tell "closed, complete" from "sampled,
truncated" without re-deriving it from the source.

## The measured repair

Applied to `technologies/asimov-1989`: the seed (three named story arcs plus
every item attested before 1700) closed under the source's own `Built on`
relation grew the corpus from **41 items to 301**, and its lattice from **40
edges to 401**. The closure terminated at a single root (`biped`), with zero
cycles — confirming, rather than assuming, that this direction was safe to
close. 260 of the 301 items were new to the corpus and required their own
verdict; none of the 41 original items' verdicts, ids, or arc attribution
changed.

## Consequence

A corpus family that derives demands from a linked field must close before
freezing, or declare in `provenance` exactly how much of the source's own
relation it has deliberately left outside the file. Selecting a corpus by any
rule that is blind to the source's own dependency edges — an era cut, a
keyword sieve, a hand-picked sample — is no longer sufficient on its own;
closure is a second, mandatory pass over whatever the selection rule admits.

**See also.** `technologies/CLAUDE.md` (the family-law statement of this
rule); `docs/superpowers/specs/2026-09-12-the-cadastre-design.md` §1, §4, §7;
the campaign ledger, entry #1 and Task 1.
