# 0030. The Confidence Gradient is re-scored, not frozen

**Status:** Accepted (2026-07-10) · **Decider:** Nathan

In the context of the book's Confidence Gradient chapter
(`book/src/open-questions.md`) having sat frozen at its Campaign-1a wording
while some two dozen campaigns silently resolved several of the "genuinely
open" bets it listed, we decided that **the chapter is a re-scored map, not a
frozen one: a campaign that resolves or moves one of its bets re-scores that
chapter as part of the Definition-of-Done freshness sweep, and CI link-checks
the chapter so its evidence pointers cannot rot silently**, accepting that the
re-scoring itself is a human judgment the sweep prompts but no test can prove.

**Context.** The chapter classifies the project's open bets by whether the
world can *score itself* on them — self-scorable via the Lab (high confidence)
versus taste-gated (low). That classification changes as campaigns ship: the
enrichment thesis went from the year-one research bet to a twice-validated
verification method, and phenomena-interface generality went from open to
largely settled, yet the chapter still filed both under "low confidence — the
actual research." It is uniquely easy to forget because, unlike the almanacs,
maps, and registry dumps the drift check regenerates, it is **not a generated
artifact** — nothing failed when it went stale.

**Consequence.** The DoD book sweep (decision 0013) names the chapter by name;
a resolved bet is discharged there with a chronicle or gallery pointer, and the
map's goalposts move to the current open frontier rather than the retired one.
`cli/tests/docs_consistency.rs` asserts the chapter's book-internal links
resolve, so a renamed chronicle or gallery target fails loudly. What we
knowingly give up: a mechanical proof that a resolved bet was *actually*
re-scored. That stays a review judgment — deliberately not over-fit into a test
that would force busywork edits on every merge and punish the honest case where
a campaign resolves nothing on the map.

**See also.** Decision 0013 (Definition of Done includes the project book —
this refines it for one chapter); the chapter itself
(`book/src/open-questions.md`); registry row PROC-5; the
`the_confidence_gradient_links_resolve` test in
`cli/tests/docs_consistency.rs`. Slug filename per decision 0026.
