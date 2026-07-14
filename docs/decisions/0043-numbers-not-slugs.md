# 0043. Numbers, not slugs

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of the decision log having run mixed-era since decision 0026
(thirteen slug-named records interleaved among numbered ones, with the
chronology recoverable only from git), facing an index that no longer read
in order and citations in two competing styles, we decided that **decision
records return to `NNNN-kebab-title` filenames, numbered in order of
creation** — the slug-era records were renumbered 0027–0039 by git creation
date, and the three records minted as 0027–0029 on 2026-07-13 (nextest,
libm, manual-only CI) moved to 0040–0042 — accepting the one renumbering
0026 forswore, with every in-repo cross-reference rewritten to match.

**Context.** Decision 0026 keyed all new durable artifacts by slug because
parallel sessions kept colliding on sequence numbers. For decisions the cost
outweighed the cure: records are minted rarely, usually at merge time on
`main`, and the log's index is read ordered. This record supersedes **only
0026's decision-record provision**; its other provisions stand — studies
stay keyed by study name, chronicle entries and retrospectives stay
slug-only, campaigns stay name-only, and registry rows stay category+slug.

**Consequence.** The renumbered filenames keep their old slug as the tail
(`0033-serialized-floats-are-quantized-for-cross-platform-determinism.md`),
so slug cites in history keep resolving — `docs_consistency`'s cite checker
already accepts a numbered record's slug tail. House style for new cites
returns to `decision NNNN`. One dating caveat is permanent: git commit
messages written before 2026-07-13 that cite decisions 0027–0029 mean
today's 0040–0042. Sessions minting a decision off `main` must again
confirm the next free number at merge, exactly the coordination 0026
removed.

**See also.** Decision 0026 (superseded here for decision records);
decision 0017 (campaigns dropped Year naming — 0026's supersession of its
number+name convention stands: campaigns remain name-only).
