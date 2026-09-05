# 0788. A fork is named by role, never by index

**Status:** Accepted (2026-09-05) · **Decider:** Nathan (G3 / autopilot) ·
**Campaign:** The Cruck · **Relates:**
[0786](0786-a-built-structures-chamber-graph-is-a-derived-tree.md) (the tree
that makes a fork reachable),
[0021](0021-no-alignment-axis.md) (a reading the player can see, never an
index),
[0082](0082-locale-chamber-place.md) (the chamber band this speaks at)

In the context of a dwelling that can now fork, we decided that **`further
in` keeps its meaning where there is exactly one way in and becomes a refusal
that names the ways where there is more than one, and that a way is named by
its role noun** — `the hearth`, `the store`, `the loomroom`, `the smithy`,
`the shrine`, `the hall` — or, as before, by a prose noun of the destination,
accepting that the old "a prose noun only where exactly one aperture is open"
restriction relaxes to "unique among this chamber's apertures".

`Session::further_in` picks the lowest-numbered higher neighbour. On a chain
that is unambiguous; at a fork it is a silent guess, and the session's own
footer text had already said that a fixed "two ways" would be "a lie told to
a real player" under a richer topology. Ordinal ways — "first way", "second
way" — were rejected: an index is not a reason, and the player cannot see it.
Siblings never share a role, because the grammar admits one rule per role, so
role nouns are unique among a chamber's apertures by construction.

The footer lists what it will accept: `Ways on: out, the hearth, the store.`
at a fork, `Ways on: out, further in.` on a chain. That the footer's own
words must be typable is part of the decision, not a detail of it — the first
real walk that named a way found the parser taking `hearth` and refusing
`the hearth`, which is the render/command parity defect this project has
mechanized elsewhere, reappearing on a surface no check covered.

**Accepted cost.** Two committed gallery transcripts that walked a built
dwelling by typing `enter further in` had to be rewritten to name their way,
and any future transcript through a forking structure must do the same. A
player who learned `further in` at a chain meets a refusal at a fork; the
refusal answers with the vocabulary that works.

**See also.** [The Cruck design](../superpowers/specs/2026-09-04-the-cruck-design.md)
§5.3 and [campaign ledger](../superpowers/ledgers/2026-09-04-the-cruck.md)
#7 and #14, and the Task 3 record's two findings.
