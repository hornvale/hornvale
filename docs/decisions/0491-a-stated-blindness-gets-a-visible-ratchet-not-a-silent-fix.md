# 0491. A stated blindness gets a visible ratchet, not a silent fix

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0028](0028-the-bare-ok-rubric.md) (the same append-and-review
ratchet idiom applied to a different registry) ·
[0457](0457-a-generated-paths-author-absence-is-declared-not-deleted.md) ·
[The Cartulary](../../book/src/chronicle/the-cartulary.md), Task 3

In the context of Task 3's non-empty-ledger check needing an exemption list
for campaigns that predate this practice, matching specs and plans to
campaign ledgers by exact slug, and the reviewer demonstrating that a future
spec/plan pair whose names defeat that exact-slug matcher (the `the-deed-
design`/`the-deed-state` shape the code's own comment already names) is
invisible to **both** the "missing" check and the exemption list — never
flagged, never exempted, simply unseen, permanently — we decided **the count
of unmatched spec/plan pairs (54, at time of writing) is itself frozen as a
ratchet**, so a new unmatched campaign moves the count and reddens the day it
happens, accepting that this makes the hole visible without closing it, and
says so in its own doc comment.

## Context

The code's own doc comment already stated this blindness in the harsh,
accurate form. What read as reassuring was the *report's* summary line
("safe for the ratchet's correctness") — a narrower claim, true only of the
exemption list's internal consistency, not of whether every campaign is
actually seen. This campaign's whole thesis is that an absence needs a row;
a campaign the check never looks at is exactly that — an absence with no row,
inside the instrument built to remove them.

`>=` was considered for the ratchet's comparison and rejected: the reviewer
checked by hand that `>=` would have let its own probe pass (55 >= 54),
making the ratchet a no-op against the exact defect it exists to catch.
Unlike a decision-block reservation count, where growth is inherently safe
because reservations are append-never, this population mixes legitimate
spec-less growth with real matcher misses, so a rise cannot be waved through
by rule — `==` is necessary, not merely defensible.

## The rule

Freeze the unmatched-plan count the same way `tropes check`, the timings
baseline, and type-audit's `waiver(...)` freeze their own counts: a
committed number that must move deliberately, reviewed as a diff, rather than
drift silently. The fix's own doc comment states exactly what it does and
does not guarantee: *"This test does not close the hole ... it only makes
the count that hole hides in impossible to move quietly."*

## Consequences

- A rise in the frozen count is now a visible, reviewable diff rather than a
  silent gap — a human is forced to look, even though the check still cannot
  tell them whether the new unmatched pair is legitimate spec-less growth or
  a genuine matcher miss.
- The reviewer reproduced the red with a different shape than the
  implementer used (a one-day date drift rather than a stage word), so the
  ratchet's guarantee does not rest on one self-selected failure case.
