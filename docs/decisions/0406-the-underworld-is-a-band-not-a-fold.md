# 0406. The underworld is a band, not a fold

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0069](0069-fine-position-is-never-serialized.md) (the FRAME
tier the descent stays at) ·
[The Gallery](../../book/src/chronicle/the-gallery.md)

In the context of a possession being able to stand in a generated cave with
no pane of its own, we decided that **`SpatialChannel` gains a fourth
variant, `Underground`, and the pane and the `map` verb move onto it
together** — never separately, and never by widening `Chamber` to cover a
cave that has no chambers.

## Context

`the_underground_band_folds_into_walk_as_map_does` had pinned, deliberately,
that a possession standing in a cave still saw the country overhead: not a
pane bug, since the `map` verb guarded on `inside` alone and answered the
same way. `CLIENT-band-fold` named the gap as a sim question first — what
does a surface chart mean from below — and The Gallery answers it: nothing,
because there is now a real chart to draw instead.

## The rule

`Session.underground` mirrors `Inside` (`descent`, `rung`, `level`, `cell`,
`seed`), FRAME-tier throughout (decision 0069) — the descent is a pure
function of seed and address, nothing here is serialized. The session struct
and the wire variant are named differently on purpose: the precedent
(`Inside` vs `Chamber`) already keeps a session-state type and a channel tag
as different things with different jobs.

## Consequences

- **The old pin is replaced, not deleted** — `the_pane_and_the_verb_agree_
  underground` asserts the same invariant (pane and verb cannot drift apart)
  against the new answer, so a future fifth band still has a guard to
  extend.
- **`submerged` is untouched, and stays folded into `walk` on purpose**
  (`CLIENT-band-fold`, narrowed not closed): the water column has no lattice
  to step across, so routing it onto its own band would need geometry this
  campaign does not build.
- **A `Level` is not a `Lattice` retyped.** `CellKind::Floor(usize)` carries a
  chamber index a cave does not have, and a second shadowcaster would
  duplicate an implementation whose symmetry is property-tested; §3.3
  generalizes the existing shadowcaster over a transparency predicate
  instead.
