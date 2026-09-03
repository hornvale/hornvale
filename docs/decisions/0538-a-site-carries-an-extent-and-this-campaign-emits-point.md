# 0538. A `Site` carries an `Extent`, and this campaign only ever emits `Point`

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) ·
**Relates:**
[0039](0039-epochs-replace-tiers-refine.md),
[0536](0536-the-enterability-gate-is-a-site-not-built.md),
[0537](0537-a-placed-site-is-re-sited-to-a-facet-by-a-seeded-draw.md),
[0539](0539-a-sites-tier-is-placed-or-derived.md)

In the context of sites becoming a first-class thing consumers read, and of
Nathan's observation that exotic sites are not uniform in scale, we decided
that **`Site` carries an `Extent` from the outset, with `Point` as the only
variant this campaign emits**, accepting one unused variant.

## Context

A cave mouth is a point. A wasteland or a cursed land is miles across and has
components inside it. Modelling every site as a point is right for what The
Prospect ships and wrong for what the model is for.

## Decision

```rust
pub enum Extent {
    Point,                   // one facet — all this campaign emits
    Region { /* reserved */ },
}
```

## The original justification was withdrawn, and the conclusion survived it

The spec argued: *"since §7 mints an epoch anyway, carving the shape out now is
free."*

Decision 0537 establishes that §7 mints **no** epoch, so that argument is void.
It is recorded here rather than quietly dropped, because a conclusion that
outlives its stated reason is exactly the kind of thing a later reader treats
as still-justified.

The conclusion stands on a different and better footing: one enum plus one
match arm, now, against a migration of **every consumer** later — with no
determinism cost whatever, since `Extent` is a presentation-side shape that
consumes no draws. It was cheaper than it looked, not free because something
else was already being paid for.

## Consequences

**A multi-facet site is a fill-in, not a migration.** Every consumer already
matches on `Extent`, so `Region` is a new arm rather than a new parameter
threaded through call sites.

**One variant is deliberately unconstructed.** `Region` is uninhabited until a
campaign builds it, and the reserved body is deliberately empty: naming its
fields now would guess at a geometry nobody has designed. This is the same
posture 0539 takes toward `Tier::Derived`.

**A test asserting "a new site is a point" is vacuous while `Extent` has one
inhabited variant**, and one was written and removed for that reason. What
replaced it is a round-trip plus a stated forward guard — the useful assertion
is that the field is *carried*, not that the only value is the only value.
