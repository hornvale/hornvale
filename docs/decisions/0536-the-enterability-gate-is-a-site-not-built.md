# 0536. The enterability gate is a `Site`, and `built` narrows to one kind of it

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) ·
**Relates:**
[0022](0022-sim-emits-data-clients-render.md),
[0025](0025-one-concept-name-one-owner.md),
[0537](0537-a-placed-site-is-re-sited-to-a-facet-by-a-seeded-draw.md),
[0538](0538-a-site-carries-an-extent-and-this-campaign-emits-point.md),
[0539](0539-a-sites-tier-is-placed-or-derived.md)

In the context of The Prospect making caves and exotic sites enterable, and
faced with a gate named `built` that every enterable place hung off, we decided
that **the gate becomes an `Option<Site>` and `built` narrows to a property of
one site kind**, accepting that one surviving use of `built` is deliberately
*not* migrated.

## Context

`Brief::built` was documented as *"whether a **structure stands here**."* That
is "made by hands" — false for a cave, which water dissolved, and false for a
fungal canopy, which grew. Widening `built` to admit them would have put a lie
in the predicate the whole enterability path reads.

The distinction is not pedantry: two different generators hang off it. A
settlement gets rectilinear rooms; a cave gets a grown blob.

## Decision

`Brief` carries `site: Option<Site>`, and that field is the enterability gate:

```rust
// windows/vessel/src/structure.rs
brief.site.as_ref()?;          // was: if !brief.built { return None; }
```

`Terrain::is_built` survives unchanged as what makes a `Settlement` a
settlement. The word "site" is the project's own: `plate.rs` already said "an
undiscovered site", the CLI already said "placed exotic sites".

**The spec proposed a `site_at(&Facet) -> Option<Site>` function; that is not
what shipped.** `Brief` already resolves per-facet and already carried `built`,
so the site rides on the brief instead, and no new resolution path was
introduced. The record describes the code, not the proposal.

## The use of `built` that deliberately survives

`lattice::embed_with` still reads `brief.built`, and a reviewer reading this
decision without the code would "fix" it:

```rust
// This is a GENERATOR DISPATCH, not the enterability gate: it asks
// "constructed or natural" to choose between rectilinear rooms and a grown
// blob, which is exactly what `built` means. A cave has a site but was never
// built, and must still `grow`; reading this as the gate and rewriting it to
// the site would generate every cave as a rectilinear building.
```

This is the decision's whole point stated in the negative. `built` was
overloaded to mean two things — *is there anything here* and *was it
constructed* — and the fix separates them rather than renaming one. The gate
moves to `site`; the constructed/natural question keeps `built`, correctly.

## Consequences

**A predicate that had one honest meaning and one dishonest one now has one.**
`built` is true only where a structure was in fact built.

**The gate is extensible without touching the predicate.** Admitting a fourth
site kind adds a `SiteKind` variant, not a widening of a boolean whose name
would then be wrong again. That is the failure this decision exists to prevent
recurring.

**One-concept-one-owner (0025) is preserved.** "Built" belongs to settlements;
"site" belongs to the enterability question. Neither borrows the other's word.
