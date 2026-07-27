# The Hoist — design

**Campaign:** The Hoist
**Date:** 2026-07-27
**Status:** SHIPPED — see [The Hoist](../../../book/src/chronicle/the-hoist.md)
and the plan's Measured section (`docs/superpowers/plans/2026-07-27-the-hoist.md`).
Result: **-24.3%** on the census probe, byte-identical; `terrain_of` gone from
the census profile entirely. Predicted 15-20%; the excess was a third
double-sculpt site (`history_for`) found by the call-site audit.
**Predecessor:** The Local Census, which shipped the consumer side of the
Single Sculpt idiom and explicitly deferred this piece ("Fix D", the
view-chain double sculpt, "changes `build_world_to`'s signature") to a
genesis-perf campaign.

## 1. The problem, measured

`windows/worldgen/src/lib.rs::build_to` sculpts terrain exactly once and
keeps it, threading that one value through the climate, settlement, and
history stages — the Single Sculpt discipline, already applied internally
and documented in the source. At Full depth it likewise builds climate once
and threads it.

Then it returns `World` and **drops both**.

One stack frame above, the lab's view chain rebuilds what was just
discarded:

- `windows/lab/src/metrics.rs::TerrainView::build_to` calls
  `terrain_of(&astronomy.world)` — a second full sculpt.
- `ClimateView::build_to` calls `climate_from(..)` off that second terrain —
  a second climate build.

`the-census` selects all metrics, and the runner builds each study to its
deepest metric's rung, so **every census row is a Full build and pays both
duplicates**.

Measured on lefford (1000-world census, frame-pointer profile, 761,726
stacks):

```
30.90%  hornvale_terrain::globe::generate   (any path)
15.16%    via worldgen::terrain_of            <- the duplicate
14.86%    via worldgen::build_world_to        <- the original
 0.87%    other
10.41%  hornvale_climate::*
```

The near-perfect 50/50 split is the double sculpt made visible, and the
15.16% is real executing work, not a cache hit.

## 2. Non-goals

- **`terrain_of` is not being removed, and re-derivation is not being
  weakened.** This is the constitutional claim of the architecture — "a
  world is a seed plus a ledger; everything else is re-derived
  deterministically" — made executable. The CLI, the almanac, and any
  consumer holding a *loaded* world legitimately have no build in hand and
  must re-derive. This campaign adds a fast path **alongside**
  re-derivation, never a replacement for it.
- **Terrain is not stored in the `World`.** That would contradict
  seed-plus-ledger directly.
- **No new seeded draws, no epoch, no save-format change.** This campaign
  moves no physics; it stops recomputing physics that was already computed.
- **No split of `lib.rs`.** It is ~6k lines and merge-hot from parallel
  sessions (`windows/worldgen/CLAUDE.md`); this campaign adds one function
  and edits none of the existing ones beyond a thin delegation.

## 3. Design

### 3.1 The producer side (worldgen)

Add an artifacts-returning sibling. Shape:

```rust
/// What a build produced beyond the ledger — the values a consumer would
/// otherwise re-derive. Each is `Some` exactly when the requested depth
/// built it; `None` means "this rung never produced that artifact", NOT
/// "rebuild it".
pub struct BuildArtifacts {
    pub world: World,
    pub terrain: Option<GeneratedTerrain>,   // Some iff depth >= Terrain
    pub climate: Option<GeneratedClimate>,   // Some iff depth >= Settlements
}

pub fn build_world_to_with_artifacts(
    /* same parameters as build_world_to */
) -> Result<BuildArtifacts, BuildError>;
```

`build_world_to` keeps its exact current signature and becomes a thin
wrapper returning `.world`, so **all ~18 existing call sites are untouched**
and the merge-hot file takes one additive function rather than a signature
edit across the tree.

Naming follows the repo's `*_from` / `*_with_components` sibling convention;
the exact final name is a spec-level detail settled here as
`build_world_to_with_artifacts` unless review prefers otherwise.

### 3.2 The consumer side (lab view chain)

The view rungs are layered — `TerrainView::build_to` calls
`AstronomyView::build_to`, which is where the world build happens — so the
artifacts must reach the rung that wants them. Introduce a private helper in
`metrics.rs` that performs the single build and hands each rung its
artifact, replacing:

- `terrain_of(&astronomy.world)` → the hoisted `artifacts.terrain`
- `climate_from(&world, &terrain)` → the hoisted `artifacts.climate`

**Fallback rule:** when an artifact is `None` (the rung genuinely did not
build it), the consumer falls back to today's derivation exactly. This makes
the change strictly not-slower and byte-identical on every path, and it
keeps the depth ladder honest rather than forcing a deeper build to obtain
an artifact.

## 4. Why this is byte-identical, and how it is verified

The argument is from construction, and it has one real dependency that was
checked rather than assumed.

`build_to` sculpts from the `terrain_pins` **argument**. `terrain_of`
reconstructs pins by **parsing `TERRAIN_PIN` facts back out of the ledger**.
The two terrains are identical only if that round trip is exact identity.

Verified: `domains/terrain/src/pins.rs::pin_strings_round_trip_through_parse_pin`
asserts `parse_pin(pin_strings(p)) == p` over a fully-populated pin set
including the `f64` `ocean_fraction`, and passes:

```
$ cargo test -p hornvale-terrain --lib pins::
test pins::tests::pin_strings_round_trip_through_parse_pin ... ok
test pins::tests::default_pins_pin_nothing ... ok
test result: ok. 7 passed; 0 failed
```

For the census the argument is stronger still: it builds with
`TerrainPins::default()`, and `default_pins_pin_nothing` confirms default
pins emit **no** facts, so `terrain_of` parses nothing and reconstructs the
same defaults. The census path cannot diverge.

Verification plan, in order:

1. **Equivalence test (new).** Assert the hoisted artifact equals the
   re-derived one. Neither `GeneratedTerrain` nor `GeneratedClimate` derives
   `PartialEq` (both are `Debug, Clone` only), so the test compares
   metric-visible projections — the elevation/water `CellMap`s (`CellMap`
   *does* derive `PartialEq`) and the `GlobeSummary` — rather than the whole
   struct. Deriving `PartialEq` on the providers is an alternative;
   flagged in §6 as a call for review.
2. **Probe A/B.** 200 seeds x all metrics, three alternating rounds, timings
   serial; `rows.csv` must be byte-identical. This harness exists and was
   used for both of today's landed perf changes.
3. **`depth.rs` must stay green unmodified.** It asserts `build_world_to`'s
   depth prefix property and is the ladder's guard.
4. **`lab`'s `depth_ladder` test must stay green** — depth-scoped metrics
   must still equal full-build values.
5. **Full `HV_CENSUS=1` regen producing a ZERO diff** across every committed
   artifact, as the final gate (owner-authorized; see §6).

## 5. Expected win

Hypothesis, not a promise: eliminating the second sculpt is worth ~15% of
the census, and eliminating the second climate build some part of the
10.41% climate share. The honest prediction is **15-20%**, to be replaced by
a measurement before close. Today's two landed changes both came in away
from their predicted size (one 3x larger, one ~60x smaller), so this number
is treated as a hypothesis to be falsified.

## 6. Flagged for review (G3)

- **Census regen is a carve-out** and needs explicit authorization. This
  campaign should not need one — no physics moves, so a correct
  implementation produces a zero diff — but the zero-diff regen IS the final
  proof, so the authorization is for the *verification*, not for a
  re-pinning.
- **`PartialEq` on `GeneratedTerrain` / `GeneratedClimate`?** Deriving it
  would make the equivalence test a one-liner and is arguably correct for
  value-like providers, but it widens a public API surface and invites
  comparison of large structures in hot paths. The spec's default is to
  compare projections instead and NOT derive it. Reviewer's call.
- **Non-default terrain pins are the only divergence risk**, and only if the
  pin round trip is ever made lossy in future. Recommend the equivalence
  test run over a pinned world too, not just the default, so the guard fails
  loudly if that ever changes.
- **`lib.rs` is merge-hot.** This campaign should absorb main at every plan
  stage boundary per the root `CLAUDE.md`, and keep its footprint in that
  file to the one new function plus the one-line delegation.

## 7. Acceptance criteria

- `build_world_to`'s signature is unchanged and its ~18 call sites are
  untouched.
- The lab view chain performs exactly one terrain sculpt and one climate
  build per row at Full depth, demonstrated by profile (the `terrain_of`
  path disappears from the census flamegraph).
- `rows.csv` byte-identical on the probe A/B; full census regen zero-diff.
- `make gate` green; `depth.rs` and `depth_ladder` green and unmodified.
- A measured speedup, reported honestly whatever it turns out to be.
