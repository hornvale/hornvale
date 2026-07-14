# Single-Craton Hypsometry — Design

**Status:** Draft (queued terrain task; not for immediate execution)
**Date:** 2026-07-13
**Roadmap slot:** terrain overhaul, Sculpting era (after Crust) — the
craton-loop work already flagged in the Crust retrospective.

## Problem

`domains/terrain/src/elevation.rs::a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry`
is `#[ignore]`d. It asserts that a one-continent world (`TerrainPins {
continents: Some(1), .. }`, seed 3) has a bimodal hypsometric distribution
(D > 1.5) and a continental-shelf band (0.02 < shelf < 0.5). It cannot pass
today, for a measured geometric reason (Crust Task 9, "measured not silently
retuned"):

- Cratons are clamped to **0.6 rad** radius (`crust.rs:304`,
  `c.radius_rad = (c.radius_rad * scale).min(0.6)`), a directed-lobing design
  constant. A spherical cap at 0.6 rad covers **(1 − cos 0.6)/2 = 8.73%** of
  the sphere — the hard ceiling on one craton's land area.
- Sea level is set by percentile to hit the ocean-fraction-implied land quota
  (20–50%) **exactly**.
- So with one craton (≤ 8.7% continental crust) and a 20–50% land target, the
  percentile sea level drops into the abyssal plain to find enough "land." The
  result is a broad flat expanse, not a continent with a shelf: no bimodal
  separation, no shelf band. Swept seeds 1..=40 pass 0/40; the craton clamps
  to exactly 0.6 rad every time.

Neither knob authorized in Task 9 (budget range, `PEAK_MIN`) touches cap
*area*, so the fix was out of that task's scope and correctly deferred here.

## Goal

Make single-continent worlds produce a real continent — a bimodal hypsometry
with a shelf — so the test passes **un-`#[ignore]`d**, without changing
multi-craton worlds and without a broad craton/lobing redesign (that is
Sculpting's larger scope).

## Approaches

1. **Set sea level from the continental extent, not a global percentile,
   when land supply is capped below the quota (recommended).** When the
   available continental crust cannot meet the ocean-fraction land quota,
   fall back to placing sea level at the craton's isostatic shelf break
   rather than forcing the quota by drowning into the abyssal plain. This
   makes the ~8.7% craton read as a bimodal continent-plus-ocean, which is
   the physically correct outcome for a small-continent world. The
   ocean-fraction pin becomes a *target* that a supply-limited world may not
   reach, rather than a hard percentile — a principled softening.
2. **Raise the 0.6 rad clamp for the single-craton case.** Let one pinned
   craton grow past 8.7% so it can meet the quota. Rejected as the primary
   route: 0.6 rad exists for the directed-lobing behavior, and special-casing
   the clamp by continent count entangles two systems.
3. **Weaken the test's expectation for the edge case.** Rejected: the test
   encodes a real property (a continent has a shelf); weakening it hides the
   limitation rather than resolving it.

Route 1 is the recommendation; the exact shelf-break criterion is the design
work this spec hands to the implementer.

## Determinism and scope (must be settled first)

- **Which worlds change?** `continents` is a pin. Determine whether the
  *default* roster / any committed artifact builds with `continents: Some(1)`
  (or a default that resolves to a single craton). If nothing shipped uses a
  single craton, only explicitly-pinned single-craton worlds change and the
  artifact churn is nil. If a default path can yield one craton, the change
  touches generated worlds → the drift check fires and the seed-42 artifacts
  + censuses must be regenerated (`make rebaseline`, on the cloud).
- **Multi-craton must stay byte-identical.** Route 1 only activates when land
  supply < quota; assert that the ≥2-craton swept scenario (seeds 1..=40) is
  unchanged, ideally byte-for-byte.
- **Coarse-constrains-fine.** Whatever sea-level rule is chosen must remain a
  deterministic function of the seed and pins, quantized at the emit boundary
  as today; no new chaotic state.

## Success criteria

1. `a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry` passes with
   the `#[ignore]` removed.
2. A swept single-craton scenario (seeds 1..=40, mirroring the existing
   multi-craton sweep) passes, not just seed 3 — the fix is general, not
   tuned to one seed.
3. Multi-craton worlds unchanged (byte-identical where measured).
4. If any committed artifact changes, it is regenerated and the drift check
   is green; the change is recorded in the terrain chronicle.
5. The ocean-fraction pin's new "target, not guarantee" semantics for
   supply-limited worlds is documented (module doc + a decision note if it
   rises to a contract change).

## Out of scope

- Any change to the 0.6 rad clamp or the lobing model (Sculpting).
- Multi-craton sea-level behavior.
- The fast-gate-tiers work — this spec exists because that change surfaced
  the ignored test; the two are independent.
