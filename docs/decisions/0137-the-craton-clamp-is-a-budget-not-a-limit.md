# 0137. The craton clamp is a budget, not a limit

**Status:** Accepted (2026-08-14) · **Decider:** Nathan · **Relates:**
[0053](0053-ocean-fraction-is-a-target-under-supply-limited-crust.md),
[0057](0057-banked-coastal-mechanisms-activate-on-measured-demand.md),
[0106](0106-a-constants-justification-must-match-its-kind.md)

In the context of the craton rescale under-delivering its own continental
budget by ~34% — which cut every world's coastline ~1.1 km below the isostatic
shelf break and made all land read high, the measured root cause of the
census's 65%-ice population — we decided to **solve the rescale exactly, raise
`CRATON_RADIUS_MAX_RAD` from 0.6 to 0.8, enforce `repel_cratons`'s
already-documented monotonicity guarantee, and repair `assemble_cratons` so
it attains contact**, because the first two alone trade the defect for a
worse one, and the last two turned out to be repairing pre-existing bugs
rather than accommodating new ones.

## Context

**The rescale missed twice, for two independent reasons.** It matched a
craton set's continental cap area to a budget of `(1 − ocean_target) ×
(1 + margin)` using the closed form `s = sqrt(target / current)`, which is
exact only if cap area scaled as `r²`. It scales as `2π(1 − cos r)` —
sub-quadratic at these radii — so the closed form systematically
under-delivers. It then applied the radius clamp *after* solving, silently
discarding area the solve had counted on. Measured over 200 default seeds,
realised supply was 0.2681 against a budget near 0.4123; on seed 0 the
shortfall was 40.6%.

**But solving it exactly at the old clamp is worse than the defect.** Over
20,000 simulated draws plus a 4,000-point sphere overlap Monte Carlo, the
budget is unreachable under a 0.6 clamp on **97.1% of worlds**, so an exact
solve there can only pin every craton at the clamp: radius CV collapses from
0.227 to **0.0013** — a world of identical continents. That is why this
decision is about the clamp and not only about the solver.

**The clamp sweep says 0.8 is the safest value, not the boldest.** Measured
over 200 default seeds with the exact solve:

```
config                     supply   worse/200  worst_ratio  coincident  radiusCV
SHIPPED (0.6, closed form) 0.2681     35/200      0.0613       0/200      0.2286
exact solve @ 0.60         0.3496     88/200      0.0000       6/200      0.0013
exact solve @ 0.65         0.3970    148/200      0.0000      47/200      0.0385
exact solve @ 0.70         0.4078     94/200      0.0000       3/200      0.1511
exact solve @ 0.75         0.4079     65/200      0.0000       2/200      0.2273
exact solve @ 0.80         0.4079     55/200      0.0000       1/200      0.2765
```

Supply **saturates at 0.70** — everything above it buys geometry, not land —
while every pathology count falls monotonically from 0.65 to 0.80 and variety
rises. Overlap-deducted continental area saturates by ~0.8 for the same
reason: added radius lands on ground another craton already covers. There is
no case for retreating to a lower clamp; the lower values are where the
trouble lives.

**The pathology those columns count was already there.** `repel_cratons`'s
doc comment has claimed since Task 9 iteration 2 that "the minimum pairwise
separation across the whole set never gets worse than it was before the
pass," and `repulsion_reduces_crowding_without_new_draws` has asserted it.
Nothing implemented it. The extrapolative slerp that clears craton `i` of
craton `j` can drop it onto a third craton, and the pass had no way to
notice. On **main's shipped code** — 0.6 clamp, closed-form solve, the
configuration every committed artifact was generated under — the pass reduces
the global minimum on **35 of 200 seeds**, worst post/pre ratio 0.0613. The
test only sampled seeds 0..8, all of which happened to hold.

Delivering the real budget crowds the sphere harder and made it worse (55/200,
and one world driven to exactly coincident craton centres), which is what
surfaced the bug — but coincident centres appear at *every* clamp value under
the exact solve, including 0.60. The cause is the solve, not the clamp:
`repel_cratons` was calibrated against a pipeline under-delivering by 34%.

## The decision

**1. `solve_radius_scale` replaces the closed form.** A deterministic
bisection on the real objective, with `CRATON_RADIUS_MAX_RAD` applied *inside*
it so clamped cratons do not discard area the solve is counting on. Fixed
80 iterations, no tolerance-based early exit, no platform-dependent
behaviour; the objective is pure and never mutates a radius, or the bisection
would read its own side effects. Where the target is unreachable at any scale
the bracket search stops at 1024 and every radius pins at the clamp — the most
area the set can deliver.

**2. `CRATON_RADIUS_MAX_RAD` = 0.8**, kind **hornvale-choice** (0106). Not a
geometric limit and not a measured one: a bound on how much of one world a
single craton may be. This decision does not supersede 0053, which is about
where sea level may land, not how big a craton may be.

**3. `repel_cratons` gets a monotonicity guard.** Every candidate move is
applied, the global minimum pairwise separation is recomputed, and the move is
**rolled back unless the minimum is at least as large as before**. The running
floor is therefore always the set's true minimum and is non-decreasing by
construction. This makes the property already claimed true rather than
inventing a new one, and it repairs the pre-existing 35/200 as well as the new
1/200: measured with the guard, **0/200 worse and 0/200 coincident at clamp
0.8, and 0/200 worse on main's own shipped 0.6 configuration.**

Deliberately *not* done, and each for a reason: no extra stream draws
(repulsion stays draw-free, so pin isolation and stream order are untouched);
still one pass in id order; **no iteration-to-convergence and no retune of
`REPEL_SEPARATION_FACTOR`** — that is the radius-aware re-calibration this
decision declines, because the guard makes it unnecessary. On a world too
crowded to improve, every move is rejected and the pass is a no-op; that is
the correct outcome, not a failure, and there is no fallback that forces a
move.

**4. `assemble_cratons` attains contact.** The supercontinent assembly's
outward branch searched an *unbounded extrapolation multiplier* along the
great circle from the anchor. `slerp` evaluates `sin(t · omega)`, so at the
search's 2^33 cap the sine argument reached ~6.7e9 radians, where its own
ULP is ~1e-6: `assembly_slack` degraded into a staircase and the bisection
converged to a step edge, returning cratons floating clear by ~1.4e-7 rad
while reporting success. It was also searching a space that does not exist —
no point on a sphere is more than π from another.

The repair moves the search to the surface the answer lives on. For each
placed craton, the points at exactly `contact_separation` from it form a
circle on which **every point attains contact by construction**; each circle
is scanned by azimuth outward from the bearing of the craton's drawn
position, and the nearest clear tangency across all hosts wins. There is
nothing to converge to, so no iteration budget can be wrong.

Three simpler repairs were tried and measured first, and each failed
structurally rather than for want of budget — recorded because each looks
correct until measured:

- **Bisecting the arc angle** (bounded, so the precision bug is gone) still
  assumes a single sign change and silently requires the antipode to be
  clear; when it was not, cratons were buried 0.05–0.6 rad deep.
- **Relaxation** — push to exact contact with the worst violator, repeat —
  **limit-cycles**: caps of 16, 64, 256, 1024 and 4096 sweeps all leave
  craton 7 of the seed-42 assembly overlapping, alternating between two
  violators forever.
- **A bracketing scan of the whole arc** reported honestly that it found no
  clear sample, which is what identified the one-dimensional search space —
  not the search — as the defect.

## Consequences

- **This is a byte-identity epoch for the whole terrain pipeline.** New
  coastlines, new biomes, new elevation maps, new census. Every committed
  artifact downstream of terrain moves.
- **The coastline lands where it was supposed to.** Measured over the probe's
  12 census seeds: the grid area clearing `CONTINENTAL_THRESHOLD_KM` rises
  0.2724 → **0.3836** against a land quota of 0.3775 — **1.016 × quota**,
  against 0.730 before, so the coastline now sits *at or above* the shelf
  break rather than 1113 m below it (sea level clears it by 158 m). Mean land
  elevation falls 2257.21 m → **1783.37 m**. The residual is the greenhouse's
  to carry, and spec §4.1's bound already says so.
- **Variety improved rather than collapsed.** Radius CV 0.2428 → **0.2837**
  measured on the grid; cratons at the clamp 50.8% → 34.1%.
- **`SUPPLY_SHORTFALL_FACTOR` (0053) becomes a less marginal choice**, not a
  more marginal one. The default-draw supply/quota floor over the frozen
  1000-seed census rises ≈ 0.554 → **1.050**, and the single-craton ceiling
  ≈ 0.18 → **0.25**; 0.5 still bisects empty space, now with a wider margin.
  `default_worlds_never_trip_the_supply_fallback` holds more comfortably than
  before.
- **Every craton now touches; a minority still interpenetrate, and that is
  geometric, not algorithmic.** Over 40 pinned seeds, floating cratons go
  **31/400 → 0/400** (they were 105/400 with the clamp raised and the
  assembly unrepaired). Overlaps do not go to zero, and cannot: a contact
  separation reaches `CONTACT_FACTOR × 1.6 ≈ 1.36` rad, so one craton's
  forbidden cap covers ~31% of the sphere and eight cannot avoid covering all
  of it. An independent 200,000-point sphere sample — asking whether *any*
  position clears every placed craton, with no reference to how the assembly
  searches — finds **55 of 400** cratons here with no clear position
  anywhere, against **6 of 400** on shipped `main`. The repaired search's
  fallbacks are exactly those 55, which is what establishes it as optimal
  rather than merely better. For them it takes the least-overlapping
  tangency, so contact still holds and the violation is minimised; the
  realised overlap total is 67/400 (against 9/400 on `main`).
  `pinned_supercontinent_is_sutured` therefore asserts contact **strictly**
  and ratchets overlap at its measured floor. Lowering that floor means
  moving `CONTACT_FACTOR` or the clamp — a decision 0057 question this
  campaign deliberately did not open.
- **The suture test now sweeps 40 seeds, not one.** Seed 42 alone is what hid
  the defect through every campaign that touched the assembly, exactly as
  seeds 0..8 hid the repulsion defect. One seed is not a property.
- **Three tests changed with recorded reasons**, all in the commit that lands
  this. `continental_supply_is_the_area_the_rescale_budgets`' single-craton
  bound (0.037 → 0.064, recomputed from the new clamp with its comment
  rewritten). `the_wedge_builds_a_shelf_mode_wide_on_passive_margins`' pinned
  seed (42 → 12), a **post-unblinding re-pin** declared as decision 0016
  requires, with the full before/after survey in its comment — the whole
  population's tied blocks shrank (max 148 → 40), so its floor of 25 is a
  materially tighter fit than before. And `trim_recaps_hold_after_the_final_solve`'s
  barrier assert, which compared a metre-scale elevation against an
  order-statistic sea level with a bare `>=` while the asserts on either side
  of it carried 1e-6; it failed by 3e-13 m on seed 34, and now carries the
  same 1e-6 — an asymmetry corrected, not a standard relaxed.

## See also

`domains/terrain/src/crust.rs` (`solve_radius_scale`, `craton_budget`,
`CRATON_RADIUS_MAX_RAD`, `repel_cratons`'s guard and `min_pairwise_separation`,
`settle_against_a_host` and `tangent_toward`),
`domains/terrain/src/elevation.rs` (`SUPPLY_SHORTFALL_FACTOR`'s re-measured
endpoints), `docs/audits/land-elevation-attribution.md` §5 Route 3 (the
standing case that opened this), `docs/superpowers/plans/2026-08-13-the-glasshouse-b-recentring.md`
Task 2.
