# Single-Craton Hypsometry

**July 2026 · outcome: merged — the last `#[ignore]`d terrain test from the
Crust campaign passes for real: a lone continent gets a shelf and a bimodal
hypsometry, and the ocean-fraction pin gains documented "target, not
guarantee" semantics for supply-limited worlds**

## What was attempted

Crust Task 9 fixed continental shelves for the general, multi-craton
population — rescaling drawn craton radii so their total spherical-cap area
matched the land-quota budget — and left one edge case named and deferred:
`a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry`, `#[ignore]`d,
asserting that a one-continent world (`TerrainPins { continents: Some(1),
.. }`) has a bimodal hypsometric distribution and a real continental shelf.
This campaign resolves it, without touching the lobing model, the craton
clamp, or any multi-craton behavior.

## The 8.7% ceiling

Cratons are clamped to 0.6 rad radius — a directed-lobing design constant,
not a knob this campaign was permitted to move. A spherical cap at 0.6 rad
covers `(1 − cos 0.6) / 2 ≈ 8.73%` of the sphere: the hard ceiling on what a
single craton can ever contribute as continental crust, regardless of seed.
Meanwhile sea level is placed by an **exact percentile** — the elevation
value that puts precisely the ocean-fraction-implied land quota (20–50% by
pin) of cells strictly below it. Give a world one craton capped at 8.7% land
and ask the percentile mechanism for 20–50%, and the percentile has nowhere
honest to go: it drops past the craton's edge and into the abyssal plain to
find enough cells to call "land." The result reads as a broad flat expanse
with no shelf and no separation between land and ocean elevations — the
opposite of what a continent looks like, and exactly what the ignored test
was catching. Swept seeds 1..=40 failed 0/40 under the old mechanism; the
craton clamps to exactly 0.6 rad every time, so the failure was not seed
noise, it was geometry.

Two rejected fixes are worth naming because they were the obvious first
moves. Raising the 0.6 rad clamp would let a lone craton grow past 8.7% and
meet any quota — but the clamp exists for the directed-lobing behavior
multi-craton worlds depend on, and special-casing it by continent count
would entangle two systems that have no business knowing about each other.
Weakening the test's bounds would make it pass trivially — but the test
encodes a real property, "a continent has a shelf," and weakening it away
would hide the limitation rather than resolve it.

## The shelf-break fallback

The chosen route treats the ocean-fraction pin as a *target* a
supply-limited world may not reach, rather than a percentile the crust must
be forced to honor. `crust::continental_supply` computes, analytically and
without new stream draws, how much land a world's craton set can actually
supply. `elevation::effective_ocean_target` compares that supply against the
pin's implied land quota: when supply falls below `SUPPLY_SHORTFALL_FACTOR`
(0.5) times the quota, sea level is placed instead at the craton's isostatic
shelf break — `SHELF_BREAK_LAND_FACTOR × supply` land, via the same
exact-percentile mechanism the unconstrained path already uses. This is not
a special-cased shortcut; it is the same physical idea Task 9 already
proved for the general population, applied at the point where supply runs
out instead of past it. The softening is pure arithmetic over
already-derived values — no new draws, so pin isolation and stream-consumption
order are untouched — and it announces itself: a supply-limited world
records a degradation note ("sea level set at the shelf break, ocean-fraction
target unmet") in its genesis notes, so the trade-off is visible in the
world's own record rather than silent.

The gap the `SUPPLY_SHORTFALL_FACTOR` threshold bisects turns out to be
wide and empty, not a knife-edge: default worlds (8–14 cratons) sit at
supply/quota ratios above 0.7, while a lone 0.6 rad-clamped craton sits
below 0.15. A dedicated separation guard
(`default_worlds_never_trip_the_supply_fallback`) proves the fallback cannot
activate on any default draw, which is why this campaign leaves every
committed artifact and the frozen census untouched — confirmed by a clean
`git status` after a full non-census regeneration pass.

## The exact-percentile floor turned out to be measuring the wrong sphere

Fixing sea level placement did not, by itself, make the test's *shelf*
bound satisfiable. The original bound was `shelf_fraction > 0.02` — at
least 2% of the whole sphere's cells within ±200 m of sea level — inherited
unchanged from the multi-craton test it was modeled on. A pre-registered
calibration swept `SHELF_BREAK_LAND_FACTOR` (κ) over `{0.8, 0.9, 1.0, 1.25,
1.5, 2.0}` against that bound across the single-craton seed sweep, and hit
its own STOP rule: no κ cleared the floor. The best case, κ = 2.0, passed
18 of 40 seeds. The reason is geometric, not a tuning miss: a continent
capped at ~8.7% of the sphere's *area* cannot put 2% of the *whole sphere*
within a narrow elevation band around its own shoreline — the absolute
floor was implicitly calibrated against continents several times larger
than the one this test was ever going to produce.

The calibration ran one further measurement before reporting back: the same
sweep, scored against a **land-normalized** shelf ratio (`shape::
shelf_land_ratio`, shelf cells divided by land cells rather than by the
whole sphere) instead of the whole-sphere fraction. At κ = 1.0 — the
untuned, physically literal shelf break, no fudge factor — the
land-normalized ratio spanned 0.075–0.309 across the sweep (median 0.171),
overlapping and at the median *exceeding* the same statistic measured
across the default multi-craton population (0.097–0.165, median 0.130),
with Ashman's D ≥ 3.14 confirming genuine bimodal separation rather than a
washed-out ramp. The single continent's shelf, relative to its own land, was
never thin — the metric asking about it relative to the whole sphere was
asking the wrong question of a small world.

Nathan's ruling: the metric's denominator was wrong, not the world. The
single-craton test now asserts `shelf_land_ratio > 0.05` in place of the
whole-sphere floor, plus the original absolute `shelf_fraction < 0.5`
ceiling retained unchanged — that ceiling still guards the failure mode the
whole campaign exists to catch, a world drowned so deep into the abyssal
plain that "everything is shelf." `SHELF_BREAK_LAND_FACTOR` stays at 1.0,
the physical shelf break, un-tuned by this exercise. This is a
re-normalization backed by measurement, not the "weaken the test" route the
design spec rejected outright: the property under test — a continent has a
shelf — is unchanged and now scales correctly with the continent it is
measuring, rather than with a sphere-sized yardstick a small world was
never going to fill. The whole-sphere `shelf_fraction` metric itself is
untouched, because the frozen census depends on its exact current
definition; `shelf_land_ratio` is a new, unregistered shape function that
exists specifically for this comparison.

## What this leaves in place

Nothing about the 0.6 rad clamp, the lobing model, or multi-craton sea
level moved. The separation guard and the artifact zero-churn check both
hold the same claim from two directions — one analytically (supply ratios
never cross the threshold on default draws), one empirically (a full
non-census artifact regeneration left every committed gallery, reference,
and laboratory file byte-identical). Decision
[0053](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0053-ocean-fraction-is-a-target-under-supply-limited-crust.md)
records both the shelf-break fallback and its land-normalization amendment
in one place; the design spec that set this campaign's scope is marked
Shipped with the amendment noted inline against the criterion it revises.

## The road ahead

The clamp, the lobing redesign, and directed continental shapes generally
still belong to Sculpting — this campaign deliberately left all three
untouched so a small, well-scoped fix would not entangle a much larger
pending redesign. `shelf_land_ratio` is now available to any future
terrain work that wants a shelf metric that scales with its subject rather
than with the sphere it sits on.
