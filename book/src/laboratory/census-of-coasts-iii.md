# The Census of Coasts III

**Probe verdicts pending canonical.** This page records Sculpting's tuning
season on **100-seed tuning-probe medians** (`studies/sculpting-probe.study.json`,
default pins, canonical globe level, release build) — the same fast
instrument Crust's iterations were tuned against. It is **not** the
2,000-seed-or-larger canonical census verdict: per decision 0046, censuses
regenerate only on the AWS box, once, at campaign close (never locally), so
every table below is honestly a probe reading until that regeneration lands.
Cells that the canonical run may move are marked **(probe)**; the regen
commit that follows this page's own commit is the one that resolves them to
canonical medians and, if any verdict flips, records that here as its own
supersession note.

## The acceptance bands (spec §8)

Preregistered before any generator code landed; judged on v3@L6 medians;
undershoot and overshoot both fail, same discipline as Census II:

```
metric                    band              status (from Crust's v2@L6)
------------------------  ----------------  --------------------------------
shoreline-development     9.51 - 21.95      must come inside (from 6.785)
shelf-fraction             0.08 - 0.22      must come inside (from 0.0483)
hypsometric-bimodality     2.0 - 8.0        must stay inside
continent-count            3 - 12           must stay inside
largest-continent-share    0.25 - 0.65      must stay inside
plate-size-gini            0.45 - 0.75      must stay inside
```

## v2 → v3 median movement (probe)

| metric | v2@L6 canonical (2,000 seeds, Census II) | v3 baseline (probe, 100 seeds, untuned carve) | v3 final (probe, 100 seeds, iteration 4) | band | verdict (probe) |
|---|---:|---:|---:|---|---|
| shoreline-development | 6.785 | 6.9255 | **7.3202** | 9.51 – 21.95 | **outside, low** — still short of the floor |
| hypsometric-bimodality | 3.282 | 3.2955 | 3.1520 | 2.0 – 8.0 | **inside** |
| shelf-fraction | 0.0483 | 0.0680 | **0.0916** | 0.08 – 0.22 | **inside** |
| continent-count | 8.0 | 8 | 8 | 3 – 12 | **inside** |
| largest-continent-share | 0.304 | 0.2847 | 0.2823 | 0.25 – 0.65 | **inside** |
| plate-size-gini | 0.707 | 0.7061 | 0.7061 | 0.45 – 0.75 | **inside** |

**5 of 6 inside on the tuning probe.** The v3 baseline column is the carve
wired in with every tuning constant still at its Task-13 default — before
any of the season's four iterations touched a knob — and it already reads
higher than v2's canonical median on both movers, consistent with the
decorations and carve adding real texture even unturned. **TO BE CONFIRMED
at close**: the 2,000-seed AWS regeneration is the canonical verdict; a
100-seed probe's medians have historically tracked the larger runs closely
(Census II's own resolution section measured 2k-vs-10k sampling noise as
far smaller than any verdict's margin), but shoreline-development sits close
enough to open questions of its own (see below) that the regen commit should
be read as the authoritative table, not this one.

## Supersession note: shoreline-development's band, and the anchor contamination

Recorded openly, per the preregistration discipline Census II established.
**Verdict: open, handed to the rift-and-fit fake-history coda.** Shoreline-development moved a real,
positive, measured +8.85% across the tuning season's four iterations
(6.7250 → 7.3202, iteration 4 alone) atop a real, if smaller, movement from
Crust's shipped v2 median (6.785 → v3's untuned 6.9255) — every mechanism the
spec banked for this band (wave-cut erosion, the sediment wedge, deltas,
atolls, and, added mid-season, barrier islands and lagoons) is now built and
firing on the large majority of default-pin worlds. It still lands below the
9.51 floor.

A dedicated instrument (`.superpowers/sdd/shoreline-diagnostic.md`) resolved
the load-bearing question this leaves — is the floor reachable at all at
this mesh resolution — with two independent findings:

1. **The estimator is not saturated.** `shoreline-development = L / (2√(πA))`
   is a whole-globe, not per-landmass, statistic that rewards perimeter edges
   almost independent of shape smoothness. An analytic isolated-single-cell
   supremum and an empirical parity-crenulation experiment over a real
   seed's coastal fringe both cleared the *entire* 9.51–21.95 band by
   2.9×–3.5×, using pathological single-hex-scale alternation over a
   minority of the globe. There is ample formula headroom above the current
   median; getting there needs texture at roughly single-cell scale almost
   everywhere along every coast, not the smoother multi-cell embayment
   widening the carve's processes (wave-cut, wedge fill, barriers) produce.
2. **The floor's own anchor is partly contaminated.** The band was set at
   1.3×–3.0× the v1@L6 interim median (7.315138). That interim generator
   predates Crust entirely and its coastline complexity came in significant
   part from a **fragment-swarm boundary-noise generator that Crust
   deliberately removed** as fragmentation, not texture (continent-count
   collapsed from an unfloored 29–54 range down to 8 specifically by
   suppressing that swarm). The formula and the mesh level are unchanged
   since the anchor was set — confirmed by `git log -p --follow` showing no
   definition drift — but the *generator* producing the coastline the
   formula measures is a different one. The anchor and the current
   generator's coastline-production mechanism are measuring different
   things even though the arithmetic is identical.

**Nathan's ruling (2026-07-16, tuning-season close, ledger #11): open
verdict, handed to the rift-and-fit coda.** No new floor is invented here — that is not
this campaign's call to make unilaterally, per the preregistration
discipline. The band stays recorded as outside, low, exactly as measured;
this note is the formal record of the contamination finding; and the
**rift-and-fit fake-history coda** (the banked coda deferred after Sculpting) now
explicitly inherits the open band as part of its evidence baseline — the
same handoff shape Crust used one epoch ago, hydraulics reversed: Crust
handed Sculpting two bands its own knobs could not reach; Sculpting hands
the rift-and-fit coda one band its own knobs (now fully built, all of them) could not
reach either, alongside the diagnostic showing why the floor itself may need
re-deriving before a fifth mechanism is attempted.

## The reroute story — engine A's self-consistency verdict

The escalation criterion (spec §8) measures the **rerouted-flow fraction**:
of each world's twenty largest rivers, the share whose post-carve drainage
path diverges from its pre-carve path. Thresholds: **< 0.10** self-consistent
(ship engine A alone); **0.10 – 0.30** flag, Nathan decides whether engine B
enters evaluation; **> 0.30** engine A rejected outright, B enters.

| stage | reroute median | seeds ≥ 0.10 | seeds ≥ 0.30 |
|---|---:|---:|---:|
| baseline (untuned carve) | 0.0866 | 27% | 0% |
| iteration 1 (land-only slope, blocked) | 0.0607 | 0% | 0% |
| iteration 2 (+ wave-cut) | 0.0806 | 14% | 0% |
| iteration 3 (+ relief frequency) | 0.0834 | 20% | 0% |
| iteration 4 (+ barriers, final) | **0.0834** | 20% | 0% |

**A stands; B never entered.** The season closed at a reroute median of
**0.0834**, comfortably under the 0.10 line, with zero seeds ever crossing
0.30 across every iteration measured. Barriers touch only already-ocean
cells and never move the fluvial network, which is why iteration 4 left the
reroute number unchanged from iteration 3. The two flagged seeds (99 and
1234) that sat inside the 0.10–0.30 band at Task 13's earlier L6 sweep never
pushed the *probe median* itself over the governing line, so per Nathan's
Task-13 ruling (ledger #5b: "B enters only if the 100-seed probe median
crosses 0.10"), engine B was never entered into evaluation. Engines B and C
remain a designed, unbuilt seam.

## The tuning-iteration table

| # | knob / mechanism | shoreline | shelf | reroute (median) | single-craton floor (median / min) | verdict |
|---|---|---:|---:|---:|---:|---|
| baseline | untuned carve wired in (Task 13's shipped defaults) | 6.9255 | 0.0680 | 0.0866 | 0.2557 / 0.1124 | 4/6 inside; shoreline + shelf outside, low |
| 1 (blocked, not shipped standalone) | incision slope restricted to land neighbors only (ledger #6) | 6.6633 (further out) | 0.0582 (further out) | 0.0607 | 0.1698 / 0.0130 | **BLOCKED** — Nathan-ruled single-craton floor breaks (4/40 seeds below 0.05) |
| 2 (shipped, 0dafafa) | ledger #6 kept + wave-cut coastal erosion activated (ledger #7), `wave_cut_m` = 1000 | 6.7055 | **0.0893** (inside) | 0.0806 | 0.3357 / 0.1667 | 5/6 inside; shelf-fraction now inside; shoreline still open |
| 3 (shipped, f337307) | `RELIEF_FREQUENCY` 48 → 8 (T5 sub-Nyquist aliasing fix) | 6.7250 (+0.02, stop condition) | 0.0894 | 0.0834 | 0.3381 / 0.1744 | 5/6 inside; fix correct on its own merits, shoreline barely moved |
| 4 (shipped, 04730e9, final) | barrier islands + lagoons (spec's last banked coastal mechanism); `barrier_supply_per_cell` = 400.0, `barrier_height_m` = 3.0 | **7.3202** (+0.60, season's best mover) | 0.0916 | 0.0834 | **0.4628 / 0.1905** (season best) | 5/6 inside; shoreline improved but band NOT reached — stop condition, band-supersession flagged |

Season total movement: shoreline-development 6.785 (v2 canonical) →
6.9255 (v3 untuned baseline) → **7.3202** (v3 final) — roughly +8% across
the tuning season alone, +11% total since Crust. Every stayer band (bimodality,
continent-count, largest-share, gini) never wavered outside its band across
any iteration.

## Magnitude notes for the two mid-season mechanism activations

- **`wave_cut_m` = 1000** (default; iteration 2). Not the coordinator's
  initial guess of 60 — that value left the single-craton battery red
  (worst seed 9 at 0.0256, below the 0.05 floor) because a ≤150 m cut
  (60 × 2.5 peak erodibility) cannot reach L4 craton-rim cliffs the old
  any-neighbor slope term had been cutting up to `max_incision_m` = 900 m.
  A direct probe swept candidate values through the incision→repose→wave
  prefix on the eight worst single-craton sweep seeds: the binding seed (36)
  needed ~1000 (600 left it at 0.035, still below 0.05). The field is
  documented in code as an *epoch's planation capacity* at ~10²-km cell
  scale, not a literal wave height — a sheltered hard rim still cuts only
  ~40 m while an exposed soft coast planes fully to the platform.
- **Barrier parameters** (iteration 4). `barrier_supply_per_cell` was swept
  at 50.0, 400.0 (shipped), and 5000.0: 50.0 and 400.0 tied *exactly*
  (shoreline 7.3202, shelf 0.0916, sediment-volume 1,235,516 identical to
  four significant figures) — at this geometry, the candidate-rule and
  alternating-spacing pool bind before the supply budget does — while
  5000.0 visibly gated barriers back down toward iteration 3's un-barriered
  6.9227, confirming the supply gate is real and does bind once cost is
  high enough. 400.0 shipped as the more physically legible of the two
  non-binding settings, with the sweep demonstrating real headroom above
  it. `barrier_height_m` (3.0) was not swept — it affects only raised
  volume and the mass-balance books, never the perimeter term the estimator
  reads.

## Shelf-width asymmetry: tail dominance, not the median

Per Nathan's Task-13 ruling (ledger #5d), the shelf-width asymmetry battery
is judged on **tail dominance** (passive-margin cells sitting at the 8-hop
BFS reach cap occur at ≥ 1.5× the rate of active-margin cells at the same
cap), not the median width, which ties at 1.0/1.0 (baseline) or 2.0/2.0
(from iteration 2 onward) across the whole tuning season and was never
observed to separate — recorded openly here rather than silently swapped
for a criterion that happened to pass. Tail dominance held throughout: 1.79×
at the Task-13 sea-trim measurement, comfortably above the 1.5× floor.

## Non-banded metrics, final (iteration 4, probe)

| metric | median | notes |
|---|---:|---|
| shelf-width-passive-median | 2.0 | ties active; asymmetry lives in the tail (see above) |
| shelf-width-active-median | 2.0 | — |
| sediment-volume | 1,235,516 | +35% over the v3 untuned baseline (915,672); wave-cut + barrier fill both feed the wedge |
| waterfall-count | 0.0 | median 0 across the season; a rarity metric (walk-scale landmark), not a band |
| delta-count | 5.0 | stable across every iteration; top-K = 3 discrete lobes, remainder diffuse |

## Resolution: the tuning season is closed; the census regen is the canonical readout

Task 14's tuning season closed 2026-07-16 (ledger #11) with 5 of 6 bands
inside on the probe, shoreline-development's band left explicitly open and
handed to the rift-and-fit coda, and the escalation criterion never firing (engine A
stands, reroute median 0.0834, zero seeds ever exceeding the 0.30 rejection
line). This page's tables are the tuning-season record; the AWS census
regeneration that follows Sculpting's merge is the canonical, larger-N
readout, and any verdict this page's probe tables get wrong will be recorded
as a dated supersession note appended below this line — never a silent edit.
