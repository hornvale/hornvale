# Sculpting

**July 2026 · outcome: merged — the carve is built, five of six preregistered
bands inside, shoreline-development honestly handed open to the rift-and-fit coda**

## What was attempted

Crust gave the land its shape and handed forward two bands its craton and
lobing knobs provably could not reach: coastline complexity and a true
depositional shelf are products of *erosion and sediment*, not tectonics.
Sculpting was terrain epoch v3 — the third and final leg of the terrain
overhaul roadmap (Measured Coast → Crust → Sculpting) — and it took the full
mandate in one epoch rather than staging it: every parked tectonic decoration
(terranes, microcontinents, hotspot trails, atoll chains, discrete island
arcs, belt anatomy, a trench) landed upstream of a genuine erosion/deposition
carve, so the carve would have real texture to work on rather than the smooth
rule-table surface Crust left behind.

The carve itself — **engine A** — is a one-shot correction field: incision,
hillslope relaxation, routing and floodplain deposition, a coastal sediment
wedge, top-K deltas, endorheic playas, and (added mid-season) wave-cut coastal
erosion and barrier islands, all computed once as pure functions of fields
that already exist. It was deliberately built behind a **potential-agnostic
seam** — the module reads "a potential, a flow network derived from it, a
resistivity field," with water-over-elevation as only this campaign's
instantiation — so a later thaumic re-instantiation (ley-lines as a
flux-over-a-different-potential carve) can reuse the machinery whole, never
touching this campaign's code. Two heavier engines, B (an implicit
steady-state solver) and C (fixed-N geomorphic iteration), were designed for
but never built: the seam exists to hold them, and a preregistered
self-consistency diagnostic — the **rerouted-flow fraction** over each
world's twenty largest rivers, comparing pre- and post-carve drainage trees —
was the number that would decide whether they were ever needed.

Two bands anchored the whole season, preregistered before any generator code
landed: **shoreline-development**, 9.51–21.95 (from Crust's 6.785), and
**shelf-fraction**, 0.08–0.22 (from Crust's 0.0483). Both had to come inside
while the four bands Crust already met stayed inside. The ria–wedge
antagonism was named in the spec as the mechanism that would make this hard:
incision carves rias (shoreline-development up), the wedge fills them back
into estuaries and coastal plain (shelf-fraction up, shoreline-development
back down) — the same cells, pulling in opposite directions, with per-mouth
sediment supply as the only field that lets a world hold both processes at
once, the way Earth does.

## What landed

The genesis pipeline grew from five stages to nine: plates and crust as
before, then terranes and microcontinents appended as two new drawn streams
(`terrain/terranes`, `terrain/microcontinents`), then the tectonic
decorations as derived terms and hash-noise (no new draws), then an
induration field pulled forward to precede elevation (The Ground's axis,
computed early because the carve's erodibility divisor needs it before a
surface exists to carve), then provisional drainage, then the carve itself,
then a **second, final** sea-level solve and drainage re-derivation over the
carved surface, then The Ground's lithology buffer assembled over real
valleys instead of a slope proxy. Every existing stream survived with
identical consumption order — the conservative-epoch discipline Crust set —
and drainage now computes twice, the first deliberate doubling of the
single most expensive derivation, measured and accepted (~1.10× per-world
build time, +4.5 minutes at census scale) without a silent cut.

**A hard stop mid-pipeline.** Wiring the carve into `generate` surfaced a
spec ambiguity ledger #4 caught before any tuning began: the marine
freeboard constants (the wedge cap, the atoll rim) were written against the
*pre-carve* sea level, but a one-shot engine's final sea level moves after
the carve fills the ocean — so wedge-capped shelves and atoll rims were
landing systematically emergent (above the real, final sea level) rather
than submerged. Nathan's ruling added a **sea-trim**: a bounded second marine
pass that derives a provisional post-carve sea level, trims every wedge and
atoll top back down to its freeboard below *that* level (delta lobes stay
exempt, subaerial by design), and only then re-derives the true final sea
level and drainage. The trimmed volume books as oceanic loss at the generate
level, closing the mass balance honestly rather than approximately. Deep
Time's eustatic dividend survived the whole detour: sea-level swings over the
now-real shelf flood and expose **3.3×** the land area they did under Crust's
cliff-coast world, with no paleoclimate code touching to earn it.

## What was learned — the tuning season as a falsification engine

Task 14's tuning season is the campaign's real story, and it earned that
status by being wrong on purpose, twice, in the same way Crust's did.

**Iteration 1 was blocked by its own prediction failing.** The first
tuning move — restricting incision's slope term to land neighbors only, so a
cliff face into the abyss stops reading as a river channel — was the
spec-faithful fix for a carry-forward flagged since Task 7: coastal cells
were saturating at the incision cap regardless of drainage, which is not
what stream power means. The prediction was that removing this would let
rias differentiate and shoreline-development rise. It did the opposite:
shoreline-development moved *further out* (6.93 → 6.66) and shelf-fraction
moved further out too (0.068 → 0.058), while a Nathan-ruled gate — the
single-craton `shelf_land_ratio > 0.05` floor from the single-craton
hypsometry campaign — broke outright (4 of 40 sweep seeds fell below it).
The diagnosis: the old any-neighbor slope term had been an *accidental wave-
erosion proxy*, a crude coastal-planation and bay-cutting process the spec's
own ideonomy grid had flagged as "waves half-present incognito." Removing it
without a replacement removed a real mechanism, not just a bug.

**Iteration 2 kept the fix and built the mechanism it had been standing in
for.** The land-only slope term is genuinely correct fluvial semantics (it
fixed the rerouted-flow diagnostic outright, 27% of seeds flagged down to 0%)
and was kept; alongside it, the spec's banked wave-cut extension — "built
only if the shoreline band demands it" — activated for the first time,
landed in the same commit as the slope fix per the adjudication discipline.
Wave-cut differential coastal erosion, scaled by exposure and erodibility,
restored what the accidental proxy had been doing on purpose: shelf-fraction
came inside its band (0.068 → **0.089**), the single-craton floor recovered
above its pre-iteration-1 level (median 0.336 vs. 0.256 baseline), and the
passive/active shelf-width inversion cleared. The wave-cut depth scale,
`wave_cut_m`, landed at **1000** — not the coordinator's initial guess of 60
— chosen from a direct probe sweep against the worst single-craton seed
rather than blind retries, and documented as an epoch's planation capacity at
~10²-km cell scale, not a literal wave height.

**Iteration 3 tested a real bug and correctly reported that fixing it wasn't
enough.** The relief noise's dominant octave was sub-Nyquist at the
canonical grid — aliasing to per-cell jitter the sea-level percentile
averages away, a genuine defect independent of whether fixing it moved any
band. It was fixed (`RELIEF_FREQUENCY` 48 → 8) and shipped on its own merits;
shoreline-development moved a bare +0.02, far short of the season's +0.5
threshold, honestly reported as a stop condition rather than chased further.

**Iteration 4 built the spec's last banked mechanism and hit a wall a
diagnostic had already predicted.** A dedicated shoreline diagnostic (see
below) established that the estimator was nowhere near a ceiling — it needed
finer, single-hex-scale texture, not smoother multi-cell embayments — so the
season's fourth iteration built barrier islands and lagoons: a detached-cell
candidate rule plus alternating spacing that produces exactly the land/ocean
alternation the estimator rewards, gated on real per-mouth sediment supply
from passive-margin coasts. It is a real, physically-named process the spec
had explicitly banked for this contingency. It moved shoreline-development
further than any other single lever in the season — 6.7250 → **7.3202**,
+8.85%, every other guard green or improved, the single-craton floor
*improved* to 0.463/0.191 — and still landed well short of the 9.51 floor.
With the full banked coastal stack (wave-cut, wedge/shelf, deltas, atolls,
barriers/lagoons) now built in its entirety, this was the preregistered stop
condition for a **band-supersession conversation**, not a further tuning
knob.

**The shoreline diagnostic and Nathan's ruling.** Before that conversation,
a read-only instrument answered the load-bearing question: is 9.51 even
reachable at this mesh resolution? The estimator (`D = L / 2√(πA)`, unchanged
since inception) rewards single-hex land/ocean alternation almost
exclusively; two independent probes — an analytic isolated-island supremum
and an empirical parity-crenulation experiment over a real seed's coastal
band — both cleared the entire band by 2.9×–3.5×, proving the formula is not
saturated. But the same archaeology also showed the band's own floor was
partly built on sand: the v1@L6 interim anchor (7.315, the number the
9.51 floor was set at 1.3× of) was inflated by a fragment-swarm boundary
noise generator that Crust *deliberately removed* as fragmentation, not
texture — the anchor and the current generator's coastline-production
mechanism are measuring different things even though the formula and grid
are identical. Nathan's ruling (ledger #11, 2026-07-16) closed the season on
an **open verdict**: no new floor was invented, Census of Coasts III records
the miss honestly with the anchor-contamination finding as a formal
supersession note, and the banked **rift-and-fit fake-history coda**
inherits the open band explicitly as its evidence baseline — mirroring
Crust's own handoff of this exact pair of bands to Sculpting one epoch ago.

**The escalation criterion never fired.** The rerouted-flow-fraction
diagnostic — engine A's own preregistered self-consistency check — closed
the season at a median of **0.0834**, comfortably under the 0.10
self-consistency line, with zero seeds ever crossing the 0.30 rejection
line across the whole season. Engine A stands as the sole engine; engines B
and C remain seam-only, never entered into evaluation.

Season total: five of six bands land inside their Earth-anchored ranges
(hypsometric-bimodality, shelf-fraction, continent-count,
largest-continent-share, plate-size-gini), all improved or held stable
across four tuning iterations; shoreline-development moved a real +8% and
still sits below its floor, handed forward with its evidence rather than
forced.

## The road ahead

Sculpting completes the terrain-overhaul roadmap Measured Coast opened.
The **rift-and-fit fake-history coda** is the one deferred piece: a
supercontinent fractured along noisy cracks with conjugate coastlines fit by
Euler-pole displacement, judged against this campaign's Census of Coasts
III baseline — and now, per Nathan's ruling, also the band expected to make
the strongest case on the shoreline-development question this campaign
leaves open. The ideonomy session that shook the spec loose also banked six
registry rows for a possible future metaphysics overlay (ley-lines as a
thaumic re-instantiation of the carve seam, Mythic landform drawn sets,
gated anti-physics landforms, purposeful sculptors and wyrm tunnels, tidal
causeways, and a single-metaphysics-dial umbrella row spanning all of them) —
none built, all zero-cost hooks left exactly where the carve's own seams
already accommodate them. Aeolian transport, loess, and dune fields stay
banked as they were before this campaign; barrier cells classify through the
generic lithology buffer today, with true unconsolidated-sand lithology
recorded as a named follow-up rather than built in-season.
