# The Newel — design

Campaign B of a three-way split over a batch of thirteen client bug
reports. Campaign A (The Ken) merged at `e6fcf92f0`. Campaign C
(vocabulary) is separate and later.

Ledger: `docs/superpowers/ledgers/2026-09-06-the-newel.md`, which carries
the full reproduction evidence this document summarises.

A newel is the post a spiral stair turns about — the thing that makes a
vertical passage navigable. Four of the six reports are about knowing
where you are and how to move between levels.

## 1. The six reports

| # | report, verbatim from Nathan |
|---|---|
| B1 | `>` `<` should work for entering and exiting from the walk band |
| B2 | You should be able to `enter Doaba`, `enter Dvoas[tab]`, etc. |
| B3 | I press the left arrow, sometimes I move southwest. What? |
| B4 | Map marquee is almost useless. It should show information about the selected item and not a legend for the map itself. |
| B5 | Looks like we're still drawing terrain at the vertex level, not facet level. |
| B6 | Open water doesn't look like it's getting a day/night/etc cycle appearance change. |

They do not share a cause. B1 and B2 are the command surface; B3 is a
disclosure gap; B4 is a content decision; B5 and B6 are both the map's
colour resolution, one in space and one in time.

## 2. What was reproduced before this spec was written

Every report was reproduced against real output. The ledger holds the
figures; the four that changed what this spec says are:

**B3 is not a defect in any component, and the surface everyone assumed is
dead code.** `chart::draw` is unreachable in the shipped client: on the
walk band `world_plate_for_redraw` returns `Some`, and
`spread::compose` draws the chart only when it returns `None`. The
surface actually drawn is the north-up Mercator raster, and it
reproduces the report at seed 42's flagship with no seam and no polar
face: forty `west` presses scroll the window `(-1, 0)` thirty-eight
times and `(-1, +1)` — west and south together — twice. The room's
`west` neighbour bears **267.28 degrees**, the closest of the eight the
lattice offers; `cos(267.28) = -0.047`, so each step carries 0.047 of a
plate row south and every ~21 steps that is a whole row.

**B2 is a different bug than the one reported.** `enter Doaba` already
works. So does `enter banana`, and so does `enter Nenotata` — a
settlement on the other side of the planet. Outdoors, `enter` never reads
its argument at all.

**B5's written confession is a red herring.** `plate.rs:938-940`'s "the
wire's own per-facet colour is not used here" sits on `MARK_COLOR` and is
about the observer's marker, correctly. The real defect is that
`reflectance_at_facet` takes `dominant_corner` — one vertex — for both
biome expression and lithology. Measured at the shipped default rung,
12,000 distinct facets render as **one** colour, because they share five
vertices.

**B6 is one line, and it is deliberate.** `plate::color_for`'s two wet
arms call `obs.show(...)`, which takes no illuminant. The doc beside it
states the reason: there is no spectrum in the sim for open water seen
from above.

## 3. The organising decisions

Three ratified decisions do most of the work here and are cited rather
than re-argued.

- **0788** — a way is named by role, never by index; more than one way
  makes it a refusal that names the ways; and *"the footer's own words
  must be typable."* This is the rule B1 and B2 both answer to.
- **0687** — a derived surface reads continuous causes, never categorical
  labels. It names B5's failure in advance: *"a per-vertex categorical
  answer read across a 1.126 km walk band paints a uniform label across
  the whole band."*
- **0117** — a client may render a datum the simulation emits; it may not
  re-derive a decision the simulation makes. This is why B1's dispatch is
  sim-side and B2's completion candidates must come from the wire.

## 4. The changes

### 4.1 `>` and `<` become band transit (B1)

The sim has four descents and four ascents — `enter`/`out`,
`delve`/`climb`, `dive`/`surface`, `down`/`up` (stairs) — and `<`/`>`
reach only the stairs pair, which refuses everywhere except underground.

Add one verb each way. `descend` takes whichever descent this position
offers; `ascend` takes whichever ascent. The dispatch is **sim-side**
(0117): only the session knows which of the four applies.

Resolution, in the state the possession is in:

| state | `descend` | `ascend` |
|---|---|---|
| inside a chamber | the structure's own further-in, if unambiguous | `out` |
| underground | `down` (stairs) | `up`, or `climb` at the top |
| submerged | `dive` | `surface` |
| walk band | whichever of `enter`/`delve`/`dive` this facet offers | nothing to ascend — say so |

**More than one available descent is a refusal that names them**, not a
guess. That is The Ken's rule (`session.rs:1584-1591`) and 0788's, and it
is why this is a verb rather than a client-side chain of attempts.

**The new verbs must be typable and must appear in `HELP`** — 0788's
parity clause. `<` and `>` then bind to them in
`clients/game/bin/src/input.rs` in place of `Move("up")`/`Move("down")`,
which keeps the roguelike convention the reporter is using.

**Preregistered measurement (freeze before the code):** how often does a
walk-band facet offer more than one descent? The refusal arm is only
worth its complexity if the answer is not "never". Measure over a uniform
sample of walk-depth facets on seed 42 and record the count in the ledger
before implementing the arm. **A null is a finding**: if no facet in the
sample offers two, the refusal still ships (a facet CAN carry a
settlement and a cave — `brief_of` builds up to three `Site` candidates)
but the spec records that it is unexercised in practice.

### 4.2 An argument to `enter` is checked, never swallowed (B2)

Two halves, and the first is the actual bug.

**(a) Outdoors, `enter <target>` resolves against what is here.** The
session already knows the facet's single `Site` and, for a settlement,
its name (`Brief::site.name`). So:

- no argument — unchanged, enter the site here;
- an argument naming this site (its name, or its kind noun: `settlement`,
  `cave`, `site`) — enter it;
- anything else — **refuse, and name what is here**, in the wording the
  walk-band prose already uses.

This is the rule `map` and `sleep` already apply in this same file
(`session.rs:5649-5652`, `:4022-4028`): *"an ignored argument is how a
player comes to believe they asked for something and got it."* `enter` is
the last verb in the roster that still swallows one.

Note the asymmetry the site data forces: a settlement has a name, a cave
and an exotic site do not (`Site::placed`'s call sites in `brief.rs`). So
`enter cave` must work by kind noun where `enter <name>` cannot.

**(b) Completion candidates widen beyond `narration.nouns`.** Tab is
already bound (`input.rs:125`) and the engine already works
(`clients/game/core/src/lexicon.rs`); its only candidate source is
`narration.nouns`, six entries at the flagship. So `examine
Dvoashngashngo` resolves while `enter Dvoas[TAB]` completes nothing — the
completion set is strictly narrower than the resolver's.

The wire already carries the missing names: `spatial.chart.cells[].marks[]`
and `spatial.chart.legend[]`, both already parsed by the client-core
schema (`schema.rs:265-283`). Adding them as a second `CandidateSource`
re-derives nothing (0117).

**The hazard, and it is not optional: decision 0670 withholds a placed
site's proper NAME until discovery** (its glyph is ungated; its name is
not). Completing an undiscovered settlement's name would leak exactly what
0670 protects. **Candidates from marks must therefore be gated by the same
discovery predicate the cursor readout already uses.** A task that adds
the source without the gate is a defect even though nothing would go red.

### 4.3 A held heading must walk true (B3)

**This section was rewritten after G3.** Its first draft said nothing
computes a wrong answer and that a held course had been deliberately
closed. Both halves are withdrawn; see ledger R9-R10 for the measurement.

**The Pavement preregistered exactly this and its hypothesis is
falsified.** Spec section 7, H1: *"From 200 distinct seed-42 start cells,
walking `n` for 500 steps leaves the walker within 0.5 cell of the
starting meridian at every step. This is the direct repair of The Rhumb's
falsified H1 (unbounded drift, ~0.086 step-lengths per step)."*

Re-run over 216 start cells at walk depth 13, cross-track in step-lengths,
equatorial faces:

| word | worst mean | fails 0.5 | vs the great circle |
|---|---|---|---|
| N | **0.00** | 0/144 | 0.00 |
| S | **0.00** | 0/144 | 0.00 |
| E | **69.85** | 144/144 | 69.85 |
| W | **69.85** | 144/144 | 69.85 |

North and south are exact — on a cube face the constant-`a` lines cut the
sphere in meridians, which are great circles, so a held northward heading
walks one. **East and west fail at every equatorial start cell**, drifting
**0.1445 step-lengths per step** from both the parallel and the great
circle — **1.68x The Rhumb's 0.086, the number The Pavement quotes as the
defect it repairs.** In absolute terms: ~78 km off course over 563 km
walked.

H1 chose the one direction that cannot fail: for `n` the meridian and the
great circle are the same line. And it appears never to have been run —
the plan's Step 1 names a probe file that does not exist, and neither the
chronicle nor the retrospective reports an H1 result, though both report
H2 and H3.

**Why the current design cannot self-correct.** A greedy per-step
nearest-bearing rule re-resolves the word at every room, which sounds
self-correcting and is not: the move that would cancel accumulated drift
is a diagonal 45 degrees off the word, and it always scores worse on the
per-step metric than the ~8-degree-off cardinal that caused the drift. So
the residual is systematic, never cancelled, and unbounded.

**And the argument that deleted `course.rs` is about one step, not a
walk.** The Pavement's section 3.4: *"With eight real edges the mapping is
the identity and the module's whole subject is gone."* True of a single
bearing. False of a sequence, which still accumulates cross-track error
and still needs the ideal line to correct against — which is what `Course`
held.

**So the change is a correction, not a disclosure**, and it needs a
decision record superseding The Pavement's section 3.4 rather than a task
under it. The shape is a cross-track-corrected heading: the walk carries
the line it is meant to be on, and each step takes the neighbour that best
serves it rather than the one that best matches the bearing in isolation.

**Two things must be decided before any of that is built, and they are
Nathan's:**

1. **Which line does `west` name?** A parallel (constant bearing — what a
   compass-holder walks) or a great circle (straight ahead, initially
   west — what a body walks)? They differ, and **the project has never
   decided.** The Rhumb chose the parallel by construction; The Pavement
   chose neither, by deleting the choice; H1 chose the direction where
   they coincide. Every number above is reported against both so the
   choice is not prejudged.
2. **Is 0.5 step-lengths still the right bar?** It is H1's, and H1 was
   never run. A correction rule should be preregistered against a bar
   chosen on purpose.

Disclosure — the bearing in the endpaper, the walked trail on the plate —
survives as a **complement**, not a substitute. Even a corrected walk
takes discrete edges and wobbles by a fraction of a cell; saying so is
still worth doing, and it is what makes the correction checkable by eye.

### 4.4 The marquee describes the cell, and the map's own facts decay (B4)

Measured at the flagship: 188 characters, of which 25 (13.3%) describe the
thing under the cursor and 160 (85.1%) are four map-wide clauses — the
rung line, the tile count, the oversample disclosure, the clamp caption.
The strip is 40 columns at the 80x24 floor, so at 300 ms per column the
last clause is reachable after 44.4 seconds. That is the report.

The map-wide clauses are not noise, though: three of them are 0196's
disclosure that a map may state its own resolution. Deleting them would
trade one defect for another. **They are transient facts about a gesture
you just made** — you zoomed, so the rung changed — **not standing facts
about where the cursor is.** So:

- the strip's **standing** content becomes the cell under the cursor;
- the map-wide clauses **appear when they change and decay**, which
  preserves 0196's disclosure, needs no new gesture, and costs the
  standing line nothing.

What the client can already say about a cell, without a new derivation:
the containment chain it already prints, plus `TileTerrain`'s `water`
class, relief `band`, continuous `height_asl`, and `ocean` flag
(`plate.rs:2170-2233`), plus whether a `MapSite` stands there. Biome is
**not** on the map path today (it is chart-wire only) and a settlement's
NAME is on `MapSite` neither — so the readout is built from what is in
hand, and any addition is a separate, argued step. **Decision 0670 gates
a site's name here too**, exactly as in 4.2.

**Two existing tests pin that the marquee advances on ticks and not on
player actions** — `driver.rs:5069` and `:5093`, added 2026-08-24 at
Nathan's explicit request. Both run green in this worktree and **neither
may break.** Note also that `driver.rs:5177` still carries a doc claiming
the pre-2026-08-24 design; it passes only because it never ticks. Do not
read it as pinning anything.

### 4.5 Terrain colour becomes a facet-level answer (B5)

Two independent vertex reads, and they want different fixes.

**(a) The land/water boundary should read a continuous cause — IN THE
SIM, NOT THE CLIENT.** `tile.ocean` is `terrain.is_ocean(vertex)` and
`tile.water` is `water_kind_at(vertex)` — categorical, per-vertex, and
`color_for` branches on `water` before it ever reaches reflectance. So the
coastline drawn at rung 13 is a 110 km Voronoi edge between icosphere
vertices. `tile.height_asl` is **already blended** across the facet's four
corners and the scene already carries `sea_level_m`, so comparing them is
0687 exactly: a continuous cause at facet resolution, inside the convex
hull of its samples (0676).

**The first draft put that comparison in the client, and that is wrong.**
The walk-band prose decides "open water" from
`v.locale.biome_kind.is_marine()` (`windows/vessel/src/focalize.rs:228`),
which is per-vertex. A client-only refinement means that near every
coastline a player stands on a tile the map draws as ocean and is told
they are in a forest — decision 0141's one-turn observable contradiction,
and decision 0117 forbids the client re-deriving a decision the sim makes.
So the refinement belongs in `windows/locale`, where the prose and the map
both see it, or nowhere. That is sim-side, with walk-band prose fallout
and committed-fixture fallout, and it is the strongest argument for
splitting B5 into its own campaign.

`plate_vocabulary.rs:153-157` asserts
`terrain.is_ocean(tile.vertex) == tile.ocean` and therefore pins the
current behaviour. That assertion moves with the change — it is the
statement of the defect, not a guard against a regression.

**(b) The nominal fields need a finer partition, not a blend.** Biome
expression and lithology are categorical, and **decision 0121 forbids
banding a blend for a nominal field** — it must take a partition,
evaluated per room. So the fix is not "interpolate them": it is to make
the partition finer. Decision **0667** is the ratified precedent for
exactly this move (a placed site re-sited from vertex to facet by a
seeded draw). A facet chooses among its four corner vertices by a seeded
draw weighted by `corner_weights`, instead of always taking
`dominant_corner`. That is deterministic, per-facet, still a partition,
and every value it can return is one of the samples.

**The seeded draw is honest per value and dishonest per pattern**, and
this is the fidelity call. It satisfies 0676 (every value it returns is
one of the samples) and 0121 (still a partition). But it manufactures
high-frequency spatial structure the field does not have: a
salt-and-pepper transition reads to a player as real patchiness, in a
world holding one biome sample per 110 km. A better member of the same
family: **warp the partition boundary with a smooth field rather than
dicing it** — keep the hard per-facet partition, but let a low-frequency
noise term decide which corner wins, so patches stay coherent and only
the boundary path is invented. `domains/terrain` already warps with
`Fbm`, and a per-facet `micro` term already exists on this path, so the
instrument is precedented rather than new.

Stated plainly because it is the crux: **the map already invents boundary
shape.** Today's hard edge at 1.1 km is derived from 110 km data and is no
more justified than any other curve. The question is not whether to
invent but which invention misleads less — a fidelity tradeoff, and
therefore Nathan's.

**This touches `windows/locale`, which is inside the cargo workspace**,
and `reflectance_at_facet` also feeds the sim's own chart colouring
(`windows/scene/src/surrounds.rs:812`). So it moves the committed client
fixtures under `clients/game/core/tests/fixtures/`, which carry per-cell
colours. **Flagged for G3.**

### 4.6 Open water takes the light (B6)

`plate::color_for`'s wet arms call `obs.show(OCEAN_COLOR)` /
`obs.show(SALT_BASIN_COLOR)`, and `TerminalObserver::show` has no
illuminant parameter. Measured: the ocean ink is byte-identical from 80
degrees of sun elevation to 60 degrees below the horizon, over a window
that is 75% ocean.

The blocker the existing doc names is real — the sim has no water-surface
spectrum, and inventing one in a domain crate would be a determinism-
surface change for a presentation problem. But it is not the only way
through. **The client's water palette entries become reflectances rather
than sRGB triples**, and then take the ordinary `obs.observe(..., illum)`
path like every other surface. The view owns the observer (0716), so a
client-side stand-in spectrum is exactly where this belongs; nothing
crosses the wire, no schema moves, no determinism surface widens, and the
whole change stays inside `clients/`.

**Two adjacent findings are NOT in scope and are recorded as follow-ups**,
because folding them in would make this report a different campaign:

- night has no gradation anywhere — `MAX_AIRMASS = 38.0`
  (`domains/astronomy/src/illuminant.rs:50`) makes every sun elevation
  below about 1.5 degrees identical, and nothing moonlit or starlit
  contributes to any illuminant. Water joining land in that flatness is
  the correct outcome of 4.6 and does not fix it.
- the habitat descriptor's light word is clock-blind
  (`windows/locale/src/grammar.rs:218-227` reads the static
  `micro.aspect`), so a room reads `sun-warmed` at midnight — on land as
  well as water.

## 5. Testing

The gate that covers five of these six changes is **`make game-check`**,
not `make gate-commit`: the workspace gate does not scan `clients/`.
4.1, 4.2(a) and 4.5(b) also touch workspace crates and need
`make gate-commit`. Both are required; neither implies the other.

- **B1** — a table test over the four states x the available transits,
  and one test that the ambiguity arm refuses and names. Plus the
  preregistered availability measurement (4.1).
- **B2** — `enter <wrong name>` refuses, `enter <right name>` and
  `enter <kind noun>` succeed, and a completion test that an undiscovered
  settlement's name is **not** offered (the 0670 gate). That last one is
  the test most likely to be written vacuously: it must be shown to go red
  when the gate is removed.
- **B3** — a walk test reading `position()`/the window origin, never
  prose. The reproduction is already a positive control: 40 west steps
  from the flagship must produce a `(-1, +1)` window step, and the
  disclosure must name the bearing that causes it.
- **B4** — the two tick tests stay green (verify by running them, not by
  reading them), the strip's standing content names the cursor cell, and
  the map-wide clauses still reach the player when they change.
- **B5** — a resolution test: distinct colours per distinct vertex must
  rise above 1.0 at the shipped rung. The current measurement (12,000
  facets, 5 vertices, **1** colour) is the pre-fix baseline and belongs in
  the test's own doc. `wash.rs:701`'s `if t.water < 2 { continue; }` skip
  and `plate_vocabulary.rs:153`'s vertex assertion both move.
- **B6** — render the same ocean tile at two sun elevations and assert the
  inks differ. **No such test exists today for any tile**; `plate_at`
  deliberately uses a flat light.

**Nothing in `clients/` is covered by a committed byte-golden**, so every
assertion here is a property assertion and each must be shown to fail
before it passes.

## 6. Flagged for review

**Revised after the first G3 round.** Nathan's challenge to B3 was correct
and changed the campaign's centre of gravity.

1. **B3 is now the largest item, and it wants a decision record.** A
   preregistered hypothesis is falsified; the fix is a correction rule,
   not a disclosure; and it supersedes The Pavement's section 3.4. Two
   sub-questions are Nathan's alone: which line `west` names (parallel or
   great circle), and what the drift bar should be now that H1's 0.5 was
   never tested.
2. **B5's half (a) grew.** It cannot live in the client without making
   the map contradict the prose, so it is a `windows/locale` change with
   prose and fixture fallout.
3. **B5's half (b) is a fidelity call** — a seeded draw invents
   high-frequency structure; a noise-warped boundary invents only the
   boundary's path; today's hard edge already invents a path. Unpacked
   here, not decided.
4. **Scope.** With B3 grown and B5(a) moved sim-side, six reports in one
   campaign is probably wrong. Three shapes:
   - **all six**, sequencing B3 first and accepting that it dominates;
   - **split three ways** — B3 alone (it is a movement-correctness
     campaign with a decision record), B5 alone (a resolution campaign),
     and B1/B2/B4/B6 as the command-and-appearance campaign this one
     started as;
   - **split two ways** — B3 out, everything else here.
5. **4.6 puts an invented spectrum in the client.** It moves no contract,
   but the client would author a physical quantity. 0716 permits it (the
   view owns the observer); saying so out loud is the point.
6. **4.4 changes a surface Nathan personally specified.** The tick
   behaviour is untouched and pinned; only the content moves.

## 7. Non-goals

- Campaign C's vocabulary rename (`Room` to `Facet`, `Chamber` to
  something). `[room]` and `[chamber]` will churn; this campaign neither
  invests in nor pre-empts it.
- Reviving `chart::draw`, or fixing its projection defect (it loses a
  third of its cells and three of the eight immediate neighbours at the
  flagship). Real, measured, and off the player's path — a follow-up in
  the ledger.
- The night-gradation and clock-blind-descriptor findings under 4.6.
- Biome on the map-cursor path, and a settlement's name on `MapSite`.
  Both are real gaps 4.4 works around rather than closes.
