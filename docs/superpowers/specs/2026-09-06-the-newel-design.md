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

They do not share a cause, and after the G3 stop they no longer share a
campaign. **B3 and B5 are split out** — see 4.3 and 4.5. This campaign is
**B1, B2, B4 and B6**: the command surface, the map's text furniture, and
one line of colour.

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

### 4.3 SPLIT OUT — B3 is now The Sett

`docs/superpowers/specs/2026-09-06-the-sett-design.md`. Approved by Nathan
at the G3 stop, 2026-09-06: the walk rung draws the adjacency graph through
a transported local frame rather than a Mercator projection of it, decided
on the measurement in ledger R14. The reasoning, the falsified Pavement
hypothesis and the raster comparison stay in this campaign's ledger (R9,
R10, R13, R14, decisions #1, #7, #8), which The Sett's design cites rather
than copies.

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

### 4.5 SPLIT OUT — B5 is now The Stipple

`docs/superpowers/specs/2026-09-06-the-stipple-design.md`. Approved at the
same stop. Two halves: the land/water boundary reads the already-blended
height in `windows/locale` (not the client, which would make the map
contradict the prose), and the nominal fields take a finer partition rather
than a blend. The fidelity call between a seeded draw and a noise-warped
boundary is carried forward to that campaign, unmade.

Note for this campaign: **4.6 touches the same function** (`color_for`'s
water arms). The two changes are independent — 4.6 changes what the wet
arms are made of, The Stipple changes when they are taken — but whichever
lands second will meet the other's edit.

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
- **B6** — render the same ocean tile at two sun elevations and assert the
  inks differ. **No such test exists today for any tile**; `plate_at`
  deliberately uses a flat light.

**Nothing in `clients/` is covered by a committed byte-golden**, so every
assertion here is a property assertion and each must be shown to fail
before it passes.

## 6. Flagged for review

**G3 is closed.** Nathan approved the three-way split on 2026-09-06: B3
became The Sett, B5 became The Stipple, and this campaign is B1, B2, B4 and
B6. What remains flagged is carried to the G6 digest rather than held here:

1. **4.6 puts an invented spectrum in the client.** It moves no contract,
   but the client would author a physical quantity. 0716 permits it (the
   view owns the observer); saying so out loud is the point.
2. **4.4 changes a surface Nathan personally specified.** Tick behaviour
   untouched and pinned; only the content moves.
3. **4.2(b)'s discovery gate is the campaign's one silent-failure risk.**
   Widening completion without it leaks what decision 0670 withholds, and
   nothing would go red. Its test must be shown to fail with the gate
   removed.

## 7. Non-goals

- **B3 (The Sett) and B5 (The Stipple)**, both split out at G3 with their
  own designs. Their analysis lives in this campaign's ledger and is cited,
  not duplicated.
- Campaign C's vocabulary rename (`Room` to `Facet`, `Chamber` to
  something). `[room]` and `[chamber]` will churn; this campaign neither
  invests in nor pre-empts it.
- Reviving `chart::draw` or fixing its projection defect — that question
  now belongs to The Sett, which either revives it in graph space or
  deletes it.
- The night-gradation and clock-blind-descriptor findings under 4.6.
- Biome on the map-cursor path, and a settlement's name on `MapSite`.
  Both are real gaps 4.4 works around rather than closes.
