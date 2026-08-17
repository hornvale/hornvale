# The Rhumb — compass navigation over a triangular world

**Campaign:** The Rhumb
**Branch:** `campaign/the-rhumb`
**Status:** COMPLETE — Tasks 1–5 and 7 shipped; Task 6 (the north-up chart)
deferred to the successor appearance campaign, blocked on `SurroundsCell`
carrying no per-cell position (`NAV-north-up-needs-per-cell-position`).
Ratified as decision 0141 — **not 0140**, see §11's own correction and the
retrospective's §7: The Sluice minted two decisions, so the board claim that
named 0139 under-reported by one.

A player walking this world is offered eight compass directions and given
three, and *which* three depends on which way the triangle under their feet
happens to point. This campaign makes the compass mean what it says, without
touching the geometry the world is made of.

---

## 1. What this campaign produces

- `go <dir>` accepts all eight compass points from every cell, resolving each
  to an edge-neighbour by a carried **rhumb course** rather than by exact
  bearing match.
- Repeated stepping in one direction tracks a line of constant bearing:
  walking east forty times leaves you east, not somewhere on a random walk.

**Correction (final review F4, 2026-08-16): the two bullets below did NOT
ship.** They described Task 6, which the status header above already marks
deferred — but this section itself went unedited when that deferral was
decided, so a reader who starts at "what this campaign produces" (as the
section is named) was told two false things. Left here, struck through,
rather than silently deleted, because the deferral is a real fact about this
campaign and §8/the registry rows it points to are the load-bearing record
of it, not this list:

- ~~The walk-band chart is drawn **north-up** rather than lattice-aligned, so
  the picture and the compass agree.~~ Not shipped — see the status header
  and `NAV-north-up-needs-per-cell-position`.
- ~~Seam cells become drawable, because a bearing exists where a lattice
  coordinate does not.~~ Not shipped — same reason.

It changes no world. No new seeded draw, no new stream label, no epoch.

---

## 2. The defect, read from the code

Three findings, each cited to what was actually read. None of this was
measured against a running program — see §6, which is not a formality.

**2.1 — Exactly three lateral exits, labelled by true bearing.**
`windows/locale/src/lib.rs::exits_of` walks the three edge-neighbours and
pushes, for each, `Direction::Compass(compass(addr.bearing_to(&n)))`. The
count is pinned by a test named `exits_are_three_lateral_plus_vertical`
(`lib.rs:2159`). `compass(bearing_deg)` buckets a quantized bearing to eight
points; the quantize-first step is deliberate and its comment says why
("keeps it cross-platform stable").

**2.2 — `go` requires an exact match, and its refusal misdescribes the
world.** `Session::go` (`windows/vessel/src/session.rs:1671`) searches the
locale's exits for `e.direction == Direction::Compass(wanted)` and, finding
none, answers:

```rust
return Turn::Out(format!("No way {} from here.", dir.to_lowercase()));
```

That sentence claims the world affords no path in that direction. What is
actually true is that no *edge* of this triangle buckets to that compass
point. The player is told a fact about the world when they have been given a
fact about the tiling.

**2.3 — Which three you get flips with orientation.** A triangle's three
edge-normals are fixed relative to the triangle, and `RoomAddr::neighbors`
returns them in a fixed order relative to its corners
(`kernel/src/room.rs:490`). An up-pointing triangle therefore offers a set
of bearings roughly complementary to a down-pointing one, and the two
alternate as you walk. The consequence is the one that matters: **there is
no sequence of `go e` that walks east.** Each step alternates between the
two edges nearest east, and near a bucket boundary the *label* flips too.

**What is not wrong.** The geometry is not wrong, the bearings are not
wrong, and `exits_of` is not wrong. Every part is locally correct. The
defect is that the eight-point vocabulary in the help text
(`n ne e se s sw w nw`) promises an affordance the tiling cannot deliver,
and nothing in the code says so.

---

## 3. The model — a dead-reckoned rhumb course

The session carries a **course**: a bearing, and a *reckoned point* which is
a coordinate, not a cell.

```
Course {
    bearing_deg: f64,     // 0, 45, 90 … the compass point last asked for
    reckoned: Coord,      // lat/lon; advances exactly, never snaps
}
```

**Setting.** `go <dir>` whose compass point differs from `bearing_deg` seeds
a new course: `bearing_deg` from the compass point, `reckoned` at the
current cell's centroid.

**Stepping.** Every `go`:

1. advance `reckoned` along the rhumb of `bearing_deg` by one **step
   length**;
2. move to whichever of `position.neighbors()` lies nearest the new
   `reckoned` by great-circle distance;
3. tie-break on packed room id, ascending.

**Continuing.** `go <dir>` whose compass point equals `bearing_deg` keeps
the existing `reckoned` — it is not re-seeded from the cell just landed on.
That is the whole mechanism, and §3.1 is why.

### 3.1 Why dead reckoning rather than nearest-bearing

Picking the nearest-bearing neighbour at each step, statelessly, already
fixes the *availability* half of the defect: all eight directions resolve
from every cell. It does not fix the *path*. With no memory, each step's
rounding error is independent, so the walk is a random walk about the
intended line and its deviation grows without bound.

The reckoned point is exact and is never corrupted by the snap. The position
error is therefore bounded by the snap alone — at most half a cell — and
**does not accumulate**. This is the entire reason the course is carried
rather than recomputed.

### 3.2 Step length is derived

The step length is the mean great-circle distance from the current cell's
centroid to its three edge-neighbours' centroids, computed per step.

No constant, no table. It scales with refinement depth automatically, so a
future change to the walk band's depth needs no retuning here — and a
constant would silently become a save-format-adjacent number the moment
anyone tuned it.

### 3.3 Rhumb, not great circle

A rhumb line holds a constant bearing; a great circle is the shortest path
and its bearing changes as you walk. Both are defensible and they are
different worlds. This campaign takes the rhumb, because `go e` is a
player's instruction to a body holding a compass, not a flight plan: a
player who walks east expects their compass to keep reading east, and
expects to come back around the world to where they started.

The cost is stated rather than hidden: a rhumb at a constant non-cardinal
bearing spirals toward a pole and is not the shortest path between its
endpoints. Neither consequence is visible at the distances a walk covers,
and both are true of every terrestrial compass.

### 3.4 What stays exactly as it is

`exits_of` keeps producing the three true edges. `RoomAddr::neighbors` is
untouched. The sim's adjacency graph — which A\*, `Geosphere::hops_between`,
ecology, drive planning and settlement fitting all ride — does not change,
and no domain crate is edited.

This is not a workaround. `RoomAddr::neighbors`' own doc already reserves
the space:

> The three edge-neighbour rooms, at the same depth (the geometric base
> graph). … **passability and overlay edges are higher layers and never
> enter here.**

Compass navigation is such a higher layer. It belongs in the window that
presents movement to a player, not in the kernel that defines adjacency.

---

## 4. The chart goes north-up — DEFERRED, DID NOT SHIP

> **This section describes work that was not done.** Task 6 was deferred to
> the successor appearance campaign; the chart still draws lattice-aligned
> and `orientation` still reads `"lattice"`. The section is retained
> unedited below because it is the successor's starting point, and because
> deleting a design after deciding not to build it destroys the reasoning
> the next campaign needs.
>
> **What re-planning found, which this section could not have known:**
> `SurroundsCell` carries no latitude, longitude or bearing — only `room`
> and the lattice offsets. So "cells place by bearing and great-circle
> distance" is not implementable by any client at all, and north-up needs
> per-cell position on the wire first. See the plan's Task 6 and
> `NAV-north-up-needs-per-cell-position`.
>
> §5 and §12 below likewise describe the campaign as originally scoped.
> The status header at the top of this file is authoritative on what shipped.

`SurroundsScene` already carries `pub orientation: String`
(`windows/scene/src/surrounds.rs:304`), set to `"lattice"` at line 502. This
campaign adds `"north-up"` as a second value. The field was built to hold a
value; adding one is additive.

Cells place by **bearing and great-circle distance from the observer**
rather than by the `(v, w, up)` lattice projection
`row = -w, col = 2v + (up ? 0 : 1) + w` that `surrounds_ascii.rs` and
`clients/game/core/src/chart.rs` each implement.

Two consequences worth stating up front:

- **Seam cells become drawable.** `chart.rs` skips them today because their
  `u/v/w/up` are `null` where the lattice bends across a base face, and
  "inventing a position for them would be a worse lie than omitting them."
  A bearing and a distance exist for a seam cell. The lie is retired rather
  than worked around.
- **Collisions become possible.** A lattice projection is injective by
  construction; a bearing/distance projection onto a character grid is not.
  The renderers already have a collision discipline —
  `chart.rs::draw`'s second pass lets the most salient mark win its box —
  and this campaign must extend it to terrain cells rather than assume it
  will not be needed. **This is the highest-risk part of §4 and the reason
  the north-up chart is its own plan stage.**

Whether both projections coexist behind the `orientation` value, or the
lattice projection is retired outright, is a plan-stage decision to be made
against what the fixtures show — not decided here.

---

## 5. Refusals

If the resolved neighbour cannot be entered, `go` refuses **with the
physical reason**, matching the standard the indoor path already holds
(`the_blocking.rs::a_wall_refuses_with_a_physical_reason`).

`"No way {dir} from here."` is retired as a message about tiling. Whether it
survives at all depends on §6.1.

---

## 6. What is unverified, and how each is settled

The box this spec was written on carried a load average of 107–128 (three
runaway VS Code ripgrep processes and three days of pegged Spotlight
indexing). A `rustc` accumulated 30 seconds of CPU in 63 minutes of wall
clock, and no build completed. **Nothing below was measured. Each is a
task-one probe, and each is written as a branch table rather than a
prediction**, because a step that reads as a thing to do hides the claim
inside it.

### 6.1 Does anything block movement outdoors?

Run a possession and attempt to walk into water from a coastal cell.

- *Water refuses* → §5 stands; keep a refusal path and give it the physical
  reason.
- *Water is walkable* → there are no outdoor refusals at all; delete §5's
  path rather than carrying dead code, and say so in the chronicle.

Settlements can be marine, so "the sea is impassable" must not be assumed
in either direction.

### 6.2 What does a rhumb do at a pole?

Walk a constant `ne` course from a high-latitude cell until the position
stops changing or the course degenerates.

- *Terminates at a polar cell* → pin that cell and that behaviour in a test.
- *Cycles between two cells* → the course needs a termination rule; propose
  one in the plan, do not invent one here.
- *`bearing_to` returns a degenerate value* → the guard belongs in the
  course, not in `bearing_to`, which is a kernel function with other
  callers.

### 6.3 Do the three renderers agree after the change?

`clients/game/core/src/chart.rs` reimplements `surrounds_ascii.rs`'s
projection formula *by hand*, because `hornvale-game-core` carries no
hornvale crate in its graph by design. Its module doc records that a
plausible-looking wrong formula once passed every test in the file and was
caught only by comparing against the sim's own render
(`the_shape_matches_the_sims_own_ascii_render`).

That comparison test is the control for §4 and must be kept green across the
projection change, not rebaselined to whatever the new code emits.

### 6.4 A recycled-worktree hazard, already printed

`make worktree-take` warned that binaries under
`.claude/worktrees/the-rhumb/{target,tools/*/target}` bake the previous
occupant's path, so `env!("CARGO_MANIFEST_DIR")` and `CARGO_BIN_EXE_*` point
somewhere that no longer exists. Those failures "read exactly like a red
main and are not one." Force a rebuild of the affected crates before
trusting any red.

---

## 7. Preregistered measurement

Frozen before the code that would move it (decision 0016). Two claims, both
about the walk, both falsifiable.

**H1 — the walk does not drift.** Walking `go e` forty times from a
mid-latitude seed, the great-circle cross-track distance from the start
point's east-bearing rhumb is bounded by **one step length** (§3.2) at
*every* one of the forty steps.

The bound is one step length rather than half, because the snap error and
the step-length approximation each contribute. **The bound is the whole
test, and it discriminates on its own**: a memoryless walk accumulates
independent rounding errors, so its deviation grows as roughly the square
root of the step count — about six step lengths by step 40, comfortably
outside the bound. An earlier draft of this section added a "does not grow
monotonically" clause; that clause is worthless, because a random walk is
not monotonic either, and it would have passed for exactly the
implementation this campaign is trying not to ship.

**The positive control:** implement nearest-bearing first, in a scratch
test, and confirm H1 goes **red** against it. A bound that no
implementation ever violates is not evidence.

**H2 — every direction resolves.** For a sample of at least 200 walk-band
cells across at least 8 seeds, all eight compass points resolve to a
neighbour from every cell. A cell where any point fails to resolve
falsifies the campaign's central claim.

Sampling across seeds rather than walking one world is deliberate: one world
is an anecdote, and a triangle's orientation is exactly the kind of property
a single trajectory can fail to vary.

**A falsified prediction is a finding.** If H1 fails and nearest-bearing
turns out to be indistinguishable at the distances a player walks, that is
worth shipping as the null and taking the simpler implementation.

### 7.1 RESULT (post-hoc) — H1's second clause is FALSIFIED

**Nothing above this line has been edited.** H1 as preregistered is left
exactly as frozen; this section records what measurement returned. Rewriting
the hypothesis to match the result is the thing the freeze exists to prevent.

H1 bundled two claims, and they have different fates.

**The meridian invariant HOLDS, everywhere.** On a due-north course the
carried reckoned point never leaves its meridian, to within 5.68e-14° of
floating-point wobble in `normalize_lon`'s modulo chain — constant across
forty steps, non-accumulating. This is the dead-reckoning property, it is
what distinguishes the design from a memoryless walk, and it is the half
worth having.

**The one-step cross-track bound on the WALKED CELL is false in general.**
Measured on correct code, the walked cell's deviation from the meridian
exceeds one step length at 27 of 40 steps at some addresses, and **the error
is unbounded**: 8.8 step-lengths at step 99, 44.0 at 499, **172.6 at 1,999**,
growing linearly at ~0.086 per step.

**The mechanism is lattice-meridian alignment, not latitude.** This was
mis-diagnosed twice before it was measured — first as an equatorial effect,
then as a latitude effect — and neither survives. Two addresses at latitude
*exactly* 0.0 show zero exceedances; four between 31.7° and 58.3° show 19 of
40; and one fixture's sibling at the *identical* latitude on another base
face is among the worst in an 80-address sweep. What actually governs is the
local triad's geometry, and the lattice alternates orientations so a walker
meets two triads on alternate steps:

- Where one triad offers an edge at bearing **exactly 0.00°** and the other
  offers symmetric **±65.35°**, the walk closes into a 4-cycle with zero net
  bias and stays bounded — 0.851 step-lengths, forever.
- Where the two near-north edges are **asymmetric** (+18.0° and −47.35°,
  midpoint −14.7°), no combination of available edges points north, and bias
  accumulates without limit.

**What this changes, and what it does not.** It does not touch the design:
compass navigation still works, every direction still resolves, and the
course still holds its bearing. What it retires is the belief that a
discrete triangular lattice can track a rhumb to within a cell everywhere —
it cannot, and the residual is a property of the tiling rather than of the
navigation. A player walking a long due-north line will drift off their
meridian at a rate set by the ground they cross.

**Disclosure, because the process matters as much as the number.** The
falsification surfaced when a test fixture was moved from an address where
the assertion failed to one where it passed. That move is metric-chasing by
the standard test — it would not have been made had the assertion passed —
and it was caught only because the change was disclosed in full and a
reviewer re-derived the mechanism rather than accepting the stated one. The
fixture stays where it is; the assertion stays too, now documented as
pinning a lattice-quantization property true *at that address* and known
false in general.

**Follow-up worth a registry row, not this campaign:** whether a
bias-correcting resolution (choosing the edge that minimises *accumulated*
cross-track rather than distance to the reckoned point) would bound the
error everywhere. That is a different algorithm, not a fix to this one.

---

## 8. Out of scope, carried forward rather than dropped

This conversation settled four things that are **not** this campaign. They
are recorded so none is re-derived; each wants a registry row and the
appearance work wants its own spec.

- **The appearance protocol.** `scene/surrounds/v3` carries the possessed
  character's per-channel `Signal` — quantized at emit — instead of a
  finished `[u8;3]`; the client owns the projection to its own device. The
  cut sits after the character's eye and before the device's, because the
  eye is deterministic in-world biology and the projection is a declared
  lie (`Observer::to_srgb` already returns `Option` for this reason).
  Migration control: projecting a v3 signal through the carried projection
  must reproduce the v2 `color` byte-for-byte.
- **The vocabulary.** Nominal → colour, ordinal → glyph, epistemic →
  weight. One ordinal per band, re-meaning across rungs, capped at eleven
  characters total. Colour comes from extending `lithology::reflectance`
  from a mineral `Mixture` to a **surface** mixture — chlorophyll, litter,
  snow, sand, silt — weighted by cover and by `temperature_at(cell, day)`,
  so a peak is white in winter because its mixture changed. A nominal
  `cover` class ships alongside the continuous signal, because autotiling
  and low-colour quantization are both categorical questions a mixture
  cannot answer crisply. A client with no chromatic channel **loses** the
  nominal axis and says so in its caption; it does not reallocate the glyph.
- **The zoom ladder.** Session-remembered zoom, `-` / `+` / `=`, look-only
  above the default scale, band-crossing out of a chamber with marks
  redacted, and the snapshot channel's hardcoded `purview(0)`
  (`CLIENT-snapshot-chart-cannot-zoom`).
- **The world-map rung.** A fogged whole-planet view carrying the same three
  epistemic states the local chart does. `scene/tiles/v1` and
  `clients/atlas`' viewport already exist; an ASCII equirectangular renderer
  does not, and that is the real cost.

**Why navigation goes first:** it settles the chart's orientation before the
appearance campaign rewrites how that chart is drawn. The other order draws
the chart twice.

---

## 9. Costs and artifacts

A rendering-and-movement change is an artifact change. Regenerate with
`make rebaseline` and diff the paths `docs/generated-paths.txt` declares.
Branch table, not a prediction:

- *Only possession transcripts and gallery charts move* → expected; commit
  in the same commit as the change.
- *`clients/game/core/tests/fixtures/` also moves* → also expected once §4
  lands, since the committed session snapshots embed `SurroundsScene`;
  review the chart shape by eye before accepting.
- *`docs/audits/` moves* → a pub boundary changed; regenerate the type-audit
  report in the same commit.
- *`book/src/domesday/` or a census CSV moves* → **STOP.** Nothing here
  should reach a census. That is a signal the change leaked into a domain.

No epoch: no new stream label, no new draw, no world moves.

---

## 10. Testing

- The three renderers agree (§6.3), kept green rather than rebaselined.
- H1 and H2 as property batteries over seeds, in
  `windows/vessel/tests/`.
- A course-continuity test: `go e` twice does not re-seed `reckoned` from
  the second cell. This is the one line whose loss would silently reduce the
  campaign to the stateless variant while every availability test stayed
  green — it is the seam most worth an explicit assertion.
- Pole behaviour, pinned to whatever §6.2 finds.
- `compass_variants_must_all_be_rostered` still holds.

---

## 11. Decisions to promote

One candidate, at **0140**.

**Not 0139.** An earlier draft of this spec said 0139, reasoning from
`docs/decisions/` alone — 0137 and 0138 landed with The Glasshouse, so the
next free number *looks* like 0139. It is not: **`campaign/the-sluice`
holds 0139**, unmerged, and a Glasshouse board post says so in as many
words. Decision-number collisions are **silent** — the slugs differ, so a
merge keeps both files with no conflict marker, and the digest renders one
line per file. `no_gaps_in_the_decision_log` cannot catch it either, because
a duplicate creates no hole. The board is the only place the claim exists
before the file does, so **`ls docs/decisions/` is not sufficient to mint a
number** — sync and read the board first.

> **Compass navigation is an overlay, never the graph.** A player-facing
> heading resolves to an edge of the geodesic adjacency; it never adds,
> removes or reweights one. The three-edge graph stays the single definition
> of adjacency for every consumer that is not a player.

---

## 12. Task outline

1. **Probe** — §6.1, §6.2, §6.3 against a running possession; record real
   output. Nothing else starts until this reports.
2. **The course** — `Course`, step length, resolution, `go`'s new arm.
   H2 goes green here.
3. **Dead reckoning** — course continuity and the non-re-seeding rule. H1
   goes green here.
4. **Refusals and prose** — `exits_of`'s player-facing listing, and §5 as
   §6.1 decided it.
5. **North-up chart** — the projection, collisions, seam cells, all three
   renderers.
6. **Artifacts and close** — rebaseline against §9's branch table,
   chronicle, retrospective, registry rows for §8.
