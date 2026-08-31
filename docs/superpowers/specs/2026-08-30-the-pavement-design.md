# The Pavement — the ground itself becomes squares

**Branch:** `campaign/the-pavement`, from `origin/main` @ `4b80ad3c0` ·
**Decision block:** 0506–0515 · **Drafted:** 2026-08-30 ·
**Status:** G3 package pending.

*The Quadrat laid a square frame over irregular ground and said, correctly,
"the frame is not a claim about the ground." This campaign makes the claim.*

---

## 0. What this is

Hornvale has four square grids and one triangle, and the triangle is the one
the player spends their whole life standing on.

| band | scale | drawn as | walked on |
|---|---|---|---|
| A — interiors | feet | square | square, 4-connected |
| underground level | per-cave | square | square, 4-connected |
| **B — the walk band** | **1.87 km** | **square raster** (The Quadrat) | **triangles, 3-connected + a rhumb overlay** |
| C/D/E — map rungs | 120 km+ | square raster | not walked |

This campaign moves band B's *ground* into line with its picture, makes every
lattice in the project 8-connected, and in doing so closes a design
conversation the project recorded as stalled four campaigns ago.

**The stalled conversation is the point.** The Lexicon of Place
(`2026-08-24`, §1) says why it exists, verbatim:

> It exists because a larger design conversation (**a uniform square grid at
> every zoom rung**) stalled on the fact that the participants could not be
> sure they meant the same thing by "level", "band", or "room". The vocabulary
> is a prerequisite for that spec, not a nicety.

The vocabulary shipped — `CellId`→`Vertex`, `RoomAddr`→`Facet`, ~2,500 sites.
The spec it was a prerequisite *for* was never written. This is that spec.

## 1. The defect, read from the code

`Facet::neighbors` returns `[Facet; 3]` (`kernel/src/room.rs`). The compass
offers eight bearings. Decision 0141 resolved the mismatch by making the
compass an *overlay*: `Session::go` carries a dead-reckoned rhumb course and
snaps to whichever of the three real edges lands nearest the ideal point.

Measured on seed 42, 64 steps of `s` from the flagship start:

```
step   bearing°   km        64 steps: path 70.07 km,
   1     206.7   1.083      net displacement 62.69 km
   2     153.6   1.107      (89.5% efficient)
   3     206.7   1.083      mean step 1.095 km
   4     153.6   1.107
   …  (alternating, forever)
```

Not one step is southward. The walk tacks. 0141's own Consequences record the
residual as the campaign's headline finding: the walked cell's error from the
ideal rhumb is **unbounded** — 172.6 step-lengths at 2,000 steps, growing
~0.086 per step — and the mechanism is the local triad's alignment, not
latitude, so the behaviour is *qualitatively different on different ground*.

Six days after 0141, The Stride made arrow keys the startup default. The
mismatch is now in the player's hands.

### 1.1 0141's blocking argument was never true

0141 rejected a grid on this basis:

> Adding edges (a 12-neighbour lattice) or reweighting them both make `go e`
> work by changing what "adjacent" means — and every non-player consumer would
> inherit it silently: path costs move, ecology's spread changes, settlement
> fitting sees a different neighbourhood.

At `92a4b059e`, the commit that added 0141, `git grep '\.neighbors()' --
domains/` returns **nothing**. Same at HEAD. Ecology is `VertexMap`/`Vertex`
(`kernel/src/ecology.rs:484`); `domains/settlement` contains no mesh type at
all; `hops_between` is `Geosphere`; `kernel/src/astar.rs` is generic over its
state space and says so in its own doc. Face adjacency has never had a domain
consumer.

`docs/CLAUDE.md` sets "new information" as the bar for reopening a ratified
decision. This is that information. **0506 supersedes 0141** — appended, never
edited.

## 2. The model: two meshes, one for fields and one for feet

The primal/dual confusion the Lexicon of Place named ("`Geosphere` is an
icosphere; `Vertex` is a vertex; `Facet` is a face") resolves into a clean
split once the two are allowed to be *different meshes* rather than the
primal and dual of one:

- **The icosphere stays the FIELD substrate.** `Geosphere`, `Vertex`, the
  `--globe-level` pin, and every consumer in `domains/` — terrain, plates,
  drainage, channels, climate, ecology, settlement fitting, paleoclimate.
  **Nothing here changes.** This is decision 0038's canonical grid, and 0038
  is what licenses the split: *"The canonical grid bears identity … Fields are
  resolution-free: any grid at any level samples the same underlying field."*

- **A cube-sphere becomes the OCCUPANCY lattice.** `Facet` — where a body is,
  and how it moves. Squares, 8-connected, sampling the fields above by
  position.

`windows/scene/src/region.rs` already carries a cube-sphere quadtree, and its
face bases, addressing and quadtree descent transfer directly. **Its
projection does not, and this is the one place the first draft of this spec
was wrong.**

### 2.0 The projection must be tangent-warped, and the naive one is a trap

A cube-sphere maps a face's `(a, b) ∈ [-1, 1]²` grid onto the sphere. The
obvious map — `normalize(n + a·û + b·v̂)`, which is what `region.rs` does —
bunches cells at the face corners and stretches them at the face centre.
Measured, sweeping one face and converging in `N`:

```
    N      naive    tan-warped        max/min CELL AREA across one face
    8     3.740x       1.276x
   32     4.856x       1.379x
  128     5.114x       1.406x
  256     5.155x       1.410x         naive -> 3*sqrt(3) = 5.196 (analytic)
```

**The naive cube-sphere is ~5.2x area distortion — worse than the icosphere
it replaces**, whose own room-mesh spec (§13.4) records "centre children
~1.5-2x the corner children". Adopting it verbatim would have made the very
property this campaign exists to improve strictly worse, while the spec
claimed the opposite.

The fix is the standard tangent warp, applied to each face parameter before
projection:

```
a' = tan(a * PI/4)      b' = tan(b * PI/4)      p = normalize(n + a'*u + b'*v)
```

which measures **1.41x** — better than the icosphere, and the "relatively
undistorted local grid" the campaign was asked for. The inverse uses `atan`
and is equally exact.

**This costs the transcendental-free property, and that is affordable but must
be stated.** `locate_on_cube` today is dot products and one division, so it is
byte-identical across platforms by construction. With the warp it calls `tan`
and `atan`, which under decision 0041 route through `kernel/src/math.rs`'s
pure-Rust `libm` — bit-identical across platforms by the same guarantee every
other transcendental in this project already relies on. It is a weaker
guarantee (a library contract rather than an arithmetic one) traded for a 3.7x
reduction in distortion.

**What it breaks — narrower than the first draft implied.** The game client's
world plate does **not** draw through `region.rs`: `plate.rs` holds zero
references to `RegionScene`/`tiles_region_scene` and projects through
`crate::mercator` (verified with The Legend). So the 5.2x figure does not
reach the game client at all. It reaches **`clients/atlas`**, which consumes
the wire, and `scene/tiles-region/v1` is a wire schema, so warping changes what
a tile means.

**The schema has MOVED this week and a reissue must carry the movement.**
`RegionScene` now carries two fields appended after `precip_mm_yr`
(`windows/scene/src/region.rs:338,341`, verified):

```rust
pub relief: Vec<u32>,
pub relief_legend: Vec<String>,
```

— the six-band `relief_band` classification, nearest-vertex like
`ocean`/`biome`/`plate`. **Built through `ReferenceElevation::above`, never
`SeaLevelHeight::from_metres`**, whose own doc calls itself "the hole through
which the datum-confusion class returns" and which this crate has already
shipped as a bug once. Any reissue preserves both the fields and that datum
discipline.

**One downstream conclusion of The Legend's own spec is weakened by this, and
it should know before it hardens.** Its §3.0 ruled against the plate fetching
`RegionScene` on two grounds: that `RegionScene` resamples geosphere values
onto a *different* mesh, so a plate fetching it would resample twice
(violating 0287), and 0196's never-invent-detail rule. **This campaign removes
the first objection** — after it there is one mesh, so there is no second
resample — and leaves 0196 standing alone. Its "share a function, not a fetch"
conclusion may still be right, but it now rests on one leg instead of two. Decision 0356 retired the external clients, so the cross-repo
"additive-or-versioned" constraint that would once have forced a version bump
has lapsed — but the two projections must not silently disagree inside this
repo. Either `region.rs` adopts the warp (one projection, one definition,
`scene/tiles-region` reissued) or the campaign keeps two and names which is
which. **G3 flagged; recommendation is one projection.**

### 2.1 The address space survives untouched

This is what makes the change tractable rather than a rewrite.

```rust
pub struct Facet { face: u8, path: Vec<u8> }         // UNCHANGED
pack() = (pathword << 5) | face                       // UNCHANGED (5 face bits)
child(digit)  where digit < 4                         // UNCHANGED
parent(), depth(), prefix containment                 // UNCHANGED
```

A quad subdivides into four quads exactly as a triangle subdivides into four
triangles. `pack` reserves five bits of face and uses 20 of 32; a cube needs
6. So `pack`/`unpack`, `parent`, `child`, the 0077 zoom ladder ("zoom is path
truncation, never aggregation"), the epistemic fog's integer prefix test, and
`map out N` all carry over **without edit**.

What changes in `kernel/src/room.rs`:

| item | today | after |
|---|---|---|
| base faces | 20 triangles | 6 cube faces (`region.rs`'s `FACES`, verbatim) |
| `neighbors()` | `[Facet; 3]` | 8 (7 at the eight cube corners) |
| `corners()` | 3 points | 4 points |
| `face_lattice()` | barycentric triple + `up` | integer `(x, y)` at `scale = 2^depth` |
| `containing()` | barycentric descent | `locate_on_cube` + dyadic bisection |
| `coord()` | triangle centroid | face-unit centre |

### 2.2 The neighbour walk

Three cases, and the third must be stated honestly rather than smoothed over:

1. **Interior to a face** — `(x ± 1, y ± 1)`, all eight, trivially.
2. **Across a face seam** — the standard cube-map edge adjacency (12 edges),
   each carrying a fixed rotation between the two faces' `(u, v)` bases. A
   table of 12, integer-only, testable exhaustively.
3. **At the eight cube corners** — three faces meet at a point, so a corner
   cell has **seven** neighbours, not eight. The missing one is a diagonal
   that does not exist.

Case 3 is the cube's equivalent of the icosphere's twelve pentagons, and it is
strictly better: 8 singular points instead of 12, and they are *corners of the
world* rather than points scattered across it. Precedent for how to handle it
is 0141's own treatment of the pole: *"No termination rule was invented,
because inventing one would be inventing a defect to fix."* A corner cell
reports seven neighbours. A step into the absent diagonal is refused with the
geometry as the reason. No special case leaks upward.

### 2.3 Walk depth

Measured, not assumed:

```
ICOSPHERE (today)                    CUBE-SPHERE (after)
 depth 12: 1.874 km side              depth 12: 2.251 km side
                                      depth 13: 1.126 km side
 effective step today = 1.08 km centre-to-centre
 (measured: 1.083 / 1.107 km alternating, seed 42)
```

`walk_depth = globe_level + 6` today (`windows/vessel/src/agent.rs:16`). On the
cube base it becomes **`globe_level + 7`** — depth 13, 1.126 km — because that
is what preserves the step length, and therefore keeps `clock.rs`'s authored
0.1-day `MoveTo` and every duration calibrated against it honest. Depth 12
would silently double the ground covered per step.

## 3. Movement

### 3.1 Eight headings, everywhere

Band B, band A (interiors) and the underground level all become 8-connected.
`cell_delta` (`session.rs:6578`) currently returns `None` for all four
diagonals; it returns offsets for all eight.

### 3.2 Octile cost

`windows/vessel/src/clock.rs:166` charges a **flat** cost per step:

```rust
Action::MoveTo(_) => TickSpan::from_ticks(10_000),
```

independent of distance. On an 8-connected lattice with a flat cost, a
diagonal buys √2 ≈ 1.414× the ground for the same time, so zigzagging travels
41% faster than walking straight. In a project that charges movement against
body mass and terrain, **that is a physics defect, not a UX preference**, and
it is latent in the clock the moment diagonals exist.

A diagonal step therefore costs √2 orthogonal steps, at both scales
(`MoveTo` and `MoveWithin`). One multiplier, at the two sites that already
modulate by mass and climb.

### 3.3 The corner rule

A diagonal is **refused when both flanking orthogonal cells are impassable**,
and **permitted when either is open**.

That is the physical reading: passing through the point where two walls meet
is not a way through a building; brushing one corner is. The sentence already
exists and is already phrased as geometry rather than as a rule —
`UNDERGROUND_DIAGONAL_REFUSAL`: *"There is no slipping through a corner down
here either."* Its indoor twin's doc states the standard this campaign holds
to: *"Refused with the geometry as the reason, not with a parse complaint."*

Two rejected alternatives, recorded: refusing when *either* flank is
impassable is stricter than the physical claim; permitting always
reintroduces the exploit §3.2 exists to close.

### 3.4 The rhumb course is deleted, not adapted

`windows/vessel/src/course.rs` exists to resolve eight bearings against three
edges. With eight real edges the mapping is the identity and the module's
whole subject is gone. `Course`, `rhumb_advance`, `step_length_rad`,
`nearest_neighbour`, `POLE_LIMIT` and `Session.course` are removed; the
`back` verb's course-clearing goes with them.

`windows/vessel/tests/suite/course_properties.rs` is deleted, not ported: its
H1 measured drift from an ideal rhumb, and after this campaign there is no
approximation to drift from. **This is a hypothesis whose subject is removed,
not a hypothesis confirmed** — the same distinction The Quadrat drew about its
own H2, and it must be reported that way.

### 3.5 Input

The Stride's ratified key routing is an **input** decision, not a geometry one,
and survives as the primary binding:

- Arrows stay 4-way and primary.
- Every destination remains reachable by cardinals alone, so diagonals are a
  convenience, never a competence requirement — which is what makes an awkward
  binding acceptable on a laptop with no numpad.
- Diagonals are bound (`y`/`u`/`b`/`n`, the vi-keys, plus the CLI's existing
  `ne`/`nw`/`se`/`sw` words which already parse).
- Travel/pathing commands use all eight.

## 4. The epoch

Authorised explicitly (ledger #2). `Facet` values change meaning, so:

- every committed `agent-at` place changes;
- a world file written before the flip **does not load** — deliberately, on
  0189's precedent, and it is regenerated from its seed and pins;
- `vessel/session/v2` fixtures, the seed-42 gallery transcripts, the committed
  almanacs and `scene/surrounds` goldens all rebaseline.
- **Volume, not risk, and it stacks with a campaign already in flight.** The
  Legend reports having already moved `windows/scene/tests/fixtures/region-
  seed-1-f0-l3.json`, `book/src/gallery/scene-tiles-region-seed-42.json`, the
  seed-42 session fixtures and `docs/audits/glyph-specimen-sheet.txt`; all
  rebaseline again under this flip. None is a determinism contract.

**What does NOT move, and this is the load-bearing half:** no seed label, no
stream label, no draw, no stream consumption order. The occupancy lattice
consumes no randomness. Terrain, climate and settlement all still generate on
the icosphere from the same streams in the same order, so **the world itself
is unchanged** — only the addressing of where you stand in it.

## 5. What this does not touch

- `Geosphere`, `Vertex`, `--globe-level`, and every crate in `domains/`.
- The `ALLOWED_EXTERNAL` dependency allowlist; nothing new is added.
- `scene/tiles-region/v1` and `RegionAddr`. After this campaign `Facet` *is* a
  cube-face quadtree and `RegionAddr` is a second name for one thing, but
  unifying them touches a wire schema. Recorded as a followup, not scope.
- `windows/scene/src/surrounds_ascii.rs`. Decision 0290 records that The
  Quadrat's square grid landed in `clients/game` only and the sim's own ASCII
  chart deliberately stayed a polar ring projection. Whether it follows the
  ground is a separate call, and is a G3 flagged item below.

## 6. Refusals

- A step into a cube corner's absent eighth neighbour: refused, geometry as
  the reason (§2.2).
- A diagonal through a two-walled corner: refused, geometry as the reason
  (§3.3).
- A rung finer than walk depth: unchanged — 0196 forbids inventing detail
  below the datum.

## 7. Preregistered measurement

Frozen before the code exists (decision 0016). A falsified prediction is a
finding.

**H1 — a held heading walks true.** From 200 distinct seed-42 start cells,
walking `n` for 500 steps leaves the walker within **0.5 cell** of the
starting meridian at every step. This is the direct repair of The Rhumb's
falsified H1 (unbounded drift, ~0.086 step-lengths per step). *Positive
control:* the same probe on today's triangular lattice must reproduce the
unbounded drift, or the probe is not measuring what it claims.

**H2 — octile cost closes the speed exploit.** Travelling `k` cells diagonally
costs within 0.5% of `√2 · k` orthogonal steps' worth of ticks, across the
body-mass and climb range. *Positive control:* with the multiplier removed,
the same probe shows the 41% discrepancy.

**H3a — the named casualty: a test whose PREMISE dissolves.** Reported by
The Legend on the wire, verified in its tree at
`clients/game/bin/src/plate.rs:1918`:

```rust
let point_vertex = index.nearest(&geo, lat, lon);
let tile = terrain_at_tile(...);
total += 1;
if tile.vertex == point_vertex { agree += 1; }
```

with `assert_eq!(agree, total)` — mesh addressing reproduces
`NearestVertexIndex::nearest` **exactly**. Under a cube-sphere a cell's corners
are not geosphere vertices, so there is no `tile.vertex` for that equality to
be about. **It does not get slower; it stops meaning anything**, and it must be
reported as dissolved rather than passed — the same discipline The Quadrat drew
about its own H2 and §3.4 draws about `course_properties`. It is also *more*
exposed than The Quadrat left it: The Legend retargeted it this week from
comparing drawn glyphs to comparing vertices, because the glyph form was a
weaker proxy (two distinct vertices both ocean draw the same glyph, so it
passed even when addressing disagreed).

**0287 SURVIVES; ITS COROLLARY IS THE CASUALTY, and the distinction is worth
stating precisely.** Decision 0287 holds that "a zoom rung is a refinement
depth of the facet tree — a tile at rung `d` is a facet at depth `d`." That is
untouched here: a tile is still a facet at a depth, the facet is simply square.
What breaks is the *corollary* The Quadrat drew from it — that a facet's
corners are geosphere vertices, and terrain therefore needs no spatial search.
Nothing in 0287's own text asserts that corollary. This campaign owes it a
recorded amendment, not a supersession.

**H3b — the plate-draw budget after losing that corollary.** The Quadrat's
headline was that "a tile IS a facet", so terrain came from that facet's three
corner *geosphere vertices* by direct addressing: 1,960,000 vertex candidates
scanned falling to 90 on a 200×200 plate. **A cube-sphere cell's corners are
not geosphere vertices**, so sampling returns to `NearestVertexIndex` lookups.
`region.rs` already does exactly this for the tiles path, so the machinery
exists — the budget does not. **No success threshold is preregistered for H3b**,
deliberately: this is a measurement whose result informs a decision, not a
prediction to pass or fail.

**The baseline must not be taken from The Quadrat's published figures.** That
campaign measured 65.3 / 65.6 / 70.3 / 91.5 ms for one uncached code path — a
**1.40x spread** — and explicitly refused to pin a fifth number, recording the
path as load-sensitive instead. Quoting any one of them as "the" baseline
would repeat the error `docs/CLAUDE.md` records against reading a cost off
prose. The Legend's Task 11 measures a fresh warm redraw with replicates
against a preregistered 0.20 ms bar, on the same code path this campaign would
regress and including the two O(1) per-tile lookups its own branch adds.
**That is H3b's baseline; take it from there, or take it fresh, and never from
a remembered figure.**

## 8. Testing

- Exhaustive: the 12 seam-edge rotations, and all 8 corner cells reporting
  exactly 7 neighbours.
- Property: `neighbors()` is symmetric (`b ∈ neighbors(a) ⟺ a ∈ neighbors(b)`)
  at every depth and across every seam — the invariant that catches a
  rotation table off by one turn.
- Property: `containing(centroid(f), depth(f)) == f`, the round trip
  `Facet::containing` already holds.
- Byte-identity: `locate_on_cube` used by both `region.rs` and `room.rs`
  returns identical bytes for the same position — one definition, asserted,
  not two that happen to agree.
- The corner rule in both directions: one flank open ⇒ permitted; both walled
  ⇒ refused. (The one-directional version of this test is the trap
  `docs/CLAUDE.md` names: a check asserting only refusal is blind to
  over-refusal.)

## 9. Decisions to promote (block 0506–0515)

- **0506** — The occupancy lattice is a cube-sphere; the icosphere stays the
  field substrate. *Supersedes 0141.*
- **0507** — Every lattice in the project is 8-connected.
- **0508** — A diagonal costs √2, because the movement clock is flat.
- **0509** — A diagonal through a two-walled corner is refused; one open flank
  permits it.
- **0510** — Compass input is 4-way primary and 8-way capable; no destination
  requires a diagonal.
- **0511** — Walk depth is `globe_level + 7`, chosen to preserve step length.
- **0512** — The cube-sphere projection is tangent-warped, trading the
  transcendental-free property for a 3.7x reduction in area distortion; there
  is one projection in the repository, not two.

## 10. Task outline

1. Cube base geometry in `kernel/src/room.rs` — `FACES`, the §2.0 tangent
   warp, `coord`, `corners`, `containing`, `face_lattice`; `pack`/`child`/
   `parent` untouched. Re-measure the distortion table against the shipped
   code, not against the spec's Python.
1b. Reconcile `region.rs` to one projection (0512).
2. The neighbour walk — interior, the 12 seams, the 8 corners; symmetry
   property test.
3. `walk_depth` → `globe_level + 7`; the §2.3 measurement.
4. Delete `course.rs` and its suite; `go` resolves a heading directly.
5. `cell_delta` → eight; the corner rule at all three lattice bands.
6. Octile cost in `clock.rs`, both movement scales.
7. Input: vi-key diagonals; pathing over all eight.
8. Rebaseline every golden; the epoch note in `streams::reload_notice`.
9. H1/H2/H3 probes and their positive controls.
10. Book: chronicle, the Confidence Gradient re-score, the Lexicon of Place's
    stalled-conversation note closed.
