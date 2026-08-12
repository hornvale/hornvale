# The Rill — the drainage network is space-filling and self-similar

*Campaign spec. Status: G3 review. Follows [The Ford stage
1](2026-08-10-the-ford-design.md) and [stage
2](2026-08-11-the-ford-stage-2-design.md), and supersedes the scoped campaign
drafted as "The Seep", which is folded in here as one consumer.*

## 1. What this campaign is for

The Ford established that a river is a **line, not an area**, and that a
polyline with a width function is the same object at 110 km and at 27 m. It
then drew only the lines above a discharge threshold. This campaign makes the
network **space-filling** — small streams flowing into creeks flowing into
rivers, with divides between them at every scale — because that is what a
drainage network is, and because a room cannot have a stream through it while
the finest watercourse in the model is 110 km from the next one.

Two facts about the existing world drive the design.

**The branches already exist and are discarded at render time.**
`TectonicGlobe.downhill` is defined for *every* cell (`None` only on ocean and
terminal sinks) and `drainage` (flow accumulation) is computed for every cell.
`RIVER_MIN_DRAINAGE = 15.0` makes rivers ~6.7% of seed 42's land, and
`ChannelNetwork::build` renders only those. The world computes a complete
space-filling flow tree over all land and draws one fifteenth of it.

**Rendering all of it is still ~50× short of real drainage density.** A
level-6 cell is ~110 km across and a walk-depth room ~1.7 km, so a
through-going line crosses ~65 of a cell's ~4,200 rooms. Drawing every land
cell's flow line takes rooms-with-a-watercourse from ~0.1% to ~1.6%. Real
terrain has a stream every few hundred metres to a few kilometres. So
structure **below** cell scale is required, and that is generation rather than
derivation.

## 2. Keystone

> **The coarse flow graph is a boundary condition, not a suggestion.**

`MAP-subcell-hydrology` is **rejected** — refining elevation per cell and
recomputing flow accumulation below the cell floor forces laziness and, in the
registry's own words, *"a lazily-refined flow field carries no guarantee of
agreeing with the coarse answer it constitutionally may not contradict."*

This campaign does not recompute flow. It takes the coarse graph's answer as a
**constraint on the fine one**: inside a cell, the network must collect what
the coarse graph says flows in, and deliver it to the single neighbour the
coarse graph says it flows out to. Agreement is then structural rather than
hoped for, and "coarse constrains fine" is satisfied the way The Ford
satisfied it — by construction, not by a conservation test.

This is a materially different claim from the rejected row and the spec should
be read as reopening nothing: `MAP-subcell-hydrology` proposed a **new fine
model**; this proposes a **rendering of the existing coarse model at finer
resolution under its own constraints**.

## 3. What must not move

Asserted by test. These are the falsifiers for §2's framing — if any moves,
the campaign has become the rejected row wearing a new name.

1. **The coarse flow graph is unchanged.** `downhill`, `drainage`,
   `endorheic`, and cell-scale `WaterKind` are read, never rewritten. No new
   elevation model, no re-accumulation.
2. **Cell-scale answers are unchanged.** A query at level 6 returns what it
   returns today, up to the width-law change in §4.1 which is stated and
   measured rather than incidental.
3. **No new seed draws in existing streams, and no reordering.** New
   generation gets its own stream label; it does not borrow an existing one.
   In particular `LOCALE_MICRO`'s four draws keep their count and order (§5.3).
4. **No epoch on any schema.** Values move; shapes do not.

## 4. Design

### 4.1 Tier 1 — render the tree that exists

Every land cell with a downhill target joins a run, and runs are rendered as
polylines exactly as today. The network goes from ~700 seed-42 cells to
~10,450.

**This reverses a documented deliberate choice.** `channel_half_width` returns
**exactly `0.0`** below `RIVER_MIN_DRAINAGE`, commented *"a sub-threshold
trickle is not a channel."* Because every band edge derives from that
half-width, a sub-threshold line would read `Dry` at its own centre and carry
no bank, floodplain or terrace. Tier 1 requires the width law to extend below
the threshold: a creek is a **narrow** channel, not an absent one. That
reversal wants a decision record, claimed after checking the log for the next
free number (the contiguity guard in `cli/tests/docs_consistency.rs` makes a
gap or duplicate a red gate).

`WaterKind::River` is **not** widened to match. A cell can carry a rendered
watercourse and still classify as `DryLand`, exactly as ~49.6% of river cells
already disagree with their own transverse band — that disagreement is
documented, measured, and stays.

**Carried in from the retired Seep draft, because it is a prerequisite here
rather than an aside:** `build` stops a run *before* a non-river downhill
target and then discards runs shorter than two cells, which is why 39 of seed
42's 700 river cells carry no polyline (stage 1 verified the 39-and-39 as the
same set, by intersection). A run must **include the cell it drains into**, so
every run reaches its outlet and the singleton special case disappears. Under
Tier 1 this matters more, not less: with all land cells in the network, the
number of runs terminating at a non-river cell rises sharply.

### 4.2 Tier 2 — the drainage tree rides the room mesh

`RoomAddr::corners()` is a deterministic 4-way descent whose doc states it is
**byte-identical to the same face in `Geosphere::new(self.path.len())`**. The
room mesh and the geosphere are one lattice, so a room at depth *d* **is** a
level-*d* cell. Drainage can therefore subdivide exactly as rooms do, and the
topology forces most of the answer:

- a parent flows out through **one** edge;
- the **central** child (`digit 3`, `[ab, bc, ca]`) touches no parent edge, so
  it can never be the outlet and must drain into a corner child;
- the corner child on the parent's outflow edge **is** the outlet, forced;
- the remaining children form a tree draining into it.

The only freedom is which spanning tree the remaining children form. That is a
seeded draw from a small, enumerable set of valid configurations — bounded,
deterministic, and resolvable from a room's address in `O(depth)` with nothing
stored: descend the path, resolving flow at each level. The same rule at every
level is what makes the result self-similar rather than merely fractal-looking.

Inflow boundary conditions come from the coarse graph the same way: a parent
edge across which a neighbour flows *in* is an inlet, and the child owning
that edge receives it.

### 4.3 The width law is already scale-free — asserted, not assumed

**This section previously said the opposite, and it was wrong.** It claimed
`drainage` being an upstream cell count made the width law scale-dependent, so
`Q` had to become a drained area. Measurement at Task 1 falsified it, and the
reason is geometric rather than incidental.

Cells tile the sphere, so `N` cells have mean area `4π/N`, and a locally
hexagonal tiling has nearest-neighbour spacing `d = √(2/√3)·√A = 1.0746·√A`.
Measured across levels 4–7, `cell_spacing / √(4π/cell_count)` is **1.07824 at
every level**, constant to five digits — the hexagonal packing constant, off by
0.34% from the twelve pentagons and the curvature. So

```
  w = a · edge · √count
    = a · 1.0746 · √A_cell · √count
    = (a · 1.0746) · √(A_cell · count)
    = (a · 1.0746) · √(drained area)
```

`cell_edge` **already carries the count→area conversion.** Multiplying by area
while keeping `edge` would apply the grid factor twice, rescaling every width
by `√(N₆/N_L)` — ×2 at level 5, **×1/64 at level 12** — which is the scale
error this campaign exists to remove, sign-flipped.

Two consequences the rest of the design depends on:

1. **No units change is needed, and none is made.** `TectonicGlobe.drainage`
   stays a count, `channel_half_width` and `band_edges` keep their signatures,
   and `CHANNEL_WIDTH_COEFF` is not recalibrated. `RIVER_MIN_DRAINAGE` keeps
   comparing against a count, which it must — expressed as a steradian area,
   `15.0` would exceed every discharge in the world and zero every channel.
2. **Tier 2 must supply LOCAL units.** Scale-freeness holds only when the
   accumulated count and the spacing are at the *same* level. Below cell scale
   there is no `Geosphere` to ask (level 12 would be ~671 million cells), so
   the subdivision must derive its spacing from `RoomAddr::corners()` and
   accumulate counts in its own sub-triangle units. Mixing a sub-cell count
   with a cell-scale spacing reintroduces exactly the error this section
   originally imagined.

The invariant is therefore promoted from an assumption to an asserted property
(R-3), because Tier 2's correctness rests on it.

### 4.4 The consumer — wetness reads the network

`micro_field` draws four axes off `LOCALE_MICRO` and consults the world for
none of them, while the same document carries `fields.moisture`, real climate
moisture blended from the room's corner cells. The descriptor reads the noise,
so a room can render "damp" in a desert and "dry" on a riverbank.

With a space-filling network in hand, `wetness` becomes a **budget and an
allocation**: climate supplies water per cell; position relative to the local
watercourse redistributes it. This is the Topographic Wetness Index's shape —
wet where water collects, dry where it sheds — without needing TWI's upslope
integral, because the network already says where the water is.

The coherence this buys is the point: once the variety clause and the habitat
clause read **the same number**, a room cannot render "a stream gully, dry".
The contradiction becomes unreachable rather than guarded against.

**`relief`, `aspect` and `openness` stay noise.** Each may deserve grounding;
each is a separate measurable change; bundling them would hide three unmeasured
effects inside one prose drift.

## 5. Determinism and save format

- **A new stream label** for the sub-cell spanning-tree draw, declared in the
  owning crate's `streams` module and published through `stream_labels()` into
  the generated manifest. New label, not a borrowed one.
- **`LOCALE_MICRO` keeps its four draws in order.** Deriving `wetness` instead
  of drawing it would remove a draw and silently shift `openness` in every room
  of every world. The draw is kept and **spent differently** — as local
  variation about a grounded value.
- **No epoch.** `locale/room/v2` and `scene/surrounds/v2` keep their tags;
  `wetness` is an existing key whose value changes. `scene/surrounds/v2`
  reaches the external Orrery client, so this is a **cross-repo value change** —
  legal, since additive-or-versioned governs shape, and worth stating rather
  than discovering.
- **Quantize at emit only.** Sub-cell flow resolution runs at full precision.
- **Every committed artifact that touches water moves**, including the census's
  five channel columns — so a census refresh on lefford is part of the close,
  and The Ford's H1/H2/H4 need re-measuring rather than re-recording.

## 6. Preregistered hypotheses

Frozen before the code (decision 0016). Each states a floor and a ceiling and
names where its reference comes from. Where a claim is true by construction it
is labelled a **witness**, not a hypothesis test.

- **R-1 — the network is space-filling.** Every land cell with a downhill
  target appears in some run. Floor **100%**, on **≥ 3 seeds**. Reference: the
  set intersection over `water_kind`/`run_cells` stage 1 used, outside `build`.
  It can fail: terminal sinks have no downhill and are the known exception.
- **R-2 — every run reaches its outlet.** The last vertex of every polyline is
  a cell that has no downhill target, or is the outlet it drains into, or was
  already claimed by another run. Reference: `TectonicGlobe.downhill`.
- **R-3 — the width law is invariant under a change of level.** For a fixed
  physical drained area, the rendered width is the same whichever level's units
  express it: `w(count, edge)` equals `w(4·count, edge/2)` to within
  quantization, across at least six doublings. This is stated over the **law**,
  not over the world — the drainage field itself changes with resolution
  because a finer elevation field routes flow differently, so a same-basin
  comparison across levels measures physics as well as units and cannot
  isolate this claim. R-3 is the invariant Tier 2's correctness rests on, and
  it fails loudly if a sub-cell count is ever paired with a cell-scale spacing.
  **Additionally:** for every subdivided cell, the sum of its children's own
  areas plus its inflows equals its own accumulation, to within quantization —
  conservation under subdivision, which is a separate claim from invariance
  under rescaling and can fail independently.
- **R-4 — the fine network cannot contradict the coarse one.** For every
  subdivided cell, the sub-network has exactly one outlet, on the parent's
  outflow edge, and no child drains across a parent edge that is not an
  inlet or the outlet. **100%.** This is a test of whether the implementation
  honours §4.2's forced topology, not of the design — the design cannot
  express a violation, so a failure means a coding error.
- **R-5 — the generated network obeys Horton's laws.** Under Strahler
  ordering at walk depth, the **bifurcation ratio** `R_b` lies in
  **[3.0, 5.0]** and the **length ratio** `R_l` in **[1.5, 3.5]**, on **≥ 3
  seeds**. *This is the campaign's central claim and its reference is entirely
  outside this codebase* — those intervals are the empirically observed ranges
  for real river networks, not values fitted here. A generated network that
  merely looks branchy will fail them.
- **R-6 — a walk gets damper as it descends.** Over sampled descending walks
  of ≥ 8 rooms at walk depth, `wetness` is non-decreasing in at least **80%**
  of steps. Reference: the elevation the walk descends, outside the wetness
  computation.
- **R-7 — the contradiction is unreachable.** No sampled room renders a
  riparian variety clause together with a "dry" habitat clause. **0
  occurrences** — a **witness**, since both read the same number once §4.4
  lands; it exists to catch a regression that reintroduces two sources.

## 7. What this campaign does not do

- **No riparian variant conditioning.** The Ford's stage 3 becomes much
  smaller once wetness is coherent, and stays a separate campaign so its effect
  on variant mass is attributable. Note for that campaign: **its preregistered
  H5 cannot detect what it most plausibly gets wrong** — H5 measures
  *formation* fractions while conditioning moves *variants within* a formation,
  so it reads null regardless. H5 is an overreach guard, not a check the
  feature worked.
- **No scene emission and no client work.** Unchanged from The Ford's staging.
- **No change to `WaterKind`**, no re-accumulation, no new elevation model.
- **No seasonality.** `MAP-seasonal-band-stage` stays unbuilt; keep discharge
  an argument to the band functions, never baked into a stored width.

## 8. Tasks

1. **Discharge as area** — §4.3, the recalibration, and R-3. Nothing renders
   differently yet; this is the units change alone.
2. **Every run reaches its outlet** — §4.1's carried repair, R-2.
3. **Tier 1: render the whole tree** — the width law below threshold, the
   decision record, R-1.
4. **Tier 2: subdivision under boundary conditions** — §4.2, R-4, R-5.
5. **Wetness reads the network** — §4.4, R-6, R-7. Prose moves only here,
   which is what makes its effect attributable.
6. **Close** — gate, drift, census refresh, The Ford's H1/H2/H4 re-measured,
   chronicle, retrospective, Confidence Gradient.

Tasks 1–4 cannot move prose, so they are collectively a clean before-arm for
task 5. That sequencing is deliberate and is the campaign's attribution
strategy.

## 9. Risks

1. **§4.3 is the load-bearing correctness claim.** If `Q` stays a count, every
   sub-cell width is wrong by a factor of 4096 per six levels and the network
   will still look plausible. R-3 is the guard and it must be written before
   the subdivision, not after.
2. **R-5 is the campaign's honesty check and it may fail.** Horton ratios are
   an external standard; a generated network can easily land outside them. A
   failure is a finding to ship, not a reason to retune the spanning-tree
   draw — and any post-unblinding change to the generator must be counted and
   disclosed, not merely justified one at a time.
3. **The blast radius is the largest of the arc.** Every channel reading in
   every world moves. The census refresh and the H1/H2/H4 re-measurement are
   part of the campaign, not the close's paperwork.
4. **Cost at query time.** Resolving flow to walk depth is `O(depth)` per room
   with no storage, but it lands inside `describe`, which is already on the
   surrounds hot path and already pays an `O(network)` scan. The plan must
   carry a before-arm measurement, and `RoomMeshMemo` is the caching seam if
   one is needed.
5. **Tier 2 is invention.** It is constrained invention, and §3's invariants
   are what keep it honest — but a reviewer should judge at G3 whether the
   boundary-condition argument genuinely distinguishes this from the rejected
   `MAP-subcell-hydrology`, rather than accepting the distinction because this
   spec asserts it.
