# The Ford — authoritative sub-cell water

*Campaign spec. Status: G3 review. Autopilot engaged; ledger at
`.superpowers/sdd/decision-ledger.md` (worktree scratch, promoted into the
retrospective at close).*

## 1. The defect

A river in Hornvale is roughly 110 km wide, at every scale, in every client.

Three readings establish it, and the third is the one that hurts:

- `windows/scene/src/lib.rs:522` — the global raster takes water by
  nearest-cell lookup on the canonical level-6 geosphere
  (`GLOBE_LEVEL = 6`, `domains/terrain/src/lib.rs:65`).
- `windows/scene/src/region.rs:407` — the deep-zoom regional tile, which
  exists precisely to sample below the global lattice, takes water by the
  *same* nearest-cell lookup. Descending does not narrow the river.
- `windows/locale/src/lib.rs:540` — a room inherits its containing cell's
  `WaterKind` categorically, "never blended". At an L18 room edge of ~27 m,
  the room reports the class of a cell four thousand times its width.

Zooming cannot fix this, and the reason is worth stating exactly, because it
rules out a whole family of repairs. `windows/locale/src/lib.rs:523-527`
blends the continuous fields as an integer-weighted mean over the room's three
corner cells — linear barycentric interpolation. **A linear interpolant on a
simplex attains its extrema at the vertices**, so no interior minimum exists
anywhere below the cell floor. There is no sub-cell valley for a river to
occupy. Detail below the floor must be *added*; it can never be recovered by
interpolating what is already there.

The sub-cell machinery that does exist cannot help either.
`windows/locale/src/micro.rs:11-20` gives every room a `wetness` axis drawn as
`s.next_f64()` off the room's own address seed — **spatially uncorrelated white
noise**. Sibling rooms are independent by construction (the test at
`micro.rs:43` asserts exactly that), so no connected watercourse can ever
emerge from it.

And the consequence reaches the prose. `windows/locale/src/grammar.rs:275-291`
draws a room's variety from `(formation, stratum, substrate)` and the room
seed, with **water as no part of the input**. So `Variant::GalleryForest`,
documented at `domains/climate/src/variants.rs:278` as *"Forest following a
watercourse,"* is drawn 2-in-7 in any tropical rainforest room, watercourse or
not.

## 2. Keystone — river-as-area is a type error

The carrier of a hydrographic feature should match its dimensionality:

| mesh element | feature | correct today? |
|---|---|---|
| vertex | spring, pond | unmodelled |
| **edge** | **river** | **no — stored as a face** |
| face | ocean, lake, playa | yes |

Ocean-as-face is right. Salt-basin-as-face is right. River-as-face is the
category mistake, and ~110 km is merely how large the mistake is.

This reframing is load-bearing rather than decorative. It says the problem is
*not* a refinement problem, and so the campaign does not have to design,
calibrate or defend a coarse-to-fine refinement rule. **A polyline carrying a
width function is the same object at 110 km and at 27 m.** The Constitution's
"coarse constrains fine" is satisfied vacuously, because there is only one
representation and every scale evaluates it directly.

Consistency between map, locale and the walk therefore comes from **shared
derivation, not shared storage**: all three call one function of position.

## 3. Non-goals

- **Genuine sub-cell hydrology** — refining elevation per cell and recomputing
  flow accumulation. Rejected on arithmetic: an L18 refinement is ~4^12 rooms
  per cell across 40,962 cells, so laziness is forced rather than preferred,
  and a lazily-refined flow field carries no guarantee of agreeing with the
  coarse answer it must not contradict.

  **This campaign fires MAP-26's reconsideration trigger and then declines
  it,** which is worth recording because the row invites exactly this
  reopening. MAP-26 (adaptive / local mesh refinement) was rejected at Crust
  spec §12 with the clause *"reconsider only if a mesh-bound quantity
  (drainage, placement) is shown to need local resolution a uniform level
  cannot afford."* §1 is that demonstration for drainage. The answer is still
  not a finer mesh: per §2 the quantity should stop being **mesh-bound at
  all**. MAP-26's rejection stands, and stands for a better reason than
  before.
- **Client-side cosmetic channels** (the MAP-49 / MAP-erosion-style family).
  Collides with MAP-64's ratified keystone — *the client renders emitted
  water, never derives it* — and would leave map and walk disagreeing.
- **The client render itself.** Producer-side only, exactly as The Freshwater
  staged it. The wasm release and Orrery re-pin are a separate authorization
  (§9).
- **Seasonal stage.** The model admits it (§5.4) and must not foreclose it.
  Not built here.
- **Splitting lakes out of `WaterKind::River`.** Real debt (§8), older than
  this campaign.
- **Naming rivers.** Still a language-domain followup, as MAP-64 left it.

## 4. What must not move

These are invariants, asserted by test, not aspirations:

1. **`river_proximity` keeps its meaning and its consumers.**
   `domains/terrain/src/water.rs:102` answers *fresh water available here* at
   cell scale and feeds carrying capacity (The Confluence). It is not the
   defective predicate and does not change.
2. **The toponymic `Steeped` gates do not move.** `exposure_from`'s `river`,
   `ford`, `hill`, `valley`, `marsh`, `spring`, `island` rules
   (`windows/lab/src/metrics.rs:6416`) gate **lexicon exposure across 1000
   worlds**. They ask an availability question about a species' territory,
   which is correctly coarse. A change here would move language output, and
   that is not this campaign.
3. **Cell-scale `WaterKind` for ocean, salt-basin and lake is unchanged.**
4. **No new seed draws in the band derivation** (§6).

Points 1 and 2 are the campaign's central discipline: today one predicate is
conflated, and the split is *water-at-this-point* (render/walk, needs channel
geometry — the new thing) versus *fresh-water-available-here* (habitability
and naming, coarse — the existing thing, untouched).

The Confluence already hit this wall from the opposite side:
`windows/worldgen/tests/confluence.rs:41` records that at `RIVER_REACH = 7`,
**90% of seed-42 land counts as near-river**. Both predicates are broken by
one coarseness, in opposite directions; this campaign fixes one of them and
deliberately leaves the other alone.

## 5. Design

### 5.1 The primitive is generic

The ordinal is a **banding of a signed distance field to a linear feature**.
Everything reduces to one scalar — `d`, distance from a point to the channel
network — plus thresholds. Coastline (beach/backshore), scarp
(cliff/talus/apron), treeline and later roads band identically, so the
primitive is built generic and **water is its first instance, not its owner**.

It evaluates anywhere, needs no traversal, and is scale-free.

### 5.2 The network

The polyline is *constructed to satisfy known flux*, not simulated. Drainage
is already retained per cell (`TectonicGlobe.drainage`), so each cell has a
known discharge `Q`.

**The downhill graph, however, is not retained.** `globe.rs:421` computes
`post_downhill` as a **local**, hands it to `water_field` and
`rerouted_flow_fraction`, and drops it — it is not a `TectonicGlobe` field.
An earlier draft of this spec said "already retained", which was wrong, and
the correction is not free: **Task 1 owes the retention.** The adjacent
comment (`globe.rs:415-419`) records that `downhill_targets` is cheap
relative to `drainage_field`, so recomputing is a legitimate alternative;
retention is preferred because the network wants the graph on every read.
Either way it is a step, not an assumption.

With drainage and downhill in hand each cell has a known entry, a known exit,
and a known discharge. The channel routes between them.

Meander is a deterministic displacement of that route, and **must be sampled
from a position-continuous field** (the kernel's `Fbm`, derive-once per
`kernel/CLAUDE.md`), never from a room-address hash. Address-hashed noise is
what makes today's `wetness` axis unable to form a watercourse; reproducing it
here would speckle every band edge and flip a walker in and out of "bank" room
by room.

Meander amplitude scales inversely with gradient — low slope meanders, steep
slope runs straight — which is both real hydrology and free, since slope is
already derivable from the elevation map.

### 5.3 The bands

```
   terrace    floodplain   bank | channel |  bank   floodplain    terrace
 ------------------------------\|~~~~~~~~~|/--------------------------------
 |d| :   > V/2      < V/2    > w/2      0      < w/2    < V/2      > V/2
```

**All widths are angular, not metric — there is no length scale to use.**
`domains/terrain` works on the unit sphere: `shape.rs:33` computes
`cell_area = 4π / cell_count`, a dimensionless solid angle, and **no planet
radius exists anywhere in the codebase** (`radius_km` in
`domains/astronomy/src/moons.rs` is for moons, and a domain may not depend on
a sibling in any case). "Channel width in metres" therefore has no defined
meaning here.

This is not a gap to paper over: it is *better* expressed dimensionlessly,
because the campaign's actual claim is about the **ratio of channel width to
cell width**. Widths are fractions of the canonical cell edge; a window that
wants metres may multiply by a reference radius it declares, and introducing
a world radius is its own decision, not something to smuggle in here.

| border | threshold on \|d\| | derived from |
|---|---|---|
| channel / bank | `w/2` | `w = a·Q^b` — downstream hydraulic geometry; `Q` is the existing drainage field |
| bank / floodplain | `w/2 + k·w` | `k` a calibrated constant |
| floodplain / terrace | `V/2` | `V = g(Q, slope)` — **confinement**: a gorge has no floodplain |
| terrace / dry | beyond `V/2` | falls through to today's answer |

`d` is distance to the **network**, not to a single edge, so confluences need
no special case.

**`d` is signed, and the sign is load-bearing.** The bands above are stated on
`|d|`, but the stored and evaluated quantity must keep which *side* of the
channel a point lies on. Two things collapse without it:

1. **Fording is a side change.** A crossing is precisely the path
   `bank → channel → bank` in which the sign flips. A model carrying only
   `|d|` cannot distinguish crossing the river from walking to the water and
   turning back — so it cannot express the campaign's namesake at all.
2. **Cut-bank versus point-bar** — the outside of a bend erodes, the inside
   deposits — is `sign(d)` read against meander curvature sign. Real
   hydrology, no extra machinery, but unreachable from `|d|`.

Symmetric bands are also simply wrong about rivers: real floodplains are
frequently one-sided. The `|d|` formulation would impose a symmetry the world
does not have, and the sign costs nothing to carry.

### 5.4 Seasonality is a parameter, not a mechanism

Raising `Q` widens every band and closes the ford. The Mire's spec already
wished for this ("snow melts, the lake thaws, the ford drops"). Not built
here; the design must not foreclose it, which means `Q` enters the band
functions as an argument rather than being baked into a stored per-cell width.

## 6. Determinism

- **No new seed draws in the band derivation.** `Q`, slope, and the downhill
  graph are committed state; the bands are arithmetic over them.
- **The meander field is a new noise field and therefore a new
  seed-derivation label.** It must be declared in the owning crate's `streams`
  module and reach the generated manifest via `stream_labels()`. This is the
  recurring Few-and-Many gap; the plan carries an explicit manifest-regen step
  rather than trusting it to be noticed.
- Quantization stays at the emit boundary only. Band thresholds and distances
  are computed at full precision; only serialized values quantize.
- The band predicate is a pure function of position and committed state, so
  map, locale and walk agree by construction rather than by convention.

## 7. Save-format and schema impact

**This is the expensive part and it leads the G3 flagged list.**

1. **`locale/room/v2` → `locale/room/v3`.** The `water` field's meaning
   changes from "the containing cell's class" to "this point's transverse
   position". That is a changed meaning, not an addition, so CLAUDE.md's rule
   applies: a new epoch suffix, never a rename.
2. **`scene/tiles/v1` and `scene/tiles-region/v1` gain the network
   additively.** Both are **cross-repo contracts** — the Orrery consumes the
   released catalog — so additive-or-versioned only.
3. **Generated-artifact drift is broad.** Committed scene fixtures, the
   type-audit report (new `pub` boundary items), the digest, and
   `book/src/domesday/` (a pure read over the committed census) are all in the
   drift surface. Per the branch-table discipline rather than a prediction:
   *`docs/audits/` moved → regenerate and commit in the same commit;
   `book/src/gallery/` moved → STOP, that is an unplanned epoch event;
   `book/src/domesday/` moved with no census change → expected, commit it.*

**No census regen is authorized by this spec.** Nothing here requires one.

## 8. Registered debt (not fixed here)

`domains/terrain/src/water.rs:17-19` folds through-flow lakes into
`WaterKind::River`. The naive reading of this campaign — "rivers become
polylines, therefore `River` becomes a polyline" — would silently convert
every lake into a line. **The network carries rivers only**; lakes, playas and
ocean stay face-carried, per §2. The river/lake conflation is real modelling
debt, predates this campaign, and is registered rather than repaired.

## 9. Stages

1. **The primitive and the network** — generic distance-banded linear feature;
   river polyline with meander; band functions. Terrain domain, producer only,
   no consumer moved. Lab metrics land here.
2. **Locale reads it** — `locale/room/v3` with the ordinal; the epoch event;
   walk/vessel consumers follow.
3. **Riparian conditioning** — `Variant::GalleryForest` and its kin
   conditioned on band instead of drawn by dice. **On `bank` and
   `floodplain`, never on `channel`:** gallery forest grows *beside* a
   watercourse, and conditioning it on the channel band would plant a forest
   underwater. Expressing that distinction is the whole reason §2's ordinal
   beats a boolean.
4. **Scene emission** — additive on both scene schemas. Producer side only.
   **No client.**
5. *(Out of scope — separate authorization.)* Wasm release, Orrery re-pin,
   client render. This needed Nathan's carve-out in The Freshwater and needs
   it again.

## 10. Preregistered hypotheses

Frozen before the code that would move them (decision 0016). Each names its
axis and states a floor *and* a ceiling; a result outside the interval is a
finding, not a failure.

- **H1 — channel area.** With `a`, `b` calibrated so seed-42's largest river
  is **1/100 of a canonical cell edge** wide (dimensionless, per §5.3 — this
  is the "not one cell wide" claim stated as a ratio), the fraction of
  seed-42 **land area** classified `channel`
  falls in **[0.005%, 0.5%]**. Today ~6.7% of land classifies `River` at cell
  scale (`domains/terrain/src/water.rs:70-78`, The Freshet's tuning note —
  **re-measure rather than inherit**; a committed baseline is a claim with a
  date). Below the floor, rivers are invisible at room scale; above the
  ceiling, the type error has not actually been fixed.

  **The calibration and the prediction are deliberately on different
  statistics**, so this cannot be self-fulfilling: `a` and `b` are fitted to a
  single *point* (the largest river's width), while H1 predicts an *aggregate*
  over the whole discharge distribution. Fitting the point does not determine
  the aggregate — that is the free consequence being tested.
- **H2 — longitudinal connectivity.** Walking downstream by room adjacency
  from a sampled headwater channel room, **≥95%** of walks reach a sink or the
  sea without leaving the `channel` band. Falsified below 95%: a river you
  fall out of is not a river.
- **H3 — the ford exists.** Over sampled transects of the network, the
  fraction offering a **crossable profile** lies in **[0.15, 0.60]**. Near 0
  the walk is walled by impassable water; near 1 crossing carries no meaning
  and the campaign's namesake is decorative.

  *Crossable* is defined here, before measurement, in terms of quantities that
  exist: channel width `w` at or below a stated **cell-edge fraction**
  (angular, per §5.3 — never metres) **and** discharge `Q` below
  `hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE`.
  Depth is **not** modelled by this campaign and must not enter the
  definition — the existing `lab_is_fordable_cell` uses the same discharge
  proxy for the same reason.
- **H4 — no speckle** (the position-continuous-noise guard, expressed as a
  measurement). A straight transect across the channel yields a **monotone**
  band sequence with no band re-entry in **≥99%** of sampled transects.
  Falsification here means address-hashed noise leaked into the band edge.
- **H5 — a null we expect and will ship as one.** Riparian conditioning is a
  sub-cell descriptor change, so global formation fractions should not move:
  **|Δ| < 0.5%** on every formation fraction. A larger move means conditioning
  reached something it should not have.

Nothing in H1–H5 may be retuned after unblinding without saying so in the
chronicle.

## 11. Risks

1. **The calibration constants (`a`, `b`, `k`, `g`) are four knobs.** Every
   one sourced from an existing field is a knob that cannot be tuned to rescue
   a prediction. Prefer derivation over fitting; where a constant must be
   fitted, fit it against H1's stated interval and record the fit.
2. **The meander label is a save-format contract** the moment it lands. Adding
   it later, or renaming it, corrupts every world.
3. **`ford` already means something.** It is a lab detector
   (`lab_is_fordable_cell`, a cell-scale drainage proxy) and a toponymic gate.
   This campaign gives it a real transverse meaning at room scale; the
   cell-scale gate must keep working unchanged (§4.2), so the two readings
   coexist and the plan must not "unify" them.
4. **Stage 2 is an epoch event in the middle of the campaign.** It cannot be
   absorbed mid-measurement — a preregistered study's baseline and readout
   must see the same physics.
