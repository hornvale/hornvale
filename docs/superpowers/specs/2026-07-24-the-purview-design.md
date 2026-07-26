# The Purview — Design

**Status: SHIPPED 2026-07-26.** Chronicle:
[the-purview](../../../book/src/chronicle/the-purview.md). Retrospective:
[the-purview](../../retrospectives/the-purview.md). Decisions promoted:
[0076](../../decisions/0076-the-situated-pole-is-egocentric-and-knowledge-limited.md),
[0077](../../decisions/0077-zoom-in-the-room-mesh-is-path-truncation.md).
Two departures from this spec, both ledgered at G4 and carried into the
shipped design: the observer's bearing-of-north field was dropped (§5.1) and
strange-site marks were trimmed to a followup (§5.2).

**Campaign:** The Purview — the spatial lens: a locale-scale, egocentric,
fogged map of the possessed agent's surroundings, one lens with the prose,
joined by attention.
**Date:** 2026-07-24 · **Registry:** RENDER-4 (first rung; flips `raw` →
`elaborated`/`shipped` at close), cross-links RENDER-1/2/9, TOOL-2, EXP-3.
**Parent spec:** `2026-07-09-rendering-strategy-design.md` (Ring 2, the
situated pole) · **Prior rung:** The First Mark
(`2026-07-22-the-first-mark-design.md`; chronicle `the-first-mark.md`).
**Repo:** hornvale (workspace + one client line) · **Autopilot** engaged
(G3/G6 hard stops).

---

## 1. Goal

The First Mark proved the game loop text-only: possess, provoke, a grievance
folds, an NPC turns hostile, `possess --out` persists it, `why` traces the
wake. What the walker still lacks is a **body that knows where it is**. Every
spatial fact — three ways on, a settlement over there, ground you have
already crossed — arrives as a sentence the player must assemble into a mental
map. The prose is doing two jobs, and doing the second one badly.

The Purview gives the possession a coarse spatial lens: a small, deterministic,
fogged chart of the rooms around the agent, at the scale the agent walks, drawn
from the same query surface the prose is drawn from and sharing the prose's
noun catalog. Focalized prose stays **primary** (Constitution §3.5); the chart
is secondary, and its job is to *locate and signal*, never to describe.

This is RENDER-4's first rung. It is deliberately **not** the tilemap client:
it builds the protocol half (the scene protocol's long-unbuilt situated pole)
and the honest in-process render, so that a graphical client later becomes pure
client work against a fixed contract.

## 2. The governing framing

From The First Mark's founding brainstorm, restated as this campaign's
invariant:

> Map and prose are two **grains of one lens** over one query surface, joined
> by **attention**. Not two content pipelines. The map locates and signals
> salience (coarse); the prose deepens what you attend to (fine).

Everything below is downstream of that sentence. The design's job is to make it
**falsifiable** (§8), not merely aspirational.

## 3. Keystones from the ideonomy pass

Three axes were applied (homogeneity, rate, hierarchicalness) under
substitution and negation, captured as a state machine. Two findings changed
the design's shape and one fixed its boundary.

**K1 — Zoom is path truncation; there is no aggregation layer.** The room mesh
is natively hierarchical. Verified on seed 42: room `858020851` at depth 12
reports three lateral exits (`E`, `Nw`, `Sw`), one `exit` to parent room
`214505203`, and four `enter[0..3]` children. So the "coarse map" is not a
second data structure — it is **the same builder run at a shallower depth
around the observer's ancestor**. A whole planned aggregation layer
disappears.

**K2 — The two grains are one cell schema whose detail varies by epistemic
state.** Substituting the map's homogeneity (uniform grid → differentiated)
relocates the two-grains thesis out of "two renderers" and into the data: a
cell carries the detail its epistemic state warrants, and the finest grain of
all — prose — is simply the cell you are attending to. This is what makes map
and prose one lens rather than two pipelines that agree.

**K3 — Egocentric is the only non-duplicative pole.** Negating "egocentric"
gives an allocentric north-up regional chart; that pole is already shipped
twice (the atlas, the Orrery's Map rung). The situated, observer-relative,
knowledge-limited pole is the one the protocol has never had.

**K4 — Placement must be lattice, not bearing.** Ten observers sampled from
the equator to 89.5°N each returned three *distinct* compass buckets
(`{E,Nw,Sw}`, `{N,Se,W}`, `{Ne,Se,W}`, …) — two orientation families
(up-triangle, down-triangle) rotated by local face orientation. Exits never
collide into one bucket, but the lattice's rotation relative to north drifts
across the globe, so laying cells out by bearing would distort the very mesh
the chart claims to depict.

## 4. The lens state machine

A cell of the chart is in exactly one epistemic state, and the state
determines its grain.

```
  STATE        DRAWN FROM                    GRAIN
  ----------   ---------------------------   -----------------------------
  here         ground truth                  full + prose focalization
  sensed       ground truth, within radius   biome / water / relief band
  remembered   Knowledge  room/<id>          what was absorbed on the visit
  unknown      nothing                       void — not drawn at all

  TRANSITIONS
    unknown    --(enters radius on a move)-->  sensed
    sensed     --(go)----------------------->  here
    here       --(go away)------------------>  remembered
    remembered --(re-enters radius)--------->  sensed
    any drawn cell --(examine)-------------->  attended   (prose grain)

  FORBIDDEN
    unknown -> here       no teleport
    unknown -> attended   you cannot examine what no grain has surfaced
                          (this is already the vessel's examine contract)

  ABSORBING
    nothing returns to unknown — v1 never forgets.
```

Two consequences the machine makes visible:

- The absorbing `unknown` state is precisely where **MEM-1 forgetting** later
  plugs in, and where a `remembered` cell later becomes *fallible* rather than
  merely *stale*.
- Because knowledge only ever grows and is never invalidated, the fog is
  **byte-identical by construction** — the monotone-quantity discipline, in its
  strongest form: nothing is committed at all.

"In radius ⇒ sensed" is the honest rule at this scale, not a simplification.
Measured on seed 42: room `858020851` sits at (12.5004, 40.0051) and its `E`
neighbour `858020819` at (12.5005, 40.0141) — a centroid step of ~1.5 × 10⁻⁴
rad, so a depth-12 room's edge subtends ~0.015° of arc and a radius-4
neighbourhood spans ~0.07°, about 1/5000 of the globe. That is a plausible
horizon from open ground rather than an abstraction.

**A caution the plan must respect: the sim defines no planetary radius.** The
mesh lives on the unit sphere and elevation is the only metric length in the
model; a grep across the kernel, terrain, and astronomy finds no
planet-radius constant. So the chart's scale is stated in **arc**, and any
metric figure is an illustration conditioned on an assumed radius (on an
Earth-sized world the numbers above would read ~1.7 km per room and ~8 km
across). Neither the schema nor the caption may assert metres per cell.

## 5. Scope — what v1 ships

### 5.1 `scene/surrounds/v1` — the scene protocol's situated pole

A new scene kind in `windows/scene`, semantic-only (RENDER-1's rule: what an
observer can see, never how to draw it — no glyphs, no colours, no projection
choices), deterministic, and a save-format-class contract versioned by epoch
suffix.

Document shape (field order is JSON key order and is contract):

- `schema`, `seed`, `day`
- `observer` — room id, face, path, depth, quantized centroid lat/lon, and the
  **bearing of north** in the local lattice frame (so a client can orient
  without recomputing geometry)
- `radius` (BFS rings; default 4 → **31 cells**, verified by BFS over the
  triangular face-adjacency lattice: ball sizes 1, 4, 10, 19, 31, 46, 64 —
  i.e. `1 + 3k(k+1)/2`), `depth`
- `cells[]`, deterministically ordered — for each room in the neighbourhood:
  - `id` — packed room id
  - `u`, `v`, `up` — **exact face-local integer lattice coordinates relative
    to the observer, and triangle orientation**; absent when `seam` is set
  - `seam` — set when the cell lies on a different base icosahedron face than
    the observer, so the lattice bends and no honest local coordinate exists
  - `state` — `here | sensed | remembered` (a fog-free emitter writes `sensed`
    for every non-observer cell; see §6)
  - layers at the state's grain — for `sensed`: `biome`, `water`, and a
    **relief band** (a small ordered set of coarse elevation classes, named in
    a `relief_legend` on the document exactly as `biome_legend` and
    `water_legend` work in `scene/tiles-region/v1`; the band boundaries are
    fixed at plan time and are contract). For `remembered` and `here`,
    additionally the `regime` descriptor and the blended fields.
  - `marks[]` — salience-ranked, each with a **noun key** and a datum
- `legend` — the noun catalog: every mark and cell class the chart surfaced,
  as `(noun, datum)` pairs, in the same shape as `Focalized.nouns`

CLI: `hornvale scene surrounds --world W --room ID [--radius R] [--depth D]`,
emitting the fog-free document — usable by `clients/world-wasm` and any
external client, exactly as the other scene kinds are.

### 5.2 The fog and the marks — `windows/vessel`

The vessel overlays what only a session knows, and **writes nothing**:

- `remembered` is a read of the `room/<id>` keys `IdentityProjection` already
  absorbs on each visit. No `Projection` change, no new stored state, no epoch.
- Agent marks — co-located and nearby derived NPCs (liveness), the mint
  settlement, placed strange sites — are added to `cells[].marks` and to the
  legend, salience-ranked by a deterministic key.

### 5.3 The `map` verb and its zoom rung

`map` renders the chart at walk depth. `map out [N]` (N ≤ 3) re-runs the same
builder at depth − N around the observer's ancestor room (K1); a coarse cell is
`remembered` when any `room/<id>` in Knowledge is a path-prefix descendant of
it — an integer prefix test, no aggregation code.

`enter` / `exit` **stay refused**. The chart may show a scale the body cannot
walk into, and that asymmetry is the honest statement of UNI-37's deferral
rather than a gap.

### 5.4 The in-process ASCII render

A deterministic triangular-lattice ASCII chart (Ring 0/1 under decision 0022 —
the same class as the gallery's three existing ASCII maps; no client toolchain,
no graphics dependency). It emits three parts:

1. a **caption** naming the lens, depth, radius and the observer's north — per
   RENDER-9, the caption is the load-bearing honesty, and it states what the
   lens omits (including any seam in view);
2. the cell grid, one glyph per cell, chosen by a **registered lens** — v1
   ships one (`terrain`) and names the registry so a second (`knowledge`,
   `salience`) is purely additive;
3. the legend.

The exact glyph layout is fixed at plan time behind a golden test.

### 5.5 The attention join

- The chart's legend is a noun catalog in the same `(noun, datum)` shape as
  `Focalized.nouns`.
- `examine` widens from "nouns **the prose** surfaced" to "nouns **this lens**
  surfaced, at either grain."
- A noun the prose mentions that has a place gains a mark on the chart.

### 5.6 Artifacts, book, and the client dividend

- `book/src/gallery/scene-surrounds-seed-42.json` and a schema reference page
  `book/src/reference/scene-surrounds-v1.md`, both joining
  `scripts/regenerate-artifacts.sh` and CI's drift check.
- The possession galleries gain `map` turns (a regenerated, drift-checked
  change to `possession-seed-42.md` and kin).
- **The Casement gets the chart for free**: it is terminal text, so the browser
  exhibit needs only one arm in `splitResponse` classing map lines as
  monospace. One client file, a few lines — and the map ships to all three
  surfaces (CLI, galleries, browser) from one render.

## 6. Architecture and placement

```
kernel  ──  RoomAddr: exact face-local lattice coordinates + orientation (new)
              |
windows/locale ── Locale: the room as an observable place (unchanged)
              |
windows/scene  ── scene/surrounds/v1 schema + FOG-FREE ground-truth builder
              |                                        + the ASCII render
windows/vessel ── epistemic overlay (Knowledge) + agent marks + `map` verb
              |
cli / clients ── `hornvale scene surrounds`; the Casement's monospace arm
```

`windows/scene` stays **stateless** — it must, because `clients/world-wasm`
serves it to other repos with no session. The split is therefore: scene owns
the schema and ground truth; vessel owns everything only a session knows.

Layering verified: `windows/scene` does not depend on `windows/vessel`, so the
new `vessel → scene` edge introduces no cycle; `scene → locale` is an ordinary
window→window edge (precedent: `vessel → locale`, `lab → worldgen`).

The kernel addition is contract-appropriate: `kernel/src/room.rs` already
states "Identity, adjacency, and seeding are integer/rational; transcendentals
live only in position geometry," and it already computes face-local integer
barycentrics privately (`bary_triple`). This exposes what it computes; no
transcendental enters layout.

## 7. Determinism and save-format posture

- **Nothing is committed.** The chart is a pure derived view over (position,
  radius, depth, Knowledge) — UNI-20's "nothing stored that re-derives." A
  possession that draws the map a thousand times is byte-identical to one that
  never draws it.
- **No epoch, no new seed-derivation label, no stream-order change.** No draw
  is consumed by any of this.
- **New save-format-class contract:** the `scene/surrounds/v1` schema, governed
  by epoch-suffix versioning like every scene kind, and — per decision 0055 —
  a cross-repo contract the moment a second repo parses it.
- **New pub-boundary primitives** (the kernel lattice accessor; the scene
  document's fields) carry `type-audit:` verdict tags. A boundary change means
  the **full gate** before pushing, per standing practice.
- Quantization stays at the emit boundary only: lat/lon in the document are
  quantized as the other scene kinds quantize; lattice coordinates are
  integers and are not.

## 8. The falsifiable claim

The campaign's thesis-bearing test — the reason this is a campaign and not a
feature:

> For every turn of a scripted possession, the set of nouns `examine` accepts
> equals `prose_nouns ∪ map_nouns`; every mark on the chart resolves to a
> datum; and **a noun surfaced at both grains resolves to the same datum.**

If that last clause can be made to fail, map and prose are two pipelines
wearing one name and the framing is wrong. It is the sharp form of "one lens,
two grains," and it exists because the *rate* asymmetry (prose is event-rate,
the chart is state-rate) means the noun catalog is the only thing the two
grains can genuinely share.

Supporting tests: determinism of the document and the render; the seam case
(§9); the zoom rung's prefix-descendant fog; `map` idempotence (drawing the
chart never advances or perturbs the session).

## 9. Risks

- **The seam is rare and therefore easy to leave untested.** Order-of-magnitude
  estimate (arithmetic, not measured): a radius-4 neighbourhood crosses a
  base-face boundary for well under 1% of observers at depth 12 — a face
  carries 4¹² ≈ 16.7 M triangles and only ~10⁵ of them lie within 4 steps of a
  boundary. The plan **must place an observer deliberately** near a
  face edge and near an icosahedral vertex, and assert the `seam` marker and
  the caption's disclosure. Do not rely on a random walk finding it.
- **Golden churn.** Adding `map` turns to the possession galleries regenerates
  committed transcripts. Intended, but it must be a deliberate regeneration
  step, and the `map` verb being additive means no *existing* scripted turn
  changes — to be verified at implementation, not assumed.
- **Scope creep toward the client.** The graphical tilemap is RENDER-4's next
  rung, not this one. The ASCII render is the honest v1 and the protocol is the
  durable deliverable.
- **`Knowledge` growth.** The map reads a map that grows with every visited
  room. Already true today; the chart does not change the growth rate.

## 10. Out of scope — indexed, not lost

Each is routed to the idea registry or the followup register at close; none is
a prerequisite for the others.

- **The graphical tilemap client** — the browser drawing from
  `scene/surrounds/v1` instead of ASCII (RENDER-4's next rung).
- **Per-species sense radius** — the radius is a constant with the
  `PerceptionVector` slot reserved; EXP-3/EXP-9 make it a species scalar.
- **Memory decay and the false map** — MEM-1 at the absorbing `unknown` state;
  a `remembered` cell that is *wrong*, not merely stale.
- **The anti-map** — salient absence drawn on the chart (RENDER-7): where the
  field-prior expects something the ledger lacks.
- **An NPC's own map** — The Surmise already derives a creature's belief about
  where water lies; rendering that belief through this same lens.
- **The diegetic chart** — an in-world cartographer's map with its own errors
  (TOOL-2, MAP-8).
- **The non-spatial lens** — the same machinery over a non-spatial index (the
  social graph, the drive stack).
- **Map-primary presentation** — closed by Constitution §3.5.

## 11. Definition of Done

- `make gate` green; `make gate-full` before merge (a pub-boundary touch).
- Type-audit clean — every new pub-boundary primitive carries a verdict tag.
- Committed artifacts regenerated and drift-checked; `regenerate-artifacts.sh`
  updated with the `scene surrounds` line.
- Chronicle entry `book/src/chronicle/the-purview.md`.
- Freshness sweep of the chapters this touches — the scene-protocol reference,
  the possession/game chapters, the rendering chapter.
- Retrospective in `docs/retrospectives/`.
- Registry: RENDER-4 flipped and elaborated; new rows for the deferrals in §10;
  cross-links to RENDER-1/7/9, TOOL-2, EXP-3, MEM-1, UNI-37.
- Confidence-Gradient re-score if this moves a bet in `open-questions.md`.

## 12. Decisions to promote at merge

- Scene protocol: **the situated pole is egocentric and knowledge-limited**;
  the allocentric pole is the atlas/Orrery's and is not duplicated.
- **Zoom in the room mesh is path truncation**, so scale views need no
  aggregation layer.
- **The chart may show a scale the body cannot enter** — rendering a coarser
  depth is a lens; possessing it is UNI-37.
