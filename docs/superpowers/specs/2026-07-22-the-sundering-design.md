# The Sundering — Campaign 2, Slice 2: The Moving Sea

**Status:** design (G3 re-review — revised after the hard-block hit the preregistered depopulation risk)
**Program:** The Living Community engine (campaign 2 of ~5), slice 2 of the Connection Graph
**Slice:** the deep-history dynamics follow a **time-varying** transport graph — the sea itself moves with the ice. A genesis epoch.
**Base:** origin/main @94d5308e.

---

## 1. The payoff

The sea is a barrier that breathes. When the ice comes, the sea *falls* — up to 120 m at
the glacial maximum — and the drowned shelf between a mainland and an island rises into a
**land bridge**. When the ice retreats, the sea *rises* and the bridge drowns, **sundering**
the peoples who crossed. This slice makes the deep-history bake follow that moving sea:

- **Glacial eras open the bridges — the diaspora crosses.** A community whose home freezes
  migrates toward the refugia that survive the cold; in the glacial era the shelf to those
  refugia is exposed land, so the migration reaches them. Peoples spread along the bridges
  the low sea uncovers — a real ice-age diaspora.
- **Interglacial and present drown the bridges — the peoples sunder.** As the sea rises the
  bridge becomes ocean, and the peoples who crossed are cut off on their landmass. Their
  territory becomes distinct by *isolation*, not niche. C1 made isolation legible; this makes
  it dynamic — and gives it a cause you can name: *they were one people, before the sea rose.*

The sea is barrier **and** bridge — not by a fixed rule, but because it is *the same sea at a
different time*. That is the whole campaign, and it is exactly how the Earth's ice-age
biogeography actually worked (Beringia, Doggerland, Wallace's Line).

## 2. Context — why the graph must move

The Connection Graph (MAP-61) is a multi-slice campaign. Slice 1 (shipped @cfaf57e3) derived
the legible substrate — a `ConnectionGraph` over `CellId` nodes with `Adjacency` /
`WaterRoute` / `LandRoute` edges — and proved the keystone finding: **ocean, not elevation,
is the real separator.** Slice 1 did not touch the dynamics.

This slice reroutes C1's bake to follow that graph. The **first** design (a single,
present-coastline graph; oceans an absolute block; hop-distance) was implemented and its
mechanism proven correct — but it hit the spec's own preregistered **depopulation risk** on
the real seed-42 world: with a static present sea, the glacial refugia are islands the
land BFS can no longer reach, so climate-driven migration collapsed **51 → 1**. The map was
not depopulated and peoples stayed separated, but the diaspora went inert.

The measured cause pointed straight at the fix. A *static present coastline* is an
anachronism: ice-age migration to refugia happened across land bridges the low glacial sea
uncovered. And the substrate to model that **already exists** — `EraClimate.sea_level`
carries each era's eustatic sea level (present + change, down to −120 m at the glacial
maximum), and "land this era" is already `elevation ≥ era.sea_level` in the paleoclimate.
The connection graph is the **one** Hornvale substrate still frozen in time while
habitability, ice, and sea level around it already vary per era. This slice conforms it to
the sim's own principle — *fields are functions over space × time* — by making the graph a
function of the era: **`graph(era)`**.

The later slices remain future work: conductance-*weighted* distance; field-diffusion /
fixed-point coupling; built roads + portals; and — the deeper, slower second layer of the
moving coastline — **tectonic** uplift over deep time.

## 3. Architecture (constitutional layering)

Nothing new is committed to the ledger; the graph stays a derived read. What moves is the
*skeleton the bake produces*.

- **`windows/worldgen` (composition root)** — the bake lives here. Before the bake, the root
  derives **one geography graph per era** (`CLIMATE_ERAS = 25`), each with the era's own
  marine mask (`elevation < era.sea_level` ⇒ ocean this era), and passes them into `bake`
  aligned with the era series. The three site-picking paths traverse **the graph for the era
  in force**.
- **`domains/topology`** — reused unchanged (`ConnectionGraph`, `edges`, `reachable_regions`).
- **`windows/worldgen/graph_derive` + `traversal`** — gain an **era-aware** derivation: a
  traversal-cost / marine test keyed to a per-era sea level rather than the present
  `biome.is_marine()`. The present graph (era = now) is still the slice-1 graph, so the
  legibility surface and the isolation payoff are unchanged.
- **`windows/almanac`** — reuses slice 1's `render_connections` isolation readout on the
  present (era = now) graph. No new emit.

**Derived per era, never committed.** Each era's graph is a total function of terrain +
that era's sea level — no seed draw, no wall-clock, never written to the ledger (no new
save-format field). Twenty-five cheap derivations (adjacency is O(cells); the cost check in
§7 bounds the total).

## 4. The mechanism

### 4.1 The era-aware graph

The essential era-varying part is **adjacency** — whether the cell-pair is land-connected
this era. A cell is ocean in era E iff `elevation < era.sea_level(E)`; an adjacency edge's
conductance is 0 if either endpoint is ocean this era (unchanged conductance rule, era-aware
inputs). So the exposed glacial shelf carries positive-conductance adjacency edges — the land
bridges — that vanish as the sea rises. Sailing lanes (`WaterRoute`) use the era's coastline
for launch points but the **present** current field (Hornvale has no paleo-current model —
a documented simplification; the land bridges, not the lanes, are the era-varying heart).

### 4.2 The three site-picking paths (unchanged from the first design)

C1's bake reaches other cells via raw `geo.neighbors` at exactly three sites. Each iterates
`traversable_neighbors(graph, cell)` — edges with **`conductance > 0.0`**, ascending, deduped
— where `graph` is **the era's graph**:

- `nearest_dest` — the migration / flee-resettle BFS.
- `raid_target` — the raid scan (all movement follows the graph — sea raids across a lane, an
  isolated component un-raidable from outside).
- the daughter-founding pick in `grow`.

Every tie-break (refugia > river > `CellId`; raid population > `CellId`; daughter
`capacity*river_factor` > `CellId`) is unchanged. `traversable_neighbors` and the
sunder/leapfrog proof from the first implementation are reused verbatim; only the graph handed
to them becomes era-specific.

### 4.3 Threading the era

The bake's epoch loop already selects the era in force (`era_for(year)`). It now also selects
that era's graph (parallel to the era series) and hands it to `step_community`. Genesis
seeding uses the earliest era's graph.

## 5. The epoch — the load-bearing decision (leads the G3 flagged section)

Unchanged from the first design: this is a **genesis epoch**. The dynamics' topology now
varies with era, changing which communities exist and where — the committed skeleton moves.

- **No new stream label, no relabel, no new committed field.** The `history/bake` /
  `history/genesis/<people>` labels' draw semantics are unchanged; the graph is never
  committed. (Determinism-contract judgment for Nathan to confirm.)
- **Byte-identity breaks by design; the census regenerates on `lefford`** (decision 0063;
  macOS cannot commit census goldens). **Census regen is a carve-out — explicit
  authorization at G6.** The seed-42 keystone fixture refreezes at merge.
- **The census-close cascade (from C1)** applies: `rows.csv` → `golden-pins.sql` +
  `calibration.rs` (`make census-check`) → `branches_family_calibration.rs`,
  `gathering_calibration.rs`.

## 6. Determinism

Within a seed the bake stays fully deterministic. Each era's graph derivation is
byte-identical (slice-1 contract: `CellId`-ascending, `f64::total_cmp`, no `HashMap`, no
wall-clock). The era-aware marine test (`elevation < era.sea_level`) reads committed terrain
+ the paleoclimate's already-committed per-era sea level. No new seed draw. `total_cmp` at
every float comparison.

## 7. Success criteria — measure, don't narrate

The fold-in gives the diaspora its **best physical shot** — but its efficacy on seed-42 is
*measured*, not assumed. If the glacial low-stand genuinely reaches the refugia, displacement
fires again and the payoff lands. If seed-42's refugia are deep-ocean islands no −120 m
bridge reaches, migration stays low — and *that is the honest finding*, at which point the
displacement gate is re-scoped to the isolation that fires (labelled post-data, as C1 did),
not floored. Either outcome is legitimate; the gate adjudicates.

1. **Displacement fires (measured, re-pin-or-re-scope).** With the moving sea, migration on
   seed-42 is re-measured. If it fires at volume, re-pin `MIGRATION_FLOOR` clear of the new
   value (labelled). If it stays inert *despite* the bridges, re-scope the displacement gate
   to the isolation/divergence signal and record the "refugia are true islands" finding — do
   not floor it. (This is the resolution of the first design's block, made explicit.)
2. **The map is not depopulated.** Alive-at-`now` count stays in the walkable band
   (`history_placement`'s `40..=400`); collapse (Famine) share stays under a preregistered
   ceiling set above the measured value.
3. **Isolation predicts divergence.** On the **present** (era = now) graph, ≥ 2 inhabited land
   components, and at least one isolated component hosts a *proper subset* of the world's
   peoples — the structural, un-tunable signature of divergence-by-isolation (no new committed
   field). The drowned-bridge legibility ("these two peoples were joined before the sea rose")
   is captured as a follow-up, not built here.

A preregistered **cost check** confirms the 25 per-era graph derivations + the era-graph bake
stay within the commit-gate wall-time budget (adjacency is cheap; measured, not asserted).

## 8. Non-goals (§9 — read before assuming scope)

- **Tectonic coastline evolution.** The moving sea here is *eustatic* (ice-volume / climate).
  Coastlines moving with plate uplift over deep time is the deeper, slower second layer — the
  terrain domain, a later campaign.
- **Paleo-currents.** Sailing lanes use the era's coastline but the present current field
  (no paleo-current model). The era-varying land bridges are the heart; era-specific currents
  are a captured follow-up.
- **Conductance-weighted distance** — distance stays hop-count over the era graph. Weighting
  by ease-of-travel is a named later slice.
- **Settlement land-routes in the dynamics** — the bake follows era adjacency + sailing lanes;
  settlement-pair corridors stay a slice-1 present-world read.
- **Field-diffusion / fixed-point coupling; built roads + portals** — later.
- **A new committed graph, sea-level, or divergence field** — the graph stays derived per era;
  divergence stays emergent and measured.

## 9. Definition of Done (per CLAUDE.md)

- The bake derives one era-aware graph per era (`elevation < era.sea_level` marine mask) and
  the three site-picking paths traverse the era-in-force graph via `traversable_neighbors`
  (`conductance > 0`); `traversable_neighbors` + the sunder/leapfrog proof are reused.
- The §7 gates pass on the merged tree (displacement re-pinned **or** re-scoped per the
  measured reality, labelled); the cost check is within budget.
- Census regenerated on lefford (authorized at G6); the census-close cascade re-pinned green;
  the seed-42 keystone fixture refrozen from main's tip.
- Chronicle, retrospective, book freshness sweep (living-community + connection-graph
  chapters), Confidence Gradient re-score if a bet moved, registry flip (MAP-61 slice-2 →
  shipped; repoint Where), full gate + artifact drift.
