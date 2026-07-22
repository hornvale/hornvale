# The Sundering — Campaign 2, Slice 2: The Diaspora on Routes

**Status:** design (G3 review)
**Program:** The Living Community engine (campaign 2 of ~5), slice 2 of the Connection Graph
**Slice:** the deep-history dynamics follow the derived transport graph — a genesis epoch.
**Base:** origin/main @94d5308e.

---

## 1. The payoff

The sea stops being a road you can walk. Today a fleeing community in the deep-history
bake finds its next home by breadth-first search over the raw icosphere mesh — which
steps straight across open ocean as if it were meadow. This slice makes migration follow
the **real transport topology** slice 1 derived, and the world changes shape:

- **Oceans sunder.** Land travel can no longer cross the sea, so a people on a separate
  landmass — or behind an impassable strait — with no current-lane out is genuinely
  *stranded*. It stays, it clusters, and its territory becomes distinct by isolation, not
  by niche. C1 made isolation *legible*; this makes it *dynamic* — isolation now shapes who
  ends up where.
- **Sailing lanes leapfrog.** A current-lane between two coasts is a single hop, so a
  coastal community can cross to a far shore in one step. Maritime migration — and, because
  all reach is now graph-reach, sea raids and daughter-colonies across the water — fall out
  of the same rule.

One swap, a dual consequence: **the sea is barrier and bridge**. That duality is the whole
campaign.

## 2. Context — what slice 1 left, and what this is

The Connection Graph (MAP-61) is a multi-slice campaign. Slice 1 (The Connection Graph,
shipped @cfaf57e3) derived the legible substrate — a `ConnectionGraph` over `CellId` nodes
with `Adjacency` / `WaterRoute` / `LandRoute` edges, each carrying a conductance — and
proved the keystone finding: **on the icosphere, ocean (not elevation) is the real
separator**, because a land BFS routes through mountain gaps at zero hop-cost but cannot
cross an impassable-ocean cell. Slice 1 deliberately did **not** touch the dynamics: C1's
displacement still runs on raw mesh adjacency, and the graph is a pure read.

This slice spends that finding. It reroutes C1's deep-history bake — the forward simulation
that seeds proto-communities and marches epochs across paleoclimate era-variance — so that
every place a community "reaches" is a graph edge, not a raw mesh neighbour. The later
slices remain future work: conductance-*weighted* distance (slice "conductance gating"),
field-diffusion / fixed-point coupling (trade, culture, disease), and built roads + portals.

## 3. Architecture (constitutional layering)

Nothing new is committed to the ledger; the graph stays a derived read, exactly as slice 1
built it. What moves is the *skeleton the bake produces*.

- **`windows/worldgen` (composition root)** — the bake already lives here
  (`history_bake::bake`), because it reads terrain, paleoclimate, and demography together.
  Before the bake, the root derives a **geography-stable `ConnectionGraph`** and passes a
  `&ConnectionGraph` into `bake`. The three site-picking paths traverse it.
- **`domains/topology`** — reused unchanged. The bake consumes `ConnectionGraph::edges`
  (adjacency list) and the existing `reachable_regions` for the isolation legibility. No
  new topology API is required for the core rewire (an optional small helper is allowed if
  the plan finds the BFS cleaner factored into the crate — see §4.4).
- **`windows/almanac`** — reuses slice 1's `render_connections` isolation readout to *name*
  which present communities are cut off (the divergence legibility). No new emit.

**The geography-stable graph.** Adjacency and sailing-lane edges depend only on terrain and
the ocean-current field — never on who lives where — so the graph is built **once**, before
the bake, from the same inputs `connection_graph_of` already reads. Concretely it is
`connection_graph(geo, elevation, biome, current, &[], cfg)` with an **empty settlement
slice**: adjacency + water routes are assembled; `add_land_routes` over no settlements is a
no-op. Land routes are settlement-dependent and the bake's settlements move every epoch, so
including them would mean re-deriving O(N²) bounded A\* every one of ~80 epochs — rejected on
cost; land routes stay a slice-1 present-world read.

**Derived, never committed — but a genesis epoch.** The graph is not written to the ledger
(no new save-format field). But because the dynamics it drives change which cells get
occupied, the committed occupation skeleton changes: **byte-identity deliberately breaks and
the census moves.** That is the defining property of this campaign and §5 treats it as the
load-bearing decision.

## 4. The mechanism

C1's bake has exactly three call sites that reach other cells via raw `geo.neighbors`. Each
is rerouted to traverse graph edges whose **conductance is strictly positive** (the
traversability line: ocean-touching adjacency edges are stored at conductance 0, so a
`> 0.0` test is exactly "you can travel this edge"). Sailing-lane edges carry the current's
magnitude as conductance, positive for any real current, so lanes are traversable and a lane
is one hop.

### 4.1 `nearest_dest` — migration and flee-resettle

The BFS that finds the nearest vacant habitable cell keeps its structure and its
tie-break (refugial cells first, then river-adjacent, then lowest `CellId` — a total,
deterministic order), but expands over **graph edges with conductance > 0** instead of
`geo.neighbors`. Ocean is no longer walked through; a sailing-lane edge reaches the far
coast in one BFS layer (leapfrog). If the whole reachable component is full or hostile,
the result is `None` — and the community collapses (Famine), exactly as today, but now
"reachable" honours the sea.

### 4.2 `raid_target` — conflict

The scan for the wealthiest occupied neighbour expands over the same conductance-positive
graph edges (Nathan's ruling, decision #2: all movement follows the graph). A coastal
community can raid across a current-lane (a sea raid); an isolated component cannot be
raided from outside. Tie-break unchanged (highest population, then lowest `CellId`).

### 4.3 daughter-founding in `grow`

The daughter settles the vacant habitable **graph-neighbour** of highest river-weighted
capacity (a daughter can be a maritime colony across a lane), replacing the direct
`geo.neighbors` pick. Scoring (`capacity * river_factor`) and tie-break (lowest `CellId`)
are unchanged.

### 4.4 The traversal helper

The BFS/neighbour-scan currently reads `geo.neighbors(c)`. After the rewire each site-picking
path needs "the conductance-positive graph neighbours of `c`". The plan may either inline a
filtered `graph.edges(c)` iteration at each site or factor a small
`traversable_neighbors(&ConnectionGraph, CellId) -> impl Iterator<Item = CellId>` helper;
either is acceptable so long as the filter (`conductance > 0.0`) and the ascending-`CellId`
determinism are identical across all three sites.

## 5. The epoch — the load-bearing decision (leads the G3 flagged section)

This is a **genesis epoch**. It changes the deterministic topology the bake's dynamics use,
which changes which communities exist and where, which shifts every downstream artifact.

- **No new stream label, no relabel.** The seed-derivation labels (`history/bake`,
  `history/genesis/<people>`) are unchanged in meaning — the bake still draws genesis site
  picks and daughter probabilities from them in commit order. What changes is the *world
  state* those draws act on, not the labels' semantics. Per the save-format contract, an
  epoch suffix is for *redefining a label's draws*; that is not happening here, so no
  `history/bake/v2`. (This is the specific determinism-contract judgment for Nathan to
  confirm at G3.)
- **Byte-identity breaks by design; the census regenerates.** Same seed + pins is still
  byte-identical *after* this change (the bake stays a total function of the committed
  world), but the seed-42 skeleton is different from main's. The census must regenerate on
  the canonical Linux box `lefford` (decision 0063; macOS cannot commit census goldens),
  and the seed-42 keystone fixture refreezes at merge from main's tip. **Census regen is a
  carve-out — explicit authorization at G6.**
- **The census-close cascade (from C1).** After the lefford regen: `rows.csv` goldens →
  `golden-pins.sql` + `calibration.rs` (`make census-check`, column order = live-computed,
  pinned-literal) → and the other census-fixture-backed calibrations that run in the normal
  gate (`branches_family_calibration.rs`, `gathering_calibration.rs`). Fixture-backed
  re-pins can be done on macOS; only the `rows.csv` regen must be on lefford.

## 6. Determinism

Within a seed the bake remains fully deterministic. The graph derivation is byte-identical
(slice 1's contract: `CellId`-ascending iteration, `f64::total_cmp` tie-breaks, no
`HashMap`/`HashSet`, no wall-clock). The rerouted BFS/scans keep every existing tie-break and
add one filter (`conductance > 0.0`). No new seed draw is introduced (destination selection
is deterministic, not stochastic), so the `history/bake` stream's *draw sequence per
world-state* is unchanged in kind — only the world-state it walks differs. `total_cmp` at
every float comparison; graph conductances are compared, never serialized in the path.

## 7. Success criteria — measure, don't narrate

The campaign lives or dies on three preregistered gates over the seed-42 (and a small seed
sample) baked skeleton. Each is a real assertion with a mutation-testable failure, not a
narration.

1. **Displacement still fires at volume.** Migration + flee + resettle events stay above the
   volume floor C1 established (the bake is not allowed to go inert). Rerouting must not
   silently convert the dynamics into stasis.
2. **The map is not depopulated (the headline risk).** Blocking ocean crossings must not
   strand peoples into mass extinction: the alive-at-`now` settlement count stays within the
   walkable band the C1 quality gate defines (`tests/history_placement.rs`), and the
   collapse (Famine) share does not blow past a preregistered ceiling. **If graph-blocking
   depopulates the map, that is a fidelity/tuning finding brought to Nathan (carve-out) — it
   is never patched with a floor.**
3. **Isolation predicts divergence.** At least one seed exhibits a genuinely isolated graph
   component (a landmass or strait-sealed region) whose occupations form a **distinct
   people/lineage territory cluster** — measured structurally over the existing record
   fields (no new committed field; decision #3). A metric shows isolation correlating with
   territory divergence, the dynamic form of C1's region-overlap result.

A preregistered **cost check** confirms the graph derivation + the rerouted bake stay within
the commit-gate wall-time budget (the geography-stable graph is one cheap derivation; graph
BFS is ~mesh-BFS cost — but this is measured, not asserted by hand, per the Sounding's
lesson).

## 8. Non-goals (§9 — read before assuming scope)

- **Conductance-weighted distance.** Distance stays hop-count over the graph. Weighting
  migration by conductance (seasonal/gradual ease-of-travel) is the named later slice.
- **Land-routes in the dynamics.** Settlement-pair land corridors stay a slice-1 present-world
  read; the bake follows only the geography-stable adjacency + sailing lanes.
- **Field-diffusion / fixed-point coupling** (trade, culture, disease over the graph) — later.
- **Built roads and portals** — later (society's marks; magic/astronomy-gated).
- **Cultural drift per isolated component** (tongue/deity divergence) — the language/religion
  domains' job; C1 leaves those `None` in the bake. Captured as a followup, not built here.
- **A new committed graph or divergence field** — the graph stays derived; divergence stays
  emergent and measured.

## 9. Definition of Done (per CLAUDE.md)

- The three site-picking paths (`nearest_dest`, `raid_target`, daughter-founding) traverse
  the geography-stable `ConnectionGraph` (conductance > 0), built once at the worldgen root.
- The three §7 gates pass on the merged tree; the cost check is within budget.
- Census regenerated on lefford (authorized at G6); the census-close cascade re-pinned green;
  the seed-42 keystone fixture refrozen from main's tip.
- Chronicle entry, retrospective, book freshness sweep (the living-community + connection-graph
  chapters), Confidence Gradient re-score if a bet moved, registry flip (MAP-61 slice-2 →
  shipped; repoint Where), full gate + artifact drift check.
