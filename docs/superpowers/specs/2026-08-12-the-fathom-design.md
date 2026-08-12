# The Fathom — every realm gets its depth coordinate

*Campaign spec. Campaign 0 of [The
Chorography](2026-08-12-the-chorography-metaplan.md). Status: G3 review.
Branch: `the-fathom`.*

## 1. What this campaign produces

A **column-aware accessor**, and the deletion of the assumption that the world
has two realms.

Today `GeneratedClimate::biome_at(&self, cell: CellId) -> Biome`
(`domains/climate/src/provider.rs:471`) returns one place-type per cell. The
type it is built from already carries a vertical coordinate —
`BiomeExpr { realm, formation, stratum }` — and the accessor discards it. **The
accessor is behind the type.**

This campaign adds the column, uses the sea as its first consumer, fixes the one
live two-realm defect, and documents the three latent ones. It adds **no world
content** and **changes no world bytes**.

## 2. Non-goals

- **No underworld communities.** The rock column stays empty; `BiomeExpr::biome()`
  keeps its `unreachable!()` on cave formations. That is campaign 2.
- **No axis decomposition.** `Formation` stays a flat enum. That is campaign 1.
- **No sky strata.** `Medium::AirOverRock` keeps its single `Surface` rung. The
  asymmetry is real and is campaign 1's or later.
- **No behaviour repair.** Where the column makes an existing inconsistency
  visible, this campaign *reports* it and does not fix it (see §6, H-2). Fixing
  behaviour inside a refactor destroys the byte-identity proof that makes the
  refactor safe.
- **No `Biome` enum change.** No new variants, no registry migration, no
  `biome_legend` change, so the cross-repo scene contract is untouched.

## 3. The defects, measured

At `44d7fb9f`:

**Corrected while writing the implementation plan.** The first draft of this
section called all four sites re-keys. Three of them are **latent, not live**,
and the reason is structural: they read the **cell's surface projection**
(`CellMap<Biome>`), and under §2 the underworld is never a cell's biome — it is
a stratum *beneath* the cell. A cell's expression stays `OVERWORLD` or
`WATERWORLD` forever, so all three remain correct after campaign 2. Writing
re-key tasks for them would have produced three no-ops.

| # | Site | Assumption | Verdict |
|---|------|-----------|---------|
| 1 | `climate/src/provider.rs:703` | `if b.is_marine() { marine } else { land }` | **latent** — buckets cells by surface medium, which stays well-posed. Wants a per-realm *companion*, not a repair |
| 2 | `worldgen/src/graph_derive.rs:126` | `is_marine()` is the connection-graph separator | **latent** — surface traversal stays correct. The underworld's *absence* from the graph is new work (campaign 2), not a mis-classification |
| 3 | `climate/src/provider.rs:1206` | asserts `realm == WATERWORLD` ⟺ `is_marine()` | **latent** — quantified over cell expressions, which stay two-realm. Stays true |
| 4 | `vessel/src/vantage.rs:64` | `submerged := stratum != Surface` | **LIVE.** `describe_at(.., stratum)` takes `Option<Stratum>` and campaign 2 will pass a rock rung. The one real defect |
| 5 | census column `dominant-land-biome` | the dominant biome is a land question | **correctly named** — wants a companion, not a repair |

Item 4 is fixed. Items 1, 2, 3 and 5 are **documented, not changed**: each gains
a doc line naming what its predicate answers and what it does not — "this asks
whether the cell's *surface* is water; it is not a question about the column."
That is the *name the direction a check enforces* discipline, it costs no
behaviour, and it is what actually stops campaign 2 tripping over them. Re-keying
them would be churn that spends this campaign's byte-identity budget on nothing.

## 4. Design

### 4.1 The accessors

**Corrected while writing the implementation plan.** The first draft named the
new accessor `biome_expr_at(cell, stratum)`. **That name is already taken** —
`GeneratedClimate::biome_expr_at(&self, cell) -> BiomeExpr` exists at
`provider.rs:481`, and the struct already stores *both* `biome: CellMap<Biome>`
and `biome_expr: CellMap<BiomeExpr>`, with `biome` derived from `biome_expr`
(`provider.rs:252`). The draft would have been a breaking signature change to a
public function, not an addition.

Added to `GeneratedClimate`. Both existing accessors keep their signatures and
behaviour exactly:

```rust
/// Every stratum present at this cell, shallowest first.
pub fn strata_at(&self, cell: CellId) -> Vec<Stratum>;

/// The community at a cell and a stratum. `None` when that stratum is not
/// present at that cell.
pub fn biome_expr_at_stratum(&self, cell: CellId, stratum: Stratum)
    -> Option<BiomeExpr>;
```

`Vec<Stratum>` rather than a slice because the set is per-cell; the ladders
themselves stay `&'static` on `Realm::strata()`.

**Both are pure reads over the already-stored `biome_expr` map — no new inputs,
no new storage, no change to construction order.** The cell's stored expression
already carries its floor stratum, so the column is derivable from it alone.

That yields a free and strong consistency assertion, which Task 1 makes its
headline test:

```
  biome_expr_at_stratum(cell, biome_expr_at(cell).stratum) == Some(biome_expr_at(cell))
```

— the column must agree with the existing accessor at the cell's own rung, at
every cell in the world.

### 4.2 The sea's column, derived from data that already exists

`classify_marine_expr` derives `stratum = Stratum::at_depth_m(depth_m)` from the
**floor** depth, and a community from a precedence chain. The column follows
without a single new input:

```
  for an ocean cell whose floor lies in stratum F:

    strata_at(cell)              = [Epipelagic ..= F]        (shallowest first)
    biome_expr_at_stratum(cell, F)
                                 = the cell's STORED expression <- UNCHANGED,
                                                                  the seafloor community
    biome_expr_at_stratum(cell, s), s shallower than F
                                 = BiomeExpr { WATERWORLD, OpenWater, s }
    biome_expr_at_stratum(cell, s), s deeper than F
                                 = None                        (below the floor)
```

This is the physically correct reading and the one `classify_marine_expr`'s own
doc already argues for: a vent is *a community at a depth*, so the water above
it is open water at its own depth. A reef cell (floor above 200 m) has a
one-stratum column; an abyssal cell has four.

For land, `strata_at` returns `[Surface]` and `biome_expr_at_stratum(cell, Surface)`
returns the land expression. Unchanged in every respect.

### 4.3 The one live fix, and the three documented

**Fixed — `vessel/src/vantage.rs:64`.** Today:

```rust
submerged: matches!(stratum, Some(st) if st != hornvale_climate::Stratum::Surface),
```

`submerged` must ask about the **medium**, not about "is this the surface rung".
The stratum alone cannot answer it: `Basement` is not `Surface` and is not wet.
The replacement asks the realm that owns the stratum. Values are **identical
today** — no stratum other than `Surface` occurs on a land cell in any shipped
world, which is exactly why the committed client fixtures are the guard (§8) —
and correct tomorrow, when campaign 2 passes a rock rung.

Follows the recipe already executed in `windows/locale/src/grammar.rs`: capture a
before-arm fixture from unmodified code, commit it alone, re-key, assert the
output did not move.

**Documented, not changed — sites 1, 2, 3 and 5.** Each gains a doc line naming
the direction it enforces. `is_marine()` keeps its name, its signature and every
caller; its doc gains the sentence that it asks whether a cell's **surface**
medium is water and is not a question about the column. `graph_derive`'s
`marine` binding gains the note that it separates **surface** traversal, and
that underworld edges are a later campaign's addition rather than a defect in
this line.

Nothing here changes a value, which is the point: a re-key with no behavioural
difference spends the byte-identity budget and buys nothing, while a doc line
that names the direction is what a future reader actually needs.

### 4.4 Direction, stated in the doc comments

Each new accessor's doc names what it enforces and what it does not — the
discipline that a check "asserting *declared ⊆ resolvable* is structurally blind
to over-admission and still reads as total to the next person." `strata_at`
answers *which strata exist here*, never *which are reachable*: a sealed void
exists and is unreachable, and `Access` is the axis for that question.

## 5. Save-format and determinism

- **No new draws, no new streams, no new seed labels.** Every accessor is a
  pure read over already-derived data.
- **Nothing new is committed.** `strata_at` and `biome_expr_at` are derived;
  no ledger fact, no epoch, no `/v2` label.
- **The scene contract is untouched.** `biome_at` is unchanged, so
  `biome_legend` and its append-only cross-repo order do not move.
- **Type-audit:** the two new `pub` functions carry verdict tags in the same
  commit as their introduction, and `docs/audits/type-audit-report.md` is
  regenerated in that commit.

## 6. Preregistered predictions

Frozen before the code. **W** is a witness; **H** are hypotheses that can fail.

**W-1 — the re-key is invisible.** Every committed artifact is byte-identical
after this campaign, and the seed-42 world is byte-identical. A *witness, not a
hypothesis test*: once the accessors are additive and the one fix preserves
values, this is true by construction, and it exists to catch a regression that
reintroduces a behavioural change.

**H-1 — the sea's column is non-degenerate.** Over seed 42's ocean cells, the
distribution of column height (count of strata present) satisfies **all** of:

- at least **3** distinct column heights occur;
- the **median** column height is **≥ 3** strata;
- **fewer than 5%** of ocean cells are single-stratum;
- **no single height holds more than 90%** of ocean cells.

The last clause is the ceiling: a degenerate depth field would pin nearly every
cell at one height, and a floor-only prediction could not tell that apart from
a healthy world. Falsified if any clause fails.

**Measured on seed 42** (`BuildDepth::Terrain`, `windows/worldgen/tests/
fathom_column_probe.rs`), over 29,896 `Realm::WATERWORLD` cells — column
height → cell count: `{1: 1,749, 2: 6,669, 3: 21,478}`.

| clause | requirement | measured | verdict |
|---|---|---|---|
| 1 (distinct heights) | ≥ 3 | 3 | CONFIRMED |
| 2 (median height) | ≥ 3 | 3 | CONFIRMED |
| 3 (single-rung share) | < 5% | 1,749 / 29,896 = 5.85% | **PREREGISTERED, NOT MET** |
| 4 (ceiling, tallest bucket) | ≤ 90% | 21,478 / 29,896 = 71.8% | CONFIRMED |

Clauses 1, 2 and 4 confirm: the column is not degenerate — three distinct
heights occur, the median cell is three strata deep, and no single height
dominates. Clause 3 measured 5.85% against its <5% ceiling — narrowly over,
and preregistered-not-met rather than weakened. Nathan's ruling: **the world
is not wrong, the clause was.** A single-rung column is a floor shallower than
200 m — a continental-shelf cell — and Earth's own shelf is roughly 7-8% of
ocean area, so 5.85% is physically unremarkable; the 5% ceiling was authored
before anyone measured a real world's shelf fraction against it. The
threshold is not being moved (a threshold moved after unblinding is worth less
than a falsification kept on the record); clause 3 is carried under the
repo's `PREREGISTERED, not met:` idiom
(`h1_clause_3_single_rung_share_preregistered_not_met`), filed to
[`CLIM-shelf-single-rung-threshold`](https://github.com/hornvale/hornvale/blob/main/book/src/frontier/idea-registry.md)
for a successor to re-derive the ceiling from a measured seed set.

Separately: the all-or-nothing bundling of four clauses under one stop
condition was itself a preregistration-design defect, independent of clause
3's miscalibration — the intent behind clause 3 (catch a degenerate column)
is amply met by this distribution (94.15% multi-rung cells, 3 distinct
heights, no bucket over 71.8%), which is why clauses 1/2/4 were split into
their own heavy-tier test rather than staying gated behind clause 3's result.

**H-2 — sea ice occurs below the epipelagic.** `classify_marine_expr` selects
`Formation::SeaIce` in its **first** arm, on sea-surface temperature alone,
with no depth condition, while `stratum` is derived from the floor. So a polar
abyssal cell should yield `BiomeExpr { WATERWORLD, SeaIce, Abyssal }` — ice at
4 km down.

Predicted: **at least one** seed-42 cell pairs `SeaIce` with a stratum deeper
than `Epipelagic`. If confirmed, it is **reported and not repaired** — the
repair changes world bytes and belongs to campaign 1, which is already
re-authoring what a community means. If falsified, the marine classification is
already depth-consistent and campaign 1 inherits one fewer problem.

This is the honest shape: H-2 is a prediction *about a defect*, and either
outcome is a finding.

**Measured on seed 42** (same build as H-1): 9,695 `Formation::SeaIce` cells
total (a non-empty denominator, so H-2 is measurable rather than a null), and
**8,916 of them (91.96%) sit below the epipelagic** — example cell `CellId(4)`
at `Stratum::Bathypelagic`. **CONFIRMED, strongly** — this is not a marginal
artifact: nearly all sea ice the model places is filed at depth, because
`classify_marine_expr` reads only surface temperature while `stratum` is read
from the floor, and the column Task 1's accessors expose is what made the
mismatch visible for the first time. Recorded as an artifact for campaign 1;
**not repaired here**, per spec §2.

## 7. Acceptance criteria

- [ ] `strata_at` and `biome_expr_at` exist, documented, type-audit-tagged.
- [ ] The sea's column is derivable at every ocean cell, with the floor stratum
      carrying the seafloor community and shallower strata open water.
- [ ] `vantage.rs`'s `submerged` asks the realm's medium, not the stratum; sites
      1, 2, 3 and 5 each carry a doc line naming the direction they enforce;
      `is_marine()` keeps its name, signature and every caller.
- [ ] W-1 holds: `make rebaseline` produces an empty diff across
      `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/`,
      `docs/audits/`, `docs/digest/`, `book/src/domesday/` and
      `clients/game/core/tests/fixtures/` — **except** `docs/audits/`, which
      drifts because this campaign adds `pub` items, and is regenerated in the
      same commit that adds them.
- [ ] H-1 and H-2 measured and reported, whatever they say.
- [ ] `make gate` green; `make gate-full` green.
- [ ] Book chronicle entry; freshness sweep of the biome and stratum chapters;
      retrospective in `docs/retrospectives/`.

Note the branch table rather than a prediction on the rebaseline diff: *`docs/audits/`
moved → regenerate and commit in the same commit; anything else moved → STOP,
the re-key was not invisible and W-1 has failed.*

## 8. Risks

**The `vantage.rs` re-key touches the client-facing snapshot.** Mitigation: the
committed seed-42 session fixtures in `clients/game/core/tests/fixtures/` are
part of W-1's byte-identity set, so a behavioural change there fails the gate
rather than shipping.

**Collision with live campaigns.** `the-rill` works in `domains/terrain/` and
`windows/locale/`; `the-fathom` works in `domains/climate/`, `windows/worldgen/`
and `windows/vessel/`. Overlap is small but not zero — both touch
`windows/worldgen/`. Absorb main at every task boundary, per the standing rule.

**The temptation to fix things.** The column will make more than H-2 visible.
Everything it exposes goes to the followup register and campaign 1; the value of
this campaign is entirely in its byte-identity proof, and one "obvious" repair
destroys it.

## 9. Definition of done

The standard project definition, plus: the metaplan's §7 reconstruction test is
written into campaign 1's spec before campaign 1 opens, so the axis list cannot
reach implementation unvalidated.

## 10. Decisions promoted from the ledger

Auto-resolved under `campaign-autopilot` during the brainstorm and plan
stages; promoted here in full, since `.superpowers/sdd/decision-ledger.md`
is git-ignored scratch that does not survive the worktree.

- **The legacy `Biome` enum gains no rock variants.** The underworld exists
  only as a `BiomeExpr`; widening `Biome` would drag glyph, colour,
  `biome_class`, the map, the census, and the append-only cross-repo
  `biome_legend` behind it for no reachable benefit yet. 1 ideonomy pass, 0
  overturns.
- **A chamber's community is not `f(CaveKind, stratum)`.** Overturned
  mid-brainstorm (2 passes, 1 overturn): cave kind is an *input* to a future
  classification, not its output — `Formation` already means community, and
  `KarstCave`/`LavaTube`/`FractureCave` are the process that made the void,
  misfiled into the community slot before the underworld had life to name.
  The discarded alternative, `f(CaveKind, deepest_band)`, would have capped
  the underworld at three communities forever. Belongs to campaign 2.
- **The sea and the underworld are not separate planes with their own
  index.** Column realms (this campaign's subject) co-register on one
  `CellId` space and differ only by stratum; only *transit* realms
  (campaign 3) get their own index. The coupling is physical —
  `is_ocean = elevation < sea_level` makes bathymetry and topography the
  same field, and a water cycle cannot close across independently-indexed
  spaces. 2 passes, 0 overturns.
- **Mangroves, cypress domes, and gallery forests are not taxonomy.** They
  are room-grain content (~1 km) inside a ~110 km cell and belong to the
  fine layer (The Grain's territory), not `Formation` — the same category
  error as `Formation::KarstCave` would have been. 2 passes, 0 overturns.
- **Campaign 0's scope is a pure refactor gated on byte-identical worlds,
  plus the sea's column as its first consumer.** The expand-contract recipe
  (captured before-arm, draw-for-draw assertion) was already executed once
  in this tree (`windows/locale/src/grammar.rs`'s `Biome`-to-`Formation`
  re-key); a byte-identity proof is what makes a cross-cutting re-key safe
  to review at all. Discarded: including the sky's upper strata (new
  content, breaks the proof) and repairing the sea-ice-at-depth artifact
  (same reason — it ships as H-2, a reported finding, instead).
- **`dominant-land-biome` is not renamed.** It is correctly named — a land
  question — and wants a per-realm companion column, not a repair; renaming
  a committed census column for no behavioural change is pure artifact
  drift, and drift checks freeze what they pin. Companion columns belong to
  campaign 2.
- **Campaign naming: The Chorography (program), The Fathom (campaign 0).**
  Grepped before adopting — "The Column" hits 61 files as a common noun and
  would be unsearchable, "The Reaches" collides with The Rill's own reach
  vocabulary, "The Gazetteer" hits 8 files. Both chosen names hit 0.
- **The axis list for campaign 1 is validated by reconstruction against the
  existing corpus, not by argument.** The proposed six axes are this
  author's synthesis, never measured, and two are suspected collinear with
  inputs `classify_land` already reads — which would make the decomposition
  a factoring rather than a fidelity gain. Campaign 1 must say which,
  measured, before building on the list. Captured as a gate on campaign 1's
  Definition of Done (§9 above), not left as a hope.
- **G4 plan self-review: proceed to execution.** Two spec defects were found
  and corrected while *writing the plan* (the accessor name collision with
  the existing `biome_expr_at(cell)`, and three of four "binary-world
  assumption" sites turning out latent rather than live), and two plan
  defects were found by checking rather than assuming (no `GeneratedClimate`
  constructor reachable from `domains/climate/tests/`, and
  `hornvale-game-core` excluded from the cargo workspace). All four are the
  same failure — a document asserting something about code it had not
  read — caught by running a command in every case, never by re-reading the
  prose. See the retrospective for the full accounting, which grew to seven
  instances by the campaign's close.
