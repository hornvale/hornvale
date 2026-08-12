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

This campaign adds the column, uses the sea as its first consumer, and re-keys
four sites that treat "not marine" as "land". It adds **no world content** and
**changes no world bytes**.

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

| # | Site | Assumption | Consequence once a third realm exists |
|---|------|-----------|----------------------------------------|
| 1 | `climate/src/provider.rs:703` | `if b.is_marine() { marine } else { land }` | a cavern is tallied as land |
| 2 | `worldgen/src/graph_derive.rs:126` | `is_marine()` is the connection-graph separator | the underworld is absorbed into land adjacency, destroying the shortcut property |
| 3 | `climate/src/provider.rs:1206` | asserts `realm == WATERWORLD` ⟺ `is_marine()` | the assertion becomes false and the invariant is lost |
| 4 | `vessel/src/vantage.rs:64` | `submerged := stratum != Surface` | a chamber tells the client the player is underwater |
| 5 | census column `dominant-land-biome` | the world's dominant biome is a land question | no defect — it is *correctly* named, and needs a **companion**, not a repair |

Items 1–4 are re-keys. Item 5 is deliberately left alone: renaming a committed
census column for no behavioural change is churn and artifact drift, and its
companions belong with the realms that need them.

## 4. Design

### 4.1 The accessors

Added to `GeneratedClimate`, alongside the existing `biome_at`, which keeps its
signature and its behaviour exactly:

```rust
/// Every stratum present at this cell, shallowest first.
pub fn strata_at(&self, cell: CellId) -> Vec<Stratum>;

/// The community at a cell and a stratum. `None` when that stratum is not
/// present at that cell.
pub fn biome_expr_at(&self, cell: CellId, stratum: Stratum) -> Option<BiomeExpr>;
```

`Vec<Stratum>` rather than a slice because the set is per-cell; the ladders
themselves stay `&'static` on `Realm::strata()`.

### 4.2 The sea's column, derived from data that already exists

`classify_marine_expr` derives `stratum = Stratum::at_depth_m(depth_m)` from the
**floor** depth, and a community from a precedence chain. The column follows
without a single new input:

```
  for an ocean cell whose floor lies in stratum F:

    strata_at(cell)              = [Epipelagic ..= F]        (shallowest first)
    biome_expr_at(cell, F)       = classify_marine_expr(...)  <- UNCHANGED, the
                                                                 seafloor community
    biome_expr_at(cell, s) for s shallower than F
                                 = BiomeExpr { WATERWORLD, OpenWater, s }
    biome_expr_at(cell, s) for s deeper than F
                                 = None                       (below the floor)
```

This is the physically correct reading and the one `classify_marine_expr`'s own
doc already argues for: a vent is *a community at a depth*, so the water above
it is open water at its own depth. A reef cell (floor above 200 m) has a
one-stratum column; an abyssal cell has four.

For land, `strata_at` returns `[Surface]` and `biome_expr_at(cell, Surface)`
returns the land expression. Unchanged in every respect.

### 4.3 The re-keys

Each follows the recipe already executed in `windows/locale/src/grammar.rs`:
capture a before-arm fixture from unmodified code, commit it alone, re-key,
assert the output did not move.

```
  1,2,3  is_marine()  ->  the realm's medium
         `realm_at(cell) -> Realm`, and consumers ask `medium == Medium::Water`.
         `is_marine()` is RETAINED as a legacy surface predicate so no external
         consumer breaks; its doc gains the sentence naming what it can and
         cannot answer.

  4      vantage.rs   ->  submerged := realm_at(cell).medium == Medium::Water
         Identical values today, because no stratum other than Surface occurs
         on a land cell. Correct tomorrow, when one does.
```

Item 2 deserves its own note: `graph_derive` does not actually want *marine* —
it wants **"is this cell traversable by land travel"**. Re-keying it to the
medium states the real question and leaves the values untouched.

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
hypothesis test*: once the accessors are additive and the re-keys preserve
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

## 7. Acceptance criteria

- [ ] `strata_at` and `biome_expr_at` exist, documented, type-audit-tagged.
- [ ] The sea's column is derivable at every ocean cell, with the floor stratum
      carrying the seafloor community and shallower strata open water.
- [ ] Sites 1–4 read the realm's medium; `is_marine()` survives as a documented
      legacy surface predicate.
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
