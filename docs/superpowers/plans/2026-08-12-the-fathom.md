# The Fathom Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give every realm its depth coordinate — a column-aware accessor on
`GeneratedClimate` — and prove it changed no world bytes.

**Architecture:** Two additive accessors (`strata_at`, `biome_expr_at_stratum`)
derived purely from the already-stored `biome_expr: CellMap<BiomeExpr>`. No new
inputs, no new storage, no change to construction. One live defect fixed
(`vantage.rs`'s `submerged`), three latent sites documented rather than
re-keyed.

**Tech Stack:** Rust 2024, `hornvale-climate` / `hornvale-vessel` /
`hornvale-worldgen`, `cargo nextest`, the project's own type-audit tool.

**Spec:** [The Fathom](../specs/2026-08-12-the-fathom-design.md) ·
**Program:** [The Chorography](../specs/2026-08-12-the-chorography-metaplan.md)

## Global Constraints

- **No new dependencies.** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap`/`HashSet`.** `BTreeMap` / `BTreeSet` / `Vec` only, enforced by
  `clippy.toml` `disallowed-types`.
- **No wall-clock time.** `std::time::Instant` is banned in test code too.
- **Every `pub` item gets a one-line doc comment** (`#![warn(missing_docs)]`).
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag**,
  and `docs/audits/type-audit-report.md` is regenerated **in the same commit**
  that adds the `pub` item — it drifts on any pub-boundary change.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the
  most common review finding here.
- **A signature change and its call sites cannot be separate commits** — the
  pre-commit hook runs `make quick` workspace-wide and the intermediate state
  does not compile. Bypassing the hook is forbidden.
- **Byte-identity is this campaign's acceptance criterion.** Nothing here may
  change a world, an almanac, a fixture, or a census value.
- **Absorb main at every task boundary** — `make preflight` from the branch; on
  an ancestry NO-GO merge main *into* the branch and re-run there.

---

## File Structure

| File | Responsibility |
|---|---|
| `domains/climate/src/provider.rs` | **Modify.** Add `strata_at` and `biome_expr_at_stratum` to `GeneratedClimate`, beside the existing `biome_at` / `biome_expr_at`, which are untouched. Add the direction-naming doc lines to sites 1 and 3. |
| `domains/climate/src/biome.rs` | **Modify.** One doc line on `is_marine()` naming what it answers. No signature or body change. |
| `domains/climate/tests/column.rs` | **Create.** AMENDED AT EXECUTION: `provider::test_support` is `#[cfg(test)]`-gated and therefore invisible to an integration test (proven with an E0432 during Task 1), and ungating it would pull it into the type-audit's pub surface. So the column's tests are an integration test carrying a ~15-line local fixture, documented at the top of the file. |
| `windows/worldgen/tests/fathom_column_probe.rs` | **Create.** The H-1 / H-2 measurement over a real seed-42 world. Heavy-tier. |
| `windows/vessel/src/vantage.rs` | **Modify.** `submerged` asks the medium, not the stratum. |
| `windows/vessel/tests/submerged.rs` | **Create.** The failing-first test that a rock stratum is not submerged. |
| `windows/worldgen/src/graph_derive.rs` | **Modify.** One doc line on the `marine` binding. No behaviour change. |

---

### Task 1: The column accessors

**Files:**
- Modify: `domains/climate/src/provider.rs` — two methods near `biome_expr_at` (`:481`), and their tests in the file's existing `#[cfg(test)] mod tests`

**Interfaces:**
- Consumes: the existing `GeneratedClimate::biome_expr_at(&self, cell: CellId) -> BiomeExpr`, the stored `biome_expr: CellMap<BiomeExpr>`, and `hornvale_climate::{Realm, Stratum, Formation, BiomeExpr}`.
- Produces, for every later task:
  - `pub fn strata_at(&self, cell: CellId) -> Vec<Stratum>`
  - `pub fn biome_expr_at_stratum(&self, cell: CellId, stratum: Stratum) -> Option<BiomeExpr>`

**The rule the implementation must satisfy** (write this as the doc comment,
not as prose in the plan):

```
  let e = biome_expr_at(cell);           // the cell's stored expression
  let ladder = e.realm.strata();         // &'static [Stratum], shallowest first
  let floor = position of e.stratum in ladder;

  strata_at(cell)                      = ladder[0 ..= floor]
  biome_expr_at_stratum(cell, s):
      s not in ladder                  -> None    (wrong realm's rung entirely)
      position(s) >  floor             -> None    (below this cell's floor)
      position(s) == floor             -> Some(e) (the cell's own expression,
                                                   UNCHANGED -- this is what makes
                                                   the identity below hold)
      position(s) <  floor             -> Some(BiomeExpr { realm: e.realm,
                                                           formation: OpenWater,
                                                           stratum: s })
```

Land falls out without a special case: `OVERWORLD.strata()` is `[Surface]`, so
`floor == 0`, the column is `[Surface]`, and the only legal query returns the
cell's own expression.

- [ ] **Step 1: Write the failing tests**

**Where these go, and why not an integration test.** Every existing
`GeneratedClimate` test lives in `provider.rs`'s own `#[cfg(test)] mod tests`
and builds one via `GeneratedClimate::generate(&inputs(&geo, &elev, &sea,
regime))`, where `inputs(..)` is a **private** helper in that module
(`provider.rs:768` and following). `ClimateInputs` is `pub`, so an integration
test *could* assemble one — at the cost of duplicating that helper. Follow the
existing pattern and add these to the same test module. The project's own rule
is that the split is by what a test needs, never by unit-vs-integration.

Add the six tests below to that module. Each opens with a placeholder comment
where the climate comes from — fill it with the module's own
`GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime))` idiom and the
`geo` those helpers already build. **Do not add a new constructor**; if the
existing helpers are awkward to reuse six times, factor them once inside that
module rather than duplicating them.

```rust
//! THE FATHOM: the column — every stratum present at a cell, and the
//! community at each. Additive over the stored `biome_expr`.

/// THE HEADLINE. The column must agree with the accessor that already
/// exists, at the cell's own rung, at EVERY cell. This is what makes the
/// column a re-reading of the stored expression rather than a second,
/// silently-diverging derivation of it.
#[test]
fn the_column_agrees_with_biome_expr_at_at_every_cell() {
    let (geo, climate) = /* the module's own inputs(..) idiom -- see above */;
    for cell in geo.cells() {
        let e = climate.biome_expr_at(cell);
        assert_eq!(
            climate.biome_expr_at_stratum(cell, e.stratum),
            Some(e),
            "column disagrees with biome_expr_at at {cell:?}"
        );
    }
}

/// A land cell's column is exactly one rung.
#[test]
fn a_land_cell_has_a_one_rung_column() {
    let (geo, climate) = /* the module's own inputs(..) idiom -- see above */;
    let land = geo
        .cells()
        .find(|c| climate.biome_expr_at(*c).realm == Realm::OVERWORLD)
        .expect("seed 42 has land");
    assert_eq!(climate.strata_at(land), vec![Stratum::Surface]);
}

/// A marine cell's column runs from the surface down to its own floor, in
/// order, with no gaps and no repeats.
#[test]
fn a_marine_column_runs_from_the_surface_to_its_own_floor() {
    let (geo, climate) = /* the module's own inputs(..) idiom -- see above */;
    for cell in geo.cells() {
        let e = climate.biome_expr_at(cell);
        if e.realm != Realm::WATERWORLD {
            continue;
        }
        let column = climate.strata_at(cell);
        let ladder = e.realm.strata();
        assert_eq!(column.first(), Some(&ladder[0]), "column must start at the top");
        assert_eq!(column.last(), Some(&e.stratum), "column must end at the floor");
        let expected: Vec<Stratum> = ladder
            .iter()
            .copied()
            .take_while(|s| *s != e.stratum)
            .chain(std::iter::once(e.stratum))
            .collect();
        assert_eq!(column, expected, "column is the ladder prefix at {cell:?}");
    }
}

/// Below the floor is not a place. A stratum deeper than the cell's own is
/// absent, not empty-but-present.
#[test]
fn nothing_exists_below_the_floor() {
    let (geo, climate) = /* the module's own inputs(..) idiom -- see above */;
    for cell in geo.cells() {
        let e = climate.biome_expr_at(cell);
        let ladder = e.realm.strata();
        let floor = ladder.iter().position(|s| *s == e.stratum).expect("floor is on its own ladder");
        for deeper in &ladder[floor + 1..] {
            assert_eq!(
                climate.biome_expr_at_stratum(cell, *deeper),
                None,
                "{deeper:?} is below the floor at {cell:?} and must be absent"
            );
        }
    }
}

/// Water above a seafloor community is open water AT ITS OWN DEPTH — the
/// reading `classify_marine_expr`'s own doc argues for ("a vent is a
/// community AT a depth"). Asserted on a cell deep enough to have water
/// above it, and skipped as inapplicable if this world has none.
#[test]
fn water_above_the_floor_is_open_water_at_its_own_depth() {
    let (geo, climate) = /* the module's own inputs(..) idiom -- see above */;
    let deep = geo.cells().find(|c| climate.strata_at(*c).len() > 1);
    let Some(deep) = deep else {
        panic!("H-1 says >95% of ocean cells are multi-rung; found none — \
                the column is degenerate and this is the finding, not a skip");
    };
    let column = climate.strata_at(deep);
    for s in &column[..column.len() - 1] {
        let above = climate
            .biome_expr_at_stratum(deep, *s)
            .expect("a stratum in the column is present by construction");
        assert_eq!(above.formation, Formation::OpenWater, "at {s:?}");
        assert_eq!(above.stratum, *s, "an expression carries its own stratum");
    }
}

/// A rung from another realm's ladder is not a query this cell can answer.
#[test]
fn a_rung_from_another_realms_ladder_is_absent() {
    let (geo, climate) = /* the module's own inputs(..) idiom -- see above */;
    let land = geo
        .cells()
        .find(|c| climate.biome_expr_at(*c).realm == Realm::OVERWORLD)
        .expect("seed 42 has land");
    assert_eq!(climate.biome_expr_at_stratum(land, Stratum::Abyssal), None);
    assert_eq!(climate.biome_expr_at_stratum(land, Stratum::Basement), None);
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo nextest run -p hornvale-climate provider`
Expected: **FAIL to compile**, with `no method named strata_at` and `no method
named biome_expr_at_stratum`.

**A compile-error red proves nothing about an assertion** — it only proves the
method is absent. That is acceptable *here* because these tests assert on a
type that does not exist yet; the behavioural red that matters in this campaign
is Task 3's, which is captured against the live surface before any code moves.

- [ ] **Step 3: Implement**

In `domains/climate/src/provider.rs`, beside `biome_expr_at`:

```rust
    /// Every stratum present at a cell, shallowest first — the cell's
    /// **column**.
    ///
    /// Derived from the cell's stored [`BiomeExpr`] alone: its `realm` names
    /// the ladder and its `stratum` names how far down this cell's floor
    /// reaches, so the column is that ladder's prefix. Pure; no new inputs.
    ///
    /// **Direction:** this answers *which strata exist here*, never *which are
    /// reachable*. A sealed void exists and is unreachable; reachability is
    /// [`hornvale_climate::Access`]'s question, not this one.
    pub fn strata_at(&self, cell: CellId) -> Vec<Stratum> {
        let e = self.biome_expr_at(cell);
        let ladder = e.realm.strata();
        let floor = ladder
            .iter()
            .position(|s| *s == e.stratum)
            .expect("a cell's stratum is always on its own realm's ladder");
        ladder[..=floor].to_vec()
    }

    /// The community at a cell and a stratum, or `None` when that stratum is
    /// not present there — below this cell's floor, or on another realm's
    /// ladder entirely.
    ///
    /// At the cell's own stratum this returns the stored expression
    /// unchanged, which is what keeps it and [`Self::biome_expr_at`] from
    /// drifting apart. Above it, the water is open water at its own depth —
    /// the reading [`crate::biome::classify_marine_expr`] already argues for,
    /// where a vent is a community *at* a depth rather than one that displaced
    /// a depth.
    pub fn biome_expr_at_stratum(&self, cell: CellId, stratum: Stratum) -> Option<BiomeExpr> {
        let e = self.biome_expr_at(cell);
        let ladder = e.realm.strata();
        let floor = ladder
            .iter()
            .position(|s| *s == e.stratum)
            .expect("a cell's stratum is always on its own realm's ladder");
        let here = ladder.iter().position(|s| *s == stratum)?;
        match here.cmp(&floor) {
            std::cmp::Ordering::Greater => None,
            std::cmp::Ordering::Equal => Some(e),
            std::cmp::Ordering::Less => Some(BiomeExpr {
                realm: e.realm,
                formation: Formation::OpenWater,
                stratum,
            }),
        }
    }
```

Add `Formation` and `Stratum` to the file's `use` list if not already imported.

**If `Formation::OpenWater` above a land cell is unreachable** — it is, because
land's ladder has one rung so `here < floor` cannot occur — leave the arm as
written rather than special-casing the realm. The arm is correct for any realm
whose ladder is longer than one, which is the point of writing it this way.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo nextest run -p hornvale-climate provider`
Expected: PASS — the six new tests plus the module's existing ones, all green.

- [ ] **Step 5: Tag and regenerate the type audit**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

If it names either new method, add the verdict tag it asks for to that
method's doc comment, then:

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

**Branch table** — the report drifts on any pub-boundary change, and this task
adds two:
- `docs/audits/type-audit-report.md` moved → **expected**; commit it in this
  same commit.
- nothing moved → also fine; `CellId`, `Stratum` and `BiomeExpr` are all
  non-primitive, so the tool may have nothing to say. Do not go looking for a
  way to make it move.

- [ ] **Step 6: Verify nothing else moved**

Run: `cargo nextest run -p hornvale-climate`
Then: `cargo fmt && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: green. These methods are additive and no existing code calls them, so
no existing test may change.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add domains/climate/src/provider.rs docs/audits/type-audit-report.md
git commit -m "feat(climate): the column — every stratum present at a cell"
```

---

### Task 2: Measure H-1 and H-2

**Files:**
- Create: `windows/worldgen/tests/fathom_column_probe.rs`
- Modify: `docs/superpowers/specs/2026-08-12-the-fathom-design.md` (§6, record the measured numbers beside the predictions)

**Interfaces:**
- Consumes: Task 1's `strata_at` and `biome_expr_at_stratum`; the project's
  existing seed-42 world construction in `windows/worldgen`.
- Produces: measured values only. No code any later task calls.

This task **reports and does not repair**, per spec §2. If H-2 confirms, the
sea-ice-at-depth artifact is written down and left alone.

- [ ] **Step 1: Write the probe**

Create `windows/worldgen/tests/fathom_column_probe.rs`. It builds a seed-42
world, so it is heavy-tier:

```rust
//! THE FATHOM: the preregistered measurements, H-1 and H-2 (spec §6).
//! Reports; repairs nothing.

use std::collections::BTreeMap;

/// H-1 — the sea's column is non-degenerate. Four clauses, ALL required:
/// at least 3 distinct column heights occur; the median height is >= 3;
/// fewer than 5% of ocean cells are single-rung; and no single height holds
/// more than 90% of ocean cells. The last clause is the ceiling — a floor-only
/// prediction cannot tell a healthy world from a degenerate depth field.
#[test]
#[ignore = "heavy: builds a full seed-42 world to measure the sea's column distribution"]
fn h1_the_seas_column_is_non_degenerate() {
    // Build the seed-42 world using whatever constructor this crate's own
    // tests already use -- read windows/worldgen/tests/ first and reuse it.
    // Then, over every cell whose realm is WATERWORLD:
    let mut heights: BTreeMap<usize, usize> = BTreeMap::new();
    // heights.entry(climate.strata_at(cell).len()).and_modify(|n| *n += 1).or_insert(1);

    let ocean: usize = heights.values().sum();
    assert!(ocean > 0, "seed 42 must have ocean cells");
    let distinct = heights.len();
    let single = *heights.get(&1).unwrap_or(&0);
    let tallest_bucket = *heights.values().max().expect("non-empty");
    let median = {
        let mut seen = 0;
        let mut m = 0;
        for (h, n) in &heights {
            seen += n;
            if seen * 2 >= ocean {
                m = *h;
                break;
            }
        }
        m
    };

    println!("H-1 column heights over {ocean} ocean cells: {heights:?}");
    println!("H-1 distinct={distinct} median={median} single={single} tallest_bucket={tallest_bucket}");

    assert!(distinct >= 3, "H-1 clause 1: only {distinct} distinct column heights");
    assert!(median >= 3, "H-1 clause 2: median height {median}");
    assert!(
        (single as f64) < 0.05 * ocean as f64,
        "H-1 clause 3: {single} of {ocean} ocean cells are single-rung"
    );
    assert!(
        (tallest_bucket as f64) <= 0.90 * ocean as f64,
        "H-1 clause 4 (the CEILING): one height holds {tallest_bucket} of {ocean} \
         ocean cells — the depth field is degenerate"
    );
}

/// H-2 — sea ice occurs below the epipelagic. `classify_marine_expr` selects
/// `Formation::SeaIce` in its FIRST arm on surface temperature alone, with no
/// depth condition, while `stratum` comes from the floor. Predicted: at least
/// one seed-42 cell pairs SeaIce with a stratum deeper than Epipelagic.
///
/// **Either outcome is a finding.** Confirmed: an artifact for campaign 1,
/// recorded and NOT repaired here (repair changes world bytes, spec §2).
/// Falsified: the marine classification is already depth-consistent.
#[test]
#[ignore = "heavy: builds a full seed-42 world to count sea-ice-below-epipelagic cells"]
fn h2_sea_ice_below_the_epipelagic() {
    // Count cells whose stored expression has formation == SeaIce and
    // stratum != Epipelagic; print that count, the SeaIce total, and one
    // example cell.
    //
    // ASSERT THE DENOMINATOR, NOT THE VERDICT. A test that asserted the
    // defect is present goes red the day campaign 1 fixes it -- the wrong
    // direction for a ratchet. But a probe that asserts nothing cannot tell
    // "measured zero" from "measured nothing", and a null needs its
    // denominator. So: assert the world has SeaIce cells at all, and print
    // how many of them sit below the epipelagic.
    let sea_ice_total = /* count formation == SeaIce over all cells */;
    let below_epipelagic = /* of those, how many have stratum != Epipelagic */;
    println!("H-2: {below_epipelagic} of {sea_ice_total} sea-ice cells sit below the epipelagic");
    assert!(
        sea_ice_total > 0,
        "H-2 has no denominator: seed 42 has no sea-ice cells at all, so this \
         probe measured nothing rather than measuring zero"
    );
}
```

**If `sea_ice_total` is 0**, that is not a passing probe — it is a probe with
no population, and the assertion above is what distinguishes the two. Report it
as *H-2 not measurable on seed 42* rather than as *H-2 falsified*, and say so
in the spec.

**The `todo!()` is deliberate and must not survive Step 2** — it marks the one
place where the plan cannot write the code, because the counting loop depends
on the world constructor this crate actually exposes. Read it, then write the
count.

**H-2 asserts nothing.** It prints. A test that asserted "the defect is
present" would go red the day campaign 1 fixes it, which is the wrong
direction for a ratchet.

- [ ] **Step 2: Run the probe**

Run: `cargo nextest run -p hornvale-worldgen --test fathom_column_probe --run-ignored all 2>&1 | tee /tmp/hv-fathom-probe.txt`

Read the file. Do not re-run to see a second line.

- [ ] **Step 3: Record the result in the spec**

Edit spec §6, appending the measured values beneath each prediction. Write what
was measured, not whether you are pleased with it.

**Branch table:**
- H-1 passes → record the distribution and move on.
- **H-1 fails any clause → STOP and report.** A degenerate sea column means
  either the depth field or Task 1's derivation is wrong, and both invalidate
  the campaign's premise. Do not weaken a clause to make it pass.
- H-2 count > 0 → record it, add a followup for campaign 1, **do not repair**.
- H-2 count == 0 → record the null; campaign 1 inherits one fewer problem.

- [ ] **Step 4: Confirm the heavy-tier reason parses**

Run: `cargo nextest run -p hornvale --test heavy_tier`
Expected: PASS. The `heavy:` token is matched **verbatim**; if this reddens,
the reason string is malformed, not the tier.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/fathom_column_probe.rs docs/superpowers/specs/2026-08-12-the-fathom-design.md
git commit -m "test(the-fathom): measure H-1 and H-2, and record what they said"
```

---

### Task 3: `submerged` asks the medium, and the latent sites say so

**Files:**
- Create: `windows/vessel/tests/submerged.rs`
- Modify: `windows/vessel/src/vantage.rs:64`
- Modify: `domains/climate/src/biome.rs` (`is_marine()` doc only)
- Modify: `domains/climate/src/provider.rs` (sites 1 and 3, doc only)
- Modify: `windows/worldgen/src/graph_derive.rs:126` (doc only)

**Interfaces:**
- Consumes: `hornvale_climate::{Realm, Medium, Stratum}`.
- Produces: nothing later tasks call. This is a behaviour fix plus doc lines.

- [ ] **Step 1: Capture the before-arm**

On **unmodified** code, dump `submerged` for a deterministic sample of
positions from the committed seed-42 session fixtures — both bands — to a
fixture under `windows/vessel/tests/fixtures/`. Commit it **alone**, before any
change. A before-arm re-derived from the new code proves nothing.

```bash
git add windows/vessel/tests/fixtures/
git commit -m "test(vessel): capture submerged's before-arm, unmodified"
```

- [ ] **Step 2: Write the failing test**

The property: **a rock stratum is not submerged, and a water stratum is.**
`Stratum::Basement` is not `Surface` and is not wet, and today's predicate
cannot tell those apart.

`vantage`'s `submerged` is currently computed inline inside a function that
needs a whole world. **Extract the predicate so a test can reach it** — the
exact factoring is yours; name it for the question it answers, put it beside
the call site, and give it a doc comment naming its direction. Then:

```rust
//! THE FATHOM: `submerged` is a question about the MEDIUM, not about which
//! rung of a ladder you are standing on.

use hornvale_climate::{Realm, Stratum};

/// The live defect. Today `submerged` is `stratum != Surface`, so any rock
/// rung reads as underwater — which is what the game client would be told the
/// moment campaign 2 walks a player into a chamber.
#[test]
fn a_rock_stratum_is_not_submerged() {
    for rung in Realm::UNDERDARK.strata() {
        assert!(
            !hornvale_vessel::vantage::submerged_in(Some(*rung)),
            "{rung:?} is rock, not water"
        );
    }
}

/// The other direction, so the fix cannot be "always false".
#[test]
fn a_water_stratum_is_submerged() {
    for rung in Realm::WATERWORLD.strata() {
        assert!(
            hornvale_vessel::vantage::submerged_in(Some(*rung)),
            "{rung:?} is water"
        );
    }
}

/// And the surface, and the absent case, keep their present answers — this
/// is the half that must not move.
#[test]
fn the_surface_and_the_absent_case_are_unchanged() {
    assert!(!hornvale_vessel::vantage::submerged_in(Some(Stratum::Surface)));
    assert!(!hornvale_vessel::vantage::submerged_in(None));
}
```

Adjust the path/name to the factoring you chose.

- [ ] **Step 3: Run to verify it fails — and check WHICH way**

Run: `cargo nextest run -p hornvale-vessel --test submerged`
Expected: `a_rock_stratum_is_not_submerged` **FAILS on the assertion**, not on
a compile error, once the extraction from Step 2 exists. The other two pass.

**This is the campaign's one behavioural red.** If it fails to compile instead,
finish the extraction before continuing — a compile-error red says nothing
about whether an assertion would have caught the defect.

- [ ] **Step 4: Implement**

`submerged` asks whether the stratum's realm has `Medium::Water`. Derive the
realm from the stratum rather than threading a new parameter through
`vantage_at` — `Realm::WATERWORLD.strata()` and `Realm::UNDERDARK.strata()`
are `&'static`, so membership is a cheap, total answer.

- [ ] **Step 5: Run to verify it passes**

Run: `cargo nextest run -p hornvale-vessel --test submerged`
Expected: PASS, 3 tests.

- [ ] **Step 6: Prove the values did not move**

```bash
cargo nextest run -p hornvale-vessel 2>&1 | tee /tmp/hv-fathom-vessel.txt
make game-check 2>&1 | tee /tmp/hv-fathom-gamecore.txt
```

**`hornvale-game-core` is NOT a workspace member** — `clients/game/core` is
in `Cargo.toml`'s `exclude` list, so `-p hornvale-game-core` fails with
"package not found", not with a test failure. `make game-check` is its gate
(fmt, clippy and `cargo test` against its own manifest). Its committed seed-42
session fixtures are the guard that matters here.

**Branch table:**
- All green → the fix is value-preserving on every shipped world, which is the
  claim. Proceed.
- **A fixture moved → STOP.** That means a shipped world already walks a
  non-`Surface` stratum on a non-water cell, which contradicts spec §4.3 and is
  a finding that outranks this task.

- [ ] **Step 7: The doc lines on the three latent sites**

No behaviour changes here. Each gains one sentence naming the direction it
enforces:

- `Biome::is_marine()` — asks whether a cell's **surface** medium is water. Not
  a question about the column; a cell with an underworld beneath it is not
  marine and never will be, because the underworld is a stratum beneath the
  cell rather than the cell's own biome.
- `provider.rs:703`'s land/marine bucketing — buckets **cells by surface
  medium**. A per-realm census is a companion to this, not a correction of it.
- `provider.rs:1206`'s assertion — quantified over **cell expressions**, which
  stay `OVERWORLD`/`WATERWORLD`. It is not a claim about strata.
- `graph_derive.rs:126`'s `marine` binding — separates **surface** traversal.
  Underworld edges are a later campaign's addition, not a defect in this line.

- [ ] **Step 8: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/vessel/src/vantage.rs windows/vessel/tests/submerged.rs \
        domains/climate/src/biome.rs domains/climate/src/provider.rs \
        windows/worldgen/src/graph_derive.rs
git commit -m "fix(vessel): submerged is a question about the medium, not the rung"
```

---

### Task 4: Close the campaign

**Files:**
- Create: `book/src/chronicle/the-fathom.md`
- Create: `docs/retrospectives/the-fathom.md`
- Modify: `book/src/SUMMARY.md` (chronicle entry)
- Modify: `book/src/frontier/idea-registry.md` (status flips)

- [ ] **Step 1: Absorb main and gate**

```bash
make preflight
make gate 2>&1 | tee /tmp/hv-fathom-gate.txt
```
On an ancestry NO-GO, merge main **into** the branch and re-run the gate here.
Budget ~8 min, and stagger against the other live campaigns — two concurrent
gates on this Mac cost about thirty minutes and both look hung.

- [ ] **Step 2: Prove W-1 — byte-identity**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ \
    docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
git diff --stat docs/audits/
```

**Branch table** (this is W-1's whole content, so read it carefully):
- Nothing moved outside `docs/audits/` → **W-1 holds.** Commit any
  `docs/audits/` drift.
- `docs/audits/` moved → **expected**, this campaign adds `pub` items.
- **Anything else moved → W-1 has FAILED.** The campaign's entire claim is that
  it changed no world bytes. Stop, find what moved and why, and report it
  before touching anything else. Do not rebaseline past it.

- [ ] **Step 3: `make gate-full`**

Run: `make gate-full 2>&1 | tee /tmp/hv-fathom-gatefull.txt`
This is where Task 2's heavy probe actually runs in anger. Expected green.

- [ ] **Step 4: Chronicle and retrospective**

The chronicle entry is book prose — technical, comprehensible without the code
it may show. Its content is the campaign's actual result: the column, the two
spec defects caught by reading the code the spec named, and what H-1 and H-2
measured.

The retrospective is **process lessons, not product**. The two candidates this
campaign already generated, both about plan-time verification:
- the accessor name collision (`biome_expr_at` already existed) — a spec named
  a function without reading its signature;
- the three latent sites — a spec counted four defects where the structure
  gives one, because it reasoned about the symptom class rather than about
  where the underworld actually lives.

- [ ] **Step 5: Flip the registry rows**

`MAP-realm-column` and `MAP-two-realm-assumption` move `spec'd` → `shipped`,
**Where** repointed at the chronicle. Repointing **replaces** a row's prose; it
does not append to it. Keep each Idea cell ≤ 600 characters — the budget test
is in the gate and the waiver list is append-never.

Run: `cargo nextest run -p hornvale --test docs_consistency`

- [ ] **Step 6: Promote the scratch before teardown**

`.superpowers/sdd/` is git-ignored and **dies with the worktree**. Promote
`followups.md`'s eight entries into the retrospective's follow-up section, and
the decision ledger's material entries into the spec's decisions section,
*before* removing the worktree. Never force-add the scratch directory.

- [ ] **Step 7: Commit and hand off**

```bash
cargo fmt
git add book/ docs/retrospectives/the-fathom.md
git commit -m "docs(the-fathom): the chronicle, the retrospective, and the registry flips"
```

Then `superpowers:finishing-a-development-branch`.

---

## Self-Review

**Spec coverage.** §4.1 accessors → Task 1. §4.2 the sea's column → Task 1
(rules) and Task 2 (measurement). §4.3 the live fix and the four doc lines →
Task 3. §5 save-format → Task 1 Step 5 (type-audit) and Task 4 Step 2
(byte-identity); no epoch is claimed anywhere, and no task touches a stream
label, a draw, or the ledger. §6 W-1 → Task 4 Step 2; H-1 and H-2 → Task 2. §7
acceptance → Tasks 1–4, with the `make gate` / `make gate-full` items in Task 4.
§9 DoD's extra clause — the reconstruction test written into campaign 1's spec —
is **not** a task here, because campaign 1's spec does not exist yet; it is
carried in the metaplan §7 and in Task 4's retrospective hand-off.

**Placeholder scan.** One `todo!()` survives, in Task 2 Step 1, and it is
labelled as the single place the plan cannot write the code plus an instruction
to remove it in Step 2. Two tasks say "the exact factoring is yours" (Task 3
Step 2) and "reuse whatever constructor the existing tests use" (Task 1
Step 1) — both deliberate: a plan author does not know which constructor a
crate exposes or how a predicate should be extracted, and prescribing either
from outside the code has produced nulls in this project before. Each names the
*property* required instead.

**Type consistency.** `strata_at(cell) -> Vec<Stratum>` and
`biome_expr_at_stratum(cell, stratum) -> Option<BiomeExpr>` are used with those
exact names and signatures in Tasks 1 and 2. `biome_expr_at(cell) -> BiomeExpr`
is the existing one-argument accessor throughout and is never called with two.
`Realm::strata() -> &'static [Stratum]`, `Realm::UNDERDARK`,
`Realm::WATERWORLD`, `Formation::OpenWater` and `Stratum::{Surface, Abyssal,
Basement}` are all existing items, verified in `domains/climate/src/facets.rs`.
