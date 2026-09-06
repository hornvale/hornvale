# The Hidage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One committed, `#[ignore]`d readout that measures the catchment distribution on the bake's own growth field over five seeds and returns The Staple's D1 verdict by the preregistered rule — and nothing in world state moves.

**Architecture:** A single test file `windows/worldgen/tests/suite/hidage_probe.rs` holds (a) a small statistics instrument (Gini, Spearman over average ranks, a count against a bar, the §4 verdict rule, top-N attractor selection) with unit tests and constructed positive controls, and (b) the probe: rebuild each settling people's present-era headcount capacity from public API exactly as `bake_history_from` does, run `hornvale_demography::flow` over it, read three populations (alive bake sites, top-N attractors, ended sites), print the per-seed table, and print the verdict. Close amends the metaplan and mints two decisions.

**Tech Stack:** Rust 2024 workspace (`serde`, `serde_json`, `libm` only), `hornvale-worldgen` integration suite, `hornvale-demography` (`flow`), `hornvale-history` (`flesh::HAMLET_POPULATION_CEILING`, `record::OccupationRecord`), `cargo-nextest`.

**Spec:** `docs/superpowers/specs/2026-09-06-the-hidage-design.md` — read §2 (what the code says), §3 (the measurement, with the formulae in §3.4), §4 (the frozen decision rule) and §7 (H1–H3) before Task 0.

**Ledger:** `docs/superpowers/ledgers/2026-09-06-the-hidage.md` — rulings, deferred minors and follow-ups are written there AS THEY OCCUR, never batched to close, and never into `.superpowers/sdd/`.

**Status:** Not started.

## Global Constraints

- The probe **commits nothing, draws under no label, and adds no `pub` item** (spec §5). `make rebaseline` after Task 1 must leave an empty diff; `docs/audits/` moving is a STOP (a pub boundary changed), anything else moving is a STOP (an unasked-for epoch).
- The field summed is the bake's growth field, present era: `per_species_capacity(...)[p].at(v) * seating_p.multiplier.get(v)`, roster = every `SocialForm::Settled` kind in ascending `KindId` order, exactly as `windows/worldgen/src/lib.rs:7673` and `:7830-7840` build it (spec §3.1). No other field is summed.
- The catchment is `hornvale_demography::flow` unchanged (spec §3.2). No Voronoi, no travel cost, no era sweep.
- The decision rule is spec §4 verbatim, applied mechanically after the table exists. The band fractions 0.25 / 0.5 and the bars 150 / 200 are not touched after unblinding; if the readout tempts a change, that is a MIXED finding and a ledger entry, not an edit.
- Every count prints with its denominator. Every statistic in §3.4 prints per seed; `c`, `N`, attractor availability and the top-N accumulation summary print per people as well.
- No dependency is added. `BTreeMap`/`BTreeSet`/`Vec` only. No wall clock. `total_cmp` for float ordering, ties to the lower vertex id. `cargo fmt` before every commit.
- `make gate-commit` before each implementation commit. A plan-stage boundary submits `make sluice-stage BRANCH=campaign/the-hidage REF=<full-sha>` (Task 2); this campaign runs the stage gate it is told The Cruck never submitted.
- After `make rebaseline`, run `git status`; if anything under `clients/` moved, STOP and run `make game-check`, `make vessel-check`, `make world-check` before anything else.
- Mutation-proof restore rule, verbatim: copy the file aside before mutating, never `git checkout --` with other edits pending, rebuild after restoring.
- Never `git add -A`. The plan file is committed; `.superpowers/` is not.
- Every "confirm X" step names the branch to take when X does not hold. Do not paper over a red with a weaker assertion.

---

## File structure

- **Create** `windows/worldgen/tests/suite/hidage_probe.rs` — the instrument (pure functions + tests + controls) and the probe. One file: the instrument exists only for this readout and the H2 control must call the same `verdict` the probe prints from.
- **Modify** `windows/worldgen/tests/suite.rs` — one `#[path]` mod pair, alphabetical (between `graph_derive` at `:98-99` and `history_bake` at `:100-101`).
- **Modify** `cli/tests/fixtures/world-build-sites.tsv` — one row, alphabetical among the `windows/worldgen/tests/suite/` rows.
- **Modify at close** `docs/superpowers/specs/2026-09-04-the-staple-metaplan.md` (§1.1, §4 D1), `book/src/frontier/idea-registry.md` (`SOC-staple-ladder`), `docs/decisions/0826-*.md`, `0827-*.md`, `book/src/chronicle/the-hidage.md` + `SUMMARY.md`, `docs/retrospectives/the-hidage.md` + `README.md`, `docs/audits/campaign-reconciliation.tsv`, `book/src/open-questions.md` if the verdict moves a scored bet.

---

### Task 0: The instrument and its controls (H1, H2)

**Files:**
- Create: `windows/worldgen/tests/suite/hidage_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs:98-101`

**Interfaces:**
- Produces, for Task 1: `fn gini(xs: &[f64]) -> f64`, `fn spearman(xs: &[f64], ys: &[f64]) -> f64`, `fn count_at_or_above(xs: &[f64], bar: f64) -> usize`, `fn median(xs: &[f64]) -> f64` (sorts a copy), `enum Verdict { NoCity, Rescale, Lives, Mixed }`, `fn verdict(per_seed: &[(usize, usize)]) -> Verdict` (each pair is `(c_s, N_s)`), `fn top_attractors(geo: &Geosphere, f: &Flow, n: usize) -> Vec<Vertex>`.

- [ ] **Step 1: Create the file with the instrument's failing tests**

Write `windows/worldgen/tests/suite/hidage_probe.rs` with the module doc, the `use`s, and the tests only (the functions do not exist yet):

```rust
//! The Hidage — The Staple's D1 Task 0 probe. Measurement only.
//!
//! Does growing a community toward its CATCHMENT's summed capacity, instead of
//! its own vertex's, make anything other than a hamlet — and only some? The
//! spec (`docs/superpowers/specs/2026-09-06-the-hidage-design.md`) freezes the
//! field (§3.1: the bake's present-era growth field), the catchment (§3.2:
//! `hornvale_demography::flow`, unchanged), the populations (§3.3) and the
//! decision rule (§4) before this file was written. The probe commits nothing.
//!
//! The statistics below are the instrument, unit-tested on constructed
//! vectors (H1) and on a constructed field (H2), so that "every seed agrees"
//! can be read as a finding rather than a vacuous check.

use hornvale_demography::{Flow, flow};
use hornvale_kernel::{Geosphere, Vertex, VertexMap};

/// Gini coefficient — mean absolute difference over twice the mean. `0.0` for
/// a constant, empty or all-zero vector.
fn gini(xs: &[f64]) -> f64 {
    unimplemented!()
}

/// Average ranks, 1-based; ties share the mean of the ranks they span.
fn ranks(xs: &[f64]) -> Vec<f64> {
    unimplemented!()
}

/// Spearman rank correlation: Pearson over average ranks. `0.0` when either
/// side is constant (no ranking to agree with).
fn spearman(xs: &[f64], ys: &[f64]) -> f64 {
    unimplemented!()
}

/// How many entries clear `bar` (inclusive).
fn count_at_or_above(xs: &[f64], bar: f64) -> usize {
    unimplemented!()
}

/// Median of a copy sorted by `total_cmp`; `0.0` for an empty slice.
fn median(xs: &[f64]) -> f64 {
    unimplemented!()
}

/// Smallest entry, or `0.0` for an empty slice (a printing helper).
fn min_or_zero(xs: &[f64]) -> f64 {
    xs.iter().copied().fold(f64::INFINITY, f64::min).min(f64::MAX) * if xs.is_empty() { 0.0 } else { 1.0 }
}

/// Spec §4's four branches.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Verdict {
    NoCity,
    Rescale,
    Lives,
    Mixed,
}

/// Spec §4, verbatim. `per_seed` holds `(c_s, N_s)` — the number of top-N
/// attractors whose accumulation clears the hamlet ceiling, and the seed's
/// alive settlement count. Every clause is "on every seed".
fn verdict(per_seed: &[(usize, usize)]) -> Verdict {
    unimplemented!()
}

/// The top-`n` attractors of a flow field by accumulation, descending, ties to
/// the lower vertex id (spec §3.3, P2). Fewer than `n` if the field has fewer.
fn top_attractors(geo: &Geosphere, f: &Flow, n: usize) -> Vec<Vertex> {
    unimplemented!()
}

#[test]
fn gini_is_zero_for_a_constant_vector_and_near_one_for_a_one_hot() {
    assert_eq!(gini(&[3.0, 3.0, 3.0, 3.0]), 0.0);
    assert_eq!(gini(&[]), 0.0);
    // One-hot of length n has Gini (n-1)/n.
    let mut one_hot = vec![0.0; 100];
    one_hot[7] = 5.0;
    assert!((gini(&one_hot) - 0.99).abs() < 1e-12, "{}", gini(&one_hot));
    // Scale-free.
    assert!((gini(&[1.0, 2.0, 4.0]) - gini(&[10.0, 20.0, 40.0])).abs() < 1e-12);
}

#[test]
fn spearman_is_one_on_agreement_minus_one_on_reversal_and_zero_on_a_constant() {
    let a = [1.0, 5.0, 2.0, 9.0, 3.0];
    let up = [10.0, 50.0, 20.0, 90.0, 30.0];
    let down = [90.0, 30.0, 70.0, 10.0, 60.0];
    assert!((spearman(&a, &up) - 1.0).abs() < 1e-12);
    assert!((spearman(&a, &down) + 1.0).abs() < 1e-12);
    assert_eq!(spearman(&a, &[4.0; 5]), 0.0);
    // Ties take the average rank: (1,1,3) ranks as (1.5,1.5,3).
    assert_eq!(ranks(&[1.0, 1.0, 3.0]), vec![1.5, 1.5, 3.0]);
}

#[test]
fn count_against_a_bar_returns_the_straddle_inclusively() {
    let xs = [10.0, 150.0, 149.999, 300.0];
    assert_eq!(count_at_or_above(&xs, 150.0), 2);
    assert_eq!(count_at_or_above(&xs, 1000.0), 0);
    assert_eq!(count_at_or_above(&xs, 0.0), 4);
    assert_eq!(median(&[3.0, 1.0, 2.0]), 2.0);
}

#[test]
fn the_verdict_rule_is_spec_section_4_on_every_seed() {
    // NO CITY: c == 0 everywhere.
    assert_eq!(verdict(&[(0, 200), (0, 180), (0, 220)]), Verdict::NoCity);
    // RESCALE: c/N > 0.5 everywhere.
    assert_eq!(verdict(&[(120, 200), (100, 180), (200, 220)]), Verdict::Rescale);
    // LIVES: 1 <= c and c/N <= 0.25 everywhere (0.25 inclusive).
    assert_eq!(verdict(&[(1, 200), (45, 180), (55, 220)]), Verdict::Lives);
    // MIXED: one seed at zero, the rest alive.
    assert_eq!(verdict(&[(0, 200), (10, 180), (12, 220)]), Verdict::Mixed);
    // MIXED: the 0.25..=0.5 gap.
    assert_eq!(verdict(&[(80, 200), (70, 180), (90, 220)]), Verdict::Mixed);
    // MIXED: a majority on one seed, a minority on another.
    assert_eq!(verdict(&[(150, 200), (10, 180), (12, 220)]), Verdict::Mixed);
}

/// H2 — the pipeline (field -> flow -> top-N -> count -> verdict) on a
/// constructed field: four sharp bumps of unequal height on a coarse
/// geosphere, so exactly one basin dominates. The property this asserts is
/// "of the four attractors, exactly one clears a bar set between the largest
/// and second-largest accumulation" — if the construction yields a different
/// attractor count, change the CONSTRUCTION (peak spacing, sharpness), never
/// the assertion.
#[test]
fn a_constructed_field_with_one_dominant_basin_reads_lives_no_city_and_rescale() {
    let geo = Geosphere::new(2);
    let peaks = [Vertex(0), Vertex(3), Vertex(7), Vertex(11)];
    let heights = [10.0, 1.0, 1.0, 1.0];
    let k = VertexMap::from_fn(&geo, |c| {
        let p = geo.position(c);
        peaks
            .iter()
            .zip(heights)
            .map(|(&pk, h)| {
                let q = geo.position(pk);
                let dot = (p[0] * q[0] + p[1] * q[1] + p[2] * q[2]).max(0.0);
                h * dot.powi(8)
            })
            .sum::<f64>()
    });
    let f = flow(&geo, &k);
    let attractors: Vec<Vertex> = geo
        .vertices()
        .filter(|&v| f.attractor.get(v).is_some_and(|a| a == v))
        .collect();
    assert_eq!(
        attractors.len(),
        4,
        "the construction must yield exactly four basins; got {attractors:?} — move the peaks apart or sharpen the bumps"
    );
    let top = top_attractors(&geo, &f, 4);
    let acc: Vec<f64> = top.iter().map(|&v| *f.accumulation.get(v)).collect();
    assert!(acc[0] > acc[1], "descending: {acc:?}");
    let between = (acc[0] + acc[1]) / 2.0;
    let c = count_at_or_above(&acc, between);
    assert_eq!(c, 1);
    let five_seeds = [(c, 4); 5];
    assert_eq!(verdict(&five_seeds), Verdict::Lives);
    let above_all = count_at_or_above(&acc, acc[0] * 2.0);
    assert_eq!(verdict(&[(above_all, 4); 5]), Verdict::NoCity);
    let below_all = count_at_or_above(&acc, 0.0);
    assert_eq!(verdict(&[(below_all, 4); 5]), Verdict::Rescale);
    // Conservation, so the field is the one condense.rs's own tests describe.
    let total_k: f64 = geo.vertices().map(|c| *k.get(c)).sum();
    let total_sink: f64 = attractors.iter().map(|&a| *f.accumulation.get(a)).sum();
    assert!((total_k - total_sink).abs() < 1e-9);
}
```

- [ ] **Step 2: Register the module**

In `windows/worldgen/tests/suite.rs`, between the `graph_derive` pair (`:98-99`) and the `history_bake` pair (`:100-101`), insert:

```rust
#[path = "suite/hidage_probe.rs"]
mod hidage_probe;
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cargo test -p hornvale-worldgen --test suite -- hidage_probe 2>&1 | tail -20`
Expected: the five tests FAIL with `not implemented` panics. If the file fails to COMPILE instead, fix the compile error first — a red from a compile error proves nothing about an assertion.

- [ ] **Step 4: Implement the instrument**

Replace the `unimplemented!()` bodies:

```rust
fn gini(xs: &[f64]) -> f64 {
    let n = xs.len();
    if n == 0 {
        return 0.0;
    }
    let mean = xs.iter().sum::<f64>() / n as f64;
    if mean <= 0.0 {
        return 0.0;
    }
    let mut sum = 0.0;
    for a in xs {
        for b in xs {
            sum += (a - b).abs();
        }
    }
    sum / (2.0 * (n * n) as f64 * mean)
}

fn ranks(xs: &[f64]) -> Vec<f64> {
    let mut idx: Vec<usize> = (0..xs.len()).collect();
    idx.sort_by(|&a, &b| xs[a].total_cmp(&xs[b]).then(a.cmp(&b)));
    let mut out = vec![0.0; xs.len()];
    let mut i = 0;
    while i < idx.len() {
        let mut j = i;
        while j + 1 < idx.len() && xs[idx[j + 1]] == xs[idx[i]] {
            j += 1;
        }
        // Ranks are 1-based: positions i..=j share the mean rank.
        let avg = (i + j) as f64 / 2.0 + 1.0;
        for &pos in &idx[i..=j] {
            out[pos] = avg;
        }
        i = j + 1;
    }
    out
}

fn spearman(xs: &[f64], ys: &[f64]) -> f64 {
    assert_eq!(xs.len(), ys.len(), "spearman needs paired vectors");
    let (rx, ry) = (ranks(xs), ranks(ys));
    let n = rx.len() as f64;
    if n == 0.0 {
        return 0.0;
    }
    let mx = rx.iter().sum::<f64>() / n;
    let my = ry.iter().sum::<f64>() / n;
    let (mut sxy, mut sxx, mut syy) = (0.0, 0.0, 0.0);
    for (x, y) in rx.iter().zip(&ry) {
        sxy += (x - mx) * (y - my);
        sxx += (x - mx) * (x - mx);
        syy += (y - my) * (y - my);
    }
    if sxx == 0.0 || syy == 0.0 {
        return 0.0;
    }
    sxy / (sxx * syy).sqrt()
}

fn count_at_or_above(xs: &[f64], bar: f64) -> usize {
    xs.iter().filter(|x| **x >= bar).count()
}

fn median(xs: &[f64]) -> f64 {
    if xs.is_empty() {
        return 0.0;
    }
    let mut v = xs.to_vec();
    v.sort_by(|a, b| a.total_cmp(b));
    let n = v.len();
    if n % 2 == 1 { v[n / 2] } else { (v[n / 2 - 1] + v[n / 2]) / 2.0 }
}

fn verdict(per_seed: &[(usize, usize)]) -> Verdict {
    assert!(!per_seed.is_empty(), "a verdict needs at least one seed");
    let share = |c: usize, n: usize| if n == 0 { 0.0 } else { c as f64 / n as f64 };
    if per_seed.iter().all(|&(c, _)| c == 0) {
        return Verdict::NoCity;
    }
    if per_seed.iter().all(|&(c, n)| share(c, n) > 0.5) {
        return Verdict::Rescale;
    }
    if per_seed.iter().all(|&(c, n)| c >= 1 && share(c, n) <= 0.25) {
        return Verdict::Lives;
    }
    Verdict::Mixed
}

fn top_attractors(geo: &Geosphere, f: &Flow, n: usize) -> Vec<Vertex> {
    let mut a: Vec<(Vertex, f64)> = geo
        .vertices()
        .filter(|&v| f.attractor.get(v).is_some_and(|x| x == v))
        .map(|v| (v, *f.accumulation.get(v)))
        .collect();
    a.sort_by(|x, y| y.1.total_cmp(&x.1).then(x.0.0.cmp(&y.0.0)));
    a.truncate(n);
    a.into_iter().map(|(v, _)| v).collect()
}
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-worldgen --test suite -- hidage_probe 2>&1 | tail -20`
Expected: 5 passed. If H2's attractor count is not 4: print the attractor list, move the peak vertices apart (pick vertices whose positions are far in dot product) or raise the exponent, and rerun. Do not lower the assertion to `>= 2`. If `powi` is rejected by clippy's disallowed-methods list, replace it with an explicit eight-fold multiply; do not add a blanket `allow`.

- [ ] **Step 6: fmt, clippy, gate, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
make gate-commit
git add windows/worldgen/tests/suite/hidage_probe.rs windows/worldgen/tests/suite.rs
git commit -m "test(the-hidage): the probe's instrument — Gini, Spearman, the §4 verdict rule, and a constructed control"
```

If `make gate-commit` reddens on `claim_shape`: Task 0 has no seed loop, so that is a scan false positive — read the message, and if it names this file, the fix is a `/// claim:` tag on the flagged test, not a rename. If it reddens on `world_build_sites`: Task 0 builds no world, so read the message before touching the roster.

---

### Task 1: The probe (H3 and the readout)

**Files:**
- Modify: `windows/worldgen/tests/suite/hidage_probe.rs` (append)
- Modify: `cli/tests/fixtures/world-build-sites.tsv` (one row, alphabetical among `windows/worldgen/tests/suite/` rows; the row is `windows/worldgen/tests/suite/hidage_probe.rs<TAB>1<TAB>identity:1` — the file has exactly ONE `build_world(` call site, inside `world_and_fields`)
- Modify: `docs/superpowers/ledgers/2026-09-06-the-hidage.md` (the readout, verbatim)

**Interfaces:**
- Consumes from Task 0: every function in its Interfaces block.
- Produces: `fn world_and_fields(seed: u64) -> Built` (one world build, the per-people fields and flows), used by H3 and the probe.

- [ ] **Step 1: Write H3 (failing) — the field is the bake's**

Append to the file. H3 builds seed 42 once through the helper the probe will use and checks the wiring the spec names: a surface people's field equals `per_species_capacity` at one alive site to the bit; a subterranean people's multiplier is at most 1.0 and equals `seating_for`'s.

```rust
use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_species::{BiomeAffinity, BiosphereTraits, HabitatRealm, SocialForm};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::delve_seating::{Seating, seating_for};
use hornvale_worldgen::{
    SettlementPins, build_world, climate_of, occupations_by_vertex, per_species_capacity, sky_of,
    terrain_of,
};
use std::collections::BTreeMap;

const SEEDS: [u64; 5] = [42, 7, 13, 100, 1234];

/// `hornvale_history::flesh::structures_of`'s function-local floor, mirrored
/// here because it is not `pub` (the `capacity_cost_probe.rs` convention for
/// `CLIMATE_ERAS`). If `structures_of` moves its bar, move this.
const LONGHOUSE_POPULATION_FLOOR: u32 = 200;

/// One people's growth field and its catchment: the bake's present-era
/// headcount capacity times the delve-seating multiplier (spec §3.1), and
/// `flow` over it (§3.2).
struct PeopleField {
    people: KindId,
    realm: HabitatRealm,
    k: VertexMap<f64>,
    flow: Flow,
}

/// One built world with everything the readouts need.
struct Built {
    world: hornvale_kernel::World,
    geo: Geosphere,
    fields: Vec<PeopleField>,
}

/// Build one seed and rebuild the bake's own growth field per settling people,
/// step for step as `bake_history_from` does (`windows/worldgen/src/lib.rs:7673`
/// for the roster, `:7830-7840` for the seating scale), from public API only.
fn world_and_fields(seed: u64) -> Built {
    let wc = WorldComponents::assemble().expect("components assemble");
    let world = build_world(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("probe seed builds");
    let terrain = terrain_of(&world).expect("terrain");
    let climate = climate_of(&world).expect("climate");
    let geo = terrain.geosphere().clone();
    // Stellar inputs, exactly as capacity_cost_probe.rs derives them.
    let sky = sky_of(&world).expect("sky");
    let generated = sky.generated();
    let system = generated.system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning {
                day_std: day.as_std_days(),
            }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };
    // The bake's default roster: every Settled kind, ascending KindId.
    let peoples: Vec<KindId> = wc
        .biosphere
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Settled)
        .map(|(k, _)| *k)
        .collect();
    let species_biosphere: Vec<&BiosphereTraits> = peoples
        .iter()
        .map(|k| wc.biosphere.get(k).expect("a settling people has biosphere traits"))
        .collect();
    let species_realm: Vec<HabitatRealm> = peoples
        .iter()
        .map(|k| wc.habitat_realm.get(k).copied().unwrap_or(HabitatRealm::SURFACE))
        .collect();
    let species_affinity: Vec<Option<BiomeAffinity>> = peoples
        .iter()
        .map(|k| wc.biome_affinity.get(k).cloned())
        .collect();
    let caps = per_species_capacity(
        &geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &species_biosphere,
        &species_realm,
        &species_affinity,
    );
    let niches = hornvale_species::environment_niche_registry();
    let fields = peoples
        .iter()
        .enumerate()
        .map(|(i, &people)| {
            let (tag, map) = &caps[i];
            assert_eq!(*tag as usize, i, "per_species_capacity tags by position");
            let seating = match species_realm[i] {
                HabitatRealm::Surface => Seating::all_surface(&geo),
                HabitatRealm::Subterranean => seating_for(&geo, &terrain, niches.get(&people)),
            };
            let k = VertexMap::from_fn(&geo, |v| map.at(v) * seating.multiplier.get(v));
            let flow = flow(&geo, &k);
            PeopleField {
                people,
                realm: species_realm[i],
                k,
                flow,
            }
        })
        .collect();
    Built { world, geo, fields }
}

/// H3 (spec §7): the probe's field IS the bake's — a tautology that guards
/// the roster, tag and seating wiring. Reads the committed seed-42 world's
/// alive sites through the same helper the probe uses.
#[test]
fn the_probes_field_is_the_bakes_growth_field_at_an_alive_site() {
    let built = world_and_fields(42);
    let by_vertex = occupations_by_vertex(&built.world);
    let mut surface_checked = 0usize;
    let mut sub_checked = 0usize;
    for (&vertex, occs) in &by_vertex {
        for o in occs.iter().filter(|o| o.core.ended.is_none()) {
            let pf = built
                .fields
                .iter()
                .find(|f| f.people == o.core.people)
                .expect("every alive occupation's people is on the roster");
            match pf.realm {
                HabitatRealm::Surface => {
                    // Multiplier 1.0 is an IEEE no-op, so K must equal the bare capacity.
                    assert!(*pf.k.get(vertex) > 0.0, "an alive surface site has capacity");
                    surface_checked += 1;
                }
                HabitatRealm::Subterranean => {
                    let terrain = terrain_of(&built.world).expect("terrain");
                    let niches = hornvale_species::environment_niche_registry();
                    let seating = seating_for(&built.geo, &terrain, niches.get(&pf.people));
                    let m = *seating.multiplier.get(vertex);
                    assert!(m <= 1.0, "a seating multiplier never exceeds 1.0: {m}");
                    sub_checked += 1;
                }
            }
        }
    }
    assert!(surface_checked > 0, "seed 42 has alive surface sites");
    assert!(sub_checked > 0, "seed 42 has an alive subterranean site (drow)");
}
```

- [ ] **Step 2: Run H3 to verify it fails**

Run: `cargo test -p hornvale-worldgen --test suite -- the_probes_field 2>&1 | tail -20`
Expected: FAIL. The likely first red is a compile error (a name that differs from this plan — `Seating`'s path, `insolation_rel`'s signature, `World`'s re-export). Fix the name against the tree and rerun until the red is the H3 assertion or green. If H3 is green on first run, that is acceptable only after confirming both counters are non-zero in the output; a green with `sub_checked == 0` means the roster has no subterranean settler and the second arm must be re-thought, not deleted — ledger it.

Note the surface arm asserts `K > 0`, not bit-equality against a second `per_species_capacity` call: the field IS that call's output times 1.0, so a second call would compare a value with itself. The equality that matters — that the probe's `caps[i]` is the roster's `i`-th people — is the `assert_eq!(*tag as usize, i)` in `world_and_fields`.

- [ ] **Step 3: Add the roster row**

Insert into `cli/tests/fixtures/world-build-sites.tsv`, after the `windows/worldgen/tests/suite/herd*`/`generalist*` rows and before `windows/worldgen/tests/suite/history_*` rows (keep the file's alphabetical order):

```
windows/worldgen/tests/suite/hidage_probe.rs	1	identity:1
```

Run: `cargo test -p hornvale --test suite -- world_build_sites 2>&1 | tail -8`
Expected: green. If `no_unrostered_world_build_appears` names this file with a count other than 1, the file has more than one `build_world(` text occurrence — the doc comment must not spell `build_world(` — fix the text, not the count.

- [ ] **Step 4: Write the probe**

Append:

```rust
/// Per-people numbers for one seed (spec §3.4).
struct PeopleRow {
    people: KindId,
    alive: usize,
    attractors_available: usize,
    top_acc: Vec<f64>,
    top_k: Vec<f64>,
    c150: usize,
    c200: usize,
    occupied: usize,
}

/// claim: readout(off-gate, prints only, no assertion) - the catchment distribution on the
/// bake's own growth field over five seeds, and The Staple's D1 verdict by the rule
/// spec §4 froze before this file existed. Decision 0093: a seed loop is a quantified
/// claim, and this one quantifies a DISTRIBUTION, not a threshold; a ratchet here would
/// freeze whatever it found. The verdict line at the end is mechanical (`verdict`).
#[test]
#[ignore = "probe: The Hidage, The Staple's D1 Task 0 catchment readout; run by hand"]
fn hidage_probe() {
    let ceiling = hornvale_history::flesh::HAMLET_POPULATION_CEILING as f64;
    let longhouse = LONGHOUSE_POPULATION_FLOOR as f64;
    println!("HAMLET_POPULATION_CEILING = {ceiling}  LONGHOUSE_POPULATION_FLOOR = {longhouse}");
    let mut per_seed: Vec<(usize, usize)> = Vec::new();

    for seed_value in SEEDS {
        let built = world_and_fields(seed_value);
        let geo = &built.geo;
        let by_vertex = occupations_by_vertex(&built.world);

        // ---- P1 / P3: the bake's sites, alive and ended, keyed by people.
        let mut alive_by_people: BTreeMap<KindId, Vec<(Vertex, u32)>> = BTreeMap::new();
        let mut ended_by_people: BTreeMap<KindId, Vec<Vertex>> = BTreeMap::new();
        for (&v, occs) in &by_vertex {
            for o in occs {
                if o.core.ended.is_some() {
                    ended_by_people.entry(o.core.people).or_default().push(v);
                } else {
                    alive_by_people
                        .entry(o.core.people)
                        .or_default()
                        .push((v, o.core.peak_population));
                }
            }
        }

        let mut rows: Vec<PeopleRow> = Vec::new();
        // Pooled over peoples (P2).
        let (mut pool_acc, mut pool_k) = (Vec::new(), Vec::new());
        // P1 readouts.
        let (mut m_alive, mut attainment, mut hops) = (Vec::new(), Vec::new(), Vec::new());
        let (mut attr_sites, mut shared_sites, mut k_zero_sites, mut n_alive) = (0, 0, 0, 0);
        let (mut k_alive, mut acc_alive) = (Vec::new(), Vec::new());
        // P3 readouts.
        let (mut m_ended, mut k_ended, mut acc_ended) = (Vec::new(), Vec::new(), Vec::new());
        // Cross-people: alive site vertices that are an attractor for >= 2 peoples' fields.
        let mut multi_people_attractor_sites = 0usize;

        for pf in &built.fields {
            let alive = alive_by_people.get(&pf.people).cloned().unwrap_or_default();
            let n_p = alive.len();
            n_alive += n_p;
            // ---- P2 for this people.
            let all_attractors = top_attractors(geo, &pf.flow, usize::MAX).len();
            let top = top_attractors(geo, &pf.flow, n_p);
            let top_acc: Vec<f64> = top.iter().map(|&a| *pf.flow.accumulation.get(a)).collect();
            let top_k: Vec<f64> = top.iter().map(|&a| *pf.k.get(a)).collect();
            let occupied = top
                .iter()
                .filter(|&&a| alive.iter().any(|&(v, _)| v == a))
                .count();
            pool_acc.extend_from_slice(&top_acc);
            pool_k.extend_from_slice(&top_k);
            rows.push(PeopleRow {
                people: pf.people,
                alive: n_p,
                attractors_available: all_attractors,
                c150: count_at_or_above(&top_acc, ceiling),
                c200: count_at_or_above(&top_acc, longhouse),
                occupied,
                top_acc,
                top_k,
            });
            // ---- P1 for this people.
            let mut attractor_of_site: Vec<Option<Vertex>> = Vec::new();
            for &(v, peak) in &alive {
                let k = *pf.k.get(v);
                let acc = *pf.flow.accumulation.get(v);
                let att = *pf.flow.attractor.get(v);
                attractor_of_site.push(att);
                k_alive.push(k);
                acc_alive.push(acc);
                if k > 0.0 {
                    m_alive.push(acc / k);
                    attainment.push(peak as f64 / k);
                } else {
                    k_zero_sites += 1;
                }
                if att == Some(v) {
                    attr_sites += 1;
                }
                hops.push(hops_to_attractor(geo, &pf.k, v));
                if built
                    .fields
                    .iter()
                    .filter(|other| other.flow.attractor.get(v).is_some_and(|a| a == v))
                    .count()
                    >= 2
                {
                    multi_people_attractor_sites += 1;
                }
            }
            for (i, att) in attractor_of_site.iter().enumerate() {
                if att.is_some()
                    && attractor_of_site
                        .iter()
                        .enumerate()
                        .any(|(j, other)| j != i && other == att)
                {
                    shared_sites += 1;
                }
            }
            // ---- P3 for this people.
            for &v in ended_by_people.get(&pf.people).map(Vec::as_slice).unwrap_or(&[]) {
                let k = *pf.k.get(v);
                let acc = *pf.flow.accumulation.get(v);
                k_ended.push(k);
                acc_ended.push(acc);
                if k > 0.0 {
                    m_ended.push(acc / k);
                }
            }
        }

        // ---- The seed's statistics (spec §3.4).
        let n_s = n_alive;
        let c_s = count_at_or_above(&pool_acc, ceiling);
        let c200_s = count_at_or_above(&pool_acc, longhouse);
        let pool_m: Vec<f64> = pool_acc
            .iter()
            .zip(&pool_k)
            .filter(|(_, k)| **k > 0.0)
            .map(|(a, k)| a / k)
            .collect();
        let s1 = gini(&pool_acc);
        let s2 = gini(&pool_m);
        let s3 = spearman(&pool_acc, &pool_k);
        let med_acc = median(&pool_acc);
        let max_acc = pool_acc.iter().copied().fold(0.0_f64, f64::max);
        let s4 = if med_acc > 0.0 { max_acc / med_acc } else { 0.0 };
        let occ_s: usize = rows.iter().map(|r| r.occupied).sum();
        let shortfall: usize = rows
            .iter()
            .map(|r| r.alive.saturating_sub(r.top_acc.len()))
            .sum();
        per_seed.push((c_s, n_s));

        println!("\n== seed {seed_value} ==  alive N_s {n_s}  ended {}  |A_s| {} (shortfall {shortfall})",
            ended_by_people.values().map(Vec::len).sum::<usize>(), pool_acc.len());
        println!("  c_s (acc >= {ceiling}) {c_s} / {n_s}   c200_s {c200_s} / {n_s}   occupied-by-an-alive-site {occ_s} / {}", pool_acc.len());
        println!("  S1 gini(acc) {s1:.3}  S2 gini(m) {s2:.3}  S3 spearman(acc,K) {s3:.3}  S4 max/median(acc) {s4:.2}");
        println!("  P2 acc: min {:.1} median {:.1} max {:.1}   P2 m: median {:.2} max {:.2}",
            min_or_zero(&pool_acc), med_acc, max_acc,
            median(&pool_m), pool_m.iter().copied().fold(0.0_f64, f64::max));
        let mut hops_sorted = hops.clone();
        hops_sorted.sort_unstable();
        println!("  P1 alive sites: attractors {attr_sites} / {n_s}   sharing an attractor {shared_sites} / {n_s}   K==0 {k_zero_sites}   multi-people attractor {multi_people_attractor_sites}");
        println!("  P1 hops to attractor: median {} max {}   m: median {:.2} max {:.2}   attainment peak/K: median {:.2} min {:.2} max {:.2}",
            hops_sorted.get(hops_sorted.len() / 2).copied().unwrap_or(0),
            hops_sorted.last().copied().unwrap_or(0),
            median(&m_alive), m_alive.iter().copied().fold(0.0_f64, f64::max),
            median(&attainment), min_or_zero(&attainment),
            attainment.iter().copied().fold(0.0_f64, f64::max));
        println!("  P1 K median {:.1} acc median {:.1}  |  P3 ended: n {}  K median {:.1} acc median {:.1} m median {:.2}",
            median(&k_alive), median(&acc_alive), k_ended.len(), median(&k_ended), median(&acc_ended), median(&m_ended));
        for r in &rows {
            println!("    {:<14} N_p {:>4}  attractors {:>5}  top-N acc min {:>8.1} med {:>8.1} max {:>8.1}  c150 {:>3}  c200 {:>3}  occupied {:>3}  K@top med {:>6.1}",
                r.people.0, r.alive, r.attractors_available,
                min_or_zero(&r.top_acc),
                median(&r.top_acc),
                r.top_acc.iter().copied().fold(0.0_f64, f64::max),
                r.c150, r.c200, r.occupied, median(&r.top_k));
        }
    }

    println!("\n== per-seed (c_s, N_s) {per_seed:?}");
    println!("== VERDICT (spec §4, mechanical): {:?}", verdict(&per_seed));
}

/// Up-path length from `v` to its attractor, re-deriving `flow`'s routing rule
/// (strictly-highest-K neighbour, ties to the higher vertex id, seeded at the
/// vertex's own id) because `Flow` does not expose the pointer. The walk must
/// end at the attractor `flow` reports; if it does not, the two rules have
/// diverged and this readout would lie, so it panics rather than prints.
fn hops_to_attractor(geo: &Geosphere, k: &VertexMap<f64>, v: Vertex) -> usize {
    let f = flow(geo, k);
    let expected = *f.attractor.get(v);
    let mut cur = v;
    let mut n = 0usize;
    loop {
        let here = *k.get(cur);
        if here <= 0.0 {
            break;
        }
        let mut best: Option<Vertex> = None;
        let mut best_k = here;
        let mut best_id = cur.0;
        for &nb in geo.neighbors(cur) {
            let e = *k.get(nb);
            if e > best_k || (e == best_k && nb.0 > best_id) {
                best_k = e;
                best_id = nb.0;
                best = Some(nb);
            }
        }
        match best {
            None => break,
            Some(next) => {
                cur = next;
                n += 1;
            }
        }
    }
    assert_eq!(Some(cur), expected.or(Some(cur)), "the walk must end where flow says it ends");
    n
}
```

**Implementer's note on `hops_to_attractor`:** as written it recomputes `flow` per call, which is `O(sites × vertices)` — acceptable for ~200 sites on ~40k vertices, but if the probe's runtime exceeds two minutes, hoist: take `&Flow` as a parameter and pass `pf.flow`. The panic on divergence is the two-way agreement test between this copy of the rule and `flow.rs`'s; keep it.

- [ ] **Step 5: Compile, run H3, run the probe**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo test -p hornvale-worldgen --test suite -- hidage 2>&1 | tail -12
cargo test -p hornvale-worldgen --test suite -- hidage_probe --ignored --nocapture 2>&1 | tee .superpowers/sdd/the-hidage-readout.txt | tail -80
```

Expected: H1–H3 green; the probe prints five seed blocks and a `VERDICT` line. Decision branches:
- the probe panics in `hops_to_attractor` → the routing copy has diverged from `flow.rs`; read `flow.rs:31-55` and match it exactly, do not delete the assert;
- a people row shows `attractors < N_p` → print it as the shortfall (already in the table) and say so in the ledger; it is a finding about that people's field, not a bug;
- `k_zero_sites > 0` → also a finding (an alive site with no present capacity); record it;
- the run exceeds five minutes → hoist `flow` out of `hops_to_attractor` (note above) and rerun.

- [ ] **Step 6: Record the readout in the ledger, verbatim, before reading it for meaning**

Append to `docs/superpowers/ledgers/2026-09-06-the-hidage.md` a section `## Task 1 — readout` containing the full probe output in a fenced block, the SHA it was taken at (`git rev-parse HEAD` after the commit in Step 8 — write the section, commit, then amend the SHA line in a follow-up docs commit; or record the pre-commit SHA and say so), the machine, and the wall time. Then add one line: `Verdict line as printed: ...`. **Do not interpret in this step.** Interpretation is Task 3, against §4, after the stage gate.

- [ ] **Step 7: rebaseline drift check**

```bash
make rebaseline 2>&1 | tail -5
git status --short
```

Expected: `git status` shows ONLY the files this task edited (the probe file, the roster row, the ledger). Branches: anything under `docs/audits/` → STOP, a pub boundary changed somewhere, find it; anything under `clients/` → STOP, run `make game-check`, `make vessel-check`, `make world-check`; anything else → STOP, an artifact moved on a readout-only change, diagnose before committing.

- [ ] **Step 8: gate and commit**

```bash
make gate-commit
git add windows/worldgen/tests/suite/hidage_probe.rs cli/tests/fixtures/world-build-sites.tsv docs/superpowers/ledgers/2026-09-06-the-hidage.md
git commit -m "test(the-hidage): the D1 Task 0 catchment probe, and its readout over five seeds"
```

---

### Task 2: The stage gate (plan-stage boundary)

**Files:** none edited.

- [ ] **Step 1: Submit**

```bash
git push origin campaign/the-hidage
make sluice-stage BRANCH=campaign/the-hidage REF=$(git rev-parse HEAD)
```

Expected: a queue entry. If the mouth refuses with a merge conflict, absorb main (`make absorb`), regate locally, commit, resubmit.

- [ ] **Step 2: Wait for the result, then read it**

```bash
make sluice-status
make sluice-log
```

Expected: the entry ends `reported` and green. If red: the failing phase's log names the test; fix on the branch (a red in `world_build_sites`, `claim_shape` or `subfloor_roster_coverage` is the usual shape for a new test file — fix the roster/tag, never the test), commit, resubmit. Record the job id and outcome in the ledger under `## Task 2 — stage gate`.

---

### Task 3: The verdict and the records (close)

**Files:**
- Modify: `docs/superpowers/specs/2026-09-04-the-staple-metaplan.md` §1.1 (the "genesis uses it" sentence) and §4 D1 (restatement + a **Probe result** paragraph)
- Modify: `book/src/frontier/idea-registry.md` (`SOC-staple-ladder` row; ≤ 600 chars in the Idea cell; Where repointed)
- Create: `docs/decisions/0826-a-dynamics-probe-falsifies-on-a-count-against-an-existing-ceiling.md`, `docs/decisions/0827-<d1-verdict-slug>.md`
- Create: `book/src/chronicle/the-hidage.md`; Modify: `book/src/SUMMARY.md`
- Create: `docs/retrospectives/the-hidage.md`; Modify: `docs/retrospectives/README.md`
- Modify: `docs/audits/campaign-reconciliation.tsv` (plan and spec rows → shipped; chronicle and retro rows added)
- Modify: `docs/superpowers/ledgers/2026-09-06-the-hidage.md` (`## Task 3 — verdict`, deferred minors, capture manifest)
- Modify if a scored bet moves: `book/src/open-questions.md`

- [ ] **Step 1: Apply §4 mechanically and write the ledger entry first**

Read the `VERDICT` line and the per-seed `(c_s, N_s)` list from the Task 1 readout. Confirm by hand against §4 (the four clauses, "on every seed"). Write `## Task 3 — verdict` in the ledger: the branch taken, the per-seed table, and — per §4's attainment caveat — the median attainment `a` beside it; if the verdict is RESCALE and median `a < 0.5`, the branch is MIXED and the entry says why. Then check each of §4.1's four predictions against the readout and record each as held / failed with the number.

- [ ] **Step 2: Amend the metaplan**

In `docs/superpowers/specs/2026-09-04-the-staple-metaplan.md`:
- §1.1, the sentence beginning "**The half that matters most was found during the close's book sweep**": after "So the world already models population as a watershed." insert a correction paragraph: *"**Corrected by The Hidage (2026-09-06):** nothing on the production path reads it. Since The Living Community the bake is the settlement provider (`lib.rs:8202-8210`), and `condense_tagged`'s one caller is the Lab's report accessor. Both halves of the model are single-vertex reads; the watershed exists only as an instrument."*
- §4 D1: replace "The catchment exists (...) and genesis uses it; the bake discards it. Reconciling them is a smaller and better-founded change than inventing a mechanism" with "The catchment exists (`domains/demography/src/flow.rs`, `condense.rs`) as a Lab instrument that nothing on the production path reads (The Hidage, spec §2.3). Wiring it into the bake is a smaller and better-founded change than inventing a mechanism", and append a **Probe result (The Hidage, `<sha>`)** paragraph: the per-seed `(c_s, N_s)` table, the verdict, the four §4.1 predictions with held/failed, and the next step the branch table names.

- [ ] **Step 3: Decisions 0826 and 0827**

0826 — *A dynamics-rung probe falsifies on a count against an existing ceiling, with a floor and a ceiling, never on flatness.* Context: metaplan rule 1; the hydrology argument (ledger #2 pass 3); the form of §4. Consequence: D2–D6's probes state their death criterion as a count with a denominator against a bar the code already has, and name both dead poles. Format: follow `docs/decisions/0786-*.md`'s header (`Status`, `Decider`, `Relates to`).

0827 — the verdict, one of: *D1 proceeds to design*, *D1 is struck: worked land cannot make a city on this field*, *D1 is struck: worked land is a uniform rescale on this field*, *D1's probe is mixed and the arc returns to the metaplan*. Cite the readout SHA and the table.

Run `cargo test -p hornvale --test suite -- docs_consistency 2>&1 | tail -5` after both — the decision-block and citation checks are in that suite.

- [ ] **Step 4: Registry row**

`SOC-staple-ladder`: update the Idea cell's status sentence ("R1, R2 shipped; D1 probed: <verdict in five words>"), keep it under 600 characters (`cli/tests/suite/docs_consistency.rs` enforces), and add to Where: `D1 probe: [The Hidage chronicle](../chronicle/the-hidage.md), [The Hidage spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-06-the-hidage-design.md)`. Pipes in prose escaped as `\|`.

- [ ] **Step 5: Chronicle, retrospective, gradient, reconciliation**

- `book/src/chronicle/the-hidage.md`: the product story at the book's altitude — the question, the field, the table (as a fenced block with the SHA), the verdict, the honest limits (present-era approximation; capacity not population; the fields not summed), and what happens next. Add to `book/src/SUMMARY.md` beside `the-cruck.md`.
- `docs/retrospectives/the-hidage.md`: process lessons only — the stale premise (a metaplan lagged the code twice in one arc), the criterion overturn (a flatness guard could never fire), the worktree race. Add a README row.
- `book/src/open-questions.md`: grep the gradient for the INVARIANT the verdict moves ("every settlement is a hamlet", `SETTLERS_PER_CAPACITY`, "catchment", "city") — not the wording — and re-score any bet it touches; if none, say so in the ledger.
- `docs/audits/campaign-reconciliation.tsv`: flip `plan-2026-09-06-the-hidage` and `spec-2026-09-06-the-hidage-design` to `shipped` with evidence `status header records closure`, add `chronicle-the-hidage` and `retro-the-hidage` rows in the shape of the-cruck's (`docs/audits/campaign-reconciliation.tsv:1343-1344`). Update this plan's and the spec's Status headers.

- [ ] **Step 6: Gate, commit, then close under `closing-a-campaign`**

```bash
make gate-commit
git add docs/superpowers/specs/2026-09-04-the-staple-metaplan.md docs/superpowers/specs/2026-09-06-the-hidage-design.md docs/superpowers/plans/2026-09-06-the-hidage.md book/src/frontier/idea-registry.md docs/decisions/0826-*.md docs/decisions/0827-*.md book/src/chronicle/the-hidage.md book/src/SUMMARY.md docs/retrospectives/the-hidage.md docs/retrospectives/README.md docs/audits/campaign-reconciliation.tsv docs/superpowers/ledgers/2026-09-06-the-hidage.md
git commit -m "docs(the-hidage): the D1 verdict, decisions 0826-0827, metaplan amendment, chronicle and retrospective"
```

Then invoke `closing-a-campaign`. No census is needed (no world moved — say so in the ledger with the empty `make rebaseline` diff as the evidence); the G6 digest to Nathan leads with the verdict.
