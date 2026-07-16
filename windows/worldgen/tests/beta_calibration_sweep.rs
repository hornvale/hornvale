//! β-calibration sweep (task A16b): measures the per-cell-diversity-vs-β
//! curve the controller needs to adjudicate the coexistence-temperature
//! constant `hornvale_demography::BETA` (currently the authored placeholder
//! 4.0; NOT frozen here — this test measures and reports only).
//!
//! For each β in [`BETAS`] and each seed in [`SEEDS`], this builds a world's
//! terrain+climate ONCE (β does not affect the geography, only the
//! competition-share packer that reads it), then packs the coexistence
//! stack — via [`hornvale_demography::report`], the SAME pure function
//! `hornvale_worldgen::demography_report_with_beta` calls internally — at
//! every β without regenerating the world. Three statistics are recorded per
//! (seed, β) and averaged across seeds:
//!
//! 1. **mean-diversity-all** — mean `byproducts.strife` over every habitable
//!    cell (the existing `per-cell-diversity` Lab metric's definition; a
//!    habitable-but-unclaimed cell reads `strife == 0.0`, which drags this
//!    mean toward 0 regardless of how balanced claimed cells are).
//! 2. **mean-diversity-claimed** — mean `byproducts.strife` over habitable
//!    cells that are additionally CLAIMED (Σ density > 0): the cleaner
//!    "when species are present, how many co-occur" signal, undiluted by
//!    unclaimed wilderness.
//! 3. **mean-occupancy** — mean count of species with density >= `FLOOR` per
//!    claimed cell: a direct occupancy count, complementing the
//!    Herfindahl-based diversity statistics above with a simple headcount.
//!
//! This is a MEASUREMENT, not a freeze: it never touches `BETA` and reports
//! only. `#[ignore]`d — a live-worldgen sweep across 13 seeds × 10 β values
//! (multiple minutes even parallelized) — not part of the commit gate or
//! `make gate-full`'s heavy tier; run explicitly with `cargo test --release
//! -p hornvale-worldgen --test beta_calibration_sweep -- --ignored
//! --nocapture`.
//!
//! **A second, PRE-TROPHIC table is also printed** (`measure_pretrophic`):
//! the same three statistics computed from [`hornvale_demography::coexist::
//! cell_share`] directly (the β-driven packer share, converted to density by
//! [`hornvale_demography::home_range`]) WITHOUT running
//! [`hornvale_demography::coexist::couple_trophic`] afterward. This exists
//! because the post-trophic table, measured first, turned out flat across
//! every β for the shipped 4-species goblinoid roster — every non-`bugbear`
//! species carries a positive `ANIMAL_PREY` niche weight (so
//! `hornvale_demography::niche::predation` treats it as an obligate
//! predator) but has no mass-ratio-eligible prey species among the other
//! three (only `bugbear` is heavy enough to have any), so
//! `couple_trophic`'s bottom-up cap — "a predator with no present prey
//! collapses to 0.0 — the dormant-apex resting state" — zeroes three of the
//! four species at EVERY β, unconditionally. The pre-trophic table isolates
//! whether β actually moves the packer's own coexistence share (it does,
//! mildly, for this roster's near-tied carrying capacities) from that
//! downstream, β-independent collapse.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellMap, Mass, ResourceVector, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, carrying_inputs_of,
    climate_of, species_carrying_input, terrain_of,
};
use std::collections::BTreeMap;

/// The competition-temperature values swept, low (broad sharing, "oatmeal")
/// to high (winner-take-all, monoculture) — brackets `hornvale_demography::
/// BETA`'s current placeholder (4.0) on both sides.
const BETAS: [f64; 10] = [0.1, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0];

/// 12 arbitrary low seeds plus the canonical 42, matching this campaign's
/// other small-sample instruments (e.g. `pin_enumeration.rs`'s seed 42, the
/// `census-of-the-gathering` convention of a handful of seeds for a quick
/// read) — a HANDFUL of seeds, never a census (`HV_CENSUS`/`make
/// rebaseline` stay untouched).
const SEEDS: [u64; 13] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 42];

/// One seed's precomputed, β-independent inputs: the geography and per-
/// species carrying-capacity fields the packer reads. Building this ONCE per
/// seed (rather than once per (seed, β) pair) avoids re-running terrain
/// genesis 10x per seed — [`hornvale_worldgen::demography_report_with_beta`]
/// itself reconstructs terrain+climate on every call (the `terrain_of`/
/// `climate_of` "reconstruct, never store" pattern), which is the right
/// design for a single-shot Lab metric but wasteful for a β-sweep that holds
/// geography fixed and varies only β.
struct SeedFixture {
    geo: hornvale_kernel::Geosphere,
    per_species_inputs: Vec<(u32, CellMap<hornvale_demography::CarryingInput>)>,
    species: Vec<(u32, Mass, ResourceVector)>,
    habitable: CellMap<bool>,
    /// Each species' carrying-capacity field, precomputed once (β-free) —
    /// both `measure` (via `hornvale_demography::report`) and
    /// `measure_pretrophic` (hand-rolled, skipping `couple_trophic`) read
    /// off the same field, never recomputing `carrying_capacity` per β.
    per_species_k: Vec<(u32, CellMap<f64>)>,
    /// The guild-overlap matrix, precomputed once — depends only on
    /// `species`' niche vectors, never on β or a cell.
    overlap: BTreeMap<(u32, u32), f64>,
    /// Each species' body mass, keyed by id — `measure_pretrophic`'s
    /// share-to-density conversion reads this via `home_range`.
    masses: BTreeMap<u32, Mass>,
}

/// Build one seed's [`SeedFixture`]: a Terrain-depth world (the shallowest
/// rung that carries the committed terrain/sky pins `terrain_of`/
/// `climate_of` need — settlement/culture/religion facts are irrelevant to
/// demography and skipping them is strictly faster, mirroring
/// `pin_enumeration.rs`'s depth-scoping), then the same per-species
/// carrying-input assembly `hornvale_worldgen::demography_inputs_for` builds
/// internally (that helper is private to the crate; `carrying_inputs_of` and
/// `species_carrying_input` are its public building blocks, reused here
/// verbatim so this fixture is byte-for-byte what the shipped path feeds the
/// packer).
fn build_fixture(seed: u64, wc: &WorldComponents) -> SeedFixture {
    let world = build_world_to(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .expect("seed builds at BuildDepth::Terrain");
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere().clone();
    let base_inputs = carrying_inputs_of(&geo, &terrain, &climate);
    let habitable = climate.habitability().clone();

    // The peopled kinds (the psyche key-set); fauna carry no psyche row. Tags
    // are the shared build-local dense index — both `per_species_inputs` and
    // `species` enumerate the SAME `wc.psyche` order.
    let per_species_inputs: Vec<(u32, CellMap<hornvale_demography::CarryingInput>)> = wc
        .psyche
        .iter()
        .enumerate()
        .map(|(tag, (_kind, psych))| {
            let inputs = CellMap::from_fn(&geo, |cell| {
                species_carrying_input(*base_inputs.get(cell), psych)
            });
            (tag as u32, inputs)
        })
        .collect();
    let species: Vec<(u32, Mass, ResourceVector)> = wc
        .psyche
        .ids()
        .enumerate()
        .map(|(tag, kind)| {
            let bio = wc
                .biosphere
                .get(kind)
                .expect("every peopled kind has a biosphere row");
            (tag as u32, bio.mass, bio.niche.clone())
        })
        .collect();

    let per_species_k: Vec<(u32, CellMap<f64>)> = per_species_inputs
        .iter()
        .map(|(tag, inputs)| (*tag, hornvale_demography::carrying_capacity(&geo, inputs)))
        .collect();
    let projected_niche: Vec<(u32, ResourceVector)> = species
        .iter()
        .map(|(id, _mass, niche)| (*id, niche.clone()))
        .collect();
    let overlap = hornvale_demography::niche::guild_overlap(&projected_niche);
    let masses: BTreeMap<u32, Mass> = species.iter().map(|(id, mass, _)| (*id, *mass)).collect();

    SeedFixture {
        geo,
        per_species_inputs,
        species,
        habitable,
        per_species_k,
        overlap,
        masses,
    }
}

/// The three statistics measured for one (seed, β) pair.
#[derive(Clone, Copy, Debug, Default)]
struct SeedBetaStats {
    /// Mean `strife` over every habitable cell.
    diversity_all: f64,
    /// Mean `strife` over habitable, CLAIMED (Σ density > 0) cells. `None`
    /// (represented as `NaN`, filtered before averaging) if no cell in this
    /// seed's world is claimed at this β.
    diversity_claimed: f64,
    /// Mean count of species with density >= FLOOR, over claimed cells.
    occupancy: f64,
    /// Whether any claimed cell existed (gates `diversity_claimed`/
    /// `occupancy`'s validity).
    has_claimed: bool,
}

/// Measure one (fixture, β) pair: pack the coexistence stack at `beta` (via
/// [`hornvale_demography::report`], the same pure computation
/// `demography_report_with_beta` wraps) and reduce it to [`SeedBetaStats`].
fn measure(fixture: &SeedFixture, beta: f64) -> SeedBetaStats {
    let report = hornvale_demography::report(
        &fixture.geo,
        &fixture.per_species_inputs,
        &fixture.species,
        beta,
        hornvale_demography::FLOOR,
        // Condensation threshold: irrelevant here — only `byproducts`/
        // `stack.density` are read, never `settlements`/`stack_settlements`.
        0.0,
    );

    let (mut sum_all, mut n_all) = (0.0_f64, 0u32);
    let (mut sum_claimed, mut n_claimed) = (0.0_f64, 0u32);
    let (mut sum_occupancy, mut n_occupancy) = (0.0_f64, 0u32);

    for cell in fixture.geo.cells() {
        if !*fixture.habitable.get(cell) {
            continue;
        }
        let strife = *report.byproducts.strife.get(cell);
        sum_all += strife;
        n_all += 1;

        let total_density: f64 = report.stack.density.iter().map(|(_, d)| *d.get(cell)).sum();
        if total_density > 0.0 {
            sum_claimed += strife;
            n_claimed += 1;

            let occupants = report
                .stack
                .density
                .iter()
                .filter(|(_, d)| *d.get(cell) >= hornvale_demography::FLOOR)
                .count();
            sum_occupancy += occupants as f64;
            n_occupancy += 1;
        }
    }

    SeedBetaStats {
        diversity_all: if n_all == 0 {
            0.0
        } else {
            sum_all / f64::from(n_all)
        },
        diversity_claimed: if n_claimed == 0 {
            0.0
        } else {
            sum_claimed / f64::from(n_claimed)
        },
        occupancy: if n_occupancy == 0 {
            0.0
        } else {
            sum_occupancy / f64::from(n_occupancy)
        },
        has_claimed: n_claimed > 0,
    }
}

/// Diagnostic companion to [`measure`]: the SAME three statistics, but
/// computed from the packer's raw per-cell [`hornvale_demography::coexist::
/// cell_share`] output (converted to a per-cell density by
/// [`hornvale_demography::home_range`], exactly as [`hornvale_demography::
/// coexist::pack`] does) WITHOUT then running [`hornvale_demography::
/// coexist::couple_trophic`] — isolating β's effect on the packer alone from
/// the downstream trophic-coupling collapse `measure` is subject to (see the
/// module doc).
fn measure_pretrophic(fixture: &SeedFixture, beta: f64) -> SeedBetaStats {
    let (mut sum_all, mut n_all) = (0.0_f64, 0u32);
    let (mut sum_claimed, mut n_claimed) = (0.0_f64, 0u32);
    let (mut sum_occupancy, mut n_occupancy) = (0.0_f64, 0u32);

    for cell in fixture.geo.cells() {
        if !*fixture.habitable.get(cell) {
            continue;
        }
        n_all += 1;

        let mut present: Vec<(u32, f64)> = fixture
            .per_species_k
            .iter()
            .map(|(id, k)| (*id, *k.get(cell)))
            .filter(|(_, k)| *k > 0.0)
            .collect();
        present.sort_by_key(|(id, _)| *id);
        let capacity: f64 = present.iter().map(|(_, k)| *k).sum();
        let shares = hornvale_demography::coexist::cell_share(
            capacity,
            &present,
            &fixture.overlap,
            beta,
            hornvale_demography::FLOOR,
        );
        let density: BTreeMap<u32, f64> = shares
            .into_iter()
            .map(|(id, share)| {
                let mass = fixture.masses[&id];
                (id, share / hornvale_demography::home_range(mass))
            })
            .collect();

        let total: f64 = density.values().sum();
        if total <= 0.0 {
            // strife contributes 0.0 at an unclaimed cell — nothing to add.
            continue;
        }
        let herfindahl: f64 = density
            .values()
            .map(|d| {
                let frac = d / total;
                frac * frac
            })
            .sum();
        let strife = 1.0 / herfindahl;
        sum_all += strife;

        sum_claimed += strife;
        n_claimed += 1;
        let occupants = density
            .values()
            .filter(|d| **d >= hornvale_demography::FLOOR)
            .count();
        sum_occupancy += occupants as f64;
        n_occupancy += 1;
    }

    SeedBetaStats {
        diversity_all: if n_all == 0 {
            0.0
        } else {
            sum_all / f64::from(n_all)
        },
        diversity_claimed: if n_claimed == 0 {
            0.0
        } else {
            sum_claimed / f64::from(n_claimed)
        },
        occupancy: if n_occupancy == 0 {
            0.0
        } else {
            sum_occupancy / f64::from(n_occupancy)
        },
        has_claimed: n_claimed > 0,
    }
}

/// Sweep `BETAS` against `fixtures` using `measure_fn`, printing a labeled
/// table (run with `--nocapture` to see it) and returning each β's averaged
/// `(beta, mean_all, mean_claimed, mean_occupancy)` row for the caller's
/// sanity check. Shared by both the post-trophic (`measure`) and pre-trophic
/// (`measure_pretrophic`) tables so the averaging logic exists exactly once.
fn run_table(
    label: &str,
    fixtures: &[(u64, SeedFixture)],
    measure_fn: impl Fn(&SeedFixture, f64) -> SeedBetaStats,
) -> Vec<(f64, f64, f64, f64)> {
    println!("\n=== {label} ===");
    println!(
        "{:>6} | {:>18} | {:>21} | {:>13}",
        "beta", "mean-diversity-all", "mean-diversity-claimed", "mean-occupancy"
    );
    println!("{}", "-".repeat(70));

    let mut rows: Vec<(f64, f64, f64, f64)> = Vec::new();
    for &beta in &BETAS {
        let mut all_sum = 0.0_f64;
        let mut claimed_sum = 0.0_f64;
        let mut claimed_n = 0u32;
        let mut occupancy_sum = 0.0_f64;

        for (_, fixture) in fixtures {
            let stats = measure_fn(fixture, beta);
            all_sum += stats.diversity_all;
            if stats.has_claimed {
                claimed_sum += stats.diversity_claimed;
                occupancy_sum += stats.occupancy;
                claimed_n += 1;
            }
        }

        let mean_all = all_sum / fixtures.len() as f64;
        let mean_claimed = if claimed_n == 0 {
            0.0
        } else {
            claimed_sum / f64::from(claimed_n)
        };
        let mean_occupancy = if claimed_n == 0 {
            0.0
        } else {
            occupancy_sum / f64::from(claimed_n)
        };

        println!("{beta:>6.2} | {mean_all:>18.4} | {mean_claimed:>21.4} | {mean_occupancy:>13.4}");
        rows.push((beta, mean_all, mean_claimed, mean_occupancy));
    }
    rows
}

/// The full β-sweep: for every β, build (or reuse) each seed's fixture,
/// measure, and average across seeds — TWO tables ([`run_table`] with
/// [`measure`], then with [`measure_pretrophic`]; see the module doc for
/// why both exist). Prints to stdout (run with `--nocapture` to see it) —
/// this is a REPORT, not an assertion; the controller adjudicates the
/// frozen β from the printed numbers, a later task freezes it.
#[test]
#[ignore = "runs a live β-calibration sweep across 13 seeds x 10 β values (multiple minutes); \
            not part of the commit gate or make gate-full's heavy tier — a one-shot \
            calibration read for task A16b's controller, not a fast-gate battery"]
fn beta_calibration_sweep() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    // Build every seed's β-independent fixture ONCE, up front.
    let fixtures: Vec<(u64, SeedFixture)> = SEEDS
        .iter()
        .map(|&seed| (seed, build_fixture(seed, &wc)))
        .collect();

    let post_trophic = run_table(
        "post-trophic (byproducts.strife off the shipped stack — what the world actually ships)",
        &fixtures,
        measure,
    );
    let pre_trophic = run_table(
        "pre-trophic (packer share only, couple_trophic skipped — isolates β's own effect)",
        &fixtures,
        measure_pretrophic,
    );

    // A sanity floor, not a freeze: every statistic in both tables must be
    // finite and non-negative — a NaN/negative reading would mean the
    // harness itself is broken, not that a β value is "wrong" (there is no
    // wrong β here, only an unadjudicated one).
    for (label, rows) in [
        ("post-trophic", &post_trophic),
        ("pre-trophic", &pre_trophic),
    ] {
        for (beta, all, claimed, occ) in rows {
            assert!(
                all.is_finite() && *all >= 0.0,
                "{label} mean-diversity-all at beta={beta} must be finite and non-negative, got {all}"
            );
            assert!(
                claimed.is_finite() && *claimed >= 0.0,
                "{label} mean-diversity-claimed at beta={beta} must be finite and non-negative, got {claimed}"
            );
            assert!(
                occ.is_finite() && *occ >= 0.0,
                "{label} mean-occupancy at beta={beta} must be finite and non-negative, got {occ}"
            );
        }
    }
}
