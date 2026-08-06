//! THE WARREN, Task 4 — measure the blast radius before touching a single
//! pin (plan `docs/superpowers/plans/2026-08-06-the-warren.md`, spec
//! `docs/superpowers/specs/2026-08-06-the-warren-design.md` §5). Task 2
//! wired `per_species_suitability` to ask `HabitatRealm` and gate a
//! `Subterranean` kind on cave availability; the spec deliberately refused
//! to predict the magnitude, so this battery measures it, over `SEEDS`
//! (25, ≥ the plan's "at least 20"), BEFORE Task 5 re-pins anything.
//!
//! Three questions, one seed loop (each seed needs two `BuildDepth::Full`
//! builds — "after" with the real, sparse `habitat_realm_registry`, and
//! "before" with that store emptied, i.e. mutation M1's shape, kept alive
//! here as the pre-campaign world rather than reverted):
//!
//! - **P1 (direction).** Rust-monster's and xorn's mean suitability over
//!   cave-bearing land cells, before vs after. ASSERTED (aggregate, pooled
//!   over every seed): rust-monster up substantially, xorn flat within
//!   noise — the spec's stated prediction.
//! - **P2 (range collapse).** Count of land cells with non-zero suitability
//!   for each kind, before vs after. ASSERTED, per seed: after must never
//!   exceed before. The plan is explicit that a rise here means "the gate is
//!   not working: stop and report that" — so this is a real assertion, not a
//!   report.
//! - **P3 (world movement).** REPORTED, not asserted — matching this
//!   crate's established convention for exploratory confirmation
//!   (`deep_realm_rehome.rs`'s H1 battery is the precedent). Seed 42 already
//!   has a passing committed-fixture check
//!   (`cli::tests::lens_purity::seed_42_world_json_matches_the_committed_fixture`)
//!   showing NO movement; this section checks whether that holds over the
//!   wider seed set and prints whichever way it lands. A surprise here (a
//!   seed whose committed facts move) is a finding to walk through via the
//!   attribution chain the campaign spec names
//!   (`niche -> suitability -> coexistence fit -> shared predator/prey
//!   pressure fields -> every other creature's affect`), not a bug to
//!   silently paper over — so it prints loudly rather than failing quietly
//!   or passing silently.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly to build its own world state, the sanctioned test-fixture
//! posture reused throughout this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_species::{BiosphereTraits, HabitatRealm};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, climate_of,
    per_species_suitability, terrain_of,
};

/// The seed sweep — 25 seeds, above the plan's "at least 20" floor. Kept to
/// `BuildDepth::Full` (not the cheaper `Terrain` rung `deep_realm_rehome.rs`
/// uses) because P3 needs the settlement-placement stage; a single seed's
/// pair of Full builds measured well under a second locally, so 25 seeds x 2
/// builds stays a "minutes" battery, not an hours one.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=25;

/// The `(names, realm slice)` construction `demography_report_with_beta_from`
/// itself does — copied rather than shared (decision 0092), matching
/// `deep_realm_rehome.rs` and `warren_gate.rs`'s own copies.
fn realm_slice(wc: &WorldComponents) -> Vec<HabitatRealm> {
    wc.biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE)
        })
        .collect()
}

/// Per-seed, per-kind P1/P2 accumulator.
#[derive(Default, Clone, Copy)]
struct KindSeedStats {
    /// Sum of "before" (surface-forced) suitability over cave-bearing land
    /// cells, this seed.
    cave_sum_before: f64,
    /// Sum of "after" (live, realm-aware) suitability over the SAME
    /// cave-bearing land cells.
    cave_sum_after: f64,
    /// Count of cave-bearing land cells this seed (the P1 population size).
    cave_n: usize,
    /// Count of ALL land cells with "before" suitability > 0.0 (the P2
    /// population size before the gate).
    land_nonzero_before: usize,
    /// Count of ALL land cells with "after" suitability > 0.0 (P2 after the
    /// gate).
    land_nonzero_after: usize,
    /// Total land cell count this seed, for context in the printed table.
    land_n: usize,
}

/// One seed's full readout: P1/P2 stats for rust-monster and xorn, plus P3's
/// world-movement observation.
struct SeedReadout {
    raw_seed: u64,
    rust_monster: KindSeedStats,
    xorn: KindSeedStats,
    /// `true` if the "before" (empty `habitat_realm` store) and "after"
    /// (real store) builds produced byte-identical committed world JSON.
    world_unchanged: bool,
    fact_count_before: usize,
    fact_count_after: usize,
}

/// Build `seed` twice at `BuildDepth::Full` — once through `wc_after` (the
/// real, sparse `habitat_realm_registry`) and once through `wc_before` (that
/// store emptied, reproducing the pre-campaign world byte-for-byte if
/// nothing downstream of the realm question moved) — and measure P1, P2 and
/// P3 together from the pair.
fn measure_seed(
    seed: Seed,
    wc_after: &WorldComponents,
    wc_before: &WorldComponents,
    bio: &[&BiosphereTraits],
    names: &[&str],
    realm_after: &[HabitatRealm],
    realm_before: &[HabitatRealm],
) -> SeedReadout {
    let world_after = hornvale_worldgen::build_world_from_components(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc_after,
    )
    .unwrap_or_else(|e| panic!("{seed:?} (after) failed to build: {e:?}"));
    let world_before = hornvale_worldgen::build_world_from_components(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc_before,
    )
    .unwrap_or_else(|e| panic!("{seed:?} (before) failed to build: {e:?}"));

    let terrain = terrain_of(&world_after).expect("terrain reconstructs");
    let climate = climate_of(&world_after).expect("climate reconstructs");
    let geo = terrain.geosphere();
    let obliquity_deg = climate.obliquity_deg();
    let insolation_scalar = climate.insolation();
    let regime = climate.regime();

    let k_after = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        bio,
        realm_after,
    );
    let k_before = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        bio,
        realm_before,
    );

    let stats_for = |label: &str| -> KindSeedStats {
        let tag = names
            .iter()
            .position(|n| *n == label)
            .unwrap_or_else(|| panic!("{label} missing from the biosphere roster"))
            as u32;
        let after_map = &k_after.iter().find(|(t, _)| *t == tag).unwrap().1;
        let before_map = &k_before.iter().find(|(t, _)| *t == tag).unwrap().1;

        let mut s = KindSeedStats::default();
        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            s.land_n += 1;
            let a = *after_map.get(cell);
            let b = *before_map.get(cell);
            if a > 0.0 {
                s.land_nonzero_after += 1;
            }
            if b > 0.0 {
                s.land_nonzero_before += 1;
            }
            if terrain.cave_at(cell).is_some() {
                s.cave_n += 1;
                s.cave_sum_after += a;
                s.cave_sum_before += b;
            }
        }
        s
    };

    let rust_monster = stats_for("rust-monster");
    let xorn = stats_for("xorn");

    let json_after = world_after.to_json();
    let json_before = world_before.to_json();
    SeedReadout {
        raw_seed: seed.0,
        rust_monster,
        xorn,
        world_unchanged: json_after == json_before,
        fact_count_before: world_before.ledger.len(),
        fact_count_after: world_after.ledger.len(),
    }
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_blast_radius_readout() {
    let wc_after = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut wc_before = WorldComponents::assemble().expect("canonical registries are well-formed");
    // Mutation M1's shape (Task 2 Step 5), kept alive as the "before" world
    // rather than reverted: an empty habitat_realm store is exactly what
    // shipped before this campaign — every kind defaults to `Surface`.
    wc_before.habitat_realm = hornvale_kernel::ComponentStore::new();

    let bio: Vec<&BiosphereTraits> = wc_after.biosphere.iter().map(|(_, b)| b).collect();
    let names: Vec<&'static str> = wc_after.biosphere.ids().map(|k| k.0).collect();
    let realm_after = realm_slice(&wc_after);
    let realm_before: Vec<HabitatRealm> = vec![HabitatRealm::Surface; bio.len()];

    let mut rows = Vec::new();
    for raw_seed in SEEDS {
        // Build once as a smoke check that terrain generation itself is
        // realm-blind (domains/terrain never sees WorldComponents), so a
        // failure here would mean the "before"/"after" pair is not the
        // controlled comparison this readout assumes.
        let seed = Seed(raw_seed);
        let smoke = build_world_to(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc_after,
            BuildDepth::Terrain,
        );
        assert!(smoke.is_ok(), "{seed:?} must build at Terrain depth");

        rows.push(measure_seed(
            seed,
            &wc_after,
            &wc_before,
            &bio,
            &names,
            &realm_after,
            &realm_before,
        ));
    }

    // --- P1: direction, pooled over every seed --------------------------
    println!("\n=== P1 — direction (mean suitability over cave-bearing land cells) ===");
    println!(
        "{:>6} | {:>12} | {:>12} | {:>12} | {:>12} | {:>8}",
        "seed", "rm_before", "rm_after", "xorn_before", "xorn_after", "cave_n"
    );
    let mut rm_before_total = 0.0;
    let mut rm_after_total = 0.0;
    let mut rm_cave_n_total = 0usize;
    let mut xorn_before_total = 0.0;
    let mut xorn_after_total = 0.0;
    let mut xorn_cave_n_total = 0usize;
    for r in &rows {
        let rm = &r.rust_monster;
        let xo = &r.xorn;
        println!(
            "{:>6} | {:>12.6} | {:>12.6} | {:>12.6} | {:>12.6} | {:>8}",
            r.raw_seed,
            rm.cave_sum_before / rm.cave_n.max(1) as f64,
            rm.cave_sum_after / rm.cave_n.max(1) as f64,
            xo.cave_sum_before / xo.cave_n.max(1) as f64,
            xo.cave_sum_after / xo.cave_n.max(1) as f64,
            rm.cave_n,
        );
        rm_before_total += rm.cave_sum_before;
        rm_after_total += rm.cave_sum_after;
        rm_cave_n_total += rm.cave_n;
        xorn_before_total += xo.cave_sum_before;
        xorn_after_total += xo.cave_sum_after;
        xorn_cave_n_total += xo.cave_n;
    }
    assert!(
        rm_cave_n_total > 0 && xorn_cave_n_total > 0,
        "the seed sweep must contain cave-bearing land cells"
    );
    let rm_mean_before = rm_before_total / rm_cave_n_total as f64;
    let rm_mean_after = rm_after_total / rm_cave_n_total as f64;
    let rm_ratio = rm_mean_after / rm_mean_before;
    let xorn_mean_before = xorn_before_total / xorn_cave_n_total as f64;
    let xorn_mean_after = xorn_after_total / xorn_cave_n_total as f64;
    let xorn_ratio = xorn_mean_after / xorn_mean_before;
    println!(
        "\nrust-monster: pooled mean before={rm_mean_before:.6} after={rm_mean_after:.6} \
         ratio={rm_ratio:.3} over {rm_cave_n_total} cave-bearing land cells across {} seeds",
        rows.len()
    );
    println!(
        "xorn:         pooled mean before={xorn_mean_before:.6} after={xorn_mean_after:.6} \
         ratio={xorn_ratio:.3} over {xorn_cave_n_total} cave-bearing land cells across {} seeds",
        rows.len()
    );
    assert!(
        rm_ratio > 1.3,
        "P1: rust-monster's pooled after/before ratio over cave-bearing land cells must rise \
         substantially (spec §5 P1); got {rm_ratio:.3}"
    );
    assert!(
        (0.75..=1.3).contains(&xorn_ratio),
        "P1: xorn's pooled after/before ratio over cave-bearing land cells must stay flat \
         within noise (spec §5 P1); got {xorn_ratio:.3}"
    );

    // --- P2: range collapse, asserted per seed ---------------------------
    println!("\n=== P2 — range collapse (land cells with non-zero suitability) ===");
    println!(
        "{:>6} | {:>8} | {:>12} | {:>11} | {:>12} | {:>11}",
        "seed", "land_n", "rm_before_nz", "rm_after_nz", "xo_before_nz", "xo_after_nz"
    );
    let mut rm_before_nz_total = 0usize;
    let mut rm_after_nz_total = 0usize;
    let mut xorn_before_nz_total = 0usize;
    let mut xorn_after_nz_total = 0usize;
    let mut land_n_total = 0usize;
    for r in &rows {
        let rm = &r.rust_monster;
        let xo = &r.xorn;
        println!(
            "{:>6} | {:>8} | {:>12} | {:>11} | {:>12} | {:>11}",
            r.raw_seed,
            rm.land_n,
            rm.land_nonzero_before,
            rm.land_nonzero_after,
            xo.land_nonzero_before,
            xo.land_nonzero_after,
        );
        assert!(
            rm.land_nonzero_after <= rm.land_nonzero_before,
            "P2: rust-monster's non-zero land cell count must NOT rise at seed {} \
             (before={}, after={}) — the plan calls a rise here 'the gate is not working'",
            r.raw_seed,
            rm.land_nonzero_before,
            rm.land_nonzero_after
        );
        assert!(
            xo.land_nonzero_after <= xo.land_nonzero_before,
            "P2: xorn's non-zero land cell count must NOT rise at seed {} (before={}, after={})",
            r.raw_seed,
            xo.land_nonzero_before,
            xo.land_nonzero_after
        );
        rm_before_nz_total += rm.land_nonzero_before;
        rm_after_nz_total += rm.land_nonzero_after;
        xorn_before_nz_total += xo.land_nonzero_before;
        xorn_after_nz_total += xo.land_nonzero_after;
        land_n_total += rm.land_n;
    }
    println!(
        "\nrust-monster: {rm_before_nz_total} -> {rm_after_nz_total} non-zero land cells \
         ({:.1}% -> {:.1}% of {land_n_total} land cells) across {} seeds",
        100.0 * rm_before_nz_total as f64 / land_n_total as f64,
        100.0 * rm_after_nz_total as f64 / land_n_total as f64,
        rows.len()
    );
    println!(
        "xorn:         {xorn_before_nz_total} -> {xorn_after_nz_total} non-zero land cells \
         ({:.1}% -> {:.1}% of {land_n_total} land cells) across {} seeds",
        100.0 * xorn_before_nz_total as f64 / land_n_total as f64,
        100.0 * xorn_after_nz_total as f64 / land_n_total as f64,
        rows.len()
    );

    // --- P3: world movement, REPORTED not asserted -----------------------
    println!("\n=== P3 — world movement (before/after committed-JSON identity) — REPORTED ===");
    let mut moved_seeds: Vec<u64> = Vec::new();
    for r in &rows {
        println!(
            "{:>6} | unchanged={:<5} | facts before={} after={}",
            r.raw_seed, r.world_unchanged, r.fact_count_before, r.fact_count_after
        );
        if !r.world_unchanged {
            moved_seeds.push(r.raw_seed);
        }
    }
    if moved_seeds.is_empty() {
        println!(
            "\nP3: world identity did NOT move at any of {} seeds — consistent with \
             cli::tests::lens_purity's passing seed-42 fixture check.",
            rows.len()
        );
    } else {
        println!(
            "\nP3: world identity MOVED at {} of {} seeds: {:?} — a genuine finding, not an \
             assertion failure; walk the attribution chain before naming a cause.",
            moved_seeds.len(),
            rows.len(),
            moved_seeds
        );
    }
}
