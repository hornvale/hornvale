//! ORE SEPARATION — would mines exist, and how many? (The Winze, Task 1)
//!
//! **A measurement that can end the campaign.** Changes no production code.
//! The campaign wants to derive `Function::Mine` from
//! `GeneratedTerrain::prospectivity_at` (currently every occupation is
//! hardcoded to `Function::Agrarian` — `windows/worldgen/src/person_promote.rs`
//! and `windows/worldgen/src/history_bake.rs`). Before writing that
//! derivation, this probe asks the question a derivation can be wrong about:
//! does mineral prospectivity actually pick out a different population of
//! sites than settlements occupy generally, and if so, how many would become
//! mines at a defensible cut? Two preceding mechanisms in this line of work
//! each died at exactly this step — a population someone assumed existed,
//! measured at zero. That is an expected outcome here, not a surprise.
//!
//! Answered in order (a population count from a cut point on a
//! non-separating field is meaningless, so separation must be settled
//! first):
//!
//! 1. **SEPARATION.** Is `prospectivity_at`, read over occupied vertices,
//!    measurably different from the same field read over land generally? If
//!    not, deriving `Mine` from it is arbitrariness dressed as derivation.
//! 2. **QUANTISATION.** Does the top decile of land carry enough distinct
//!    values to support a cut at all, or would any cut point select a tie?
//! 3. **POPULATION.** At a defensible cut, how many occupations would become
//!    mines — per world, and as a share of all occupations?
//!
//! World-building idiom copied from `delver_depth_probe.rs`
//! (`build_world`/`terrain_of`, no `BuildDepth::Full` — that symbol does not
//! exist in this idiom). `occupations_by_vertex` is already keyed by
//! `Vertex`, so this reads `prospectivity_at` at the map's own keys rather
//! than `OccupationRecord::core.site`.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
//!
//! # RE-RUN AGAINST `main` (The Sources, Task 1, 2026-08-26)
//!
//! Harvested from `campaign/the-winze` (unmerged, 403 commits behind at the
//! time of this re-run) and re-measured against `main` at `7576eca00`, after
//! The Glasshouse's temperature re-centring. n=3 seeds (42/7/1234). **A
//! figure moved but the SEPARATION verdict stands**: the top-decile
//! occupation share moved from 0.48% / 2.75% / 11.81% to 0.89% / 1.98% /
//! 8.86% (see the in-body comment at the SEPARATION assertions) — every seed
//! now sits below the ~10% indifferent-siting base rate, the same "ore does
//! not separate settlements" conclusion by a wider margin, not a flip. No
//! metaplan or registry number cites this figure, so nothing outside this
//! file's own doc comment needed updating.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, occupations_by_vertex, terrain_of,
};

/// Seeds the campaign states its preregistrations on.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Nearest-rank percentile of an ascending-sorted slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// claim: readout(off-gate, heavy:, prints only, no assertion beyond the
/// branch landed in) — the `prospectivity_at` distribution over land vs.
/// occupied vertices, for seeds 42 / 7 / 1234. Ruling 2: this is a decision
/// instrument, not a pass/fail gate on a chosen cut point.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn ore_separation_probe() {
    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let world = build_world(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("probe seed builds");
        let terrain = terrain_of(&world).expect("terrain");
        let geo = terrain.geosphere();

        // 1. The `prospectivity_at` distribution over land vertices.
        let mut land: Vec<f64> = geo
            .vertices()
            .filter(|&c| !terrain.is_ocean(c))
            .map(|c| terrain.prospectivity_at(c))
            .collect();
        land.sort_by(f64::total_cmp);

        println!("\n== seed {seed_value} ==  land vertices {}", land.len());
        println!(
            "  [1] land prospectivity:  min {:.4}  p50 {:.4}  p75 {:.4}  p90 {:.4}  p99 {:.4}  max {:.4}",
            land.first().copied().unwrap_or(f64::NAN),
            pct(&land, 0.50),
            pct(&land, 0.75),
            pct(&land, 0.90),
            pct(&land, 0.99),
            land.last().copied().unwrap_or(f64::NAN),
        );

        // 2. Distinct values in the top decile of land.
        let p90 = pct(&land, 0.90);
        let mut top_decile: Vec<f64> = land.iter().copied().filter(|&v| v >= p90).collect();
        top_decile.sort_by(f64::total_cmp);
        let mut distinct_top_decile = top_decile.clone();
        distinct_top_decile.dedup_by(|a, b| a == b);
        println!(
            "  [2] top decile (>= p90 {p90:.4}): {} vertices, {} distinct values",
            top_decile.len(),
            distinct_top_decile.len(),
        );

        // 3. Total occupations, and the `prospectivity_at` distribution
        // *at occupied vertices* — one entry per occupation, not per vertex,
        // since item 4 counts occupations becoming mines.
        let by_vertex = occupations_by_vertex(&world);
        let mut occ_prospectivity: Vec<f64> = Vec::new();
        for (&vertex, occs) in &by_vertex {
            let p = terrain.prospectivity_at(vertex);
            for _ in occs {
                occ_prospectivity.push(p);
            }
        }
        occ_prospectivity.sort_by(f64::total_cmp);
        let total_occ = occ_prospectivity.len();
        println!(
            "  [3] occupations: {total_occ} total;  at-occupied-vertices prospectivity:  min {:.4}  p50 {:.4}  p75 {:.4}  p90 {:.4}  p99 {:.4}  max {:.4}",
            occ_prospectivity.first().copied().unwrap_or(f64::NAN),
            pct(&occ_prospectivity, 0.50),
            pct(&occ_prospectivity, 0.75),
            pct(&occ_prospectivity, 0.90),
            pct(&occ_prospectivity, 0.99),
            occ_prospectivity.last().copied().unwrap_or(f64::NAN),
        );

        // 4. Candidate cut points, drawn from the land distribution's own
        // percentiles (item 1) so a cut point is stated relative to what a
        // world actually produces rather than an arbitrary absolute number:
        // how many occupations would become mines at each?
        let cut_candidates = [
            ("land p50", pct(&land, 0.50)),
            ("land p75", pct(&land, 0.75)),
            ("land p90", pct(&land, 0.90)),
            ("land p95", pct(&land, 0.95)),
            ("land p99", pct(&land, 0.99)),
        ];
        println!("  [4] candidate cut -> occupations at/above cut (would-be mines):");
        for (label, cut) in cut_candidates {
            let count = occ_prospectivity.iter().filter(|&&v| v >= cut).count();
            println!(
                "      {label} ({cut:.4}): {count} / {total_occ}  ({:.2}%)",
                count as f64 / total_occ.max(1) as f64 * 100.0,
            );
        }

        // SEPARATION verdict (branch table, question 1 — answered first per
        // N6). Two independent signals, both already computed above:
        //
        //   (a) the median barely moves: occupied-vertex p50 sits within 0.02
        //       of land p50 on every seed measured (actual gap 0.0000-0.0021,
        //       against a distribution spanning ~0.06-0.74) — the centre of
        //       mass of occupied sites is not shifted toward high
        //       prospectivity at all.
        //   (b) the top decile is not over-represented among occupations: if
        //       siting were indifferent to prospectivity, ~10% of occupations
        //       would fall in land's own top decile by chance; measured share
        //       is 0.89% / 1.98% / 8.86% across the panel (re-measured against
        //       `main` at `7576eca00`, The Sources Task 1, 2026-08-26; the
        //       figure moved from the branch's 0.48% / 2.75% / 11.81%, but the
        //       verdict is unaffected — every seed now sits *below* the ~10%
        //       base rate, which is the same conclusion by a wider margin) —
        //       at or below that base rate on every seed, never above it.
        //
        // Landed branch: "occupied-vertex prospectivity is indistinguishable
        // from land overall" -> ore does not separate settlements. Per the
        // brief and N5/N6, this is where the probe stops: no cut point is
        // treated as a real population count (item 4 above is printed for
        // the record, not adopted), and QUANTISATION/POPULATION are not
        // reached. These two assertions are what make that verdict re-checked
        // by every future run, so the probe reddens rather than staying
        // silently stale if a later change makes ore actually separate
        // settlements.
        let land_p50 = pct(&land, 0.50);
        let occ_p50 = pct(&occ_prospectivity, 0.50);
        assert!(
            (occ_p50 - land_p50).abs() < 0.02,
            "seed {seed_value}: SEPARATION verdict may have changed — occupied-vertex \
             median prospectivity ({occ_p50:.4}) now diverges from land median \
             ({land_p50:.4}) by more than 0.02. This probe's STOP verdict \
             (task-1-report.md) assumed the median does not move; re-derive \
             the branch table before trusting a cut point."
        );
        let land_p90 = pct(&land, 0.90);
        let top_decile_occ_share = occ_prospectivity.iter().filter(|&&v| v >= land_p90).count()
            as f64
            / total_occ.max(1) as f64;
        assert!(
            top_decile_occ_share < 0.20,
            "seed {seed_value}: SEPARATION verdict may have changed — \
             {:.2}% of occupations now fall in land's top decile (>= \
             {land_p90:.4}), well above the ~10% a prospectivity-indifferent \
             siting rule would produce by chance. This probe's STOP verdict \
             assumed no such concentration; re-derive the branch table before \
             trusting a cut point.",
            top_decile_occ_share * 100.0,
        );
    }
}
