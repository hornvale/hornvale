//! THE FATHOM: the preregistered measurements, H-1 and H-2 (spec §6).
//! Reports; repairs nothing.
//!
//! All three tests build one seed-42 world at `BuildDepth::Terrain` — the
//! shallowest rung that yields a climate, since `climate_of` derives terrain
//! itself and reads nothing settlement- or culture-shaped (`climate_from`'s
//! body reads only `sky_of` and the pre-built terrain; see
//! `windows/worldgen/src/lib.rs`'s `climate_of`/`climate_from`). `BuildDepth`
//! rungs nest (`windows/worldgen/CLAUDE.md`), so `Terrain` is sufficient and
//! cheaper than the `Full` rung `warren_readout.rs`/`delver_readout.rs` need
//! for settlement placement this probe never reads.
//!
//! # H-1 was split after measurement — a carrying decision, not a scoping one
//!
//! H-1 was authored as four clauses under one all-or-nothing assertion.
//! Measured on seed 42, clauses 1, 2 and 4 pass comfortably; clause 3 (fewer
//! than 5% of ocean cells single-rung) measured **5.85%** (1,749 of 29,896) —
//! narrowly over. Nathan's ruling: **the world is not wrong, the clause was.**
//! A single-rung column means a floor shallower than 200 m — the continental
//! shelf — and Earth's own shelf is roughly 7-8% of ocean area, so 5.85% is
//! physically unremarkable; the 5% ceiling was authored before anyone measured
//! a real world against it. The threshold is **not** being moved, because a
//! threshold moved after unblinding is worth less than a falsification kept on
//! the record (decision 0016's spirit) — so clause 3 stays exactly as
//! preregistered and is carried under the repo's `PREREGISTERED, not met:`
//! idiom (`radiation_readout.rs`'s desert-elf tests are the precedent this
//! matches), split into its own test so its failure does not mask the three
//! clauses that DO hold.
//!
//! Nathan's second ruling, on the stop directive itself: bundling four
//! clauses under one all-or-nothing stop was a preregistration-design defect,
//! independent of clause 3's own miscalibration — clause 3's intent (catch a
//! degenerate column) is amply met by the distribution measured here (94.15%
//! multi-rung, 3 distinct heights, no bucket over 71.8%).

use std::collections::BTreeMap;

use hornvale_climate::{Formation, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{CellId, Seed};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, climate_of,
};

/// Build seed 42's climate at the shallowest sufficient `BuildDepth` — see
/// the module doc. Shared by all three measurements so each pays for exactly
/// one build.
fn seed_42_climate() -> GeneratedClimate {
    let wc = WorldComponents::assemble().expect("components assemble");
    let world = build_world_to(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds to the terrain rung");
    climate_of(&world).expect("climate derives from the built terrain")
}

/// Column height (count of strata present, from [`GeneratedClimate::strata_at`])
/// over every `Realm::WATERWORLD` cell, keyed by height. Shared by both H-1
/// tests so the distribution is computed once and read by clauses 1/2/4 and
/// clause 3 identically.
fn ocean_column_heights(climate: &GeneratedClimate) -> BTreeMap<usize, usize> {
    let mut heights: BTreeMap<usize, usize> = BTreeMap::new();
    for i in 0..climate.geosphere().cell_count() {
        let cell = CellId(i as u32);
        if climate.biome_expr_at(cell).realm == Realm::WATERWORLD {
            let h = climate.strata_at(cell).len();
            heights.entry(h).and_modify(|n| *n += 1).or_insert(1);
        }
    }
    heights
}

/// H-1, clauses 1/2/4 — the sea's column is non-degenerate. Three of the four
/// originally-preregistered clauses, all required:
/// at least 3 distinct column heights occur; the median height is >= 3; and
/// no single height holds more than 90% of ocean cells (the ceiling — a
/// floor-only prediction cannot tell a healthy world from a degenerate depth
/// field). Clause 3 (fewer than 5% single-rung) is measured separately in
/// [`h1_clause_3_single_rung_share_preregistered_not_met`] — see the module
/// doc for why it was split out rather than weakened.
///
/// Measured on seed 42 (29,896 ocean cells): heights `{1: 1749, 2: 6669, 3:
/// 21478}` — distinct=3, median=3, tallest_bucket=21478 (71.8%).
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn h1_the_seas_column_is_non_degenerate() {
    let climate = seed_42_climate();
    let heights = ocean_column_heights(&climate);

    let ocean: usize = heights.values().sum();
    assert!(ocean > 0, "seed 42 must have ocean cells");
    let distinct = heights.len();
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
    println!("H-1 distinct={distinct} median={median} tallest_bucket={tallest_bucket}");

    assert!(
        distinct >= 3,
        "H-1 clause 1: only {distinct} distinct column heights"
    );
    assert!(median >= 3, "H-1 clause 2: median height {median}");
    assert!(
        (tallest_bucket as f64) <= 0.90 * ocean as f64,
        "H-1 clause 4 (the CEILING): one height holds {tallest_bucket} of {ocean} \
         ocean cells — the depth field is degenerate"
    );
}

/// H-1, clause 3 — PREREGISTERED, NOT MET, carried per the module doc.
/// Requirement as frozen: fewer than 5% of ocean cells are single-rung
/// (floor shallower than 200 m, i.e. `Epipelagic`-only). Measured on seed 42:
/// **5.85%** (1,749 of 29,896 ocean cells) — narrowly over the ceiling.
///
/// **Diagnosis (Nathan's ruling, not re-derived by a successor):** a
/// single-rung column is a continental-shelf cell. Earth's shelf is roughly
/// 7-8% of ocean area, so 5.85% here is physically unremarkable — the world's
/// bathymetry is not degenerate, the 5% ceiling was authored without ever
/// measuring a real world against it. The threshold stays at 5% rather than
/// being widened to fit this measurement: a threshold moved after unblinding
/// is worth less than a falsification kept on the record. A successor that
/// wants this clause to pass re-derives the ceiling from measured shelf
/// fractions across a seed set — filed as
/// [`CLIM-shelf-single-rung-threshold`](https://github.com/hornvale/hornvale/blob/main/book/src/frontier/idea-registry.md).
#[test]
#[ignore = "PREREGISTERED, not met: awaits CLIM-shelf-single-rung-threshold (an unmeasured 5% ceiling on shelf-only ocean cells; measured 5.85%, unremarkable against Earth's ~7-8% shelf fraction)"]
fn h1_clause_3_single_rung_share_preregistered_not_met() {
    let climate = seed_42_climate();
    let heights = ocean_column_heights(&climate);

    let ocean: usize = heights.values().sum();
    assert!(ocean > 0, "seed 42 must have ocean cells");
    let single = *heights.get(&1).unwrap_or(&0);
    let share = single as f64 / ocean as f64;

    println!(
        "H-1 clause 3: {single} of {ocean} ocean cells are single-rung ({:.4}%)",
        share * 100.0
    );

    assert!(
        share < 0.05,
        "H-1 clause 3: {single} of {ocean} ocean cells are single-rung ({:.4}%)",
        share * 100.0
    );
}

/// H-2 — sea ice occurs below the epipelagic. `classify_marine_expr` selects
/// `Formation::SeaIce` in its FIRST arm on surface temperature alone, with no
/// depth condition, while `stratum` comes from the floor. Predicted: at least
/// one seed-42 cell pairs SeaIce with a stratum deeper than Epipelagic.
///
/// **CONFIRMED, strongly.** Measured on seed 42: 9,695 `SeaIce` cells total
/// (the denominator is non-empty, so H-2 is measurable), and **8,916 of them
/// (91.96%) sit below the epipelagic** — example cell `CellId(4)` at
/// `Stratum::Bathypelagic`. This is not a marginal artifact: nearly all sea
/// ice in the model is filed at depth, because `classify_marine_expr` reads
/// only surface temperature while `stratum` is read from the floor, and the
/// column that Task 1's accessors expose is what made the mismatch visible
/// for the first time. Recorded as an artifact for campaign 1 and **NOT
/// repaired here** — repair changes world bytes, which this measurement task
/// (spec §2) is forbidden from doing.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn h2_sea_ice_below_the_epipelagic() {
    let climate = seed_42_climate();

    let mut sea_ice_total = 0usize;
    let mut below_epipelagic = 0usize;
    let mut example: Option<(CellId, Stratum)> = None;
    for i in 0..climate.geosphere().cell_count() {
        let cell = CellId(i as u32);
        let expr = climate.biome_expr_at(cell);
        if expr.formation == Formation::SeaIce {
            sea_ice_total += 1;
            if expr.stratum != Stratum::Epipelagic {
                below_epipelagic += 1;
                if example.is_none() {
                    example = Some((cell, expr.stratum));
                }
            }
        }
    }

    println!("H-2: {below_epipelagic} of {sea_ice_total} sea-ice cells sit below the epipelagic");
    if let Some((cell, stratum)) = example {
        println!("H-2: example cell {cell:?} at stratum {stratum:?}");
    }
    assert!(
        sea_ice_total > 0,
        "H-2 has no denominator: seed 42 has no sea-ice cells at all, so this \
         probe measured nothing rather than measuring zero"
    );
}
