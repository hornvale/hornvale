//! THE TILTH / THE FALLOW — the deep-history shape probe. Measurement only.
//!
//! Reports, for one seed, what the bake actually produced: the event census and
//! the **stratigraphic column depths** (occupations per site, deepest first).
//! Both campaigns' preregistrations are stated in those numbers — The Fallow's H1
//! is a column depth and its H2 an eviction-cause mix — and neither had an
//! instrument that printed them.
//!
//! ## Why it exists: three claims it overturned
//!
//! The Tilth's session handoff recorded that adopting Lieth had flattened deep
//! history "from 16 stacked steadings to 1". Run against the branch it described,
//! this probe reported a **deepest column of 16**, at cells 29654 and 29659. The
//! handoff was reading the *showcase page*, which points at cell 1400 — a cell
//! that had indeed emptied. The churn had not gone; it had **moved**, and cell
//! 1400 emptied for a specific reason: it was an all-gnoll column, and gnoll is
//! the one settling people Liebig did not rescue (its arid optimum carries no
//! photosynthate-derived food). One cell's story is not the world's.
//!
//! That mattered three ways:
//!
//! 1. **The Fallow's H1 baseline** ("≥ 4 layers, against the 1 that stages 1+4
//!    leave") was stated against the wrong number. At 16, H1 was already satisfied
//!    and could discriminate nothing.
//! 2. **The hard zero was still live.** Those columns' eviction prose still read
//!    *"the cold drove them on"* — because the zero that evicts is
//!    `era.habitable`'s snowline, built in `bake_eras`, which Lieth never touched.
//!    Lieth removed the *base productivity field's* temperature zero, a different
//!    one.
//! 3. **`era.ice` is identically empty** on every production path, so the
//!    handoff's "make `factor` ice-only" would have made it `≡ 1.0`. Measured, that
//!    does not collapse deep history — it *deepens* it (16 → 17) and re-sources it
//!    from conquest (raids 173 → 288, migrations 142 → 97), because every era
//!    becomes identical and climate displacement stops firing at all.
//!
//! All three were plausible readings that one run of this probe refuted. Which is
//! the lesson it exists to keep cheap: *evaluate the function and print the
//! table.*

#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::pins::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::pins::TerrainPins;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{SettlementPins, SkyChoice, census, history_for};
use std::collections::BTreeMap;

/// The seed both campaigns' preregistrations are stated on.
const SEED: u64 = 42;

/// The spec's five probe seeds. One seed cannot tell a shift from the spread:
/// genesis siting is a uniform draw over a shortlist, so any change that moves
/// which cells are drawn re-rolls the whole world rather than nudging it. A
/// single-seed reading of a count like `records_total` is therefore one sample of
/// a wide distribution, and comparing two of them measures noise.
const SEEDS: [u64; 5] = [42, 7, 999_999, 16_244_526_067_196_353_746, 1234];

/// How many of the deepest columns to print — enough to tell one freak cell from
/// a population of deep ones.
const REPORT_COLUMNS: usize = 10;

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn deep_history_shape_at_seed_42() {
    let wc = WorldComponents::assemble().expect("components assemble");
    let h = history_for(
        Seed(SEED),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("seed 42 builds");

    let c = census(&h);
    println!("== event census (seed {SEED}) ==");
    println!("  records_total    {}", c.records_total);
    println!("  alive_at_now     {}", c.alive_at_now);
    println!("  founded / grew   {} / {}", c.founded, c.grew);
    println!(
        "  migrated         {}  (climate eviction + vassal flight)",
        c.migrated
    );
    println!("  collapsed        {}  (starved)", c.collapsed);
    println!("  raided / fled    {} / {}", c.raided, c.fled);
    println!("  resettled        {}", c.resettled);

    // Column depth: occupations per site over the whole span. This is the
    // quantity The Fallow's H1 is stated in, counted off the bake's own records
    // rather than the emitted ledger, so it needs no world file on disk.
    let mut by_cell: BTreeMap<u32, u64> = BTreeMap::new();
    for r in &h.records {
        *by_cell.entry(r.core.site.0).or_default() += 1;
    }
    // Deepest first, ties by ascending cell — a total, deterministic order.
    let mut depths: Vec<(u64, u32)> = by_cell.iter().map(|(cell, n)| (*n, *cell)).collect();
    depths.sort_by(|a, b| b.0.cmp(&a.0).then(a.1.cmp(&b.1)));

    println!("== stratigraphy ==");
    println!("  distinct sites   {}", by_cell.len());
    println!("  deepest columns:");
    for (n, cell) in depths.iter().take(REPORT_COLUMNS) {
        println!("    {n:>3} layers   cell {cell}");
    }
}

/// **Why and when occupations end**, for one seed — the eviction-cause mix and
/// the timeline of endings.
///
/// Two uses. The Fallow's H2 is stated as a cause mix ("ruins attribute to more
/// than one cause, ≥20% anthropogenic"), and nothing printed it. And a world that
/// ends with **zero** surviving settlements — seed 1234 does, both before and
/// after this campaign — is diagnosed here rather than guessed at: a die-off
/// concentrated in one epoch band is a climate event sweeping the map, while one
/// spread evenly is ordinary attrition that never recovered.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn eviction_causes_and_timeline() {
    let wc = WorldComponents::assemble().expect("components assemble");
    for seed in [42, 1234] {
        let h = history_for(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
        )
        .expect("probe seed builds");

        let mut causes: BTreeMap<String, u64> = BTreeMap::new();
        let mut alive = 0u64;
        // Endings bucketed by century of the bake window.
        let mut by_century: BTreeMap<i64, u64> = BTreeMap::new();
        let mut founded_by_century: BTreeMap<i64, u64> = BTreeMap::new();
        for r in &h.records {
            *founded_by_century
                .entry((r.core.founded / 100.0).floor() as i64)
                .or_default() += 1;
            match (r.core.ended, r.core.cause) {
                (Some(end), cause) => {
                    *causes
                        .entry(format!(
                            "{:?}",
                            cause.expect("an ended occupation names a cause")
                        ))
                        .or_default() += 1;
                    *by_century.entry((end / 100.0).floor() as i64).or_default() += 1;
                }
                (None, _) => alive += 1,
            }
        }
        println!(
            "== seed {seed}: {} records, {alive} alive ==",
            h.records.len()
        );
        println!("  causes: {causes:?}");
        println!("  founded by century: {founded_by_century:?}");
        println!("  ended   by century: {by_century:?}");
    }
}

/// The same shape across [`SEEDS`], so a count can be read against its spread.
///
/// This exists because a single-seed comparison misled twice in one session. A
/// `GENESIS_TOP_CELLS` sweep on seed 42 alone produced 433 / 483 / 558 / 281
/// records for pool sizes 8 / 16 / 32 / 64 — a *non-monotonic* curve, which is
/// the signature of re-rolling rather than of a trend. Any per-seed count here
/// has to be read against the min–max band this prints, and an invariant floor
/// (The Tithe's 400 occupations) has to be read against it too.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn deep_history_shape_across_probe_seeds() {
    let wc = WorldComponents::assemble().expect("components assemble");
    let mut records: Vec<u64> = Vec::new();
    let mut sites: Vec<usize> = Vec::new();
    let mut deepest: Vec<u64> = Vec::new();

    println!("  seed                    records  alive  sites  deepest  migr  coll  raids");
    for seed in SEEDS {
        let h = history_for(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
        )
        .expect("probe seed builds");
        let c = census(&h);
        let mut by_cell: BTreeMap<u32, u64> = BTreeMap::new();
        for r in &h.records {
            *by_cell.entry(r.core.site.0).or_default() += 1;
        }
        let deep = by_cell.values().copied().max().unwrap_or(0);
        println!(
            "  {:<22}  {:>7}  {:>5}  {:>5}  {:>7}  {:>4}  {:>4}  {:>5}",
            seed,
            c.records_total,
            c.alive_at_now,
            by_cell.len(),
            deep,
            c.migrated,
            c.collapsed,
            c.raided
        );
        records.push(c.records_total);
        sites.push(by_cell.len());
        deepest.push(deep);
    }

    let band = |v: &[u64]| {
        (
            v.iter().copied().min().unwrap_or(0),
            v.iter().copied().max().unwrap_or(0),
        )
    };
    let (rlo, rhi) = band(&records);
    let (dlo, dhi) = band(&deepest);
    println!("  --");
    println!(
        "  records_total  min {rlo}  max {rhi}  (spread {})",
        rhi - rlo
    );
    println!(
        "  distinct sites min {}  max {}",
        sites.iter().copied().min().unwrap_or(0),
        sites.iter().copied().max().unwrap_or(0)
    );
    println!("  deepest column min {dlo}  max {dhi}");
}
