//! The Lot's preregistered readout (spec §8): H-P1..H-P6 over the nine
//! seeds' first 200 lots each (indices 0-199, `Pick::default()`), printed
//! for a human to read and never asserted — the same `probe:` discipline
//! `windows/worldgen/tests/suite/lot_probe.rs` (Task 0) uses for a
//! DISTRIBUTION claim, not a threshold one. The six quantities printed here
//! are the same ones `windows/lab/src/metrics.rs`'s six `lot-*` metrics
//! compute (over 1,000 census seeds); this file is the hand-run reading of
//! them over the nine seeds spec §8 names specifically, which a study JSON
//! cannot express (ledger #10: "a study JSON cannot name nine specific
//! seeds").

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_lot::context::assemble;
use hornvale_lot::draw::{Ending, curve, draw};
use hornvale_lot::hazard::HUMAN_ANCHOR_YEARS;
use hornvale_lot::shape::Shape;
use hornvale_lot::slots::{SlotValue, tell};
use hornvale_lot::{LotIndex, Pick};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, build_world};

/// The Living Community's cross-seed sweep (spec §8), the same nine seeds
/// `lot_probe.rs` uses so the rows line up with
/// `book/src/laboratory/generated/the-history/rows.csv`.
const SEEDS: [u64; 9] = [1, 2, 3, 7, 13, 42, 100, 256, 777];

/// Lots drawn per seed: indices 0-199 (spec §8's own count).
const LOTS: u64 = 200;

/// The four story slots spec §4.4 excludes by design — mirrored here from
/// `windows/lab/src/metrics.rs`'s own `LOT_BY_DESIGN_SLOTS` rather than
/// imported: a test binary in one crate cannot reach a private const in
/// another crate's lib, the same reason `solitary_tongue.rs` mirrors
/// `GOBLINOID_DAUGHTERS` instead of importing it.
const BY_DESIGN_SLOTS: [&str; 2] = ["work", "literacy"];

/// The median of `xs` (sorted in place by `total_cmp`). This file always
/// calls it on exactly 200 values, so the empty case never fires.
fn median(xs: &mut [f64]) -> f64 {
    xs.sort_by(f64::total_cmp);
    let n = xs.len();
    if n % 2 == 1 {
        xs[n / 2]
    } else {
        (xs[n / 2 - 1] + xs[n / 2]) / 2.0
    }
}

/// claim: readout(off-gate, prints verdicts, no assertion) - The Lot's H-P1..H-P6
/// over the nine seeds' 200 lots each (spec §8). Decision 0093: a seed loop over
/// nine worlds is a quantified claim, and this one quantifies against spec §8's
/// FROZEN thresholds, printed rather than asserted — a falsified prediction is
/// the finding (spec §8's own closing rule), not a test failure that invites a
/// same-day constant move.
#[test]
#[ignore = "probe: The Lot readout, H-P1..H-P6 over nine seeds; run by hand"]
fn lot_readout() {
    // H-P1/H-P3 split the nine seeds into the eight "growing" ones and seed
    // 100 (spec §8's own split), so the per-seed loop tracks each half
    // separately and the verdict is assembled once the loop is done.
    let mut h_p1_growing_min = f64::INFINITY;
    let mut h_p1_seed_100: Option<f64> = None;
    let mut h_p2_all_in_range = true;
    let mut h_p3_growing_min = f64::INFINITY;
    let mut h_p3_growing_max = f64::NEG_INFINITY;
    let mut h_p3_seed_100: Option<f64> = None;
    let mut h_p4_all_above_half = true;
    let mut h_p5_all_at_least_15 = true;
    let mut h_p6_seed_42: Option<f64> = None;

    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let world = build_world(
            seed,
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("readout seed builds");
        let ctx = assemble(&world).expect("readout seed carries occ-person-years");

        let mut lots = Vec::with_capacity(LOTS as usize);
        for i in 0..LOTS {
            let life = draw(&ctx, LotIndex(i), &Pick::default()).expect("readout draw succeeds");
            let story = tell(&world, &ctx, &life);
            lots.push((life, story));
        }

        let cutoff = ctx.start_year + 0.75 * (ctx.present_year - ctx.start_year);
        let born_late = lots.iter().filter(|(l, _)| l.birth_year >= cutoff).count();
        let born_late_share = born_late as f64 / LOTS as f64;

        let mut scaled: Vec<f64> = lots
            .iter()
            .map(|(l, _)| {
                let lifespan = ctx.occupations[l.occ].lifespan_years;
                l.age_at_death * HUMAN_ANCHOR_YEARS / lifespan
            })
            .collect();
        let median_scaled_age = median(&mut scaled);

        let clamped_rectangles = ctx
            .occupations
            .iter()
            .filter(|p| matches!(p.shape, Shape::Rectangle { clamped: true, .. }))
            .count();

        let witnessing = lots
            .iter()
            .filter(|(l, _)| matches!(l.ending, Ending::CommunityFate(_)) || l.moved_to.is_some())
            .count();
        let witness_share = witnessing as f64 / LOTS as f64;

        let silent_subsistence = lots
            .iter()
            .filter(|(_, s)| {
                s.slot("subsistence")
                    .is_some_and(|slot| matches!(slot.value, SlotValue::Silent(_)))
            })
            .count();
        let silent_subsistence_share = silent_subsistence as f64 / LOTS as f64;

        let total_filled: usize = lots
            .iter()
            .map(|(_, s)| {
                s.slots
                    .iter()
                    .filter(|slot| {
                        !BY_DESIGN_SLOTS.contains(&slot.key)
                            && matches!(slot.value, SlotValue::Filled(_))
                    })
                    .count()
            })
            .sum();
        let mean_filled = total_filled as f64 / LOTS as f64;

        let souls_ever = curve(&ctx).souls_ever;
        let person_years_total: f64 = ctx
            .occupations
            .iter()
            .map(|p| p.record.core.person_years)
            .sum();
        let ratio = souls_ever / (person_years_total / 30.0);

        println!("\n== seed {seed_value} ==");
        println!("  born-last-quarter share          {born_late_share:.3}");
        println!("  median scaled age at death        {median_scaled_age:.2}");
        println!("  witness-community-end share      {witness_share:.3}");
        println!("  silent-subsistence share         {silent_subsistence_share:.3}");
        println!("  mean filled slots (of 23)         {mean_filled:.2}");
        println!(
            "  clamped rectangles (of {})       {clamped_rectangles}",
            ctx.occupations.len()
        );
        println!("  souls_ever / (person-years/30)    {ratio:.3}");

        if seed_value == 100 {
            h_p1_seed_100 = Some(born_late_share);
            h_p3_seed_100 = Some(witness_share);
        } else {
            h_p1_growing_min = h_p1_growing_min.min(born_late_share);
            h_p3_growing_min = h_p3_growing_min.min(witness_share);
            h_p3_growing_max = h_p3_growing_max.max(witness_share);
        }
        if !(10.0..=35.0).contains(&median_scaled_age) {
            h_p2_all_in_range = false;
        }
        if silent_subsistence_share <= 0.5 {
            h_p4_all_above_half = false;
        }
        if mean_filled < 15.0 {
            h_p5_all_at_least_15 = false;
        }
        if seed_value == 42 {
            h_p6_seed_42 = Some(ratio);
        }
    }

    let h_p1_pass =
        h_p1_growing_min >= 0.33 && h_p1_seed_100.is_some_and(|v| (0.20..=0.30).contains(&v));
    let h_p3_pass = h_p3_growing_min >= 0.03
        && h_p3_growing_max <= 0.20
        && h_p3_seed_100.is_some_and(|v| v < 0.03);
    let h_p6_pass = h_p6_seed_42.is_some_and(|v| (0.5..=2.0).contains(&v));

    println!("\n== H-P1..H-P6 verdicts (spec §8) ==");
    println!(
        "H-P1 born-last-quarter share (>=0.33 on the eight growing seeds, in [0.20,0.30] on seed 100): {}",
        if h_p1_pass { "PASS" } else { "FAIL" }
    );
    println!(
        "H-P2 median scaled age at death in [10,35] on every seed: {}",
        if h_p2_all_in_range { "PASS" } else { "FAIL" }
    );
    println!(
        "H-P3 witness-community-end share (in [0.03,0.20] on the eight growing seeds, <0.03 on seed 100): {}",
        if h_p3_pass { "PASS" } else { "FAIL" }
    );
    println!(
        "H-P4 silent-subsistence share > 0.5 on every seed: {}",
        if h_p4_all_above_half { "PASS" } else { "FAIL" }
    );
    println!(
        "H-P5 mean filled slots (of 23) >= 15 on every seed: {}",
        if h_p5_all_at_least_15 { "PASS" } else { "FAIL" }
    );
    println!(
        "H-P6 souls_ever / (person-years / 30) in [0.5,2.0] on seed 42: {}",
        if h_p6_pass { "PASS" } else { "FAIL" }
    );
}
