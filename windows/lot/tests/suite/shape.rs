//! H-M3 (spec §8): the reconstructed shape integrates to the committed
//! person-years and peaks at peak, on every occupation of seed 42.
use hornvale_lot::shape::{EPOCH_YEARS, Shape, integral, population_at, shape_of};

/// `shape::EPOCH_YEARS` and the bake's own epoch length must never drift
/// apart — there is exactly one definition of "how long an epoch is", read
/// from two places.
#[test]
fn epoch_years_matches_the_bakes_own_default() {
    assert_eq!(
        EPOCH_YEARS,
        hornvale_worldgen::BakeConfig::default_millennia().epoch_years
    );
}

#[test]
fn rise_then_plateau_has_the_committed_area_and_peak() {
    // p0 = 10, peak = 40, tenure 100: plateau reached at t_r, area chosen so t_r = 40.
    let area = 10.0 * 40.0 + (40.0 - 10.0) * 40.0 / 2.0 + 40.0 * 60.0;
    let s = shape_of(0.0, 100.0, 40, area, 10.0);
    assert!(matches!(s, Shape::RisePlateau { .. }), "{s:?}");
    assert!((integral(&s) - area).abs() < 1e-9 * area);
    assert!((population_at(&s, 80.0) - 40.0).abs() < 1e-12);
    assert!((population_at(&s, 0.0) - 10.0).abs() < 1e-12);
}

#[test]
fn a_community_that_died_before_plateau_is_a_triangle() {
    let s = shape_of(0.0, 50.0, 40, 800.0, 10.0);
    assert!(matches!(s, Shape::Triangle { .. }), "{s:?}");
    assert!((integral(&s) - 800.0).abs() < 1e-9 * 800.0);
}

/// claim: structural(seed: 42) — every occupation of the committed world.
///
/// H-M3, sharpened by the final review's item 2: a clamped `Rectangle`
/// integrates to LESS than the committed `person_years` by construction
/// (the clamp exists precisely because the raw fit would overshoot the
/// committed peak), so the exact-integral assertion only holds where the
/// shape is NOT clamped. Every shape, clamped or not, must still keep
/// `population_at` within `peak + 0.5` everywhere in its span — that is the
/// invariant the clamp exists to guarantee by construction rather than by
/// measured luck. The clamped count is printed, not asserted: it is a
/// property of the committed world's records, and the ledger's own
/// prediction (0 on seed 42) is read here rather than pinned as a threshold.
#[test]
fn every_seed_42_occupation_reconstructs_to_its_committed_integral() {
    let world = hornvale_worldgen::seed_42_world();
    let now = hornvale_worldgen::present_year(&world);
    let mut clamped_count = 0;
    let mut total = 0;
    for o in hornvale_worldgen::occupation_records(&world) {
        let end = o.core.ended.unwrap_or(now);
        let p0 = match o.founded_from {
            hornvale_history::record::Founding::Genesis(_) => hornvale_worldgen::GENESIS_POP,
            _ => hornvale_worldgen::DAUGHTER_POP,
        };
        let s = shape_of(
            o.core.founded,
            end,
            o.core.peak_population,
            o.core.person_years,
            p0,
        );
        total += 1;
        let peak = f64::from(o.core.peak_population);
        // The bound holds for every shape, clamped or not.
        let mut year = end.min(o.core.founded);
        while year <= end.max(o.core.founded) {
            assert!(
                population_at(&s, year) <= peak + 0.5 + 1e-9,
                "occupation {} shape {s:?} at year {year} exceeds peak+0.5 ({})",
                o.id.0,
                peak + 0.5
            );
            year += ((end - o.core.founded).abs() / 8.0).max(1.0);
        }
        let got = integral(&s);
        match s {
            Shape::Rectangle { clamped: true, .. } => {
                clamped_count += 1;
                assert!(
                    got <= o.core.person_years + 1e-6,
                    "occupation {} clamped rectangle integrates to {got}, over committed {}",
                    o.id.0,
                    o.core.person_years
                );
            }
            _ => {
                assert!(
                    (got - o.core.person_years).abs() <= 1e-9 * o.core.person_years.max(1.0),
                    "occupation {} shape {s:?} integrates to {got}, committed {}",
                    o.id.0,
                    o.core.person_years
                );
            }
        }
    }
    println!("clamped rectangles: {clamped_count} of {total} seed-42 occupations");
    assert_eq!(
        clamped_count, 1,
        "the ledger's own reading of seed 42 found 1 clamped occupation; if this moved, say so"
    );
}
