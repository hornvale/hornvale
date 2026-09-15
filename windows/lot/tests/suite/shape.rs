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
    // The Underworld Peoples re-seating introduces one clamped occupation in
    // seed 42; the reconstruction invariant above still holds for it.
    //
    // THE TRENCHER re-pin 1 -> 0 (2026-09-12, the repair pass, ledger
    // #25/#26). The MERGED world -- Task 4's per-metabolite supply change
    // plus the absorbed four peoples -- reseats the records again and seed
    // 42's one clamped occupation is gone, returning this count to the 0 it
    // read before the Underworld Peoples landed (which is also the value
    // this test's module doc still describes as "the ledger's own
    // prediction"). The reconstruction invariant is untouched: all 1,028
    // occupations still reconstruct, and the peak+0.5 bound above still runs
    // on every one of them.
    //
    // **COVERAGE CONSEQUENCE, and it is why this re-pin does not stand
    // alone.** At 0 the `Shape::Rectangle { clamped: true }` arm of the
    // match above never executes, so its `integral <= person_years`
    // assertion measures nothing -- and a sweep of the tree found that arm
    // had NO other coverage anywhere: `clamped` is constructed only in
    // `domains/history/src/trajectory.rs`'s private `rectangle`, and no test
    // in `domains/history` or `windows/lot` ever built one by hand. A live
    // corpus was the whole of it. So the clamped case is now pinned
    // DIRECTLY, in `a_clamped_rectangle_honours_the_peak_and_under_integrates`
    // below, where no world movement can vacate it again; this count stays a
    // descriptive readout of the committed world, which is all its own doc
    // ever claimed it was.
    // CONFIRMED INDEPENDENTLY ON `origin/main` (The Tidemark, merged
    // here): absorbing The Tidemark's six marine peoples on top of the
    // four underworld ones re-places seed 42 again on THAT tree too, and
    // the clamped occupation is gone there as well -- both branches
    // independently measured 0. This merge's own worldgen changes (both
    // sets of peoples plus the species metabolic-triple migration fix)
    // have not been re-measured against together, but 0 was the value on
    // every branch that HAS been measured, and the dedicated hand-built
    // test below covers the clamped arm regardless of what this count
    // reads.
    assert_eq!(
        clamped_count, 0,
        "the ledger's own reading of seed 42 found 0 clamped occupations; if this moved, say so"
    );
}

/// The clamped-`Rectangle` arm of
/// `every_seed_42_occupation_reconstructs_to_its_committed_integral`, pinned
/// on a HAND-BUILT case rather than on whatever the committed world happens
/// to contain (The Trencher, 2026-09-12, ledger #25/#26).
///
/// Seed 42 carried one clamped occupation before this campaign and carries
/// none after, and nothing else in the tree ever built one: `clamped` is set
/// only inside `domains/history::trajectory`'s private `rectangle`, so for
/// the life of that flag its only exercise was a live corpus that a world
/// movement could -- and did -- take away silently. `shape_of` is a pure
/// function of five numbers, so the case needs no world at all.
///
/// The construction: `person_years` deliberately exceeds what the span can
/// hold at the committed peak (100 years x peak 10 admits at most 1,050
/// person-years at the peak+0.5 bound; 5,000 is asked for), which is the
/// real situation `rectangle`'s doc describes -- a record credited more
/// epochs than its reconstructed tenure has room for. `p0` is set to the
/// peak so neither the `RisePlateau` nor the `Triangle` fit can be admitted
/// ahead of the rectangle.
///
/// Both halves of the clamp's contract are asserted, because each can fail
/// without the other: the level HONOURS the peak (`population_at` never
/// exceeds `peak + 0.5`, the guarantee `rectangle` exists to make by
/// construction), and the integral UNDER-states the committed person-years
/// rather than being silently wrong.
#[test]
fn a_clamped_rectangle_honours_the_peak_and_under_integrates() {
    let (founded, end, peak, person_years, p0) = (0.0, 100.0, 10u32, 5_000.0, 10.0);
    let s = shape_of(founded, end, peak, person_years, p0);
    let Shape::Rectangle { clamped, level, .. } = s else {
        panic!("an over-credited record reconstructs as a Rectangle, got {s:?}");
    };
    assert!(
        clamped,
        "the clamp must FIRE here, or this test pins the unclamped arm a          second time and the clamped arm stays uncovered: level {level}"
    );
    let bound = f64::from(peak) + 0.5;
    assert!(
        (level - bound).abs() < 1e-12,
        "a clamped rectangle sits exactly at peak + 0.5, got {level}"
    );
    for year in [0.0, 1.0, 50.0, 99.0, 100.0] {
        assert!(
            population_at(&s, year) <= bound + 1e-9,
            "a clamped rectangle honours the committed peak at year {year}: {}",
            population_at(&s, year)
        );
    }
    assert!(
        integral(&s) < person_years,
        "a clamped rectangle integrates to LESS than the committed \
         person-years -- honestly short, never silently wrong: {} vs {person_years}",
        integral(&s)
    );
}
