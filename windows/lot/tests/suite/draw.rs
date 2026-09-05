//! H-M4 (spec §8) and the draw's determinism.
use hornvale_kernel::Vertex;
use hornvale_lot::context::assemble;
use hornvale_lot::draw::{curve, draw, places, uniform};
use hornvale_lot::shape::population_at;
use hornvale_lot::{LotError, LotIndex, Pick};

#[test]
fn uniform_is_pure_and_label_sensitive() {
    assert_eq!(uniform(42, 0, "birth"), uniform(42, 0, "birth"));
    assert_ne!(uniform(42, 0, "birth"), uniform(42, 1, "birth"));
    assert_ne!(uniform(42, 0, "birth"), uniform(42, 0, "site"));
    for i in 0..1000 {
        let u = uniform(42, i, "x");
        assert!((0.0..1.0).contains(&u));
    }
}

/// claim: structural(seed: 42) — one world.
#[test]
fn the_same_index_draws_the_same_life_and_indices_differ() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let a = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let b = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    assert_eq!(a, b);
    let lives: Vec<_> = (0..20)
        .map(|i| draw(&ctx, LotIndex(i), &Pick::default()).unwrap())
        .collect();
    let distinct: std::collections::BTreeSet<(u64, u64)> = lives
        .iter()
        .map(|l| (l.birth_year.to_bits(), l.occupation.0.get()))
        .collect();
    assert!(
        distinct.len() > 10,
        "20 draws gave {} distinct (year, site)",
        distinct.len()
    );
}

/// claim: structural(seed: 42) — one world.
#[test]
fn pins_are_honoured_or_refused_with_the_reason() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let outside = draw(
        &ctx,
        LotIndex(0),
        &Pick {
            year: Some(5000.0),
            site: None,
        },
    );
    assert!(
        matches!(outside, Err(LotError::YearOutsideSpan { .. })),
        "{outside:?}"
    );
    let year = 1500.0;
    let alive = places(&ctx, year);
    assert!(!alive.is_empty());
    let site = alive[0].site;
    let l = draw(
        &ctx,
        LotIndex(3),
        &Pick {
            year: Some(year),
            site: Some(site),
        },
    )
    .unwrap();
    assert_eq!(l.birth_year, year);
    assert_eq!(l.site, site);
    let dead = draw(
        &ctx,
        LotIndex(3),
        &Pick {
            year: Some(year),
            site: Some(Vertex(u32::MAX)),
        },
    );
    assert!(matches!(dead, Err(LotError::SiteNotAliveInYear { .. })));
}

/// claim: structural(seed: 42) — one world.
///
/// `souls_ever` is defined as the binned sum (`Curve`'s own doc): asserted
/// here at zero tolerance, since it IS that sum by construction. The
/// closed-form `Σ births_per_year × person_years` was tried first and
/// measured (seed 42) to disagree by `5.16e-6` relative — over this test's
/// original `1e-6` target, because a piecewise-linear curve sampled at year
/// midpoints integrates exactly to `shape::integral` only over a WHOLE
/// number of years, and most occupations' spans do not land on whole-year
/// boundaries. `Curve::souls_ever`'s doc carries the full account; this test
/// keeps the closed form as a `1e-3`-relative consistency check instead —
/// still two orders of magnitude tighter than a real accounting error would
/// need to clear.
#[test]
fn the_curve_integrates_to_souls_ever_and_births_track_person_years() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let c = curve(&ctx);
    let total: f64 = c.births_by_epoch.iter().sum();
    assert!((total - c.souls_ever).abs() < 1e-6 * c.souls_ever.max(1.0));
    let closed_form: f64 = ctx
        .occupations
        .iter()
        .map(|p| p.births_per_year * p.record.core.person_years)
        .sum();
    assert!(
        (closed_form - c.souls_ever).abs() < 1e-3 * c.souls_ever,
        "closed form {closed_form} vs souls_ever {}: relative {}",
        c.souls_ever,
        (closed_form - c.souls_ever).abs() / c.souls_ever
    );
    // H-P6's consistency check: souls-ever within a factor of 2 of person-years / 30.
    let py: f64 = hornvale_worldgen::occupation_records(&world)
        .iter()
        .map(|o| o.core.person_years)
        .sum();
    let ratio = c.souls_ever / (py / 30.0);
    assert!(
        (0.5..=2.0).contains(&ratio),
        "souls_ever / (person-years/30) = {ratio}"
    );
}

#[test]
fn a_world_without_the_fact_is_refused() {
    let world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
    assert!(matches!(
        assemble(&world),
        Err(LotError::NoOccupations) | Err(LotError::NoPersonYears)
    ));
}

/// claim: structural(seed: 42) — every occupation of the committed world.
/// The Task 4 review's carried assertion, AS FALSIFIED AND CORRECTED here
/// (see the campaign ledger and the task-5 report for the measurement): the
/// review's claim was "the reconstructed curve never exceeds the committed
/// peak", reasoning that a `Triangle`'s `apex < peak` "by construction". Run
/// against every seed-42 occupation, that bound is FALSE for 3 of 1212
/// (0.25%) — all three are `Triangle`s over an exact one-epoch (25-year)
/// tenure founded at `DAUGHTER_POP`, where the committed `person_years`
/// integral (itself bounded by the bake's own accrual invariant,
/// `person_years <= (peak + 0.5) * tenure`) sits close enough to that
/// ceiling that a RISING triangle from `p0` cannot average that high without
/// its endpoint overshooting `peak` — e.g. id 10760661430244475199: peak 31,
/// apex 54.639 (excess 23.639), the campaign's largest. `RisePlateau` peaks
/// at exactly `peak`, and `Rectangle`'s level is the tenure average, so both
/// keep the tight `peak + 0.5` bound with zero measured exceptions; only
/// `Triangle`'s apex can be pushed past it, and never past the analytically
/// derived `2 * (peak + 0.5) - p0` the same accrual invariant guarantees.
#[test]
fn the_reconstructed_curve_never_exceeds_the_committed_peak() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    for p in &ctx.occupations {
        let end = p.record.core.ended.unwrap_or(ctx.present_year);
        let peak = f64::from(p.record.core.peak_population);
        let bound = match p.shape {
            hornvale_lot::shape::Shape::Triangle { p0, .. } => 2.0 * (peak + 0.5) - p0,
            _ => peak + 0.5,
        };
        let mut year = p.record.core.founded.floor();
        while year <= end {
            let pop = population_at(&p.shape, year);
            assert!(
                pop <= bound,
                "occupation {} population_at({year}) = {pop} exceeds bound {bound} (peak {peak}, shape {:?})",
                p.record.id.0,
                p.shape,
            );
            year += 1.0;
        }
    }
}
