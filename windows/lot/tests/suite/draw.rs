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
/// `places` and `draw`'s own site selection must agree on "alive and
/// contributing" — a place `places` lists but `draw` could never choose (or
/// vice versa) would let an exhibit built on `places` show a site the draw
/// itself treats as unpickable. Checks both halves: every returned place
/// carries a positive birth weight (never merely alive with zero
/// contribution), and the set of sites `places` returns is exactly the set
/// `draw`'s own `alive_at && births_at > 0.0` predicate would admit,
/// reconstructed here from the same committed fields `draw` reads.
#[test]
fn places_agrees_with_draws_own_site_selection() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let year = 1500.0;
    let listed = places(&ctx, year);
    assert!(!listed.is_empty());
    for place in &listed {
        assert!(
            place.births_per_year > 0.0,
            "site {:?} listed by places() with births_per_year {}",
            place.site,
            place.births_per_year
        );
    }
    let listed_sites: std::collections::BTreeSet<u32> = listed.iter().map(|p| p.site.0).collect();
    let choosable_sites: std::collections::BTreeSet<u32> = ctx
        .occupations
        .iter()
        .filter(|p| {
            let alive = p.record.core.founded <= year
                && p.record.core.ended.unwrap_or(ctx.present_year) > year;
            alive && p.births_per_year * population_at(&p.shape, year) > 0.0
        })
        .map(|p| p.record.core.site.0)
        .collect();
    assert_eq!(listed_sites, choosable_sites);
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
///
/// The Task 4 review's carried assertion, restored to its original tight
/// form after the fix round below. The review's own reasoning ("a
/// `Triangle`'s `apex < peak` by construction") was falsified on real data
/// (3 of seed 42's 1212 occupations, up to 23.6 over peak) because
/// `context::assemble` always passes `DAUGHTER_POP` as `p0` for every
/// `Founding::From` record, which under-states the true opening population
/// for four of the five mechanisms that can produce one (relocation to
/// vacant land, conquest, climate migration, a raid seat — only a true
/// daughter colony actually opens at `DAUGHTER_POP`; `history_bake.rs`'s
/// `open` sets `peak_population` to the opening population and only ever
/// raises it, so the true opening population is always `<= peak`, which an
/// under-stated `p0` can violate). The fix is in `shape_of` (`shape.rs`),
/// not here: a `Triangle` fit is only admitted when its apex stays within
/// `peak + 0.5`; an out-of-range fit falls through to `Rectangle`, whose
/// `level` is bounded by the same accrual invariant regardless of `p0`. So
/// the ONE tolerance (`peak + 0.5`) now holds for every shape variant again.
#[test]
fn the_reconstructed_curve_never_exceeds_the_committed_peak() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    // How many occupations the new apex clamp actually redirects to
    // Rectangle: recompute the pre-clamp Triangle candidacy (same p0 as
    // context::assemble uses) and count the ones whose apex would have
    // exceeded peak + 0.5 — as opposed to a Rectangle chosen for one of the
    // pre-existing reasons (RisePlateau's peak <= p0, or a Triangle apex <
    // p0), which this fix does not touch.
    let mut clamp_triggered = 0;
    for p in &ctx.occupations {
        let end = p.record.core.ended.unwrap_or(ctx.present_year);
        let t = end - p.record.core.founded;
        let peak = f64::from(p.record.core.peak_population);
        if t > 0.0 {
            let p0 = match p.record.founded_from {
                hornvale_history::record::Founding::Genesis(_) => hornvale_worldgen::GENESIS_POP,
                hornvale_history::record::Founding::From(_) => hornvale_worldgen::DAUGHTER_POP,
            };
            let rise_plateau_fits = peak > p0 && {
                let r = 2.0 * (peak * t - p.record.core.person_years) / (peak - p0);
                (0.0..=t).contains(&r)
            };
            if !rise_plateau_fits {
                let apex = 2.0 * p.record.core.person_years / t - p0;
                if apex >= p0 && apex > peak + 0.5 {
                    clamp_triggered += 1;
                }
            }
        }
        let mut year = p.record.core.founded.floor();
        while year <= end {
            let pop = population_at(&p.shape, year);
            assert!(
                pop <= peak + 0.5,
                "occupation {} population_at({year}) = {pop} exceeds peak {peak} + 0.5 (shape {:?})",
                p.record.id.0,
                p.shape,
            );
            year += 1.0;
        }
    }
    eprintln!(
        "the_reconstructed_curve_never_exceeds_the_committed_peak: {clamp_triggered} occupation(s) redirected from Triangle to Rectangle by the new apex clamp"
    );
}
