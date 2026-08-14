//! The event stream's two batteries (The Repose, spec §6.8).
//!
//! # What is proved here, and what is emphatically not
//!
//! The magnitude laws and the recurrence field are put in BY HAND — every
//! constant in `hornvale_worldgen::hazard` is authored, with its reasoning in
//! its own doc comment, and none of them was ever tuned to make anything
//! here pass. So the recovery tests below are `TOOL-analytic-limiting-case`
//! satisfied and nothing more: we know in closed form what must come out, a
//! large sample is drawn, and the fit is compared to the number that went in.
//! **A match proves the IMPLEMENTATION carries the authored value. It proves
//! nothing whatever about the world**, and a chronicle that presented "the
//! magnitudes follow Gutenberg-Richter" as a finding would be reporting its
//! own input back to itself. What a mismatch means is the useful half: it has
//! exactly one interpretation — the draw does not realize the law — because
//! no other quantity in the comparison is free.
//!
//! These tests cannot catch the authored law being a poor model of real
//! seismicity. That is not a question this campaign asks.
//!
//! # The property battery is a different thing
//!
//! [`a_sub_window_query_returns_exactly_the_enclosing_windows_events`] is not
//! a recovery check. It holds the structural claim that makes "narrated
//! backwards, never forward-simulated" real: the event sequence at a cell is
//! a property of the world, not of the query, so the window is a filter and
//! never a key.
//!
//! # Why nothing here is `#[ignore]`d
//!
//! Task 6's brief specified the two recovery batteries as heavy-tier, with
//! the canonical `heavy: live-worldgen battery (minutes); …` reason. They are
//! not minutes. All four tests in this file, run serially on `ambrose`, cost
//! **0.86 s** together (`cargo test -p hornvale-worldgen --test repose_laws
//! -- --ignored --nocapture --test-threads=1`, 2026-08-14) — the samples are
//! large but an event is two draws off a splitmix stream, and the level-5
//! globe each test builds dominates. Tagging them `heavy:` would have put a
//! false cost claim into a string a guard checks verbatim, and would have
//! removed the only check on the magnitude laws from the commit gate to save
//! under a second. If a later campaign grows these samples into real minutes,
//! the `heavy:` token is the move then; it is not the move now.

use hornvale_kernel::{CellId, Geosphere, Seed, WorldTime, Years, math};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::hazard::{
    B_VALUE, HazardEventKind, M_MAX, M_MIN, VEI_B, VEI_MAX, VEI_MIN, event_block_length, events_in,
    hazard_at,
};

/// The mesh level every test here builds at. Level 5 is the cheapest globe
/// that carries both a wide unrest range and edifices, and none of these
/// tests asserts anything about a cone's *extent* (the property that forced
/// level 6 on `volcano.rs`'s identity tests) — they read one cell at a time.
const LEVEL: u32 = 5;

fn globe_of(seed: Seed) -> (Geosphere, GeneratedTerrain) {
    let geo = Geosphere::new(LEVEL);
    let outcome =
        hornvale_terrain::generate(seed, &geo, &TerrainPins::default()).expect("default pins");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    (geo, terrain)
}

/// The cell with the shortest seismic interval on the globe — the busiest
/// ground there is, and the cheapest place to draw a large sample.
fn busiest_cell(geo: &Geosphere, terrain: &GeneratedTerrain) -> CellId {
    geo.cells()
        .min_by(|a, b| {
            hazard_at(terrain, *a)
                .seismic
                .get()
                .total_cmp(&hazard_at(terrain, *b).seismic.get())
        })
        .expect("a non-empty globe")
}

/// The edifice cell with the shortest eruption interval.
fn busiest_edifice(geo: &Geosphere, terrain: &GeneratedTerrain) -> CellId {
    geo.cells()
        .filter(|c| hazard_at(terrain, *c).volcanic.is_some())
        .min_by(|a, b| volcanic_years(terrain, *a).total_cmp(&volcanic_years(terrain, *b)))
        .expect("an edifice on the test globe")
}

fn volcanic_years(terrain: &GeneratedTerrain, cell: CellId) -> f64 {
    hazard_at(terrain, cell)
        .volcanic
        .expect("filtered to edifice cells")
        .get()
}

/// A window starting at genesis and long enough to expect `events` events at
/// the given mean interval.
fn window_for(events: f64, interval: Years) -> (WorldTime, WorldTime) {
    (
        WorldTime::GENESIS,
        WorldTime::new(events * interval.days()).expect("a finite span"),
    )
}

fn mean(values: &[f64]) -> f64 {
    values.iter().sum::<f64>() / values.len() as f64
}

/// The maximum-likelihood `b` of a Gutenberg-Richter sample:
/// `b = log10(e) / (mean(m) - m_min)`.
///
/// Stated for the UNTRUNCATED law, and applied to a sample drawn from the
/// truncated one. The bias that introduces is computed in
/// [`drawn_magnitudes_recover_the_authored_gutenberg_richter_b_value`]'s doc
/// and is four orders of magnitude below the tolerance, so it is named rather
/// than corrected for.
fn gutenberg_richter_b(magnitudes: &[f64]) -> f64 {
    math::log10(std::f64::consts::E) / (mean(magnitudes) - M_MIN)
}

/// The closed-form mean of the authored truncated-exponential magnitude law
/// on `[min, max]` with decade-decay `b`: `1/beta - L*e^{-beta L}/(1 - e^{-beta L})`
/// above `min`, where `beta = b * ln(10)` and `L = max - min`.
fn authored_mean(min: f64, max: f64, b: f64) -> f64 {
    let beta = b * std::f64::consts::LN_10;
    let l = max - min;
    let tail = math::exp(-beta * l);
    min + 1.0 / beta - l * tail / (1.0 - tail)
}

fn magnitudes_of(
    seed: Seed,
    terrain: &GeneratedTerrain,
    cell: CellId,
    window: (WorldTime, WorldTime),
    kind: HazardEventKind,
) -> Vec<f64> {
    events_in(seed, terrain, cell, window)
        .into_iter()
        .filter(|e| e.kind == kind)
        .map(|e| e.magnitude)
        .collect()
}

/// **The structural property**: the window is a filter, never a key.
///
/// A query for a sub-window returns exactly the events of the enclosing
/// window that fall inside it — same days, same kinds, same magnitudes, same
/// order. Held over four seeds and, on each, over both the busiest ground and
/// an edifice cell (so both processes are exercised), against six fixed
/// nested windows — one of them opening before genesis — **and** against
/// windows cut at days taken from the draw itself.
///
/// Direction: this is red the moment anything about the draw depends on the
/// bounds of the request — keying the stream on the window, drawing a
/// magnitude only for the events that survive the filter, or seeding a walk
/// at the window's start instead of at the fixed block lattice. It is not a
/// determinism test (two identical calls agreeing is a weaker claim, held in
/// `hazard.rs`'s own module tests); it is the claim that two DIFFERENT
/// questions get consistent answers.
///
/// **The derived cuts are not decoration.** With the six fixed windows alone
/// this test was GREEN against a filter-then-draw mutation: a round-numbered
/// boundary almost never lands strictly between two events of one block, and
/// that is the only configuration in which the two designs differ. The cuts
/// taken from the draw construct that configuration on purpose, and
/// `interior_cuts` asserts enough of them occurred.
///
/// claim: invariant(forall-seed) — a fixed, small seed set standing in for a
/// structural property of the derivation, in the shape
/// `volcano.rs::an_edifices_source_is_itself_an_edifice` already uses here.
/// Not a rate and not a reachability claim, so not a census candidate.
#[test]
fn a_sub_window_query_returns_exactly_the_enclosing_windows_events() {
    // Days, chosen so the sub-windows cut inside blocks (a 1,000-year block
    // is 365,250 days) as well as on them, and so one window opens before
    // genesis.
    let outer = (
        WorldTime::new(-500_000.0).expect("finite"),
        WorldTime::new(4_000_000.0).expect("finite"),
    );
    let inner: [(f64, f64); 6] = [
        (-500_000.0, 4_000_000.0),
        (-500_000.0, 0.0),
        (0.0, 365_250.0),
        (1_234.5, 987_654.5),
        (365_250.0, 730_500.0),
        (3_999_999.0, 4_000_000.0),
    ];
    let block_days = event_block_length().days();
    let block_of = |day: f64| (day / block_days).floor() as i64;
    let mut checked = 0_u32;
    let mut interior_cuts = 0_u32;
    for seed in [42, 43, 44, 45] {
        let (geo, terrain) = globe_of(Seed(seed));
        for cell in [
            busiest_cell(&geo, &terrain),
            busiest_edifice(&geo, &terrain),
        ] {
            let all = events_in(Seed(seed), &terrain, cell, outer);
            assert!(
                !all.is_empty(),
                "seed {seed}: {cell:?} produced no events at all over the outer window"
            );
            // Cuts taken FROM the draw, at an event that shares its block
            // with an earlier one. Only such a cut leaves a block partially
            // requested, which is the configuration that separates "draw the
            // block, then filter" from "filter, then draw": under the latter
            // the skipped events' magnitude draws are never consumed, so
            // everything after them in that block shifts. A fixed list of
            // round-numbered windows hits this by luck or not at all.
            let cuts: Vec<f64> = all
                .windows(2)
                .filter(|p| block_of(p[0].day.day()) == block_of(p[1].day.day()))
                .map(|p| p[1].day.day())
                .take(4)
                .collect();
            interior_cuts += cuts.len() as u32;
            let derived: Vec<(f64, f64)> = cuts
                .iter()
                .flat_map(|cut| [(*cut, outer.1.day()), (outer.0.day(), *cut)])
                .collect();
            for (lo, hi) in inner.into_iter().chain(derived) {
                let sub = (
                    WorldTime::new(lo).expect("finite"),
                    WorldTime::new(hi).expect("finite"),
                );
                let expected: Vec<_> = all
                    .iter()
                    .copied()
                    .filter(|e| e.day.day() >= lo && e.day.day() < hi)
                    .collect();
                let got = events_in(Seed(seed), &terrain, cell, sub);
                assert_eq!(
                    got, expected,
                    "seed {seed}, {cell:?}, window [{lo}, {hi}): the sub-query is not the \
                     enclosing window's events filtered — the window is acting as a key"
                );
                checked += u32::from(!expected.is_empty());
            }
        }
    }
    // Non-vacuity, in the two ways this test can be hollow. An all-empty
    // comparison would pass the loop above without ever comparing an event;
    // and a run with no block-interior cut would pass it without ever asking
    // two questions that disagree about a single block, which is the case
    // that separates the shipped draw from a filter-then-draw one.
    assert!(
        checked >= 8,
        "only {checked} non-empty sub-window comparisons — the property was barely exercised"
    );
    assert!(
        interior_cuts >= 8,
        "only {interior_cuts} cuts landed inside a block with events on both sides — the \
         discriminating case was barely exercised"
    );
}

/// §6.8's implementation check for seismicity. Draws ~200,000 events at the
/// busiest cell on a seed-42 globe, fits `b` by maximum likelihood, and
/// compares it to the authored [`B_VALUE`].
///
/// **The law was authored; recovering it says the draw works, not that the
/// world is like this.** See the module doc.
///
/// Tolerance arithmetic, so the number is not arbitrary: the MLE's relative
/// standard error is `1/sqrt(N)`, which at N = 200,000 is 0.22%, so 0.02
/// absolute on `b = 1.0` is a nine-sigma band. The sample is drawn from the
/// TRUNCATED law while the estimator is stated for the untruncated one; that
/// bias is `L*e^{-beta L}/(1 - e^{-beta L})` = 1.8e-5 magnitude units at
/// `L = 4.5`, i.e. about 4e-5 in `b` — four orders of magnitude inside the
/// tolerance, and it is named here rather than corrected for.
///
/// Fitted at the quiet end as well as the busy end, because the magnitude law
/// must be independent of the rate: the same `b` comes out of ground that
/// acts every thirty years and ground that acts every twenty millennia.
#[test]
fn drawn_magnitudes_recover_the_authored_gutenberg_richter_b_value() {
    let (geo, terrain) = globe_of(Seed(42));
    let quietest = geo
        .cells()
        .max_by(|a, b| {
            hazard_at(&terrain, *a)
                .seismic
                .get()
                .total_cmp(&hazard_at(&terrain, *b).seismic.get())
        })
        .expect("a non-empty globe");
    for (label, cell, target) in [
        ("busiest", busiest_cell(&geo, &terrain), 200_000.0),
        ("quietest", quietest, 50_000.0),
    ] {
        let interval = hazard_at(&terrain, cell).seismic;
        let window = window_for(target, interval);
        let magnitudes = magnitudes_of(Seed(42), &terrain, cell, window, HazardEventKind::Seismic);
        let fitted = gutenberg_richter_b(&magnitudes);
        println!(
            "{label} cell {cell:?}: interval {:.1} y, {} events, b_fit = {fitted:.5} \
             (authored B_VALUE = {B_VALUE})",
            interval.get(),
            magnitudes.len()
        );
        assert!(
            magnitudes.len() as f64 > 0.9 * target,
            "{label}: only {} events — the sample is too small for the stated tolerance",
            magnitudes.len()
        );
        assert!(
            (fitted - B_VALUE).abs() < 0.02,
            "{label}: fitted b = {fitted} against the authored {B_VALUE}"
        );
        assert!(
            magnitudes.iter().all(|m| (M_MIN..=M_MAX).contains(m)),
            "{label}: a magnitude escaped the authored bracket"
        );
    }
}

/// §6.8's implementation check for the inter-event times: given the rate, the
/// process is Poisson, so the mean interval between consecutive events
/// recovers the authored recurrence.
///
/// **The recurrence was authored** (`hazard.rs`'s four interval constants) —
/// this recovers the field's own value through the event draw, and is a check
/// on the plumbing between them, not a measurement of anything.
///
/// Tolerance: the relative standard error of a mean of N exponential
/// intervals is `1/sqrt(N)`, which at N = 200,000 is 0.22%; 2% is a
/// nine-sigma band. Held at three cells spanning the field's range, because a
/// draw that ignored the rate entirely would still pass at one.
#[test]
fn inter_event_times_recover_the_authored_recurrence() {
    let (geo, terrain) = globe_of(Seed(42));
    let mut by_interval: Vec<CellId> = geo.cells().collect();
    by_interval.sort_by(|a, b| {
        hazard_at(&terrain, *a)
            .seismic
            .get()
            .total_cmp(&hazard_at(&terrain, *b).seismic.get())
    });
    let middle = by_interval[by_interval.len() / 2];
    for (label, cell, target) in [
        ("busiest", by_interval[0], 200_000.0),
        ("median", middle, 100_000.0),
        ("quietest", by_interval[by_interval.len() - 1], 50_000.0),
    ] {
        let interval = hazard_at(&terrain, cell).seismic;
        let window = window_for(target, interval);
        let days: Vec<f64> = events_in(Seed(42), &terrain, cell, window)
            .into_iter()
            .filter(|e| e.kind == HazardEventKind::Seismic)
            .map(|e| e.day.day())
            .collect();
        assert!(
            days.len() as f64 > 0.9 * target,
            "{label}: only {} events — the sample is too small",
            days.len()
        );
        let gaps: Vec<f64> = days.windows(2).map(|w| w[1] - w[0]).collect();
        let fitted = Years::from_days(mean(&gaps)).expect("a positive mean gap");
        println!(
            "{label} cell {cell:?}: {} events, mean interval {:.3} y \
             (authored recurrence {:.3} y)",
            days.len(),
            fitted.get(),
            interval.get()
        );
        assert!(
            (fitted.get() - interval.get()).abs() / interval.get() < 0.02,
            "{label}: mean interval {} y against the authored {} y",
            fitted.get(),
            interval.get()
        );
    }
}

/// §6.8's implementation check for the eruption-size law: the drawn VEI-shaped
/// magnitudes recover the authored truncated-exponential mean.
///
/// **Authored, not measured.** [`VEI_B`] encodes "each step up the scale is
/// about five times rarer"; this asserts the draw realizes that, and says
/// nothing about volcanism.
///
/// The comparison is against the closed-form mean of the truncated law rather
/// than against `1/beta`, because truncating at [`VEI_MAX`] shifts the mean
/// by a computable amount and comparing to the untruncated mean would be
/// asserting the wrong number. Tolerance 0.02 on a distribution with standard
/// deviation about 0.62 and N = 50,000 gives a standard error of 0.0028 —
/// a seven-sigma band.
#[test]
fn drawn_eruption_sizes_recover_the_authored_vei_law() {
    let (geo, terrain) = globe_of(Seed(42));
    let cell = busiest_edifice(&geo, &terrain);
    let interval = hazard_at(&terrain, cell)
        .volcanic
        .expect("the busiest edifice erupts");
    let window = window_for(50_000.0, interval);
    let sizes = magnitudes_of(Seed(42), &terrain, cell, window, HazardEventKind::Eruption);
    let expected = authored_mean(VEI_MIN, VEI_MAX, VEI_B);
    let fitted = mean(&sizes);
    println!(
        "edifice {cell:?}: interval {:.1} y, {} eruptions, mean VEI = {fitted:.5} \
         (authored law's closed-form mean = {expected:.5})",
        interval.get(),
        sizes.len()
    );
    assert!(
        sizes.len() as f64 > 45_000.0,
        "only {} eruptions — the sample is too small",
        sizes.len()
    );
    assert!(
        (fitted - expected).abs() < 0.02,
        "mean VEI {fitted} against the authored law's {expected}"
    );
    assert!(
        sizes.iter().all(|v| (VEI_MIN..=VEI_MAX).contains(v)),
        "an eruption size escaped the authored bracket"
    );
}
