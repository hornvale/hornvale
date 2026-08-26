//! The vessel scheduler's tick and the kernel's tick are NOT the same unit,
//! and the `f64` bridge between them differs by a tick (The Foliot, stage 1).
//!
//! `clock.rs` declares `BASE_TICKS_PER_STD_DAY = 100_000`, tied to
//! `WorldTime::TICKS_PER_STD_DAY` by nothing but agreement, and
//! `Session::charge` crosses `Ticks -> f64 days -> WorldTime` on every
//! action.
//!
//! **The campaign's opening claim — that this crossing carries no
//! information — was WRONG, and these tests are what refuted it.** A local
//! day is an exact integer of VESSEL ticks (that is what
//! `ticks_per_local_day` exists to guarantee) but `d * B` KERNEL ticks, which
//! is not an integer. So one vessel tick is `d*B / round(d*B)` kernel ticks
//! and the conversion `days_of` performs is CORRECT. Replacing it with the
//! identity, as the spec originally proposed, would have introduced an error
//! while claiming to remove one.
//!
//! These tests establish the premise the stage-1 fix rests on, BEFORE the
//! fix: the two rates agree, a local day is a whole number of ticks, and the
//! round trip nonetheless loses ticks for day lengths the rotation pin
//! admits.
//!
//! What the probe found when it was written (2026-08-26): 438 differing
//! (day length, mass, terrain) combinations, each by exactly one tick —
//! 0.864 s. **The effect is symmetric, not a leak**: over a wider sweep,
//! 213 losses against 211 gains, net -2 ticks, because `round(d*B)` sits
//! above `d*B` 651 times and below it 650. Nothing accumulates. The registry
//! row calling this "one `Session::charge` rounding from observable"
//! overstates it. The cheapest REACHABLE cost that loses one is 77,765 ticks, a
//! 1,000 kg creature on maximum-climb ground, which is an ordinary large
//! animal rather than an exotic edge. The maximum cost any authored species
//! can incur is 121,709 ticks (a 6,000 kg woolly mammoth at the climb
//! ceiling); `MASS_BAND_KG` clamps at 100,000 kg but nothing in the registry
//! approaches it, so probing the band's ceiling would answer a question no
//! world asks.

use hornvale_kernel::WorldTime;
use hornvale_kernel::room::Facet;
use hornvale_vessel::action::Action;
use hornvale_vessel::clock::{
    BASE_TICKS_PER_STD_DAY, REFERENCE_MASS_KG, Ticks, climb_factor, cost_ticks, days_of,
    ticks_per_local_day,
};

/// `clock.rs`'s `MAX_CLIMB_FACTOR`, mirrored because it is crate-private.
/// Widening a constant's visibility so a test can read it would make the
/// test's convenience part of the crate's public surface; a mirrored literal
/// with this note is the smaller commitment. If `climb_factor`'s clamp ever
/// moves, `the_mirrored_climb_ceiling_still_matches` fails.
const MAX_CLIMB_FACTOR: f64 = 4.0;

/// Day lengths the rotation pin admits, walked in minutes: 16h to 40h.
/// `anchor.rs` draws a spinning world's day as `(16.0 + u * 24.0) / 24.0`
/// standard days, so this is that interval at minute resolution.
fn admitted_day_lengths() -> impl Iterator<Item = f64> {
    ((16 * 60)..=(40 * 60)).map(|minutes| minutes as f64 / 24.0 / 60.0)
}

/// The heaviest creature the authored biosphere actually places: a woolly
/// mammoth (`domains/species/src/lib.rs`). `MASS_BAND_KG` clamps at
/// 100,000 kg, but nothing in the registry approaches that, and probing the
/// band's ceiling instead of the roster's would answer a question no world
/// asks.
const HEAVIEST_SPECIES_KG: f64 = 6_000.0;

/// The costs `cost_ticks` can actually produce, derived from the real cost
/// function rather than invented.
///
/// The ceiling is NOT `base_ticks`'s 10,000: `cost_ticks` scales it by
/// `tempo(mass)` and by a terrain factor up to `MAX_CLIMB_FACTOR` = 4.0.
/// That matters because the round trip's error is `t * eps / round(d * B)`
/// with `|eps| <= 0.5`, so a lost tick needs `t` on the order of
/// `round(d * B)` — a whole local day. A probe sampling only up to 10,000
/// sits ~25x below where the defect can appear and reports a false negative,
/// which is exactly what the first version of this test did.
fn reachable_costs() -> Vec<(f64, f64, Ticks)> {
    let mv = Action::MoveTo(Facet {
        face: 0,
        path: vec![0],
    });
    let mut out = Vec::new();
    for mass in [
        0.001,
        1.0,
        REFERENCE_MASS_KG,
        500.0,
        1_000.0,
        5_400.0,
        HEAVIEST_SPECIES_KG,
    ] {
        for terrain in [1.0, 2.0, 3.0, MAX_CLIMB_FACTOR] {
            out.push((mass, terrain, cost_ticks(&mv, mass, terrain)));
        }
    }
    out
}

/// The two lattices are declared by separate literals; assert they agree.
///
/// If this ever fails, the identity conversion the charge path relies on is
/// no longer an identity and `kernel_span` must be rederived.
#[test]
fn the_vessel_base_rate_is_the_kernel_tick_rate() {
    assert_eq!(
        BASE_TICKS_PER_STD_DAY as i64,
        WorldTime::TICKS_PER_STD_DAY,
        "vessel and kernel declare the same tick rate through separate \
         literals; they have drifted apart"
    );
}

/// A round trip through `f64` days does not always return the tick count it
/// started with — this searches for the day lengths where it does not, rather
/// than asserting a triple guessed from outside the code.
///
/// The difference is real but symmetric, so this documents a cost of the
/// two-lattice design rather than a bug in the conversion. The Foliot removes
/// the cost by removing the second lattice: once a local day is an exact
/// integer of kernel ticks, there is nothing to convert.
#[test]
fn the_f64_bridge_differs_by_a_tick_for_some_day_length() {
    let costs = reachable_costs();
    let mut witnesses = Vec::new();
    for d in admitted_day_lengths() {
        for (mass, terrain, cost) in &costs {
            let days = days_of(*cost, Some(d));
            let round_tripped = WorldTime::from_std_days(days)
                .expect("a finite day value converts")
                .ticks();
            if round_tripped != cost.0 as i64 {
                witnesses.push((d, *mass, *terrain, cost.0, round_tripped));
            }
        }
    }

    println!(
        "reachable (mass, terrain) cost points: {}, max cost {} ticks",
        costs.len(),
        costs.iter().map(|(_, _, c)| c.0).max().unwrap_or(0)
    );
    println!("bridge-loss witnesses: {}", witnesses.len());
    if let Some(min_cost) = witnesses.iter().map(|(_, _, _, t, _)| *t).min() {
        println!("cheapest REACHABLE cost that loses a tick: {min_cost}");
    }
    let worst = witnesses
        .iter()
        .map(|(_, _, _, t, got)| (*t as i64 - got).abs())
        .max()
        .unwrap_or(0);
    println!("largest loss, in ticks: {worst}");
    for (d, mass, terrain, t, got) in witnesses.iter().take(3) {
        println!("  day={d:.9} mass={mass}kg terrain={terrain}: {t} -> {got}");
    }

    assert!(
        !witnesses.is_empty(),
        "expected the f64 bridge to lose at least one tick somewhere in the \
         admitted rotation range; an EMPTY list refutes stage 1's premise -- \
         STOP and report rather than proceeding to the fix"
    );
}

/// `ticks_per_local_day` is exact by construction: a whole local day is a
/// whole number of ticks, which is what makes the local lattice usable for
/// `ActivityCycle` without dawn beating against the day cycle.
#[test]
fn a_local_day_is_a_whole_number_of_ticks() {
    for d in admitted_day_lengths() {
        assert!(
            ticks_per_local_day(Some(d)) >= 1,
            "every admitted day length divides into at least one tick"
        );
    }
    assert_eq!(
        ticks_per_local_day(None),
        BASE_TICKS_PER_STD_DAY,
        "a tidally-locked world has no day to divide and takes the base rate"
    );
}

/// The mirrored `MAX_CLIMB_FACTOR` above is a literal copy of a
/// crate-private constant. This is what keeps the copy honest: `climb_factor`
/// clamps at that ceiling, so an absurd climb must return exactly it.
#[test]
fn the_mirrored_climb_ceiling_still_matches() {
    assert_eq!(
        climb_factor(0.0, 1.0e9),
        MAX_CLIMB_FACTOR,
        "climb_factor's clamp has moved away from this file's mirrored copy"
    );
}
