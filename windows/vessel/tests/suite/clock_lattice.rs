//! There is ONE tick lattice, and vessel's scheduler shares it with the
//! kernel (The Foliot).
//!
//! **The history this file records, because it cost a campaign three wrong
//! answers.** `clock.rs` used to declare `BASE_TICKS_PER_STD_DAY` as its own
//! literal and cross into the kernel's lattice through `f64` days on every
//! action. The campaign that came to delete that bridge asserted it was "pure
//! loss — a vessel tick IS a kernel tick." That was **false**: a local day was
//! an exact integer of VESSEL ticks (which is what `ticks_per_local_day`
//! existed to guarantee) but `d * B` KERNEL ticks, which is not an integer, so
//! the conversion was CORRECT and deleting it as an identity would have
//! introduced an error.
//!
//! Measured before anything changed: over the admitted rotation range and the
//! costs `cost_of` can actually produce, 438 differing (day length, mass,
//! terrain) combinations, each by exactly one tick (0.864 s). Over a wider
//! sweep, 213 losses against 211 gains, net -2 ticks — symmetric noise with
//! nothing accumulating, because `round(d*B)` sat above `d*B` 651 times and
//! below it 650. The idea registry's "one `Session::charge` rounding from
//! observable" overstated it.
//!
//! Two sampling errors are recorded because either alone gives a wrong answer.
//! Sampling costs to only `base_cost`'s 10,000 finds ZERO witnesses and reads
//! as "no defect" — the error is `t * eps / round(d*B)`, so a differing tick
//! needs `t` on the order of a whole local day. Sampling to `MASS_BAND_KG`'s
//! 100,000 kg overstates it the other way — no authored species exceeds
//! 6,000 kg, so the true reachable maximum was 121,709 ticks (a woolly mammoth
//! at the climb ceiling), not 246,000, and the cheapest reachable differing
//! cost was 77,765 — a 1,000 kg creature on steep ground, an ordinary large
//! animal rather than an exotic edge.
//!
//! **What fixed it was removing the reason for two lattices**, not managing
//! the conversion between them: a world's day is now drawn as an exact tick
//! count, so a local day divides the kernel lattice exactly. The tests below
//! assert the resulting identity, which is what makes `Ticks` and `days_of`
//! deletable rather than merely renameable.

use hornvale_kernel::WorldTime;
use hornvale_kernel::room::Facet;
use hornvale_kernel::units::TickSpan;
use hornvale_vessel::action::Action;
use hornvale_vessel::clock::{
    BASE_TICKS_PER_STD_DAY, REFERENCE_MASS_KG, climb_factor, cost_of, ticks_per_local_day,
};

/// `clock.rs`'s `MAX_CLIMB_FACTOR`, mirrored because it is crate-private.
/// Widening a constant's visibility so a test can read it would make the
/// test's convenience part of the crate's public surface; a mirrored literal
/// with this note is the smaller commitment. If `climb_factor`'s clamp ever
/// moves, `the_mirrored_climb_ceiling_still_matches` fails.
const MAX_CLIMB_FACTOR: f64 = 4.0;

/// The heaviest creature the authored biosphere actually places: a woolly
/// mammoth (`domains/species/src/lib.rs`).
const HEAVIEST_SPECIES_KG: f64 = 6_000.0;

fn move_action() -> Action {
    Action::MoveTo(Facet {
        face: 0,
        path: vec![0],
    })
}

/// Day lengths as a world now draws them: exact tick counts spanning the
/// admitted 16h–40h rotation range.
fn admitted_day_ticks() -> impl Iterator<Item = TickSpan> {
    ((16 * 60)..=(40 * 60)).map(|minutes| {
        let std_days = minutes as f64 / 24.0 / 60.0;
        TickSpan::from_std_days(std_days).expect("an admitted rotation is representable")
    })
}

/// The rate is the kernel's constant, not a second literal that agrees.
///
/// The distinction is the whole point: two independently-declared literals
/// that happen to match are what let two lattices coexist unnoticed.
#[test]
fn the_vessel_base_rate_is_the_kernel_tick_rate() {
    assert_eq!(BASE_TICKS_PER_STD_DAY, WorldTime::TICKS_PER_STD_DAY);
}

/// A local day is a whole number of KERNEL ticks, for every admitted rotation.
///
/// This is the property that did not hold before, and its absence is the
/// entire reason a second lattice existed.
#[test]
fn a_local_day_is_a_whole_number_of_kernel_ticks() {
    for day in admitted_day_ticks() {
        assert!(day.ticks() >= 1);
        assert_eq!(
            ticks_per_local_day(Some(day)),
            day.ticks(),
            "the scheduler reports the day as drawn, with no rounding of its own"
        );
    }
    assert_eq!(
        ticks_per_local_day(None),
        BASE_TICKS_PER_STD_DAY,
        "a tidally-locked world has no day to divide and takes the base rate"
    );
}

/// No reachable cost differs by a tick on any admitted world — the same sweep
/// that used to find 438 counterexamples.
///
/// Kept as a sweep rather than collapsed into an assertion about `cost_of`'s
/// return type, because the claim is about the whole reachable space and the
/// old failures were visible only by sweeping it.
#[test]
fn no_reachable_cost_differs_by_a_tick_on_any_admitted_world() {
    let mv = move_action();
    let mut differing = 0;
    let mut checked = 0;
    for day in admitted_day_ticks() {
        for mass in [0.001, 1.0, REFERENCE_MASS_KG, 1_000.0, HEAVIEST_SPECIES_KG] {
            for terrain in [1.0, 2.0, MAX_CLIMB_FACTOR] {
                let cost = cost_of(&mv, mass, terrain, 1.0);
                checked += 1;
                // The round trip the old bridge made, now expressed through
                // the kernel's own hatch: a span out to continuous days and
                // back must be the span it started as.
                let back = TickSpan::from_std_days(cost.as_std_days())
                    .expect("a cost is representable")
                    .ticks();
                if back != cost.ticks() {
                    differing += 1;
                }
                // And the planet never enters the arithmetic at all any more.
                assert_eq!(
                    ticks_per_local_day(Some(day)),
                    day.ticks(),
                    "the day is reported, not recomputed"
                );
            }
        }
    }
    assert!(checked > 0, "the sweep is vacuous");
    assert_eq!(
        differing, 0,
        "{differing} of {checked} reachable costs still differ by a tick; \
         before The Foliot this number was 438"
    );
}

/// A move costs the same span on every world, exactly.
///
/// Base costs are authored in ticks, so a bear's gait is set by the bear and
/// not by the sky (spec §4.1). This was previously true only to within 0.1%,
/// because the cost crossed the local lattice and back.
#[test]
fn a_move_costs_the_same_span_on_every_world() {
    let mv = move_action();
    let reference = cost_of(&mv, REFERENCE_MASS_KG, 1.0, 1.0);
    for day in admitted_day_ticks() {
        assert_eq!(ticks_per_local_day(Some(day)), day.ticks());
        assert_eq!(cost_of(&mv, REFERENCE_MASS_KG, 1.0, 1.0), reference);
    }
}

/// The mirrored `MAX_CLIMB_FACTOR` above is a literal copy of a crate-private
/// constant. This is what keeps the copy honest: `climb_factor` clamps at that
/// ceiling, so an absurd climb must return exactly it.
#[test]
fn the_mirrored_climb_ceiling_still_matches() {
    assert_eq!(
        climb_factor(0.0, 1.0e9),
        MAX_CLIMB_FACTOR,
        "climb_factor's clamp has moved away from this file's mirrored copy"
    );
}
