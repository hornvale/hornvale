//! A world's local day is an exact integer of kernel ticks (The Foliot).
//!
//! Before this, the day was a drawn `f64` and `d * TICKS_PER_STD_DAY` had a
//! fractional part, which is why `windows/vessel` had to keep a second tick
//! lattice and convert between them on every action. Quantizing at the draw
//! removes the reason two lattices existed, rather than managing the
//! conversion between them.
//!
//! The draw itself is unchanged — `anchor.rs` still takes the same two
//! `next_f64()` values in the same order — so no seed label takes an epoch
//! suffix and the pin-isolation tests hold. Only the derived value moves, by
//! up to half a tick (0.432 s).
//!
//! Built through `generate`, astronomy's own entry point, rather than
//! `hornvale_worldgen::build_world`: a domain's tests may not reach a window
//! any more than its source may (the layering rule).

use hornvale_astronomy::{Rotation, RotationPin, SkyPins, calendar_of, generate};
use hornvale_kernel::{Seed, WorldTime};

/// Every spinning world's day divides the tick lattice exactly.
///
/// Swept across many seeds deliberately: the drawn day length varies per
/// world, so a single seed would establish nothing about a property that is
/// about all of them.
///
/// claim: invariant(census: none — a structural property of the draw, not a
/// measured rate; every spinning world satisfies it or the quantization is
/// broken)
#[test]
fn every_spinning_worlds_day_is_a_whole_number_of_ticks() {
    let mut spinning = 0;
    for seed in 0..128 {
        let outcome = generate(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"));
        if let Rotation::Spinning { day, .. } = &outcome.system.anchor.rotation {
            spinning += 1;
            let ticks = day.ticks();
            assert!(ticks > 0, "seed {seed}: a spinning day is positive");
            assert_eq!(
                WorldTime::from_ticks(ticks).ticks(),
                ticks,
                "seed {seed}: the day is not lattice-aligned"
            );
        }
    }
    assert!(
        spinning > 0,
        "no spinning world in 128 seeds — the probe is vacuous"
    );
}

/// A pinned day length is quantized on the same path the drawn one is.
///
/// The pin route is a separate construction site in `anchor.rs`, so it can
/// drift from the drawn route silently; 12.5 hours is deliberately not a
/// whole number of ticks before quantization.
#[test]
fn a_pinned_day_length_is_quantized_too() {
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(12.5)),
        ..Default::default()
    };
    let outcome = generate(Seed(42), &pins).expect("12.5 hours is a legal pin");
    let Rotation::Spinning { day, .. } = &outcome.system.anchor.rotation else {
        panic!("a pinned period produces a spinning world");
    };
    assert_eq!(
        WorldTime::from_ticks(day.ticks()).ticks(),
        day.ticks(),
        "a pinned day is lattice-aligned"
    );
}

/// The continuous view is derived from the integer, and round-trips.
///
/// `Calendar::day_length()` keeps returning `StdDays` so its 31 call sites do
/// not move; this pins that the conversion is lossless in the direction that
/// matters — integer out, integer back.
///
/// claim: invariant(census: none — a round-trip identity over the derived
/// view, true of every world or the derivation is wrong)
#[test]
fn the_continuous_day_length_round_trips_through_the_lattice() {
    for seed in 0..64 {
        let outcome = generate(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"));
        let Rotation::Spinning { day, .. } = &outcome.system.anchor.rotation else {
            continue;
        };
        let calendar = calendar_of(&outcome.system);
        let continuous = calendar
            .day_length()
            .expect("a spinning world has a day length");
        let back = (continuous.get() * WorldTime::TICKS_PER_STD_DAY as f64).round() as i64;
        assert_eq!(
            back,
            day.ticks(),
            "seed {seed}: the continuous view does not round-trip to the stored ticks"
        );
    }
}
