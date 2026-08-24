//! Fields: typed, lazily-evaluated functions over (space × time).
//! Fields are the "coarse" in coarse-constrains-fine (Constitution §2.2).

use crate::noise::fbm_2d;
use crate::seed::Seed;
use serde::{Deserialize, Serialize};

/// A location in world space. Units and topology are a terrain-domain
/// concern; the kernel only requires a metric-ish plane.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct Position {
    /// Horizontal coordinate.
    pub x: f64,
    /// Vertical coordinate.
    pub y: f64,
}

/// Simulated time since world genesis, exposed as an exact tick count.
///
/// **Why an integer and not `f64` days, and why this type still holds an
/// `f64` today** (The Escapement, decision 0186): an instant IS a tick — the
/// tick lattice is the time domain, not a quantization of it. A `WorldTime`
/// used to be emitted through [`crate::quantize`] like every other float,
/// and 8 *significant* digits give precision proportional to MAGNITUDE.
/// Time is the only unbounded quantity in the system, so a committed day
/// decayed with world age, measured at 43.2 s of resolution at world-year
/// 100 and **12 hours** at 200,000, a horizon
/// `windows/worldgen/src/hazard.rs` actually constructs.
///
/// **The migration is staged** (Ruling 8 — see "Execution phasing" in
/// `docs/superpowers/plans/2026-08-23-the-escapement.md`): flipping the
/// field to `i64` changes behaviour workspace-wide the instant a value
/// rounds to its nearest tick at construction, and most of the affected
/// comparison sites are in `windows/vessel`, held off by
/// `campaign/the-hand`. **Phase A (this commit)** adds the full tick-shaped
/// surface — [`WorldTime::from_ticks`], [`WorldTime::ticks`],
/// [`WorldTime::from_std_days`], [`WorldTime::as_std_days`],
/// [`WorldTime::whole_days`], [`WorldTime::tick_of_day`] — as accessors over
/// the SAME `f64` field this type has always had, so nothing rounds on
/// storage and no behaviour changes; its job is letting later tasks port
/// call sites to the new names one crate at a time, ahead of the
/// representation flip. **Phase B** flips the field itself to an exact
/// `i64`, derives `Ord`/`Eq`/`Hash`, deletes the shims below, and lands in
/// one commit together with the tick-domain comparison fixes and the vessel
/// port.
///
/// The field is private and the crossings are named, because this type's
/// whole job is that a value in some *other* unit cannot be stored here. A
/// year stamped into a day-typed slot is what made `person-died`
/// uncommittable in every world (The Ell); decision 0014 declined this
/// wrapper, 0126 superseded it, and 0186 makes it exact (Phase B).
///
/// **Negative is legal.** A day is a *point on an axis*, not a duration:
/// a founder born before the history record begins has a negative birth
/// day. Do not copy `Years`'s non-negative rule here.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct WorldTime {
    day: f64,
}

impl WorldTime {
    /// World genesis — day zero.
    pub const GENESIS: WorldTime = WorldTime { day: 0.0 };

    /// Ticks per STANDARD day; one tick is 0.864 s.
    ///
    /// Promoted from `windows/vessel`'s scheduler clock (`BASE_TICKS_PER_STD_DAY`)
    /// rather than invented, so the walk band's clock and the kernel's agree by
    /// construction instead of by a lossy bridge. `Years::DAYS_PER_YEAR` is
    /// 365.25, so a year is exactly 36,525,000 ticks — no repeating fraction.
    /// type-audit: bare-ok(count)
    pub const TICKS_PER_STD_DAY: i64 = 100_000;

    /// MIGRATION SHIM — deleted in Phase B, do not use in new code.
    ///
    /// Preserves the pre-Escapement `new(day)` signature and validation so
    /// the workspace keeps compiling while crates are ported one at a time.
    /// type-audit: bare-ok(constructor-edge: day)
    pub fn new(day: f64) -> Result<WorldTime, crate::units::UnitError> {
        WorldTime::from_std_days(day)
    }

    /// MIGRATION SHIM — deleted in Phase B, do not use in new code.
    /// See [`WorldTime::new`]. At a ported site prefer
    /// [`WorldTime::as_std_days`] (Phase A: identical) or
    /// [`WorldTime::ticks`] (exact once Phase B lands).
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn day(self) -> f64 {
        self.as_std_days()
    }

    /// Build from an exact tick count.
    ///
    /// Phase A note: the field behind this type is still `f64`, so the
    /// result is the closest representable day to
    /// `ticks / TICKS_PER_STD_DAY` — not yet bit-exact the way Phase B's
    /// `i64` field will make it.
    /// type-audit: bare-ok(count: ticks)
    pub fn from_ticks(ticks: i64) -> WorldTime {
        WorldTime {
            day: ticks as f64 / Self::TICKS_PER_STD_DAY as f64,
        }
    }

    /// Ticks since genesis, rounded to the nearest tick.
    ///
    /// Phase A note: this ROUNDS a continuous `f64` day; it is not yet the
    /// type's stored representation. Phase B makes it exact and cheap (a
    /// field read, not a multiply-and-round).
    /// type-audit: bare-ok(count: return)
    pub fn ticks(self) -> i64 {
        (self.day * Self::TICKS_PER_STD_DAY as f64).round() as i64
    }

    /// Build from fractional standard days.
    ///
    /// Phase A note: stores `days` EXACTLY — this constructor does not round
    /// to a tick, because the field itself is still a continuous `f64`
    /// (Phase B is what makes "constructed from a day" and "an exact tick"
    /// the same fact). The tick-range check runs regardless, so a value
    /// Phase B could not represent is already rejected today.
    /// type-audit: bare-ok(constructor-edge: days)
    pub fn from_std_days(days: f64) -> Result<WorldTime, crate::units::UnitError> {
        if !days.is_finite() {
            return Err(crate::units::UnitError {
                unit: "standard days",
                value: days,
                reason: "must be finite",
            });
        }
        // `round` is IEEE-exact and platform-stable (see kernel/src/math.rs's
        // module doc); only transcendentals need the libm crate.
        let ticks = (days * Self::TICKS_PER_STD_DAY as f64).round();
        if ticks < i64::MIN as f64 || ticks > i64::MAX as f64 {
            return Err(crate::units::UnitError {
                unit: "standard days",
                value: days,
                reason: "outside the representable tick range",
            });
        }
        Ok(WorldTime { day: days })
    }

    /// Fractional standard days since genesis.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn as_std_days(self) -> f64 {
        self.day
    }

    /// Whole standard days since genesis, **flooring** — so one tick before
    /// genesis is day `-1`, not day `0`. `trunc`-style truncation toward zero
    /// is what this type exists to make impossible.
    /// type-audit: bare-ok(count: return)
    pub fn whole_days(self) -> i64 {
        self.day.floor() as i64
    }

    /// Tick within the current standard day, always in
    /// `0..TICKS_PER_STD_DAY` even for negative instants.
    /// type-audit: bare-ok(count: return)
    pub fn tick_of_day(self) -> i64 {
        self.ticks().rem_euclid(Self::TICKS_PER_STD_DAY)
    }
}

impl std::ops::Sub for WorldTime {
    type Output = crate::units::TickSpan;
    fn sub(self, rhs: WorldTime) -> crate::units::TickSpan {
        crate::units::TickSpan(self.day - rhs.day)
    }
}

impl std::ops::Add<crate::units::TickSpan> for WorldTime {
    type Output = WorldTime;
    fn add(self, rhs: crate::units::TickSpan) -> WorldTime {
        WorldTime {
            day: self.day + rhs.0,
        }
    }
}

/// A typed field over (space × time). Implementations must be pure:
/// same (pos, time) → same value, always.
pub trait Field<T> {
    /// Sample the field's value at the given position and time.
    fn sample(&self, pos: Position, time: WorldTime) -> T;
}

/// The tier-0 field: the same value everywhere, forever.
#[derive(Clone, Debug)]
pub struct ConstantField<T: Clone>(
    /// The value returned for every position and time.
    pub T,
);

impl<T: Clone> Field<T> for ConstantField<T> {
    fn sample(&self, _pos: Position, _time: WorldTime) -> T {
        self.0.clone()
    }
}

/// A time-invariant fbm noise field in [0, 1). `scale` is the feature
/// wavelength in world units.
/// type-audit: bare-ok(count: octaves), pending(wave-1: scale)
#[derive(Clone, Copy, Debug)]
pub struct NoiseField {
    /// Seed driving the underlying noise stream.
    pub seed: Seed,
    /// Number of fbm octaves to accumulate.
    pub octaves: u32,
    /// Feature wavelength in world units.
    pub scale: f64,
}

impl Field<f64> for NoiseField {
    fn sample(&self, pos: Position, _time: WorldTime) -> f64 {
        fbm_2d(
            self.seed,
            pos.x / self.scale,
            pos.y / self.scale,
            self.octaves,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn noon() -> WorldTime {
        WorldTime::from_ticks(WorldTime::TICKS_PER_STD_DAY / 2)
    }

    // This is also the sole coverage for the claim
    // `kernel::ledger::tests::non_finite_day_is_rejected` used to make at the
    // ledger boundary (Task 2, The Ell, fix round 1): once `Fact.day` became
    // `Option<WorldTime>`, that test's assertion was word-for-word this one —
    // a `Fact` can no longer be BUILT with a non-finite day, so there was
    // nothing left for `Ledger::check`/`commit` to reject, and the duplicate
    // was deleted rather than kept under a name that still promised
    // ledger-level rejection.
    #[test]
    fn a_world_time_cannot_be_built_from_a_non_finite_number_of_days() {
        assert!(
            WorldTime::from_std_days(f64::NAN).is_err(),
            "NaN is not a point on the time axis"
        );
        assert!(WorldTime::from_std_days(f64::INFINITY).is_err());
        assert!(WorldTime::from_std_days(f64::NEG_INFINITY).is_err());
    }

    #[test]
    fn a_world_time_cannot_be_built_from_days_beyond_the_tick_range() {
        // 1e300 days is ~1e305 ticks: far outside i64. Must be a typed error,
        // never a saturating `as` cast. The range check runs in Phase A too,
        // even though storage doesn't round yet (see from_std_days's doc).
        assert!(WorldTime::from_std_days(1e300).is_err());
        assert!(WorldTime::from_std_days(-1e300).is_err());
    }

    #[test]
    fn a_world_time_may_be_negative_because_a_day_is_a_point_not_a_duration() {
        // The Particular's founders are born before the history record begins.
        let t = WorldTime::from_std_days(-20_164.663).expect("a negative day is legal");
        assert!(t.ticks() < 0, "a pre-genesis instant has negative ticks");
    }

    #[test]
    fn a_tick_count_round_trips_through_from_ticks_and_ticks() {
        // Phase A: WorldTime still stores a continuous f64 day, so this only
        // proves from_ticks/ticks agree with each other at ordinary
        // magnitudes — not yet the exact-at-deep-time claim, which needs
        // Phase B's i64 field and moves there.
        for n in [0_i64, 1, -1, 12_345, -987_654, WorldTime::TICKS_PER_STD_DAY] {
            assert_eq!(
                WorldTime::from_ticks(n).ticks(),
                n,
                "from_ticks/ticks should round-trip at ordinary magnitudes"
            );
        }
    }

    #[test]
    fn whole_days_and_tick_of_day_floor_rather_than_truncate() {
        // The defect this replaces: `trunc() as u64` rounds toward zero and
        // saturates. Flooring/rem_euclid is what a calendar means, and
        // negative days are legal.
        let t = WorldTime::from_ticks(-1);
        assert_eq!(
            t.whole_days(),
            -1,
            "one tick before genesis is day -1, not day 0"
        );
        assert_eq!(t.tick_of_day(), WorldTime::TICKS_PER_STD_DAY - 1);

        let g = WorldTime::GENESIS;
        assert_eq!(g.whole_days(), 0);
        assert_eq!(g.tick_of_day(), 0);
    }

    #[test]
    fn a_difference_of_two_instants_is_a_signed_span() {
        let a = WorldTime::from_ticks(10);
        let b = WorldTime::from_ticks(4);
        assert_eq!((a - b).ticks(), 6);
        assert_eq!((b - a).ticks(), -6, "a span is signed; order is not lost");
        assert_eq!(
            (b + (a - b)).ticks(),
            a.ticks(),
            "add is the inverse of sub"
        );
    }

    // `world_time_is_ordered_and_equatable_so_it_can_key_a_map` moves to
    // Phase B: with the field still `f64`, WorldTime cannot derive
    // `Ord`/`Eq`/`Hash` (no total order over floats), so the claim it made —
    // that WorldTime itself can key a BTreeMap — isn't true yet. `ticks()`
    // already gives the exact integer key Phase B will store directly, so a
    // caller that needs one today uses that.
    #[test]
    fn world_time_ticks_can_key_a_map_even_though_the_type_itself_cannot_yet() {
        let mut m: std::collections::BTreeMap<i64, &str> = std::collections::BTreeMap::new();
        m.insert(WorldTime::from_ticks(2).ticks(), "later");
        m.insert(WorldTime::from_ticks(1).ticks(), "earlier");
        let order: Vec<&str> = m.values().copied().collect();
        assert_eq!(order, vec!["earlier", "later"]);
    }

    #[test]
    fn constant_field_returns_its_value_everywhere() {
        let f = ConstantField(18.0_f64);
        assert_eq!(f.sample(Position { x: 0.0, y: 0.0 }, noon()), 18.0);
        assert_eq!(f.sample(Position { x: 1e6, y: -1e6 }, noon()), 18.0);
    }

    #[test]
    fn constant_field_works_for_non_numeric_types() {
        let f = ConstantField("temperate forest".to_string());
        assert_eq!(
            f.sample(Position { x: 3.0, y: 4.0 }, noon()),
            "temperate forest"
        );
    }

    #[test]
    fn noise_field_is_deterministic_and_bounded() {
        let f = NoiseField {
            seed: Seed(42),
            octaves: 3,
            scale: 10.0,
        };
        let p = Position { x: 12.5, y: -7.25 };
        let a = f.sample(p, noon());
        assert_eq!(a, f.sample(p, noon()));
        assert!((0.0..1.0).contains(&a));
    }

    #[test]
    fn noise_field_scale_stretches_space() {
        // Two points one unit apart are nearly identical under a huge scale.
        let f = NoiseField {
            seed: Seed(42),
            octaves: 1,
            scale: 1000.0,
        };
        let a = f.sample(Position { x: 0.0, y: 0.0 }, noon());
        let b = f.sample(Position { x: 1.0, y: 0.0 }, noon());
        assert!((a - b).abs() < 0.01);
    }

    #[test]
    fn position_and_time_serialize_roundtrip() {
        let p = Position { x: 1.5, y: -2.5 };
        let t = WorldTime::from_ticks(1_225_000);
        let p2: Position = serde_json::from_str(&serde_json::to_string(&p).unwrap()).unwrap();
        let t2: WorldTime = serde_json::from_str(&serde_json::to_string(&t).unwrap()).unwrap();
        assert_eq!((p2.x, p2.y, t2.ticks()), (p.x, p.y, t.ticks()));
    }

    /// The direct wire-shape assertion `#[serde(transparent)]` exists for.
    /// A round-trip test alone cannot tell "bare scalar" from "single-field
    /// object" apart — both round-trip identically — and no `Serialize`-
    /// deriving struct in this repo holds a `WorldTime` field yet, so no
    /// committed artifact drift check can catch a lost attribute either.
    /// Task 2 stores a `WorldTime` directly in `Fact`; losing this attribute
    /// then would silently rewrite every world's save format from a bare
    /// `12.25` to `{"day":12.25}`. Mutation-proved in the fix-round report:
    /// deleting `#[serde(transparent)]` reddens this test.
    #[test]
    fn world_time_serializes_as_a_bare_scalar_not_an_object() {
        // Phase A: the field is still f64, so the wire shape is still a bare
        // day float. Phase B is what turns "12.25" into an integer tick
        // count; that assertion moves there with the representation flip.
        let t = WorldTime::from_std_days(12.25).expect("finite");
        let json = serde_json::to_string(&t).unwrap();
        assert_eq!(
            json, "12.25",
            "WorldTime must serialize as the bare day scalar, not a \
             {{\"day\":...}} object — #[serde(transparent)] is what keeps it that way"
        );
    }
}
