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

/// Simulated time since world genesis, as an exact tick count.
///
/// **Why an integer and not `f64` days** (The Escapement, decision 0186):
/// an instant IS a tick — the tick lattice is the time domain, not a
/// quantization of it. A `WorldTime` used to be emitted through
/// [`crate::quantize`] like every other float, and 8 *significant* digits
/// give precision proportional to MAGNITUDE. Time is the only unbounded
/// quantity in the system, so a committed day decayed with world age,
/// measured as a full lattice spacing of 86.4 s between adjacent storable
/// instants at world-year 100 and **24 hours** at 200,000, a horizon
/// `windows/worldgen/src/hazard.rs` actually constructs.
/// Ticks do not decay: `Ledger::commit` no longer canonicalises a day at
/// all, because an integer needs no canonicalisation.
///
/// **One domain per comparison** (spec §2.1). Code that draws a continuous
/// time converts to ticks ONCE, at the draw, via
/// [`WorldTime::from_std_days`], and compares ticks exactly thereafter —
/// never a raw `f64` draw against a round-tripped bound.
///
/// The field is private and the crossings are named, because this type's
/// whole job is that a value in some *other* unit cannot be stored here. A
/// year stamped into a day-typed slot is what made `person-died`
/// uncommittable in every world (The Ell); decision 0014 declined this
/// wrapper, 0126 superseded it, and 0186 makes it exact.
///
/// **Negative is legal.** A day is a *point on an axis*, not a duration:
/// a founder born before the history record begins has a negative birth
/// day. Do not copy `Years`'s non-negative rule here.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct WorldTime {
    ticks: i64,
}

impl WorldTime {
    /// World genesis — day zero.
    pub const GENESIS: WorldTime = WorldTime { ticks: 0 };

    /// Ticks per STANDARD day; one tick is 0.864 s.
    ///
    /// Promoted from `windows/vessel`'s scheduler clock (`BASE_TICKS_PER_STD_DAY`)
    /// rather than invented, so the walk band's clock and the kernel's beat at
    /// the same rate and no change of scale sits between them. They are still
    /// two independent literals, not one constant: vessel keeps its own copy
    /// and still bridges through `f64` days (`clock::days_of`), which The
    /// Escapement did not unify and its retrospective carries as follow-up
    /// work. `Years::DAYS_PER_YEAR` is
    /// 365.25, so a year is exactly 36,525,000 ticks — no repeating fraction.
    /// type-audit: bare-ok(count)
    pub const TICKS_PER_STD_DAY: i64 = 100_000;

    /// Build from an exact tick count. This is the type's own
    /// representation, so it is infallible and lossless.
    /// type-audit: bare-ok(count: ticks)
    pub const fn from_ticks(ticks: i64) -> WorldTime {
        WorldTime { ticks }
    }

    /// Ticks since genesis — an exact field read.
    /// type-audit: bare-ok(count: return)
    pub const fn ticks(self) -> i64 {
        self.ticks
    }

    /// Build from fractional standard days, **rounding to the nearest
    /// tick**. This is the one crossing from the continuous domain into the
    /// lattice, and spec §2.1 requires it happen ONCE, at the draw — after
    /// which comparisons are exact tick comparisons.
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
        // The upper bound is exclusive and is 2^63, NOT `i64::MAX as f64` —
        // see `units::TICKS_EXCLUSIVE_UPPER_BOUND` for why those differ.
        if ticks < i64::MIN as f64 || ticks >= crate::units::TICKS_EXCLUSIVE_UPPER_BOUND {
            return Err(crate::units::UnitError {
                unit: "standard days",
                value: days,
                reason: "outside the representable tick range",
            });
        }
        Ok(WorldTime {
            ticks: ticks as i64,
        })
    }

    /// Fractional standard days since genesis. Lossless below 2^53 ticks
    /// (~2.47e8 years), which is every horizon the project has ever used.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn as_std_days(self) -> f64 {
        self.ticks as f64 / Self::TICKS_PER_STD_DAY as f64
    }

    /// Whole standard days since genesis, **flooring** — so one tick before
    /// genesis is day `-1`, not day `0`. `trunc`-style truncation toward zero
    /// is what this type exists to make impossible.
    ///
    /// Derived from the stored tick count, never from an `f64` day: one
    /// domain per computation (spec §2.1), never an f64-derived value
    /// compared against a tick-derived one.
    /// type-audit: bare-ok(count: return)
    pub const fn whole_days(self) -> i64 {
        self.ticks.div_euclid(Self::TICKS_PER_STD_DAY)
    }

    /// Tick within the current standard day, always in
    /// `0..TICKS_PER_STD_DAY` even for negative instants.
    ///
    /// Derived from the stored tick count, same as `whole_days()`.
    /// `div_euclid`/`rem_euclid` satisfy `q * T + r == n` for every `n`, so
    /// `whole_days() * TICKS_PER_STD_DAY + tick_of_day() == ticks()` holds
    /// structurally rather than by luck.
    /// type-audit: bare-ok(count: return)
    pub const fn tick_of_day(self) -> i64 {
        self.ticks.rem_euclid(Self::TICKS_PER_STD_DAY)
    }
}

impl std::ops::Sub for WorldTime {
    type Output = crate::units::TickSpan;
    /// Checked: `from_ticks` is infallible and accepts any `i64`, so two
    /// instants near the representable edges (Ruling 11) can differ by more
    /// ticks than `i64` holds. An unchecked subtraction would wrap silently
    /// in release — the worst failure class this kernel has, a world that
    /// looks valid and disagrees with itself — so this panics loudly instead.
    fn sub(self, rhs: WorldTime) -> crate::units::TickSpan {
        let ticks = self.ticks.checked_sub(rhs.ticks).unwrap_or_else(|| {
            panic!(
                "WorldTime subtraction overflowed i64 ticks: {} - {} \
                 (minuend instant ticks minus subtrahend instant ticks)",
                self.ticks, rhs.ticks
            )
        });
        crate::units::TickSpan(ticks)
    }
}

impl std::ops::Add<crate::units::TickSpan> for WorldTime {
    type Output = WorldTime;
    /// Checked, for the same reason as `Sub` above: an instant plus a span
    /// can be constructed to overflow `i64`, and a silent wraparound would
    /// produce an instant that is not the instant it claims to be.
    fn add(self, rhs: crate::units::TickSpan) -> WorldTime {
        let ticks = self.ticks.checked_add(rhs.0).unwrap_or_else(|| {
            panic!(
                "WorldTime + TickSpan overflowed i64 ticks: {} + {} \
                 (instant ticks plus span ticks)",
                self.ticks, rhs.0
            )
        });
        WorldTime { ticks }
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
        // never a saturating `as` cast. The exact boundary — where "far
        // outside" becomes "one step outside" — is pinned by the test below.
        assert!(WorldTime::from_std_days(1e300).is_err());
        assert!(WorldTime::from_std_days(-1e300).is_err());
    }

    #[test]
    fn the_tick_range_check_rejects_exactly_two_to_the_sixty_three() {
        // The campaign's own defect class, one level down: a check that looks
        // exact and is off by one representable step. `i64::MAX` is 2^63 - 1,
        // which `f64` CANNOT represent — `i64::MAX as f64` rounds UP to 2^63.
        // So a guard written `ticks > i64::MAX as f64` admits a tick count of
        // exactly 2^63, and `as i64` then SATURATES it to `i64::MAX`: a
        // silently wrong instant one step past the end of the axis, where a
        // typed refusal belongs.
        let two_pow_63 = 9_223_372_036_854_775_808.0_f64;
        assert_eq!(
            i64::MAX as f64,
            two_pow_63,
            "the cast rounds up, and that is the entire hazard"
        );

        let per_day = WorldTime::TICKS_PER_STD_DAY as f64;
        let over = two_pow_63 / per_day;
        assert_eq!(
            (over * per_day).round(),
            two_pow_63,
            "this day count really does land on 2^63 ticks"
        );
        assert!(
            WorldTime::from_std_days(over).is_err(),
            "2^63 ticks is one past the axis, not the last point on it"
        );

        // ...and the guard must not over-reject: the largest `f64` strictly
        // below 2^63 is 2^63 - 1024, a perfectly legal tick count.
        let last = f64::from_bits(two_pow_63.to_bits() - 1);
        let under = last / per_day;
        assert_eq!(
            WorldTime::from_std_days(under)
                .expect("2^63 - 1024 ticks is inside the axis")
                .ticks(),
            9_223_372_036_854_774_784,
        );

        // The negative edge needs no equivalent care and is pinned here so a
        // future tightening cannot quietly narrow it: `i64::MIN` is -2^63
        // exactly, so it is representable and legal.
        assert_eq!(
            WorldTime::from_std_days(-two_pow_63 / per_day)
                .expect("i64::MIN ticks is on the axis")
                .ticks(),
            i64::MIN,
        );
    }

    #[test]
    fn a_world_time_may_be_negative_because_a_day_is_a_point_not_a_duration() {
        // The Particular's founders are born before the history record begins.
        let t = WorldTime::from_std_days(-20_164.663).expect("a negative day is legal");
        assert!(t.ticks() < 0, "a pre-genesis instant has negative ticks");
    }

    #[test]
    fn a_tick_count_round_trips_through_from_ticks_and_ticks() {
        for n in [0_i64, 1, -1, 12_345, -987_654, WorldTime::TICKS_PER_STD_DAY] {
            assert_eq!(
                WorldTime::from_ticks(n).ticks(),
                n,
                "from_ticks/ticks must round-trip exactly"
            );
        }
    }

    #[test]
    fn a_tick_count_round_trips_exactly_where_an_f64_day_could_not() {
        // The whole point of the campaign: at deep time, an f64 day quantized
        // to 8 significant digits could not separate two instants less than a
        // full day apart. Ticks do not decay.
        let deep = 200_000.0 * crate::units::Years::DAYS_PER_YEAR;
        let t = WorldTime::from_std_days(deep).expect("finite");
        let one_tick_later = WorldTime::from_ticks(t.ticks() + 1);
        assert_ne!(
            t, one_tick_later,
            "adjacent ticks stay distinct at 200,000 years"
        );
        assert_eq!(one_tick_later.ticks() - t.ticks(), 1);

        // The negative control: the SAME pair of instants is indistinguishable
        // once each is put through the quantization that used to run at every
        // commit.
        let q = |x: WorldTime| crate::quantize::quantize(x.as_std_days());
        assert_eq!(
            q(t),
            q(one_tick_later),
            "8 significant digits cannot separate adjacent ticks at this horizon"
        );
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

    /// The Escapement's whole motivation is negative-time correctness, but
    /// nothing in the tree exercised a NEGATIVE, non-tick-aligned `f64` day
    /// -- one whose tick value does not land on a day boundary -- and
    /// checked that `whole_days()` and `tick_of_day()` still agree with
    /// `ticks()`. A reviewer hand-checked several values and found no
    /// disagreement; this makes that check a test instead of a memory.
    #[test]
    fn whole_days_and_tick_of_day_agree_with_ticks_for_a_negative_off_boundary_day() {
        // -3.0000001 days is the verified counterexample from fix round 1's
        // code review: `-3.0000001 * TICKS_PER_STD_DAY == -300_000.01`,
        // which ROUNDS to `ticks() == -300_000` -- exactly on a day
        // boundary in tick space -- while the raw f64 day is a hair below
        // -3.0, so a `whole_days()` computed from `self.day.floor()`
        // (the pre-fix implementation) disagreed with one computed from
        // `ticks()`: floor gave -4, ticks-based division gives -3. That
        // mismatch is precisely what this test exists to catch, which is
        // why an earlier version of this test (using the exactly
        // representable -2.75, which never rounds) passed trivially and
        // let the bug through -- see the fix-round-1 report for the
        // captured pre-fix red on this exact value.
        let t = WorldTime::from_std_days(-3.0000001).expect("finite, in range");
        assert_eq!(
            t.ticks(),
            -300_000,
            "the f64 day rounds onto a tick boundary"
        );

        let whole = t.whole_days();
        let tick_of_day = t.tick_of_day();
        assert_eq!(
            whole, -3,
            "ticks()-derived, not floor(day)-derived -- floor(-3.0000001) is -4"
        );
        assert_eq!(tick_of_day, 0);
        assert_eq!(
            whole * WorldTime::TICKS_PER_STD_DAY + tick_of_day,
            t.ticks(),
            "whole_days and tick_of_day must reconstruct the exact tick count"
        );
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

    /// Ruling 11: `from_ticks` is infallible and accepts any `i64`, so
    /// `from_ticks(i64::MIN) - from_ticks(i64::MAX)` is constructible and its
    /// true difference does not fit in an `i64` span. Unchecked, this wraps
    /// silently in release; checked, it panics loudly in both profiles.
    #[test]
    #[should_panic(expected = "WorldTime subtraction overflowed i64 ticks")]
    fn subtracting_instants_panics_rather_than_wrapping_at_the_i64_boundary() {
        let _ = WorldTime::from_ticks(i64::MIN) - WorldTime::from_ticks(i64::MAX);
    }

    /// The `Add` twin of the above: an instant already at `i64::MAX` plus any
    /// positive span cannot be represented, and must panic rather than wrap
    /// to a negative instant.
    #[test]
    #[should_panic(expected = "WorldTime + TickSpan overflowed i64 ticks")]
    fn adding_a_span_panics_rather_than_wrapping_at_the_i64_boundary() {
        let _ = WorldTime::from_ticks(i64::MAX) + crate::units::TickSpan::from_ticks(1);
    }

    /// An exact integer representation is what makes `Eq`/`Ord`/`Hash`
    /// derivable at all — an `f64` day has no total order — and four memo
    /// keys in `windows/vessel` spelled `day().to_bits()` for exactly this
    /// reason before the flip. They key on the instant itself now.
    #[test]
    fn world_time_is_ordered_and_equatable_so_it_can_key_a_map() {
        let mut m: std::collections::BTreeMap<WorldTime, &str> = std::collections::BTreeMap::new();
        m.insert(WorldTime::from_ticks(2), "later");
        m.insert(WorldTime::from_ticks(1), "earlier");
        m.insert(WorldTime::from_ticks(-1), "before genesis");
        let order: Vec<&str> = m.values().copied().collect();
        assert_eq!(order, vec!["before genesis", "earlier", "later"]);

        // The identity the `.to_bits()` keys were reaching for: two instants
        // built by different routes are the SAME key when they are the same
        // tick.
        assert_eq!(
            WorldTime::from_std_days(12.25).expect("finite"),
            WorldTime::from_ticks(1_225_000)
        );
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
        // The save-format epoch (decision 0186): the wire value is the exact
        // integer tick count, not a fractional day. 12.25 days is 1,225,000
        // ticks and serializes as that integer.
        let t = WorldTime::from_std_days(12.25).expect("finite");
        let json = serde_json::to_string(&t).unwrap();
        assert_eq!(
            json, "1225000",
            "WorldTime must serialize as the bare tick scalar, not a \
             {{\"ticks\":...}} object — #[serde(transparent)] is what keeps it that way"
        );
    }
}
