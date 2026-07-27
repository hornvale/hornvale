//! The ACTION CLOCK: what an action costs, in exact integer ticks.
//!
//! Scheduling is integer and internal; committing is `f64` days and unchanged
//! (spec §4). `Ticks` is never serialized — it exists so the scheduler's
//! ordering is a total order with exact arithmetic, the same reason
//! `kernel/src/astar.rs` uses `u64` costs.

use crate::liveness::Action;

/// An exact count of scheduler ticks. Internal; never serialized.
/// type-audit: bare-ok(count)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ticks(pub u64);

/// The base resolution: ticks per STANDARD day, before the planet's rotation is
/// taken into account. `100_000` puts a tick at ~0.86 seconds, which is what
/// makes a within-room step (seconds long) REPRESENTABLE — at `1_000` a tick is
/// ~86 seconds and the fine layer could not be expressed at all (spec §3.2).
/// An Earth-like world therefore has `10_000` ticks per `MoveTo`, the historical
/// `MOVE_DURATION` of `0.1` days.
/// type-audit: bare-ok(count)
pub const BASE_TICKS_PER_STD_DAY: u64 = 100_000;

/// How many ticks make one LOCAL day on a world whose rotation period is
/// `day_length_std` standard days — `round(day × base)`, at least one.
///
/// Deriving this rather than fixing it is what makes a whole local day an
/// EXACT integer of ticks (spec §4.1). `ActivityCycle` is the sim's one
/// local-day-keyed mechanism, and under an arbitrary granularity every dawn
/// rounds to the nearest tick and the error beats against the day cycle over a
/// long run. `None` — a tidally-locked world, which the rotation pin admits —
/// has no day to divide, so it takes the base rate.
/// type-audit: bare-ok(ratio: day_length_std), bare-ok(count: return)
pub fn ticks_per_local_day(day_length_std: Option<f64>) -> u64 {
    match day_length_std.filter(|d| d.is_finite() && *d > 0.0) {
        Some(d) => ((d * BASE_TICKS_PER_STD_DAY as f64).round() as u64).max(1),
        None => BASE_TICKS_PER_STD_DAY,
    }
}

/// The mass at which `tempo` is exactly `1.0` — a human-scale creature.
/// Authored.
/// type-audit: bare-ok(ratio)
pub const REFERENCE_MASS_KG: f64 = 70.0;

/// The allometric exponent for biological TIMES (stride period, heart interval,
/// lifespan): roughly the quarter power of mass. Authored, and the same
/// allometry the species domain invokes for basal rate.
/// type-audit: bare-ok(ratio)
pub const TIME_EXPONENT: f64 = 0.25;

/// The mass band `tempo` clamps to, so a missing or absurd trait cannot produce
/// a zero or infinite cost mid-walk.
/// type-audit: bare-ok(ratio)
const MASS_BAND_KG: (f64, f64) = (0.001, 100_000.0);

/// The climb, in metres, that doubles a move's cost. Authored.
/// type-audit: bare-ok(ratio)
const CLIMB_SCALE_M: f64 = 500.0;

/// The ceiling on [`climb_factor`], so a cliff cannot stall a walk outright.
/// type-audit: bare-ok(ratio)
const MAX_CLIMB_FACTOR: f64 = 4.0;

/// `t` in STANDARD days — the conversion at the commit boundary, where floats
/// belong. A local day of `day_length_std` is exactly
/// [`ticks_per_local_day`] ticks, so this is that ratio scaled.
/// type-audit: bare-ok(ratio: day_length_std), bare-ok(ratio: return)
pub fn days_of(t: Ticks, day_length_std: Option<f64>) -> f64 {
    let per_day = ticks_per_local_day(day_length_std);
    match day_length_std.filter(|d| d.is_finite() && *d > 0.0) {
        Some(d) => t.0 as f64 * d / per_day as f64,
        None => t.0 as f64 / per_day as f64,
    }
}

/// How much slower than reference this creature acts: `(mass / reference) ^
/// TIME_EXPONENT`, clamped to [`MASS_BAND_KG`] and **quantized**.
///
/// The quantization is load-bearing, not hygiene (spec §3): `powf` routes to the
/// platform libm, whose last ULP differs, and this value immediately crosses a
/// rounding boundary into an integer tick count where one ULP could flip the
/// result. Quantizing to 8 significant digits first makes the boundary
/// reproducible across platforms.
/// type-audit: bare-ok(ratio: mass_kg), bare-ok(ratio: return)
pub fn tempo(mass_kg: f64) -> f64 {
    let m = if mass_kg.is_finite() {
        mass_kg.clamp(MASS_BAND_KG.0, MASS_BAND_KG.1)
    } else {
        REFERENCE_MASS_KG
    };
    hornvale_kernel::quantize::quantize(hornvale_kernel::math::powf(
        m / REFERENCE_MASS_KG,
        TIME_EXPONENT,
    ))
}

/// The authored base cost of each action, before the creature's tempo. Five
/// dials replacing the single historical `MOVE_DURATION`; none is zero, so the
/// cost model is TOTAL (spec §2 rung 1). `Rest` keeps its jump-to-waking
/// elsewhere — this is only the cost of the act of lying down.
///
/// The match is exhaustive by variant deliberately, the same discipline
/// `liveness::precondition_reads_committed_state` keeps: a new `Action` must
/// fail to compile here rather than silently become free.
pub fn base_ticks(action: &Action) -> Ticks {
    match action {
        // 10_000 ticks = 0.1 days on an Earth-like world: today's MOVE_DURATION.
        Action::MoveTo(_) => Ticks(10_000),
        // A step WITHIN a room (The Threshold): a tenth of a room-to-room move,
        // which is the ratio that campaign authored for it (`MOVE_DURATION /
        // 10.0`), carried over exactly. Crossing a room is at most eight
        // anchor-hops, against the mesh-scale distances a between-room walk
        // covers, so it should cost a proportionally smaller slice of a `wait`.
        // Held here rather than as its own `f64` constant so it scales with body
        // mass like every other act — a bear crosses a room more slowly than a
        // person does — and so the two movement scales stay comparable by
        // construction as either is retuned.
        Action::MoveWithin(_) => Ticks(1_000),
        // A drink is quick — a couple of minutes.
        Action::Drink => Ticks(150),
        // A meal is not — the better part of an hour.
        Action::Eat => Ticks(3_000),
        // Lying DOWN is quick; the sleep itself is the jump-to-waking, not this.
        Action::Rest => Ticks(150),
    }
}

/// The uphill penalty on a room-to-room move: `1 + climb / CLIMB_SCALE_M`,
/// clamped, and `1.0` whenever either elevation is non-finite
/// (`Terrain::elevation` returns `INFINITY` for an undescribable room, by its
/// own documented convention). Only UPHILL costs — a walking creature does not
/// descend meaningfully faster, and modelling that would be a dial earning
/// nothing.
///
/// Takes ELEVATIONS rather than rooms and a `&dyn Terrain` deliberately: this
/// module is pure arithmetic with no ledger or world access, which is what lets
/// it be tested without building a terrain. The caller does the two lookups.
/// type-audit: bare-ok(ratio: from_elev_m), bare-ok(ratio: to_elev_m), bare-ok(ratio: return)
pub fn climb_factor(from_elev_m: f64, to_elev_m: f64) -> f64 {
    if !from_elev_m.is_finite() || !to_elev_m.is_finite() {
        return 1.0;
    }
    let climb = (to_elev_m - from_elev_m).max(0.0);
    (1.0 + climb / CLIMB_SCALE_M).clamp(1.0, MAX_CLIMB_FACTOR)
}

/// What `action` costs a creature of `mass_kg` over ground of
/// `terrain_factor` (`1.0` for level or non-move actions), rounded to an exact
/// tick count and never zero — a free action would let a creature act
/// unboundedly at one instant.
/// type-audit: bare-ok(ratio: mass_kg), bare-ok(ratio: terrain_factor)
pub fn cost_ticks(action: &Action, mass_kg: f64, terrain_factor: f64) -> Ticks {
    let factor = if terrain_factor.is_finite() && terrain_factor > 0.0 {
        terrain_factor
    } else {
        1.0
    };
    let scaled = base_ticks(action).0 as f64 * tempo(mass_kg) * factor;
    Ticks((scaled.round() as u64).max(1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::liveness::Action;
    use hornvale_kernel::room::RoomAddr;

    #[test]
    fn a_move_costs_exactly_todays_duration_on_an_earthlike_world() {
        // The bridge to today's behaviour: MOVE_DURATION was 0.1 days, and an
        // Earth-like rotation gives 100_000 ticks per local day, so a
        // reference-mass creature's move is 10_000 ticks = 0.1 days exactly.
        let mv = Action::MoveTo(RoomAddr {
            face: 0,
            path: vec![0],
        });
        assert_eq!(base_ticks(&mv), Ticks(10_000));
        assert_eq!(cost_ticks(&mv, REFERENCE_MASS_KG, 1.0), Ticks(10_000));
        assert_eq!(days_of(Ticks(10_000), Some(1.0)), 0.1);
    }

    #[test]
    fn the_local_day_is_an_exact_integer_number_of_ticks() {
        // THE REASON THE RATE IS DERIVED (spec §4.1). Whatever the rotation, a
        // whole local day must be a whole number of ticks — otherwise every
        // dawn rounds and the error beats against the day cycle over a long run.
        for d in [1.0_f64, 0.41, 2.7, 1.0 / 3.0, 17.25] {
            let n = ticks_per_local_day(Some(d));
            assert!(n >= 1, "day {d} gives {n} ticks");
            // A day is exactly `n` ticks by construction, so converting them
            // back lands on the day length itself.
            let round_trip = days_of(Ticks(n), Some(d));
            assert!(
                (round_trip - d).abs() < 1e-12,
                "day {d}: {n} ticks converts back to {round_trip}, not {d}"
            );
        }
    }

    #[test]
    fn the_tick_stays_approximately_absolute_across_worlds() {
        // The other half of §4.1: base costs are authored in TICKS, so a move
        // must mean the same absolute duration whatever the planet does — a
        // bear's gait is set by the bear, not by the sky. Under 0.1% spread.
        let mv = Action::MoveTo(RoomAddr {
            face: 0,
            path: vec![0],
        });
        let reference = days_of(cost_ticks(&mv, REFERENCE_MASS_KG, 1.0), Some(1.0));
        for d in [0.41_f64, 2.7, 17.25] {
            let here = days_of(cost_ticks(&mv, REFERENCE_MASS_KG, 1.0), Some(d));
            assert!(
                ((here - reference) / reference).abs() < 0.001,
                "a move takes {here} days on a {d}-day world vs {reference} on Earth"
            );
        }
    }

    #[test]
    fn a_tidally_locked_world_falls_back_to_the_base_rate() {
        // A world with no dawn is exactly the world a day-derived clock cannot
        // derive from. Stated, not unwrap_or'd (spec §4.1).
        assert_eq!(ticks_per_local_day(None), BASE_TICKS_PER_STD_DAY);
        assert_eq!(BASE_TICKS_PER_STD_DAY, 100_000);
        assert_eq!(days_of(Ticks(100_000), None), 1.0);
    }

    #[test]
    fn no_action_is_free() {
        // THE TOTALITY PROPERTY (spec §2 rung 1). Every action costs something,
        // so a future action cannot silently be added for free.
        let every = [
            Action::MoveTo(RoomAddr {
                face: 0,
                path: vec![0],
            }),
            Action::Drink,
            Action::Rest,
            Action::Eat,
        ];
        for a in &every {
            assert!(
                base_ticks(a).0 > 0,
                "{a:?} is free — every action must cost time"
            );
            assert!(
                cost_ticks(a, REFERENCE_MASS_KG, 1.0).0 > 0,
                "{a:?} costs nothing"
            );
        }
    }

    #[test]
    fn tempo_is_monotone_in_mass_and_unity_at_reference() {
        assert_eq!(tempo(REFERENCE_MASS_KG), 1.0);
        let (mouse, bear) = (0.02_f64, 400.0_f64);
        assert!(tempo(mouse) < 1.0, "a mouse acts faster than a human");
        assert!(tempo(bear) > 1.0, "a bear acts slower than a human");
        assert!(
            tempo(mouse) < tempo(1.0) && tempo(1.0) < tempo(bear),
            "monotone"
        );
        // The quarter power is a GENTLE spread: 20000x mass is ~12x time, not
        // 20000x. A creature must not be pinned in place by being large.
        assert!(
            tempo(bear) / tempo(mouse) < 20.0,
            "the allometric spread is gentle: {} vs {}",
            tempo(mouse),
            tempo(bear)
        );
    }

    #[test]
    fn tempo_is_quantized_so_the_rounding_boundary_is_reproducible() {
        // THE DETERMINISM RULE (spec §3). `powf` is a libm transcendental whose
        // last ULP differs across platforms, and its result immediately crosses
        // a rounding boundary into an integer. Quantizing first makes the
        // boundary reproducible — so `tempo` must return an already-quantized
        // value, i.e. quantizing it again is a no-op.
        for m in [0.02_f64, 1.0, 12.5, 70.0, 400.0, 6000.0] {
            let t = tempo(m);
            assert_eq!(
                hornvale_kernel::quantize::quantize(t),
                t,
                "tempo({m}) is not already quantized"
            );
        }
    }

    #[test]
    fn climbing_costs_more_and_descending_costs_the_same() {
        // The macro cost function's other half (spec §3.1). Uphill is slower;
        // downhill is NOT faster (a walking creature does not gain by descending,
        // and modelling that would be a dial earning nothing); an undescribable
        // room (elevation INFINITY, Terrain's documented convention) is neutral.
        assert_eq!(
            climb_factor(100.0, 100.0),
            1.0,
            "level ground is unmodified"
        );
        assert!(climb_factor(100.0, 600.0) > 1.0, "uphill costs more");
        assert_eq!(climb_factor(600.0, 100.0), 1.0, "downhill is not faster");
        assert_eq!(
            climb_factor(f64::INFINITY, 0.0),
            1.0,
            "undescribable is neutral"
        );
        assert_eq!(climb_factor(0.0, f64::INFINITY), 1.0);
        // Bounded: a cliff must not stall a walk outright.
        assert!(climb_factor(0.0, 1.0e9) <= MAX_CLIMB_FACTOR);
        // And it reaches the cost model.
        let mv = Action::MoveTo(RoomAddr {
            face: 0,
            path: vec![0],
        });
        let level = cost_ticks(&mv, REFERENCE_MASS_KG, 1.0);
        let steep = cost_ticks(&mv, REFERENCE_MASS_KG, climb_factor(0.0, 500.0));
        assert!(
            steep > level,
            "the climb reaches the cost: {steep:?} vs {level:?}"
        );
    }

    #[test]
    fn a_nonsense_mass_falls_back_to_reference_rather_than_exploding() {
        // Fail loudly is the rule for pins, but a missing/absurd mass trait must
        // not produce a zero or infinite cost mid-walk. Clamp to the authored
        // band and document it.
        for bad in [0.0_f64, -5.0, f64::NAN, f64::INFINITY] {
            let t = tempo(bad);
            assert!(t.is_finite() && t > 0.0, "tempo({bad}) = {t}");
        }
    }
}
