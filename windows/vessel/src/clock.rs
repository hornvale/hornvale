//! The ACTION CLOCK: what an action costs, in exact integer ticks.
//!
//! Scheduling is integer, the same reason `kernel/src/astar.rs` uses integer
//! costs: an exact total order.
//!
//! **This clock IS the kernel's clock now** (The Foliot). It was not, and the
//! history is worth keeping because the reason was subtle. The Escapement
//! (decision 0186) retyped `WorldTime` to exact ticks at the same
//! 100,000/std-day rate this module declared, but a world's day length was a
//! drawn `f64`, so a local day was an exact integer of VESSEL ticks and
//! `d * 100_000` KERNEL ticks — not an integer. The two lattices therefore
//! differed by up to half a tick per day, and every charge crossed between
//! them through `f64` days.
//!
//! That crossing was CORRECT, not a bug: the campaign that came to delete it
//! measured 213 losses against 211 gains, net -2 ticks, over 5,764 samples —
//! symmetric noise with nothing accumulating. Deleting it as an identity, as
//! that campaign first intended, would have introduced an error.
//!
//! What actually fixed it was removing the REASON for two lattices: a world's
//! day is now drawn as an exact tick count, so a local day divides the kernel
//! lattice exactly, a vessel tick IS a kernel tick, and there is nothing left
//! to convert. `Ticks` and `days_of` are gone rather than renamed — with one
//! lattice there is one tick concept and no second name for it.

use crate::action::Action;
use hornvale_kernel::component::ComponentStore;
use hornvale_kernel::room::Facet;
use hornvale_kernel::units::TickSpan;
use hornvale_kernel::{KindId, WorldTime};
use hornvale_species::BiosphereTraits;

/// The base resolution: ticks per STANDARD day.
///
/// **This is the kernel's constant, not a second copy of it** (The Foliot).
/// It used to be an independent literal that happened to agree, which is
/// what let two tick lattices exist side by side; re-exporting the kernel's
/// makes the agreement structural. `100_000` puts a tick at ~0.86 seconds,
/// which is what makes a within-room step (seconds long) REPRESENTABLE — at
/// `1_000` a tick is ~86 seconds and the fine layer could not be expressed at
/// all (spec §3.2). An Earth-like world therefore has `10_000` ticks per
/// `MoveTo`, the historical `MOVE_DURATION` of `0.1` days.
/// type-audit: bare-ok(count)
pub const BASE_TICKS_PER_STD_DAY: i64 = WorldTime::TICKS_PER_STD_DAY;

/// How many ticks make one LOCAL day.
///
/// **A field read now, not a computation** (The Foliot). It used to round
/// `day_length_std * base` out of an `f64`, which is what created a second
/// lattice: the rounded value and the true `d * base` differed by up to half
/// a tick, so a vessel tick was not quite a kernel tick and every action had
/// to convert between them. A world's day is now drawn as an exact tick count
/// (`Rotation::Spinning`), so this just reports it.
///
/// A whole local day being an EXACT integer of ticks is what `ActivityCycle`
/// needs (spec §4.1): under an arbitrary granularity every dawn rounds and the
/// error beats against the day cycle over a long run. `None` — a tidally
/// locked world, which the rotation pin admits — has no day to divide, so it
/// takes the base rate.
/// type-audit: bare-ok(count: return)
pub fn ticks_per_local_day(day: Option<TickSpan>) -> i64 {
    match day.filter(|d| d.ticks() > 0) {
        Some(d) => d.ticks(),
        None => BASE_TICKS_PER_STD_DAY,
    }
}

/// The mass at which `tempo` is exactly `1.0` — a human-scale creature.
/// Authored.
/// type-audit: bare-ok(ratio)
pub const REFERENCE_MASS_KG: f64 = 70.0;

/// The authored biosphere roster's shape, as `liveness.rs` already holds it:
/// `hornvale_species::biosphere_registry()` returns exactly this. Named here so
/// [`mass_for_species`] can borrow one without this module learning how to
/// build one — the registry is a free function over authored data, so nothing
/// about it requires a world and `clock` stays unit-testable.
pub type Biosphere = ComponentStore<KindId, BiosphereTraits>;

/// A body's mass in kilograms — THE ONE DERIVATION, shared by every body.
///
/// [`cost_ticks`] already charges time as a function of the body with no driver
/// parameter, so a possessed body and a creature pay the same tariff exactly
/// when they read their mass the same way. That was previously two byte-
/// identical inline lookups in `liveness.rs` and nothing at all for the
/// player; it is this function now.
///
/// Falls back to [`REFERENCE_MASS_KG`] when the biosphere is absent **or** the
/// species is not in it. [`tempo`] clamps a nonsense value anyway, but the
/// fallback is stated rather than left implicit, so a defaulted body reads at
/// exactly tempo `1.0`.
/// type-audit: bare-ok(identifier-text: species), bare-ok(ratio: return)
pub fn mass_for_species(species: &str, biosphere: Option<&Biosphere>) -> f64 {
    biosphere
        .and_then(|b| b.get_by_label(species))
        .map(|t| t.mass.kilograms())
        .unwrap_or(REFERENCE_MASS_KG)
}

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

// `days_of` is DELETED (The Foliot). It converted a scheduler tick count into
// continuous standard days so a charge could re-enter the lattice through
// `WorldTime::from_std_days`. With the day lattice-aligned that conversion is
// the identity, so the function would be a no-op wearing the costume of a
// unit conversion — the most misleading thing it could be. A caller that
// genuinely wants continuous days converts through the kernel's own named
// hatch at its own call site, where the crossing is visible.

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
/// creature dials replacing the single historical `MOVE_DURATION`; none of
/// those five is zero, so the cost model is TOTAL for every act a creature
/// can plan (spec §2 rung 1). `Rest` keeps its jump-to-waking elsewhere —
/// this is only the cost of the act of lying down.
///
/// Group A's seven operator instruments (The Deed) are the deliberate
/// exception: an out-of-character act "charges nothing by default" (spec
/// Arc I.b §3.4), so their dial is `TickSpan::from_ticks(0)`. This is inert today — nothing
/// routes a group-A `Action` through [`cost_ticks`], dispatch stays
/// string-based for them — but the match must still be exhaustive, and
/// `0` is the honest answer for what they *would* cost if ever charged.
///
/// The match is exhaustive by variant deliberately, the same discipline
/// `action::precondition_reads_committed_state` keeps: a new `Action` must
/// fail to compile here rather than silently become free.
pub fn base_cost(action: &Action) -> TickSpan {
    match action {
        // 10_000 ticks = 0.1 days on an Earth-like world: today's MOVE_DURATION.
        //
        // FLAT, and deliberately so: this is the cost of ONE ORTHOGONAL step,
        // the unit the rest of the model is expressed in. It is not the cost of
        // a move — an `Action::MoveTo(Facet)` names a destination and cannot
        // know whether reaching it was a diagonal, and on an 8-connected lattice
        // a flat charge is a ~41% travel-speed exploit (a diagonal buys √2 the
        // ground for the same time; measured 1.411786 on the real walk-depth
        // lattice, so 41.18%). `cost_of`'s `step_factor` is where that closes;
        // see [`DIAGONAL_STEP_FACTOR`] for the figure and the mechanism.
        Action::MoveTo(_) => TickSpan::from_ticks(10_000),
        // A step WITHIN a room (The Threshold): a tenth of a room-to-room move,
        // which is the ratio that campaign authored for it (`MOVE_DURATION /
        // 10.0`), carried over exactly. Crossing a room is at most eight
        // anchor-hops, against the mesh-scale distances a between-room walk
        // covers, so it should cost a proportionally smaller slice of a `wait`.
        // Held here rather than as its own `f64` constant so it scales with body
        // mass like every other act — a bear crosses a room more slowly than a
        // person does — and so the two movement scales stay comparable by
        // construction as either is retuned.
        Action::MoveWithin(_) => TickSpan::from_ticks(1_000),
        // A drink is quick — a couple of minutes.
        Action::Drink => TickSpan::from_ticks(150),
        // A meal is not — the better part of an hour.
        Action::Eat => TickSpan::from_ticks(3_000),
        // Lying DOWN is quick; the sleep itself is the jump-to-waking, not this.
        Action::Rest => TickSpan::from_ticks(150),
        // Group A: operator instruments charge nothing by default (spec
        // §3.4) — see the doc above.
        Action::Why
        | Action::Npcs
        | Action::Help
        | Action::Eyes
        | Action::Whoami
        | Action::Provoke
        | Action::Soothe => TickSpan::from_ticks(0),
        // Group B's objective halves (The Deed, Task 6) charge nothing for
        // the same reason, `!wait` INCLUDED — and that last one is worth a
        // sentence, because spec §3.4 calls `!wait` "the exception that moves
        // the clock" and this arm looks like it contradicts that.
        //
        // It does not. `Session::wait` advances the day by the SPAN THE
        // PLAYER ASKED FOR (`wait 3` is three days, under either mood), which
        // is the act's effect and is parameterised by its argument.
        // `base_ticks` answers a different question — what an act costs the
        // body that performs it, keyed on `Action` alone with nowhere to put
        // a span — and the honest answer for an out-of-character act is
        // still zero. Charging a constant here would add a second, silent
        // clock movement on top of the one the player named.
        Action::ObjectiveMap
        | Action::ObjectiveExamine
        | Action::ObjectiveNeeds
        | Action::ObjectiveWait
        | Action::ObjectiveLook
        | Action::ObjectiveKnows => TickSpan::from_ticks(0),
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

/// How many of a room's [`Facet::neighbors`] are EDGE-adjacent: the pinned
/// prefix `[..4]`.
///
/// The kernel's [`Facet::neighbor_steps`] states the invariant this rests on —
/// the first four steps are the four edge steps and everything from index 4 on
/// shares only a single corner, and the step a cube corner drops is always a
/// diagonal, so the prefix survives the short arity — and
/// `the_first_four_neighbours_are_always_the_four_edge_neighbours` (in
/// `kernel/tests/suite/cube_adjacency.rs`) pins it. Named here rather than
/// spelled `4` at the one site that indexes, so the reliance is greppable.
/// type-audit: bare-ok(count)
const EDGE_ADJACENT_NEIGHBOURS: usize = 4;

/// What a DIAGONAL walk-band step costs relative to an edge step: `√2`,
/// because it covers `√2` times the ground.
///
/// # The 41% exploit this closes
///
/// [`base_cost`] prices `Action::MoveTo` at a flat 10,000 ticks whatever room
/// it leads to. The walk band is 8-connected since The Pavement, so on a flat
/// cost a diagonal step buys `√2 ≈ 1.414` times the ground for the same time:
/// **a creature or a player travels ~41% faster by zigzagging than by walking
/// straight**, which is a physics falsehood rather than a preference. Charging
/// this factor on the diagonal steps is the whole fix, and the figure is
/// recorded here because the campaign's H2 asserts against it: with the
/// multiplier forced to `1.0` the ~41% gap must reappear
/// (`windows/vessel/tests/suite/octile_cost.rs`, the positive control).
///
/// **`√2` is the ideal, and the real lattice agrees with it to 0.17%** —
/// measured rather than assumed, because the cube-sphere's tangent warp
/// distorts a quad and there was no reason a priori for the diagonal to sit at
/// exactly `√2` edges. Over 4,000 interior rooms at walk depth spread across
/// all six faces
/// (`the_diagonal_is_root_two_edges_on_the_lattice_we_actually_walk`), the
/// mean diagonal centroid separation is **1.411786** edge separations
/// (`-0.172%` against `√2`), spread **1.366086–1.434180** per room. So the
/// exploit was **41.18%** on the ground the project actually walks, against
/// the ideal 41.42%.
///
/// Not a mean over all eight neighbours: the two groups are averaged
/// SEPARATELY. That is Addendum 2's trap — the deleted `course.rs`'s
/// `step_length_rad` divided by `ns.len()` and so returned ~1.21 edge steps
/// once the mesh went 8-connected, a number that looks like one step and is
/// not.
/// type-audit: bare-ok(ratio)
pub const DIAGONAL_STEP_FACTOR: f64 = std::f64::consts::SQRT_2;

/// The geometry multiplier for one walk-band step: [`DIAGONAL_STEP_FACTOR`]
/// when `to` is a diagonal (corner-adjacent) neighbour of `from`, and `1.0`
/// when it is edge-adjacent.
///
/// **A pair that is not a step at all charges the orthogonal unit, and says so
/// loudly in a debug build.** Every caller passes a real step (both
/// `Session::charge`'s `MoveTo` and the creature walk's read the destination
/// out of the action they are charging), so the arm is unreachable in
/// practice — but "unreachable in practice" is an argument, and this project
/// fails loudly rather than resting on one. The `debug_assert!` turns the
/// argument into something a test run enforces; the release fallback stays
/// `1.0` because a mid-walk panic is worse than an under-charge, and because
/// returning `1.0` keeps this function total, which is what lets it be called
/// from a match arm without an `expect`.
///
/// Geometry only — no world, no ledger, no terrain. [`Facet::neighbors`] is
/// integer lattice arithmetic, so this stays inside the module's own rule that
/// `clock` is testable without building a world.
/// type-audit: bare-ok(ratio: return)
pub fn step_factor(from: &Facet, to: &Facet) -> f64 {
    match from.neighbors().iter().position(|n| n == to) {
        Some(i) if i >= EDGE_ADJACENT_NEIGHBOURS => DIAGONAL_STEP_FACTOR,
        Some(_) => 1.0,
        None => {
            debug_assert!(
                from == to,
                "step_factor asked to price a step between rooms that do not \
                 touch: {from:?} -> {to:?}. Every caller reads the destination \
                 out of the `MoveTo` it is charging, so this means a caller \
                 has begun charging a move the mesh does not admit — the \
                 orthogonal fallback below would under-charge it silently."
            );
            1.0
        }
    }
}

/// What `action` costs a creature of `mass_kg` over ground of
/// `terrain_factor` (`1.0` for level or non-move actions) taking a step of
/// `step_factor` ([`step_factor`]; `1.0` for an edge step or a non-move
/// action), rounded to an exact tick count and never zero — a free action
/// would let a creature act unboundedly at one instant.
///
/// # Why the step geometry is its OWN parameter
///
/// `Action::MoveTo(Facet)` names a destination and cannot know whether
/// reaching it was a diagonal, so the factor has to arrive from outside. It
/// arrives as a fourth argument named for what it is rather than folded into
/// `terrain_factor`, because ground difficulty and step geometry vary
/// independently and one number cannot carry both — the defect decision 0143
/// exists to prevent, one ladder over. Considered and rejected:
/// `Action::MoveTo { to, diagonal: bool }`, which would put a geometry fact
/// inside a planner-facing enum that [`crate::action`]'s A* also constructs.
///
/// The planner has the same fix in integer form —
/// `action::ORTHOGONAL_STEP`/`action::DIAGONAL_STEP` — and the two must not
/// drift apart: `the_planner_and_the_clock_price_a_diagonal_alike` holds them
/// to the same 0.5% the campaign preregistered for H2.
/// type-audit: bare-ok(ratio: mass_kg), bare-ok(ratio: terrain_factor), bare-ok(ratio: step_factor)
pub fn cost_of(action: &Action, mass_kg: f64, terrain_factor: f64, step_factor: f64) -> TickSpan {
    let positive = |f: f64| if f.is_finite() && f > 0.0 { f } else { 1.0 };
    let scaled = base_cost(action).ticks() as f64
        * tempo(mass_kg)
        * positive(terrain_factor)
        * positive(step_factor);
    TickSpan::from_ticks((scaled.round() as i64).max(1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::action::Action;

    #[test]
    fn a_move_costs_exactly_todays_duration_on_an_earthlike_world() {
        // The bridge to today's behaviour: MOVE_DURATION was 0.1 days, and an
        // Earth-like rotation gives 100_000 ticks per local day, so a
        // reference-mass creature's move is 10_000 ticks = 0.1 days exactly.
        let mv = Action::MoveTo(Facet {
            face: 0,
            path: vec![0],
        });
        assert_eq!(base_cost(&mv), TickSpan::from_ticks(10_000));
        assert_eq!(
            cost_of(&mv, REFERENCE_MASS_KG, 1.0, 1.0),
            TickSpan::from_ticks(10_000)
        );
        // A cost IS a kernel span now, so its length in days is the span's
        // own continuous view (The Foliot) — there is no clock conversion in
        // between to check.
        assert_eq!(TickSpan::from_ticks(10_000).as_std_days(), 0.1);
    }

    #[test]
    fn the_local_day_is_an_exact_integer_number_of_ticks() {
        // THE REASON §4.1 WANTS IT, now true by construction rather than by
        // rounding (The Foliot). A whole local day must be a whole number of
        // ticks — otherwise every dawn rounds and the error beats against the
        // day cycle over a long run. The day arrives as a tick count, so this
        // asserts the report is faithful rather than that a rounding behaved.
        for ticks in [100_000_i64, 41_000, 270_000, 33_333, 1_725_000] {
            let d = TickSpan::from_ticks(ticks);
            assert_eq!(ticks_per_local_day(Some(d)), ticks);
            // And the continuous view round-trips exactly, which is what makes
            // the old `days_of` conversion unnecessary rather than merely
            // redundant.
            assert_eq!(
                (d.as_std_days() * BASE_TICKS_PER_STD_DAY as f64).round() as i64,
                ticks
            );
        }
    }

    #[test]
    fn the_tick_stays_approximately_absolute_across_worlds() {
        // The other half of §4.1: base costs are authored in TICKS, so a move
        // must mean the same absolute duration whatever the planet does — a
        // bear's gait is set by the bear, not by the sky.
        //
        // This used to allow a 0.1% spread, because the cost crossed the
        // local-tick lattice and back and the two rates differed slightly. It
        // is now EXACTLY equal on every world (The Foliot): a cost is a kernel
        // span and the planet never enters the arithmetic. Asserting equality
        // rather than a tolerance is the stronger claim the unification earned.
        let mv = Action::MoveTo(Facet {
            face: 0,
            path: vec![0],
        });
        let reference = cost_of(&mv, REFERENCE_MASS_KG, 1.0, 1.0);
        for ticks in [41_000_i64, 270_000, 1_725_000] {
            assert_eq!(
                ticks_per_local_day(Some(TickSpan::from_ticks(ticks))),
                ticks,
                "the day is reported as drawn"
            );
            assert_eq!(
                cost_of(&mv, REFERENCE_MASS_KG, 1.0, 1.0),
                reference,
                "a move costs the same span on every world"
            );
        }
    }

    #[test]
    fn a_tidally_locked_world_falls_back_to_the_base_rate() {
        // A world with no dawn is exactly the world a day-derived clock cannot
        // derive from. Stated, not unwrap_or'd (spec §4.1).
        assert_eq!(ticks_per_local_day(None), BASE_TICKS_PER_STD_DAY);
        assert_eq!(BASE_TICKS_PER_STD_DAY, 100_000);
        // And the base rate is the KERNEL's rate, not a second literal that
        // happens to agree — which is what let two lattices coexist.
        assert_eq!(BASE_TICKS_PER_STD_DAY, WorldTime::TICKS_PER_STD_DAY);
    }

    #[test]
    fn no_action_is_free() {
        // THE TOTALITY PROPERTY (spec §2 rung 1) — for CREATURE acts. Every
        // creature action costs something, so a future creature action
        // cannot silently be added for free. Narrowed from "every action"
        // (fix round 1, Finding 3): group A's seven operator instruments
        // (The Deed) are the deliberate exception — `base_ticks`'s own doc
        // states it plainly, "an out-of-character act charges nothing by
        // default" (spec §3.4) — so this property was never meant to hold
        // for them, and this list stays the hand-picked creature roster
        // rather than `Action::all()` so it cannot silently start failing
        // on an instrument this test was never about.
        let every = [
            Action::MoveTo(Facet {
                face: 0,
                path: vec![0],
            }),
            Action::Drink,
            Action::Rest,
            Action::Eat,
        ];
        for a in &every {
            assert!(
                base_cost(a).ticks() > 0,
                "{a:?} is free — every action must cost time"
            );
            assert!(
                cost_of(a, REFERENCE_MASS_KG, 1.0, 1.0).ticks() > 0,
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
        let mv = Action::MoveTo(Facet {
            face: 0,
            path: vec![0],
        });
        let level = cost_of(&mv, REFERENCE_MASS_KG, 1.0, 1.0);
        let steep = cost_of(&mv, REFERENCE_MASS_KG, climb_factor(0.0, 500.0), 1.0);
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
