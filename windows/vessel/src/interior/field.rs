//! Anchor-emitted FIELDS — what a hearth gives off, read where you stand. The
//! shape is `liveness::alarm_field`'s: a quantity summed over emitters and read
//! at a position, sparse and order-independent.
//!
//! DECAY IS OVER GRAPH DISTANCE, never euclidean (spec §2.1) — there is no
//! euclidean distance in this model to decay over, and introducing one would put
//! a metric on an outcome path.

use super::anchor::{AnchorId, AnchorKind, Interior};
use super::route::route_within;

/// The warmth a hearth emits at its own anchor, in °C, read with ZERO decay
/// (`warmth_at` applies `WARMTH_DECAY.powi(0) == 1` at the emitter itself) —
/// so this is not a whole-room average, it is the boost felt standing right
/// at the fire, relative to the same room unlit. Byte-identity is structural
/// regardless of this value's scale (an emitter-free room yields zero
/// everywhere, the additive-latent pattern), so changing it is safe for
/// determinism; it is NOT safe for the campaign's own frozen-constant rule
/// without the reasoning below.
///
/// **Provenance (The Threshold, task 5d):** originally an authored
/// placeholder of `1.0`. Task 5c measured a bit-identical null on seed 13's
/// cold-dominated population — `1.0` °C, decayed to `0.125` °C at a landing
/// anchor three hops from the hearth, is a rounding error against the
/// 40–80 °C real thermal deficits the qualifying rooms carry. The owner then
/// authorized calibrating this constant from physical reasoning, under the
/// rule that the argument and number be fixed and committed BEFORE
/// re-running the measurement (never adjusted to make a result move) — so
/// this value's provenance is a post-hoc calibration following a null
/// result, not a first-principles guess: a later reader is entitled to know
/// that history.
///
/// **The physical argument** (a Q/UA energy-balance estimate for a small
/// pre-modern single-room dwelling with an open hearth): a ≈65 m² envelope
/// at U≈3 W/(m²·K) plus draughty-infiltration losses at ≈4 air changes/hour
/// over ≈37 m³ gives a total heat-loss coefficient UA ≈ 245 W/K; a household
/// hearth delivering ≈1.5–2 kW net into the room (after open-smoke-hole
/// losses) yields a room-AVERAGE steady-state rise `ΔT = Q/UA ≈ 7 °C`. But
/// this constant is read at the fire itself, not the room average — radiant
/// flux and incomplete mixing concentrate the warming there by roughly
/// 2–3× the room average in experimental hearth reconstructions (the same
/// effect the preregistration is built on: occupants cluster AT the hearth
/// rather than being indifferent within an evenly-heated room), putting the
/// at-the-fire figure at ≈15–20 °C. `15.0` is chosen from the conservative
/// (low) end of that range. Full derivation:
/// `.superpowers/sdd/task-5d-report.md`.
/// type-audit: bare-ok(ratio)
pub const HEARTH_WARMTH: f64 = 15.0;

/// The multiplier per graph step away from an emitter.
/// type-audit: bare-ok(ratio)
pub const WARMTH_DECAY: f64 = 0.5;

/// The warmth at `at`: the sum over every hearth of its emission decayed by the
/// number of steps from it, `0.0` where no hearth is reachable. Deterministic —
/// the anchor order is the `Vec` order and the route is A*'s.
///
/// # Determinism
///
/// The decay is `powi`, an INTRINSIC — `clippy.toml` allows it exactly because
/// it is not one of the libm transcendentals that differ in the last ULP across
/// platforms, so the value is bit-identical everywhere. Accumulation is a sum
/// over `ids()` in ascending order, and the step count comes from the planner,
/// never from a coordinate.
///
/// type-audit: bare-ok(ratio: return), bare-ok(count: budget)
pub fn warmth_at(interior: &Interior, at: AnchorId, budget: usize) -> f64 {
    let mut total = 0.0_f64;
    for id in interior.ids() {
        if interior.anchor(id).kind != AnchorKind::Hearth {
            continue;
        }
        if let Some(path) = route_within(interior, at, id, budget) {
            total += HEARTH_WARMTH * WARMTH_DECAY.powi(path.len() as i32);
        }
    }
    total
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::{AnchorKind, Interior};

    #[test]
    fn warmth_falls_off_with_graph_distance_from_the_fire() {
        // hearth — hall — far. Warmth is greatest AT the fire, less one step
        // away, less again two steps away, and never negative.
        let mut i = Interior::new();
        let hearth = i.push(AnchorKind::Hearth, None);
        let hall = i.push(AnchorKind::Bed, None);
        let far = i.push(AnchorKind::Threshold, None);
        i.connect(hearth, hall);
        i.connect(hall, far);

        let w0 = warmth_at(&i, hearth, 64);
        let w1 = warmth_at(&i, hall, 64);
        let w2 = warmth_at(&i, far, 64);
        assert!(
            w0 > w1,
            "at the fire is warmer than one step away: {w0} vs {w1}"
        );
        assert!(w1 > w2, "one step is warmer than two: {w1} vs {w2}");
        assert!(w2 >= 0.0, "warmth is never negative");
    }

    #[test]
    fn a_room_with_no_fire_is_cold_everywhere() {
        // ADDITIVE-LATENT: with no emitter the field is zero, so a creature is
        // unchanged by construction — this is what makes byte-identity structural.
        let mut i = Interior::new();
        let a = i.push(AnchorKind::Bed, None);
        let b = i.push(AnchorKind::Threshold, None);
        i.connect(a, b);
        assert_eq!(warmth_at(&i, a, 64), 0.0);
        assert_eq!(warmth_at(&i, b, 64), 0.0);
    }

    #[test]
    fn warmth_sums_over_multiple_fires() {
        let mut i = Interior::new();
        let hall = i.push(AnchorKind::Bed, None);
        let f1 = i.push(AnchorKind::Hearth, None);
        let f2 = i.push(AnchorKind::Hearth, None);
        i.connect(hall, f1);
        i.connect(hall, f2);
        let mut one = Interior::new();
        let h = one.push(AnchorKind::Bed, None);
        let g = one.push(AnchorKind::Hearth, None);
        one.connect(h, g);
        assert!(
            warmth_at(&i, hall, 64) > warmth_at(&one, h, 64),
            "two fires warm a hall more than one"
        );
    }
}
