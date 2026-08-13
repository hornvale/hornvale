//! The sub-cell micro-field: a few grounded per-room continuous axes drawn
//! from the room's address noise, so a walk through homogeneous biome still
//! varies room-to-room (the "miles and miles of forest" answer).
//!
//! **`wetness` is the one axis that consults the world** (The Rill, R-7/R-8).
//! The other three are address noise and nothing else. See
//! [`grounded_wetness`] for the model and [`LOCAL_VARIATION`] for the one
//! constant it carries.

use crate::regime::MicroField;
use crate::streams::LOCALE_MICRO;
use hornvale_climate::{BiomeExpr, Formation, Medium};
use hornvale_kernel::{Seed, quantize};
use hornvale_terrain::branch::RillReading;

/// Whether a room's wetness axis is grounded in the world at all: bare ground
/// under open air, and not permanent ice.
///
/// **The one copy of this predicate.** At sea the same axis is the set of the
/// current, on ice it is snow cover, and in the rock column it is seep, and a
/// river's proximity governs none of those — so those rooms keep the address
/// draw unchanged. It is `pub` because a *measurement* of the wetness axis has
/// to score the same population the grounding writes to; a second copy of this
/// test is exactly how an emitted arm and a grounded arm quietly stop
/// describing the same rooms.
///
/// **There is, in fact, a second expression of this partition, and it is the
/// failure mode above.** `grammar::micro_habitat`
/// (`grammar.rs:95-104`) matches independently on `Medium::AirOverRock` and
/// `Formation::Ice` to choose between the land, ice, water and rock clause
/// sets, rather than calling this. The two agree **exactly today** — that was
/// verified, so this is drift risk and not a live divergence — and collapsing
/// them was deliberately not done at The Rill's close, because it is a
/// behaviour change and the close is not where an unverified one ships. If you
/// change either arm, change both, and prefer making the renderer call this.
/// type-audit: bare-ok(flag: return)
pub fn wetness_is_grounded(expr: BiomeExpr) -> bool {
    expr.realm.medium == Medium::AirOverRock && expr.formation != Formation::Ice
}

/// How much of the wetness axis the room's own address draw may still move,
/// as a share of the headroom the grounded value leaves.
///
/// The grammar's clause thresholds sit at ±0.33 (`grammar::land_micro_habitat`),
/// so a tenth cannot carry a room from the middle of one clause band into a
/// neighbouring one: the draw **varies** a room, it does not **decide** it.
/// Fixed before R-7 was measured, and unmoved after.
const LOCAL_VARIATION: f64 = 0.1;

/// The wetness a room's ground is grounded at, before local variation: a
/// **budget and an allocation**.
///
/// The budget is the climate supply the room's cells receive — `moisture`,
/// the same blended number the room's own document emits. The allocation is
/// where the room sits relative to its local watercourse: at the channel the
/// ground gets everything the supply left unclaimed, at the dry edge of the
/// valley it gets only the supply, and beyond the valley — or where the coarse
/// graph gives the cell no outflow at all, so there is no watercourse to be
/// near — it gets only the supply as well.
///
/// So a wet cell reads wet everywhere and a dry cell reads dry except along
/// its rivers, which is the riparian corridor a desert actually has.
///
/// `moisture` is deliberately the **already-quantized** blend the document
/// carries. The reason is *not* the one `height_asl_m` gives one screen away in
/// `describe_with_weights` — that precedent rests on a consumer re-deriving the
/// value from the emitted document, and no consumer can re-derive this one:
/// [`RillReading`]'s `distance` and `band_edges` are never serialized, so the
/// allocation's **inputs** are unavailable outside the process.
///
/// Read that as the narrow claim it is. The inputs are unserialized; the
/// **result is not**. This function's return reaches `micro.wetness`, which is
/// emitted — The Rill moved that leaf in the gallery, in three vessel
/// snapshots and in two game-core fixtures. So nothing upstream of here is
/// free to change: a reordering inside
/// `hornvale_terrain::branch::rill_reading`, or an index over its
/// nearest-branch search, silently rewrites committed worlds even though no
/// field it touches is ever written down. Its tie-break is documented as a
/// contract at its own definition for exactly this reason. The reason here is
/// that
/// the choice is **determinism-neutral and precision-immaterial**: the blend is
/// quantized at emit either way, so taking it is one fewer recomputation of the
/// same three-corner mean rather than a second, differently-rounded copy of it,
/// and the model is a bounded monotone map with no chaotic amplification, so
/// eight significant digits of supply cannot move the emitted axis by more than
/// its own quantization.
/// type-audit: bare-ok(ratio: moisture), bare-ok(ratio: return)
pub fn grounded_wetness(moisture: f64, rill: Option<RillReading>) -> f64 {
    let supply = moisture.clamp(0.0, 1.0);
    let proximity = match rill {
        None => 0.0,
        Some(r) => {
            // `band_edges[0]` is the channel/bank border and `[3]` the
            // terrace/dry one — the valley's own width, in the same angular
            // units the distance is measured in, so no length scale is
            // invented here.
            let (near, far) = (r.band_edges[0], r.band_edges[3]);
            if r.distance <= near {
                1.0
            } else if r.distance >= far || far <= near {
                0.0
            } else {
                1.0 - (r.distance - near) / (far - near)
            }
        }
    };
    2.0 * (supply + (1.0 - supply) * proximity) - 1.0
}

/// Four axes in [-1, 1], each a distinct sub-stream of the room's micro label,
/// quantized at emit (platform-exact).
///
/// `grounded` is [`grounded_wetness`] where the room is bare ground under open
/// air, and `None` everywhere else — at sea, on ice and in the rock column the
/// axis is read as current, snow cover and seep, none of which a river's
/// proximity governs, so those rooms keep the address draw unchanged.
///
/// **All four draws are spent, in order, either way.** Wetness is the third;
/// a grounding that stopped consuming its draw would shift `openness` in every
/// room of every world, which is a save-format break and not a style choice.
pub(crate) fn micro_field(room_seed: Seed, grounded: Option<f64>) -> MicroField {
    let mut s = room_seed.derive(LOCALE_MICRO).stream();
    let mut axis = || s.next_f64() * 2.0 - 1.0;
    let relief = axis();
    let aspect = axis();
    let draw = axis();
    let openness = axis();
    let wetness = match grounded {
        None => draw,
        // The draw spends itself inside the headroom the grounded value
        // leaves, so a room already at either extreme — a channel bed, a
        // terrace beyond a desert's last river — has none to spend.
        Some(g) => (g + LOCAL_VARIATION * draw * (1.0 - g.abs())).clamp(-1.0, 1.0),
    };
    MicroField {
        relief: quantize(relief),
        aspect: quantize(aspect),
        wetness: quantize(wetness),
        openness: quantize(openness),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellId, RoomAddr, Seed};

    fn seed_for(path: Vec<u8>) -> Seed {
        RoomAddr { face: 3, path }.seed(Seed(42))
    }

    fn reading(distance: f64) -> RillReading {
        RillReading {
            distance,
            band_edges: [0.01, 0.02, 0.03, 0.04],
            catchment: 1e-4,
            cell: CellId(0),
        }
    }

    #[test]
    fn micro_field_is_deterministic_and_in_range() {
        let s = seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]);
        let a = micro_field(s, None);
        let b = micro_field(s, None);
        assert_eq!(a, b);
        for v in [a.relief, a.aspect, a.wetness, a.openness] {
            assert!((-1.0..=1.0).contains(&v));
        }
    }

    #[test]
    fn a_grounded_field_is_deterministic_and_in_range() {
        let s = seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]);
        let g = grounded_wetness(0.4, Some(reading(0.015)));
        let a = micro_field(s, Some(g));
        assert_eq!(a, micro_field(s, Some(g)));
        assert!((-1.0..=1.0).contains(&a.wetness));
    }

    #[test]
    fn sibling_rooms_differ() {
        let a = micro_field(seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 0]), None);
        let b = micro_field(seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1]), None);
        assert_ne!(a, b);
    }

    /// The other three axes are the same numbers whether or not wetness is
    /// grounded — the draw order is untouched.
    #[test]
    fn grounding_wetness_moves_no_other_axis() {
        let s = seed_for(vec![1, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]);
        let plain = micro_field(s, None);
        let grounded = micro_field(s, Some(0.5));
        assert_eq!(plain.relief, grounded.relief);
        assert_eq!(plain.aspect, grounded.aspect);
        assert_eq!(plain.openness, grounded.openness);
        assert_ne!(plain.wetness, grounded.wetness);
    }

    /// The allocation: at the channel the ground reads wet whatever the
    /// climate supplies; beyond the valley it reads exactly the supply.
    #[test]
    fn the_channel_wets_ground_the_climate_does_not() {
        let dry_climate = 0.1;
        let in_channel = grounded_wetness(dry_climate, Some(reading(0.005)));
        let on_the_terrace_edge = grounded_wetness(dry_climate, Some(reading(0.04)));
        let no_river = grounded_wetness(dry_climate, None);
        assert_eq!(in_channel, 1.0);
        assert_eq!(on_the_terrace_edge, 2.0 * dry_climate - 1.0);
        assert_eq!(no_river, 2.0 * dry_climate - 1.0);
        assert!(in_channel > on_the_terrace_edge);
    }

    /// The budget: with the watercourse held fixed, more climate moisture is
    /// never less wetness.
    #[test]
    fn more_supply_is_never_less_wetness() {
        let mut prev = f64::NEG_INFINITY;
        for i in 0..=10 {
            let g = grounded_wetness(i as f64 / 10.0, Some(reading(0.025)));
            assert!(g >= prev, "supply {i}/10 lowered the grounded wetness");
            prev = g;
        }
    }

    /// The allocation is monotone the other way too: nearer the watercourse is
    /// never drier.
    #[test]
    fn nearer_the_watercourse_is_never_drier() {
        let mut prev = f64::INFINITY;
        for i in 0..=10 {
            let g = grounded_wetness(0.3, Some(reading(0.005 + 0.004 * i as f64)));
            assert!(g <= prev, "distance step {i} raised the grounded wetness");
            prev = g;
        }
    }
}
