//! The light field: what light reaches a cell (The Lantern, spec §4).
//!
//! # Light needs no new geometry
//!
//! [`shadowcast`](crate::lattice::shadowcast) is **symmetric** between
//! passable cells, so "what can see this cell" and "what light reaches this
//! cell" are the same set. A source's reach is therefore the shipped field of
//! view at the source's radius, and this module adds no ray casting, no
//! second traversal and no third opinion about what a wall does. That is the
//! whole architectural claim of spec §4.1, and
//! `light_reaches_exactly_what_sight_reaches` in
//! `windows/vessel/tests/lantern_light.rs` states it on production lattices
//! rather than on a drawn box.
//!
//! # The sum is the kernel's declared law
//!
//! `hornvale_kernel::color`'s [`Mixture`](hornvale_kernel::color::Mixture)
//! doc names three ways colour combines and implements one, deferring this
//! campaign's by name: *"Additive (not implemented; arrives with
//! multi-light): two torches on one wall. **Sum the illuminants**, not the
//! reflectances."* [`light_field`] implements that sentence — precedent, not
//! invention.
//!
//! A max, an average and a last-writer-wins all produce lit rooms that look
//! entirely plausible, and none of them is the declared law. Only a positive
//! control separates them, which is what
//! `two_sources_are_strictly_brighter_than_either_alone` is for.
//!
//! # Attenuation is load-bearing, not cosmetic
//!
//! [`ATTENUATION`] is the *only* thing producing a light gradient in a
//! possessed chamber, and the reason is the symmetry above: the implicit
//! torch (spec §4.2) rides on the observer, so every cell you can see is lit
//! by your own torch **by construction**. Nothing else varies across a room a
//! few cells wide. See [`attenuate`] for why that forbids tuning it.
//!
//! # Nothing here is drawn, stored or serialized
//!
//! The field is a pure function of a lattice and its sources — no seed, no
//! `streams.rs` label, no epoch (spec §5). A window that draws has quietly
//! become a domain (`windows/CLAUDE.md`), and the lattice it reads is
//! `FRAME`-tier besides (decision 0069).

use crate::lattice::{Cell, Lattice, shadowcast};
use hornvale_kernel::color::{BANDS, Illuminant};
use std::collections::BTreeMap;

/// The colour temperature of a carried flame — a torch or a lamp. The
/// possession's implicit light (spec §4.2), and Nathan's call at G3: a
/// possession is assumed to be carrying one, so an explicit torch becomes a
/// refinement rather than a new mechanism.
/// type-audit: bare-ok(ratio)
pub const TORCH_KELVIN: f64 = 1900.0;

/// The colour temperature of a hearth fire — a bed of embers under flame,
/// cooler and redder than a torch, which is what makes a hearth-lit wall and
/// a doorway-lit wall different colours in the same room.
/// type-audit: bare-ok(ratio)
pub const HEARTH_KELVIN: f64 = 1200.0;

/// Something that emits light, at a cell.
///
/// A source is **placed**, never drawn: a hearth is at its chamber's wall, a
/// doorway is a `Threshold`, and the implicit torch is wherever the observer
/// is standing. Its spectrum is an [`Illuminant`] rather than a temperature
/// so that daylight — which is not a blackbody by the time it has crossed a
/// sky — can be a source on the same footing as a flame.
/// type-audit: bare-ok(count: radius)
#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    /// Where it burns.
    pub at: Cell,
    /// What it emits, at its own cell, before any distance falloff.
    pub illuminant: Illuminant,
    /// How far its light carries, in the **Chebyshev** metric
    /// [`shadowcast`](crate::lattice::shadowcast) bounds — the same radius a
    /// field of view takes, because it is the same call.
    pub radius: i32,
}

/// Chebyshev distance between two cells, as a ratio.
///
/// Chebyshev and not Euclidean because it is the metric
/// [`shadowcast`](crate::lattice::shadowcast) bounds its reach by: a source's
/// lit set is a square, and attenuating by a different metric than the one
/// that decided the set would make the outermost ring of a torch's reach
/// brighter along the diagonals than the reach itself admits.
/// type-audit: bare-ok(ratio: return)
fn chebyshev(a: Cell, b: Cell) -> f64 {
    f64::from((a.0 - b.0).abs().max((a.1 - b.1).abs()))
}

/// How much of a source's light survives one cell of distance, as the
/// denominator's quadratic term: light at distance `d` is scaled by
/// `1 / (1 + ATTENUATION * d²)`.
///
/// **Authored at 1, and not a free parameter.** See [`attenuate`].
/// type-audit: bare-ok(ratio)
pub const ATTENUATION: f64 = 1.0;

/// A source's light after travelling `distance` cells: every band scaled by
/// `1 / (1 + ATTENUATION * distance²)`.
///
/// **This constant is load-bearing, and it may not be tuned** (spec §4.2,
/// §11 risk 2). Because `shadowcast` is symmetric and the implicit torch
/// rides on the observer, every visible cell is lit by construction — so
/// this falloff is the *sole* source of light gradient in a possessed
/// chamber, and chambers are a few cells across. A later reading (H4a) asks
/// how dark a chamber actually gets, and that reading rides entirely on this
/// number. Moving it afterwards would be tuning the instrument to the
/// answer; an earlier draft of the spec called it cosmetic and was wrong.
///
/// The inverse-square shape is the physical one; the `1 +` keeps a source's
/// own cell finite (at `distance` 0 the light is returned unchanged) instead
/// of infinite, which is what a point source without a radius would be.
///
/// `distance` must be finite and non-negative — [`light_field`] only ever
/// passes a [`chebyshev`] distance, which both are.
/// type-audit: bare-ok(ratio: distance)
pub fn attenuate(illuminant: &Illuminant, distance: f64) -> Illuminant {
    let factor = 1.0 / (1.0 + ATTENUATION * distance * distance);
    let mut bands = [0.0f64; BANDS];
    for (out, value) in bands.iter_mut().zip(illuminant.get()) {
        *out = value * factor;
    }
    Illuminant::new(bands)
        .expect("scaling a valid illuminant by a factor in (0, 1] keeps every band valid")
}

/// The light arriving at every cell `sources` reach, summed.
///
/// Each source's reach is `shadowcast(lattice, source.at, source.radius)` and
/// nothing else, so a wall blocks light exactly as it blocks sight — the same
/// call, not a second implementation that agrees with it today.
///
/// **An unreached cell is ABSENT from the map, not present with a zero
/// illuminant.** The two render identically and they are different models:
/// `illuminant × reflectance × observer` over an absent cell correctly yields
/// *nothing at all*, which is what makes a darkness claim reachable at the
/// model level rather than only at the pixel. A `BTreeMap` entry holding
/// `Illuminant::new([0.0; BANDS])` would pass a screenshot and fail the
/// model.
///
/// The sum is band-wise and taken in `sources` order, which is the caller's
/// order and therefore stable: nothing here sorts by a float.
pub fn light_field(lattice: &Lattice, sources: &[Source]) -> BTreeMap<Cell, Illuminant> {
    let mut arriving: BTreeMap<Cell, [f64; BANDS]> = BTreeMap::new();
    for source in sources {
        for cell in shadowcast(lattice, source.at, source.radius) {
            let here = attenuate(&source.illuminant, chebyshev(source.at, cell));
            let total = arriving.entry(cell).or_insert([0.0; BANDS]);
            for (band, value) in total.iter_mut().zip(here.get()) {
                // THE ADDITIVE LAW (spec §4.1). A max, an average or a
                // last-writer-wins would each look plausible here.
                *band += value;
            }
        }
    }
    arriving
        .into_iter()
        .map(|(cell, bands)| {
            (
                cell,
                Illuminant::new(bands).expect("a sum of valid illuminants is valid"),
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lattice::{CellKind, Rect};
    use hornvale_kernel::color::blackbody;

    /// A rectangular room: a solid wall ring around one chamber's floor.
    ///
    /// Authored geometry, deliberately — these are unit claims about the
    /// *law*, and a drawn box states the distances they turn on out loud.
    /// The claims that must hold on the geometry production actually
    /// embeds are in `windows/vessel/tests/lantern_light.rs`, on real
    /// lattices.
    fn open_room(w: i32, h: i32) -> Lattice {
        let mut cells = BTreeMap::new();
        for y in 0..h {
            for x in 0..w {
                let shell = x == 0 || y == 0 || x == w - 1 || y == h - 1;
                cells.insert(
                    Cell(x, y),
                    if shell {
                        CellKind::Wall
                    } else {
                        CellKind::Floor(0)
                    },
                );
            }
        }
        Lattice {
            extent: Rect { x: 0, y: 0, w, h },
            cells,
            doorways: Vec::new(),
            dof: 0,
        }
    }

    /// An open room with one wall cell standing in the middle of it, on the
    /// row a source at `Cell(1, 4)` shares with `Cell(8, 4)` — so the two
    /// cells' centre-to-centre line passes straight through the pillar.
    fn room_with_a_wall_pillar() -> Lattice {
        let mut lattice = open_room(12, 9);
        lattice.cells.insert(Cell(4, 4), CellKind::Wall);
        lattice
    }

    /// The additive law, which the kernel declared and deferred to this
    /// campaign BY NAME (`color.rs`: "two torches on one wall — sum the
    /// illuminants, not the reflectances"). A POSITIVE CONTROL: two sources
    /// must make a cell strictly brighter than either alone, in every band.
    ///
    /// FIRES WHEN: the sum is replaced by a max, an average, or a
    /// last-writer-wins — all of which look plausible and none of which is
    /// the declared law. The midpoint is equidistant from both sources on
    /// purpose, so a max returns *exactly* the one-source value and the
    /// strict comparison catches it.
    #[test]
    fn two_sources_are_strictly_brighter_than_either_alone() {
        let lattice = open_room(9, 9);
        let a = Source {
            at: Cell(2, 4),
            illuminant: blackbody(TORCH_KELVIN),
            radius: 8,
        };
        let b = Source {
            at: Cell(6, 4),
            illuminant: blackbody(TORCH_KELVIN),
            radius: 8,
        };
        let both = light_field(&lattice, &[a.clone(), b.clone()]);
        let just_a = light_field(&lattice, &[a]);
        let just_b = light_field(&lattice, &[b]);
        let mid = Cell(4, 4);
        for band in 0..BANDS {
            let s = both[&mid].get()[band];
            assert!(
                s > just_a[&mid].get()[band] && s > just_b[&mid].get()[band],
                "band {band} at the midpoint is not strictly brighter under both \
                 sources: {s} against {} and {}",
                just_a[&mid].get()[band],
                just_b[&mid].get()[band]
            );
        }
    }

    /// Attenuation is the ONLY thing producing a gradient under the implicit
    /// torch, because shadowcast is symmetric and the torch rides on the
    /// observer — so the lit set IS the FOV set (spec §4.2). If this is flat,
    /// the whole pane is uniformly lit and H4a can never read anything.
    ///
    /// FIRES WHEN: attenuation is made distance-independent.
    #[test]
    fn light_falls_off_with_distance() {
        let lattice = open_room(15, 3);
        let field = light_field(
            &lattice,
            &[Source {
                at: Cell(1, 1),
                illuminant: blackbody(TORCH_KELVIN),
                radius: 14,
            }],
        );
        let near = field[&Cell(2, 1)].get()[5];
        let far = field[&Cell(12, 1)].get()[5];
        assert!(
            far < near,
            "far cell {far} is not dimmer than near cell {near}"
        );
    }

    /// The falloff's shape, pinned. **Not a cosmetic constant** (spec §4.2,
    /// §11 risk 2): it is the sole source of gradient in a possessed
    /// chamber, so H4a's reading rides on it and moving it after unblinding
    /// would be tuning the instrument to the answer.
    ///
    /// FIRES WHEN: the law stops being `1 / (1 + d²)` — a linear falloff, a
    /// different exponent, or a scaled `ATTENUATION` all move at least one of
    /// these three points. Its own cell is checked too: a source that dimmed
    /// itself would make `light_falls_off_with_distance` pass for the wrong
    /// reason.
    #[test]
    fn the_falloff_is_one_over_one_plus_distance_squared() {
        let source = blackbody(TORCH_KELVIN);
        for (distance, factor) in [(0.0, 1.0), (1.0, 0.5), (3.0, 0.1)] {
            let got = attenuate(&source, distance);
            for band in 0..BANDS {
                let want = source.get()[band] * factor;
                assert!(
                    (got.get()[band] - want).abs() < 1e-12,
                    "at distance {distance}, band {band} is {} and should be {want}",
                    got.get()[band]
                );
            }
        }
    }

    /// A wall blocks light exactly as it blocks sight, because it is the same
    /// call. This is the claim that "light needs no new geometry" rests on.
    ///
    /// FIRES WHEN: light_field stops routing through shadowcast and starts
    /// computing its own reach. The pillar's own cell is asserted lit as
    /// well, so a `light_field` that reached nothing at all could not pass.
    #[test]
    fn a_wall_casts_a_light_shadow() {
        let lattice = room_with_a_wall_pillar();
        let field = light_field(
            &lattice,
            &[Source {
                at: Cell(1, 4),
                illuminant: blackbody(TORCH_KELVIN),
                radius: 12,
            }],
        );
        assert!(
            field.contains_key(&Cell(4, 4)),
            "the pillar's own face is unlit, so this fixture cannot show a shadow"
        );
        assert!(
            !field.contains_key(&Cell(8, 4)),
            "the cell directly behind the pillar received light"
        );
    }

    /// An unlit cell has NO illuminant — not a dark one. `illuminant x
    /// reflectance x observer` then correctly yields nothing, which is what
    /// makes H4 reachable at the model level at all.
    ///
    /// FIRES WHEN: absent cells are filled with a zero illuminant, which
    /// reads the same on screen and is a different model.
    #[test]
    fn an_unreached_cell_is_absent_not_zero() {
        let lattice = open_room(20, 3);
        let field = light_field(
            &lattice,
            &[Source {
                at: Cell(1, 1),
                illuminant: blackbody(TORCH_KELVIN),
                radius: 3,
            }],
        );
        assert!(
            field.contains_key(&Cell(3, 1)),
            "nothing inside the radius is lit either, so absence proves nothing here"
        );
        assert!(!field.contains_key(&Cell(18, 1)));
    }

    /// A window may not draw (`windows/CLAUDE.md`), and this campaign
    /// declares no stream label at all (spec §5) — so the field must be a
    /// pure function of the lattice and its sources.
    ///
    /// FIRES WHEN: the field becomes seed-dependent, order-dependent within a
    /// source's reach, or accumulates state between calls.
    #[test]
    fn the_light_field_is_a_pure_function() {
        let lattice = open_room(11, 11);
        let sources = [
            Source {
                at: Cell(2, 2),
                illuminant: blackbody(TORCH_KELVIN),
                radius: 6,
            },
            Source {
                at: Cell(8, 8),
                illuminant: blackbody(HEARTH_KELVIN),
                radius: 6,
            },
        ];
        assert_eq!(
            light_field(&lattice, &sources),
            light_field(&lattice, &sources)
        );
    }
}
