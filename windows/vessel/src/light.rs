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

use crate::lattice::{Cell, CellKind, Lattice, neighbours, shadowcast};
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

/// The wall cell a chamber's hearth burns at, if the chamber has a wall of
/// its own.
///
/// # The join the interior model never had
///
/// `Cell` appears nowhere in `windows/vessel/src/interior/`: the interior
/// model is **topological** (anchors and the relations between them) and the
/// lattice is **spatial** (cells), and until now nothing joined the two. So
/// of spec §4.2's three light sources the torch (the observer's own cell) and
/// the doorway (`Lattice::doorways`) could be placed and `AnchorKind::Hearth`
/// could not — the spec said the hearth was "already in built interiors",
/// which is true of the interior and false of the lattice.
///
/// [`CellKind::Wall`]'s own doc already states where a hearth goes — *"a
/// place in its own right — an alcove, a screen or a fireplace is an anchor
/// AT one of these"* — so this builds the join that doc described rather than
/// inventing a placement. Not the chamber's centroid: a fire in the middle of
/// the floor is a campfire, and this is a built interior.
///
/// # Derived, never drawn
///
/// A pure function of the lattice: no `Seed`, no `Stream`, no draw, and this
/// campaign declares no `streams.rs` label at all (spec §5). A window that
/// draws has quietly become a domain with no registry entry and no
/// pin-isolation test (`windows/CLAUDE.md`). The candidates are collected in
/// [`Cell`]'s own total order and the first is taken, so the answer depends
/// on nothing but the cells.
///
/// # Which wall — DETERMINISTIC, NOT MEANINGFUL
///
/// Recorded plainly so the next campaign need not guess: **the rule is the
/// least wall cell orthogonally adjacent to this chamber's floor, by `Cell`'s
/// derived `Ord`** — lowest `x`, then lowest `y`, which on a rectangular
/// chamber is the top of its western wall. That is defensible (a hearth is
/// against a wall of the room it heats) but it is *not* a claim that a
/// builder would have chosen this wall over the opposite one. It is the
/// cheapest total order over a set that has to be broken by something.
///
/// Two richer rules were considered and declined here:
///
/// - **Avoid a threshold-adjacent wall** — a fire beside a doorjamb is
///   draughty and stands in the traffic. Genuinely better modelling, and
///   deliberately not taken in this campaign: it needs a fallback for the
///   chamber whose every wall touches a door, and it would move the hearth
///   *towards* the reading H2 wants to make (a hearth-lit cell and a
///   doorway-lit cell differing), which is the wrong order to do things in.
///   Take it in a campaign that is not also measuring it.
/// - **The longest unbroken wall run** — how a builder actually picks. It
///   needs a tie-break of its own on a square room, so it does not remove the
///   arbitrariness, only moves it.
///
/// If either lands later, re-pin
/// `the_hearth_cell_is_pinned_for_the_two_chamber_house` in the same commit
/// and say there why the hearth moved.
///
/// Returns `None` for a chamber that owns no wall — a chamber with no floor,
/// or one whose floor is entirely enclosed by another chamber's. Never
/// somebody else's wall: fail loudly, never guess.
/// type-audit: bare-ok(index: chamber)
pub fn hearth_cell(lattice: &Lattice, chamber: usize) -> Option<Cell> {
    lattice
        .cells
        .iter()
        .filter(|(_, kind)| **kind == CellKind::Wall)
        .map(|(cell, _)| *cell)
        .filter(|cell| {
            neighbours(*cell)
                .iter()
                .any(|n| lattice.cells.get(n) == Some(&CellKind::Floor(chamber)))
        })
        // `min`, not `next`: the total order is the RULE, and taking it from
        // the map's iteration order would leave the rule implicit in
        // `BTreeMap`'s contract instead of stated here.
        .min()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lattice::Rect;
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

    /// Chamber 0's floor rect in [`two_chamber_house`], as authored.
    const HOUSE_CHAMBER_0: Rect = Rect {
        x: 1,
        y: 1,
        w: 5,
        h: 7,
    };
    /// Chamber 1's floor rect in [`two_chamber_house`], as authored.
    const HOUSE_CHAMBER_1: Rect = Rect {
        x: 7,
        y: 1,
        w: 5,
        h: 7,
    };
    /// The one doorway between them in [`two_chamber_house`], as authored.
    const HOUSE_DOORWAY: Cell = Cell(6, 4);

    /// Two chambers side by side inside one exterior shell, sharing a wall
    /// column with a single threshold in it — the shape `allocate` produces
    /// for a two-chamber structure, drawn by hand.
    ///
    /// Authored rather than embedded, for the same reason [`open_room`] is:
    /// these are unit claims about *which* cell gets chosen, and a fixture
    /// whose geometry is only knowable by running the embedder cannot state
    /// the answer out loud.
    fn two_chamber_house() -> Lattice {
        let (w, h) = (13, 9);
        let mut cells = BTreeMap::new();
        for y in 0..h {
            for x in 0..w {
                let cell = Cell(x, y);
                let kind = if HOUSE_CHAMBER_0.contains(cell) {
                    CellKind::Floor(0)
                } else if HOUSE_CHAMBER_1.contains(cell) {
                    CellKind::Floor(1)
                } else if cell == HOUSE_DOORWAY {
                    CellKind::Threshold(0, 1)
                } else {
                    CellKind::Wall
                };
                cells.insert(cell, kind);
            }
        }
        Lattice {
            extent: Rect { x: 0, y: 0, w, h },
            cells,
            doorways: vec![(0, 1, HOUSE_DOORWAY)],
            dof: 0,
        }
    }

    /// The wall cells belonging to chamber 0 of [`two_chamber_house`],
    /// enumerated from the AUTHORED rects above rather than by re-running the
    /// adjacency scan [`hearth_cell`] uses.
    ///
    /// A helper that recomputed the implementation's own set would make the
    /// containment assertion tautological — it would agree with a broken
    /// `hearth_cell` exactly as readily as with a correct one.
    fn authored_walls_of_chamber_zero() -> Vec<Cell> {
        let r = HOUSE_CHAMBER_0;
        let mut walls = Vec::new();
        for y in r.y..r.y + r.h {
            walls.push(Cell(r.x - 1, y));
            walls.push(Cell(r.x + r.w, y));
        }
        for x in r.x..r.x + r.w {
            walls.push(Cell(x, r.y - 1));
            walls.push(Cell(x, r.y + r.h));
        }
        walls.retain(|c| *c != HOUSE_DOORWAY);
        walls
    }

    /// A lattice in which chamber 0 has walls all around it that are somebody
    /// ELSE's: its single floor cell sits at the centre of chamber 1's floor,
    /// so every wall in the lattice is adjacent to chamber 1 and none is
    /// adjacent to chamber 0.
    ///
    /// Deliberately not "a lattice with no walls at all" — that would let a
    /// `hearth_cell` which happily returns another chamber's wall pass, since
    /// there would be no other chamber's wall to return.
    fn wall_less_lattice() -> Lattice {
        let (w, h) = (7, 7);
        let mut cells = BTreeMap::new();
        for y in 0..h {
            for x in 0..w {
                let shell = x == 0 || y == 0 || x == w - 1 || y == h - 1;
                cells.insert(
                    Cell(x, y),
                    if shell {
                        CellKind::Wall
                    } else {
                        CellKind::Floor(1)
                    },
                );
            }
        }
        cells.insert(Cell(3, 3), CellKind::Floor(0));
        Lattice {
            extent: Rect { x: 0, y: 0, w, h },
            cells,
            doorways: Vec::new(),
            dof: 0,
        }
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

    /// A hearth sits at a WALL cell of its own chamber — the placement
    /// `CellKind::Wall`'s doc already describes ("a fireplace is an anchor AT
    /// one of these"). Not the centroid: a fire in the middle of the floor is
    /// a campfire, and this is a built interior.
    ///
    /// FIRES WHEN: the chosen cell is passable, or belongs to another
    /// chamber. It CANNOT see *which* of the chamber's own walls was chosen —
    /// measured, not assumed: swapping `min` for `max` leaves this green,
    /// which is what `the_hearth_cell_is_pinned_for_the_two_chamber_house`
    /// exists to cover.
    #[test]
    fn a_hearth_sits_at_a_wall_of_its_own_chamber() {
        let lattice = two_chamber_house();
        let cell = hearth_cell(&lattice, 0).expect("chamber 0 has walls");
        assert_eq!(lattice.cells.get(&cell), Some(&CellKind::Wall));
        assert!(
            authored_walls_of_chamber_zero().contains(&cell),
            "{cell:?} is a wall, but not one of chamber 0's own"
        );
    }

    /// DERIVED, never drawn. A window that draws has quietly become a domain
    /// with no registry entry and no pin-isolation test
    /// (`windows/CLAUDE.md`) — and this campaign declares no stream label at
    /// all (spec §5).
    ///
    /// FIRES WHEN: the choice becomes seed-dependent, or dependent on
    /// anything outside the lattice. It checks REPEATABILITY only — same
    /// input, same output — and is therefore blind to a change of *rule*,
    /// which is a different claim and has its own test below.
    #[test]
    fn the_hearth_cell_is_a_pure_function_of_the_lattice() {
        let lattice = two_chamber_house();
        let a = hearth_cell(&lattice, 0);
        let b = hearth_cell(&lattice.clone(), 0);
        assert_eq!(a, b);
    }

    /// The chosen cell itself, pinned.
    ///
    /// **Added because the two tests above were measured to be blind to the
    /// rule.** Changing `min` to `max` in [`hearth_cell`] moves the hearth
    /// from chamber 0's north-west wall to its south-east one and leaves
    /// `a_hearth_sits_at_a_wall_of_its_own_chamber` green (both are walls of
    /// chamber 0) and `the_hearth_cell_is_a_pure_function_of_the_lattice`
    /// green (both are repeatable). A guard that cannot distinguish two
    /// implementations of the thing it names is not guarding it.
    ///
    /// FIRES WHEN: the selection rule changes at all — the total order, the
    /// element taken from it, or the adjacency that builds the candidate set.
    /// Re-pin it deliberately, in the commit that moves the rule, and say in
    /// that commit why the hearth moved.
    #[test]
    fn the_hearth_cell_is_pinned_for_the_two_chamber_house() {
        assert_eq!(
            hearth_cell(&two_chamber_house(), 0),
            Some(Cell(0, 1)),
            "chamber 0's hearth moved off the first cell of its own wall set"
        );
        assert_eq!(
            hearth_cell(&two_chamber_house(), 1),
            Some(Cell(6, 1)),
            "chamber 1's hearth moved off the first cell of its own wall set"
        );
    }

    /// A chamber with no wall of its own yields None rather than a wall
    /// belonging to somebody else. Fail loudly, never guess.
    ///
    /// FIRES WHEN: the candidate scan stops asking whose floor a wall
    /// touches. The fixture is full of walls — chamber 1's — so a
    /// `hearth_cell` that returned the lattice's first wall regardless of
    /// chamber would have something to return here and would fail.
    #[test]
    fn a_chamber_with_no_wall_of_its_own_has_no_hearth_cell() {
        let lattice = wall_less_lattice();
        assert!(
            hearth_cell(&lattice, 1).is_some(),
            "chamber 1 owns every wall in this fixture; if it has none either, \
             the fixture proves nothing about ownership"
        );
        assert_eq!(hearth_cell(&lattice, 0), None);
    }
}
