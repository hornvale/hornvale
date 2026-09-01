//! The corner rule: a diagonal step is refused only when BOTH flanking
//! orthogonal cells are walls (Task 5).
//!
//! **Both directions, on purpose.** A test asserting only refusal is
//! structurally blind to over-refusal, so every case below pairs a refusal
//! with a permission that differs from it by exactly one flank.
//!
//! **`diagonal_is_blocked` takes a passability CLOSURE, not a `Lattice`**
//! (Fix round 1: the brief that specified a `&Lattice` signature served only
//! one of the two `Cell`-addressed bands the rule governs). That closure
//! shape is exercised against BOTH representations below: [`Lattice`]/
//! [`CellKind`] for interiors, and `CellGrid<LevelCellKind>`/`movement_mode`
//! — the underground band's own real types, not a stand-in — for the other.
//!
//! **Scope: this file still wires nothing into a step path.**
//! `Session::go` (the walk band) has no walls at all to ask this question
//! about (`go` performs no passability check; decision 0141), and wiring the
//! rule into the two bands that DO have walls (interiors, underground) is
//! Task 6's job, not this file's.

use hornvale_vessel::lattice::{
    Cell, CellKind, HEADINGS, Lattice, Rect, diagonal_is_blocked, kind_of,
};
use hornvale_vessel::underworld_level::{CellGrid, LevelCellKind, movement_mode};
use std::collections::BTreeMap;

/// A small open lattice — floor everywhere in a 5x5 square centred on the
/// origin — with `walls` set to `CellKind::Wall` and everything else left as
/// `CellKind::Floor(0)`.
///
/// Big enough that `Cell(0, 0)`'s full diagonal neighbourhood (all eight of
/// [`hornvale_vessel::lattice::neighbours`]) lies inside the extent, so
/// `diagonal_is_blocked` never has to fall back on its outside-the-extent
/// convention for any cell this file asks about (except the one test that
/// asks for that convention deliberately).
fn lattice_with_walls(walls: &[Cell]) -> Lattice {
    let extent = Rect {
        x: -2,
        y: -2,
        w: 5,
        h: 5,
    };
    let mut cells: BTreeMap<Cell, CellKind> = BTreeMap::new();
    for x in extent.x..(extent.x + extent.w) {
        for y in extent.y..(extent.y + extent.h) {
            let cell = Cell(x, y);
            let kind = if walls.contains(&cell) {
                CellKind::Wall
            } else {
                CellKind::Floor(0)
            };
            cells.insert(cell, kind);
        }
    }
    Lattice {
        extent,
        cells,
        doorways: Vec::new(),
        dof: 0,
    }
}

/// The passability oracle `diagonal_is_blocked` wants, built over a
/// [`Lattice`]: a cell is open iff `kind_of` reports a passable kind.
/// `kind_of` returning `None` (outside the extent) reads as impassable here
/// — see `a_flank_outside_the_extent_counts_as_impassable`, which is a
/// property of THIS closure, not of `diagonal_is_blocked` itself (the
/// function's own doc says as much: what counts as impassable is entirely
/// the closure's call).
fn open_in(lattice: &Lattice) -> impl Fn(Cell) -> bool + '_ {
    move |c: Cell| kind_of(lattice, c).is_some_and(|k| k.passable())
}

#[test]
fn a_diagonal_is_refused_only_when_both_flanks_are_walls() {
    // Both flanks walled: the step would pass through a point. Refused.
    let l = lattice_with_walls(&[Cell(1, 0), Cell(0, 1)]);
    assert!(diagonal_is_blocked(Cell(0, 0), (1, 1), open_in(&l)));

    // One flank open: brushing a single corner is physical. PERMITTED.
    let l = lattice_with_walls(&[Cell(1, 0)]);
    assert!(!diagonal_is_blocked(Cell(0, 0), (1, 1), open_in(&l)));
    let l = lattice_with_walls(&[Cell(0, 1)]);
    assert!(!diagonal_is_blocked(Cell(0, 0), (1, 1), open_in(&l)));

    // Open ground. Permitted.
    let l = lattice_with_walls(&[]);
    assert!(!diagonal_is_blocked(Cell(0, 0), (1, 1), open_in(&l)));
}

/// The same claim restated over the other three diagonal directions, so the
/// rule is checked as a property of `(dx, dy)` rather than pinned at one
/// corner. Each direction's two flanks are `(dx, 0)` and `(0, dy)` away from
/// `from`, mirroring the doc comment on `diagonal_is_blocked`.
#[test]
fn the_rule_holds_for_every_diagonal_direction() {
    for &(dx, dy) in &[(1, 1), (1, -1), (-1, 1), (-1, -1)] {
        let from = Cell(0, 0);
        let flank_a = Cell(from.0 + dx, from.1);
        let flank_b = Cell(from.0, from.1 + dy);

        let l = lattice_with_walls(&[flank_a, flank_b]);
        assert!(
            diagonal_is_blocked(from, (dx, dy), open_in(&l)),
            "({dx},{dy}): both flanks walled must refuse"
        );

        let l = lattice_with_walls(&[flank_a]);
        assert!(
            !diagonal_is_blocked(from, (dx, dy), open_in(&l)),
            "({dx},{dy}): flank_a open must permit"
        );

        let l = lattice_with_walls(&[flank_b]);
        assert!(
            !diagonal_is_blocked(from, (dx, dy), open_in(&l)),
            "({dx},{dy}): flank_b open must permit"
        );

        let l = lattice_with_walls(&[]);
        assert!(
            !diagonal_is_blocked(from, (dx, dy), open_in(&l)),
            "({dx},{dy}): open ground must permit"
        );
    }
}

/// A `Threshold` is fabric a door was cut into, not fabric — flanking one
/// must not refuse a diagonal the way flanking a `Wall` does. This is what
/// makes `CellKind::passable` (never `== CellKind::Wall`) the right basis
/// for `open_in`'s closure, the same discipline `classify.rs` states for
/// every rule in that module.
#[test]
fn a_threshold_flank_does_not_block_a_diagonal() {
    let mut l = lattice_with_walls(&[Cell(1, 0), Cell(0, 1)]);
    // Both flanks were walls, so the step is refused first...
    assert!(diagonal_is_blocked(Cell(0, 0), (1, 1), open_in(&l)));
    // ...then one flank becomes a doorway instead of fabric, and the corner
    // opens: a door is not a wall to cut past.
    l.cells.insert(Cell(1, 0), CellKind::Threshold(0, 1));
    assert!(!diagonal_is_blocked(Cell(0, 0), (1, 1), open_in(&l)));
}

/// A flank outside the lattice's extent counts as impassable under
/// `open_in` — `kind_of` returning `None` there means "outside the extent",
/// and there is no built corner for a mover to brush past. This is a claim
/// about `open_in`'s own closure (see its doc); `diagonal_is_blocked` itself
/// takes whatever the closure says on faith.
#[test]
fn a_flank_outside_the_extent_counts_as_impassable() {
    // A 1x1 lattice: only the origin is defined, so both diagonal flanks of a
    // step from it are outside the extent.
    let mut cells = BTreeMap::new();
    cells.insert(Cell(0, 0), CellKind::Floor(0));
    let l = Lattice {
        extent: Rect {
            x: 0,
            y: 0,
            w: 1,
            h: 1,
        },
        cells,
        doorways: Vec::new(),
        dof: 0,
    };
    assert!(diagonal_is_blocked(Cell(0, 0), (1, 1), open_in(&l)));
}

/// Called with a non-diagonal delta, the corner rule has nothing to cut
/// through and must not refuse — `diagonal_is_blocked` is only ever meant to
/// gate [`hornvale_vessel::lattice::HEADINGS`]'s diagonal entries (indices
/// `4..8`), and this pins the orthogonal entries as a documented no-op rather
/// than leaving the boundary unchecked.
#[test]
fn an_orthogonal_delta_is_never_blocked() {
    let l = lattice_with_walls(&[Cell(1, 0), Cell(0, 1), Cell(-1, 0), Cell(0, -1)]);
    for d in [(1, 0), (0, 1), (-1, 0), (0, -1)] {
        assert!(!diagonal_is_blocked(Cell(0, 0), d, open_in(&l)));
    }
}

/// **`HEADINGS`'s first four entries are the four orthogonal headings, in
/// exactly this order — membership AND order, both a save-format contract,
/// not merely a documentation claim.** `grow.rs`'s `rotated()` maps a
/// `ROOM_LAYOUT_GROWN` stream draw through `HEADINGS[0..4]` to pick a
/// chamber's tunnel direction, so the ORDER of these four entries — not just
/// which four cells count as neighbours — determines generated geometry for
/// a given seed. A plausible-looking tidy-up (reordering to match
/// `session.rs`'s `COMPASS_SQUARE`'s N-first order, say) would silently
/// change every grown lattice with nothing objecting, because ten call sites
/// across this crate only ever assert MEMBERSHIP (`[..4]`/`.take(4)`) and
/// none pinned order until now.
///
/// This is exactly the contract `kernel/src/room.rs`'s `neighbor_steps`
/// already states and tests for the cube's own quads
/// (`the_first_four_neighbours_are_always_the_four_edge_neighbours`,
/// `kernel/tests/suite/cube_adjacency.rs`) — restated here because this
/// crate's `HEADINGS` is an independent array with the same load-bearing
/// property.
#[test]
fn the_first_four_headings_are_the_orthogonals_in_order() {
    assert_eq!(
        HEADINGS[..4],
        [(1, 0), (0, 1), (-1, 0), (0, -1)],
        "rotated() (windows/vessel/src/lattice/grow.rs) indexes \
         HEADINGS[0..4] to turn a ROOM_LAYOUT_GROWN stream draw into a \
         chamber tunnel direction — reordering these four changes every \
         grown lattice for every seed, silently"
    );
}

/// The closure form exercised against the SECOND `Cell`-addressed band's own
/// real representation — `CellGrid<LevelCellKind>` queried through
/// `movement_mode`, exactly the path `underground.rs`'s `peek` (Task 6) will
/// wire this into — not a `Lattice`-shaped stand-in. A `&Lattice` signature
/// could only ever ARGUE this function serves both bands; this test is what
/// makes the claim checked rather than merely plausible.
#[test]
fn the_closure_form_is_exercised_against_the_underground_bands_own_grid() {
    let extent = Rect {
        x: -2,
        y: -2,
        w: 5,
        h: 5,
    };
    let mut grid = CellGrid::new(extent, LevelCellKind::Wall);
    for x in extent.x..(extent.x + extent.w) {
        for y in extent.y..(extent.y + extent.h) {
            grid.set(Cell(x, y), LevelCellKind::Floor);
        }
    }

    // Open ground: permitted.
    assert!(!diagonal_is_blocked(Cell(0, 0), (1, 1), |c: Cell| grid
        .get(c)
        .and_then(movement_mode)
        .is_some()));

    // Both flanks natural rock: refused, the same rule as a built wall.
    grid.set(Cell(1, 0), LevelCellKind::Wall);
    grid.set(Cell(0, 1), LevelCellKind::Wall);
    assert!(diagonal_is_blocked(Cell(0, 0), (1, 1), |c: Cell| grid
        .get(c)
        .and_then(movement_mode)
        .is_some()));

    // One flank standing water (`movement_mode` still returns `Some`, just
    // `Wade` rather than `Walk`) instead of rock: permitted. Passability, not
    // dry footing, is what `diagonal_is_blocked` asks `movement_mode` for.
    grid.set(Cell(1, 0), LevelCellKind::Flooded);
    assert!(!diagonal_is_blocked(Cell(0, 0), (1, 1), |c: Cell| grid
        .get(c)
        .and_then(movement_mode)
        .is_some()));
}
