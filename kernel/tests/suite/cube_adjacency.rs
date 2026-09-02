//! The 8-connected cube-sphere neighbour walk: interior steps, the twelve
//! seams, and the eight cube corners where a quad honestly has seven
//! neighbours instead of eight (The Pavement, spec §2.2 and §8).
//!
//! **The load-bearing test here is `adjacency_is_symmetric_across_every
//! _seam`.** `Facet::neighbors`' seam handling is derived from
//! `cube::CUBE_FACES` rather than written out as a rotation table, and this
//! file is what validates the derivation: an off-by-one-turn in a seam's
//! rotation satisfies every property that does not compare the two sides of
//! that seam — arity, distinctness, same-depth-ness, even "the neighbour is a
//! real address" — and fails symmetry immediately.

use hornvale_kernel::Facet;
use hornvale_kernel::cube;
use std::collections::BTreeSet;

/// Build a `Facet` from integer face-lattice coordinates: the inverse of
/// `Facet::face_lattice`, interleaving the bits of `i` and `j` into
/// `(hi_x << 1) | hi_y` digits.
fn facet_at(face: u8, i: u64, j: u64, depth: u32) -> Facet {
    let mut path = Vec::with_capacity(depth as usize);
    for k in (0..depth).rev() {
        let hx = ((i >> k) & 1) as u8;
        let hy = ((j >> k) & 1) as u8;
        path.push((hx << 1) | hy);
    }
    let f = Facet { face, path };
    let l = f.face_lattice();
    assert_eq!(
        (l.x as u64, l.y as u64, l.scale),
        (i, j, 1i64 << depth),
        "facet_at is wrong"
    );
    f
}

/// Every `Facet` on every base face at `depth`.
fn every_facet(depth: u32) -> Vec<Facet> {
    let scale = 1u64 << depth;
    let mut out = Vec::new();
    for face in 0..6u8 {
        for i in 0..scale {
            for j in 0..scale {
                out.push(facet_at(face, i, j, depth));
            }
        }
    }
    out
}

/// The eight cube corners are the only unit-sphere points with all three
/// components of equal magnitude, `1/sqrt(3)`. If `p` is one, return the sign
/// triple naming which corner; otherwise `None`. Purely geometric — it never
/// consults the lattice, so it is an independent witness that a 7-neighbour
/// quad really does sit at a cube corner.
fn cube_corner_of(p: [f64; 3]) -> Option<[i8; 3]> {
    let root_third = 1.0f64 / 3.0f64.sqrt();
    if (0..3).all(|i| (p[i].abs() - root_third).abs() < 1e-12) {
        Some([
            p[0].signum() as i8,
            p[1].signum() as i8,
            p[2].signum() as i8,
        ])
    } else {
        None
    }
}

/// **THE TEST THAT CATCHES A ROTATION OFF BY A TURN.** If `a` is a neighbour
/// of `b`, `b` must be a neighbour of `a` — on the same face, across each of
/// the twelve seams, and at a cube corner.
#[test]
fn adjacency_is_symmetric_across_every_seam() {
    for depth in [1u32, 2, 3, 4] {
        let mut checked = 0u64;
        for f in every_facet(depth) {
            for n in f.neighbors() {
                assert!(
                    n.neighbors().contains(&f),
                    "depth {depth}: {f:?} -> {n:?} but not back"
                );
                checked += 1;
            }
        }
        assert!(checked > 0, "depth {depth} checked nothing");
    }
}

/// Arity is eight everywhere except beside a cube corner, where it is seven.
///
/// **THE EXPECTED COUNT IS 24, NOT 8, AND THE BRIEF'S 8 WAS AN ARITHMETIC
/// SLIP WORTH SPELLING OUT.** The cube has eight corners; each is a lattice
/// vertex of degree THREE, shared by one quad on each of the three faces that
/// meet there. So the number of *quads* that lose their outward diagonal is
/// `8 * 3 = 24` at every depth, not 8 — equivalently, four corner quads per
/// face times six faces. Asserting 8 here would have failed against a
/// perfectly correct walk. The test keeps the brief's real intent by
/// asserting BOTH halves, which is strictly stronger than either: 24 quads
/// are short one neighbour, and they group onto exactly 8 distinct cube
/// corners, three quads apiece.
#[test]
fn arity_is_eight_except_at_the_cubes_eight_corners() {
    for depth in [1u32, 2, 3, 4] {
        let mut sevens = 0u32;
        let mut corners: BTreeSet<[i8; 3]> = BTreeSet::new();
        let mut per_corner: Vec<[i8; 3]> = Vec::new();
        for f in every_facet(depth) {
            let n = f.neighbors().len();
            assert!(n == 7 || n == 8, "arity {n} is neither 7 nor 8 at {f:?}");
            if n == 7 {
                sevens += 1;
                // Independent, geometric witness: one of this quad's own four
                // corner positions must BE a cube corner.
                let found: Vec<[i8; 3]> = f
                    .corners()
                    .iter()
                    .filter_map(|&p| cube_corner_of(p))
                    .collect();
                assert_eq!(
                    found.len(),
                    1,
                    "a 7-neighbour quad must touch exactly one cube corner, {f:?} touches {found:?}"
                );
                corners.insert(found[0]);
                per_corner.push(found[0]);
            }
        }
        assert_eq!(
            sevens, 24,
            "depth {depth}: 8 cube corners x 3 incident quads = 24 quads short one neighbour"
        );
        assert_eq!(
            corners.len(),
            8,
            "depth {depth}: expected exactly 8 distinct cube corners"
        );
        for c in &corners {
            assert_eq!(
                per_corner.iter().filter(|x| *x == c).count(),
                3,
                "depth {depth}: cube corner {c:?} must be shared by exactly 3 quads"
            );
        }
    }
}

/// The eight steps are eight DISTINCT rooms, never the room itself, and never
/// at a different depth. A seam rotation that folded two steps onto one
/// destination would still be symmetric; this is what refuses it.
#[test]
fn neighbours_are_distinct_same_depth_rooms_and_never_the_room_itself() {
    for depth in [1u32, 2, 3, 4] {
        for f in every_facet(depth) {
            let ns = f.neighbors();
            let uniq: BTreeSet<&Facet> = ns.iter().collect();
            assert_eq!(uniq.len(), ns.len(), "duplicate neighbour at {f:?}: {ns:?}");
            assert!(!ns.contains(&f), "{f:?} is its own neighbour");
            for n in &ns {
                assert_eq!(n.depth(), depth, "{n:?} is not at depth {depth}");
                assert!(n.face < 6, "{n:?} is not on a cube face");
                assert!(n.pack().is_ok(), "{n:?} is not addressable");
            }
        }
    }
}

/// **THE OTHER GEOMETRIC CHECK ON THE SEAM TABLE.** Adjacency is supposed to
/// mean *physically touching*: an edge neighbour shares two of its four corner
/// positions with us, a diagonal neighbour shares exactly one. A seam whose
/// along-coordinate is reversed the wrong way, or which lands on the wrong
/// destination axis, produces a room somewhere else on the neighbouring face
/// — still symmetric in some cases, but sharing NO corner at all.
///
/// **The tolerance was not slack when this test was written, and it is a real
/// finding about `cube.rs` — HISTORICAL now, not current.** Within one base
/// face, two quads sharing an edge agree BIT for bit (Task 2's
/// `corners_are_watertight_across_the_lattice`). Across a seam they used not
/// to, because `cube::warp` was `tan(t * PI/4)` and `tan(PI/4)` evaluates to
/// `0x3fefffffffffffff` — one ULP below `1.0`. A seam point therefore used to
/// come out as `(1, T, b)` from one face and `(T, 1, b)` from the other, with
/// `T = 1 - 1ulp`, and the two normalized to positions differing by
/// `1.11e-16` in one component. Measured, not assumed. `1e-13` was three
/// orders of margin over that and still ~10^10 times tighter than the
/// smallest inter-corner spacing at these depths.
///
/// **The Pavement's Task 4 closed this gap**: `cube::warp`/`cube::unwarp` now
/// special-case `±1.0` to exactly `±1.0` (Task 4, addendum 1), so a seam
/// corner's two pre-normalization vectors are componentwise IDENTICAL rather
/// than merely close, and normalize to byte-identical results — measured
/// true of the diagonal (cube-corner) case too, not only the edge case this
/// paragraph originally described. The tolerance here stays at `1e-13`
/// regardless: this test's job is the POSITIONAL claim (which index shares
/// how many corners, asserted by `assert_eq!(shared, want)` just below), and
/// loosening its inner comparison buys that claim nothing now that bit-exactness
/// is asserted directly and unconditionally by
/// `seam_corners_are_bit_exact_across_adjoining_faces`, below.
#[test]
fn every_neighbour_physically_touches_the_room_it_neighbours() {
    let tol = 1e-13;
    for depth in [1u32, 2, 3] {
        for f in every_facet(depth) {
            let mine = f.corners();
            let steps = f.neighbors();
            for (k, n) in steps.iter().enumerate() {
                let theirs = n.corners();
                let shared = mine
                    .iter()
                    .filter(|p| {
                        theirs
                            .iter()
                            .any(|q| (0..3).all(|i| (p[i] - q[i]).abs() < tol))
                    })
                    .count();
                // POSITIONAL, which is the point: `neighbor_steps` promises
                // that `[..4]` is the edge-adjacent set and everything after
                // it shares a single corner, and that promise is what lets a
                // caller take the 4-connected subgraph as a prefix. An
                // interleaved order, or a diagonal dropped from the middle
                // rather than the tail, fails here.
                let want = if k < 4 { 2 } else { 1 };
                assert_eq!(
                    shared, want,
                    "{f:?}'s neighbour {k} ({n:?}) shares {shared} corners, expected {want}"
                );
            }
            assert!(steps.len() >= 4, "{f:?} must keep all four edge neighbours");
        }
    }
}

/// **What the `warp(±1.0)`/`unwarp(±1.0)` special case in `kernel/src/cube.rs`
/// buys (The Pavement, Task 4, addendum 1): the same physical point, reached
/// by projecting from either of two adjoining faces, is byte-identical — not
/// merely close.** `every_neighbour_physically_touches_the_room_it_neighbours`
/// above already checks a shared corner exists, at a `1e-13` tolerance that
/// long predates this fix; this test is the tighter claim the fix actually
/// makes true, with `assert_eq!` and no tolerance at all, so a future "clean
/// up that redundant-looking special case" cannot pass unnoticed — it would
/// reopen the measured 1.11e-16 seam gap this file's other test used to carry
/// as a documented finding.
#[test]
fn seam_corners_are_bit_exact_across_adjoining_faces() {
    for depth in [1u32, 2, 3] {
        for f in every_facet(depth) {
            let mine = f.corners();
            for n in f.neighbors() {
                if n.face == f.face {
                    continue; // not a seam crossing; covered by Task 2's own test
                }
                let theirs = n.corners();
                let exact = mine.iter().filter(|p| theirs.contains(p)).count();
                assert!(
                    exact >= 1,
                    "{f:?} and cross-face neighbour {n:?} share no BIT-EXACT corner \
                     (only within a tolerance) — the warp(±1.0) special case is not \
                     doing its job"
                );
            }
        }
    }
}

/// A base face (depth 0) has four neighbours: the four faces across its four
/// sides. All four of its diagonals leave range on both axes at once — every
/// corner of a base face IS a cube corner — so there is no fifth or sixth
/// entry, and the opposite face is correctly not a neighbour.
#[test]
fn a_base_face_has_exactly_its_four_side_neighbours() {
    for face in 0..6u8 {
        let f = Facet { face, path: vec![] };
        let ns = f.neighbors();
        assert_eq!(ns.len(), 4, "base face {face} has four side neighbours");
        let faces: BTreeSet<u8> = ns.iter().map(|n| n.face).collect();
        assert_eq!(faces.len(), 4, "base face {face}: four distinct faces");
        assert!(!faces.contains(&face), "a face is not its own neighbour");
        // The one face NOT reachable is the opposite one — the face whose
        // normal is the negation of ours. Derived from CUBE_FACES, not named.
        let [n, _, _] = cube::CUBE_FACES[face as usize];
        let opposite = cube::CUBE_FACES
            .iter()
            .position(|[m, _, _]| (0..3).all(|i| (m[i] + n[i]).abs() < 1e-12))
            .expect("every cube face has an opposite");
        assert!(
            !faces.contains(&(opposite as u8)),
            "base face {face} must not neighbour its opposite {opposite}"
        );
    }
}

/// The walk is closed: starting anywhere and taking neighbours transitively
/// reaches every room at that depth. A seam table that mapped a face onto
/// itself, or that left one face unreachable, would pass symmetry and arity
/// and fail this.
#[test]
fn the_lattice_is_connected_across_all_six_faces() {
    for depth in [1u32, 2, 3] {
        let all: BTreeSet<Facet> = every_facet(depth).into_iter().collect();
        let start = facet_at(0, 0, 0, depth);
        let mut seen: BTreeSet<Facet> = BTreeSet::new();
        let mut stack = vec![start];
        while let Some(f) = stack.pop() {
            if !seen.insert(f.clone()) {
                continue;
            }
            for n in f.neighbors() {
                if !seen.contains(&n) {
                    stack.push(n);
                }
            }
        }
        assert_eq!(
            seen.len(),
            all.len(),
            "depth {depth}: the walk reached {} of {} rooms",
            seen.len(),
            all.len()
        );
        assert_eq!(
            seen, all,
            "depth {depth}: the walk reached a room off the mesh"
        );
    }
}

/// **THE EDGE-PREFIX INVARIANT, ASSERTED IN ITS OWN RIGHT.**
///
/// `Facet::neighbor_steps` promises that `neighbors()[..4]` is always exactly
/// the four EDGE-adjacent rooms, present at every room including a cube corner,
/// with the short arity always falling in the diagonal TAIL. That promise is
/// the reason the order is load-bearing rather than incidental, and until this
/// test it was only ever implied — `every_neighbour_physically_touches_the_room
/// _it_neighbours` happens to check it positionally while checking something
/// else, and nothing said the prefix was a contract.
///
/// **~30 call sites in the repository depend on it.** Every `neighbors()[k]` in
/// `windows/vessel/src/liveness.rs`, `windows/vessel/src/knowledge.rs`,
/// `windows/lab/src/synthetic.rs` and `windows/vessel/tests/suite/
/// course_properties.rs` indexes 0, 1 or 2 — inside the prefix — so **no call
/// site can panic at a cube corner**, where the returned `Vec` is only seven
/// long. That is a real dividend of the edges-first ordering rather than luck,
/// and a future campaign that reorders the steps to put diagonals first would
/// break all thirty silently. This test is what refuses that.
///
/// Three things are asserted, and the third is the one an interleaved order
/// fails:
///
/// 1. **Length.** Never fewer than 4 — the prefix is always whole.
/// 2. **Identity.** Each of the first four is the room reached by the
///    corresponding EDGE step in `neighbor_steps()`, sharing two corner
///    positions with us (a full quad edge).
/// 3. **Placement of the drop.** At a cube corner the missing step is the
///    outward DIAGONAL: index 4.. is short, index ..4 is not. Checked by
///    reconstructing what the full eight would have been and confirming the
///    dropped one is a diagonal every time.
#[test]
fn the_first_four_neighbours_are_always_the_four_edge_neighbours() {
    let tol = 1e-13;
    let steps = Facet::neighbor_steps();
    // Self-check on the roster this test is about, so it cannot silently
    // become vacuous if the step list is reordered: the first four must BE
    // the axis-aligned steps and the last four the diagonals.
    for (k, (dx, dy)) in steps.iter().enumerate() {
        let diagonal = *dx != 0 && *dy != 0;
        assert_eq!(
            diagonal,
            k >= 4,
            "neighbor_steps()[{k}] = ({dx}, {dy}) is on the wrong side of the \
             edge/diagonal split — the prefix invariant is not just unasserted, \
             it is broken"
        );
    }

    for depth in [0u32, 1, 2, 3] {
        let scale = 1i64 << depth;
        for f in every_facet(depth) {
            let l = f.face_lattice();
            let ns = f.neighbors();
            assert!(
                ns.len() >= 4,
                "{f:?} returned {} neighbours; the edge prefix must always be whole",
                ns.len()
            );
            let mine = f.corners();
            for (k, n) in ns.iter().enumerate().take(4) {
                let theirs = n.corners();
                let shared = mine
                    .iter()
                    .filter(|p| {
                        theirs
                            .iter()
                            .any(|q| (0..3).all(|i| (p[i] - q[i]).abs() < tol))
                    })
                    .count();
                assert_eq!(
                    shared, 2,
                    "{f:?}'s prefix entry {k} ({n:?}) shares {shared} corners, not the \
                     two a full quad edge shares"
                );
            }
            // Where the drop lands. Recompute which of the eight steps leaves
            // range on BOTH axes — the only step `neighbors` ever omits — and
            // require every omission to be a diagonal, i.e. at index >= 4.
            let dropped: Vec<usize> = steps
                .iter()
                .enumerate()
                .filter(|(_, (dx, dy))| {
                    !(0..scale).contains(&(l.x + dx)) && !(0..scale).contains(&(l.y + dy))
                })
                .map(|(k, _)| k)
                .collect();
            assert_eq!(
                ns.len(),
                8 - dropped.len(),
                "{f:?}: arity does not account for the dropped steps {dropped:?}"
            );
            for k in &dropped {
                assert!(
                    *k >= 4,
                    "{f:?} dropped step {k}, which is inside the edge prefix"
                );
            }
        }
    }
}

/// The invariant's payoff, stated as a test rather than left to a reader's
/// arithmetic: indexing `[0]`, `[1]` or `[2]` is total over the whole mesh, at
/// every depth, including the eight cube corners where the `Vec` is only seven
/// long. That is what keeps the ~30 existing `neighbors()[k]` call sites from
/// panicking.
///
/// **THIS TEST IS THE WEAKER HALF AND IT MUST NOT BE READ AS GUARDING THE
/// ORDER.** Measured, by reordering `neighbor_steps` to put diagonals first
/// and re-running: this test still PASSES, because a dropped diagonal at index
/// 0..4 still leaves a length-7 `Vec` and `[0]`, `[1]`, `[2]` still exist. The
/// call sites would not crash — they would silently start receiving DIAGONAL
/// neighbours, which is the failure that matters and the one
/// `the_first_four_neighbours_are_always_the_four_edge_neighbours` catches (it
/// fails on that same reorder, at its first assertion). Panic-freedom and
/// edge-semantics are two claims; this one covers the first only.
#[test]
fn indexing_the_first_three_neighbours_never_panics_anywhere_on_the_mesh() {
    for depth in [0u32, 1, 2, 3, 4] {
        for f in every_facet(depth) {
            let ns = f.neighbors();
            let picked = [&ns[0], &ns[1], &ns[2]];
            for n in picked {
                assert_ne!(n, &f, "{f:?} indexed itself");
                assert_eq!(n.depth(), depth);
            }
        }
    }
}
