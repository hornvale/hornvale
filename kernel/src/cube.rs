//! The tangent-warped cube-sphere projection: the one definition of "face,
//! (a, b) parameters" <-> "unit sphere position", shared by every consumer
//! that needs an 8-connected quad lattice on a sphere (Spec §2.0). A naive
//! cube-sphere projection squashes the grid toward each face's corners; this
//! warps the face parameters by `tan`/`atan` before/after projecting so the
//! grid comes out even once the sphere has bent it.

/// The six cube-face bases `(n, u, v)`: a face point is `normalize(n + a·u +
/// b·v)` for `(a, b) ∈ [-1, 1]²`. Copied verbatim from `windows/scene/src/
/// region.rs`'s `FACES` so face numbering is unchanged; both must agree on
/// which face is which.
/// type-audit: bare-ok(ratio)
pub const CUBE_FACES: [[[f64; 3]; 3]; 6] = [
    [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],
    [[-1.0, 0.0, 0.0], [0.0, -1.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, 1.0, 0.0], [-1.0, 0.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, -1.0, 0.0], [1.0, 0.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, 0.0, 1.0], [1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
    [[0.0, 0.0, -1.0], [-1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
];

/// Warp a face parameter so cells come out even AFTER the sphere bends them.
/// The naive cube-sphere squashes the grid toward a face corner; this
/// pre-spreads it by the compensating amount. Measured: 5.16x max/min cell
/// area unwarped, 1.41x warped (spec section 2.0).
///
/// **`warp(±1.0)` is special-cased to exactly `±1.0`, rather than falling
/// through to `tan(±π/4)`.** `math::tan` evaluates that at one ULP below
/// `1.0` (`0x3fefffffffffffff`), so without the special case two faces
/// meeting at a seam compute the shared edge from componentwise-DIFFERENT
/// pre-normalization vectors and land 1.11e-16 apart once normalized —
/// measured, not assumed (`kernel/tests/suite/cube_adjacency.rs`'s
/// `every_neighbour_physically_touches_the_room_it_neighbours`, before this
/// fix). The snap is safe rather than a discontinuity, for three reasons:
/// `tan(π/4)` **is** exactly `1` mathematically, so snapping to it REDUCES
/// error against the true function rather than introducing any; `warp` is
/// already monotone increasing on `[-1, 1]`, and pinning only the two
/// endpoints to the values the true function already approaches there
/// cannot break that monotonicity; and it only ever changes geometry at an
/// EXACT face edge (`t = ±1`), which is precisely where two adjoining faces'
/// projections must agree bit-for-bit for the cube-sphere to be watertight
/// ACROSS a seam, not merely within one face (the within-face case was
/// already exact — dyadic parameters through identical arithmetic — see
/// `corners_are_watertight_across_the_lattice`). Do not "simplify" this
/// special case away: removing it reintroduces the measured seam gap.
/// `kernel/tests/suite/cube_adjacency.rs`'s
/// `seam_corners_are_bit_exact_across_adjoining_faces` is the regression
/// test that would catch it.
fn warp(t: f64) -> f64 {
    if t == 1.0 {
        return 1.0;
    }
    if t == -1.0 {
        return -1.0;
    }
    crate::math::tan(t * std::f64::consts::FRAC_PI_4)
}

/// The inverse of [`warp`], special-cased at `±1.0` for the same reason and
/// by the same argument: `atan(1) / (π/4)` is exactly `1` mathematically, so
/// pinning the endpoint keeps `unwarp` the exact inverse of `warp` there
/// too, rather than landing one ULP off in the opposite direction.
fn unwarp(t: f64) -> f64 {
    if t == 1.0 {
        return 1.0;
    }
    if t == -1.0 {
        return -1.0;
    }
    crate::math::atan(t) / std::f64::consts::FRAC_PI_4
}

/// The warped forward projection: a face index and `(a, b) ∈ [-1, 1]²` face
/// parameters to a unit sphere position.
///
/// **Panics with the physical reason on an invalid `face`, not an opaque
/// index-out-of-bounds.** `face` must be `0..CUBE_FACES.len()`; an
/// unconditional `assert!` names the actual problem (a cube-sphere has
/// exactly six faces) and the valid range, matching how this project words
/// a refusal elsewhere (`GenesisError`, the pin refusals — the physical
/// reason, not the mechanism), and it is the same silent/uninformative-
/// failure family `FacetId::unpack`'s bound closed one layer up. Deliberately
/// an `assert!`, not `debug_assert!`: a debug-only check disappears in a
/// release build and leaves nothing behind but the bare indexing panic this
/// replaces, so a release build would be back to reading `CUBE_FACES[face]`
/// with no guard at all — this check runs in every build profile.
/// type-audit: bare-ok(index: face), bare-ok(ratio: a), bare-ok(ratio: b), bare-ok(ratio: return)
pub fn face_unit(face: usize, a: f64, b: f64) -> [f64; 3] {
    let faces = CUBE_FACES.len();
    assert!(
        face < faces,
        "invalid cube face {face}: a cube-sphere has exactly {faces} faces, numbered 0..{faces}"
    );
    let [n, u, v] = CUBE_FACES[face];
    let (wa, wb) = (warp(a), warp(b));
    let q = [
        n[0] + wa * u[0] + wb * v[0],
        n[1] + wa * u[1] + wb * v[1],
        n[2] + wa * u[2] + wb * v[2],
    ];
    let m = (q[0] * q[0] + q[1] * q[1] + q[2] * q[2]).sqrt();
    [q[0] / m, q[1] / m, q[2] / m]
}

/// The warped inverse projection: a unit sphere position to the face index
/// and `(a, b)` face parameters that produced it. Face assignment is the
/// standard cube-map rule (the face whose normal has the largest dot
/// product with `p`); ties break last-wins via `total_cmp`, matching
/// `region.rs`'s existing rule so face assignment on a seam does not move.
/// type-audit: bare-ok(ratio: p), bare-ok(ratio: return)
pub fn locate(p: [f64; 3]) -> (usize, f64, f64) {
    let dot = |x: [f64; 3], y: [f64; 3]| x[0] * y[0] + x[1] * y[1] + x[2] * y[2];
    let (face, _) = CUBE_FACES
        .iter()
        .enumerate()
        .map(|(f, [n, _, _])| (f, dot(p, *n)))
        .max_by(|x, y| x.1.total_cmp(&y.1))
        .expect("CUBE_FACES is nonempty");
    let [n, u, v] = CUBE_FACES[face];
    let pn = dot(p, n);
    (face, unwarp(dot(p, u) / pn), unwarp(dot(p, v) / pn))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locate_inverts_face_unit_on_every_face() {
        for face in 0..6 {
            for &a in &[-0.9, -0.5, -0.1, 0.0, 0.1, 0.5, 0.9] {
                for &b in &[-0.9, -0.5, -0.1, 0.0, 0.1, 0.5, 0.9] {
                    let p = face_unit(face, a, b);
                    let (f2, a2, b2) = locate(p);
                    assert_eq!(f2, face, "face {face} at ({a},{b})");
                    assert!((a2 - a).abs() < 1e-12, "a: {a2} vs {a}");
                    assert!((b2 - b).abs() < 1e-12, "b: {b2} vs {b}");
                }
            }
        }
    }

    /// Local distortion falls as 1/N. Nathan's acceptance criterion is LOCAL
    /// ("as long as the area around the cursor itself is distorted minimally"),
    /// so this measures ADJACENT cells, not the whole-face spread.
    ///
    /// **WHICH ASSERTION DISCRIMINATES, corrected from what this comment used
    /// to imply.** It implied the 1/N TREND check is what catches a projection
    /// that is locally smooth but globally wrong. Task 1's reviewer measured
    /// that and it is false: run against the NAIVE (unwarped) projection, the
    /// halving ratio is ~2.03 — comfortably inside the same `(1.7, 2.3)` band
    /// the warped projection's 1.96 satisfies — because that ratio falls close
    /// to 1/N for essentially ANY smooth cube-sphere reparametrization. The
    /// trend check cannot tell the two apart.
    ///
    /// The absolute `e64 < 0.030` check is doing all of the discriminating:
    /// naive measures ~0.048 there and correctly fails; warped measures 0.0236
    /// and passes.
    ///
    /// Both assertions stay. The trend check is still a real regression guard —
    /// it fails if the projection stops being smooth at all, which the absolute
    /// check at a single N would not see — it is simply not the guard the
    /// comment advertised, and a reader deciding what this test protects needs
    /// the two roles named correctly.
    #[test]
    fn adjacent_cell_distortion_falls_as_one_over_n() {
        fn tri(a: [f64; 3], b: [f64; 3], c: [f64; 3]) -> f64 {
            let (u, v) = (
                [b[0] - a[0], b[1] - a[1], b[2] - a[2]],
                [c[0] - a[0], c[1] - a[1], c[2] - a[2]],
            );
            let x = [
                u[1] * v[2] - u[2] * v[1],
                u[2] * v[0] - u[0] * v[2],
                u[0] * v[1] - u[1] * v[0],
            ];
            0.5 * (x[0] * x[0] + x[1] * x[1] + x[2] * x[2]).sqrt()
        }
        fn worst_adjacent_ratio(n: usize) -> f64 {
            let area = |i: usize, j: usize| {
                let p = |di: usize, dj: usize| {
                    face_unit(
                        0,
                        -1.0 + 2.0 * (i + di) as f64 / n as f64,
                        -1.0 + 2.0 * (j + dj) as f64 / n as f64,
                    )
                };
                let (q00, q10, q01, q11) = (p(0, 0), p(1, 0), p(0, 1), p(1, 1));
                tri(q00, q10, q11) + tri(q00, q11, q01)
            };
            let mut worst: f64 = 1.0;
            for i in 0..n {
                for j in 0..n {
                    let a = area(i, j);
                    for (di, dj) in [(1usize, 0usize), (0, 1)] {
                        if i + di < n && j + dj < n {
                            let r = a / area(i + di, j + dj);
                            worst = worst.max(r).max(1.0 / r);
                        }
                    }
                }
            }
            worst
        }
        let e64 = worst_adjacent_ratio(64) - 1.0;
        let e128 = worst_adjacent_ratio(128) - 1.0;
        assert!(e64 < 0.030, "adjacent-cell excess at N=64 was {e64}");
        let halving = e64 / e128;
        assert!(
            (1.7..2.3).contains(&halving),
            "excess must fall as 1/N; doubling N changed it by {halving}x"
        );
    }
}
