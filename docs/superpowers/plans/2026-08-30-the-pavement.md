# The Pavement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the walk band's 3-connected icosahedral triangle lattice with an 8-connected, tangent-warped cube-sphere quad lattice, so that a compass heading is a real edge rather than a dead-reckoned approximation.

**Architecture:** `Facet { face, path }` keeps its address space, packing, parent/child and zoom ladder untouched; only its *base geometry* changes from 20 triangles to 6 tangent-warped cube faces. The icosphere (`Geosphere`/`Vertex`) remains the field substrate for every `domains/` crate and does not move. Movement gains four diagonals at √2 cost with a no-corner-cutting rule; `windows/vessel/src/course.rs` is deleted outright.

**Tech Stack:** Rust 2024, `serde`/`serde_json`/`libm` only (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs` — do not add a dependency). `cargo nextest` for tests, doctests via the workspace doc target.

**Spec:** `docs/superpowers/specs/2026-08-30-the-pavement-design.md`

## Global Constraints

- **Decision block is 0506–0515.** Author records as `docs/decisions/0506-<slug>.md` upward. Do not mint a number outside the block.
- **No new dependencies.** The allowlist is `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml` `disallowed-types`).
- **Every transcendental routes through `hornvale_kernel::math`** (decision 0041), never `f64::tan`/`f64::atan` directly. `floor`/`sqrt` stay intrinsic.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and variant needs a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are this project's most common review finding.
- **Run `make gate-commit` before each commit.** Never `--no-verify`.
- **No seed label, stream label, draw, or stream consumption order may change.** The occupancy lattice consumes no randomness. If a task finds itself editing a `streams` module or adding a `Stream` draw, STOP — that is out of scope and means something has gone wrong.
- **`Geosphere`, `Vertex`, `--globe-level` and every crate under `domains/` are out of scope** except `domains/terrain/tests/suite/rill_probe.rs` (Task 9), which hardcodes the icosphere face-count formula in a test.
- **Run the suite ONCE and grep the file.** Never re-run to ask a second question — capture to a log, then grep it freely.

---

## File Structure

| file | responsibility | task |
|---|---|---|
| `kernel/src/cube.rs` **(new)** | The tangent-warped cube-sphere projection and its inverse — the one definition in the repository | 1 |
| `kernel/src/room.rs` | `Facet` base geometry: `corners`, `centroid`, `coord`, `containing`, `face_lattice` | 2 |
| `kernel/src/room.rs` | `neighbors` — interior, the 12 seams, the 8 corners | 3 |
| `windows/scene/src/region.rs` | Reconcile to the one projection (decision 0512) | 4 |
| `windows/vessel/src/lattice/mod.rs` | `HEADINGS` 4 → 8; the corner rule | 5 |
| `windows/vessel/src/session.rs` | `cell_delta` 8-way; `go` resolves a heading directly | 6 |
| `windows/vessel/src/course.rs` | **Deleted** | 6 |
| `windows/vessel/src/clock.rs` | Octile cost | 7 |
| `clients/game/bin/src/input.rs` | Diagonal bindings | 8 |
| `windows/vessel/src/agent.rs` | `walk_depth` +6 → +7 | 2 |

**Sequencing:** Tasks 1–4 are the kernel and must land green before movement. Tasks 5–7 depend on 3. Task 8 depends on 6. Task 9 (the epoch) depends on everything. Task 10 measures.

---

### Task 1: The tangent-warped cube-sphere projection

The one definition of the projection, in the kernel so both `room.rs` and `region.rs` can call it. Spec §2.0.

**Files:**
- Create: `kernel/src/cube.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod cube;` and re-export)
- Test: in-module `#[cfg(test)] mod tests` in `kernel/src/cube.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::math` (for `tan`, `atan`).
- Produces:
  - `pub const CUBE_FACES: [[[f64; 3]; 3]; 6]` — `(n, u, v)` per face, copied verbatim from `windows/scene/src/region.rs:23-30` so the face numbering is unchanged.
  - `pub fn face_unit(face: usize, a: f64, b: f64) -> [f64; 3]` — warped forward projection.
  - `pub fn locate(p: [f64; 3]) -> (usize, f64, f64)` — warped inverse.

- [ ] **Step 1: Write the failing round-trip test**

```rust
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
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-kernel -E 'test(locate_inverts_face_unit)'`
Expected: FAIL to compile — `cannot find function face_unit`.

- [ ] **Step 3: Implement the projection**

`CUBE_FACES` is copied verbatim from `region.rs:23-30`. The warp is applied to the face parameters before projection and undone after the inverse:

```rust
/// Warp a face parameter so cells come out even AFTER the sphere bends them.
/// The naive cube-sphere squashes the grid toward a face corner; this
/// pre-spreads it by the compensating amount. Measured: 5.16x max/min cell
/// area unwarped, 1.41x warped (spec section 2.0).
fn warp(t: f64) -> f64 {
    crate::math::tan(t * std::f64::consts::FRAC_PI_4)
}

/// The inverse of [`warp`].
fn unwarp(t: f64) -> f64 {
    crate::math::atan(t) / std::f64::consts::FRAC_PI_4
}

pub fn face_unit(face: usize, a: f64, b: f64) -> [f64; 3] {
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
```

`max_by` with `total_cmp` (last-wins on a tie) is `region.rs`'s existing tie-break; keep it identical so face assignment on a seam does not move.

- [ ] **Step 4: Run the round-trip test**

Run: `cargo nextest run -p hornvale-kernel -E 'test(locate_inverts_face_unit)'`
Expected: PASS.

- [ ] **Step 5: Write the distortion test — the acceptance criterion**

Spec §2.0 and §8. This asserts the *trend*, not a single value: a single-value assertion at one depth passes on a projection that is locally smooth and globally wrong.

```rust
/// Local distortion falls as 1/N. Nathan's acceptance criterion is LOCAL
/// ("as long as the area around the cursor itself is distorted minimally"),
/// so this measures ADJACENT cells, not the whole-face spread.
#[test]
fn adjacent_cell_distortion_falls_as_one_over_n() {
    fn tri(a: [f64; 3], b: [f64; 3], c: [f64; 3]) -> f64 {
        let (u, v) = ([b[0] - a[0], b[1] - a[1], b[2] - a[2]],
                      [c[0] - a[0], c[1] - a[1], c[2] - a[2]]);
        let x = [u[1] * v[2] - u[2] * v[1],
                 u[2] * v[0] - u[0] * v[2],
                 u[0] * v[1] - u[1] * v[0]];
        0.5 * (x[0] * x[0] + x[1] * x[1] + x[2] * x[2]).sqrt()
    }
    fn worst_adjacent_ratio(n: usize) -> f64 {
        let area = |i: usize, j: usize| {
            let p = |di: usize, dj: usize| {
                face_unit(0,
                    -1.0 + 2.0 * (i + di) as f64 / n as f64,
                    -1.0 + 2.0 * (j + dj) as f64 / n as f64)
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
```

- [ ] **Step 6: Run both**

Run: `cargo nextest run -p hornvale-kernel -E 'test(cube)'`
Expected: PASS, both tests.

- [ ] **Step 7: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add kernel/src/cube.rs kernel/src/lib.rs
git commit -m "feat(kernel): the tangent-warped cube-sphere projection, and the distortion trend it must hold"
```

---

### Task 2: `Facet` base geometry becomes the cube

**Files:**
- Modify: `kernel/src/room.rs` — `Facet` doc, `corners` (`:367`), `centroid` (`:391`), `containing` (`:405`), `coord` (`:464`), `face_lattice`, `FaceLattice` (`:42`), `pack`'s face bound (`:330`)
- Modify: `windows/vessel/src/agent.rs:16-18` — `walk_depth`
- Test: `kernel/src/room.rs` in-module tests

**Interfaces:**
- Consumes: `crate::cube::{CUBE_FACES, face_unit, locate}` from Task 1.
- Produces:
  - `Facet::corners(&self) -> [[f64; 3]; 4]` (was `[[f64; 3]; 3]`)
  - `FaceLattice { x: i64, y: i64, scale: i64 }` — replaces `{ a, b, c, up, scale }`. The `up` orientation flag is **deleted**: a quad's four children are all the same handedness, which is why the triangle needed it and the quad does not.
  - `Facet::containing(position: [f64; 3], depth: u32) -> Facet` — signature unchanged.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn containing_round_trips_a_facets_own_centroid_at_every_depth() {
    for depth in [0u32, 1, 3, 6, 12, 13] {
        for face in 0..6u8 {
            let f = Facet { face, path: vec![0; depth as usize] };
            assert_eq!(Facet::containing(f.centroid(), depth), f);
        }
    }
}

#[test]
fn a_facet_has_four_corners_and_no_orientation_flag() {
    let f = Facet { face: 0, path: vec![1, 2, 3] };
    assert_eq!(f.corners().len(), 4);
    let l = f.face_lattice();
    assert_eq!(l.scale, 1 << 3);
    assert!(l.x >= 0 && l.x < l.scale && l.y >= 0 && l.y < l.scale);
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo nextest run -p hornvale-kernel -E 'test(a_facet_has_four_corners)'`
Expected: FAIL to compile — `corners()` returns 3 elements, `FaceLattice` has no `x`.

- [ ] **Step 3: Implement**

`path` digits stay `0..4` and are now `(hi_x << 1) | hi_y`. `face_lattice` walks the path accumulating integer coordinates — no transcendental, which preserves the cross-platform-stable relative-offset property `FaceLattice`'s own doc promises:

```rust
pub fn face_lattice(&self) -> FaceLattice {
    let (mut x, mut y, mut scale) = (0i64, 0i64, 1i64);
    for &d in &self.path {
        scale <<= 1;
        x = (x << 1) | i64::from(d >> 1);
        y = (y << 1) | i64::from(d & 1);
    }
    FaceLattice { x, y, scale }
}
```

`corners` maps the cell's four `(a, b)` parameters through `cube::face_unit`; `centroid` maps the cell's parameter-space centre through the same; `containing` calls `cube::locate` then bisects both parameters `depth` times, emitting `(hi_x << 1) | hi_y` at each step.

`pack`'s guard `if self.face >= 20` becomes `>= 6`. `MAX_DEPTH`, the leading-1 sentinel and the 2-bits-per-digit layout are untouched — that is the whole reason this campaign is affordable.

`walk_depth` becomes `ctx.globe_level() + 7` (spec §2.3 — cube depth 13 is 1.126 km per side against the icosphere's 1.08 km effective step; depth 12 would be 2.251 km and silently double the ground covered per step, invalidating `clock.rs`'s authored 0.1-day `MoveTo`).

- [ ] **Step 4: Run the kernel suite once, then grep**

```bash
cargo nextest run -p hornvale-kernel > /tmp/pave-t2.log 2>&1; echo "exit=$?"
grep -E "^ *(FAIL|PASS)|Summary" /tmp/pave-t2.log
```

Expected: the two new tests PASS. **Other kernel tests WILL fail here** — anything asserting triangle-specific geometry. For each failure ask: was it asserting the *address space* (must still hold — fix the code, not the test) or the *triangle* (rewrite the test)? **Do not delete an assertion you cannot classify; bring it to review.**

- [ ] **Step 5: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add kernel/src/room.rs windows/vessel/src/agent.rs
git commit -m "feat(kernel): a facet is a cube-sphere quad, and walk depth moves to globe_level + 7"
```

---

### Task 3: The neighbour walk — interior, 12 seams, 8 corners

The heart of the campaign. Spec §2.2.

**Files:**
- Modify: `kernel/src/room.rs` — `neighbors` (`:503`), `neighbors_memo` (`:819`)
- Create: `kernel/tests/suite/cube_adjacency.rs`; register it in `kernel/tests/suite.rs`

**Interfaces:**
- Produces: `Facet::neighbors(&self) -> Vec<Facet>` — **8 entries interior, 7 at a cube corner.** A fixed-size array cannot express the corner case; returning `Vec` makes the arity honest at the type level rather than padding with a sentinel a caller would have to know to skip. `neighbors_memo` mirrors the signature.

- [ ] **Step 1: Write the symmetry property test — the one that catches a rotation off by a turn**

```rust
/// Build a Facet from integer face-lattice coordinates: the inverse of
/// Task 2's `face_lattice`, interleaving the bits of i and j.
fn facet_at(face: u8, i: u64, j: u64, depth: u32) -> Facet {
    let mut path = Vec::with_capacity(depth as usize);
    for k in (0..depth).rev() {
        let hx = ((i >> k) & 1) as u8;
        let hy = ((j >> k) & 1) as u8;
        path.push((hx << 1) | hy);
    }
    Facet { face, path }
}

#[test]
fn adjacency_is_symmetric_across_every_seam() {
    for depth in [1u32, 2, 3, 4] {
        let mut checked = 0u64;
        for face in 0..6u8 {
            for i in 0..(1u64 << depth) {
                for j in 0..(1u64 << depth) {
                    let f = facet_at(face, i, j, depth);
                    for n in f.neighbors() {
                        assert!(
                            n.neighbors().contains(&f),
                            "depth {depth}: {f:?} -> {n:?} but not back"
                        );
                        checked += 1;
                    }
                }
            }
        }
        assert!(checked > 0, "depth {depth} checked nothing");
    }
}

#[test]
fn exactly_eight_cube_corners_have_seven_neighbours() {
    for depth in [1u32, 2, 3, 4] {
        let mut sevens = 0;
        for face in 0..6u8 {
            for i in 0..(1u64 << depth) {
                for j in 0..(1u64 << depth) {
                    let n = facet_at(face, i, j, depth).neighbors().len();
                    assert!(n == 7 || n == 8, "arity {n} is neither 7 nor 8");
                    if n == 7 {
                        sevens += 1;
                    }
                }
            }
        }
        assert_eq!(sevens, 8, "depth {depth}: expected exactly 8 corner cells");
    }
}
```

- [ ] **Step 2: Run and watch both fail**

Run: `cargo nextest run -p hornvale-kernel --test suite -E 'test(cube_adjacency)'`
Expected: FAIL — `neighbors()` still returns 3.

- [ ] **Step 3: Implement**

Three cases:

1. **Interior:** `(x±1, y±1)` filtered to `[0, scale)` on both axes.
2. **Seam:** a step leaving range on exactly one axis. Consult a 12-entry edge table giving `(other_face, rotation)` and re-express the coordinate on the neighbouring face.
3. **Corner:** a step leaving range on *both* axes. It exists only where the two constituent seam-steps agree on a destination; at the 8 cube corners they do not, and that cell simply yields no eighth neighbour.

**Derive the 12-entry edge table from `CUBE_FACES` at construction rather than hand-writing it**, and let the symmetry test above be what validates it. A hand-written rotation table is exactly where an off-by-one-turn bug hides, and it will pass every test that does not check symmetry across the seam.

- [ ] **Step 4: Run**

Run: `cargo nextest run -p hornvale-kernel --test suite -E 'test(cube_adjacency)'`
Expected: PASS both.

- [ ] **Step 5: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add kernel/src/room.rs kernel/tests/suite/cube_adjacency.rs kernel/tests/suite.rs
git commit -m "feat(kernel): eight neighbours, twelve seams, and eight corners that honestly have seven"
```

---

### Task 4: One projection in the repository (decision 0512)

**Files:**
- Modify: `windows/scene/src/region.rs` — delete its private `FACES` (`:23`), `param` (`:34`), `locate_on_cube` (`:47`), `face_unit` (`:77`); call `hornvale_kernel::cube` instead
- Modify: `windows/scene/src/region.rs:236` — the stale discrete-layer enumeration
- Create: `windows/scene/tests/suite/one_projection.rs`; register in `windows/scene/tests/suite.rs`

- [ ] **Step 1: Write the failing agreement test**

```rust
/// Spec section 2.0: there is ONE projection in this repository, not two that
/// happen to agree today.
#[test]
fn region_and_room_project_through_the_same_function() {
    for face in 0..6usize {
        for &(a, b) in &[(-0.7, 0.3), (0.0, 0.0), (0.55, -0.85)] {
            let p = hornvale_kernel::cube::face_unit(face, a, b);
            let f = hornvale_kernel::Facet::containing(p, 8);
            let (f2, _, _) = hornvale_kernel::cube::locate(f.centroid());
            assert_eq!(f2, face);
        }
    }
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo nextest run -p hornvale-scene --test suite -E 'test(one_projection)'`
Expected: FAIL to compile until `region.rs` exposes nothing conflicting.

- [ ] **Step 3: Replace `region.rs`'s private projection with calls into `hornvale_kernel::cube`.** `RegionAddr`'s field names, the JSON key order and `scene/tiles-region/v1`'s shape are untouched — only the numbers move.

- [ ] **Step 4: Fix the stale schema doc.** `region.rs:236` reads *"discrete layers (`ocean`, `biome`, `plate`) are nearest-vertex"* — three, where the code has at least five: `ocean` (`:431`), `water` (`:432`), `relief` (`:434`), plus `biome` and `plate`. `relief` arrived with The Legend's Task 4 without the doc following. The block above it declares "field order is the JSON key order and is contract", which makes a stale enumeration inside it read as authoritative.

- [ ] **Step 5: Run**

Run: `cargo nextest run -p hornvale-scene --test suite -E 'test(one_projection)'`
Expected: PASS. `windows/scene/tests/fixtures/region-seed-1-f0-l3.json` will now differ — leave it, Task 9 rebaselines.

- [ ] **Step 6: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add windows/scene/src/region.rs windows/scene/tests/suite/one_projection.rs windows/scene/tests/suite.rs
git commit -m "refactor(scene): one projection in the repository, and a doc that lists all five discrete layers"
```

---

### Task 5: Eight headings and the corner rule

**Files:**
- Modify: `windows/vessel/src/lattice/mod.rs:192-197` — `HEADINGS`, `neighbours`
- Create: `windows/vessel/tests/suite/corner_rule.rs`; register in `windows/vessel/tests/suite.rs`

**Interfaces:**
- Produces: `pub const HEADINGS: [(i32, i32); 8]`; `pub fn neighbours(cell: Cell) -> [Cell; 8]`; `pub fn diagonal_is_blocked(lattice: &Lattice, from: Cell, d: (i32, i32)) -> bool`.

- [ ] **Step 1: Write the corner-rule test IN BOTH DIRECTIONS**

A test asserting only refusal is structurally blind to over-refusal — the failure the root `CLAUDE.md` names at its seam-guard STALE-DECL rule ("a one-directional acknowledgement can only ever be satisfied, so it rots") and that decision 0456 states as a rule. **Corrected 2026-08-31: this cited `docs/CLAUDE.md`, which says nothing about one-directional checks — it is about idea-registry drift discipline. The bad citation propagated into an implementer's code before being caught.**

```rust
#[test]
fn a_diagonal_is_refused_only_when_both_flanks_are_walls() {
    // Both flanks walled: the step would pass through a point. Refused.
    let l = lattice_with_walls(&[Cell(1, 0), Cell(0, 1)]);
    assert!(diagonal_is_blocked(&l, Cell(0, 0), (1, 1)));

    // One flank open: brushing a single corner is physical. PERMITTED.
    let l = lattice_with_walls(&[Cell(1, 0)]);
    assert!(!diagonal_is_blocked(&l, Cell(0, 0), (1, 1)));
    let l = lattice_with_walls(&[Cell(0, 1)]);
    assert!(!diagonal_is_blocked(&l, Cell(0, 0), (1, 1)));

    // Open ground. Permitted.
    let l = lattice_with_walls(&[]);
    assert!(!diagonal_is_blocked(&l, Cell(0, 0), (1, 1)));
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(corner_rule)'`
Expected: FAIL to compile — `diagonal_is_blocked` does not exist.

- [ ] **Step 3: Implement.** `HEADINGS` gains the four diagonals *after* the four orthogonals, so any existing code indexing the first four is unaffected. `diagonal_is_blocked` returns true only when *both* flanking orthogonal cells are impassable.

**Which bands this applies to, stated because the answer is not symmetric.**
The corner rule governs the two `Cell`-addressed lattice bands — interiors
(band A) and the underground level — because those are the only bands with
*walls* for two of them to meet at a point. **The walk band has no walls**
(`go` performs no passability check at all; decision 0141 recorded that water
was already walkable), so `diagonal_is_blocked` must NOT be wired into
`Session::go`. Doing so would invent a refusal on a path that has never had
one — the precise mistake spec §2.2 warns against about the cube corner, in a
different place.

- [ ] **Step 4: Run**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(corner_rule)'`
Expected: PASS.

- [ ] **Step 5: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/lattice/mod.rs windows/vessel/tests/suite/corner_rule.rs windows/vessel/tests/suite.rs
git commit -m "feat(vessel): eight headings, and a diagonal refused only through a two-walled corner"
```

---

### Task 6: `go` resolves a heading directly; `course.rs` is deleted

**Files:**
- Modify: `windows/vessel/src/session.rs` — `cell_delta` (`:6578`), `go` (`:3573`), `back` (`:3632`), the `Session.course` field
- Delete: `windows/vessel/src/course.rs`, `windows/vessel/tests/suite/course_properties.rs`
- Modify: `windows/vessel/tests/suite.rs`, `windows/vessel/src/lib.rs`

- [ ] **Step 1: Write the failing test — a held heading walks true**

```rust
#[test]
fn four_norths_and_four_souths_return_you_to_where_you_started() {
    let mut s = session_at_flagship();
    let start = s.position();
    for _ in 0..4 {
        s.handle("n");
    }
    assert_ne!(s.position(), start, "north must actually move");
    for _ in 0..4 {
        s.handle("s");
    }
    assert_eq!(s.position(), start, "n*4 then s*4 must be the identity");
}
```

This is **false today** — the rhumb course tacks 206.7°/153.6° and never returns — and must be true after.

- [ ] **Step 2: Run and watch it fail**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(four_norths)'`
Expected: FAIL — position after the round trip is not the start.

- [ ] **Step 3: Implement.** `cell_delta` returns `Some` for all eight compass points. `go` maps the heading to one of `Facet::neighbors()` by bearing and steps it — no `Course`, no reckoned point, no `rhumb_advance`, no `POLE_LIMIT`. Delete `course.rs`, the `course` field, and `back`'s course-clearing.

- [ ] **Step 4: Run**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(four_norths)'`
Expected: PASS.

- [ ] **Step 5: Refuse the cube corner's absent diagonal, with geometry as the reason**

Spec §6, and the self-review found no task covered it — Task 3 makes the arity
honest, but a player standing on one of the 8 corner cells and typing the
absent diagonal needs a sentence. Follow the precedent both existing refusals
set (`UNDERGROUND_DIAGONAL_REFUSAL`, `INDOOR_DIAGONAL_REFUSAL`): name the
geometry, never lodge a parse complaint, and name the bearings that do work.

```rust
/// What a step into a cube corner's absent eighth neighbour says. Eight cells
/// in the world have seven neighbours because three cube faces meet at a
/// point; the missing bearing is not a wall and not a bad word, it is a
/// direction that does not exist there. Precedent for stating the geometry
/// rather than refusing the token: `INDOOR_DIAGONAL_REFUSAL`'s own doc.
const CORNER_BEARING_REFUSAL: &str =
    "The land folds away to nothing that way; no ground lies in that direction      at all. The other seven bearings hold.";
```

Test it in both directions, the same discipline Task 5 uses: a corner cell
refuses exactly one bearing, and an interior cell refuses none.

```rust
#[test]
fn only_a_cube_corner_refuses_a_bearing_and_it_refuses_exactly_one() {
    let interior = session_at_flagship();
    assert_eq!(refused_bearings(&interior).len(), 0);
    let corner = session_at_cube_corner();
    assert_eq!(refused_bearings(&corner).len(), 1);
}
```

- [ ] **Step 6: Record the dissolution in the commit message.** `course_properties` measured drift from an ideal rhumb; afterwards there is no approximation left to drift from. It is **dissolved, not passed** (spec §3.4) — the same distinction The Quadrat drew about its own H2. A deleted test whose subject vanished must not read in the log as a test that was merely removed.

- [ ] **Step 7: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add -A windows/vessel
git commit -m "feat(vessel): a heading is an edge now — the rhumb course is deleted, its suite dissolved not passed"
```

---

### Task 7: Octile cost

**Files:**
- Modify: `windows/vessel/src/clock.rs:163` (`base_cost`), `:238` (`cost_of`)
- Modify: `cost_of` call sites in `windows/vessel/src/session.rs`
- Create: `windows/vessel/tests/suite/octile_cost.rs`; register in `windows/vessel/tests/suite.rs`

**Interfaces:**
- Produces: `pub fn cost_of(action: &Action, mass_kg: f64, terrain_factor: f64, step_factor: f64) -> TickSpan`.

**A design point the spec left open, settled here.** `Action::MoveTo(Facet)` cannot know whether the step was diagonal, and folding √2 into `terrain_factor` would put two independently-varying quantities — ground difficulty and step geometry — into one number, which is the defect decision 0143 exists to prevent one ladder over. So `cost_of` takes a **fourth parameter named for what it is**. Considered and rejected: `Action::MoveTo { to, diagonal: bool }`, which would put a geometry fact inside a planner-facing enum that `action.rs`'s A* also constructs.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_diagonal_step_costs_root_two_orthogonal_steps() {
    let a = Action::MoveTo(some_facet());
    let orth = cost_of(&a, 70.0, 1.0, 1.0).ticks() as f64;
    let diag = cost_of(&a, 70.0, 1.0, std::f64::consts::SQRT_2).ticks() as f64;
    let ratio = diag / orth;
    assert!(
        (ratio - std::f64::consts::SQRT_2).abs() < 0.005,
        "diagonal/orthogonal was {ratio}, wanted sqrt(2)"
    );
}
```

- [ ] **Step 2: Run and watch it fail**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(octile_cost)'`
Expected: FAIL to compile — `cost_of` takes three arguments.

- [ ] **Step 3: Implement.** **CORRECTED 2026-08-31 — the rule below is WRONG and Task 7 rightly
      refused it.** `go` resolves a heading through `heading_rose`, a greedy one-to-one assignment, so
      the compass WORD does not determine the step's geometry: measured in-tree, 1216/4800 assignments
      mismatch (25.3%) — 0% on the four equatorial faces, 74.5%/77.5% on the two polar ones. A
      word-keyed rule would also have covered neither `back` nor the creature walk. Derive the factor
      from the step's ACTUAL GEOMETRY in `Session::charge` instead. Original text follows:
      `cost_of` multiplies by `step_factor`; `Session::go` passes `std::f64::consts::SQRT_2` for a diagonal heading and `1.0` otherwise. `base_cost` is unchanged — the flat 10,000 ticks remains the orthogonal unit.

- [ ] **Step 4: Run**

Run: `cargo nextest run -p hornvale-vessel --test suite -E 'test(octile_cost)'`
Expected: PASS.

- [ ] **Step 5: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/clock.rs windows/vessel/src/session.rs windows/vessel/tests/suite/octile_cost.rs windows/vessel/tests/suite.rs
git commit -m "feat(vessel): a diagonal costs root two, so a zigzag buys no ground"
```

---

### Task 8: Diagonal input, never required

**Files:**
- Modify: `clients/game/bin/src/input.rs` (routing, and its own test module)

Spec §3.5. Arrows stay 4-way and primary — The Stride's ratified routing is untouched. Add `y`/`u`/`b`/`n` (vi-keys) for the four diagonals. The CLI's `ne`/`nw`/`se`/`sw` words already parse through `parse_compass` and need no change.

- [ ] **Step 1: Write the failing routing test.** Each of `y`/`u`/`b`/`n` in `Focus::Walk` produces the corresponding diagonal `Action::Move`; every other printable key still bounces to the CLI (totality across all three focus states is preserved, as The Stride requires).
- [ ] **Step 2: Run and watch it fail** — `make game-check`
- [ ] **Step 3: Implement** the four bindings.
- [ ] **Step 4: Run** — `make game-check`, expect PASS.
- [ ] **Step 5: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add clients/game/bin/src/input.rs
git commit -m "feat(game): diagonals are bound and never required"
```

---

### Task 9: The epoch

**Files:**
- Modify: `domains/terrain/tests/suite/rill_probe.rs:59,209-213`
- Modify: the reload notice consulted by `cli/src/streams.rs`'s `reload_notice`
- Regenerate: everything in `docs/generated-paths.txt`
- Create: `docs/decisions/0506-*.md` … `0512-*.md`

- [ ] **Step 1: Fix `rill_probe.rs`.** The one file under `domains/` this campaign touches. `WALK_DEPTH` becomes 13, and `20u64 << (2 * WALK_DEPTH)` — the icosphere face-count formula — becomes `6u64 << (2 * WALK_DEPTH)`.

- [ ] **Step 2: Verify the world itself did not move.** The load-bearing check of the entire campaign:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/pave-42.json
python3 -c "import json; w=json.load(open('/tmp/pave-42.json')); print('facts:', len(w['ledger']['facts']))"
```

Compare against the same command on `main`. Expected: **identical fact count**. A different count means a seed label, draw or stream order moved — **STOP**, that is out of scope and something is wrong.

- [ ] **Step 3: Rebaseline**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

- [ ] **Step 4: Read the diff before accepting it.** A branch table, not a prediction:
  - `book/src/gallery/` moved → **expected** (room ids change; this is the epoch).
  - `docs/audits/type-audit-report.md` moved → **expected** (`FaceLattice`'s fields changed at a pub boundary).
  - `docs/digest/` moved → **expected** (new decision records).
  - `clients/game/core/tests/fixtures/` moved → **expected** (session snapshots carry positions).
  - **A census column moved → STOP.** The census is a separate refresh on lefford at pre-merge close and is not part of `make rebaseline`.

- [ ] **Step 5: Author decisions 0506–0512** per spec §9. **0506 must state that it AMENDS 0287 rather than superseding it** — 0287's core (a tile at rung *d* is a facet at depth *d*) survives untouched; only the corner-is-a-vertex corollary retires. Verified: `grep -cie 'vertex|vertices|corner'` over the 0287 record returns 0.

- [ ] **Step 6: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "chore(the-pavement): the epoch — regenerated artifacts and the decision block"
```

---

### Task 10: The preregistered measurements

**Files:**
- Create: `windows/vessel/examples/pavement_probe.rs`
- Create: `book/src/chronicle/the-pavement.md`, `docs/retrospectives/the-pavement.md`

Spec §7. H1 and H2 each need their **positive control** — a probe that cannot reproduce the old defect is not measuring what it claims.

- [ ] **Step 1: H1 — a held heading walks true.** 200 seed-42 start cells, 500 steps of `n`, assert every step stays within 0.5 cell of the starting meridian. **Positive control:** the same probe run against `main`'s triangular lattice must reproduce the unbounded drift (~0.086 step-lengths per step, 172.6 at 2,000 steps). Record both numbers.

- [ ] **Step 2: H2 — octile closes the exploit.** `k` diagonal cells cost within 0.5% of `√2·k` orthogonal steps, across the body-mass and climb range. **Positive control:** with `step_factor` forced to 1.0, the 41% discrepancy must reappear.

- [ ] **Step 3: H3a — the dissolved test.** `clients/game/bin/src/plate.rs:1918`'s `assert_eq!(agree, total)` over `tile.vertex == point_vertex`. Report it **dissolved, not passed** — its premise, that a facet's corners are geosphere vertices, no longer exists. It does not get slower; it stops having a subject.

- [ ] **Step 4: H3b — the plate-draw budget.** Take the baseline from The Legend's Task 11 (warm redraw, 200×200, coarsest rung, idle box, ≥3 replicates, 0.20 ms bar; re-measured rather than averaged above a 1.4× spread). **Do not quote The Quadrat's published figures** — it measured 65.3/65.6/70.3/91.5 ms for one path, a 1.40× spread it deliberately refused to pin. No preregistered threshold: this measurement informs a decision rather than passing or failing.

- [ ] **Step 5: Close the campaign.** Chronicle entry, a freshness sweep of stale chapters, re-score any Confidence Gradient bet this moves (decision 0030), and the retrospective — promoting `.superpowers/sdd/followups.md` into it **before** teardown, since that scratch is git-ignored and dies with the worktree.

- [ ] **Step 6: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "docs(the-pavement): the four measurements, two positive controls, and one dissolution"
```
