# 0513. A quadrilateral lattice carries no third axis or orientation flag

**Status:** Accepted (2026-08-31) · **Decider:** Nathan · **Relates:**
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md),
[0356](0356-the-external-clients-are-retired.md)

In the context of The Pavement replacing the room mesh's triangular
icosphere faces with cube-sphere quads, facing a live wire schema
(`scene/surrounds/v1`/`v2`) whose per-cell lattice address carried a
three-axis barycentric offset and a triangle-orientation flag that a quad
has no equivalent for, we decided that **`SurroundsCell::w` and
`SurroundsCell::up` are permanently `null` from The Pavement forward,
while `u`/`v` keep carrying real per-cell values**, accepting that a
client which inferred "all four lattice fields move together" from the old
triangular mesh now reads a schema where two of the four never do.

## Context

A triangular face needed three barycentric-style coordinates to address a
cell unambiguously, plus a flag for which of the two possible triangle
windings a given cell was — the `up` field. A cube-sphere quad's four
children all share their parent's handedness (`kernel/src/room.rs`'s
`FaceLattice` doc: "there is no orientation flag any more, and its absence
is the point"), and a quad's own face-local address is the pair `(u, v)`
alone — there is no third axis to report at all.

`windows/scene/src/surrounds.rs` (Task 2) already writes `w: None, up:
None` unconditionally for every cell, seam or not — the mesh underneath it
changed; the wire never had a way to say so. That is the right shape: the
alternative, dropping the fields outright, is a schema change with no
migration path for a reader still expecting the keys to exist, and this
project's version-locking (0099) does not extend to a *live* wire schema a
client parses turn by turn.

## What was decided

- **`w` and `up` are always `null`**, on every `SurroundsCell`, seam or
  not — not conditionally, not only where the old mesh would have made
  them meaningless.
- **`u` and `v` are unaffected.** They still carry the real per-cell
  lattice offset on a non-seam cell and `null` on a seam cell, exactly as
  before. A reader that inferred "the four lattice fields are one unit —
  all `Some` together or all `null` together" from the triangular mesh was
  relying on an invariant this record retires: `u`/`v` and `w`/`up` now
  vary independently, and only the latter pair is pinned to always-`null`.
- **No version bump.** Decision 0356 retired the external clients that
  decision 0055 built the additive-or-versioned discipline for
  (`scene/system/v1`, `scene/tiles/v1`, and siblings including
  `scene/surrounds`, consumed by the now-gone Orrery). With no reader
  outside this repository, the cross-repo contract that would once have
  forced a `scene/surrounds/v2`-style bump for this change has lapsed —
  see `CLAUDE.md`'s "THE EXTERNAL CLIENTS ARE RETIRED" paragraph. This
  record is the place a future reader who finds `w`/`up` always null with
  no matching version bump can look to understand why that was allowed.

## Consequences

- **A wire reader must not treat `w`/`up` as signal.** They are retained
  keys for schema stability, not retired ones, so `serde`'s default
  behaviour (present, `null`) is correct and no reader should special-case
  their absence.
- **`clients/game/core/src/schema.rs`'s `ChartCell::w`/`::up` docs and
  `clients/game/core/src/chart.rs`'s `chart_cell` test helper are updated
  in the same change** that ratifies this record, so the mirror does not
  claim a non-seam cell carries real values in those two fields.
- **Retiring the fields outright remains available later**, the same way
  any wire field may be dropped — as its own additive-or-versioned-style
  decision, should a future campaign judge the dead weight worth the
  migration. This record only settles that carrying them as permanent
  `null` is the correct shape *now*, not that they must stay forever.

## See also

- `kernel/src/room.rs`'s `FaceLattice` doc (the geometric argument: a quad
  has no orientation bit left to carry).
- `windows/scene/src/surrounds.rs`'s `SurroundsCell::w`/`::up` doc comments
  (Task 2, The Pavement).
- `CLAUDE.md` "Architecture" — the external-clients-retired correction
  this record's no-version-bump clause depends on.
