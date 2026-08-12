# The Ford, stage 2 — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let a room report its relation to the nearest river by carrying the
*measured quantity* — a signed distance and that room's band edges — rather
than one consumer's classification of it, and let crossing be asked as a
traversal query.

**Architecture:** The bank convention (left/right facing downstream) lands in
`domains/terrain` beside the network that defines it. `windows/locale` appends
trailing keys to `locale/room/v2` and exposes the ordinal and the crossing
query as functions. **There is no epoch**: the schema tag is unchanged and keys
append, the shape main used for `cave`.

**Tech Stack:** Rust 2024, `serde`/`serde_json`/`libm` only. `cargo nextest`.
No new crates.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml`).
- **No wall-clock time**, including `std::time::Instant` in test code.
- **Float sorting uses `total_cmp`** with deterministic tie-breaks.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a
  one-line doc comment.
- **Type-audit tags on every `pub`-boundary primitive.** Ratified `bare-ok`
  classes are exactly: `ratio`, `count`, `index`, `constructor-edge`,
  `envelope`, `identifier-text`, `prose`, `artifact`, `diagnostic-value`,
  `render-internal`, `flag`. **Radians and unit-sphere positions take
  `pending(wave-1: <name>)`.** Regenerate `docs/audits/type-audit-report.md` in
  the same commit whenever a `pub` boundary moves.
- **Transcendentals via `kernel/src/math.rs`**; `floor`/`sqrt` stay intrinsic.
- **Quantize at emit only** — never in the compute path.
- **All distances are angular** (radians on the unit sphere) or in cell-edge /
  room-edge fractions. There is no planet radius; do not introduce one.
- **Never serialize a polyline index** — it is build-order, not a contract.
- **No new seed draws and no new stream label.** Stage 2 is a pure read.
- **`cargo fmt` last.** Scope tests with `-p`; the controller runs `make gate`.

## File Structure

| File | Responsibility |
|---|---|
| `domains/terrain/src/channel.rs` (modify) | The bank convention: which side of a channel a point is on, facing downstream. |
| `windows/locale/src/lib.rs` (modify) | The three appended keys, their serializers, and the ordinal/crossing functions. |
| `domains/terrain/tests/channel_properties.rs` (modify) | The stable-sign property (H2-1). |
| `windows/locale/tests/` (create `water_reading.rs`) | H2-2, H2-3, H2-4. |

---

### Task 1: The bank convention

Stage 1's sign is relative to the winning segment's travel direction, and the
line index that referent depends on is build-order and unserializable. This
task gives the sign a durable meaning: **left/right facing downstream**.

**Files:**
- Modify: `domains/terrain/src/channel.rs`
- Test: `domains/terrain/tests/channel_properties.rs`

**Interfaces:**
- Consumes: `ChannelNetwork::nearest_line(&self, position: [f64;3]) -> Option<(usize, f64)>`, `ChannelNetwork::polylines`, `ChannelNetwork::run_cells`, `TectonicGlobe.downhill`.
- Produces: `ChannelNetwork::bank_signed_distance(&self, position: [f64; 3]) -> Option<f64>` — angular distance to the nearest channel, **positive on the left bank facing downstream**, `None` when the network is empty.

- [ ] **Step 1: Read how the run's direction is already established**

Read `ChannelNetwork::build` in `domains/terrain/src/channel.rs`. Runs are
constructed by following `downhill` from a head, so **`run_cells[i]` is already
in downstream order** and `polylines[i].points` is in the same order. That
ordering is the referent — you do not need the globe again. Confirm this by
reading, and say so in your report; if it is not true, STOP and report,
because the whole task rests on it.

- [ ] **Step 2: Write the failing tests**

```rust
/// The sign means left-of-downstream, and it is the SAME answer whichever
/// polyline index happens to win — that is the whole point of the referent
/// change. Two points mirrored across a segment must differ in sign and
/// agree in magnitude.
#[test]
fn the_bank_sign_is_left_of_downstream_and_mirrors_exactly() {
    let terrain = seed_42_terrain();
    let net = terrain.channels();
    let (i, j) = first_interior_vertex(net);
    let (a, b) = (net.polylines[i].points[j], net.polylines[i].points[j + 1]);
    let (left, right) = mirrored_pair_across(a, b, 2.0e-4);
    let dl = net.bank_signed_distance(left).expect("network is non-empty");
    let dr = net.bank_signed_distance(right).expect("network is non-empty");
    assert!(dl * dr < 0.0, "expected opposite banks, got {dl} and {dr}");
    assert!((dl.abs() - dr.abs()).abs() < 1e-12, "{dl} vs {dr}");
    assert!(dl > 0.0, "the +normal side must be LEFT of travel, got {dl}");
}

/// H2-1: the sign is stable across builds. Two independently built terrains
/// from the same seed must agree on the sign for every sampled room.
#[test]
fn the_bank_sign_is_identical_across_two_builds() {
    let a = seed_42_terrain();
    let b = seed_42_terrain();
    let (na, nb) = (a.channels(), b.channels());
    let mut checked = 0usize;
    for p in sample_positions_near_channels(na, 400) {
        let (sa, sb) = (na.bank_signed_distance(p), nb.bank_signed_distance(p));
        assert_eq!(sa.map(f64::to_bits), sb.map(f64::to_bits), "diverged at {p:?}");
        checked += 1;
    }
    assert!(checked >= 200, "only {checked} positions sampled; the sweep is too thin");
}
```

Write `seed_42_terrain`, `first_interior_vertex`, `mirrored_pair_across` and
`sample_positions_near_channels` as helpers in the test file. Match the
existing file's construction style — read it first. `mirrored_pair_across`
must be **distance-preserving**: reflect across the great circle through the
segment, so both points are equidistant. `channel_golden.rs`'s module doc
records this recipe and names the non-isometric trap; read it before writing
your own.

- [ ] **Step 3: Run to verify they fail**

Run: `cargo test -p hornvale-terrain --test channel_properties bank_sign`
Expected: FAIL — no method `bank_signed_distance`.

- [ ] **Step 4: Implement**

`nearest_line` already gives the winning line and the unsigned distance.
The sign is the side of that line's **local downstream direction**: take the
winning segment `(a, b)` in `points` order (which Step 1 confirmed is
downstream), and the point is on the left iff `dot(p, normalize(cross(a, b)))`
is positive — the same left-positive convention `SphericalPolyline`'s doc
already states, now anchored to downstream rather than to whatever order the
run happened to be built in.

Add a one-line doc comment stating the convention in words a reader will
recognize ("left bank facing downstream"), and a `type-audit:
pending(wave-1: return), pending(wave-1: position)` tag.

- [ ] **Step 5: Run to verify they pass**

Run: `cargo test -p hornvale-terrain --test channel_properties`
Expected: PASS, and no existing test in that file reddens.

- [ ] **Step 6: `cargo fmt`, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-terrain --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add domains/terrain/src/channel.rs domains/terrain/tests/channel_properties.rs docs/audits/type-audit-report.md
git commit -m "feat(terrain): the bank convention — left of downstream

The sign is load-bearing (a ford is a sign change) but stage 1 referenced it
to the winning line's build order, which is not serializable. Downstream is."
```

---

### Task 2: The three appended keys

**Files:**
- Modify: `windows/locale/src/lib.rs`
- Test: create `windows/locale/tests/water_reading.rs`

**Interfaces:**
- Consumes: `ChannelNetwork::bank_signed_distance` (Task 1); `GeneratedTerrain::channels()`; `hornvale_terrain::channel::band_edges(drainage, slope, cell_edge) -> [f64; 4]`.
- Produces: three `Locale` fields — `channel_distance: Option<f64>`, `channel_bands: Option<[f64; 4]>`, `resolution: Resolution` — plus `pub struct Resolution` with per-field resolution names.

- [ ] **Step 1: Read the append precedent**

Read `windows/locale/src/lib.rs` around the `Locale` struct. `cave:
Option<CaveKind>` was appended after `exits` with a doc comment explaining that
appending keeps a pre-existing document byte-identical up to the new key, and
`serialize_cave_kind` (line ~186) shows how a domain type that cannot derive
`Serialize` is emitted. **Follow both patterns exactly.** Append after `cave`;
do not insert.

- [ ] **Step 2: Write the failing tests**

```rust
/// H2-2 — appending is byte-clean. A document is byte-identical to its
/// pre-stage-2 form up to the first new key. This is the no-epoch claim,
/// asserted rather than assumed.
#[test]
fn the_document_is_byte_identical_up_to_the_first_new_key() {
    for room in sample_rooms(200) {
        let json = render_locale_json(&room);
        let cut = json.find("\"channel_distance\"").expect("new key present");
        let prefix = &json[..cut];
        assert_eq!(prefix, expected_prefix_for(&room), "room {room:?} moved before the new keys");
    }
}

/// The schema tag did NOT move. If this fails, an epoch happened by accident.
#[test]
fn the_schema_tag_is_still_v2() {
    assert_eq!(hornvale_locale::ROOM_SCHEMA, "locale/room/v2");
}

/// H2-3 — the ordinal is reproducible from what the document carries. If a
/// consumer cannot recompute the classification from the stored quantity,
/// the document is storing the wrong thing.
#[test]
fn the_band_recomputes_from_the_stored_distance_and_edges() {
    let mut checked = 0usize;
    for room in sample_rooms(400) {
        let loc = describe(&room);
        let (Some(d), Some(edges)) = (loc.channel_distance, loc.channel_bands) else { continue };
        let recomputed = Transverse::from_band(hornvale_kernel::band(d, &edges));
        assert_eq!(recomputed, transverse_of(&room), "room {room:?}");
        checked += 1;
    }
    assert!(checked >= 100, "only {checked} rooms carried a reading; sweep too thin");
}
```

`expected_prefix_for` must come from a **committed fixture** captured before
this task's field additions, not from re-rendering with the new code — a
prefix compared against itself proves nothing. Capture it as the first step of
Step 4 below.

- [ ] **Step 3: Run to verify they fail**

Run: `cargo test -p hornvale-locale --test water_reading`
Expected: FAIL — no field `channel_distance`.

- [ ] **Step 4: Capture the pre-change fixture, then implement**

First, on the **unmodified** code, render the sampled rooms and commit the
prefixes as a fixture under `windows/locale/tests/fixtures/`. Then add:

```rust
    /// Signed angular distance to the nearest river channel — **positive on
    /// the left bank facing downstream** — or `None` where the world has no
    /// channel network. Quantized at emit. Appended after `cave` rather than
    /// inserted, so a document built before this field existed is still
    /// byte-identical up to this new trailing key.
    /// type-audit: pending(wave-1: channel_distance)
    #[serde(serialize_with = "serialize_opt_quantized")]
    pub channel_distance: Option<f64>,
    /// This room's four band edges — channel, bank, floodplain, terrace — in
    /// the same angular units as `channel_distance`. A consumer bands the
    /// distance for its own question rather than receiving one classification.
    /// type-audit: pending(wave-1: channel_bands)
    #[serde(serialize_with = "serialize_opt_quantized_array")]
    pub channel_bands: Option<[f64; 4]>,
    /// Which of this document's fields are decided at canonical-cell
    /// resolution and which at channel resolution (decision 0123).
    pub resolution: Resolution,
```

`Resolution` is a small `Serialize` struct of `&'static str` field-name →
resolution-name pairs. **Read decision 0123's own shape first**
(`docs/decisions/0123-disclose-a-resolution-rather-than-refine-a-field.md`)
and match the precedent it establishes rather than inventing a second shape —
the spec's §10.2 risk names exactly this.

- [ ] **Step 5: Run to verify they pass**

Run: `cargo test -p hornvale-locale`
Expected: PASS, no existing locale test reddens.

- [ ] **Step 6: `cargo fmt`, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-locale --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add windows/locale/src/lib.rs windows/locale/tests/ docs/audits/type-audit-report.md
git commit -m "feat(locale): the room carries the quantity, not the classification

Three trailing keys on locale/room/v2 — no epoch. A consumer bands the
distance for its own question; a stored class would go stale under
seasonality while a stored distance never does."
```

---

### Task 3: Crossing is a traversal query

**Files:**
- Modify: `windows/locale/src/lib.rs`
- Test: `windows/locale/tests/water_reading.rs`

**Interfaces:**
- Consumes: everything from Tasks 1–2.
- Produces: `pub fn crossing_between(&self, a: &RoomAddr, b: &RoomAddr) -> Crossing` and `pub enum Crossing { NotACrossing, Fordable, Impassable }`.

- [ ] **Step 1: Write the failing tests**

```rust
/// A ford is a SIGN CHANGE — the state machine's permitted left->right
/// transition. Two rooms on the same bank are not a crossing at all.
#[test]
fn same_bank_neighbours_are_not_a_crossing() {
    let (a, b) = same_bank_adjacent_pair();
    assert_eq!(crossing_between(&a, &b), Crossing::NotACrossing);
}

/// H2-4 — fords exist and are not everywhere. The denominator is TRANSECTS
/// OF THE NETWORK, deliberately: a population of "adjacent pairs whose sign
/// differs" would BE the ford set by construction (a one-step sign change is
/// only possible when the channel is narrower than one step), and the
/// fraction would read ~1.0 no matter what the world looked like.
#[test]
fn the_fordable_fraction_of_the_network_is_within_its_interval() {
    let transects = network_transects(400);
    let fordable = transects.iter().filter(|t| t.crossing == Crossing::Fordable).count();
    let frac = fordable as f64 / transects.len() as f64;
    assert!(
        (0.10..=0.70).contains(&frac),
        "fordable fraction {frac} outside the spec's [0.10, 0.70]"
    );
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-locale --test water_reading crossing`
Expected: FAIL — no function `crossing_between`.

- [ ] **Step 3: Implement**

`NotACrossing` when the two rooms' `channel_distance` share a sign or either is
`None`. Otherwise `Fordable` iff **both** hold, per spec §8:

- the full channel width — twice `channel_bands[0]`, which is the **half**-width
  — is less than one room edge at the pair's depth; and
- the nearest line's drainage is below
  `hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE`.

Otherwise `Impassable`. Derive the room edge at depth from the room mesh, not
from a literal.

- [ ] **Step 4: Run, and report the measured fraction**

Run: `cargo test -p hornvale-locale --test water_reading 2>&1 | tee /tmp/hv-ford2-h24.txt`

**Take the branch, do not tune:**
- **inside [0.10, 0.70]** → confirmed; report the number.
- **near 1.0** → suspect the denominator collapsed back into the criterion.
  Report it as a vacuity finding, not a success.
- **outside, either way** → a finding. H2-4 is a **late freeze** and carries
  less evidential weight than a preregistration; say so rather than adjusting
  the criterion to land inside.

- [ ] **Step 5: `cargo fmt`, clippy, type-audit, commit with the number**

```bash
cargo fmt
cargo clippy -p hornvale-locale --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add windows/locale/src/lib.rs windows/locale/tests/water_reading.rs docs/audits/type-audit-report.md
git commit -m "feat(locale): crossing is a traversal query

<paste the measured fordable fraction here>"
```

---

### Task 4: Close stage 2

- [ ] **Step 1:** `make gate` (foreground, `timeout: 3600000`). Report `rc`.
  If red, **stop and report BLOCKED**. Budget ~8 min.
- [ ] **Step 2:** `make rebaseline`, then the drift branch table:

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

  - **`book/src/reference/locale-seed-42.json` moved** → expected, it carries
    the new keys. Commit it.
  - **`docs/audits/` moved** → expected. Commit in the same commit.
  - **`book/src/gallery/` moved** → **STOP.** Stage 2 consumes no draws; a
    moved gallery means the determinism contract broke.
  - **`book/src/laboratory/` moved** → **STOP.** No metric was added.
- [ ] **Step 3: Confirm no epoch leaked.**

```bash
grep -rn 'locale/room/v' windows/locale/src/lib.rs | grep -o 'v[0-9]' | sort -u
```
  Expected: `v2` only. A `v3` means the design was abandoned mid-flight.
- [ ] **Step 4:** Promote `.superpowers/sdd/` followups into
  `docs/retrospectives/the-ford-stage-2.md` — **the worktree scratch dies with
  the worktree.**
- [ ] **Step 5:** Chronicle `book/src/chronicle/the-ford-stage-2.md` + SUMMARY;
  freshness sweep; Confidence Gradient re-score only if a bet actually moved
  (decision 0030) — say so plainly if none did. **Registry IDs may not appear
  outside `book/src/frontier/`.**
- [ ] **Step 6:** Leave `MAP-ford-subcell-water` at `spec'd` — stages 3–5
  remain.
- [ ] **Step 7: Report H2-1 … H2-4 plainly**, each with its number and verdict,
  and state that H2-4 was a late freeze.

---

## Self-Review

**Spec coverage.** §3 bank referent → Task 1. §5.1 appended keys + resolution →
Task 2. §5.2 ordinal as a function → Task 2 (H2-3 exercises it). §5.3 crossing
as a traversal query → Task 3. §6 no epoch / no draws → Task 2 Step 2's schema
test and Task 4 Step 3. §7 H2-1…H2-4 → Tasks 1, 2, 3. §8's late freeze → Task 3
Step 3. **Deliberately not covered:** §5.4's three exclusions (drinkability,
riparian conditioning, scene emission) — all out of scope by the spec.

**Type consistency.** `bank_signed_distance` keeps its name and
`Option<f64>` return in Tasks 1–3. `channel_distance` / `channel_bands` /
`resolution` keep their names and types in Tasks 2–4. `Crossing`'s three
variants are used only in Task 3.

**Known soft spot.** Task 2's `Resolution` shape is specified by *precedent*
("match 0123's own shape") rather than by literal fields, because 0123 landed
days ago in another campaign and its published shape is the authority — not a
shape guessed here from outside it. That is deliberate, and the spec's §10.2
risk names it.
