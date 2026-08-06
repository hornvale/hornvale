# The Deep Realm Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the world an inside — a derived graph of underground chambers you
can descend into, with the xorn and rust monster living there instead of faking
cave-dark on the surface.

**Architecture:** A chamber is an **address in a fixed lattice**, sparsely
occupied; existence and content are pure functions of the address. Entrances,
and how deep a system runs, are existing terrain facts (`cave_at`,
`depth_reach_bands`). Nothing is stored.

**Tech Stack:** Rust 2024, `cargo nextest`, `serde`/`serde_json`/`libm` only.

## The design change this plan makes to the spec

**Spec §3.2 names edge symmetry as "the one genuinely hard problem"** — deriving
A's neighbours and B's neighbours independently, and needing them to agree.

This plan **dissolves it** rather than solving it. Chambers occupy a **fixed
address lattice**, and `exists(addr)` is a per-address derived predicate. An
edge is then simply *"two adjacent addresses that both exist"* — and adjacency
is symmetric by construction, so `A→B` iff `B→A` with nothing to maintain.

That also strengthens §3.1: an address in a fixed lattice cannot encode
generation order even by accident, because the lattice exists before anything
is generated into it.

**H4 stays** — the two-way property is now cheap to assert rather than hard to
guarantee, which is the right direction, and it still catches a lattice bug.

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` ONLY. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, **including
  test code**. Float sorting via `total_cmp`. **`std::time::Instant` is banned
  in test code too.** No wall-clock time.
- **Layering:** `domains/*` depends on `hornvale-kernel` and nothing else,
  never a sibling. `windows/worldgen` is the composition root.
- `#![warn(missing_docs)]` — every public item, field and variant.
- type-audit tags exactly `bare-ok(<class>)` or `bare-ok(<class>: <field>)`.
- `Seed` is a tuple struct `Seed(pub u64)` — `Seed::new` does not exist.
- Any `#[ignore]` reason containing `heavy:` must match **verbatim**:
  `"heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"`
- **`cargo fmt` as the final step before every commit.** Never `--no-verify`.
- Idea-registry cells: five columns, Idea ≤ 600 chars, non-empty **Where**.
- **Nothing in this campaign serializes a `ChamberAddr`.** The moment one is
  written, it becomes a permanent key (spec §3.1).

## The three rules this campaign must not break

1. **An address names a place, never a construction step.** The lattice is
   fixed and exists before generation. Never introduce an ordinal that counts
   what the generator made — that is the rule The Salt, 0102 and The Tolerance
   each learned separately.
2. **Nothing is stored.** Existence and content derive from the address. The
   override seam is a *lookup* in this campaign; there is no writer.
3. **Task 0 can stop the campaign**, and is meant to be able to.

---

### Task 0: Measure the cave substrate — the gate

Caves have never had a consumer, so their distribution has never been
validated. Everything downstream assumes they are common enough and deep
enough to be worth a realm.

**Files:**
- Create: `windows/worldgen/tests/deep_realm_substrate.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::{build_world_to_with_artifacts, BuildDepth, WorldComponents, SkyChoice, SettlementPins}`, `hornvale_terrain::TerrainPins`, `hornvale_astronomy::SkyPins`.
- Produces: printed distributions, pasted into the campaign's chronicle.

- [ ] **Step 1: Write the measurement**

Follow `windows/worldgen/tests/generalist_baseline.rs` for the world-building
preamble (decision 0092's sanctioned fixture posture) — read it first.

```rust
//! The Deep Realm, Task 0: is there an underworld worth building?
//!
//! `cave_at` has shipped since The Lode and has never had a consumer, so its
//! distribution has never been checked against anything. This battery is the
//! campaign's gate: if caves are vanishingly rare, or almost none reach past
//! `Regolith`, the underworld is a scattering of shallow pockets and the
//! campaign reports that and stops.
#![allow(clippy::disallowed_methods)]

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_cave_substrate() {
    // per seed: land cells, cells with a cave, and the histogram of
    // depth_reach_bands over those caves.
}
```

Report, over seeds 1..=30:
1. fraction of **land** cells with `cave_at(cell).is_some()`;
2. the histogram of `depth_reach_bands` (1, 2, 3, 4) over those cells;
3. the fraction of caves reaching band 4;
4. whether deep caves cluster — report the count of cave cells whose
   neighbours also have caves, against the count that stand alone.

- [ ] **Step 2: Add the guard assertions**

Required — a harness that measures nothing looks identical to one that works:

```rust
    assert!(!per_seed.is_empty(), "no seeds sampled");
    assert!(per_seed.iter().all(|r| r.land_cells > 0), "a seed had no land");
    assert!(
        per_seed.iter().all(|r| r.cave_cells <= r.land_cells),
        "more caves than land cells — the land mask and cave_at disagree"
    );
```

- [ ] **Step 3: Run once, capture everything**

Run: `cargo nextest run -p hornvale-worldgen --test deep_realm_substrate --run-ignored all --no-capture 2>&1 | tee /tmp/hv-deep-t0.txt`

Grep the file afterwards. **Do not re-run to see a second line.**

- [ ] **Step 4: STOP and report to the controller**

Do not proceed to Task 1. Report the four numbers. The interpretation is fixed
in the spec: abundant caves with varied reach ⇒ proceed; vanishingly rare or
almost none past `Regolith` ⇒ **the campaign stops here and that is the
finding.**

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/deep_realm_substrate.rs
git commit -m "test(deep-realm): Task 0 — is there an underworld worth building?"
```

---

### Task 1: The realm — `Medium::Rock`, a graduated `Access`, cave formations

**Files:**
- Modify: `domains/climate/src/facets.rs`
- Modify: `windows/locale/src/grammar.rs` (the one exhaustive `Medium` match)
- Modify: `domains/climate/src/biome.rs`, `crops.rs`, `variants.rs` (exhaustive `Formation` matches)
- Modify: `domains/climate/tests/` — add coverage beside the existing facet tests

**Interfaces:**
- Produces: `Medium::Rock`, `Access` as a graduated enum, `Realm::UNDERDARK`,
  `Formation::{KarstCave, LavaTube, FractureCave}`, and
  `Realm::strata()` returning the rock column.

- [ ] **Step 1: Write the failing test**

In `domains/climate/tests/facets.rs` (create if absent):

```rust
#[test]
fn the_underworld_is_a_realm_with_a_rock_column() {
    let r = hornvale_climate::Realm::UNDERDARK;
    assert_eq!(r.medium, hornvale_climate::Medium::Rock);
    assert_eq!(
        r.strata(),
        &[
            hornvale_climate::Stratum::Regolith,
            hornvale_climate::Stratum::Cover,
            hornvale_climate::Stratum::Basement,
            hornvale_climate::Stratum::Roots,
        ]
    );
}

#[test]
fn the_aperture_is_ordered_from_sealed_to_merged() {
    use hornvale_climate::Access::*;
    let ladder = [Sealed, Crack, CaveMouth, WorkedWay, Gate, ShaftNet, Merged];
    for w in ladder.windows(2) {
        assert!(w[0] < w[1], "{:?} must sort below {:?}", w[0], w[1]);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-climate`
Expected: FAIL — `Medium::Rock` does not exist.

- [ ] **Step 3: Add the variants**

`Access` gains the seven rungs of spec §3.4 and derives `PartialOrd, Ord` so the
ladder is orderable. `Dive` is **kept** — the Waterworld uses it — and takes its
place on the ladder. Document each rung with the one-line gloss from the spec.

`Stratum` gains `Regolith, Cover, Basement, Roots`, each documented as a **rock
depth register**, explicitly not something you move between (spec §3).

`Medium` gains `Rock` — "solid, with voids; you move through the gaps in it."

`Formation` gains `KarstCave`, `LavaTube`, `FractureCave`, documented as
corresponding one-to-one with `hornvale_terrain::CaveKind`. **Climate may not
import terrain** (sibling domains), so this is a deliberate duplicate per
decision 0094 — a shared roster, never a shared derivation.

- [ ] **Step 4: Fix the exhaustive matches**

`windows/locale/src/grammar.rs:91-94` matches `Medium` exhaustively. Add a
`Medium::Rock` arm returning a subterranean micro-habitat phrase. The three
climate files matching `Formation` exhaustively (`biome.rs`, `crops.rs`,
`variants.rs`) each need cave arms — caves grow no crops and are not a surface
biome, so those arms should say so rather than defaulting.

- [ ] **Step 5: Add the correspondence test**

Spec §3 requires the duplicate roster stay aligned. In
`cli/tests/` (which is where workspace-wide enforcement lives):

```rust
#[test]
fn every_cave_kind_has_exactly_one_cave_formation() {
    // hornvale_terrain::CaveKind has three variants; hornvale_climate::Formation
    // must carry exactly three cave variants, one per kind. If terrain adds a
    // fourth CaveKind, this reddens rather than silently giving the underworld
    // a formation it has no rock for.
}
```

- [ ] **Step 6: Run to verify it passes**

Run: `cargo nextest run -p hornvale-climate -p hornvale-locale -p hornvale && cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(climate): the underworld is a realm — rock, a graduated aperture, cave formations"
```

---

### Task 2: The chamber address — a fixed lattice, sparsely occupied

**This is the campaign's highest-risk task.** Read rule 1 above before writing
anything.

**Files:**
- Create: `windows/worldgen/src/chamber.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod chamber;`)
- Modify: `windows/worldgen/src/streams.rs`
- Create: `windows/worldgen/tests/deep_realm_chamber.rs`

**Interfaces:**
- Consumes: `hornvale_terrain::features::Cave`, `hornvale_kernel::{CellId, Seed, StreamLabel}`.
- Produces:
  - `pub struct ChamberAddr { pub cell: CellId, pub entrance: u8, pub band: u8, pub slot: u8 }`
  - `pub fn chamber_exists(seed: Seed, cave: &Cave, addr: ChamberAddr) -> bool`
  - `pub fn chamber_at(seed: Seed, cave: &Cave, addr: ChamberAddr) -> Option<Chamber>`
  - `pub const SLOTS_PER_BAND: u8`

- [ ] **Step 1: Write the address-stability test FIRST**

This is the whole reason the task is risky:

```rust
/// The rule The Salt, 0102 and The Tolerance each learned separately:
/// generation order is never an identity. A `ChamberAddr` names a PLACE in a
/// lattice that exists before anything is generated into it, so nothing about
/// which chambers happen to exist can move another chamber's address.
#[test]
fn an_addresss_meaning_does_not_depend_on_which_other_chambers_exist() {
    // Two caves at the same cell differing ONLY in depth_reach_bands.
    // A chamber that exists in BOTH must have identical content in both —
    // its address cannot have been renumbered by the deeper cave having
    // more chambers.
}

#[test]
fn the_lattice_is_fixed_and_existence_is_sparse() {
    // Over a cave with reach 4: the address space is SLOTS_PER_BAND * 4,
    // constant; the number that EXIST is strictly less, and varies by seed.
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-worldgen --test deep_realm_chamber`
Expected: FAIL — `chamber_exists` does not exist.

- [ ] **Step 3: Declare the stream label**

A permanent save-format contract. In `windows/worldgen/src/streams.rs`, inside
the existing `hornvale_kernel::stream_labels! { … }` block:

```rust
    /// The underworld chamber derivation (The Deep Realm). Keyed on a
    /// ChamberAddr — a place in a fixed lattice, never a generation ordinal.
    CHAMBER = "chamber/v1" => "the underworld chamber derivation";
```

Then **regenerate the manifest** — a `stream_labels()` addition without a
manifest regen is a known recurring miss. `make rebaseline`, then confirm
`git diff --exit-code book/src/reference/` is clean, and include the
regenerated page in the commit.

- [ ] **Step 4: Implement**

`ChamberAddr` is four small integers naming a place: which cell, which entrance
of that cell, which depth band, and which slot within that band. `band` indexes
`Realm::UNDERDARK.strata()`; `slot` indexes a fixed lattice of size
`SLOTS_PER_BAND`, **not** a count of what was generated.

`chamber_exists` derives a stream from `(seed, CHAMBER, addr)` and thresholds
it, gated so that `addr.band` exceeds nothing beyond `cave.depth_reach_bands`.
`chamber_at` returns `None` when it does not exist, else derives the content.

Follow the composed-label pattern at `windows/worldgen/src/lib.rs:5098`:
`seed.derive(streams::CHAMBER).derive(StreamLabel::dynamic(&key)).stream()`.

- [ ] **Step 5: Run to verify both pass**

Run: `cargo nextest run -p hornvale-worldgen --test deep_realm_chamber 2>&1 | tee /tmp/hv-deep-t2.txt`

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/worldgen/src/ windows/worldgen/tests/deep_realm_chamber.rs book/src/reference/
git commit -m "feat(worldgen): the chamber lattice — an address names a place, never a step"
```

---

### Task 3: Passages, and the two-way property

**Files:**
- Modify: `windows/worldgen/src/chamber.rs`
- Modify: `windows/worldgen/tests/deep_realm_chamber.rs`

**Interfaces:**
- Produces: `pub fn passages_from(seed: Seed, cave: &Cave, addr: ChamberAddr) -> Vec<ChamberAddr>`

- [ ] **Step 1: Write the failing test**

```rust
/// Spec H4. Passages are two-way — which is also why the underworld is
/// frightening: if you can go down, things can come up.
#[test]
fn every_passage_is_traversable_in_both_directions() {
    // For every existing chamber in a probe cave, for every neighbour it
    // lists: that neighbour must list this chamber back.
}
```

- [ ] **Step 2: Run to verify it fails**

Expected: FAIL — `passages_from` does not exist.

- [ ] **Step 3: Implement**

An edge is **two adjacent addresses that both exist**. Adjacency is a property
of the lattice — same band and adjacent slot, or adjacent band and the same
slot — so it is symmetric by construction and the test above cannot fail unless
adjacency itself is written asymmetrically.

Document this in the function: it is what dissolves the spec's §3.2 hard
problem, and a future edit that makes adjacency depend on anything but the two
addresses re-creates it.

- [ ] **Step 4: Add the connectivity guard**

```rust
#[test]
fn a_cave_mouth_reaches_at_least_one_chamber() {
    // An entrance you cannot get anywhere from is not an entrance.
    // Report the fraction of probe caves whose entrance chamber has no
    // passages; assert it is not ALL of them.
}
```

- [ ] **Step 5: Run to verify they pass**

Run: `cargo nextest run -p hornvale-worldgen --test deep_realm_chamber`

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(worldgen): passages, symmetric by construction"
```

---

### Task 4: The override seam — a lookup, no writer

**Files:**
- Modify: `windows/worldgen/src/chamber.rs`
- Modify: `windows/worldgen/tests/deep_realm_chamber.rs`

**Interfaces:**
- Produces: `chamber_at` gains a `world: &World` parameter and consults the
  ledger before deriving.

- [ ] **Step 1: Write the failing test**

```rust
/// The seam, per spec 3.3: a chamber's content is its own latest override
/// fact, else its address-derived default. This campaign ships no WRITER —
/// the test commits a fact by hand to prove the lookup is wired.
#[test]
fn a_committed_override_wins_over_the_derived_default() {
    // derive a chamber; commit an override fact for its address by hand;
    // assert the lookup now returns the override, and that a DIFFERENT
    // address is unaffected.
}
```

- [ ] **Step 2: Run to verify it fails**

Expected: FAIL — nothing consults the ledger.

- [ ] **Step 3: Implement**

Mirror `hornvale_species::instance_biosphere` — read it first; it is the
workspace's only instance lens and this is the same pattern one level over.

- [ ] **Step 4: Run to verify it passes, and commit**

```bash
cargo fmt
git add -A
git commit -m "feat(worldgen): the override seam — differences, never the world"
```

---

### Task 5: Descent at the vessel seam

**Files:**
- Modify: `windows/vessel/src/session.rs`
- Modify: `windows/vessel/tests/` — beside the existing dive coverage

**Interfaces:**
- Consumes: `chamber_at`, `passages_from` from Tasks 2–3.
- Produces: a `delve` verb and its `climb` inverse, mirroring `dive`/`surface`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn you_can_enter_a_cave_and_come_back_out() {
    // At a cell with a cave: `delve` enters; `climb` returns to the surface.
    // Refusal at a cell with no cave must NAME what is missing — `dive`'s
    // own doc says "you cannot" without saying what stopped you reads as a
    // parse failure rather than the bottom of the sea.
}
```

- [ ] **Step 2: Run to verify it fails**

- [ ] **Step 3: Implement**

Read `session.rs:939-975` (`dive`/`surface`) first and follow its shape exactly:
a `chamber_column_here()` analogous to `column_here()`, a `self.underground`
analogous to `self.submerged`, and refusals that name the obstacle. Register
`delve`/`climb` in the verb table beside `"dive" => self.dive()`, and add them
to the help text at `session.rs:158`.

- [ ] **Step 4: Run to verify it passes, and commit**

```bash
cargo fmt
git add -A
git commit -m "feat(vessel): delve and climb — the underworld is somewhere you can be"
```

---

### Task 6: Subterranean conditions, and rehoming the xorn

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (the substrate the niche is scored against)
- Modify: `domains/species/src/lib.rs` (xorn and rust-monster niches)
- Create: `windows/worldgen/tests/deep_realm_rehome.rs`

**Interfaces:**
- Consumes: `Realm::UNDERDARK`, the chamber lattice.
- Produces: subterranean condition values, and re-authored niches.

- [ ] **Step 1: Write the failing readout**

```rust
/// Spec H1, REPORTED not asserted. The xorn's niche today approximates
/// cave-dark with an insolation optimum of 0.05 and near-zero devotion on
/// every surface axis. After rehoming it is scored against subterranean
/// conditions and its surface suitability collapses.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_the_xorn_before_and_after() {
    // report surface vs subterranean suitability for xorn and rust monster.
}
```

- [ ] **Step 2: Derive the conditions**

A chamber's conditions come from the cell above it and its band: temperature
buffered toward the annual mean with depth, insolation zero, moisture high.
Supply stays surface-fed — cave ecology here is **allochthonous** (spec §6);
chemosynthesis is The Keeping's step D and must not be smuggled in.

- [ ] **Step 3: Re-author the two niches**

Remove the low-insolation fake. State in each doc comment what the old curve was
approximating and why the approximation is gone — the frame-stating discipline
The Manikin established.

- [ ] **Step 4: Run and report both numbers**

Run: `cargo nextest run -p hornvale-worldgen --test deep_realm_rehome --run-ignored all --no-capture 2>&1 | tee /tmp/hv-deep-t6.txt`

**Report whichever way it came out.** H1 is reported, never asserted.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(species): the xorn stops faking darkness"
```

---

### Task 7: The mutation proof

The program's shared acceptance criterion. A green test proves the code ran;
only the mutation proves the axis is visible.

**Files:**
- Create: `windows/worldgen/tests/deep_realm_mutation.rs`

- [ ] **Step 1: Write the mutation test**

```rust
/// Spec H3. Setting a cave's depth_reach_bands to 1 MUST collapse its chamber
/// graph to a shallow pocket. If it does not, the budget is not being read and
/// the terrain coupling is decorative.
#[test]
fn a_shallow_cave_has_a_shallow_graph() {
    let deep = chamber_count(&cave_with_reach(4));
    let shallow = chamber_count(&cave_with_reach(1));
    assert!(deep > shallow, "reach 4 gave {deep}, reach 1 gave {shallow}");
    assert!(
        deepest_band(&cave_with_reach(1)) == 0,
        "a reach-1 cave reached past Regolith — the budget is not being read"
    );
}
```

- [ ] **Step 2: Run and confirm BOTH assertions hold**

**Report both measured numbers.** A green test whose two cases produce similar
numbers is a false pass. This campaign's sibling learned that the hard way: a
mutation applied at the derivation proves the function reads its argument, not
that the pipeline passes the authored value — so **also mutate the pipeline**
(hand the generator a fabricated budget) and confirm something reddens.

- [ ] **Step 3: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/deep_realm_mutation.rs
git commit -m "test(deep-realm): the mutation proof — the budget is read"
```

---

### Task 8: The preregistered readout

**Files:**
- Modify: `windows/worldgen/tests/deep_realm_substrate.rs`

- [ ] **Step 1: Add the H2 readout**

Over seeds 1..=30, **reported and never asserted**:

- **H2** — chambers per cell is heavily zero-weighted, and counts vary where
  non-zero. If every cell with a cave gets a similar graph, the generator is
  producing a uniform column with extra steps — spec §2's discarded framing
  returning as an implementation accident.

Keep Task 0's guards.

- [ ] **Step 2: Run once, capture everything**

Run: `cargo nextest run -p hornvale-worldgen --test deep_realm_substrate --run-ignored all --no-capture 2>&1 | tee /tmp/hv-deep-t8.txt`

- [ ] **Step 3: Record the result whichever way it came out**

**The falsification (spec §7):** if the underworld can be walked but nothing
about it differs by place, it is a dungeon bolted to a worldmap. Report that;
do not retune to rescue it.

- [ ] **Step 4: Commit**

---

### Task 9: Artifacts, decision record, book, close

- [ ] **Step 1: Regenerate and inspect**

`make rebaseline`, then diff `book/src/gallery/`, `book/src/reference/`,
`book/src/laboratory/`, `docs/audits/`. The type-audit report is the commonly
missed one.

- [ ] **Step 2: Write the decision record**

Spec §8 flag 1: this **supersedes The Stratum's D3 for the rock realm only**.
Next free number, name it by slug per 0026. State what D3 ruled, why it was
right for water, what is new, and that D3's actual objection (touching
`RoomAddr`) is not incurred.

- [ ] **Step 3: The epoch, on evidence**

This campaign **adds a stream label** (`chamber/v1`). Per 0089, check whether
that label appears on `origin/main` — if not, v1 is rideable and no suffix is
owed. Check `*/streams.rs`, `kernel/src/seed.rs`, `kernel/src/noise.rs` and the
artifact diff. Per 0084, declare only on evidence; an empty epoch is itself a
defect.

- [ ] **Step 4: Book, chronicle, retrospective**

Chronicle entry + `SUMMARY.md` wiring; a one-page retrospective (process
lessons, not product); a freshness sweep of chapters describing the realms.
**The chronicle may not carry registry IDs** outside the Frontier part.
Re-score the Confidence Gradient if a bet moved (0030).

- [ ] **Step 5: Registry bookkeeping**

Flip `MAP-10`/`MAP-69`-adjacent rows only if genuinely shipped. `MAP-cave-shelter-gamble`,
`MAP-underworld-shortcut` and `MAP-undersea-void` stay `raw` — this campaign
does not touch them. Count characters before writing; the cap is 600.

- [ ] **Step 6: The full gate**

Run: `make gate 2>&1 | tee /tmp/hv-deep-gate.txt; echo "EXIT=$?"`

**Check the exit code explicitly.** Then `make census-check` and `shellcheck`
on any changed script; **neither runs inside `make gate`**. Note that the heavy
tier now exits non-zero by design while `disposition_calibration` is
deliberately red — read the failure list, not the exit code.

---

## Self-review

**Spec coverage.** §3 lattice → Tasks 2–3. §3.1 addresses → Task 2 Step 1.
§3.2 edge symmetry → Task 3 (dissolved; see the header). §3.3 override seam →
Task 4. §3.4 aperture scale → Task 1. §4 extents → **not built**, per §4.2:
every term belongs to a holding that digs and C2a has none. §5 macro/derived →
nothing to build; C2a places no one. §6 scope → Tasks 1–6. §7 Task 0/H1/H2/H3 →
Tasks 0, 6, 7, 8. §8 flags → Task 9.

**One deliberate divergence from the spec**, stated at the top of this plan:
the fixed-lattice address dissolves §3.2's hard problem instead of solving it.
That is a simplification, not a scope cut, and H4 survives as a cheap assertion.

**Placeholders.** Task 2's `SLOTS_PER_BAND` value and Task 6's authored niche
numbers are deliberately left to the implementer — inventing them here without
measuring is the failure Task 0 exists to prevent, and Task 0's output is the
input to both.

**Type consistency.** `ChamberAddr`, `chamber_exists`, `chamber_at`,
`passages_from`, `SLOTS_PER_BAND`, `CHAMBER` are used with identical names
throughout.

**One risk this plan cannot remove:** Task 0 may end the campaign. That is the
point of putting it first.
