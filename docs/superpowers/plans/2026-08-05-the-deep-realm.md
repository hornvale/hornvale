# The Deep Realm Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the world an inside — a derived graph of underground chambers you
can descend into, with the xorn and rust monster living there instead of faking
cave-dark on the surface.

**Architecture:** A chamber is an **address in a fixed lattice**, sparsely
occupied; existence and content are pure functions of the address. Entrances,
and how deep a system runs, are existing terrain facts (`cave_at`,
`deepest_band`). Nothing is stored.

> **Updated 2026-08-06 after The Hollow.** The substrate this plan spends
> changed type: `Cave::depth_reach_bands` (a `u32` count, `1..=4`) is now
> `Cave::deepest_band` (a `BandKind`: `Regolith`/`Cover`/`Basement`/`Roots`/
> `Underneath`). Task 0 has been re-run and **the campaign is unblocked** —
> see its results below. Every task's text is corrected for the new type; the
> spec's frozen §7 is not, and carries an amendment at §7.1 instead.

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

   **1a. `ChamberAddr.band` indexes the permanent `BandKind` ladder, never a
   count of realized bands.** *(Added 2026-08-06, ledger #16 — this is rule 1
   arriving through a door the plan did not guard.)* The measured budget takes
   only **three** of `BandKind`'s five values today (`Cover`/`Basement`/
   `Roots`; `Regolith` and `Underneath` never occur — Task 0). It is therefore
   tempting to index `band` against the three bands that *actually happen*.
   **Do not.** "The 2nd distinct band that occurs" is a construction step
   wearing a place's clothes: the open `MAP-cave-depth-weld` fix would change
   the realized set from three values to four or five, and every address would
   silently relocate — with every override landing on the wrong chamber or on
   none. Indexed against the ladder, that same fix changes only *which*
   addresses are occupied, and no address moves. The depth stack is
   `LADDER → ADDRESS`, then `BUDGET → OCCUPANCY`; the budget must never
   acquire an edge to the address.
2. **Nothing is stored.** Existence and content derive from the address. The
   override seam is a *lookup* in this campaign; there is no writer.
3. **Task 0 can stop the campaign**, and is meant to be able to. *(It ran, and
   it cleared — see Task 0's RESULT block below.)*

## The depth weld is consumed, not fixed — and C2a is its instrument

`MAP-cave-depth-weld` is open and stays open: `cave_depth`'s deepening step
still reads the same `proneness` scalar the presence gate reads, so "how likely
is a cave here" and "how deep does it go" are one number. **C2a consumes the
budget as-is** (ledger #16). The weld lives in `domains/terrain`, and splitting
it means re-calibrating the cave model again and moving the census metric, the
almanac and the map palette — the artifact blast radius The Hollow just paid,
for a model that just passed five preregistered criteria.

The sequencing argument runs the other way from the obvious one. **C2a is the
weld's first real consumer**, and The Hollow's own dominant lesson is that a
field nothing reads cannot be observed to be wrong. Splitting before a consumer
exists calibrates against a guess. So Task 8's H2 readout is the evidence: if
the chamber graph comes back flat *because* a 3-valued budget is too coarse,
that is a measured case for the split; if it comes back varied, the weld was
never C2a's problem. **Task 8 must report the budget's contribution to H2
explicitly**, not merely report H2.

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

- [x] **Step 1: Write the measurement**

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
    // deepest_band over those caves.
}
```

Report, over seeds 1..=30:
1. fraction of **land** cells with `cave_at(cell).is_some()`;
2. the histogram of `deepest_band` over those cells, across **all five**
   `BandKind` variants;
3. band **variety** — how many distinct bands occur, overall and per seed —
   and, beside it as a ceiling check, the fraction reaching `Roots` and
   `Underneath`. Variety is the load-bearing number, not reach-the-deepest:
   see spec §7.1 for why the naive "fraction reaching `Roots`" translation is
   unfaithful;
4. whether deep caves cluster — report the count of cave cells whose
   neighbours also have caves, against the count that stand alone.

- [x] **Step 2: Add the guard assertions**

Required — a harness that measures nothing looks identical to one that works:

```rust
    assert!(!per_seed.is_empty(), "no seeds sampled");
    assert!(per_seed.iter().all(|r| r.land_cells > 0), "a seed had no land");
    assert!(
        per_seed.iter().all(|r| r.cave_cells <= r.land_cells),
        "more caves than land cells — the land mask and cave_at disagree"
    );
```

- [x] **Step 3: Run once, capture everything**

Run: `cargo nextest run -p hornvale-worldgen --test deep_realm_substrate --run-ignored all --no-capture 2>&1 | tee /tmp/hv-deep-t0.txt`

Grep the file afterwards. **Do not re-run to see a second line.**

- [x] **Step 4: STOP and report to the controller**

Do not proceed to Task 1. Report the four numbers. The interpretation is fixed
in the spec: abundant caves with varied reach ⇒ proceed; vanishingly rare or
almost none past `Regolith` ⇒ **the campaign stops here and that is the
finding.**

**RESULT — run 2, 2026-08-06, post-Hollow. GATE CLEARED.** Seeds 1..=30,
469,122 land cells, 55,947 caves (14.5 s):

```
  1. cave fraction of land       0.119259   (was 0.002554 -- run 1 stopped here)
  2. deepest_band  Regolith      0.000000        0 caves
                   Cover         0.400290   22,395
                   Basement      0.361163   20,206
                   Roots         0.238547   13,346
                   Underneath    0.000000        0 caves
  3a. past Regolith              1.000000   55,947/55,947
  3b. band variety               3 of 5 bands occur; per-seed min=3 max=3;
                                 seeds with <=1 band: 0/30
  3c. ceiling: Roots 0.238547    Underneath 0.000000
  4. clustering                  0.985218 clustered / 0.014782 solitary
  CaveKind  karst 0.408351  lava_tube 0.175827  fracture 0.415822
```

**Interpretation against the frozen criterion:** caves are abundant (11.9% of
land, no caveless world) and **100% reach past `Regolith`**. Neither clause of
the stop-condition is met. §3 proceeds.

**Three findings the gate did not ask for, all of which bind later tasks:**

1. **The budget takes only three of five values.** `Regolith` and `Underneath`
   never occur. No real cave is a one-band pocket, and none reaches the fifth
   band. Task 1's ladder and Task 7's mutation both depend on this — see
   spec §7.1.
2. **Variety is present but *identical across worlds*:** every one of the 30
   seeds shows exactly 3 distinct bands (min = max = 3). Depth differs by
   *place within* a world — which is what the chamber graph consumes — but not
   *between* worlds. H2 should not expect cross-world spread in the budget.
3. **`Fracture` is the plurality kind (41.6%), not `Karst` (40.8%).** Task 6's
   niche authoring and Task 1's formation roster must not assume karst is the
   default cave.

- [x] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/deep_realm_substrate.rs
git commit -m "test(deep-realm): Task 0 — is there an underworld worth building?"
```

---

### Task 1: The realm — `Medium::Rock`, a graduated `Access`, cave formations

**Files** *(corrected 2026-08-06 by reading each file — ledger #17; the
original list was wrong in both directions)*:
- Modify: `domains/climate/src/facets.rs` — `Medium`, `Access`, `Stratum`,
  `Formation`, `Realm::UNDERDARK`, `Realm::strata()` (matches on `self.medium`),
  and the exhaustive `Stratum` match at **`facets.rs:211`** inside `biome()`
- Modify: `domains/climate/src/variants.rs` — **two** matches, not one:
  the exhaustive `match (formation, ground)` at **:354**, and the exhaustive
  inner `match stratum` at **:655**
- Modify: `windows/vessel/src/session.rs` — the exhaustive `Stratum` match at
  **:2394** (six arms, no catch-all). **The original file list omitted this
  crate entirely.**
- Modify: `windows/locale/src/grammar.rs` — the exhaustive `Medium` match in
  `micro_habitat()` (~:86, not :91-94)
- Modify: `domains/climate/tests/` — add coverage beside the existing facet tests
- **NOT `biome.rs`** — it only *constructs* `Formation`s; it has no exhaustive
  match over them.
- **NOT `crops.rs`** — `arable()` at :58 is a `matches!` fallthrough that
  already returns `false` for any unlisted formation, which is the correct
  answer for a cave. Confirm, do not edit.

Let the compiler find any match this list still misses; `-D warnings` plus
exhaustiveness will name them. Do **not** add a catch-all arm to silence one.

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
    // FIVE bands, mirroring hornvale_terrain::BandKind exactly. See ledger
    // #18A / rule 1a: a four-band ladder cannot absorb the open depth-weld
    // fix without relocating every ChamberAddr.
    assert_eq!(
        r.strata(),
        &[
            hornvale_climate::Stratum::Regolith,
            hornvale_climate::Stratum::Cover,
            hornvale_climate::Stratum::Basement,
            hornvale_climate::Stratum::Roots,
            hornvale_climate::Stratum::Underneath,
        ]
    );
}

#[test]
fn the_rock_ladder_matches_terrains_band_roster_one_for_one() {
    // Decision 0094: a shared roster, never a shared derivation. Climate may
    // not import terrain, so this is the only thing keeping the duplicate
    // honest. If terrain adds a sixth BandKind, this reddens rather than
    // silently giving the underworld a band it has no rock for.
    //
    // Assert the COUNT and the ORDER by name. Do not cast either enum to an
    // integer -- that would weld the ladder to a declaration position.
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
ladder is orderable. Document each rung with the one-line gloss from the spec.

**Declaration order is load-bearing (ledger #18B).** The seven rock rungs come
**first**, in spec order, then the two existing realm-entry modes:

```rust
Sealed, Crack, CaveMouth, WorkedWay, Gate, ShaftNet, Merged, Default, Dive
```

Both `Default` (the Overworld) and `Dive` (the Waterworld) are **kept** — the
original plan text mentioned only `Dive` and left `Default` unaddressed. Neither
is an aperture, so both sort *above* the ladder rather than inside it, and the
type's doc comment must say so: **`Ord` is meaningful over the rock rungs only;
a comparison involving `Default` or `Dive` is not.**

Do not put them first. `Sealed` means "the void exists and is unreachable", so
`Default < Sealed` would make the freely-walkable overworld sort as *less*
accessible than a sealed void — a term anti-correlated with the scale it joins,
which is exactly the bug class this program names and which The Hollow then
committed in its own new code.

Reordering is safe and this was **verified, not assumed**: `Access` derives only
`Clone, Copy, Debug, PartialEq, Eq` (no `Serialize`), has no exhaustive match
anywhere, and has no consumer outside `facets.rs`.

`Stratum` gains **five** rock bands — `Regolith, Cover, Basement, Roots,
Underneath` — each documented as a **rock depth register**, explicitly not
something you move between (spec §3).

**Five, not four (ledger #18A).** The roster mirrors `hornvale_terrain::BandKind`
exactly, which has five variants. Task 0 measured `Underneath` at **0 of 55,947
caves**, so it is tempting to omit it as a dead branch. Omitting it would break
**rule 1a**: `ChamberAddr.band` indexes this ladder, and a four-band ladder
cannot absorb the open `MAP-cave-depth-weld` fix — which may make `Underneath`
occur — without relocating every address. `Underneath` is unreached by
*measurement*, not by *construction*, and that is the distinction that separates
it from The Hollow's genuinely dead branches. **Assert that it is currently
empty**, so that if it ever fills, someone notices.

`Medium` gains `Rock` — "solid, with voids; you move through the gaps in it."

`Realm::UNDERDARK` takes `access: Access::CaveMouth` — the realm's *canonical*
entrance. **Document at the constant** that an individual chamber's aperture is
a per-place property Task 2's lattice carries, and that this realm-level value
is not a claim about any particular cave (ledger #18C).

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
    // Two caves at the same cell differing ONLY in deepest_band.
    // A chamber that exists in BOTH must have identical content in both —
    // its address cannot have been renumbered by the deeper cave having
    // more chambers.
}

#[test]
fn the_lattice_is_fixed_and_existence_is_sparse() {
    // Over a cave reaching `Roots`: the address space is SLOTS_PER_BAND * 4,
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
it, gated so that `addr.band` reaches no deeper than `cave.deepest_band`.
**`band` is an index into the realm's strata ladder and `deepest_band` is a
named band — compare them through one explicit mapping, in one place.** Do not
cast a `BandKind` to an integer at two call sites.
`chamber_at` returns `None` when it does not exist, else derives the content.

Follow the composed-label pattern at `windows/worldgen/src/lib.rs:5098`:
`seed.derive(streams::CHAMBER).derive(StreamLabel::dynamic(&key)).stream()`.

**The key's spelling is a save-format contract, and the plan originally left it
unspecified (ledger #20).** `StreamLabel::dynamic` hashes the *string*, so
whatever spelling the first implementer picks is fixed forever. Two rules:

1. **Build the key in exactly one function**, declared next to `CHAMBER`. The
   pattern you are copying does this deliberately — read `deity_base_seed`'s
   doc comment: *"the one place the `religion/deity/v2` stream label is
   spelled, so [both callers] can never diverge."*
2. **Spell the band component as the band's NAME, never its index.** `cell`,
   `entrance` and `slot` are genuine integers naming a place and are spelled
   decimal; `band` is not. An index is a *declaration position*: if `Stratum`
   ever gains a variant in the middle — and the open `MAP-cave-depth-weld` work
   is the named candidate — every index below it shifts and every chamber seed
   derived from one silently moves. This is **rule 1a one level down**, applied
   to the derivation instead of the address type.

The precedent is exactly on point. `deity_name_seed` — the function this step
tells you to copy — carries *no entity id* specifically so names stay
"invariant to entity mint order", and that was the fix for a naming **epoch**
(`/v2`). Its own history is a derivation that got welded to an ordinal and had
to be re-cut. Do not re-mint that defect one realm over.

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
- Produces: `chamber_at` gains an override-source parameter and consults it
  before deriving. **Not** `&World` — taking the world would force an on-ledger
  address form, which this campaign defers (owner's ruling, 2026-08-05).

- [ ] **Step 1: Write the failing test**

```rust
/// The seam, per spec 3.3: a chamber's content is its own latest override
/// fact, else its address-derived default. This campaign ships no WRITER, and
/// **commits nothing** — the resolver is tested directly, so the address's
/// on-ledger form stays genuinely undecided until a campaign needs to dig.
/// (Owner's ruling, 2026-08-05: committing a fact here would fix that form as
/// a permanent key, which spec 8 flag 2 exists to defer.)
#[test]
fn an_override_wins_over_the_derived_default() {
    // Call the resolver with a hand-built override input and assert it
    // returns the override; call it with none and assert it returns the
    // derived default; assert a DIFFERENT address is unaffected by either.
    // Nothing is committed to a ledger.
}
```

- [ ] **Step 2: Run to verify it fails**

Expected: FAIL — nothing consults the ledger.

- [ ] **Step 3: Implement**

**Commit nothing.** The resolver takes the override source as a parameter, so
the seam is proven without deciding how an address is written down.
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

> **THERE ARE TWO DIFFERENT REFUSALS HERE, AND TASK 3 MEASURED THE SECOND ONE
> (ledger #23).** The plan's test above only anticipates "no cave at this cell".
> But a cell *with* a cave can still have **no chamber at its entrance address**
> — and that is not an edge case: measured over 1000 probe entrances, the
> entrance chamber exists only **51.5%** of the time, so nearly half of all
> caves lead nowhere.
>
> That is **not a defect**. It is spec §3.4's rung 0 — `Sealed`, *"the void
> exists and is unreachable"* — which §3.4 says explicitly "earns its place in
> C2a … because a void nobody can reach must still exist; it is what a later
> dig *finds*." The lattice is expressing sealed caves.
>
> So `delve` needs **three** outcomes, not two:
> 1. no cave here → refuse, naming the absence;
> 2. a cave here but no chamber at the entrance → refuse, **naming it sealed** —
>    the player is told the rock is closed, not that nothing is there;
> 3. a chamber → descend.
>
> Getting 2 wrong is the whole failure mode `dive`'s doc comment warns about:
> a refusal that does not say what stopped you reads as a parse failure.

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

> **HAZARD from Task 1 (ledger #19).** Three `unreachable!()` panics now guard
> combinations that were impossible when Task 1 shipped, and **this task is the
> first plausible caller of all three**:
> `domains/climate/src/facets.rs:316` panics if `BiomeExpr::biome()` is called
> with a cave `Formation`; `facets.rs:305` and `variants.rs:727` panic on a rock
> `Stratum`. They are correct and deliberate — failing loudly beats silently
> wrong data — but if deriving subterranean conditions leads you to construct a
> cave `BiomeExpr`, **do not reach for a wildcard arm to make the panic go
> away.** Decide what a cave's legacy `Biome` projection actually is, or route
> around `biome()` entirely, and say which in the commit.

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
/// Spec H3. A cave whose `deepest_band` is shallow MUST collapse its chamber
/// graph to a shallow pocket. If it does not, the budget is not being read and
/// the terrain coupling is decorative.
#[test]
fn a_shallow_cave_has_a_shallow_graph() {
    let deep = chamber_count(&cave_reaching(BandKind::Roots));
    let shallow = chamber_count(&cave_reaching(BandKind::Regolith));
    assert!(deep > shallow, "Roots gave {deep}, Regolith gave {shallow}");
    assert!(
        deepest_reached(&cave_reaching(BandKind::Regolith)) == BandKind::Regolith,
        "a Regolith cave reached deeper — the budget is not being read"
    );
}
```

- [ ] **Step 2: Run and confirm BOTH assertions hold**

**Report both measured numbers.** A green test whose two cases produce similar
numbers is a false pass. This campaign's sibling learned that the hard way: a
mutation applied at the derivation proves the function reads its argument, not
that the pipeline passes the authored value — so **also mutate the pipeline**
(hand the generator a fabricated budget) and confirm something reddens.

**The pipeline half is NOT optional here, and Task 0 is why.** Over 30 seeds,
`BandKind::Regolith` occurs **0 times in 55,947 caves** — the live generator
cannot produce the shallow cave this mutation fabricates. So the derivation
half of this test perturbs a value no real world holds, and on its own it
would prove nothing about the shipped path. Mutate the pipeline, or H3 is
decorative. (Spec §7.1.)

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

**Also report H2 broken down by the cell's `deepest_band`** (ledger #16). C2a
is the first consumer of the depth budget, so this readout is the evidence for
or against the open `MAP-cave-depth-weld` split. If graphs are uniform *within*
a band but differ *between* bands, the budget is carrying the variation and a
3-valued budget is the ceiling — a measured case for the split. If graphs vary
freely within a band, the weld is not C2a's constraint and the row can say so.
**Report which, whichever way it comes out.**

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
