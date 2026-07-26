# The Hearth — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: use
> `superpowers:subagent-driven-development` to implement this plan task-by-task
> (and `dispatching-hornvale-subagents` for every dispatch). Steps use checkbox
> (`- [ ]`) syntax for tracking.

Campaign 1 of The Rose Window as amended. Spec:
`docs/superpowers/specs/2026-07-25-the-hearth-design.md` — read §2 (the model),
§2.1 (the topology rule) and §9 (scope) before starting.

**Goal:** A room gets an interior — a small graph of named anchors with declared
topological relations, derived from authored patterns — and a cold creature
crosses that room to the fire.

**Architecture:** The fine layer is the coarse layer one scale down
(rooms:ways :: anchors:relations), so it *reuses* rather than parallels: movement
is a new `SearchSpace` over `kernel/src/astar.rs`, fields reuse `alarm_field`'s
emitter-sum shape, and the pattern inventory copies
`domains/language/src/phonology.rs`'s inventory + `permits` architecture. No
lattice, no coordinate solve, no rendering.

**Tech stack:** Rust 2024, `windows/vessel` (`hornvale-vessel`), std only.

**Two stages.** Stage A (T1–T3) is vocabulary + graph and is independently
testable with no patterns and no fields. Stage B (T4–T6) adds patterns, fields,
and the drive hookup that makes the campaign measurable.

## Global Constraints

- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float order via
  `f64::total_cmp`. A* costs are `u64` integers.
- No wall-clock time. `f64` transcendentals via `hornvale_kernel::math`.
- Every `pub` item, field and variant gets a one-line doc comment
  (`#![warn(missing_docs)]`); every primitive at a `pub` boundary carries a
  `type-audit:` verdict tag. **The ratified `bare-ok` classes are exactly**
  `ratio, count, index, constructor-edge, envelope, identifier-text, prose,
  artifact, diagnostic-value, render-internal, flag`
  (`tools/type-audit/src/tag.rs:4`). An unparseable class is a hard finding, so
  do not invent one — and a fieldless enum needs no tag at all, having no
  primitive at its boundary.
- **Outcomes read topology, never metrics** (spec §2.1). No outcome may depend on
  a distance in a plane. In v1 this is *vacuously* true because no coordinate
  exists — do not introduce one, not even "just for testing."
- **Nothing new is serialized.** Anchors have no coordinate; decision 0069 holds.
  No genesis change, no new predicate at genesis, no epoch of an existing label.
- **Two new labels, versioned from their first commit** (decision 0073):
  `room/furnishing/v1` for pattern selection. `room/layout/v1` is *reserved and
  not used in v1* — declare it only when a solve exists. Do not touch
  `room/child` or `room/face`.
- **Byte-identity is expected but must be verified, not assumed.** The warmth
  field is additive (it can only raise comfort), so a comfortable creature is
  unchanged by construction. If drift appears it is scoped drift to be *named and
  justified*, never regenerated over.
- Run `cargo fmt` as the final step of every task; `cargo clippy --workspace
  --all-targets -- -D warnings` must be clean.

## File structure

```
  windows/vessel/src/interior/mod.rs       the module seam + re-exports
  windows/vessel/src/interior/relation.rs  T1  the topological vocabulary + algebra
  windows/vessel/src/interior/anchor.rs    T2  Anchor, AnchorId, Interior (the graph)
  windows/vessel/src/interior/route.rs     T3  SearchSpace over anchors
  windows/vessel/src/interior/pattern.rs   T4  inventory, derived selection, validator
  windows/vessel/src/interior/field.rs     T5  anchor-emitted fields, graph decay
  windows/vessel/src/liveness.rs           T6  Thermal reads the warmth field
```

Each file has one responsibility and is small; `liveness.rs` is already ~9k lines
and gains only the drive hookup.

---

## Stage A — vocabulary and graph

### Task 1: The topological vocabulary and its algebra

**Files:**
- Create: `windows/vessel/src/interior/mod.rs`, `windows/vessel/src/interior/relation.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod interior;`)

**Interfaces:**
- Produces:
  - `pub enum Rcc8 { Dc, Ec, Po, Tpp, TppI, Ntpp, NtppI, Eq }`
  - `pub fn converse(r: Rcc8) -> Rcc8`
  - `pub fn is_symmetric(r: Rcc8) -> bool`
  - `pub fn is_transitive(r: Rcc8) -> bool`
  - `pub fn compose(a: Rcc8, b: Rcc8) -> std::collections::BTreeSet<Rcc8>`

**Why borrowed, not invented** (spec §5): RCC-8 is jointly exhaustive and
pairwise disjoint over region pairs, and has a published composition table. v1
implements the full 8-variant enum (so JEPD holds by construction — a function
returns exactly one), the complete converse/symmetry/transitivity declarations,
and the **containment-transitivity** entries of the composition table, which are
the only ones v1 uses. The remaining entries are reserved, and `compose` returns
the universal set for them rather than guessing — an honest "I don't know yet"
that stays sound (the universal set is always a correct over-approximation).

- [ ] **Step 1: Write the failing tests**

Create `windows/vessel/src/interior/relation.rs` with only a `mod tests` block:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn converse_is_an_involution_and_pairs_the_inverses() {
        for r in Rcc8::ALL {
            assert_eq!(converse(converse(r)), r, "converse is its own inverse: {r:?}");
        }
        assert_eq!(converse(Rcc8::Ntpp), Rcc8::NtppI);
        assert_eq!(converse(Rcc8::Tpp), Rcc8::TppI);
        // The symmetric relations are their own converse.
        for r in [Rcc8::Dc, Rcc8::Ec, Rcc8::Po, Rcc8::Eq] {
            assert_eq!(converse(r), r, "{r:?} is symmetric");
        }
    }

    #[test]
    fn symmetry_declarations_agree_with_converse() {
        // ONE SOURCE OF TRUTH: a relation is symmetric exactly when it is its
        // own converse. The declaration must not drift from the table.
        for r in Rcc8::ALL {
            assert_eq!(
                is_symmetric(r),
                converse(r) == r,
                "symmetry declaration disagrees with converse for {r:?}"
            );
        }
    }

    #[test]
    fn containment_composes_transitively() {
        // The one composition v1 actually uses: a hearth inside an alcove inside
        // a hall is inside the hall. Published RCC-8: NTPP ∘ NTPP = {NTPP}.
        assert_eq!(
            compose(Rcc8::Ntpp, Rcc8::Ntpp),
            [Rcc8::Ntpp].into_iter().collect::<std::collections::BTreeSet<_>>()
        );
        assert!(is_transitive(Rcc8::Ntpp));
        // EQ is the identity of composition.
        for r in Rcc8::ALL {
            assert_eq!(
                compose(Rcc8::Eq, r),
                [r].into_iter().collect::<std::collections::BTreeSet<_>>(),
                "EQ ∘ {r:?} = {{{r:?}}}"
            );
        }
    }

    #[test]
    fn unimplemented_compositions_return_the_universal_set_not_a_guess() {
        // Soundness over precision: an entry we have not taken from the
        // published table returns EVERY relation (a correct over-approximation),
        // never a plausible-looking single answer.
        let all: std::collections::BTreeSet<Rcc8> = Rcc8::ALL.into_iter().collect();
        assert_eq!(compose(Rcc8::Po, Rcc8::Ec), all);
    }
}
```

- [ ] **Step 2: Run the tests and watch them fail**

```bash
cargo test -p hornvale-vessel --lib interior::relation 2>&1 | tail -20
```

Expected: FAIL — `cannot find type 'Rcc8' in this scope`.

- [ ] **Step 3: Implement the vocabulary**

Create `windows/vessel/src/interior/mod.rs`:

```rust
//! The room INTERIOR (The Hearth): a small graph of named anchors with declared
//! topological relations, derived from authored patterns. The fine layer is the
//! coarse layer one scale down — rooms:ways :: anchors:relations — so it reuses
//! the kernel planner and the field shape rather than paralleling them.
//!
//! NOTHING HERE IS SERIALIZED (decision 0069): an anchor has no coordinate, and
//! outcomes read TOPOLOGY, never metrics (spec §2.1), so a future rendering
//! solve can be retuned forever without an epoch.

pub mod relation;

pub use relation::{Rcc8, compose, converse, is_symmetric, is_transitive};
```

Prepend to `relation.rs`, above the test module:

```rust
//! The topological relation vocabulary — the Region Connection Calculus
//! (RCC-8), borrowed rather than invented (spec §5). Its eight relations are
//! JOINTLY EXHAUSTIVE and PAIRWISE DISJOINT over region pairs: for any two
//! anchors exactly one holds, which is what structurally prevents the catalogue
//! sprawl a hand-rolled vocabulary invites — a partition cannot be padded.

/// One of the eight RCC-8 relations between two anchor regions. Exactly one
/// holds for any ordered pair (JEPD).
/// type-audit: bare-ok(tag)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rcc8 {
    /// Disconnected — no contact. The PRIVACY primitive (spec §5).
    Dc,
    /// Externally connected — touching, no shared interior (adjacency).
    Ec,
    /// Partial overlap — shared interior, neither contains the other.
    Po,
    /// Tangential proper part — inside and touching the boundary.
    Tpp,
    /// The converse of [`Rcc8::Tpp`] — tangentially contains.
    TppI,
    /// Non-tangential proper part — strictly inside (the containment case).
    Ntpp,
    /// The converse of [`Rcc8::Ntpp`] — strictly contains.
    NtppI,
    /// Equal — the same region.
    Eq,
}

impl Rcc8 {
    /// Every relation, in a fixed order (the JEPD partition, enumerated).
    pub const ALL: [Rcc8; 8] = [
        Rcc8::Dc,
        Rcc8::Ec,
        Rcc8::Po,
        Rcc8::Tpp,
        Rcc8::TppI,
        Rcc8::Ntpp,
        Rcc8::NtppI,
        Rcc8::Eq,
    ];
}

/// The converse: `converse(r)` holds of `(b, a)` exactly when `r` holds of
/// `(a, b)`. An involution.
pub fn converse(r: Rcc8) -> Rcc8 {
    match r {
        Rcc8::Tpp => Rcc8::TppI,
        Rcc8::TppI => Rcc8::Tpp,
        Rcc8::Ntpp => Rcc8::NtppI,
        Rcc8::NtppI => Rcc8::Ntpp,
        // DC, EC, PO and EQ are symmetric — their own converse.
        other => other,
    }
}

/// Whether `r` is symmetric — equivalently, whether it is its own converse.
/// type-audit: bare-ok(flag: return)
pub fn is_symmetric(r: Rcc8) -> bool {
    converse(r) == r
}

/// Whether `r` is transitive. Only the containment relations and equality are.
/// type-audit: bare-ok(flag: return)
pub fn is_transitive(r: Rcc8) -> bool {
    matches!(r, Rcc8::Ntpp | Rcc8::NtppI | Rcc8::Tpp | Rcc8::TppI | Rcc8::Eq)
}

/// The composition table: the relations that may hold between `a` and `c` given
/// `a ρ b` and `b σ c`.
///
/// v1 implements only the entries it USES — containment transitivity and the
/// identity — and returns the UNIVERSAL set for the rest. That is a correct
/// over-approximation (sound but imprecise), deliberately chosen over a
/// plausible-looking guess: an entry taken from the published table is knowledge,
/// an entry invented here would be a bug wearing knowledge's clothes. Filling in
/// the remaining entries is reserved.
pub fn compose(a: Rcc8, b: Rcc8) -> std::collections::BTreeSet<Rcc8> {
    let one = |r: Rcc8| [r].into_iter().collect();
    match (a, b) {
        // EQ is the identity of composition, on both sides.
        (Rcc8::Eq, r) | (r, Rcc8::Eq) => one(r),
        // Containment is transitive (published RCC-8).
        (Rcc8::Ntpp, Rcc8::Ntpp) => one(Rcc8::Ntpp),
        (Rcc8::NtppI, Rcc8::NtppI) => one(Rcc8::NtppI),
        // Not yet taken from the table: every relation remains possible.
        _ => Rcc8::ALL.into_iter().collect(),
    }
}
```

Add `pub mod interior;` to `windows/vessel/src/lib.rs` beside `pub mod liveness;`.

- [ ] **Step 4: Run the tests and verify they pass**

```bash
cargo test -p hornvale-vessel --lib interior::relation 2>&1 | tail -20
```

Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/interior/ windows/vessel/src/lib.rs
git commit -m "feat(vessel): the RCC-8 topological vocabulary, borrowed not invented (The Hearth T1)"
```

---

### Task 2: The anchor graph

**Files:**
- Create: `windows/vessel/src/interior/anchor.rs`
- Modify: `windows/vessel/src/interior/mod.rs`

**Interfaces:**
- Consumes: `Rcc8` (T1).
- Produces:
  - `pub struct AnchorId(pub u16)` — an index into an `Interior`'s anchors.
  - `pub struct Anchor { pub kind: AnchorKind, pub within: Option<AnchorId> }`
  - `pub enum AnchorKind { Hearth, Threshold, Bed, Vessel, Screen, Pool, Log }`
  - `pub struct Interior { anchors: Vec<Anchor>, adjacency: BTreeSet<(AnchorId, AnchorId)> }`
  - `pub fn Interior::relation(&self, a: AnchorId, b: AnchorId) -> Rcc8`
  - `pub fn Interior::is_connected(&self) -> bool`
  - `pub fn Interior::neighbors(&self, a: AnchorId) -> Vec<AnchorId>`

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// A hall with a hearth inside it, a threshold touching the hall, and a
    /// screen disconnected from the hearth. Three of the four v1 relations.
    fn planted() -> (Interior, AnchorId, AnchorId, AnchorId, AnchorId) {
        let mut i = Interior::new();
        let hall = i.push(AnchorKind::Pool, None); // stand-in region
        let hearth = i.push(AnchorKind::Hearth, Some(hall));
        let door = i.push(AnchorKind::Threshold, None);
        let screen = i.push(AnchorKind::Screen, None);
        i.connect(hall, door);
        i.connect(hall, screen);
        (i, hall, hearth, door, screen)
    }

    #[test]
    fn exactly_one_relation_holds_for_every_ordered_pair() {
        // JEPD, asserted rather than assumed: `relation` is a FUNCTION, so it
        // returns exactly one — this pins that it is also CONVERSE-CONSISTENT,
        // which is where a hand-written table would drift.
        let (i, ..) = planted();
        for a in i.ids() {
            for b in i.ids() {
                assert_eq!(
                    i.relation(b, a),
                    converse(i.relation(a, b)),
                    "relation({a:?},{b:?}) and its converse disagree"
                );
            }
        }
    }

    #[test]
    fn containment_reads_ntpp_and_its_converse() {
        let (i, hall, hearth, ..) = planted();
        assert_eq!(i.relation(hearth, hall), Rcc8::Ntpp, "the hearth is in the hall");
        assert_eq!(i.relation(hall, hearth), Rcc8::NtppI);
        assert_eq!(i.relation(hall, hall), Rcc8::Eq);
    }

    #[test]
    fn adjacency_reads_ec_and_non_adjacency_reads_dc() {
        let (i, hall, _hearth, door, screen) = planted();
        assert_eq!(i.relation(hall, door), Rcc8::Ec, "the threshold touches the hall");
        assert_eq!(i.relation(door, screen), Rcc8::Dc, "the PRIVACY primitive");
    }

    #[test]
    fn connectivity_is_detected_in_both_directions() {
        let (i, ..) = planted();
        assert!(i.is_connected(), "the planted interior is connected");
        // An orphan anchor makes it unreachable — the validator's rule (T4).
        let mut broken = Interior::new();
        let a = broken.push(AnchorKind::Hearth, None);
        let _b = broken.push(AnchorKind::Bed, None);
        assert!(
            !broken.is_connected(),
            "two anchors with no edge between them are disconnected"
        );
        let mut fixed = Interior::new();
        let x = fixed.push(AnchorKind::Hearth, None);
        let y = fixed.push(AnchorKind::Bed, None);
        fixed.connect(x, y);
        assert!(fixed.is_connected());
        let _ = a;
    }
}
```

- [ ] **Step 2: Run and watch it fail**

```bash
cargo test -p hornvale-vessel --lib interior::anchor 2>&1 | tail -20
```

Expected: FAIL — `cannot find type 'Interior' in this scope`.

- [ ] **Step 3: Implement the graph**

```rust
//! The anchor graph — the room's interior as nodes and edges. Anchors are
//! REGIONS (a hearth, an alcove, a threshold), which is what makes RCC-8 the
//! right vocabulary. Nothing here is serialized: an anchor has no coordinate,
//! and its identity within a room is positional, not persisted.

use super::relation::{Rcc8, converse};

/// An anchor's index within its [`Interior`]. Not an entity id and never
/// serialized — a derived anchor has no identity until promotion (spec §4,
/// reserved).
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnchorId(pub u16);

/// What an anchor IS. An object earns a place here by the activity it affords
/// (spec §7), never by decoration. (No `type-audit:` tag: a fieldless enum has
/// no primitive at its boundary, and `tag` is NOT a ratified `bare-ok` class —
/// see `tools/type-audit/src/tag.rs:4` for the eleven that are.)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AnchorKind {
    /// A fire: emits warmth and light; the canonical gathering place.
    Hearth,
    /// A doorway — an anchor that is ALSO a room-graph edge (the two-level seam).
    Threshold,
    /// A place to sleep.
    Bed,
    /// A water vessel or basin.
    Vessel,
    /// A screen or pillar: affords nothing, shapes sightlines (reserved).
    Screen,
    /// A natural pool (the wilderness half of the catalogue).
    Pool,
    /// A fallen log (the wilderness half).
    Log,
}

/// One anchor: what it is, and the anchor it lies strictly within, if any.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Anchor {
    /// What this anchor is.
    pub kind: AnchorKind,
    /// The anchor this one lies strictly inside (`Ntpp`), if any.
    pub within: Option<AnchorId>,
}

/// A room's interior: the anchors and which touch which. Deterministic
/// throughout — `Vec` order is the anchor order, adjacency is a `BTreeSet`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Interior {
    anchors: Vec<Anchor>,
    adjacency: std::collections::BTreeSet<(AnchorId, AnchorId)>,
}

impl Interior {
    /// An empty interior.
    pub fn new() -> Self {
        Self::default()
    }

    /// Append an anchor, returning its id.
    pub fn push(&mut self, kind: AnchorKind, within: Option<AnchorId>) -> AnchorId {
        let id = AnchorId(self.anchors.len() as u16);
        self.anchors.push(Anchor { kind, within });
        id
    }

    /// Record that two anchors touch (`Ec`). Symmetric: stored in both orders so
    /// the read is order-independent.
    pub fn connect(&mut self, a: AnchorId, b: AnchorId) {
        self.adjacency.insert((a, b));
        self.adjacency.insert((b, a));
    }

    /// Every anchor id, ascending.
    pub fn ids(&self) -> Vec<AnchorId> {
        (0..self.anchors.len()).map(|i| AnchorId(i as u16)).collect()
    }

    /// The anchor at `id`.
    pub fn anchor(&self, id: AnchorId) -> &Anchor {
        &self.anchors[id.0 as usize]
    }

    /// The anchors directly touching `a`, ascending.
    pub fn neighbors(&self, a: AnchorId) -> Vec<AnchorId> {
        self.adjacency
            .iter()
            .filter(|(x, _)| *x == a)
            .map(|(_, y)| *y)
            .collect()
    }

    /// Whether `a` lies strictly within `b`, following the containment chain
    /// (`Ntpp` is transitive — T1's `is_transitive`).
    fn within_chain(&self, a: AnchorId, b: AnchorId) -> bool {
        let mut cur = self.anchor(a).within;
        // The chain is finite and acyclic by construction (`within` may only
        // name an EARLIER anchor — enforced by the validator, T4).
        while let Some(p) = cur {
            if p == b {
                return true;
            }
            cur = self.anchor(p).within;
        }
        false
    }

    /// The single RCC-8 relation holding between `a` and `b` (JEPD: exactly one,
    /// because this is a function). Converse-consistent by construction.
    pub fn relation(&self, a: AnchorId, b: AnchorId) -> Rcc8 {
        if a == b {
            return Rcc8::Eq;
        }
        if self.within_chain(a, b) {
            return Rcc8::Ntpp;
        }
        if self.within_chain(b, a) {
            return converse(Rcc8::Ntpp);
        }
        if self.adjacency.contains(&(a, b)) {
            return Rcc8::Ec;
        }
        Rcc8::Dc
    }

    /// Whether every anchor is reachable from anchor `0` by adjacency or
    /// containment — the validator's well-formedness rule (T4). An empty
    /// interior is trivially connected.
    pub fn is_connected(&self) -> bool {
        if self.anchors.is_empty() {
            return true;
        }
        let mut seen: std::collections::BTreeSet<AnchorId> =
            [AnchorId(0)].into_iter().collect();
        let mut frontier = vec![AnchorId(0)];
        while let Some(cur) = frontier.pop() {
            let mut linked = self.neighbors(cur);
            if let Some(p) = self.anchor(cur).within {
                linked.push(p);
            }
            for id in self.ids() {
                if self.anchor(id).within == Some(cur) {
                    linked.push(id);
                }
            }
            for n in linked {
                if seen.insert(n) {
                    frontier.push(n);
                }
            }
        }
        seen.len() == self.anchors.len()
    }
}
```

Add to `mod.rs`: `pub mod anchor;` and
`pub use anchor::{Anchor, AnchorId, AnchorKind, Interior};`.

- [ ] **Step 4: Run and verify**

```bash
cargo test -p hornvale-vessel --lib interior:: 2>&1 | tail -20
```

Expected: PASS, 8 tests (T1's 4 plus T2's 4).

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/interior/
git commit -m "feat(vessel): the anchor graph — JEPD relations and connectivity (The Hearth T2)"
```

---

### Task 3: Within-room movement over the anchor graph

**Files:**
- Create: `windows/vessel/src/interior/route.rs`
- Modify: `windows/vessel/src/interior/mod.rs`

**Interfaces:**
- Consumes: `Interior`, `AnchorId` (T2).
- Produces:
  - `pub struct InteriorSpace<'a> { interior: &'a Interior, goal: AnchorId }`
  - `impl SearchSpace for InteriorSpace<'_>` with `State = AnchorId`,
    `Action = AnchorId` (the anchor stepped to), unit cost.
  - `pub fn route_within(interior: &Interior, from: AnchorId, to: AnchorId, budget: usize) -> Option<Vec<AnchorId>>`

**Why no lattice** (spec §3): a room's interior *is* a very small room-graph, so
movement is the existing kernel planner over 5–10 nodes. Costs are `u64` and
uniform — a step is a step. **Do not introduce a distance**; the topology rule
forbids it and nothing needs it.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::{AnchorKind, Interior};

    #[test]
    fn a_creature_routes_across_the_room_to_the_hearth() {
        // door — hall — hearth: the shortest path is two steps, and it is the
        // path A* returns. No coordinates exist anywhere in this test.
        let mut i = Interior::new();
        let door = i.push(AnchorKind::Threshold, None);
        let hall = i.push(AnchorKind::Pool, None);
        let hearth = i.push(AnchorKind::Hearth, None);
        let bed = i.push(AnchorKind::Bed, None);
        i.connect(door, hall);
        i.connect(hall, hearth);
        i.connect(hall, bed);

        let plan = route_within(&i, door, hearth, 64).expect("the hearth is reachable");
        assert_eq!(plan, vec![hall, hearth], "it steps through the hall to the fire");
    }

    #[test]
    fn an_unreachable_anchor_yields_no_route() {
        let mut i = Interior::new();
        let a = i.push(AnchorKind::Threshold, None);
        let b = i.push(AnchorKind::Hearth, None);
        // deliberately unconnected
        assert_eq!(route_within(&i, a, b, 64), None);
    }

    #[test]
    fn standing_at_the_goal_is_an_empty_route() {
        let mut i = Interior::new();
        let a = i.push(AnchorKind::Hearth, None);
        assert_eq!(route_within(&i, a, a, 64), Some(Vec::new()));
    }
}
```

- [ ] **Step 2: Run and watch it fail**

```bash
cargo test -p hornvale-vessel --lib interior::route 2>&1 | tail -20
```

Expected: FAIL — `cannot find function 'route_within' in this scope`.

- [ ] **Step 3: Implement the search space**

```rust
//! Within-room movement: a [`SearchSpace`] over the anchor graph, so the kernel
//! planner serves the fine layer exactly as it serves navigation, GOAP and
//! prophecy (UNI-19). NO LATTICE and NO COORDINATE SOLVE — a creature does not
//! need a grid to walk on, it needs somewhere to walk to.

use super::anchor::{AnchorId, Interior};
use hornvale_kernel::astar::{SearchSpace, astar};

/// The within-room search problem: step between touching anchors until the goal
/// anchor is reached. Unit cost — a step is a step; introducing a distance here
/// would violate the topology rule (spec §2.1).
pub struct InteriorSpace<'a> {
    /// The room's anchor graph.
    interior: &'a Interior,
    /// The anchor being sought.
    goal: AnchorId,
}

impl SearchSpace for InteriorSpace<'_> {
    type State = AnchorId;
    type Action = AnchorId;

    fn successors(&self, s: &AnchorId) -> Vec<(AnchorId, AnchorId, u64)> {
        self.interior
            .neighbors(*s)
            .into_iter()
            .map(|n| (n, n, 1))
            .collect()
    }

    fn goal(&self, s: &AnchorId) -> bool {
        *s == self.goal
    }

    fn heuristic(&self, _s: &AnchorId) -> u64 {
        // Admissible and trivial: with unit costs over a handful of nodes, the
        // zero heuristic (Dijkstra) is correct and cheapest to be sure of. A
        // graph-distance heuristic would be admissible too and is not worth the
        // precompute at this size.
        0
    }
}

/// The least-cost sequence of anchors to step through to reach `to`, or `None`
/// if it is unreachable within `budget` expansions. An empty `Vec` means the
/// creature is already there.
/// type-audit: bare-ok(count: budget)
pub fn route_within(
    interior: &Interior,
    from: AnchorId,
    to: AnchorId,
    budget: usize,
) -> Option<Vec<AnchorId>> {
    astar(&InteriorSpace { interior, goal: to }, from, budget)
}
```

Add to `mod.rs`: `pub mod route;` and `pub use route::{InteriorSpace, route_within};`.

If `hornvale_kernel::astar` is not the correct path, find it with
`grep -rn "pub mod astar" kernel/src/lib.rs` and use what is there.

- [ ] **Step 4: Run and verify**

```bash
cargo test -p hornvale-vessel --lib interior:: 2>&1 | tail -20
cargo test -p hornvale-vessel 2>&1 | tail -10
```

Expected: PASS, 11 interior tests; the rest of the crate unaffected (nothing
outside `interior/` has been touched).

- [ ] **Step 5: Commit — this closes Stage A**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/interior/
git commit -m "feat(vessel): within-room routing over the anchor graph (The Hearth T3, closes Stage A)"
```

---

## Stage B — patterns, fields, and the drive

### Task 4: The pattern inventory, derived selection, and the validator

**Files:**
- Create: `windows/vessel/src/interior/pattern.rs`
- Modify: `windows/vessel/src/interior/mod.rs`, `windows/vessel/src/streams.rs`

**Interfaces:**
- Consumes: `Interior`, `AnchorKind` (T2).
- Produces:
  - `pub struct Pattern { pub name: &'static str, pub anchors: &'static [AnchorKind], pub needs_cold: bool, pub built: bool }`
  - `pub const INVENTORY: [Pattern; 5]`
  - `pub fn selection(seed: &Seed, built: bool, cold: bool) -> Vec<&'static Pattern>`
  - `pub fn compose(selected: &[&Pattern]) -> Interior`
  - `pub fn permits(interior: &Interior) -> bool`
  - `pub const ROOM_FURNISHING: &str = "room/furnishing/v1"` (in `streams.rs`)

**The architecture is copied, not invented** (spec §6): `domains/language/src/phonology.rs`
builds a per-species phoneme **inventory** with an `Envelope` gating the draw and
a `permits` admissibility predicate. Read it before writing this task. One
authored inventory; per-culture *derived* selection; a validator that rejects
ill-formed compositions.

**The unit of authorship is a PATTERN, not a room** — a relational fragment
(a fire with seating by it), never a whole floorplan. If this file ever grows
toward "room templates," the campaign has become a catalogue and gone wrong.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    #[test]
    fn selection_is_derived_from_conditions_not_authored_per_culture() {
        // The SAME inventory yields different sets under different climates —
        // the culture signal is derived, exactly as a phoneme inventory is.
        let seed = Seed::new(42);
        let cold = selection(&seed, true, true);
        let warm = selection(&seed, true, false);
        assert_ne!(
            cold.iter().map(|p| p.name).collect::<Vec<_>>(),
            warm.iter().map(|p| p.name).collect::<Vec<_>>(),
            "climate must change which patterns a people uses"
        );
        assert!(
            cold.iter().any(|p| p.anchors.contains(&AnchorKind::Hearth)),
            "a cold people builds around a fire"
        );
    }

    #[test]
    fn selection_is_deterministic() {
        let a = selection(&Seed::new(42), true, true);
        let b = selection(&Seed::new(42), true, true);
        assert_eq!(
            a.iter().map(|p| p.name).collect::<Vec<_>>(),
            b.iter().map(|p| p.name).collect::<Vec<_>>()
        );
    }

    #[test]
    fn wilderness_draws_natural_patterns_and_no_built_ones() {
        // The fine layer must exist where most agents live (spec §13 item 2).
        let wild = selection(&Seed::new(7), false, false);
        assert!(!wild.is_empty(), "wilderness rooms get anchors too");
        assert!(
            wild.iter().all(|p| !p.built),
            "an unbuilt room contains no built patterns"
        );
    }

    #[test]
    fn a_composition_is_connected_and_the_validator_says_so() {
        let sel = selection(&Seed::new(42), true, true);
        let interior = compose(&sel);
        assert!(interior.is_connected(), "the composition is walkable");
        assert!(permits(&interior), "the validator accepts a connected interior");
    }

    #[test]
    fn the_validator_rejects_a_disconnected_composition() {
        // The first well-formedness rule (spec §6): an unreachable anchor means
        // part of the room cannot be used, so the composition is ill-formed.
        let mut broken = Interior::new();
        broken.push(AnchorKind::Hearth, None);
        broken.push(AnchorKind::Bed, None); // no edge — orphaned
        assert!(!permits(&broken), "the validator rejects an unreachable anchor");
    }
}
```

- [ ] **Step 2: Run and watch it fail**

```bash
cargo test -p hornvale-vessel --lib interior::pattern 2>&1 | tail -20
```

Expected: FAIL — `cannot find function 'selection' in this scope`.

- [ ] **Step 3: Implement**

First add the label to `windows/vessel/src/streams.rs`, inside the existing
`stream_labels!` block:

```rust
    /// Stream label for a room's furnishing draw (The Hearth). Versioned from
    /// its first commit (decision 0073): this layer is expected to churn, so
    /// bumping it must not disturb `room/child` or `room/face`.
    ROOM_FURNISHING = "room/furnishing/v1" => "which patterns a room draws";
```

Then `pattern.rs`:

```rust
//! The pattern inventory — authored primitives, DERIVED selection, and a
//! validator. The architecture is `domains/language/src/phonology.rs`
//! transposed: one authored inventory shared by the world, a per-culture draw
//! from it conditioned on what that culture already is, and an admissibility
//! predicate that rejects ill-formed results.
//!
//! THE UNIT OF AUTHORSHIP IS A PATTERN, NOT A ROOM. A pattern is a relational
//! fragment; a room is a composition of them. Authoring whole rooms would make
//! this a catalogue of solutions rather than a generative language, which is
//! the failure mode that killed software's borrowing of Alexander.

use super::anchor::{AnchorKind, Interior};
use hornvale_kernel::Seed;

/// One authored pattern: a named relational fragment and the anchors it brings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pattern {
    /// The pattern's name (stable; part of no save-format contract).
    pub name: &'static str,
    /// The anchors this pattern contributes, in order.
    pub anchors: &'static [AnchorKind],
    /// Whether this pattern is drawn only where warmth matters.
    pub needs_cold: bool,
    /// Whether this pattern belongs to BUILT rooms (false = wilderness).
    pub built: bool,
}

/// The authored inventory — deliberately small. Its size is not the substance;
/// the composition rules are (spec §6).
pub const INVENTORY: [Pattern; 5] = [
    Pattern {
        name: "the-fire",
        anchors: &[AnchorKind::Hearth],
        needs_cold: true,
        built: true,
    },
    Pattern {
        name: "the-threshold",
        anchors: &[AnchorKind::Threshold],
        needs_cold: false,
        built: true,
    },
    Pattern {
        name: "the-sleeping-place",
        anchors: &[AnchorKind::Bed],
        needs_cold: false,
        built: true,
    },
    Pattern {
        name: "the-water",
        anchors: &[AnchorKind::Pool],
        needs_cold: false,
        built: false,
    },
    Pattern {
        name: "the-fallen-log",
        anchors: &[AnchorKind::Log],
        needs_cold: false,
        built: false,
    },
];

/// The patterns a room draws, DERIVED from what it already is — whether it is
/// built and whether warmth matters there — never authored per culture. The
/// seed is threaded for the future variation draw; v1's selection is a pure
/// filter, so it is deterministic trivially.
pub fn selection(_seed: &Seed, built: bool, cold: bool) -> Vec<&'static Pattern> {
    INVENTORY
        .iter()
        .filter(|p| p.built == built)
        .filter(|p| !p.needs_cold || cold)
        .collect()
}

/// Compose the selected patterns into one interior: each pattern contributes its
/// anchors, and every anchor is connected to the first (a hub composition — the
/// simplest rule that guarantees the connectivity `permits` demands). Richer
/// composition rules are where later work belongs.
pub fn compose(selected: &[&Pattern]) -> Interior {
    let mut interior = Interior::new();
    let mut first: Option<super::anchor::AnchorId> = None;
    for p in selected {
        for kind in p.anchors {
            let id = interior.push(*kind, None);
            match first {
                None => first = Some(id),
                Some(f) => interior.connect(f, id),
            }
        }
    }
    interior
}

/// Whether a composition is well-formed. The first rule: the anchor graph must
/// be CONNECTED, or part of the room is unreachable and a creature could be
/// asked to walk somewhere it cannot get to.
/// type-audit: bare-ok(flag: return)
pub fn permits(interior: &Interior) -> bool {
    interior.is_connected()
}
```

Add to `mod.rs`: `pub mod pattern;` and
`pub use pattern::{INVENTORY, Pattern, compose, permits, selection};`.

Note the name collision: `pattern::compose` and `relation::compose` are both
re-exported. Re-export the relation one as `compose_relations` in `mod.rs` to
keep both unambiguous.

- [ ] **Step 4: Run and verify**

```bash
cargo test -p hornvale-vessel --lib interior:: 2>&1 | tail -20
```

Expected: PASS, 16 interior tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/interior/ windows/vessel/src/streams.rs
git commit -m "feat(vessel): the pattern inventory, derived selection, and the validator (The Hearth T4)"
```

---

### Task 5: Fields — what an anchor emits

**Files:**
- Create: `windows/vessel/src/interior/field.rs`
- Modify: `windows/vessel/src/interior/mod.rs`

**Interfaces:**
- Consumes: `Interior`, `AnchorId`, `AnchorKind` (T2), `route_within` (T3).
- Produces:
  - `pub fn warmth_at(interior: &Interior, at: AnchorId, budget: usize) -> f64`
  - `pub const HEARTH_WARMTH: f64` and `pub const WARMTH_DECAY: f64`

**The shape is `alarm_field`'s** (spec §7): a field summed over emitters and read
where you stand. **Decay is over GRAPH DISTANCE, not euclidean** — required by the
topology rule, and precedented, since `alarm_field` already decays over a one-hop
graph halo.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::{AnchorKind, Interior};

    #[test]
    fn warmth_falls_off_with_graph_distance_from_the_fire() {
        // hearth — hall — far. Warmth is greatest AT the fire, less one step
        // away, less again two steps away, and never negative.
        let mut i = Interior::new();
        let hearth = i.push(AnchorKind::Hearth, None);
        let hall = i.push(AnchorKind::Bed, None);
        let far = i.push(AnchorKind::Threshold, None);
        i.connect(hearth, hall);
        i.connect(hall, far);

        let w0 = warmth_at(&i, hearth, 64);
        let w1 = warmth_at(&i, hall, 64);
        let w2 = warmth_at(&i, far, 64);
        assert!(w0 > w1, "at the fire is warmer than one step away: {w0} vs {w1}");
        assert!(w1 > w2, "one step is warmer than two: {w1} vs {w2}");
        assert!(w2 >= 0.0, "warmth is never negative");
    }

    #[test]
    fn a_room_with_no_fire_is_cold_everywhere() {
        // ADDITIVE-LATENT: with no emitter the field is zero, so a creature is
        // unchanged by construction — this is what makes byte-identity structural.
        let mut i = Interior::new();
        let a = i.push(AnchorKind::Bed, None);
        let b = i.push(AnchorKind::Threshold, None);
        i.connect(a, b);
        assert_eq!(warmth_at(&i, a, 64), 0.0);
        assert_eq!(warmth_at(&i, b, 64), 0.0);
    }

    #[test]
    fn warmth_sums_over_multiple_fires() {
        let mut i = Interior::new();
        let hall = i.push(AnchorKind::Bed, None);
        let f1 = i.push(AnchorKind::Hearth, None);
        let f2 = i.push(AnchorKind::Hearth, None);
        i.connect(hall, f1);
        i.connect(hall, f2);
        let mut one = Interior::new();
        let h = one.push(AnchorKind::Bed, None);
        let g = one.push(AnchorKind::Hearth, None);
        one.connect(h, g);
        assert!(
            warmth_at(&i, hall, 64) > warmth_at(&one, h, 64),
            "two fires warm a hall more than one"
        );
    }
}
```

- [ ] **Step 2: Run and watch it fail**

```bash
cargo test -p hornvale-vessel --lib interior::field 2>&1 | tail -20
```

Expected: FAIL — `cannot find function 'warmth_at' in this scope`.

- [ ] **Step 3: Implement**

```rust
//! Anchor-emitted FIELDS — what a hearth gives off, read where you stand. The
//! shape is `liveness::alarm_field`'s: a quantity summed over emitters and read
//! at a position, sparse and order-independent.
//!
//! DECAY IS OVER GRAPH DISTANCE, never euclidean (spec §2.1) — there is no
//! euclidean distance in this model to decay over, and introducing one would put
//! a metric on an outcome path.

use super::anchor::{AnchorId, AnchorKind, Interior};
use super::route::route_within;

/// The warmth a hearth emits at its own anchor. Authored; the scale is
/// irrelevant to byte-identity, which is structural — an emitter-free room
/// yields zero everywhere (the additive-latent pattern).
/// type-audit: bare-ok(ratio)
pub const HEARTH_WARMTH: f64 = 1.0;

/// The multiplier per graph step away from an emitter.
/// type-audit: bare-ok(ratio)
pub const WARMTH_DECAY: f64 = 0.5;

/// The warmth at `at`: the sum over every hearth of its emission decayed by the
/// number of steps from it, `0.0` where no hearth is reachable. Deterministic —
/// the anchor order is the `Vec` order and the route is A*'s.
/// type-audit: bare-ok(ratio: return), bare-ok(count: budget)
pub fn warmth_at(interior: &Interior, at: AnchorId, budget: usize) -> f64 {
    let mut total = 0.0_f64;
    for id in interior.ids() {
        if interior.anchor(id).kind != AnchorKind::Hearth {
            continue;
        }
        if let Some(path) = route_within(interior, at, id, budget) {
            total += HEARTH_WARMTH * WARMTH_DECAY.powi(path.len() as i32);
        }
    }
    total
}
```

`powi` is an intrinsic, not a libm transcendental, so it is safe under the
determinism rules; if clippy or the kernel's `math` conventions object, use a
loop multiplying `WARMTH_DECAY` `path.len()` times.

Add to `mod.rs`: `pub mod field;` and
`pub use field::{HEARTH_WARMTH, WARMTH_DECAY, warmth_at};`.

- [ ] **Step 4: Run and verify**

```bash
cargo test -p hornvale-vessel --lib interior:: 2>&1 | tail -20
```

Expected: PASS, 19 interior tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/interior/
git commit -m "feat(vessel): anchors emit fields — warmth decaying over graph distance (The Hearth T5)"
```

---

### Task 6: The cold creature crosses the room to the fire

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (the `Thermal` drive)
- Test: `windows/vessel/src/liveness.rs` (`mod tests`)

**Interfaces:**
- Consumes: `Interior`, `AnchorId`, `warmth_at` (T5), `route_within` (T3).
- Produces: no new public signature — `Thermal` gains an optional interior field
  exactly as `Danger` gained `alarm` and `dread`.

**Read first:** `windows/vessel/src/liveness.rs`'s `Danger` struct and its
`alarm: Option<&…>` field. That is the precedent for adding an optional sensed
field to a drive: `None` ⇒ byte-identical, and the term is **additive**. Follow
it exactly.

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn a_cold_creature_crosses_the_room_to_the_fire() {
        // THE HEARTH, end to end: a thermally stressed creature in a room with a
        // hearth routes to the hearth anchor and is warmer there than where it
        // began. A creature in an identical room WITHOUT a fire has nowhere
        // warmer to go — the additive-latent control.
        use crate::interior::{AnchorKind, Interior, route_within, warmth_at};
        let mut warm_room = Interior::new();
        let door = warm_room.push(AnchorKind::Threshold, None);
        let hall = warm_room.push(AnchorKind::Bed, None);
        let hearth = warm_room.push(AnchorKind::Hearth, None);
        warm_room.connect(door, hall);
        warm_room.connect(hall, hearth);

        let here = warmth_at(&warm_room, door, 64);
        let there = warmth_at(&warm_room, hearth, 64);
        assert!(there > here, "the fire is warmer than the doorway");
        let plan = route_within(&warm_room, door, hearth, 64).expect("reachable");
        assert_eq!(plan.last(), Some(&hearth), "the plan ends at the fire");

        let mut cold_room = Interior::new();
        let d2 = cold_room.push(AnchorKind::Threshold, None);
        let h2 = cold_room.push(AnchorKind::Bed, None);
        cold_room.connect(d2, h2);
        assert_eq!(
            warmth_at(&cold_room, d2, 64),
            warmth_at(&cold_room, h2, 64),
            "with no fire, nowhere is warmer — the creature has no reason to move"
        );
    }
```

- [ ] **Step 2: Run and watch it fail**

```bash
cargo test -p hornvale-vessel --lib a_cold_creature 2>&1 | tail -20
```

Expected: FAIL — the `crate::interior` items are not yet reachable from
`liveness.rs`'s test module, or the assertion on warmth ordering fails.

- [ ] **Step 3: Wire warmth into the Thermal drive**

Give `Thermal` an optional interior reading, mirroring `Danger.alarm` exactly:

```rust
    /// The room INTERIOR's warmth at the creature's anchor (The Hearth), or
    /// `None` where no interior exists. Folded ADDITIVELY into the felt
    /// temperature, so a creature already comfortable is unchanged and an
    /// interior-free world is byte-identical by construction — the same
    /// additive-latent discipline as `Danger::alarm`.
    pub warmth: Option<f64>,
```

In `Thermal`'s urgency, add the warmth to the sensed temperature before the niche
comparison, and document that it can only *raise* it. Keep every existing
construction site compiling by adding `warmth: None`; find them with
`grep -n "Thermal {" windows/vessel/src/liveness.rs`.

- [ ] **Step 4: Run and verify**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -20
```

Expected: PASS, including every pre-existing thermal test (`warmth: None` is the
identity).

- [ ] **Step 5: The evidence**

```bash
bash scripts/regenerate-artifacts.sh 2>&1 | tail -4
git status --short
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ && echo "ARTIFACTS CLEAN"
```

Expected: `ARTIFACTS CLEAN`. Nothing in v1 constructs an `Interior` from a real
world — the drive hookup is present but every live site passes `warmth: None` —
so byte-identity should be exact. **If anything drifts, stop and report it**;
that means an interior reached a live path, which v1 does not intend.

Then the health battery, timed on the longest sim in the suite:

```bash
/usr/bin/time -f "health battery: %e s" cargo test -p hornvale-lab --test health_calibration 2>&1 | tail -20
```

Expected: PASS with chronicity still `0.0`. Record the seconds in the commit
message.

- [ ] **Step 6: The full gate, then commit**

```bash
make gate 2>&1 | tail -20
```

```bash
cargo fmt
git add -A
git commit -m "feat(vessel): the thermal drive reads the hearth's warmth (The Hearth T6)

Artifacts clean; health battery green at <N> s."
```

---

## Close (G6)

Not a task — `closing-a-campaign` owns it: the chronicle
(`book/src/chronicle/the-hearth.md` + SUMMARY), the retrospective, the book
freshness sweep, the registry work in the ledger's capture manifest (notably
**correcting `CLIENT-furnishing-ladder`'s rung text** — rung 1 is authored
patterns, not room templates), a **refining decision record for 0072** (the
causal derived geometry is the anchor graph, not the metric layout), and
`make gate-full`.

## Self-Review

**Spec coverage.** §2/§2.1 the model and the topology rule → the Global
Constraints, plus T3 and T5's explicit no-distance notes. §3 anchors and the
graph → T2; the `SearchSpace` and the two-level seam → T3 (`Threshold` is an
`AnchorKind`). §4 promotion on touch → **deliberately not implemented** (ledger
#13: no consumer in v1; the design is recorded in the spec). §5 the vocabulary,
JEPD, declared algebra, the carve → T1 and T2's converse-consistency test; v1
ships topological only. §6 patterns → T4, including the phonology transposition
and the connectivity rule. §7 fields → T5; the hazard half of the catalogue is
reserved (no v1 consumer). §8 determinism, the label, byte-identity → the Global
Constraints and T6 step 5. §9 scope → the two stages. Every success criterion has
a task: the cold creature (T6), connectivity both ways (T2, T4), JEPD (T2),
declared algebra (T1), derived selection (T4), byte-identity and the health
battery (T6).

**Placeholders.** None. T6's `<N>` is a measurement to fill in at commit time,
not an unwritten decision. T4 names a re-export collision (`pattern::compose` vs
`relation::compose`) and its fix rather than leaving it to be discovered.

**Type consistency.** `Rcc8`, `converse`, `is_symmetric`, `is_transitive`,
`compose` (T1); `AnchorId`, `AnchorKind`, `Anchor`, `Interior`, `push`,
`connect`, `ids`, `anchor`, `neighbors`, `relation`, `is_connected` (T2);
`InteriorSpace`, `route_within` (T3); `Pattern`, `INVENTORY`, `selection`,
`compose`, `permits` (T4); `warmth_at`, `HEARTH_WARMTH`, `WARMTH_DECAY` (T5);
`Thermal.warmth` (T6). Used under exactly these names throughout.

**One risk the plan carries deliberately.** v1 never builds an `Interior` from a
real world — there is no derivation from a room address yet, so the campaign is
provably byte-identical but also not yet *reachable* from the sim. That is the
honest consequence of Stage B stopping at the drive seam, and the next campaign's
first task is to derive an interior for a real room. Named here so it is a
decision rather than a surprise.
