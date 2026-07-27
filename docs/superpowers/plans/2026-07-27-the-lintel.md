# The Lintel Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the chamber band exist and be enterable — `enter` at a built locale reaches a human-scale chamber, `out` returns — with the world byte-identical.

**Architecture:** A chamber is a deeper `RoomAddr` (walk depth + 9 refinements, ≈3.3 m), which is **identity, not shape**. Terrain reads truncate to the walk band before consulting terrain, because the settlement-territory set is keyed there. Which chambers exist is a seeded draw under a new `room/chambers/v1` label; what they contain reuses The Hearth's frozen pattern composer untouched. Nothing commits: the player's position has never been a committed datum.

**Tech Stack:** Rust 2024, `hornvale-vessel` (window crate), `hornvale-kernel` seeds/streams, `hornvale-worldgen` for committed history. Tests are `cargo nextest` integration + unit tests. No new dependencies — the workspace allowlist is `serde` + `serde_json` only.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-07-27-the-lintel-design.md`. Parent: `2026-07-25-the-rose-window-metaplan-design.md` §1b.
- **No new crates.** `hornvale-vessel` already depends on `hornvale-worldgen`, `hornvale-locale`, and `hornvale-kernel`; no `Cargo.toml` change is needed or permitted.
- **The pattern inventory is FROZEN.** `ROOM_FURNISHING`'s doc comment: adding or reordering a pattern is an EPOCH, not a tweak, and inventory ORDER is load-bearing. **Do not touch `INVENTORY` or `selection`.**
- **`interior_of` must not change behaviour.** Byte-identity depends on it. Add new functions; never alter its output for a walk-band address.
- **No `HashMap`/`HashSet`** (use `BTreeMap`/`BTreeSet`/`Vec`), **no wall-clock time**, no `f64` transcendentals outside `hornvale_kernel::math`. Enforced by `clippy.toml` with `-D warnings`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field, and variant gets a one-line doc comment.
- **Type audit:** every primitive at a `pub` boundary carries a one-line `type-audit:` verdict tag. Run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` (default-deny).
- **`cargo fmt` as the final step before every commit.** Skipped fmt is this project's most common review finding.
- Verbs are parsed with std only — no `clap`.

---

### Task 1: The band notation and the truncation primitive

**Files:**
- Create: `windows/vessel/src/band.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod band;` and re-export)

**Interfaces:**
- Consumes: `hornvale_kernel::{RoomAddr, MAX_DEPTH}`; `crate::agent::walk_depth` is *not* used here (this module takes an explicit walk depth so it needs no `LocaleContext`).
- Produces: `Band` (enum: `Walk`, `Chamber`), `CHAMBER_DEPTH_OFFSET: u32`, `chamber_depth(walk_depth: u32) -> u32`, `band_of(addr: &RoomAddr, walk_depth: u32) -> Band`, `truncate_to_walk(addr: &RoomAddr, walk_depth: u32) -> RoomAddr`.

- [ ] **Step 1: Write the failing tests**

Create `windows/vessel/src/band.rs` with only the test module and the doc header:

```rust
//! The band notation (Rose Window metaplan §1b.3) in code. A place's BAND is a
//! function of its address depth: the walk band is the ~1.7 km locale a body
//! commits to, the chamber band is the ~3.3 m place inside a structure.
//!
//! An address below the walk band is IDENTITY, NOT SHAPE (§1b.3 law 3): its
//! triangle geometry means nothing, and connectivity comes from the structure's
//! own graph. What the depth *is* used for is deciding which band's rules apply,
//! and truncating back to the walk band when a walk-band-keyed datum is read.

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::RoomAddr;

    /// The walk depth on the canonical globe (`GLOBE_LEVEL` 6 + 6).
    const WALK: u32 = 12;

    fn addr(depth: u32) -> RoomAddr {
        RoomAddr {
            face: 3,
            // a fixed, arbitrary child sequence: 0,1,2,3,0,1,2,3,...
            path: (0..depth).map(|i| (i % 4) as u8).collect(),
        }
    }

    #[test]
    fn a_walk_depth_address_is_the_walk_band() {
        assert_eq!(band_of(&addr(WALK), WALK), Band::Walk);
    }

    #[test]
    fn a_deeper_address_is_the_chamber_band() {
        assert_eq!(band_of(&addr(chamber_depth(WALK)), WALK), Band::Chamber);
    }

    #[test]
    fn truncation_is_the_identity_at_the_walk_band() {
        let a = addr(WALK);
        assert_eq!(truncate_to_walk(&a, WALK), a);
    }

    #[test]
    fn truncation_yields_the_walk_band_ancestor_of_a_chamber() {
        let chamber = addr(chamber_depth(WALK));
        let walk = truncate_to_walk(&chamber, WALK);
        assert_eq!(walk.depth(), WALK);
        assert_eq!(walk.path[..], chamber.path[..WALK as usize]);
        assert_eq!(walk.face, chamber.face);
    }

    #[test]
    fn a_chamber_and_its_ancestor_pack_to_different_ids() {
        // The footgun Task 2 defends against: a chamber id is NOT its
        // locale's id, so any walk-band-keyed set must be consulted with
        // the TRUNCATED address.
        let chamber = addr(chamber_depth(WALK));
        let walk = truncate_to_walk(&chamber, WALK);
        assert_ne!(chamber.pack().unwrap().0, walk.pack().unwrap().0);
    }

    #[test]
    fn the_chamber_depth_fits_the_packing_cap() {
        assert!(chamber_depth(WALK) as usize <= hornvale_kernel::MAX_DEPTH);
        assert!(addr(chamber_depth(WALK)).pack().is_ok());
    }

    #[test]
    fn an_address_shallower_than_the_walk_band_truncates_to_itself() {
        // Coarser than the walk band: there is nothing to truncate, and this
        // must not panic on a slice out of range.
        let coarse = addr(8);
        assert_eq!(truncate_to_walk(&coarse, WALK), coarse);
    }
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-vessel band:: 2>&1 | tail -20`
Expected: FAIL to compile — `cannot find type Band`, `cannot find function band_of`, etc.

- [ ] **Step 3: Write the implementation**

Insert above the `#[cfg(test)]` module in `windows/vessel/src/band.rs`:

```rust
use hornvale_kernel::RoomAddr;

/// How many refinements below the walk band a chamber sits. Nine halvings of a
/// ~1.7 km locale edge is ≈3.3 m — a human-scale room. Declared as a constant
/// because it is a shape of the world, not a tuning knob: changing it changes
/// which addresses are chambers.
/// type-audit: bare-ok(count)
pub const CHAMBER_DEPTH_OFFSET: u32 = 9;

/// Which band an address belongs to. Deliberately only two variants: the
/// STRUCTURE band of metaplan §1b.3 has no code yet (The Precincts), and
/// inventing a variant nothing constructs would be a lie in the type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Band {
    /// The ~1.7 km locale a body commits to — the walk band.
    Walk,
    /// A human-scale place inside a structure, ≈3.3 m.
    Chamber,
}

/// The address depth chambers live at, given the world's walk depth.
/// type-audit: bare-ok(count: walk_depth), bare-ok(count: return)
pub fn chamber_depth(walk_depth: u32) -> u32 {
    walk_depth + CHAMBER_DEPTH_OFFSET
}

/// Which band `addr` is in. Anything deeper than the walk band is a chamber;
/// anything at or above it is walk-band (this campaign ships no coarser band).
/// type-audit: bare-ok(count: walk_depth)
pub fn band_of(addr: &RoomAddr, walk_depth: u32) -> Band {
    if addr.depth() > walk_depth {
        Band::Chamber
    } else {
        Band::Walk
    }
}

/// The walk-band ancestor of `addr` — 0077's path truncation, used DOWNWARD.
///
/// Every walk-band-keyed datum (the settlement-territory set, the locale
/// describer, the climate read) must be consulted with this, never with a raw
/// chamber address. An address at or above the walk band is returned unchanged,
/// so callers may apply this unconditionally — which is the whole point, and the
/// only thing this adds over the kernel primitive it delegates to.
/// type-audit: bare-ok(count: walk_depth)
pub fn truncate_to_walk(addr: &RoomAddr, walk_depth: u32) -> RoomAddr {
    // `RoomAddr::ancestor` (kernel/src/room.rs) already does the bounds-checked
    // slice and returns `None` when `walk_depth` is deeper than the address.
    // Delegate: re-deriving the slice here would duplicate a save-format-
    // adjacent primitive, and a second copy is a second thing to get wrong.
    addr.ancestor(walk_depth).unwrap_or_else(|| addr.clone())
}
```

Add to `windows/vessel/src/lib.rs`, beside the existing `pub mod` lines:

```rust
pub mod band;
```

and beside the existing re-exports:

```rust
pub use band::{Band, CHAMBER_DEPTH_OFFSET, band_of, chamber_depth, truncate_to_walk};
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel band:: 2>&1 | tail -12`
Expected: PASS, 7 tests.

- [ ] **Step 5: Check the audit and format**

Run:
```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
```
Expected: all clean. If the audit complains about a missing tag, add the one-line `type-audit:` comment it names — one line, directly above the item.

- [ ] **Step 6: Commit**

```bash
git add windows/vessel/src/band.rs windows/vessel/src/lib.rs
git commit -m "feat(vessel): the band notation and the walk-band truncation primitive

A place's band is a function of its address depth: walk band is the
~1.7 km locale a body commits to, chamber band is walk + 9 refinements
(~3.3 m). truncate_to_walk is 0077's path truncation used DOWNWARD, and
it is the defence against reading a walk-band-keyed datum with a chamber
address. Applying it is unconditional-safe: at or above the walk band it
is the identity."
```

---

### Task 2: Band-aware chamber interiors (the footgun)

**Files:**
- Modify: `windows/vessel/src/interior/derive.rs` (add a function; do NOT change `interior_of`)
- Modify: `windows/vessel/src/interior/mod.rs` (re-export)

**Interfaces:**
- Consumes: `crate::band::truncate_to_walk`; `crate::liveness::Terrain`; `interior_of`.
- Produces: `chamber_interior_of(chamber: &RoomAddr, terrain: &dyn Terrain, walk_depth: u32) -> Interior`.

**Why this is a separate function rather than a change to `interior_of`:** byte-identity. `interior_of`'s output for every walk-band address must stay bit-for-bit what The Threshold shipped, because a creature's thermal drive reads warmth from it and that history is committed.

- [ ] **Step 1: Write the failing tests**

Append to the existing `#[cfg(test)] mod tests` in `windows/vessel/src/interior/derive.rs` (it already defines `Stub` and `room()`; reuse them):

```rust
    /// A `Terrain` whose built-set is keyed at the WALK band, exactly as
    /// `LocaleTerrain` is (`liveness.rs`: built iff the packed room id is in
    /// the injected settlement-territory set).
    struct WalkKeyedTerrain {
        built_walk_ids: std::collections::BTreeSet<u64>,
    }
    impl Terrain for WalkKeyedTerrain {
        fn elevation(&self, _r: &RoomAddr) -> f64 {
            0.0
        }
        fn is_fresh_water(&self, _r: &RoomAddr) -> bool {
            false
        }
        fn temperature(&self, _r: &RoomAddr, _d: WorldTime) -> f64 {
            -20.0
        }
        fn is_built(&self, r: &RoomAddr) -> bool {
            r.pack()
                .ok()
                .is_some_and(|id| self.built_walk_ids.contains(&id.0))
        }
    }

    const WALK: u32 = 12;

    fn walk_addr() -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        }
    }

    fn chamber_addr() -> RoomAddr {
        let mut path: Vec<u8> = walk_addr().path;
        path.extend((0..crate::band::CHAMBER_DEPTH_OFFSET).map(|i| (i % 4) as u8));
        RoomAddr { face: 3, path }
    }

    #[test]
    fn a_chamber_in_a_built_locale_draws_built_patterns() {
        // THE FOOTGUN: the built-set holds the LOCALE's id, never the
        // chamber's, so a raw read would furnish a dwelling as wilderness.
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [walk_addr().pack().unwrap().0].into_iter().collect(),
        };
        assert!(
            !terrain.is_built(&chamber_addr()),
            "precondition: a raw chamber read is UNBUILT — this is the footgun"
        );
        let i = chamber_interior_of(&chamber_addr(), &terrain, WALK);
        let kinds: Vec<AnchorKind> = i.ids().iter().map(|&id| i.anchor(id).kind).collect();
        assert!(
            kinds.contains(&AnchorKind::Hearth),
            "a built-cold chamber draws a hearth, got {kinds:?}"
        );
    }

    #[test]
    fn a_chamber_in_an_unbuilt_locale_draws_wild_patterns() {
        let terrain = WalkKeyedTerrain {
            built_walk_ids: std::collections::BTreeSet::new(),
        };
        let i = chamber_interior_of(&chamber_addr(), &terrain, WALK);
        let kinds: Vec<AnchorKind> = i.ids().iter().map(|&id| i.anchor(id).kind).collect();
        assert!(
            !kinds.contains(&AnchorKind::Bed),
            "an unbuilt place has no bed, got {kinds:?}"
        );
    }

    #[test]
    fn a_chamber_composes_exactly_as_its_locale_does() {
        // THIS TEST IS SPEC §3's ADMISSIBILITY TABLE, asserted. The table's
        // content in v1 is "every kind is admissible at both bands", so the
        // observable claim is exactly that the two bands compose identically.
        // When a later campaign gives a band its own vocabulary, this test is
        // the one that must change, deliberately and with an epoch argument.
        //
        // The composer is shared and FROZEN: the chamber's interior is the
        // same graph the locale would draw, so this campaign moves no
        // behaviour (spec §2).
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [walk_addr().pack().unwrap().0].into_iter().collect(),
        };
        assert_eq!(
            chamber_interior_of(&chamber_addr(), &terrain, WALK),
            interior_of(&walk_addr(), &terrain),
        );
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel interior::derive 2>&1 | tail -20`
Expected: FAIL to compile — `cannot find function chamber_interior_of`.

- [ ] **Step 3: Implement**

Add to `windows/vessel/src/interior/derive.rs`, below `interior_of`:

```rust
/// The interior of a CHAMBER — a place below the walk band.
///
/// Identical to [`interior_of`] except that every terrain read is taken at the
/// chamber's **walk-band ancestor**. That is not a convenience: `LocaleTerrain`
/// answers `is_built` from a settlement-territory set keyed on walk-band room
/// ids, so a raw chamber address reads as unbuilt and a dwelling would furnish
/// itself with wilderness patterns.
///
/// `interior_of` is deliberately left untouched: its output for every walk-band
/// address is a committed-history input (a creature's thermal drive reads the
/// warmth it implies), so it must stay bit-for-bit what The Threshold shipped.
/// type-audit: bare-ok(count: walk_depth)
pub fn chamber_interior_of(
    chamber: &RoomAddr,
    terrain: &dyn Terrain,
    walk_depth: u32,
) -> Interior {
    let locale = crate::band::truncate_to_walk(chamber, walk_depth);
    interior_of(&locale, terrain)
}
```

Add the re-export to `windows/vessel/src/interior/mod.rs`, on the existing `pub use derive::` line:

```rust
pub use derive::{chamber_interior_of, interior_of};
```

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-vessel interior:: 2>&1 | tail -12`
Expected: PASS — the three new tests plus every pre-existing `interior::` test still green.

- [ ] **Step 5: Prove `interior_of` did not move**

Run: `cargo test -p hornvale-vessel 2>&1 | tail -8`
Expected: the whole vessel suite green, including The Threshold's occupancy and warmth tests. **If any warmth or drive test fails, stop — `interior_of` moved and byte-identity is gone.** That is the 3-attempt rule's trigger, not something to patch around.

- [ ] **Step 6: Format, audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/interior/derive.rs windows/vessel/src/interior/mod.rs
git commit -m "feat(vessel): chamber interiors read terrain at the walk band

LocaleTerrain answers is_built from a settlement-territory set keyed on
WALK-band room ids, so a raw chamber address reads as unbuilt and a
dwelling would furnish itself as wilderness. chamber_interior_of
truncates first. interior_of is untouched by construction: its output is
a committed-history input via the thermal drive."
```

---

### Task 3: The brief

**Files:**
- Create: `windows/vessel/src/brief.rs`
- Modify: `windows/vessel/src/lib.rs`
- Test: unit tests inside `brief.rs` (synthetic records) + one live assertion in Task 6's integration test

**Interfaces:**
- Consumes: `hornvale_worldgen::occupations_by_cell`; `hornvale_kernel::{CellId, RoomAddr, World, NearestCellIndex, Geosphere}`; `crate::band::truncate_to_walk`.
- Produces: `Brief` with public fields `function: Option<Function>`, `tech: Option<TechHorizon>`, `notability: Option<Notability>`, `people: Option<KindId>`, `built: bool`, `cold: bool`; and `brief_of(world, geo, index, place, terrain, walk_depth) -> Brief`.

**Determinism note the implementer must honour:** a place resolves to a cell through `RoomAddr::corner_weights`, which returns **three** corner cells with integer weights. The containing cell is the **maximum-weight** corner, tie-broken by **ascending `CellId`**. Integer comparison only — no floats, so this is cross-platform exact.

- [ ] **Step 1: Write the failing tests**

**Register the module in the same step.** Add `pub mod brief;` to
`windows/vessel/src/lib.rs` now, *before* running the tests. A test-only file
that is not in the module tree is not compiled at all, so the test filter
matches **zero tests** and you learn nothing from Step 2 — "it failed to
compile" is the signal you want, not "0 tests ran". (Task 1's implementer hit
exactly this and worked around it; the plan is corrected here.)

Create `windows/vessel/src/brief.rs`:

```rust
//! The BRIEF: the one thing micro generation may read about a place besides its
//! address and the seed (Rose Window metaplan §1b.4). Macro answers *who holds
//! this land*; micro answers *what is standing here*; the brief is the seam.
//!
//! It is derived, never stored. Fields this campaign does not read yet — the
//! ruin signature (`cause`, `ended_by`, ages) and the district vocabulary — are
//! carried from the start so that adding a consumer never changes the seam.

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{Function, Notability, TechHorizon};

    #[test]
    fn the_alive_occupation_supplies_the_briefs_axes() {
        let b = Brief::from_parts(
            Some(Function::Trade),
            Some(TechHorizon::Classical),
            Some(Notability::Seat),
            None,
            true,
            true,
        );
        assert_eq!(b.function, Some(Function::Trade));
        assert_eq!(b.tech, Some(TechHorizon::Classical));
        assert_eq!(b.notability, Some(Notability::Seat));
        assert!(b.built);
        assert!(b.cold);
    }

    #[test]
    fn an_unbuilt_place_has_an_empty_brief_but_still_reports_climate() {
        let b = Brief::from_parts(None, None, None, None, false, true);
        assert!(!b.built);
        assert!(b.cold, "climate is a property of the place, not of a people");
        assert!(b.function.is_none());
    }

    #[test]
    fn the_brief_is_a_coordinate_not_a_label() {
        // §1b.4: patterns index the CROSS-PRODUCT of axes. Two briefs sharing
        // a function but differing in tech must not compare equal, or the
        // vocabulary would collapse into a catalogue of place types.
        let a = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Neolithic),
            None,
            None,
            true,
            false,
        );
        let b = Brief::from_parts(
            Some(Function::Fort),
            Some(TechHorizon::Classical),
            None,
            None,
            true,
            false,
        );
        assert_ne!(a, b);
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel brief:: 2>&1 | tail -20`
Expected: FAIL — `cannot find type Brief`; possibly also an unresolved `hornvale_history` import (see Step 3's note).

- [ ] **Step 3: Implement**

**First check the import path.** `hornvale-vessel` depends on `hornvale-worldgen`, not on `hornvale-history` directly. Run:

```bash
grep -n "hornvale-history" windows/vessel/Cargo.toml windows/worldgen/Cargo.toml
grep -rn "pub use.*record::\|pub use hornvale_history" windows/worldgen/src/lib.rs | head
```

Use whichever is true:
- If `worldgen` re-exports the record types, import them through `hornvale_worldgen::…` and use that path in the tests too.
- If it does not, add `hornvale-history = { path = "../../domains/history" }` to `windows/vessel/Cargo.toml`. This is architecturally legal (a window may depend on a domain) but `cli/tests/architecture.rs` enforces the dependency allowlist, so run `cargo test -p hornvale --test architecture` immediately after and fix the allowlist entry if the test names one.

Then add above the test module:

```rust
use hornvale_kernel::{CellId, Geosphere, NearestCellIndex, RoomAddr, World};

/// What macro history says about a place, reduced to the axes micro generation
/// indexes. A COORDINATE in a small orthogonal space — never a label drawn from
/// a catalogue of place types (§1b.4).
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Brief {
    /// What the alive occupation here was for, if any occupation is alive.
    pub function: Option<Function>,
    /// The alive occupation's technological horizon.
    pub tech: Option<TechHorizon>,
    /// How notable the alive occupation is in its region.
    pub notability: Option<Notability>,
    /// The people occupying this place, if any.
    pub people: Option<KindId>,
    /// Whether a structure stands here — `Terrain::is_built` at the WALK band.
    pub built: bool,
    /// Whether warmth matters here — `Terrain::is_cold` at the WALK band.
    pub cold: bool,
}

impl Brief {
    /// Assemble a brief from already-resolved parts. Exists so the type can be
    /// unit-tested without a world; `brief_of` is the production path.
    /// type-audit: bare-ok(flag: built), bare-ok(flag: cold)
    pub fn from_parts(
        function: Option<Function>,
        tech: Option<TechHorizon>,
        notability: Option<Notability>,
        people: Option<KindId>,
        built: bool,
        cold: bool,
    ) -> Self {
        Self {
            function,
            tech,
            notability,
            people,
            built,
            cold,
        }
    }
}

/// The geosphere cell a place sits in: the maximum-weight corner of its
/// barycentric blend, tie-broken by ascending `CellId`.
///
/// Integer weights only (`corner_weights` returns `u64` numerators), so the
/// choice is cross-platform exact — no float comparison enters world identity.
/// Returns `None` for a place coarser than the canonical grid.
fn containing_cell(
    place: &RoomAddr,
    geo: &Geosphere,
    index: &NearestCellIndex,
) -> Option<CellId> {
    let weights = place.corner_weights(geo, index)?;
    weights
        .iter()
        .max_by(|a, b| a.1.cmp(&b.1).then(b.0.0.cmp(&a.0.0)))
        .map(|&(cell, _)| cell)
}

/// Derive the brief for `place`. Every read is taken at the walk band, so a
/// chamber and its locale yield the same brief — which is what makes a
/// structure's chambers agree about what building they are in.
/// type-audit: bare-ok(count: walk_depth)
pub fn brief_of(
    world: &World,
    geo: &Geosphere,
    index: &NearestCellIndex,
    place: &RoomAddr,
    terrain: &dyn crate::liveness::Terrain,
    walk_depth: u32,
) -> Brief {
    let locale = crate::band::truncate_to_walk(place, walk_depth);
    let built = terrain.is_built(&locale);
    let cold = terrain.is_cold(&locale);
    let alive = containing_cell(&locale, geo, index)
        .and_then(|cell| {
            // NOTE ON COST: this derives the whole per-cell occupation map on
            // every call. Correct but wasteful, and `brief_of` will be called
            // per descent. If a profile shows it mattering, hoist the map to
            // the caller (the session can hold it for the possession's life) —
            // do NOT memoize inside this function, because a hidden cache in a
            // derivation path is how derived state stops being derived.
            hornvale_worldgen::occupations_by_cell(world).remove(&cell)
        })
        .and_then(|occs| occs.into_iter().find(|o| o.ended.is_none()));
    match alive {
        Some(o) => Brief::from_parts(
            Some(o.function),
            Some(o.tech),
            Some(o.notability),
            Some(o.people),
            built,
            cold,
        ),
        None => Brief::from_parts(None, None, None, None, built, cold),
    }
}
```

Add the imports for `Function`, `Notability`, `TechHorizon`, `KindId` using the path Step 3's check established. Register the module in `windows/vessel/src/lib.rs`:

```rust
pub mod brief;
```
```rust
pub use brief::{Brief, brief_of};
```

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-vessel brief:: 2>&1 | tail -12`
Expected: PASS, 3 tests.

- [ ] **Step 5: Confirm the architecture test still passes**

Run: `cargo test -p hornvale --test architecture 2>&1 | tail -8`
Expected: PASS. This is the layering constitution; if it fails, the import path chosen in Step 3 is wrong — fix the import, do not edit the test.

- [ ] **Step 6: Format, audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/brief.rs windows/vessel/src/lib.rs windows/vessel/Cargo.toml
git commit -m "feat(vessel): the brief — the macro/micro seam

The committed occupation stratigraphy reduced to the axes micro
generation indexes: function, tech, notability, people, plus built/cold.
A coordinate, never a catalogue label. Reads are taken at the walk band,
so a chamber and its locale agree about what building they are in. The
containing cell is the max-weight barycentric corner with an ascending
CellId tie-break -- integer only, so cross-platform exact."
```

---

### Task 4: Which chambers exist — `room/chambers/v1`

**Files:**
- Create: `windows/vessel/src/structure.rs`
- Modify: `windows/vessel/src/streams.rs` (declare the label), `windows/vessel/src/lib.rs`

**Interfaces:**
- Consumes: `crate::brief::Brief`; `crate::band::chamber_depth`; `crate::streams::ROOM_CHAMBERS`; `hornvale_kernel::{RoomAddr, Seed}`.
- Produces: `Structure { threshold: RoomAddr, chambers: Vec<RoomAddr>, links: Vec<(usize, usize)> }`, `structure_at(locale: &RoomAddr, brief: &Brief, seed: Seed, walk_depth: u32) -> Option<Structure>`, `MAX_CHAMBERS: usize`.

**Sparseness (metaplan §1b.3 law 1):** existence below the walk band is a *predicate*. `structure_at` returns `None` unless `brief.built`. When it returns `Some`, `chambers.len()` is in `1..=MAX_CHAMBERS`; every other deep address is simply not a place.

- [ ] **Step 1: Declare the stream label**

Add inside the existing `hornvale_kernel::stream_labels! { … }` block in `windows/vessel/src/streams.rs`:

```rust
    /// Stream label for which chambers a structure has (The Lintel).
    ///
    /// Deliberately NOT `room/furnishing/v1`, which already exists and is live:
    /// chamber existence and pattern selection churn independently, and 0073
    /// splits labels by blast radius before the first bump. Merging them would
    /// put a frequent bump inside a label whose blast radius includes every
    /// creature's committed thermal-drive history.
    ///
    /// This is the first furnishing-family stream anything actually DRAWS from —
    /// `selection` takes no seed.
    ROOM_CHAMBERS = "room/chambers/v1" => "which chambers a structure has";
```

- [ ] **Step 2: Write the failing tests**

**Register the module in the same step** — add `pub mod structure;` to
`windows/vessel/src/lib.rs` before running the tests, or the file is not
compiled and the filter matches zero tests rather than failing (see Task 3's
note).

Create `windows/vessel/src/structure.rs` with the doc header and tests:

```rust
//! A STRUCTURE: the sparse set of chamber-band places standing at one built
//! locale, and how they connect. Existence below the walk band is a predicate
//! (metaplan §1b.3 law 1) and this module is that predicate.
//!
//! Connectivity is the structure's OWN graph, never mesh adjacency — a deep
//! address is identity, not shape (law 3), so two chambers being triangle
//! neighbours means nothing and is not consulted.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use hornvale_kernel::Seed;

    const WALK: u32 = 12;

    fn locale() -> hornvale_kernel::RoomAddr {
        hornvale_kernel::RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        }
    }

    fn built_brief() -> Brief {
        Brief::from_parts(None, None, None, None, true, true)
    }

    #[test]
    fn an_unbuilt_locale_has_no_structure() {
        let wild = Brief::from_parts(None, None, None, None, false, true);
        assert!(structure_at(&locale(), &wild, Seed(42), WALK).is_none());
    }

    #[test]
    fn a_built_locale_has_a_bounded_chamber_set() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        assert!(
            (1..=MAX_CHAMBERS).contains(&s.chambers.len()),
            "sparseness: got {} chambers",
            s.chambers.len()
        );
    }

    #[test]
    fn every_chamber_sits_at_the_chamber_depth_under_this_locale() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        for c in &s.chambers {
            assert_eq!(c.depth(), chamber_depth(WALK));
            assert_eq!(c.face, locale().face);
            assert_eq!(c.path[..WALK as usize], locale().path[..]);
            assert!(c.pack().is_ok(), "a chamber must pack: {c:?}");
        }
    }

    #[test]
    fn chambers_are_distinct() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        let ids: std::collections::BTreeSet<u64> =
            s.chambers.iter().map(|c| c.pack().unwrap().0).collect();
        assert_eq!(ids.len(), s.chambers.len(), "no chamber may repeat");
    }

    #[test]
    fn derivation_is_pure() {
        let a = structure_at(&locale(), &built_brief(), Seed(42), WALK);
        let b = structure_at(&locale(), &built_brief(), Seed(42), WALK);
        assert_eq!(a, b);
    }

    #[test]
    fn a_different_seed_or_locale_gives_a_different_structure() {
        let here = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        let mut elsewhere_path = locale().path;
        elsewhere_path[0] = (elsewhere_path[0] + 1) % 4;
        let elsewhere = hornvale_kernel::RoomAddr {
            face: 3,
            path: elsewhere_path,
        };
        let there = structure_at(&elsewhere, &built_brief(), Seed(42), WALK).expect("built");
        assert_ne!(
            here.chambers, there.chambers,
            "a structure is keyed to its own locale"
        );
    }

    #[test]
    fn the_threshold_is_a_chamber_and_the_graph_is_connected() {
        let s = structure_at(&locale(), &built_brief(), Seed(42), WALK).expect("built");
        assert!(s.chambers.contains(&s.threshold));
        // Every chamber reachable from the threshold by `links`.
        let ti = s.chambers.iter().position(|c| *c == s.threshold).unwrap();
        let mut seen = std::collections::BTreeSet::from([ti]);
        let mut frontier = vec![ti];
        while let Some(i) = frontier.pop() {
            for &(a, b) in &s.links {
                for (x, y) in [(a, b), (b, a)] {
                    if x == i && seen.insert(y) {
                        frontier.push(y);
                    }
                }
            }
        }
        assert_eq!(
            seen.len(),
            s.chambers.len(),
            "closure (§1b.8 rule 3): every chamber reachable from the threshold"
        );
    }
}
```

- [ ] **Step 3: Run to verify failure**

Run: `cargo test -p hornvale-vessel structure:: 2>&1 | tail -20`
Expected: FAIL to compile — `cannot find function structure_at`.

- [ ] **Step 4: Implement**

Add above the test module in `windows/vessel/src/structure.rs`:

```rust
use crate::band::chamber_depth;
use crate::brief::Brief;
use crate::streams::ROOM_CHAMBERS;
use hornvale_kernel::{RoomAddr, Seed};

/// The most chambers one structure may have in v1. A bound, not a target: the
/// point of law 1 is that deep addresses are SPARSE, and an unbounded count
/// would make "every deep address is a place" true by accident.
/// type-audit: bare-ok(count)
pub const MAX_CHAMBERS: usize = 4;

/// The sparse set of chambers standing at one built locale.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Structure {
    /// The chamber `enter` arrives in from the locale.
    pub threshold: RoomAddr,
    /// Every chamber, threshold included. Length is `1..=MAX_CHAMBERS`.
    pub chambers: Vec<RoomAddr>,
    /// Undirected apertures as index pairs into `chambers`. Connected, so
    /// every chamber is reachable from `threshold`.
    /// type-audit: bare-ok(index)
    pub links: Vec<(usize, usize)>,
}

/// The structure at `locale`, or `None` where nothing is built.
///
/// The draw is keyed to the locale's own seed under `room/chambers/v1`, so the
/// same locale in the same world always yields the same structure, and no other
/// locale's draw can perturb it.
/// type-audit: bare-ok(count: walk_depth)
pub fn structure_at(
    locale: &RoomAddr,
    brief: &Brief,
    seed: Seed,
    walk_depth: u32,
) -> Option<Structure> {
    if !brief.built {
        return None;
    }
    let mut stream = locale.seed(seed).derive(ROOM_CHAMBERS).stream();
    // How many chambers: 1..=MAX_CHAMBERS, one draw.
    let count = 1 + (stream.next_u64() as usize) % MAX_CHAMBERS;
    // Which descendants: one draw per chamber, rejecting repeats by scanning
    // forward deterministically rather than re-drawing (a re-draw loop would
    // consume a variable number of draws and make the stream position depend
    // on collisions).
    let depth = chamber_depth(walk_depth);
    let extra = (depth - locale.depth()) as usize;
    let mut chambers: Vec<RoomAddr> = Vec::with_capacity(count);
    for _ in 0..count {
        let draw = stream.next_u64();
        let mut candidate = child_path(locale, draw, extra);
        // Deterministic forward scan on collision: bump the last digit.
        while chambers.contains(&candidate) {
            let last = candidate.path.len() - 1;
            candidate.path[last] = (candidate.path[last] + 1) % 4;
        }
        chambers.push(candidate);
    }
    let threshold = chambers[0].clone();
    // A path graph rooted at the threshold: minimal, connected, and honest
    // about being minimal. Richer topologies are The Precincts' business.
    let links = (1..chambers.len()).map(|i| (i - 1, i)).collect();
    Some(Structure {
        threshold,
        chambers,
        links,
    })
}

/// Extend `locale`'s path by `extra` child digits taken from `draw`, two bits
/// at a time. Integer only.
fn child_path(locale: &RoomAddr, draw: u64, extra: usize) -> RoomAddr {
    let mut path = locale.path.clone();
    for i in 0..extra {
        path.push(((draw >> (2 * i)) & 0b11) as u8);
    }
    RoomAddr {
        face: locale.face,
        path,
    }
}
```

Register in `windows/vessel/src/lib.rs`:

```rust
pub mod structure;
```
```rust
pub use structure::{MAX_CHAMBERS, Structure, structure_at};
```

- [ ] **Step 5: Run to verify pass**

Run: `cargo test -p hornvale-vessel structure:: 2>&1 | tail -12`
Expected: PASS, 7 tests.

- [ ] **Step 6: Confirm the stream manifest regenerates**

Run:
```bash
cargo run -q -p hornvale -- streams | grep -n "room/chambers/v1"
```
Expected: the new label appears. Then regenerate the committed manifest page and inspect the diff — it should contain **only** the new row:
```bash
cargo run -q -p hornvale -- streams > book/src/reference/stream-manifest.md
git diff --stat book/src/reference/
```
If the file path differs, find it with `grep -rln "stream manifest" book/src/reference/`.

- [ ] **Step 7: Format, audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/structure.rs windows/vessel/src/streams.rs windows/vessel/src/lib.rs book/src/reference/
git commit -m "feat(vessel): which chambers exist, under room/chambers/v1

Existence below the walk band is a predicate: no structure where nothing
is built, and a bounded chamber set where something is. The label is
deliberately not room/furnishing/v1 -- chamber existence and pattern
selection churn independently, and merging them would put a frequent
bump inside a label whose blast radius includes committed drive history.
Collisions resolve by a deterministic forward scan rather than a re-draw,
so the stream position never depends on how many collisions occurred."
```

---

### Task 5: Chamber prose

**Files:**
- Create: `windows/vessel/src/chamber_prose.rs`
- Modify: `windows/vessel/src/lib.rs`

**Interfaces:**
- Consumes: `crate::interior::{Interior, AnchorKind}`; `crate::brief::Brief`.
- Produces: `describe_chamber(interior: &Interior, brief: &Brief) -> String`.

**Why this exists rather than reusing the locale describer — measured, not assumed.** `hornvale locale --world W --at 10,20 --depth 21` returns `biome: bathypelagic · elevation -4149 m · regime: unremarkable ground sun-warmed dry in a hollow`. It runs at chamber depth and produces *terrain* prose, with the micro-regime descriptor changing per depth because the noise samples at the room's own scale. Pointed at a 3 m room it would describe a dwelling as seafloor.

- [ ] **Step 1: Write the failing tests**

**Register the module in the same step** — add `pub mod chamber_prose;` to
`windows/vessel/src/lib.rs` before running the tests, or the file is not
compiled and the filter matches zero tests rather than failing (see Task 3's
note).

Create `windows/vessel/src/chamber_prose.rs`:

```rust
//! Prose for a CHAMBER. Deliberately not `windows/locale`'s describer: that one
//! is written for ~1.7 km places and, verified at depth 21, reports biome,
//! elevation and a terrain micro-regime — it would describe a dwelling's room
//! as seafloor. Prose is the constitutionally primary surface (§3.5), so a
//! chamber gets its own sentence built from what the chamber actually holds.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::interior::{AnchorKind, Interior};

    fn brief() -> Brief {
        Brief::from_parts(None, None, None, None, true, true)
    }

    fn interior_with(kinds: &[AnchorKind]) -> Interior {
        let mut i = Interior::new();
        let mut prev = None;
        for &k in kinds {
            let id = i.push(k, None);
            if let Some(p) = prev {
                i.connect(p, id);
            }
            prev = Some(id);
        }
        i
    }

    #[test]
    fn a_chamber_names_what_it_holds() {
        let text = describe_chamber(
            &interior_with(&[AnchorKind::Ground, AnchorKind::Hearth, AnchorKind::Bed]),
            &brief(),
        );
        assert!(text.contains("hearth"), "got: {text}");
        assert!(text.contains("bed"), "got: {text}");
    }

    #[test]
    fn a_chamber_never_speaks_of_terrain() {
        let text = describe_chamber(
            &interior_with(&[AnchorKind::Ground, AnchorKind::Hearth]),
            &brief(),
        );
        for banned in ["biome", "elevation", "moisture", "regime", "ground shaded"] {
            assert!(
                !text.contains(banned),
                "chamber prose leaked terrain vocabulary {banned:?}: {text}"
            );
        }
    }

    #[test]
    fn an_empty_chamber_still_reads_as_a_place() {
        let text = describe_chamber(&interior_with(&[AnchorKind::Ground]), &brief());
        assert!(!text.trim().is_empty());
        assert!(text.ends_with('.'), "prose is a sentence: {text}");
    }

    #[test]
    fn prose_is_a_pure_function_of_the_interior_and_brief() {
        let i = interior_with(&[AnchorKind::Ground, AnchorKind::Hearth]);
        assert_eq!(describe_chamber(&i, &brief()), describe_chamber(&i, &brief()));
    }

    #[test]
    fn the_brief_changes_the_word_for_the_place() {
        // `brief` must be READ, not merely carried: a built place is a room,
        // an unbuilt one is a hollow.
        let i = interior_with(&[AnchorKind::Ground, AnchorKind::Hearth]);
        let wild = Brief::from_parts(None, None, None, None, false, true);
        assert_ne!(describe_chamber(&i, &brief()), describe_chamber(&i, &wild));
        assert!(describe_chamber(&i, &wild).contains("hollow"));
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel chamber_prose:: 2>&1 | tail -20`
Expected: FAIL — `cannot find function describe_chamber`.

- [ ] **Step 3: Implement**

Add above the tests:

```rust
use crate::brief::Brief;
use crate::interior::{AnchorKind, Interior};

/// The noun for an anchor kind, as prose says it. `Ground` has no noun: it is
/// the chamber's own floor, not a thing standing in it.
fn noun(kind: AnchorKind) -> Option<&'static str> {
    match kind {
        AnchorKind::Ground => None,
        AnchorKind::Hearth => Some("a hearth"),
        AnchorKind::Threshold => Some("a doorway"),
        AnchorKind::Bed => Some("a bed"),
        AnchorKind::Vessel => Some("a water jar"),
        AnchorKind::Screen => Some("a screen"),
        AnchorKind::Alcove => Some("an alcove"),
        AnchorKind::Pool => Some("a still pool"),
        AnchorKind::Log => Some("a fallen log"),
    }
}

/// One sentence for a chamber: what stands in it, in the interior's own
/// deterministic anchor order.
///
/// `brief` is read, not carried: a built place is a *room*, an unbuilt one is a
/// *hollow*, and that single word is the difference between a dwelling and a
/// cave mouth. (An unused parameter would be dead weight and a reviewer would
/// be right to flag it.)
/// type-audit: bare-ok(prose: return)
pub fn describe_chamber(interior: &Interior, brief: &Brief) -> String {
    let place = if brief.built { "room" } else { "hollow" };
    let nouns: Vec<&'static str> = interior
        .ids()
        .iter()
        .filter_map(|&id| noun(interior.anchor(id).kind))
        .collect();
    match nouns.len() {
        0 => format!("A bare {place}, its floor swept and its corners empty."),
        1 => format!("A small {place}. {} stands here.", capitalize(nouns[0])),
        _ => {
            let (last, rest) = nouns.split_last().expect("len >= 2");
            format!("A small {place}, holding {} and {}.", rest.join(", "), last)
        }
    }
}

/// Capitalize a noun phrase's first letter for sentence-initial use.
fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}
```

Register in `windows/vessel/src/lib.rs`:

```rust
pub mod chamber_prose;
```
```rust
pub use chamber_prose::describe_chamber;
```

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-vessel chamber_prose:: 2>&1 | tail -12`
Expected: PASS, 4 tests.

- [ ] **Step 5: Format, audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/chamber_prose.rs windows/vessel/src/lib.rs
git commit -m "feat(vessel): chamber prose, which is not the locale describer

Verified at depth 21, the locale describer reports biome, elevation and a
terrain micro-regime -- it would describe a dwelling's room as seafloor.
A chamber gets its own sentence built from the anchors it actually holds,
and a test bans terrain vocabulary from it outright."
```

---

### Task 6: The seam — `enter` and `out`

**Files:**
- Modify: `windows/vessel/src/session.rs` (verb dispatch ~line 563-586; add state and two handlers)
- Test: `windows/vessel/tests/the_lintel.rs` (create)

**Interfaces:**
- Consumes: everything from Tasks 1-5: `structure_at`, `brief_of`, `chamber_interior_of`, `describe_chamber`, `band_of`.
- Produces: session behaviour only — no new public API beyond what Tasks 1-5 exported.

**Session state to add** (all transient; nothing commits — `Session::go` already mutates `self.agent.position` without committing a fact, and the session ledger is written only by `wait`'s tick and never written back):

```rust
/// The structure the possession is inside, if any, and which chamber. `None`
/// at the walk band. Never serialized: a band change is session state, so
/// entering and leaving cannot alter the world (decision 0069's property,
/// obtained by construction).
inside: Option<(Structure, usize)>,
```

- [ ] **Step 1: Write the failing integration test**

Create `windows/vessel/tests/the_lintel.rs`:

```rust
//! The Lintel's headline: `enter` at a built locale reaches a chamber and `out`
//! returns. Observed through a real `Session`, not demonstrated in a unit test —
//! the campaign's whole point is that descent EXISTS.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// Seed 42 — the canonical world, and it has a settlement (its village is
/// `Qvooshtvoagootao`), so possession succeeds. Setup copied from
/// `windows/vessel/tests/the_purview.rs`.
fn world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// The refusal reserved for the COARSE direction. Byte-pinned in the galleries.
const COARSE_REFUSAL: &str = "The grain of the world resists";

/// Walk until `enter` succeeds, up to `limit` steps, returning the reply that
/// worked. Scouts rather than assuming the start is built: which locales carry
/// settlement territory is a geography accident, and The Snapshot's plan
/// established that a driver must scout rather than hardcode.
fn enter_somewhere_built(session: &mut Session<'_>, limit: usize) -> Option<String> {
    for step in 0..limit {
        let reply = out(session.handle("enter"));
        if !reply.starts_with("Nothing here is built") {
            return Some(reply);
        }
        // Not built here — step along and try again.
        let moved = out(session.handle(if step % 2 == 0 { "go n" } else { "go ne" }));
        if moved.starts_with("No way") {
            let _ = session.handle("back");
        }
    }
    None
}

#[test]
fn enter_reaches_a_chamber_and_out_returns() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");

    let entered = enter_somewhere_built(&mut session, 12)
        .expect("seed 42 has settlement territory within a dozen steps of its flagship");

    assert!(
        !entered.contains(COARSE_REFUSAL),
        "fine-ward enter must not hit the coarse refusal: {entered:?}"
    );
    for terrain_word in ["biome", "elevation", "moisture", "regime"] {
        assert!(
            !entered.contains(terrain_word),
            "a chamber must not be described as terrain ({terrain_word}): {entered:?}"
        );
    }

    let before_out = out(session.handle("look"));
    let left = out(session.handle("out"));
    assert!(!left.is_empty(), "leaving says something");
    let after_out = out(session.handle("look"));
    assert_ne!(
        before_out, after_out,
        "look inside and look outside must differ, or `out` did nothing"
    );
}

#[test]
fn exit_is_still_refused_coarse_ward() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    assert!(
        out(session.handle("exit")).contains(COARSE_REFUSAL),
        "the coarse-ward refusal is byte-pinned in the galleries"
    );
}

#[test]
fn entering_where_nothing_is_built_gives_a_physical_reason() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    // Walk away from the settlement until a locale reports nothing built.
    let mut refusal = None;
    for _ in 0..12 {
        let reply = out(session.handle("enter"));
        if reply.starts_with("Nothing here is built") {
            refusal = Some(reply);
            break;
        }
        let _ = session.handle("go n");
    }
    let refusal = refusal.expect("wilderness lies within a dozen steps of a village");
    assert!(!refusal.contains(COARSE_REFUSAL), "{refusal:?}");
    assert!(
        !refusal.to_lowercase().contains("error"),
        "a physical reason, not an error: {refusal:?}"
    );
}

#[test]
fn entering_and_leaving_commits_nothing() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).expect("possession starts");
    let before = session.committed_agent_at_count();
    let _ = enter_somewhere_built(&mut session, 12);
    let _ = session.handle("out");
    assert_eq!(
        session.committed_agent_at_count(),
        before,
        "a band change is session state; nothing commits"
    );
}
```

**Two notes for the implementer.** `Session::start` returns a tuple whose second element the sibling test discards; keep that shape. And if `Session` carries a lifetime parameter different from `Session<'_>`, match whatever `the_purview.rs` uses — copy, do not guess.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --test the_lintel 2>&1 | tail -20`
Expected: FAIL — unresolved helpers, then (once the helpers are real) the `enter` assertions fail because `enter` still hits the refusal.

- [ ] **Step 3: Make the refusal directional and implement `enter` / `out`**

In `windows/vessel/src/session.rs`, replace the combined arm:

```rust
            "enter" | "exit" => Turn::Out(
                "The grain of the world resists; that way lies another scale of things."
                    .to_string(),
            ),
```

with:

```rust
            "enter" => self.enter(rest),
            "out" => self.leave(),
            // Coarse-ward is still refused: possessing a settlement, a culture
            // or a civilization is a deferred arc of its own (0077). This
            // sentence is byte-pinned in the galleries — do not reword it.
            "exit" => Turn::Out(
                "The grain of the world resists; that way lies another scale of things."
                    .to_string(),
            ),
```

Add the two handlers near `fn go`:

```rust
    /// Descend into the structure at this locale, or move to a named chamber
    /// within the one already entered. Apertures, not stairs (§7): movement
    /// inside is by name, never by compass, because a chamber address is
    /// identity and carries no bearing.
    fn enter(&mut self, target: &str) -> Turn {
        // Already inside: `enter <named>` steps through an aperture.
        if let Some((structure, at)) = self.inside.clone() {
            let Some(next) = self.named_neighbour(&structure, at, target) else {
                return Turn::Out(format!(
                    "There is no way to {} from here.",
                    if target.is_empty() { "anywhere" } else { target }
                ));
            };
            self.inside = Some((structure, next));
            return self.out(self.describe_chamber_here());
        }
        let brief = self.brief_here();
        let Some(structure) = crate::structure::structure_at(
            &crate::band::truncate_to_walk(&self.agent.position, self.walk_depth()),
            &brief,
            self.world.seed,
            self.walk_depth(),
        ) else {
            return Turn::Out("Nothing here is built; there is nothing to enter.".to_string());
        };
        let at = structure
            .chambers
            .iter()
            .position(|c| *c == structure.threshold)
            .expect("the threshold is one of the chambers");
        self.inside = Some((structure, at));
        self.out(self.describe_chamber_here())
    }

    /// Step back out — to the chamber entered from, or to the locale.
    fn leave(&mut self) -> Turn {
        match self.inside.take() {
            None => Turn::Out("You are already out of doors.".to_string()),
            Some(_) => self.out(self.describe_here()),
        }
    }
```

Add the three small helpers the handlers use (`brief_here`, `describe_chamber_here`, `named_neighbour`, `walk_depth`) beside them. `named_neighbour` matches `target` against the chamber's own prose nouns, sharing the noun catalogue with `describe_chamber` exactly as The Purview's chart shares the prose's nouns; with an empty `target` and exactly one neighbour, take it.

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-vessel --test the_lintel 2>&1 | tail -12`
Expected: PASS, 4 tests.

- [ ] **Step 5: Prove byte-identity**

Run:
```bash
cargo test -p hornvale-vessel 2>&1 | tail -6
cargo run -q --release -p hornvale -- new --seed 42 --out /tmp/lintel-42.json
cargo run -q --release -p hornvale -- new --seed 42 --out /tmp/lintel-42b.json
cmp /tmp/lintel-42.json /tmp/lintel-42b.json && echo "self-identical"
git stash push -u -m "lintel-byte-check-$$"
```
Then, on the stashed (pre-change) tree, build seed 42 again and compare against `/tmp/lintel-42.json`; restore with `git stash apply <sha>` per the worktree stash discipline (capture the SHA from `git stash list --format='%H %gs'`, apply by SHA, then drop it — never bare `git stash pop`).
Expected: **identical**. If it differs, stop: something moved `interior_of` or the stream position.

- [ ] **Step 6: Format, audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/session.rs windows/vessel/tests/the_lintel.rs
git commit -m "feat(vessel): enter and out — the descent seam

The refusal becomes directional: coarse-ward exit keeps its byte-pinned
sentence, fine-ward enter descends where something is built and fails
with a physical reason where nothing is. Movement inside is by named
aperture, never compass, because a chamber address is identity and
carries no bearing. Nothing commits -- a band change is session state,
so entering and leaving cannot alter the world."
```

---

### Task 7: Close — artifacts, book, registry, decision

**Files:**
- Modify: `scripts/possession-walk.txt` (in its **own** commit)
- Create: `book/src/chronicle/the-lintel.md`, `docs/retrospectives/the-lintel.md`, `docs/decisions/NNNN-locale-chamber-place.md`
- Modify: `book/src/frontier/idea-registry.md`, the possession/game and room-mesh chapters

- [ ] **Step 1: Extend the walk script, alone**

Append to `scripts/possession-walk.txt` after `map out 2`:

```
enter
look
out
```

Regenerate and inspect:
```bash
bash scripts/regenerate-artifacts.sh
git diff --stat book/src/gallery/
git diff book/src/gallery/ | head -60
```
Expected: the diff contains **only** the new verbs' output. Verify every changed line is attributable to `enter`/`look`/`out` before committing.

- [ ] **Step 2: Commit the artifact drift by itself**

```bash
git add scripts/possession-walk.txt book/src/gallery/
git commit -m "test(vessel): walk the descent in the seed-42 gallery

Isolated in its own commit so the artifact drift is one reviewable diff
whose every line is the new verbs' output."
```

- [ ] **Step 3: Write the chronicle entry**

Create `book/src/chronicle/the-lintel.md` following a recent sibling's shape (read `book/src/chronicle/the-threshold.md`). Cover: the scale collision and how it was found; that a locale is ~1.7 km and a chamber ~3.3 m; that descent commits nothing; the walk-band truncation footgun. **No registry IDs** — the drift-check bans them outside `book/src/frontier/`. Add the entry to `book/src/SUMMARY.md`.

- [ ] **Step 4: Write the decision record**

Find the next free number (`ls docs/decisions/ | tail -5`) and create `docs/decisions/NNNN-locale-chamber-place.md` recording the terminology: locale / chamber / place, with "room" unqualified retired. Follow the format in `docs/decisions/README.md`. State the context (the collision produced a wrong design before it was caught), the decision, and the consequence (new doc comments and specs use the band words).

- [ ] **Step 5: Book freshness sweep**

```bash
grep -rln "\broom\b" book/src/ | head -20
```
Update the room-mesh chapter and the possession/game chapters where "room" now needs to say locale or chamber. Do not rewrite history in chronicle entries — earlier campaigns said "room" and that is what they said.

- [ ] **Step 6: Flip the registry rows**

In `book/src/frontier/idea-registry.md`: flip `CLIENT-scale-bands` and `CLIENT-brief-is-the-contract` to `shipped` with **Where** repointed at the chronicle and spec; and **correct `CLIENT-depth-follows-content`**, which still describes automatic band transitions — overturned by ledger #12, since transitions happen only at thresholds.

- [ ] **Step 7: Retrospective**

Create `docs/retrospectives/the-lintel.md` — process lessons, not product (decision 0020). Candidates from this campaign's own history: the label claim that was wrong until `streams.rs` was read; the debug-vs-release measurement near-miss; and that two of the brainstorm's overturns corrected decisions the same session had adopted.

- [ ] **Step 8: The full gate and the drift check**

```bash
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo nextest run --workspace
cargo test --workspace --doc
cargo test -p hornvale --test docs_consistency
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
make gate-full
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```
Expected: all green, and the final `git diff --exit-code` clean (any drift must already be committed in Step 2).

- [ ] **Step 8a: Run the health null-control explicitly**

`make gate-full` runs the `heavy:`-tagged tier but **not** the calibration
batteries, which carry non-`heavy:` ignore reasons. The spec's success criteria
name the health null-control, so run it by hand:

```bash
cargo test -p hornvale-lab --test health_calibration -- --ignored --nocapture 2>&1 | tail -20
```
Expected: green — chronicity stays zero and every distress run recovers. This is
a **check**, not this campaign's gate: §2 keeps behaviour still, so a failure here
means something moved that should not have, not that a threshold needs adjusting.
Never edit the study to match a new number (decision 0016).

- [ ] **Step 8b: Re-score the Confidence Gradient if a bet moved**

```bash
grep -n "situated\|spatial\|scale\|client" book/src/open-questions.md | head -20
```
If any bet in the Confidence Gradient is resolved or moved by descent existing,
re-score that row and say why in one line (decision 0030). If none moved, state
that in the retrospective rather than silently skipping the step.

- [ ] **Step 9: Commit the close**

```bash
git add book/ docs/
git commit -m "docs(the-lintel): close — chronicle, decision, retro, registry, sweep"
```

---

## Notes for the executing agent

- **Stop at three failed attempts on any step** and report what failed, the exact error, and why you think it failed. Do not patch around a byte-identity failure — it means something moved that must not move.
- **Never use `--no-verify`**, never disable a test, never edit a test to match a new result (decision 0016's discipline generalises).
- **The stash is shared across worktrees.** If you must stash, use `git stash push -u -m "<unique-tag>"`, capture the SHA from `git stash list --format='%H %gs'`, restore with `git stash apply <sha>`, and drop it by re-finding its index by tag. Never bare `git stash pop`.
- **Run the cheap checks first** (`fmt`, `clippy`, the one crate's tests); `--workspace` belongs at the commit gate, not after every edit.
