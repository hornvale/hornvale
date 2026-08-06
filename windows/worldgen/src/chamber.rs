//! The chamber address lattice (The Deep Realm, Task 2).
//!
//! A chamber is a **node addressed in a fixed lattice, sparsely occupied**
//! (spec §3): existence and content are pure functions of its address, and
//! nothing about a chamber is ever stored. The lattice exists before
//! anything is generated into it, so an address can never encode a
//! construction step — see [`ChamberAddr`]'s own docs for why that matters
//! and what has gone wrong elsewhere in this codebase when it wasn't true.
//!
//! [`chamber_exists`] and [`chamber_at`] take a [`hornvale_terrain::Cave`] as
//! their only cave-specific input, and read exactly one field off it —
//! `deepest_band`, the depth **budget** a real cave system measured (spec
//! §3, "a system whose deepest band is `Roots` grows a graph down to
//! `Roots`"). `Cave::kind` is not read here: Task 6 is where a cave's
//! formation process might shape subterranean conditions, and inventing that
//! coupling now would be scope this task does not own.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, Seed, Stream};
use hornvale_terrain::{BandKind, Cave};

/// Slots in the fixed lattice per depth band. Constant regardless of what
/// any particular cave realizes — rule 1a: this is the lattice's own size,
/// never a count of what a generator produced.
///
/// **4**, chosen from Task 0's measured substrate rather than invented: a
/// cave reaching `BandKind::Roots` (the deepest band the live generator
/// ever produces — Task 0, 30 seeds, 55,947 caves) spans 4 rungs of
/// `Realm::UNDERDARK.strata()` (`Regolith..=Roots`), so 4 slots per band
/// keeps a full system's address space (`SLOTS_PER_BAND * 4` = 16) the same
/// order of magnitude as the band ladder itself — enough for `slot` to be a
/// genuinely separate axis from `band`, not so large that Task 3's
/// per-chamber neighbour walk or Task 8's per-cave enumeration become
/// expensive. This is a lattice-size judgement call, not a measured
/// quantity; Task 8's H2 readout is where the shape of chamber density
/// gets scientific scrutiny, and it can only widen this constant, never
/// relocate an existing address, because `slot` numbers positions in the
/// lattice, not generated chambers.
/// type-audit: bare-ok(count)
pub const SLOTS_PER_BAND: u8 = 4;

/// The fraction of in-budget addresses that exist, in expectation. A
/// coin-flip midpoint, not a tuned density curve: this task ships the
/// address lattice and its existence gate, and Task 8's H2 readout is
/// where the shape of chamber density is measured against the spec's
/// falsification. Kept private — not a save-format contract by itself,
/// only [`chamber_key`]'s string and [`crate::streams::CHAMBER`] are; this
/// constant may move without relocating any address, because it only
/// changes which draws cross a threshold, not what the draws are keyed on.
const EXISTENCE_DENSITY: f64 = 0.5;

/// An address in the chamber lattice — a **place**, never a construction
/// step (spec §3.1). Four small integers name: which cell, which entrance
/// of that cell, which depth band, and which slot within that band.
///
/// **`band` indexes `hornvale_climate::Realm::UNDERDARK.strata()`, never a
/// count of the bands a particular cave realizes** (rule 1a, added after
/// Task 0 measured that the live generator only ever produces 3 of the
/// ladder's 5 values). The permanent ladder has 5 rungs regardless of what
/// any world's caves reach, so `band` stays meaningful even if the open
/// `MAP-cave-depth-weld` fix changes which bands occur.
///
/// This is the third time the project has met the "generation order is
/// never an identity" wall (The Salt, decision 0102, The Tolerance) — see
/// `windows/worldgen/src/lib.rs`'s `deity_name_seed`, the pattern this
/// type's derivation copies, for the fix's own history: a naming stream
/// once welded to an entity id had to be re-cut as a `/v2` epoch. Nothing
/// here carries a generation ordinal so that mistake cannot recur.
///
/// Deliberately carries **no `Serialize`/`Deserialize`**: nothing in this
/// campaign writes a `ChamberAddr` to a ledger, and the moment one is
/// committed its on-disk spelling becomes a permanent key (spec §3.1).
/// type-audit: bare-ok(index: entrance), bare-ok(index: band), bare-ok(index: slot)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ChamberAddr {
    /// Which surface cell this chamber lies beneath.
    pub cell: CellId,
    /// Which entrance of that cell's cave system this chamber's descent
    /// starts from. Today's terrain model reports one cave per cell with no
    /// separate aperture count (`hornvale_terrain::GeneratedTerrain::
    /// cave_at`), so every current caller passes `0`; the field exists so a
    /// future terrain change (multiple apertures into one system, spec
    /// §3.4's `ShaftNet` rung) does not require relayering this type.
    pub entrance: u8,
    /// Which rung of `hornvale_climate::Realm::UNDERDARK.strata()` this
    /// chamber sits at. Indexes the permanent 5-rung ladder — see this
    /// type's own docs and rule 1a.
    pub band: u8,
    /// Which position in the fixed per-band lattice (`0..SLOTS_PER_BAND`)
    /// this chamber occupies. A lattice coordinate, not a count of
    /// chambers generated.
    pub slot: u8,
}

/// A chamber's resolved content — deliberately minimal for this task. Spec
/// §3's substrate table lists a chamber's content as "depth — `BandKind` on
/// each node — what the rock here is like", and that is exactly what this
/// carries: the named stratum its address's `band` indexes. Later tasks
/// extend this (Task 5's descent narration, Task 6's derived subterranean
/// conditions); a holding's dug-out dressing (spec §4) is explicitly out of
/// this campaign's scope, so this task does not invent fields for it.
///
/// Content is a pure function of `addr` alone — never of the `Cave` that
/// gated its existence. `an_addresss_meaning_does_not_depend_on_which_
/// other_chambers_exist` in `deep_realm_chamber.rs` is the regression guard:
/// a `Chamber` for one address must come out identical no matter which cave
/// (shallow or deep) was asked, for every address both caves admit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Chamber {
    /// The address this content was derived for.
    pub addr: ChamberAddr,
    /// The rock stratum at `addr.band` — `Realm::UNDERDARK.strata()[addr.band]`.
    pub stratum: hornvale_climate::Stratum,
}

/// The one explicit mapping between a NAMED [`BandKind`] (what
/// [`Cave::deepest_band`] carries) and `Realm::UNDERDARK.strata()`'s index
/// space (what [`ChamberAddr::band`] indexes) — rule 1a's "compare them
/// through one explicit mapping, in one place." [`chamber_exists`] is the
/// only caller, so a lattice reader and the depth-budget gate can never
/// diverge on what a `band` number means. Exhaustive: a sixth `BandKind`
/// variant fails this to compile rather than silently misplacing it.
fn band_rank(band: BandKind) -> u8 {
    match band {
        BandKind::Regolith => 0,
        BandKind::Cover => 1,
        BandKind::Basement => 2,
        BandKind::Roots => 3,
        BandKind::Underneath => 4,
    }
}

/// The inverse of [`band_rank`]: the band a [`ChamberAddr::band`] index names.
/// `None` for a rank past the ladder's end, so no caller can index out of it.
///
/// Kept beside `band_rank` on purpose — the two are one bijection, and a sixth
/// `BandKind` variant fails *both* to compile rather than leaving the pair
/// half-updated.
fn band_of_rank(rank: u8) -> Option<BandKind> {
    match rank {
        0 => Some(BandKind::Regolith),
        1 => Some(BandKind::Cover),
        2 => Some(BandKind::Basement),
        3 => Some(BandKind::Roots),
        4 => Some(BandKind::Underneath),
        _ => None,
    }
}

/// A band's spelling inside the chamber key — **a save-format contract**, and
/// the reason this is an explicit match rather than `format!("{band:?}")`.
///
/// A derived `Debug` impl renders the variant's identifier, which *looks* like
/// exactly this table and is not the same promise. `Debug` is a diagnostic
/// facility: nothing stops a later reader from writing a hand-rolled `Debug`
/// for [`BandKind`] to make some log prettier, and doing so would silently
/// re-key every chamber in every world with no test able to see it. Stating
/// the strings here makes the contract reviewable, makes a rename an obvious
/// epoch decision, and forces a sixth variant to choose its own spelling
/// instead of inheriting one.
fn band_name(band: BandKind) -> &'static str {
    match band {
        BandKind::Regolith => "regolith",
        BandKind::Cover => "cover",
        BandKind::Basement => "basement",
        BandKind::Roots => "roots",
        BandKind::Underneath => "underneath",
    }
}

/// The one place the `chamber/v1` stream key is spelled — mirrors
/// `deity_base_seed`'s discipline (`windows/worldgen/src/lib.rs`): "the one
/// place the stream label is spelled, so [every caller] can never diverge."
/// [`chamber_stream`] is the only caller.
///
/// **`cell`, `entrance` and `slot` are genuine integers naming a place and
/// are spelled decimal. `band` is spelled by its `BandKind` NAME, never its
/// numeric index.** An index is a declaration position: if `Stratum`/
/// `BandKind` ever gains a variant in the middle of the ladder — the open
/// `MAP-cave-depth-weld` work is the named candidate — every index below it
/// shifts, and a numeral-keyed chamber would silently move to a different
/// derived stream. Spelling the name instead means the key only changes if
/// the *name itself* changes, which is the same discipline a `stream_labels!`
/// rename already carries (an epoch suffix, never silent). This is rule 1a
/// one level down, applied to the derivation instead of the address type.
///
/// The name comes from [`band_name`]'s explicit table, **not** from a `Debug`
/// impl — see that function for why the distinction is load-bearing rather
/// than stylistic. A `band` past the ladder's end spells as `"out-of-ladder"`;
/// it is unreachable through either public entry point (both gate on
/// [`band_rank`] first), and naming it beats both a panic and a silent
/// collision with band 0.
fn chamber_key(addr: ChamberAddr) -> String {
    let band = band_of_rank(addr.band).map_or("out-of-ladder", band_name);
    format!("{}/{}/{band}/{}", addr.cell.0, addr.entrance, addr.slot)
}

/// The stream a chamber's own derivations draw from — [`chamber_key`]
/// composed under [`crate::streams::CHAMBER`], following the composed-label
/// pattern at `windows/worldgen/src/lib.rs`'s `deity_name_seed`.
///
/// **Precondition:** `addr.band` must be `< 5` (a valid index into
/// `Realm::UNDERDARK.strata()`). Both callers ([`chamber_exists`] and
/// [`chamber_at`]) only reach this after `addr.band` has already been
/// checked against a cave's budget via [`band_rank`], whose maximum return
/// value is `4`, so an out-of-range `band` can never survive to here.
fn chamber_stream(seed: Seed, addr: ChamberAddr) -> Stream {
    seed.derive(crate::streams::CHAMBER)
        .derive(StreamLabel::dynamic(&chamber_key(addr)))
        .stream()
}

/// Whether a chamber exists at `addr`, under `cave`'s measured depth
/// budget. Sparse and derived: no chamber is ever stored, so "exists" is a
/// per-address predicate — a fixed-density draw, gated so `addr.band`
/// reaches no deeper than `cave.deepest_band` (spec §3: the budget a real
/// cave system measured).
///
/// An out-of-lattice `slot` (`>= SLOTS_PER_BAND`) never exists — the
/// lattice is fixed-size, and an address outside it names nowhere.
/// type-audit: bare-ok(flag: return)
pub fn chamber_exists(seed: Seed, cave: &Cave, addr: ChamberAddr) -> bool {
    if addr.slot >= SLOTS_PER_BAND {
        return false;
    }
    if addr.band > band_rank(cave.deepest_band) {
        return false;
    }
    chamber_stream(seed, addr).next_f64() < EXISTENCE_DENSITY
}

/// A chamber's resolved content at `addr`, under `cave`'s measured depth
/// budget — `None` when [`chamber_exists`] is `false`, else the
/// address-derived [`Chamber`]. See [`Chamber`]'s own docs for why its
/// content never depends on `cave` beyond the existence gate.
pub fn chamber_at(seed: Seed, cave: &Cave, addr: ChamberAddr) -> Option<Chamber> {
    if !chamber_exists(seed, cave, addr) {
        return None;
    }
    let stratum = hornvale_climate::Realm::UNDERDARK.strata()[addr.band as usize];
    Some(Chamber { addr, stratum })
}

/// The chambers adjacent to `addr` that exist under `cave`'s depth budget —
/// `addr`'s passages.
///
/// **This is what dissolves spec §3.2's "one genuinely hard problem."** That
/// problem, as stated, is deriving A's neighbours and B's neighbours
/// independently and needing them to agree — two separate derivations that
/// could drift. This function does not have that shape at all: **adjacency
/// is a pure, symmetric function of two addresses alone**, defined once,
/// here, and existence (via [`chamber_exists`]) is likewise a pure function
/// of `(seed, cave, addr)`. Two addresses `A` and `B` are adjacent exactly
/// when they differ in exactly one axis by exactly one step:
///
/// - same `band`, `slot` differing by 1, **or**
/// - same `slot`, `band` differing by 1.
///
/// "Differs by 1" is symmetric in its two arguments by inspection — it is
/// not computed relative to a starting address, so there is nothing that
/// could make `A`'s view of the relation disagree with `B`'s. Consequently
/// `passages_from(A)` contains `B` if and only if `passages_from(B)`
/// contains `A`, for any two addresses, with nothing stored and nothing to
/// keep in sync. **A future edit that makes adjacency depend on anything
/// other than the two addresses themselves — which chambers happen to
/// exist, a generation order, which one was asked first — re-creates the
/// exact problem this function exists to dissolve.** If you are tempted to
/// special-case a direction, that temptation is the bug.
///
/// **Neither axis wraps.** `slot` does not wrap modularly (slot `0` is
/// adjacent only to slot `1`, not also to `SLOTS_PER_BAND - 1`), matching
/// `band`, which cannot wrap either — there is no rung before `Regolith` or
/// after `Underneath` for it to wrap into. Keeping both axes non-wrapping
/// means the lattice has one consistent shape rather than one axis behaving
/// like a line and the other like a ring; end slots and end bands simply
/// have fewer neighbours, which is the ordinary edge-of-space behaviour a
/// bounded lattice should have. Non-wrapping is symmetric for the same
/// reason wrapping would have been: "differs by 1" (or, under a modular
/// scheme, "differs by 1 mod N") is symmetric either way, so this choice is
/// about lattice shape, not about which option the two-way test would catch
/// — an asymmetric IMPLEMENTATION of either scheme (for instance, computing
/// one direction with wrapping arithmetic and the other without) is what the
/// test guards against, not the choice itself.
///
/// A non-existent `addr` has no passages — there is nothing to traverse
/// from nowhere — so this returns an empty `Vec` without deriving any
/// candidate neighbours.
pub fn passages_from(seed: Seed, cave: &Cave, addr: ChamberAddr) -> Vec<ChamberAddr> {
    if !chamber_exists(seed, cave, addr) {
        return Vec::new();
    }

    let mut candidates = Vec::new();

    // Same band, adjacent slot. Guaranteed not to underflow/overflow: addr
    // passed the chamber_exists check above, so addr.slot < SLOTS_PER_BAND.
    if addr.slot > 0 {
        candidates.push(ChamberAddr {
            slot: addr.slot - 1,
            ..addr
        });
    }
    if addr.slot + 1 < SLOTS_PER_BAND {
        candidates.push(ChamberAddr {
            slot: addr.slot + 1,
            ..addr
        });
    }

    // Same slot, adjacent band. Guaranteed not to underflow/overflow: addr
    // passed the chamber_exists check above, so addr.band <= band_rank(cave.
    // deepest_band) <= 4 (band_rank's maximum return value).
    if addr.band > 0 {
        candidates.push(ChamberAddr {
            band: addr.band - 1,
            ..addr
        });
    }
    candidates.push(ChamberAddr {
        band: addr.band + 1,
        ..addr
    });

    candidates.retain(|&candidate| chamber_exists(seed, cave, candidate));
    candidates
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The chamber key is a **save-format contract**: `StreamLabel::dynamic`
    /// hashes this string, so its spelling determines every chamber's derived
    /// stream forever. Nothing else in the workspace pins it, and a contract
    /// no failing test defends is a claim rather than a guarantee (The Vigil).
    ///
    /// If this test fails, you have re-keyed every chamber in every world.
    /// That is an **epoch** (`chamber/v2`), not a fix to this assertion.
    #[test]
    fn the_chamber_key_spelling_is_pinned() {
        assert_eq!(
            chamber_key(ChamberAddr {
                cell: CellId(9),
                entrance: 0,
                band: 2,
                slot: 3,
            }),
            "9/0/basement/3"
        );
        assert_eq!(
            chamber_key(ChamberAddr {
                cell: CellId(0),
                entrance: 1,
                band: 0,
                slot: 0,
            }),
            "0/1/regolith/0"
        );
    }

    /// The band is spelled by NAME, never by index — rule 1a one level down.
    /// A numeral here would mean that inserting a `BandKind` variant mid-ladder
    /// silently moved every chamber below it to a different stream.
    #[test]
    fn the_key_names_its_band_rather_than_numbering_it() {
        let key = chamber_key(ChamberAddr {
            cell: CellId(7),
            entrance: 0,
            band: 3,
            slot: 1,
        });
        assert!(
            key.contains("roots"),
            "band must be spelled by name; got {key:?}"
        );
        assert!(
            !key.contains("/3/"),
            "band appears as a bare index in {key:?} — an index is a \
             declaration position, not a place"
        );
    }

    /// `band_rank` and `band_of_rank` are one bijection. Kept honest here so
    /// the pair cannot drift half-updated when a sixth `BandKind` lands.
    #[test]
    fn band_rank_and_band_of_rank_round_trip() {
        for band in [
            BandKind::Regolith,
            BandKind::Cover,
            BandKind::Basement,
            BandKind::Roots,
            BandKind::Underneath,
        ] {
            assert_eq!(band_of_rank(band_rank(band)), Some(band));
        }
        assert_eq!(band_of_rank(5), None, "the ladder ends at rank 4");
    }

    /// Every band spells differently. A collision would silently merge two
    /// depths' chambers into one derived stream.
    #[test]
    fn every_band_has_a_distinct_spelling() {
        let names: Vec<&str> = (0..=4).filter_map(band_of_rank).map(band_name).collect();
        assert_eq!(names.len(), 5, "every rank 0..=4 must name a band");
        for (i, a) in names.iter().enumerate() {
            for b in &names[i + 1..] {
                assert_ne!(a, b, "two bands share the spelling {a:?}");
            }
        }
    }
}
