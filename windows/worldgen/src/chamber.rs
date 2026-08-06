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
fn chamber_key(addr: ChamberAddr) -> String {
    let band_name = format!(
        "{:?}",
        hornvale_climate::Realm::UNDERDARK.strata()[addr.band as usize]
    );
    format!(
        "{}/{}/{band_name}/{}",
        addr.cell.0, addr.entrance, addr.slot
    )
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
