//! The chamber address lattice (The Deep Realm, Task 2).
//!
//! A chamber is a **node addressed in a fixed lattice, sparsely occupied**
//! (spec §3): existence and content are pure functions of its address, and
//! nothing about a chamber is ever stored. The lattice exists before
//! anything is generated into it, so an address can never encode a
//! construction step — see [`ChamberAddr`]'s own docs for why that matters
//! and what has gone wrong elsewhere in this codebase when it wasn't true.
//!
//! [`chamber_exists`] and [`chamber_at`] read exactly one field off the
//! [`hornvale_terrain::Cave`] they are given — `depth_reach_m`, the depth
//! **budget in metres** a real cave system measured (spec §4.0). `Cave::kind`
//! is not read here: Task 6 is where a cave's formation process might shape
//! subterranean conditions, and inventing that coupling now would be scope
//! this task does not own.
//!
//! **The lattice's depth axis is the DELVE ladder, not the stratigraphic one**
//! (spec §4.1, The Underworld). `ChamberAddr.band` indexes
//! [`hornvale_terrain::DelveRung`]'s habitation rungs — ΔT classes above the
//! cell's surface datum — so the same rung sits at different metre depths in
//! different cells, and how far down the lattice a cave reaches is a fact
//! about its budget *and* its cell's geothermal gradient. That is why both
//! entry points now take a [`GeothermalGradient`]: the lattice cannot place an
//! address in a cell it knows nothing about.
//!
//! The stratigraphic ladder is still reported — [`Chamber`] carries both a
//! `rung` and a `stratum` — but the two are computed from different inputs and
//! **neither derives the other** (spec §4.1). The rung comes from the address;
//! the stratum comes from the cell's own column, read at the depth the rung
//! begins at.

use std::collections::BTreeMap;

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, Seed, Stream};
use hornvale_terrain::{
    BandKind, Cave, DelveRung, GeothermalGradient, StratigraphicColumn, delta_t_range_of,
    rung_at_depth,
};

/// Branch columns in the fixed lattice, beneath one cave-system address
/// (`cell` plus `entrance`). Constant regardless of what any particular cave
/// realizes — rule 1a: this is the lattice's own size, never a count of what
/// a generator produced.
///
/// **Renamed from `SLOTS_PER_BAND` by The Stope**, with the field it bounds
/// (spec §3.1: "`slot` reads as a position and it is an identity"). The value
/// and the axis are unchanged; only the word is. Note that the constant's old
/// name said "per band" and the axis never was per-band: [`passages_from`]
/// has always joined the same position in adjacent bands, so a branch is a
/// **column persisting downward**, one range shared by every band.
///
/// **4**, chosen from The Deep Realm's Task 0 measured substrate rather than
/// invented: a cave reaching `BandKind::Roots` (the deepest band the live
/// generator ever produces — 30 seeds, 55,947 caves) spans 4 rungs of
/// `Realm::UNDERDARK.strata()` (`Regolith..=Roots`), so 4 branches keeps a
/// full system's address space the same order of magnitude as the band ladder
/// itself — enough for `branch` to be a genuinely separate axis from `band`,
/// not so large that a per-chamber neighbour walk or a per-cave enumeration
/// becomes expensive. This is a lattice-size judgement call, not a measured
/// quantity, and it can only widen, never relocate an existing address,
/// because `branch` numbers positions in the lattice, not generated chambers.
/// type-audit: bare-ok(count)
pub const BRANCHES_PER_SYSTEM: u8 = 4;

/// The lattice's **floor ceiling**: how many floors one *run* — the floors of
/// one `branch` within one `band` — can hold. Like [`BRANCHES_PER_SYSTEM`],
/// this is the lattice's own size, and rule 1a is the whole reason it is a
/// constant here rather than a drawn quantity.
///
/// **This is deliberately NOT the realized floor count, and the distinction is
/// decision 0102's** (The Stope, spec §3.1). A run's realized floor count is
/// *drawn*, per band, within the ranges spec §3.1 froze (`Undercroft` 1–5,
/// `Shallows` 3–10, `Deeps` 5–20, `Underdeep` 5–10, `Nadir` 1–5); letting that
/// drawn count size the address space would let a generation quantity define
/// the lattice, which is exactly the defect 0102 exists to prevent. So the
/// lattice admits `0..FLOORS_PER_RUN_CEILING` at every run, and the draw
/// decides which of those addresses a world actually realizes. That draw is
/// [`floors_in_run`], and it landed in Task 2 — this constant has bounded a
/// *distribution* rather than stood in for one since.
///
/// **20**, the maximum of §3.1's own ranges (`Deeps` 5–20). Widening it later
/// is safe — a wider ceiling only admits addresses the lattice previously
/// refused, and relocates none, because [`chamber_key`] spells `floor` as a
/// place and not as a fraction of a count.
/// type-audit: bare-ok(count)
pub const FLOORS_PER_RUN_CEILING: u8 = 20;

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
/// step (spec §3.1). Five small integers name: which cell, which entrance of
/// that cell, which branch (a column persisting downward), which depth band,
/// and which floor of that branch's run within that band.
///
/// **The Stope's epoch (`chamber/v3`) reshaped this type**, and both halves
/// are save-format changes because [`chamber_key`] spells the whole address:
/// `slot` was renamed to `branch` (spec §3.1 — "slot reads as a position and
/// it is an identity"; a branch owns a character and a run of floors), and
/// `floor` was added, because the underworld had no floors at all — a band
/// was one interior-less point per branch.
///
/// **`band` indexes the delve ladder's habitation rungs
/// ([`hornvale_terrain::rungs`] minus `Surface`), never a count of the rungs a
/// particular cave realizes** (rule 1a). The permanent ladder has 5 habitation
/// rungs regardless of what any world's caves reach, so `band` stays
/// meaningful whatever the depth model does.
///
/// **It used to index `hornvale_climate::Realm::UNDERDARK.strata()`**, and
/// that is what `chamber/v1` was keyed on; rule 1a was written when Task 0
/// measured that the live generator only ever produced 3 of that ladder's 5
/// values. `MAP-cave-depth-weld` — the fix rule 1a named as the reason to keep
/// the address space wider than the realized one — landed in this campaign
/// (spec §4.0) and made the situation worse, not better: with a metre budget
/// capped at 3 km, `deepest_band` reached exactly `Basement` on 97.3–99.0% of
/// cave-bearing cells across seeds 42 / 7 / 1234, so the lattice's depth axis
/// carried almost no information at all. Re-pointing it at the delve ladder is
/// what restores it: the same three seeds spread across all five rungs
/// (`[77, 131, 399, 53, 214]`, `[84, 599, 121, 150, 727]`,
/// `[91, 144, 366, 129, 536]`). Keeping the address space at 5 rungs is why
/// no address had to move for a reason other than the epoch itself.
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
/// type-audit: bare-ok(index: entrance), bare-ok(index: branch), bare-ok(index: band), bare-ok(index: floor)
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
    /// Which branch of that cave system this chamber sits on
    /// (`0..BRANCHES_PER_SYSTEM`) — a column persisting downward through the
    /// bands, and the thing a character and a barrier state attach to (spec
    /// §3.3, §B.5). A lattice coordinate, not a count of chambers generated.
    pub branch: u8,
    /// Which rung of the delve ladder this chamber sits at. Indexes the
    /// permanent 5-rung habitation ladder — see this type's own docs and
    /// rule 1a. The field keeps the name `band` because it names a position
    /// on *the* depth axis of the lattice, whichever ladder that axis is; the
    /// ladder it indexes is stated here and nowhere else.
    pub band: u8,
    /// Which floor of this branch's run within this band
    /// (`0..FLOORS_PER_RUN_CEILING`) — the rung the lattice was missing. A
    /// lattice coordinate: the ceiling is fixed, and how many of those floors
    /// a given run *realizes* is a separate drawn quantity ([`floors_in_run`])
    /// that may never size this axis (decision 0102; see
    /// [`FLOORS_PER_RUN_CEILING`]).
    pub floor: u8,
}

impl ChamberAddr {
    /// The **run** this chamber belongs to — its address with `floor`
    /// dropped. The one place that projection is spelled, so a caller can
    /// never assemble a [`RunAddr`] that disagrees with the chamber it came
    /// from.
    pub fn run(self) -> RunAddr {
        RunAddr {
            cell: self.cell,
            entrance: self.entrance,
            branch: self.branch,
            band: self.band,
        }
    }
}

/// The address of a **run** — the floors of one `branch` within one `band`
/// (spec §3.3: "a RUN = the floors of one branch within one band, and one
/// engine owns it"). A [`ChamberAddr`] with `floor` removed.
///
/// **This type exists so that a run cannot carry a floor**, which is the
/// whole content of the distinction: a run is exactly the thing that has no
/// floor yet, because how many floors it has is what [`floors_in_run`] draws.
/// Taking a `ChamberAddr` and ignoring its `floor` would compile, and would
/// let a caller believe the answer depended on which floor they happened to
/// pass.
///
/// Every component is a coordinate in the fixed lattice — cell, entrance,
/// branch, band — and none of them is a generation ordinal (decision 0102).
/// The draw keyed on this is therefore a fact about a *place*, readable by
/// anyone who can name the place, in any order, with nothing generated first.
///
/// Deliberately carries **no `Serialize`/`Deserialize`**, for the same reason
/// [`ChamberAddr`] does not: nothing commits one, and the moment one is
/// committed its on-disk spelling becomes a permanent key.
/// type-audit: bare-ok(index: entrance), bare-ok(index: branch), bare-ok(index: band)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RunAddr {
    /// Which surface cell this run lies beneath.
    pub cell: CellId,
    /// Which entrance of that cell's cave system this run's descent starts
    /// from. See [`ChamberAddr::entrance`].
    pub entrance: u8,
    /// Which branch of that cave system this run sits on
    /// (`0..BRANCHES_PER_SYSTEM`).
    pub branch: u8,
    /// Which rung of the delve ladder this run sits at — the same 5-rung
    /// habitation ladder [`ChamberAddr::band`] indexes.
    pub band: u8,
}

/// A chamber's maker, or the lack of one — spec §3.3's opening sentence: "A
/// chamber is either **found** or **made** … what separates them is a maker
/// and a purpose, not a different generator." One taxonomy covers cave-mouth
/// shelters, Petra, sewers, catacombs, escape tunnels, dwarven halls, drow
/// cities, a dug shelter, and a hole cut by magic — this campaign ships only
/// the field and the seam that reads it, never a writer.
///
/// **`stratum` stays un-overridable and this type never touches it.** The
/// override records an *event's effect* (something happened to this place);
/// `stratum` records the *substrate* (what the place *is*). A dig does not
/// move you into different rock — see [`Chamber::stratum`]'s own docs.
///
/// **`Made` is absorbing** — see [`resolve_origin`], which states and tests
/// the rule directly: applying an override can take `Found → Made`; nothing
/// takes `Made → Found`. Tool marks do not un-cut themselves.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChamberOrigin {
    /// The address-derived default. This campaign digs nothing, so every
    /// chamber `chamber_at` produces without a matching override resolves to
    /// this.
    Found,
    /// Recorded by an override: a maker cut this chamber for a purpose.
    ///
    /// **It now carries a consequence, not just a label** (spec §4.2.1, clause
    /// 2): a made chamber is dry regardless of the water table — see
    /// [`is_sump`]. Keeping a working depth dry is what mining is, and this is
    /// what lets a people inhabit a depth the hydrology would otherwise flood.
    ///
    /// **The writer landed with spec §4.6's capacity task**, which bound it as
    /// an acceptance criterion rather than deferring it again:
    /// [`crate::delve_seating::made_chambers`] resolves a settled subterranean
    /// community's own chambers to `Made`. What it does **not** have is a call
    /// site — nothing in the shipped generation path builds the
    /// [`ChamberOverrides`] it writes into, so no world a player can reach
    /// carries this variant. See [`is_sump`] and `made_chambers` for the full
    /// disclosure; this sentence used to read "until that lands" and the
    /// landing did not change what a player sees.
    Made,
}

/// A chamber's resolved content — deliberately minimal for this task. Spec
/// §3's substrate table lists a chamber's content as "depth — `BandKind` on
/// each node — what the rock here is like", and that is exactly what
/// `stratum` carries: the named stratum its address's `band` indexes. Task 4
/// (ledger #24) adds `origin`, the seam's own payload; later tasks extend
/// this further (Task 5's descent narration, Task 6's derived subterranean
/// conditions); a holding's dug-out dressing (spec §4) is explicitly out of
/// this campaign's scope, so this task does not invent fields for it.
///
/// Content is a pure function of `(addr, gradient, column, overrides)` alone —
/// never of the `Cave` that gated its existence. `an_addresss_meaning_does_not
/// _depend_on_which_other_chambers_exist` in `deep_realm_chamber.rs` is the
/// regression guard: with the same override source, a `Chamber` for one
/// address must come out identical no matter which cave (shallow or deep) was
/// asked, for every address both caves admit. The gradient and column are
/// properties of the *place*, not of the cave, so admitting them does not
/// weaken that: they are constant for a given `addr.cell`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Chamber {
    /// The address this content was derived for.
    pub addr: ChamberAddr,
    /// Which rung of the delve ladder this chamber sits on — a habitation
    /// depth class, `rung_of_rank(addr.band)`. A pure function of the address.
    pub rung: DelveRung,
    /// The rock stratum this chamber sits in — the *archive* answer, read off
    /// the cell's own stratigraphic column at the depth [`Chamber::rung`]
    /// begins at. **Not derived from `rung`, and `rung` is not derived from
    /// it** (spec §4.1): the same rung is a different band in a cell with a
    /// different gradient or a different column.
    ///
    /// **Never overridable** — see [`ChamberOrigin`]'s own docs for why: this
    /// is the substrate, not an event's effect, and no seam in this campaign
    /// (or any later one, per that doc) should add a way to override it.
    pub stratum: hornvale_climate::Stratum,
    /// Whether this chamber was found or made — the override seam's payload
    /// (spec §3.3, Task 4, ledger #24). See [`ChamberOrigin`]'s own docs.
    pub origin: ChamberOrigin,
}

/// The one explicit mapping between a NAMED [`DelveRung`] (what
/// [`rung_at_depth`] returns for a cave's budget) and the lattice's index
/// space (what [`ChamberAddr::band`] indexes) — rule 1a's "compare them
/// through one explicit mapping, in one place." [`chamber_exists`] is the
/// only caller, so a lattice reader and the depth-budget gate can never
/// diverge on what a `band` number means. Exhaustive: a sixth `DelveRung`
/// variant fails this to compile rather than silently misplacing it.
///
/// `Surface` is `None`: it is a rung of the delve ladder but not a *habitation*
/// rung, so it has no position in a lattice of underground places. Returning
/// `None` rather than a numeral is what stops the overworld from silently
/// becoming address 0. [`rung_at_depth`] never returns it, so the `None` arm is
/// reachable only through a caller that hands this function `Surface` directly.
///
/// **Made `pub` by The Underworld's Task 8**, which needs it for the same
/// reason [`chamber_exists`] does and must not grow a second copy: the
/// `ChamberOrigin::Made` writer turns a settled community's `(cell, rung)`
/// seat into the [`ChamberAddr`]es beneath it, and that translation is exactly
/// this mapping. Rule 1a's "one explicit mapping, in one place" is what makes
/// widening it the right move rather than duplicating it in
/// [`crate::delve_seating`].
/// type-audit: bare-ok(index: return)
pub fn rung_rank(rung: DelveRung) -> Option<u8> {
    match rung {
        DelveRung::Surface => None,
        DelveRung::Undercroft => Some(0),
        DelveRung::Shallows => Some(1),
        DelveRung::Deeps => Some(2),
        DelveRung::Underdeep => Some(3),
        DelveRung::Nadir => Some(4),
    }
}

/// The inverse of [`rung_rank`]: the rung a [`ChamberAddr::band`] index names.
/// `None` for a rank past the ladder's end, so no caller can index out of it.
///
/// Kept beside `rung_rank` on purpose — the two are one bijection over the
/// habitation rungs, and a sixth `DelveRung` variant fails *both* to compile
/// rather than leaving the pair half-updated.
fn rung_of_rank(rank: u8) -> Option<DelveRung> {
    match rank {
        0 => Some(DelveRung::Undercroft),
        1 => Some(DelveRung::Shallows),
        2 => Some(DelveRung::Deeps),
        3 => Some(DelveRung::Underdeep),
        4 => Some(DelveRung::Nadir),
        _ => None,
    }
}

/// A rung's spelling inside the chamber key — **a save-format contract**, and
/// the reason this is an explicit match rather than `format!("{rung:?}")`.
///
/// A derived `Debug` impl renders the variant's identifier, which *looks* like
/// exactly this table and is not the same promise. `Debug` is a diagnostic
/// facility: nothing stops a later reader from writing a hand-rolled `Debug`
/// for [`DelveRung`] to make some log prettier, and doing so would silently
/// re-key every chamber in every world with no test able to see it. Stating
/// the strings here makes the contract reviewable, makes a rename an obvious
/// epoch decision, and forces a sixth variant to choose its own spelling
/// instead of inheriting one.
///
/// **This campaign is exactly the change that discipline was written to
/// survive**, and it did: re-pointing the axis at a different ladder changed
/// *which table* is consulted, in one place, visibly — instead of silently
/// changing what a `Debug` impl happened to print.
fn rung_name(rung: DelveRung) -> &'static str {
    match rung {
        DelveRung::Surface => "surface",
        DelveRung::Undercroft => "undercroft",
        DelveRung::Shallows => "shallows",
        DelveRung::Deeps => "deeps",
        DelveRung::Underdeep => "underdeep",
        DelveRung::Nadir => "nadir",
    }
}

/// The stratigraphic band a chamber at `rung` sits in, for a cell with this
/// column and gradient — the *archive* answer, kept strictly separate from the
/// rung itself (spec §4.1: "neither derives the other").
///
/// The rung fixes a ΔT; the gradient turns that into a depth in metres for
/// **this** cell; the column says what rock is at that depth. Two chambers on
/// the same rung under different cells can therefore sit in different bands,
/// and two chambers in the same band can sit on different rungs — which is the
/// whole reason both are reported.
///
/// The depth used is the rung's own **top** (`delta_t_range_of(rung).0`
/// divided by the gradient), not a point inside it: a rung spans a range of
/// depths and a chamber is placed *at* a rung rather than at a metre, so the
/// shallowest rock the rung touches is the one non-arbitrary choice.
///
/// **That choice makes the top rung degenerate, and Task 5 should know it
/// before picking differently.** `Undercroft` begins at ΔT = 0, so this reads
/// the column at 0 m — and `band_at_depth` answers 0 m with the topmost band
/// in *every* column under *every* gradient. So a rank-0 chamber's `stratum`
/// is a constant, and the "neither ladder derives the other" independence
/// [`Chamber::stratum`] claims is **vacuous for that one rung**: it holds for
/// ranks 1–4, where the same rung genuinely straddles different rock in
/// different cells, and says nothing at rank 0. That is a property of taking
/// the top rather than a defect in the ladder — the midpoint of a rung, or its
/// bottom, would give rank 0 a cell-varying stratum at the cost of naming a
/// depth no chamber is actually at. Whichever a later task picks, it should
/// pick knowing this is the trade, not discover it from a constant column.
fn stratum_at(
    rung: DelveRung,
    gradient: GeothermalGradient,
    column: &StratigraphicColumn,
) -> BandKind {
    let (delta_t_k, _) = delta_t_range_of(rung);
    let depth_m = delta_t_k / gradient.get() * 1000.0;
    hornvale_terrain::features::band_at_depth(column, depth_m)
}

/// The one explicit mapping from terrain's [`BandKind`] to climate's
/// [`hornvale_climate::Stratum`] — the two enums name the same five rock units
/// from two domains that may not depend on each other, so the composition root
/// is the only place allowed to state the correspondence. Exhaustive on both
/// sides: a sixth variant on either fails this to compile.
fn stratum_of_band(band: BandKind) -> hornvale_climate::Stratum {
    match band {
        BandKind::Regolith => hornvale_climate::Stratum::Regolith,
        BandKind::Cover => hornvale_climate::Stratum::Cover,
        BandKind::Basement => hornvale_climate::Stratum::Basement,
        BandKind::Roots => hornvale_climate::Stratum::Roots,
        BandKind::Underneath => hornvale_climate::Stratum::Underneath,
    }
}

/// The one place the `chamber/v3` stream key is spelled — mirrors
/// `deity_base_seed`'s discipline (`windows/worldgen/src/lib.rs`): "the one
/// place the stream label is spelled, so [every caller] can never diverge."
/// [`chamber_stream`] is the only caller.
///
/// **The key spells the WHOLE address, so every field here is a save-format
/// contract.** The Stope added `floor` and renamed `slot` to `branch`; a key
/// that omitted `floor` would derive one stream for every floor of a run,
/// which is to say the floors would all be the same chamber. That is why
/// `the_key_spells_the_floor` sweeps the axis rather than sampling it.
///
/// **`cell`, `entrance`, `branch` and `floor` are genuine integers naming a
/// place and are spelled decimal. `band` is spelled by its [`DelveRung`] NAME,
/// never its numeric index.** An index is a declaration position: if the delve ladder
/// ever gains a rung in the middle (spec §4.1 permits 4 to 6, so there is
/// room), every index below it shifts, and a numeral-keyed chamber would
/// silently move to a different derived stream. Spelling the name instead
/// means the key only changes if the *name itself* changes, which is the same
/// discipline a `stream_labels!` rename already carries (an epoch suffix,
/// never silent). This is rule 1a one level down, applied to the derivation
/// instead of the address type.
///
/// The name comes from [`rung_name`]'s explicit table, **not** from a `Debug`
/// impl — see that function for why the distinction is load-bearing rather
/// than stylistic. A `band` past the ladder's end spells as `"out-of-ladder"`;
/// it is unreachable through either public entry point (both gate on
/// [`rung_rank`] first), and naming it beats both a panic and a silent
/// collision with band 0.
pub(crate) fn chamber_key(addr: ChamberAddr) -> String {
    let band = rung_of_rank(addr.band).map_or("out-of-ladder", rung_name);
    format!(
        "{}/{}/{}/{band}/{}",
        addr.cell.0, addr.entrance, addr.branch, addr.floor
    )
}

/// The stream a chamber's own derivations draw from — [`chamber_key`]
/// composed under [`crate::streams::CHAMBER`], following the composed-label
/// pattern at `windows/worldgen/src/lib.rs`'s `deity_name_seed`.
///
/// **Precondition:** `addr.band` must be `< 5` (a valid habitation-rung
/// index). Both callers ([`chamber_exists`] and [`chamber_at`]) only reach
/// this after `addr.band` has already been checked against a cave's budget via
/// [`rung_rank`], whose maximum return value is `4`, so an out-of-range `band`
/// can never survive to here.
fn chamber_stream(seed: Seed, addr: ChamberAddr) -> Stream {
    seed.derive(crate::streams::CHAMBER)
        .derive(StreamLabel::dynamic(&chamber_key(addr)))
        .stream()
}

/// Spec §3.1's floors-per-run range for a band, inclusive on both ends —
/// **frozen preregistration, not a tunable**. `Undercroft` 1–5, `Shallows`
/// 3–10, `Deeps` 5–20, `Underdeep` 5–10, `Nadir` 1–5 (the last renamed from
/// `Sunless` by amendment B.3).
///
/// `None` for `Surface`: the overworld is not a run. Exhaustive, so a sixth
/// [`DelveRung`] fails this to compile rather than silently inheriting a
/// neighbour's range.
fn floors_range(rung: DelveRung) -> Option<(u8, u8)> {
    match rung {
        DelveRung::Surface => None,
        DelveRung::Undercroft => Some((1, 5)),
        DelveRung::Shallows => Some((3, 10)),
        DelveRung::Deeps => Some((5, 20)),
        DelveRung::Underdeep => Some((5, 10)),
        DelveRung::Nadir => Some((1, 5)),
    }
}

/// The one place the `chamber/run-floors/v1` key is spelled — [`chamber_key`]'s
/// discipline one axis over, and its own save-format contract.
///
/// Spelled to the same rules as a chamber key and for the same reasons:
/// `cell`, `entrance` and `branch` are integers naming a place and are decimal;
/// `band` is spelled by its [`DelveRung`] **name** through [`rung_name`]'s
/// explicit table, because an index is a declaration position and a mid-ladder
/// insertion would silently re-key every run below it.
///
/// **It does not spell a floor, and that is the type's whole content**: a run
/// is the thing that does not have one yet.
fn run_key(run: RunAddr) -> String {
    let band = rung_of_rank(run.band).map_or("out-of-ladder", rung_name);
    format!("{}/{}/{}/{band}", run.cell.0, run.entrance, run.branch)
}

/// The stream a run's floor count is drawn from — [`run_key`] composed under
/// [`crate::streams::RUN_FLOORS`], deliberately a **different parent** from
/// [`chamber_stream`]'s. See `RUN_FLOORS`'s own doc for why the separation
/// lives in the parent rather than in the key's shape.
fn run_stream(seed: Seed, run: RunAddr) -> Stream {
    seed.derive(crate::streams::RUN_FLOORS)
        .derive(StreamLabel::dynamic(&run_key(run)))
        .stream()
}

/// **How many floors this run realizes** — the drawn quantity
/// [`FLOORS_PER_RUN_CEILING`] is deliberately not (The Stope, Task 2, spec
/// §3.1). Uniform on the band's own frozen range: `Undercroft` 1–5,
/// `Shallows` 3–10, `Deeps` 5–20, `Underdeep` 5–10, `Nadir` 1–5.
///
/// **It takes a [`RunAddr`], not a [`ChamberAddr`] and not a bare band.** The
/// draw keys on cell, entrance, branch and band, and a function cannot key on
/// what it is not given — so every component of the key is a parameter, and
/// the parameter type is the one that *cannot* carry a floor, because a run
/// is precisely the thing that has no floor yet.
///
/// **It does NOT take a `Cave`, and that is the same rule [`Chamber`] states
/// for content**: how long a run is is a fact about a *place* in the lattice,
/// not about the cave that gated its existence. Whether the run is reachable
/// at all — whether its band is inside a cave's depth budget — is
/// [`chamber_exists`]'s question, asked with the cave, and kept separate here.
/// Amendment B.4 is the same distinction one level up: "the ladder says how
/// far the rock lets you go; the branch says what is in the way. Neither is a
/// source of truth for the other."
///
/// **A place, never an ordinal** (decision 0102). Nothing here counts runs
/// generated, and the answer for one run is independent of whether any other
/// run has ever been asked about — see
/// `a_runs_floor_count_is_deterministic_for_one_address`, which interleaves
/// unrelated queries specifically to catch a draw that advanced a shared
/// stream.
///
/// `0` for a band past the habitation ladder, which makes
/// [`chamber_exists`]'s floor gate refuse every floor there rather than
/// deriving a length for nowhere. Unreachable through `chamber_exists`, which
/// gates on [`rung_rank`] first; stated so the function is total.
/// type-audit: bare-ok(count: return)
pub fn floors_in_run(seed: Seed, run: RunAddr) -> u8 {
    let Some(rung) = rung_of_rank(run.band) else {
        return 0;
    };
    let Some((lo, hi)) = floors_range(rung) else {
        return 0;
    };
    let drawn = run_stream(seed, run).range_u32(u32::from(lo), u32::from(hi));
    // `range_u32` is inclusive and `hi` came from a `u8`, so this cannot
    // truncate; `expect` states that rather than masking it with a cast.
    u8::try_from(drawn).expect("range_u32(lo, hi) never exceeds hi, which is a u8")
}

/// Whether a chamber exists at `addr`, under `cave`'s measured depth
/// budget in this cell. Sparse and derived: no chamber is ever stored, so
/// "exists" is a per-address predicate — a fixed-density draw, gated so
/// `addr.band` reaches no deeper on the delve ladder than the cave's budget
/// does (spec §4.0's metre budget, classified by spec §4.1's ladder).
///
/// **`gradient` is the cell's own geothermal gradient**, and it is what makes
/// this a question about a *place* rather than about a length. A 480 m budget
/// is the Deeps under a 24 K/km cell and the Shallows under a 15 K/km one, so
/// the same cave reaches a different distance down the lattice depending on
/// where it is. Callers get it from
/// `hornvale_terrain::GeneratedTerrain::geothermal_gradient_at`.
///
/// An out-of-lattice `branch` (`>= BRANCHES_PER_SYSTEM`) or `floor`
/// (`>= FLOORS_PER_RUN_CEILING`) never exists — the lattice is fixed-size, and
/// an address outside it names nowhere. Likewise a `band` past the ladder's
/// end: [`rung_rank`] tops out at `4`.
///
/// **The branch axis carries the same pair** (The Stope, Task 3, spec
/// amendment C.1): [`BRANCHES_PER_SYSTEM`] sizes the address space — an
/// out-of-lattice branch is refused above — and
/// [`crate::character::branch_count_of`] draws how many of those columns
/// THIS system realizes. A branch at or past its system's drawn count is
/// refused here rather than merely unqueried, so the count is enforced by
/// the shipped existence path and not just reported by the accessor: the
/// identical lattice-ceiling/drawn-realization split the floor gates below
/// implement for runs.
///
/// **There are TWO floor gates now, and the pair is the point** (The Stope,
/// Task 2). [`FLOORS_PER_RUN_CEILING`] says which floors the address space
/// admits *at all* — refusing past it is what stops an address outside the
/// lattice from silently deriving a chamber. [`floors_in_run`] says which of
/// those admitted floors *this* run realizes, and refusing past that is what
/// makes a run a distribution rather than a constant. Neither replaces the
/// other: the first is the lattice's size and the second is a draw, and
/// letting the draw resize the lattice is the defect decision 0102 exists to
/// prevent.
///
/// **Floor 0 is admitted by every run that exists at all**, because every
/// band's frozen range has a minimum of at least 1. That is worth knowing
/// before reading any floor-0 measurement as evidence about this gate: the
/// heavy readouts that slice floor 0 are byte-identical across Task 2 by
/// construction, while the unsliced population fell ~3× (see
/// `underworld_chamber_reach`).
/// type-audit: bare-ok(flag: return)
pub fn chamber_exists(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    addr: ChamberAddr,
) -> bool {
    if addr.branch >= BRANCHES_PER_SYSTEM {
        return false;
    }
    if addr.floor >= FLOORS_PER_RUN_CEILING {
        return false;
    }
    // C.1's drawn realization: past this system's drawn branch width, no
    // chamber exists at any band or floor of the column. Branch 0 is always
    // inside the count (`branch_count_of` draws 1..=BRANCHES_PER_SYSTEM), so
    // every entrance's mouth survives this gate.
    if addr.branch >= crate::character::branch_count_of(seed, addr.cell, addr.entrance) {
        return false;
    }
    // `rung_at_depth` never returns `Surface`, so this is always `Some`; the
    // `else` arm refuses rather than unwrapping, because a future ladder that
    // could return `Surface` here must mean "no underground address at all",
    // not a panic.
    let Some(deepest) = rung_rank(rung_at_depth(cave.depth_reach_m, gradient)) else {
        return false;
    };
    if addr.band > deepest {
        return false;
    }
    if addr.floor >= floors_in_run(seed, addr.run()) {
        return false;
    }
    chamber_stream(seed, addr).next_f64() < EXISTENCE_DENSITY
}

/// The override source for a chamber's `origin` — spec §3.3's seam: "a
/// chamber's content = its own latest override fact, else its
/// address-derived default." Mirrors `hornvale_species::instance_biosphere`'s
/// pattern one level over (an instance's effective trait is its own latest
/// override fact, else its kind's authored default) — but as an ordinary
/// parameter, **not** `&Ledger`/`&World` (constraint 1, owner's ruling
/// 2026-08-05): this campaign defers how a chamber address is written down
/// to a ledger, and consulting a real ledger here would fix that form
/// permanently. A plain `BTreeMap` keeps the seam obvious — an address maps
/// to at most one recorded origin, which is exactly the shape a future dig
/// fact would need to look up, without inventing a trait this campaign has
/// no second implementor for. `BTreeMap`, never `HashMap` (workspace rule):
/// iteration order is never observed here, but the type is banned outright.
pub type ChamberOverrides = BTreeMap<ChamberAddr, ChamberOrigin>;

/// Applies an override onto a derived default, enforcing [`ChamberOrigin`]'s
/// absorbing rule (spec §3.3): once a chamber is `Made`, no override —
/// including an explicit `Some(ChamberOrigin::Found)`, and including the
/// absence of any override at all — can resolve it back to `Found`. Kept as
/// a standalone, `pub` function (rather than inlined into [`chamber_at`]) so
/// the absorbing property can be asserted directly against every
/// `(default, override)` combination, independent of the fact that
/// `chamber_at`'s own derived default is always `Found` today — see
/// `an_override_wins_over_the_derived_default` in `deep_realm_chamber.rs`.
/// **This function's `default == Made` branch has NO LIVE CALLER, deliberately,
/// and that is recorded here rather than left to be discovered.** [`chamber_at`]
/// is the only caller and always passes `Found`, because this campaign ships no
/// writer — so the absorbing rule is exercised by
/// `made_is_absorbing_over_every_default_and_override_combination` and by
/// nothing else in the shipped path.
///
/// The Hollow's dominant lesson is that a field nothing reads cannot be
/// observed to be wrong, and its remedy is to name a derived thing's first
/// consumer in the same campaign *or say plainly that it has none*. This has
/// none. **C2c (The Delvers) was named here as that first consumer and is not
/// one: it shipped a roster of three SURFACE dwarves, dug nothing, and cut both
/// subterranean kinds before merge** — so this branch is one campaign older than
/// the comment used to claim, and the deferral is longer, not shorter. Its first
/// real consumer is still whichever campaign makes a dig fact — the moment one
/// can make a chamber `Made`, a second dig over the same address must not be
/// able to un-make it, and the persistence asymmetry (an excavated extent
/// survives its maker; only the *claim* lapses) is what that campaign will be
/// reading. Until then this is a stated deferral, not an oversight.
pub fn resolve_origin(default: ChamberOrigin, over: Option<ChamberOrigin>) -> ChamberOrigin {
    if default == ChamberOrigin::Made {
        return ChamberOrigin::Made;
    }
    over.unwrap_or(ChamberOrigin::Found)
}

/// Whether a chamber at `depth_m` below the surface is a **sump** — flooded,
/// and therefore something the passage graph renders as a missing edge rather
/// than as a different kind of place (spec §4.2).
///
/// **A made chamber is never a sump, and that is the whole rule** (spec §4.2.1,
/// clause 2). A chamber cut for a purpose is kept dry regardless of where the
/// water table sits, because keeping a working depth dry is what mining *is* —
/// adits, sumps in the mining sense, wheels, pumps, and the drainage levels
/// that are among the oldest large engineering works there are. This makes a
/// dwarven hall something a people **does** rather than a place it happens to
/// find, which is the difference between a species with a habitat and a species
/// with a craft.
///
/// A found chamber gets the plain hydrology, [`hornvale_terrain::is_phreatic`].
///
/// **Why the rule lives here and not in `domains/terrain`.** The layering is
/// constitutional: terrain owns the water table and knows nothing of chambers,
/// and [`ChamberOrigin`] is a worldgen concept. So terrain answers "is this
/// depth below the table" and this function answers "does that flood *this*
/// chamber" — the hydrology is a fact about the rock, the exemption is a fact
/// about the maker.
///
/// **This ships the rule; spec §4.6's capacity task shipped its producer**,
/// as an acceptance criterion rather than a note.
///
/// **The disclosure, UPDATED, because half of it has closed and half has
/// not.** Two things were absent when this function landed. (a) Nothing in the
/// shipped path emitted `Made`, so the `Made` arm was unreachable. (b) Nothing
/// in the shipped path called this function at all.
///
/// **(b) is CLOSED.** [`crate::delve_seating::seat_at`] calls it on every
/// candidate rung of every cave-bearing column, with `ChamberOrigin::Found`,
/// to decide whether a seat is priced at [`crate::delve_seating::UNDERWORLD_WORKS_COST`]
/// — the first and only production caller. The tests and
/// `underworld_water_table_probe` are no longer the whole of the roster, and
/// this doc claimed they were for one campaign after they stopped being.
///
/// **(a) is HALF closed, and the half that remains is worse than it was.**
/// [`crate::delve_seating::made_chambers`] *writes* `Made`, so the value is
/// produced; but nothing in the shipped path constructs the
/// [`ChamberOverrides`] it writes into — `windows/vessel`'s `delve_at` hands
/// [`chamber_at`] a freshly-built empty map — so in every world a player can
/// reach, every chamber still resolves `Found` and the `Made` arm here is
/// still never taken. That is a **writer with no call site**, which
/// `made_chambers`' own docs state at length and which is the shape this
/// campaign's §3.9 finding exists to name.
///
/// The remaining consumer is the passage graph, which turns a sump into a
/// missing edge ([`passages_from`] has no production caller either). Until
/// then this is a stated deferral, not an oversight.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(diagnostic-value: water_table_m), bare-ok(flag: return)
pub fn is_sump(origin: ChamberOrigin, depth_m: f64, water_table_m: f64) -> bool {
    match origin {
        // Drained by whoever cut it. Deliberately not "drained if shallow
        // enough to drain": a threshold here would be a second, unmeasured
        // calibration, and the interesting version of that question — what a
        // people can afford to keep dry — belongs to capacity, which is the
        // task that gains the writer.
        ChamberOrigin::Made => false,
        ChamberOrigin::Found => hornvale_terrain::is_phreatic(depth_m, water_table_m),
    }
}

/// A chamber's resolved content at `addr`, under `cave`'s measured depth
/// budget — `None` when [`chamber_exists`] is `false`, else the
/// address-derived [`Chamber`], with `origin` resolved through `overrides`
/// (spec §3.3). See [`Chamber`]'s own docs for why content never depends on
/// `cave` beyond the existence gate, and [`ChamberOverrides`]'s docs for why
/// the override source is an ordinary parameter rather than `&Ledger`.
///
/// **Existence is unaffected by `overrides`.** An override changes what an
/// already-existing chamber's `origin` resolves to; it cannot conjure a
/// chamber into existence at an address `chamber_exists` rejects — that
/// would be digging, which this campaign does not ship (no writer exists to
/// produce such an override in the first place).
///
/// With an empty `overrides` map, `origin` resolves to the address-derived
/// default, `Found` — this campaign ships no writer, so that is the only
/// value the shipped path produces.
///
/// **It is NOT byte-identical to the pre-`chamber/v2` derivation, and this
/// sentence used to claim it was.** `stratum` was
/// `Realm::UNDERDARK.strata()[addr.band]` — a pure function of the address,
/// i.e. the address restated in another vocabulary — and that identity was
/// the defect spec §4.1 removed, not a property to preserve. It is now read
/// off the cell's own column at the depth the rung begins ([`stratum_at`]),
/// so it depends on `gradient` and `column` and varies between cells that
/// share an address. Two chambers on the same rung can sit in different rock,
/// which is the entire point of carrying both.
pub fn chamber_at(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    column: &StratigraphicColumn,
    addr: ChamberAddr,
    overrides: &ChamberOverrides,
) -> Option<Chamber> {
    if !chamber_exists(seed, cave, gradient, addr) {
        return None;
    }
    // `chamber_exists` has already refused any `band` past the ladder's end,
    // so the rank is in range here.
    let rung = rung_of_rank(addr.band)?;
    let stratum = stratum_of_band(stratum_at(rung, gradient, column));
    let origin = resolve_origin(ChamberOrigin::Found, overrides.get(&addr).copied());
    Some(Chamber {
        addr,
        rung,
        stratum,
        origin,
    })
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
/// - same `band`, `branch` differing by 1, **or**
/// - same `branch`, `band` differing by 1.
///
/// **`floor` is NOT an adjacency axis here, and that is a deferral rather than
/// a claim about the world** (The Stope, spec §7 task 6: "junctions: derived,
/// never drawn"). Every neighbour this function offers carries the caller's
/// own `addr.floor`, so the lattice currently has one disconnected copy of
/// this graph per floor. That is the honest state of an address space whose
/// vertical connections have not been designed yet, rather than a guess at
/// them baked into the one function whose whole virtue is that it guesses
/// nothing.
///
/// **TASK 2'S FLOOR DRAW TURNED THAT DEFERRAL INTO A SECOND, UNDESIGNED
/// STRUCTURAL GATE ON DEPTH, AND IT IS NOT A COSMETIC CONSEQUENCE.** The
/// `band + 1` candidate carries the caller's own `floor`, and
/// [`chamber_exists`] now requires `floor < floors_in_run` of THAT
/// neighbour's run. The bands' frozen ranges are not nested, so a chamber can
/// sit at a floor its own run has and the run below does not — and then it
/// cannot descend at all:
///
/// ```text
///   Deeps 5-20 sits over Underdeep 5-10
///       a Deeps chamber at floor >= 10 can NEVER descend
///   Underdeep 5-10 sits over Nadir 1-5
///       an Underdeep chamber at floor >= 5 can NEVER reach the Nadir
/// ```
///
/// Amendment B.5 makes the Nadir the endgame and B.7 preregisters the gate on
/// reaching it at **0-10%**, set by the barrier draw. After Task 2 the Nadir
/// is *additionally* reachable only from Underdeep floors 0-4 — a second gate
/// nobody designed, sitting underneath the one Nathan did, and multiplying
/// into B.7's rate. Whichever way spec §7 task 6 resolves vertical
/// connection, it should resolve this deliberately rather than inherit it.
///
/// **Deliberately left unasserted.** A test pinning "a Deeps chamber above
/// floor 10 has no descent" would freeze an accident as a contract, and the
/// task that designs junctions is the one that should choose. Recorded here,
/// and carried into that task's dispatch, rather than mechanised.
///
/// The candidate-generation rule below is unchanged from before `floor`
/// existed — it varies `branch` and `band` and nothing else. **It does not
/// follow that the floor-0 graph is unchanged**, and this doc said otherwise
/// until review caught it: `chamber_exists` draws per address, `chamber_key`
/// spells `floor`, and the `chamber/v3` epoch re-keyed every one of those
/// draws, so which candidates survive `retain` moved at every floor including
/// zero. `every_neighbour_stays_on_the_callers_floor` in
/// `deep_realm_chamber.rs` asserts the half that IS a property of this
/// function.
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
/// **Neither axis wraps.** `branch` does not wrap modularly (branch `0` is
/// adjacent only to branch `1`, not also to `BRANCHES_PER_SYSTEM - 1`),
/// matching `band`, which cannot wrap either — there is no rung before
/// `Regolith` or
/// after `Underneath` for it to wrap into. Keeping both axes non-wrapping
/// means the lattice has one consistent shape rather than one axis behaving
/// like a line and the other like a ring; end branches and end bands simply
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
pub fn passages_from(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    addr: ChamberAddr,
) -> Vec<ChamberAddr> {
    if !chamber_exists(seed, cave, gradient, addr) {
        return Vec::new();
    }

    let mut candidates = Vec::new();

    // Same band, adjacent branch. Guaranteed not to underflow/overflow: addr
    // passed the chamber_exists check above, so
    // addr.branch < BRANCHES_PER_SYSTEM.
    if addr.branch > 0 {
        candidates.push(ChamberAddr {
            branch: addr.branch - 1,
            ..addr
        });
    }
    if addr.branch + 1 < BRANCHES_PER_SYSTEM {
        candidates.push(ChamberAddr {
            branch: addr.branch + 1,
            ..addr
        });
    }

    // Same branch, adjacent band. Guaranteed not to underflow/overflow: addr
    // passed the chamber_exists check above, so addr.band <= the cave's own
    // rung rank <= 4 (rung_rank's maximum return value).
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

    candidates.retain(|&candidate| chamber_exists(seed, cave, gradient, candidate));
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
    /// That is an **epoch** (`chamber/v4` next), not a fix to this assertion.
    ///
    /// **These strings have moved twice.** The Underworld (`chamber/v2`)
    /// re-pointed `addr.band` from the stratigraphic ladder at the delve
    /// ladder, so rank 2 spells `deeps` where it used to spell `basement`.
    /// The Stope (`chamber/v3`) added `floor` and renamed `slot` to `branch`,
    /// which changes both the arity and the field order of the key. The old
    /// values are kept in this comment rather than deleted, because a reader
    /// arriving at a failing assertion needs to be able to tell "the epoch
    /// happened" from "someone broke the key":
    ///
    /// ```text
    ///           before v2            v2                    v3
    ///   first   "9/0/basement/3"     "9/0/deeps/3"         "9/0/3/deeps/0"
    ///   second  "0/1/regolith/0"     "0/1/undercroft/0"    "0/1/0/undercroft/0"
    /// ```
    #[test]
    fn the_chamber_key_spelling_is_pinned() {
        assert_eq!(
            chamber_key(ChamberAddr {
                cell: CellId(9),
                entrance: 0,
                branch: 3,
                band: 2,
                floor: 0,
            }),
            "9/0/3/deeps/0"
        );
        assert_eq!(
            chamber_key(ChamberAddr {
                cell: CellId(0),
                entrance: 1,
                branch: 0,
                band: 0,
                floor: 0,
            }),
            "0/1/0/undercroft/0"
        );
        // A floor other than 0, so the pin covers the axis The Stope added
        // rather than only its zero value.
        assert_eq!(
            chamber_key(ChamberAddr {
                cell: CellId(9),
                entrance: 0,
                branch: 3,
                band: 2,
                floor: 7,
            }),
            "9/0/3/deeps/7"
        );
    }

    /// The band is spelled by NAME, never by index — rule 1a one level down.
    /// A numeral here would mean that inserting a [`DelveRung`] variant
    /// mid-ladder silently moved every chamber below it to a different stream.
    #[test]
    fn the_key_names_its_rung_rather_than_numbering_it() {
        // `branch` and `floor` are deliberately chosen NOT to equal the band
        // index: they are spelled decimal and legitimately so, and a value
        // collision would make the second assertion below fire for the wrong
        // reason.
        let key = chamber_key(ChamberAddr {
            cell: CellId(7),
            entrance: 0,
            branch: 1,
            band: 3,
            floor: 2,
        });
        assert!(
            key.contains("underdeep"),
            "band must be spelled by name; got {key:?}"
        );
        assert!(
            !key.contains("/3/"),
            "band appears as a bare index in {key:?} — an index is a \
             declaration position, not a place"
        );
    }

    /// The key names a DELVE rung, not a stratigraphic band. Stated as its own
    /// assertion because the two ladders have five rungs each and the same
    /// arity would let a half-finished re-point look right: `basement` must not
    /// appear anywhere in a key now, at any rank.
    #[test]
    fn the_key_names_the_delve_ladder_not_the_stratigraphic_one() {
        let stratigraphic = ["regolith", "cover", "basement", "roots", "underneath"];
        for band in 0..=4u8 {
            let key = chamber_key(ChamberAddr {
                cell: CellId(1),
                entrance: 0,
                branch: 0,
                band,
                floor: 0,
            });
            for name in stratigraphic {
                assert!(
                    !key.contains(name),
                    "rank {band} spells the stratigraphic band {name:?} in {key:?} — \
                     `addr.band` indexes the delve ladder since chamber/v2"
                );
            }
        }
    }

    /// The fixture column `deep_realm_chamber.rs` states its rung/stratum
    /// arithmetic against: 401 m of cover on continental basement, band tops
    /// `[0, 1, 401, 17700.5, 35000]` m.
    fn fixture_column() -> StratigraphicColumn {
        hornvale_terrain::column(
            35.0,
            0.3,
            true,
            400.0,
            1.0,
            hornvale_terrain::RockClass::Sandstone,
            hornvale_terrain::Basement::Continental,
        )
    }

    /// **The rung→stratum map is many-to-one, and that fact consumes no
    /// existence draw.** This is the deterministic statement of the property
    /// `deep_realm_chamber.rs`'s `a_chamber_reports_both_its_rung_and_its_
    /// stratum` demonstrates end to end — and the reason it is stated here as
    /// well is that the end-to-end version can only *hunt* for an example
    /// through a random existence draw, so a re-keying can redden it while the
    /// property is untouched. The `chamber/v3` epoch did exactly that.
    ///
    /// [`stratum_at`] is private, so this can only live in this module. It
    /// reads the same table `chamber_at` reads and calls nothing that draws.
    ///
    /// Both directions, the same two the integration test asserts:
    ///
    /// - two DISTINCT rungs share a stratum (so `rung` cannot be recovered
    ///   from `stratum`) — at 24 K/km, `Shallows` (83 m) and `Deeps` (333 m)
    ///   are both in the cover, and `Underdeep` (1042 m) and `Nadir` (2083 m)
    ///   are both in the basement;
    /// - one rung sits in DIFFERENT strata under different gradients (so
    ///   `stratum` cannot be a function of `addr.band`) — `Deeps` is 533 m
    ///   under a 15 K/km cell and 267 m under a 30 K/km one, straddling the
    ///   401 m contact.
    #[test]
    fn the_rung_to_stratum_map_is_many_to_one_and_gradient_dependent() {
        let column = fixture_column();
        let gradient = GeothermalGradient::new(24.0);

        let mapped: Vec<(DelveRung, hornvale_climate::Stratum)> = (0..5u8)
            .filter_map(rung_of_rank)
            .map(|rung| (rung, stratum_of_band(stratum_at(rung, gradient, &column))))
            .collect();
        assert_eq!(mapped.len(), 5, "every rank 0..=4 must map to a stratum");

        let shares_a_stratum = mapped.iter().any(|&(rung_a, stratum_a)| {
            mapped
                .iter()
                .any(|&(rung_b, stratum_b)| stratum_a == stratum_b && rung_a != rung_b)
        });
        assert!(
            shares_a_stratum,
            "no two rungs share a stratum on this column, so `rung` could be a \
             relabelling of `stratum`: {mapped:?}"
        );

        let cool = stratum_of_band(stratum_at(
            DelveRung::Deeps,
            GeothermalGradient::new(15.0),
            &column,
        ));
        let hot = stratum_of_band(stratum_at(
            DelveRung::Deeps,
            GeothermalGradient::new(30.0),
            &column,
        ));
        assert_ne!(
            cool, hot,
            "the Deeps sits at 533 m under 15 K/km and 267 m under 30 K/km, \
             which straddle this column's 401 m contact — a `stratum` read \
             from the cell must differ, and one derived from `addr.band` \
             cannot"
        );
    }

    /// **The key must spell `floor`, or two floors of one run derive the same
    /// stream and are the same chamber.** This is the whole reason The Stope's
    /// address change is an epoch rather than an additive field.
    ///
    /// Swept over the entire floor axis rather than sampled at two values: a
    /// key that spelled `floor` for some values and not others (a key built by
    /// appending `floor` only when non-zero, say) would pass a two-value
    /// sample and still collide.
    #[test]
    fn the_key_spells_the_floor() {
        let base = ChamberAddr {
            cell: CellId(9),
            entrance: 0,
            branch: 3,
            band: 2,
            floor: 0,
        };
        let keys: std::collections::BTreeSet<String> = (0..FLOORS_PER_RUN_CEILING)
            .map(|floor| chamber_key(ChamberAddr { floor, ..base }))
            .collect();
        assert_eq!(
            keys.len(),
            usize::from(FLOORS_PER_RUN_CEILING),
            "two floors of one run share a key, so they are one chamber: {keys:?}"
        );
    }

    /// The key is **injective over the whole lattice** — every address the
    /// lattice admits spells differently from every other. A collision would
    /// silently merge two places into one derived stream, and the epoch that
    /// added two axes is exactly when that becomes possible: `branch` and
    /// `floor` are both spelled decimal, so a missing separator or a swapped
    /// pair of fields would alias.
    #[test]
    fn the_key_is_injective_over_the_lattice() {
        let mut keys = std::collections::BTreeSet::new();
        let mut count = 0usize;
        for cell in 0..3u32 {
            for entrance in 0..2u8 {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    for band in 0..5u8 {
                        for floor in 0..FLOORS_PER_RUN_CEILING {
                            count += 1;
                            keys.insert(chamber_key(ChamberAddr {
                                cell: CellId(cell),
                                entrance,
                                branch,
                                band,
                                floor,
                            }));
                        }
                    }
                }
            }
        }
        assert_eq!(
            keys.len(),
            count,
            "{} of {count} lattice addresses collide on a key",
            count - keys.len()
        );
    }

    /// The epoch label is a **save-format contract**, and reading it back is
    /// what stops a later rename: `stream_labels!` cannot tell a bump from a
    /// typo, so the literal is asserted here.
    ///
    /// `chamber/v1` (the stratigraphic address) and `chamber/v2` (the delve
    /// address without a floor) are **retired and must never be reused** — a
    /// reused label would silently hand a re-shaped address the old label's
    /// derivation.
    #[test]
    fn the_epoch_label_is_v3_and_v2_is_not_reused() {
        assert_eq!(crate::streams::CHAMBER.as_str(), "chamber/v3");
        assert_ne!(
            crate::streams::CHAMBER.as_str(),
            "chamber/v2",
            "chamber/v2 is retired; The Stope re-shaped the address it keyed"
        );
        assert_ne!(crate::streams::CHAMBER.as_str(), "chamber/v1");
    }

    /// **The SHIPPED existence draw travels [`crate::streams::CHAMBER`],
    /// keyed on the WHOLE address.** This starts from [`chamber_exists`] —
    /// the function every world actually calls — and compares it against the
    /// derivation spelled out here, so re-parenting [`chamber_stream`], or
    /// handing it anything other than `chamber_key(addr)`, reddens it.
    ///
    /// **The hole this closes had been open across two epochs.** Task 2's own
    /// [`the_run_draw_travels_the_run_floors_leg_and_not_the_chamber_leg`]
    /// says so in its closing paragraph: `chamber_stream`'s parent was
    /// unpinned, an accidental `derive(RUN_FLOORS)` there would relocate every
    /// chamber in every world, and the only thing that could have objected —
    /// the artifact drift check — was structurally blind to the chamber
    /// lattice (Task 2's §6). Both halves are fixed together in Task 2b: this
    /// is the unit half.
    ///
    /// Two arms, and the second is what makes the first non-vacuous:
    ///
    /// 1. the shipped verdict equals the `CHAMBER`-parented, whole-address
    ///    derivation at every probed address;
    /// 2. it DIFFERS from the `RUN_FLOORS`-parented derivation of the same
    ///    key at at least one of them. Arm 1 alone would survive a re-parented
    ///    `chamber_stream` if the two legs happened to agree everywhere — each
    ///    address is an independent coin flip, so agreement everywhere is
    ///    merely unlikely rather than impossible, and it is asserted rather
    ///    than assumed.
    ///
    /// **What this test cannot see, stated because the sibling test's own
    /// lesson is that a re-implementation is not a witness:** arm 1 calls
    /// [`chamber_key`], so a change to `chamber_key`'s *format string* moves
    /// both sides together and passes here. That direction is held by
    /// [`the_chamber_key_spelling_is_pinned`] and by
    /// [`the_existence_verdict_is_byte_pinned_over_a_known_lattice_slice`],
    /// whose goldens are literal integers.
    #[test]
    fn the_existence_draw_travels_the_chamber_leg_and_is_keyed_on_the_whole_address() {
        let seed = Seed(90210);
        let column = fixture_column();
        // At the reach ceiling under a 24 K/km cell: ΔT = 72 K, which is the
        // Nadir, so every band 0..=4 is inside the budget and none of
        // `chamber_exists`'s earlier gates can be what decides a verdict here.
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);

        let mut probed = 0usize;
        let mut disagreed = 0usize;
        for raw_cell in 0u32..12 {
            // Only the branches this system realizes: past the drawn count
            // (spec C.1) the branch gate refuses before any draw happens, so
            // a verdict there would say nothing about which stream was
            // consulted — the same rule the floor loop below already follows.
            let realized_branches = crate::character::branch_count_of(seed, CellId(raw_cell), 0);
            for branch in 0..realized_branches {
                for band in 0..5u8 {
                    let run = RunAddr {
                        cell: CellId(raw_cell),
                        entrance: 0,
                        branch,
                        band,
                    };
                    // Only the floors this run realizes: past them the floor
                    // gate refuses before any draw happens, so a verdict there
                    // would say nothing about which stream was consulted.
                    for floor in 0..floors_in_run(seed, run) {
                        let addr = ChamberAddr {
                            cell: CellId(raw_cell),
                            entrance: 0,
                            branch,
                            band,
                            floor,
                        };
                        let via_chamber_leg = seed
                            .derive(crate::streams::CHAMBER)
                            .derive(StreamLabel::dynamic(&chamber_key(addr)))
                            .stream()
                            .next_f64()
                            < EXISTENCE_DENSITY;
                        let via_run_leg = seed
                            .derive(crate::streams::RUN_FLOORS)
                            .derive(StreamLabel::dynamic(&chamber_key(addr)))
                            .stream()
                            .next_f64()
                            < EXISTENCE_DENSITY;

                        let shipped = chamber_exists(seed, &cave, gradient, addr);
                        assert_eq!(
                            shipped,
                            via_chamber_leg,
                            "{}: chamber_exists answered {shipped}, but the CHAMBER \
                             leg keyed on the whole address derives {via_chamber_leg} \
                             — the shipped existence draw is not travelling the \
                             derivation it declares",
                            chamber_key(addr)
                        );
                        probed += 1;
                        if via_chamber_leg != via_run_leg {
                            disagreed += 1;
                        }
                    }
                }
            }
        }
        assert!(probed > 0, "the sweep probed no realized address at all");
        assert!(
            disagreed > 0,
            "the two legs agreed on all {probed} probed addresses, so arm 1 \
             would pass under a re-parented chamber_stream — this test is \
             vacuous as written and needs a wider or different sample"
        );
    }

    /// The existence verdict is **byte-pinned over a known slice of the
    /// lattice** — one literal `u32` per `(branch, band)`, bit `f` set when
    /// floor `f` exists. The same discipline
    /// [`the_run_draw_is_byte_pinned_for_known_keys`] carries, and the
    /// cheapest possible witness that ANY part of the existence derivation
    /// moved: the parent leg, the key's spelling, `next_f64`'s draw
    /// semantics, [`EXISTENCE_DENSITY`], the floor ceiling, or the run draw
    /// the floor gate reads.
    ///
    /// **The goldens are literal integers, not values re-derived through the
    /// path they claim to pin.** That is the whole difference between a pin
    /// and a restatement: every expression this test names is one the shipped
    /// call reads, and none of them can move both sides of the comparison at
    /// once.
    ///
    /// 20 floors fit a `u32` with room to spare
    /// ([`FLOORS_PER_RUN_CEILING`]); a wider lattice would need a wider mask,
    /// and the assertion below says so rather than truncating.
    ///
    /// If this fails, every chamber in every world has moved. That is an
    /// **epoch** (`chamber/v4`), not a fix to these numbers.
    ///
    /// **The Stope's C.1 gate moved these goldens once already, on purpose**:
    /// the branch-count draw now refuses branches past their system's drawn
    /// width, so the slice covers TWO entrances (cell 9's entrance 0 draws 1
    /// branch, entrance 1 draws 2) and the unrealized branches read as
    /// literal zeros — a branch gate that stopped gating would fill those
    /// rows in and redden this pin, which is exactly what it is for.
    #[test]
    fn the_existence_verdict_is_byte_pinned_over_a_known_lattice_slice() {
        assert!(
            u32::from(FLOORS_PER_RUN_CEILING) <= u32::BITS,
            "the lattice admits {FLOORS_PER_RUN_CEILING} floors, which no \
             longer fits the u32 masks below — widen the mask type rather \
             than letting the pin silently cover only the first 32 floors"
        );

        let seed = Seed(42);
        let column = fixture_column();
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);

        // Cell 9, entrances 0 and 1. Row-major over (entrance, branch 0..4,
        // band 0..5), so each line below is one branch's undercroft /
        // shallows / deeps / underdeep / nadir. Decimal, matching what
        // `assert_eq!` prints on a failure, so the two can be compared by eye.
        //
        // A zero is a legitimate reading, not a hole in the pin: entrance 0
        // realizes one branch and entrance 1 two, so every row past those
        // counts is zero by the C.1 gate, and branch 1's nadir run under
        // entrance 1 realizes no floor at all — the sparsity this lattice is
        // supposed to have.
        let expected: [u32; 40] = [
            1, 29, 1059, 17, 1, //
            0, 0, 0, 0, 0, //
            0, 0, 0, 0, 0, //
            0, 0, 0, 0, 0, //
            21, 472, 128, 892, 0, //
            12, 5, 79535, 738, 1, //
            0, 0, 0, 0, 0, //
            0, 0, 0, 0, 0,
        ];

        let mut got = [0u32; 40];
        for entrance in 0..2u8 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for band in 0..5u8 {
                    let mut mask = 0u32;
                    for floor in 0..FLOORS_PER_RUN_CEILING {
                        if chamber_exists(
                            seed,
                            &cave,
                            gradient,
                            ChamberAddr {
                                cell: CellId(9),
                                entrance,
                                branch,
                                band,
                                floor,
                            },
                        ) {
                            mask |= 1u32 << u32::from(floor);
                        }
                    }
                    got[usize::from(entrance) * 20 + usize::from(branch) * 5 + usize::from(band)] =
                        mask;
                }
            }
        }
        assert_eq!(
            got, expected,
            "the realized-floor masks of cell 9's lattice moved off their pin"
        );
    }

    /// `chamber_exists` already refuses a `branch` outside the lattice and a
    /// `band` past the cave's budget; `floor` needs the same guard, or an
    /// address outside the lattice silently derives a chamber.
    ///
    /// Asserted with a **positive control**: the same address at an in-range
    /// floor must be reachable, or "nothing exists out of range" would be
    /// satisfied by a function that refused everything. The control looks for
    /// at least one existing chamber across the in-range floors rather than
    /// requiring a particular one, because existence is a density draw.
    #[test]
    fn an_out_of_range_floor_does_not_exist() {
        let seed = Seed(90210);
        let column = fixture_column();
        // A cave at the reach ceiling, so the BAND gate cannot be what
        // refuses — band 0 is inside any budget, but a deep cave keeps the
        // fixture honest if a later reader moves the band under test.
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);
        let base = ChamberAddr {
            cell: CellId(1),
            entrance: 0,
            branch: 0,
            band: 0,
            floor: 0,
        };

        for floor in FLOORS_PER_RUN_CEILING..=FLOORS_PER_RUN_CEILING + 8 {
            assert!(
                !chamber_exists(seed, &cave, gradient, ChamberAddr { floor, ..base }),
                "floor {floor} is outside the lattice \
                 (ceiling {FLOORS_PER_RUN_CEILING}) and must name nowhere"
            );
        }

        // Positive control: the gate refuses out-of-range floors and not the
        // whole axis.
        assert!(
            (0..FLOORS_PER_RUN_CEILING).any(|floor| chamber_exists(
                seed,
                &cave,
                gradient,
                ChamberAddr { floor, ..base }
            )),
            "no in-range floor exists either, so the refusal above proves \
             nothing about the floor gate"
        );
    }

    /// `rung_rank` and `rung_of_rank` are one bijection over the habitation
    /// rungs. Kept honest here so the pair cannot drift half-updated when a
    /// sixth [`DelveRung`] lands.
    #[test]
    fn rung_rank_and_rung_of_rank_round_trip() {
        for rung in hornvale_terrain::rungs()
            .iter()
            .copied()
            .filter(|r| *r != DelveRung::Surface)
        {
            let rank = rung_rank(rung).expect("a habitation rung has a rank");
            assert_eq!(rung_of_rank(rank), Some(rung));
        }
        assert_eq!(rung_of_rank(5), None, "the ladder ends at rank 4");
        assert_eq!(
            rung_rank(DelveRung::Surface),
            None,
            "the overworld is not an underground address"
        );
    }

    /// The drainage rule (spec §4.2.1, clause 2), stated over the whole
    /// two-by-two: a `Found` chamber tracks the hydrology in both directions,
    /// and a `Made` chamber is dry in both — including the case that carries
    /// the meaning, a made chamber a kilometre below a surface water table.
    #[test]
    fn a_made_chamber_is_dry_however_deep_the_water_table_is_above_it() {
        // Above the table: nobody is flooded.
        assert!(!is_sump(ChamberOrigin::Found, 10.0, 50.0));
        assert!(!is_sump(ChamberOrigin::Made, 10.0, 50.0));
        // Below the table: only what nobody cut.
        assert!(is_sump(ChamberOrigin::Found, 100.0, 50.0));
        assert!(!is_sump(ChamberOrigin::Made, 100.0, 50.0));
        // The case the rule exists for: a drowned column (table at the
        // surface) and a chamber a kilometre down.
        assert!(is_sump(ChamberOrigin::Found, 1000.0, 0.0));
        assert!(
            !is_sump(ChamberOrigin::Made, 1000.0, 0.0),
            "a hall is kept dry by the people who cut it, not by the rock"
        );
    }

    /// The rule must not have quietly become "made chambers are shallow" or
    /// any other predicate on depth: a made chamber is dry at every depth the
    /// ladder reaches, and a found one is flooded at every depth below the
    /// table. Swept rather than sampled, because the two-by-two above cannot
    /// tell a constant from a threshold that happens to sit outside it.
    #[test]
    fn the_drainage_exemption_is_unconditional_in_depth() {
        for depth in [0.0, 1.0, 130.0, 1042.0, 2083.0, 3000.0] {
            for table in [0.0, 50.0, 500.0, 5000.0] {
                assert!(
                    !is_sump(ChamberOrigin::Made, depth, table),
                    "made chamber flooded at depth {depth} under table {table}"
                );
                assert_eq!(
                    is_sump(ChamberOrigin::Found, depth, table),
                    depth > table,
                    "found chamber disagreed with the hydrology at {depth}/{table}"
                );
            }
        }
    }

    /// `resolve_origin`'s full truth table (spec §3.3). The two rows that
    /// matter most are the last two: once `default` is `Made`, NEITHER an
    /// explicit `Some(Found)` override NOR the absence of any override at
    /// all can pull it back to `Found` — that is the absorbing rule stated
    /// as a property, not just exercised incidentally by
    /// `an_override_wins_over_the_derived_default` in `deep_realm_chamber.rs`.
    #[test]
    fn made_is_absorbing_over_every_default_and_override_combination() {
        assert_eq!(
            resolve_origin(ChamberOrigin::Found, None),
            ChamberOrigin::Found
        );
        assert_eq!(
            resolve_origin(ChamberOrigin::Found, Some(ChamberOrigin::Found)),
            ChamberOrigin::Found
        );
        assert_eq!(
            resolve_origin(ChamberOrigin::Found, Some(ChamberOrigin::Made)),
            ChamberOrigin::Made,
            "an override must win over a Found default"
        );
        assert_eq!(
            resolve_origin(ChamberOrigin::Made, None),
            ChamberOrigin::Made,
            "a Made default must survive the absence of an override"
        );
        assert_eq!(
            resolve_origin(ChamberOrigin::Made, Some(ChamberOrigin::Found)),
            ChamberOrigin::Made,
            "Made is absorbing: an explicit Found override must not pull a \
             Made default back to Found"
        );
        assert_eq!(
            resolve_origin(ChamberOrigin::Made, Some(ChamberOrigin::Made)),
            ChamberOrigin::Made
        );
    }

    /// Spec §3.1's floors-per-band ranges, **restated as literals** rather
    /// than read from [`floors_range`]. Reading the table the draw itself
    /// reads would make the range assertion below circular — it would pass
    /// for any table at all. These five pairs are the frozen preregistration
    /// (spec §3.1, `Sunless` renamed `Nadir` by amendment B.3); moving one is
    /// a design act, and the campaign's own rule is that they are **not** to
    /// be retuned to make §4.2's readout land in its intended band.
    const FROZEN_RANGES: [(u8, u8, u8); 5] = [
        (0, 1, 5),  // Undercroft
        (1, 3, 10), // Shallows
        (2, 5, 20), // Deeps
        (3, 5, 10), // Underdeep
        (4, 1, 5),  // Nadir
    ];

    /// A run's drawn floor count lands inside its own band's frozen range —
    /// the first half of Task 2's claim.
    ///
    /// **The range check alone is satisfiable by a constant**, so the sweep
    /// also demands that every band produce at least two distinct counts.
    /// Every frozen range spans at least three values, so a band that only
    /// ever answers one number is either not drawing or drawing on a key that
    /// does not vary here.
    /// claim: invariant(forall-seed) — a run's drawn count lies in its band's
    /// frozen range, over a hand-named lattice (seedless sweep, audit §5:
    /// builds no world)
    #[test]
    fn a_runs_floor_count_falls_in_its_bands_frozen_range() {
        for (band, lo, hi) in FROZEN_RANGES {
            let mut seen = std::collections::BTreeSet::new();
            for raw_seed in [1u64, 2, 3] {
                let seed = Seed(raw_seed);
                for raw_cell in 0u32..40 {
                    for entrance in 0..2u8 {
                        for branch in 0..BRANCHES_PER_SYSTEM {
                            let run = RunAddr {
                                cell: CellId(raw_cell),
                                entrance,
                                branch,
                                band,
                            };
                            let n = floors_in_run(seed, run);
                            assert!(
                                (lo..=hi).contains(&n),
                                "band {band}: {run:?} under seed {raw_seed} drew {n} \
                                 floors, outside the frozen range {lo}..={hi}"
                            );
                            seen.insert(n);
                        }
                    }
                }
            }
            assert!(
                seen.len() > 1,
                "band {band}: every run drew the same count {seen:?} — a constant \
                 satisfies the range check without drawing anything"
            );
            assert!(
                seen.contains(&lo) && seen.contains(&hi),
                "band {band}: the sweep saw {seen:?}, which does not reach both \
                 ends of {lo}..={hi} — the draw is not covering its own range"
            );
        }
    }

    /// A run's floor count is a pure function of `(seed, run)`: asked twice,
    /// it answers the same, and asking about a *different* run in between
    /// cannot disturb it. The second half matters because a draw that
    /// advanced some shared stream would be deterministic per call sequence
    /// and not per place, which is decision 0102's defect wearing a
    /// deterministic mask.
    #[test]
    fn a_runs_floor_count_is_deterministic_for_one_address() {
        let seed = Seed(90210);
        let run = RunAddr {
            cell: CellId(9),
            entrance: 0,
            branch: 3,
            band: 2,
        };
        let first = floors_in_run(seed, run);
        for raw_cell in 0u32..20 {
            for band in 0..5u8 {
                let _ = floors_in_run(
                    seed,
                    RunAddr {
                        cell: CellId(raw_cell),
                        entrance: 1,
                        branch: 0,
                        band,
                    },
                );
            }
        }
        assert_eq!(
            floors_in_run(seed, run),
            first,
            "the same run drew a different count after unrelated runs were \
             asked — the draw is keyed on a call order, not on a place"
        );
    }

    /// **Every component of the run key is load-bearing.** Varying exactly one
    /// of `cell`, `entrance`, `branch`, `band` must be able to change the
    /// answer; a component the key dropped would make its column here
    /// constant, and the whole point of keying on a place is that each
    /// coordinate of the place names a different run.
    ///
    /// This subsumes the brief's "two different branches in one band may
    /// differ" — that is the `branch` row — and catches the three ways to get
    /// that one row right while dropping another coordinate.
    #[test]
    fn every_component_of_the_run_key_is_load_bearing() {
        let seed = Seed(4242);
        let base = RunAddr {
            cell: CellId(0),
            entrance: 0,
            branch: 0,
            band: 2,
        };

        let vary_cell = (0u32..200).any(|c| {
            floors_in_run(
                seed,
                RunAddr {
                    cell: CellId(c),
                    ..base
                },
            ) != floors_in_run(seed, base)
        });
        assert!(vary_cell, "`cell` never changed a run's floor count");

        let vary_entrance = (0u8..64).any(|e| {
            floors_in_run(
                seed,
                RunAddr {
                    entrance: e,
                    ..base
                },
            ) != floors_in_run(seed, base)
        });
        assert!(
            vary_entrance,
            "`entrance` never changed a run's floor count"
        );

        // `branch` has only BRANCHES_PER_SYSTEM values, so one cell is not
        // enough to be sure two of them differ; sweep cells until a cell whose
        // branches disagree turns up. This is the brief's own clause: two
        // different branches in one band may differ.
        let vary_branch = (0u32..200).any(|c| {
            let counts: std::collections::BTreeSet<u8> = (0..BRANCHES_PER_SYSTEM)
                .map(|branch| {
                    floors_in_run(
                        seed,
                        RunAddr {
                            cell: CellId(c),
                            branch,
                            ..base
                        },
                    )
                })
                .collect();
            counts.len() > 1
        });
        assert!(
            vary_branch,
            "no cell had two branches whose runs differed in length — `branch` \
             is not in the key, so a whole system is one column again"
        );

        let vary_band = (0..5u8)
            .any(|band| floors_in_run(seed, RunAddr { band, ..base }) != floors_in_run(seed, base));
        assert!(vary_band, "`band` never changed a run's floor count");
    }

    /// The run key is a **save-format contract**, pinned the same way
    /// [`chamber_key`]'s is, and spelled to the same rules: decimal for the
    /// integers that name a place, the rung's NAME for `band` (never its
    /// index — see [`chamber_key`]'s own doc for why an index is a
    /// declaration position rather than a place).
    ///
    /// If this fails you have re-keyed every run in every world, which
    /// re-decides how many floors every run has. That is an epoch
    /// (`chamber/run-floors/v2` next), not a fix to this assertion.
    #[test]
    fn the_run_key_spelling_is_pinned() {
        assert_eq!(
            run_key(RunAddr {
                cell: CellId(9),
                entrance: 0,
                branch: 3,
                band: 2,
            }),
            "9/0/3/deeps"
        );
        assert_eq!(
            run_key(RunAddr {
                cell: CellId(0),
                entrance: 1,
                branch: 0,
                band: 0,
            }),
            "0/1/0/undercroft"
        );
    }

    /// The run key is **injective over the run lattice** — the same guard
    /// [`chamber_key`] carries, for the same reason: two runs sharing a key
    /// would be one run, and every floor count in the world would be drawn
    /// half as many times as it looks.
    #[test]
    fn the_run_key_is_injective_over_the_lattice() {
        let mut keys = std::collections::BTreeSet::new();
        let mut count = 0usize;
        for cell in 0..5u32 {
            for entrance in 0..3u8 {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    for band in 0..5u8 {
                        count += 1;
                        keys.insert(run_key(RunAddr {
                            cell: CellId(cell),
                            entrance,
                            branch,
                            band,
                        }));
                    }
                }
            }
        }
        assert_eq!(
            keys.len(),
            count,
            "{} of {count} run addresses collide on a key",
            count - keys.len()
        );
    }

    /// **A run draw and a chamber draw cannot collide, and the reason is the
    /// PARENT, not the key.** A run key is a strict prefix of a chamber key
    /// today, so the strings can never be equal — but that is a property of
    /// today's spelling, and a campaign that made `floor` optional in the
    /// chamber key would break it with nothing to object.
    ///
    /// So this asserts the durable half instead: hand **the same string** to
    /// both legs and the derived seeds still differ, because
    /// [`crate::streams::CHAMBER`] and [`crate::streams::RUN_FLOORS`] are
    /// different parents. Under that, key-shape collisions are not a hazard
    /// this design has.
    ///
    /// **WHAT THIS TEST DOES NOT HOLD, stated because the campaign report
    /// once claimed it did.** It derives both legs INLINE and calls neither
    /// [`run_stream`] nor [`floors_in_run`], so it is a property of the two
    /// label CONSTANTS and says nothing about which parent the shipped draw
    /// actually travels. Review demonstrated the gap with a one-token
    /// mutation — `run_stream`'s `derive(RUN_FLOORS)` to `derive(CHAMBER)`,
    /// which changes every floor count in every world — and the whole crate
    /// stayed green (371 lib tests, 286 suite tests). The assertion that
    /// closes it is
    /// [`the_run_draw_travels_the_run_floors_leg_and_not_the_chamber_leg`],
    /// which starts from `floors_in_run` instead. Both are kept: this one
    /// says the separation is AVAILABLE, that one says the shipped path USES
    /// it.
    #[test]
    fn the_run_leg_and_the_chamber_leg_cannot_collide() {
        let seed = Seed(1);
        for text in [
            "9/0/3/deeps",
            "9/0/3/deeps/0",
            "0/1/0/undercroft",
            "",
            "out-of-ladder",
        ] {
            let under_chamber = seed
                .derive(crate::streams::CHAMBER)
                .derive(StreamLabel::dynamic(text));
            let under_run = seed
                .derive(crate::streams::RUN_FLOORS)
                .derive(StreamLabel::dynamic(text));
            assert_ne!(
                under_chamber, under_run,
                "the key {text:?} derives the same seed under both legs, so the \
                 two draws are not separated by their parent"
            );
        }
    }

    /// **The SHIPPED run draw travels [`crate::streams::RUN_FLOORS`], not
    /// [`crate::streams::CHAMBER`].** This starts from [`floors_in_run`] —
    /// the function the world actually calls — and compares it against the
    /// derivation spelled out here, so re-parenting [`run_stream`] reddens it.
    ///
    /// Review found that nothing held this: mutating `run_stream`'s parent
    /// from `RUN_FLOORS` to `CHAMBER` changes the floor count of every run in
    /// every world, and the entire crate stayed green. The neighbouring test
    /// looked like it covered this and did not — it derived both legs inline
    /// and never touched the shipped path. **The general lesson, which
    /// applies past this function: a test that RE-IMPLEMENTS a derivation
    /// cannot witness the real one.**
    ///
    /// Two arms, and the second is what makes the first non-vacuous:
    ///
    /// 1. the shipped answer equals the `RUN_FLOORS`-parented derivation, at
    ///    every probed run;
    /// 2. it DIFFERS from the `CHAMBER`-parented derivation at at least one
    ///    of them. Arm 1 alone would pass under the mutation if the two
    ///    parents happened to agree on every probe — with a 1-in-5 to
    ///    1-in-16 chance of agreeing per run, a small sample could be
    ///    unlucky, so the disagreement is asserted rather than assumed.
    ///
    /// **`chamber_stream`'s parent is unpinned in exactly this way**, and
    /// that is a pre-existing gap rather than a regression this campaign
    /// introduced: re-parenting it would relocate every chamber, and what
    /// would object is the artifact drift check — which §6 of this campaign's
    /// Task 2 report establishes is structurally blind to the chamber
    /// lattice. Worth a follow-up; not fixed here, because a chamber-leg
    /// assertion is not this task's subject and inventing one silently would
    /// hide that the artifact-level witness is missing.
    #[test]
    fn the_run_draw_travels_the_run_floors_leg_and_not_the_chamber_leg() {
        let seed = Seed(90210);
        let mut disagreed = false;
        let mut probed = 0usize;
        for raw_cell in 0u32..40 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for band in 0..5u8 {
                    let run = RunAddr {
                        cell: CellId(raw_cell),
                        entrance: 0,
                        branch,
                        band,
                    };
                    let rung = rung_of_rank(run.band).expect("ranks 0..=4 name a rung");
                    let (lo, hi) = floors_range(rung).expect("a habitation rung has a range");

                    let via_run_leg = seed
                        .derive(crate::streams::RUN_FLOORS)
                        .derive(StreamLabel::dynamic(&run_key(run)))
                        .stream()
                        .range_u32(u32::from(lo), u32::from(hi));
                    let via_chamber_leg = seed
                        .derive(crate::streams::CHAMBER)
                        .derive(StreamLabel::dynamic(&run_key(run)))
                        .stream()
                        .range_u32(u32::from(lo), u32::from(hi));

                    let shipped = u32::from(floors_in_run(seed, run));
                    assert_eq!(
                        shipped, via_run_leg,
                        "{run:?}: floors_in_run answered {shipped}, but the \
                         RUN_FLOORS leg derives {via_run_leg} — the shipped draw \
                         is not travelling the leg it declares"
                    );
                    probed += 1;
                    if via_run_leg != via_chamber_leg {
                        disagreed = true;
                    }
                }
            }
        }
        assert!(probed > 0, "the sweep probed nothing");
        assert!(
            disagreed,
            "the two legs agreed on all {probed} probed runs, so arm 1 above \
             would pass under a re-parented run_stream — this test is vacuous \
             as written and needs a wider or different sample"
        );
    }

    /// The run draw is **byte-pinned for four known keys**, one per distinct
    /// frozen range plus a repeat — the same discipline
    /// `tolerance_draw::the_draw_is_byte_pinned_for_a_known_key` carries, and
    /// the cheapest possible witness that ANY part of the derivation moved:
    /// the parent leg, the key spelling, `range_u32`'s draw semantics, or the
    /// range table.
    ///
    /// If this fails, every run in every world is a different length. That is
    /// an epoch (`chamber/run-floors/v2`), not a fix to these numbers.
    #[test]
    fn the_run_draw_is_byte_pinned_for_known_keys() {
        let seed = Seed(42);
        for (cell, entrance, branch, band, expected) in [
            (9u32, 0u8, 3u8, 2u8, 16u8), // 9/0/3/deeps,        range 5-20
            (0, 1, 0, 0, 2),             // 0/1/0/undercroft,   range 1-5
            (17, 0, 2, 4, 4),            // 17/0/2/nadir,       range 1-5
            (5, 0, 1, 3, 5),             // 5/0/1/underdeep,    range 5-10
        ] {
            let run = RunAddr {
                cell: CellId(cell),
                entrance,
                branch,
                band,
            };
            assert_eq!(
                floors_in_run(seed, run),
                expected,
                "the run {} moved off its pin",
                run_key(run)
            );
        }
    }

    /// **The lattice ceiling must be at least as tall as the tallest thing
    /// the draw can ask for**, or a run reports a length the lattice refuses.
    ///
    /// [`chamber_exists`] checks `floor >= FLOORS_PER_RUN_CEILING` BEFORE it
    /// checks `floor >= floors_in_run(..)`, so a future range with `hi > 20`
    /// would produce runs that claim N floors and realize 20 — a silent,
    /// permanent truncation with no error anywhere. One assertion closes it,
    /// and it belongs beside the table rather than in a reviewer's head.
    #[test]
    fn the_lattice_ceiling_covers_every_bands_frozen_maximum() {
        for rank in 0..5u8 {
            let rung = rung_of_rank(rank).expect("ranks 0..=4 name a rung");
            let (lo, hi) = floors_range(rung).expect("a habitation rung has a range");
            assert!(
                lo >= 1,
                "{rung:?} may draw {lo} floors, so its run can be empty"
            );
            assert!(
                hi <= FLOORS_PER_RUN_CEILING,
                "{rung:?} may draw {hi} floors but the lattice admits only \
                 {FLOORS_PER_RUN_CEILING}, so chamber_exists would truncate \
                 the run silently — the ceiling gate runs first"
            );
        }
    }

    /// The run-floors label is a **save-format contract**; `stream_labels!`
    /// cannot tell a bump from a typo, so the literal is asserted here — the
    /// same pin [`crate::streams::CHAMBER`] carries.
    #[test]
    fn the_run_floors_label_is_v1() {
        assert_eq!(crate::streams::RUN_FLOORS.as_str(), "chamber/run-floors/v1");
        assert_ne!(
            crate::streams::RUN_FLOORS.as_str(),
            crate::streams::CHAMBER.as_str(),
            "the run draw must not share the chamber leg's label"
        );
    }

    /// **`chamber_exists` refuses a floor past its run's drawn count** — the
    /// half of Task 2 that changes what worlds contain. Before this, every
    /// in-budget run admitted all [`FLOORS_PER_RUN_CEILING`] floors, so the
    /// realized population was the lattice ceiling wearing a distribution's
    /// clothes.
    ///
    /// Two arms, and the second is a positive control: past the drawn count
    /// **nothing** exists (a hard invariant), and below it **something** does
    /// (or "nothing past the count" would be satisfied by a gate that refused
    /// the whole axis). The control is stated as "at least one run in the
    /// sweep has at least one realized floor" rather than per-run, because
    /// existence below the count is still a coin-flip draw.
    #[test]
    fn chamber_exists_refuses_a_floor_past_its_runs_drawn_count() {
        let seed = Seed(90210);
        let column = fixture_column();
        // At the reach ceiling, so every band 0..=4 is in budget and the BAND
        // gate can never be what refuses.
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);

        let mut realized_below = 0usize;
        let mut runs_shorter_than_the_ceiling = 0usize;
        for raw_cell in 0u32..20 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for band in 0..5u8 {
                    let run = RunAddr {
                        cell: CellId(raw_cell),
                        entrance: 0,
                        branch,
                        band,
                    };
                    let drawn = floors_in_run(seed, run);
                    if drawn < FLOORS_PER_RUN_CEILING {
                        runs_shorter_than_the_ceiling += 1;
                    }
                    for floor in 0..FLOORS_PER_RUN_CEILING {
                        let addr = ChamberAddr {
                            cell: run.cell,
                            entrance: run.entrance,
                            branch: run.branch,
                            band: run.band,
                            floor,
                        };
                        let exists = chamber_exists(seed, &cave, gradient, addr);
                        if floor >= drawn {
                            assert!(
                                !exists,
                                "{addr:?} exists, but its run realizes only {drawn} \
                                 floors — the lattice ceiling is standing in for the \
                                 draw"
                            );
                        } else if exists {
                            realized_below += 1;
                        }
                    }
                }
            }
        }
        assert!(
            runs_shorter_than_the_ceiling > 0,
            "no run in the sweep drew fewer than {FLOORS_PER_RUN_CEILING} floors, \
             so the refusal above never had anything to refuse"
        );
        assert!(
            realized_below > 0,
            "no floor below any run's count exists either, so the refusal proves \
             nothing about the run gate"
        );
    }

    /// The floors-per-band table is spec §3.1's, exhaustively, and `Surface`
    /// has no run at all. Stated against the same literals
    /// [`FROZEN_RANGES`] pins, so a silent edit to [`floors_range`] fails
    /// here rather than being absorbed by a range check that reads it.
    #[test]
    fn the_floors_per_band_table_is_spec_3_1s() {
        for (band, lo, hi) in FROZEN_RANGES {
            let rung = rung_of_rank(band).expect("ranks 0..=4 name a rung");
            assert_eq!(
                floors_range(rung),
                Some((lo, hi)),
                "{rung:?} must carry spec §3.1's frozen range"
            );
        }
        assert_eq!(
            floors_range(DelveRung::Surface),
            None,
            "the overworld is not a run"
        );
    }

    /// A band outside the habitation ladder names no run, so it realizes no
    /// floors — which makes [`chamber_exists`]'s gate refuse every floor
    /// there rather than deriving a count for nowhere.
    #[test]
    fn a_band_past_the_ladder_realizes_no_floors() {
        let seed = Seed(3);
        for band in 5..=8u8 {
            assert_eq!(
                floors_in_run(
                    seed,
                    RunAddr {
                        cell: CellId(1),
                        entrance: 0,
                        branch: 0,
                        band,
                    }
                ),
                0,
                "band {band} is past the ladder and must realize no floors"
            );
        }
    }

    /// Every rung spells differently. A collision would silently merge two
    /// depths' chambers into one derived stream.
    #[test]
    fn every_rung_has_a_distinct_spelling() {
        let names: Vec<&str> = (0..=4).filter_map(rung_of_rank).map(rung_name).collect();
        assert_eq!(names.len(), 5, "every rank 0..=4 must name a rung");
        for (i, a) in names.iter().enumerate() {
            for b in &names[i + 1..] {
                assert_ne!(a, b, "two bands share the spelling {a:?}");
            }
        }
    }
}
