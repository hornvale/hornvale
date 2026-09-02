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
//! [`hornvale_kernel::Band`]'s habitation rungs — ΔT classes above the
//! vertex's surface datum — so the same rung sits at different metre depths in
//! different vertices, and how far down the lattice a cave reaches is a fact
//! about its budget *and* its vertex's geothermal gradient. That is why both
//! entry points now take a [`GeothermalGradient`]: the lattice cannot place an
//! address in a vertex it knows nothing about.
//!
//! The stratigraphic ladder is still reported — [`Chamber`] carries both a
//! `rung` and a `stratum` — but the two are computed from different inputs and
//! **neither derives the other** (spec §4.1). The rung comes from the address;
//! the stratum comes from the vertex's own column, read at the depth the rung
//! begins at.

use std::collections::BTreeMap;

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Band, Seed, Stream, Vertex};
use hornvale_terrain::{
    Cave, GeneratedTerrain, GeothermalGradient, Horizon, StratigraphicColumn, delta_t_range_of,
    rung_at_depth,
};

/// Branch columns in the fixed lattice, beneath one cave-system address
/// (`vertex` alone — amendment A.3 dissolved the per-entrance sublattice: an
/// entrance is which aperture a player used, not a coordinate in the
/// system's own address space). Constant regardless of what any particular
/// cave realizes — rule 1a: this is the lattice's own size, never a count of
/// what a generator produced.
///
/// **Renamed from `SLOTS_PER_BAND` by The Stope**, with the field it bounds
/// (spec §3.1: "`slot` reads as a position and it is an identity"). The value
/// and the axis are unchanged; only the word is. Note that the constant's old
/// name said "per band" and the axis never was per-band: [`passages_from`]
/// has always joined the same position in adjacent bands, so a branch is a
/// **column persisting downward**, one range shared by every band.
///
/// **4**, chosen from The Deep Realm's Task 0 measured substrate rather than
/// invented: a cave reaching `Horizon::Roots` (the deepest band the live
/// generator ever produces — 30 seeds, 55,947 caves) spans 4 rungs of
/// `Realm::UNDERDARK.strata()` (`Regolith..=Roots`), so 4 branches keeps a
/// full system's address space the same order of magnitude as the band ladder
/// itself — enough for `branch` to be a genuinely separate axis from `band`,
/// not so large that a per-chamber neighbour walk or a per-cave enumeration
/// becomes expensive. This is a lattice-size judgement call, not a measured
/// quantity, and it can only widen, never relocate an existing address,
/// because `branch` numbers positions in the lattice, not generated chambers.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const BRANCHES_PER_SYSTEM: u8 = 4;

/// The lattice's **level ceiling**: how many levels one *branch* — the
/// levels of one `branch` within one `band` — can hold. Like
/// [`BRANCHES_PER_SYSTEM`], this is the lattice's own size, and rule 1a is
/// the whole reason it is a constant here rather than a drawn quantity.
///
/// **Renamed from `FLOORS_PER_RUN_CEILING` by The Drift** (spec amendment
/// A.3): `floor` becomes `level` throughout the address (spec §4.3 — "a
/// screen-filling map", not a storey of a building), and "run" becomes
/// "branch" now that a run's own coordinates are `(vertex, band, branch)` with
/// no `entrance` to distinguish it from the branch it belongs to.
///
/// **This is deliberately NOT the realized level count, and the distinction is
/// decision 0102's** (The Stope, spec §3.1). A branch's realized level count is
/// *drawn*, per band, within the ranges spec §3.1 froze (`Undercroft` 1–5,
/// `Shallows` 3–10, `Deeps` 5–20, `Underdeep` 5–10, `Nadir` 1–5); letting that
/// drawn count size the address space would let a generation quantity define
/// the lattice, which is exactly the defect 0102 exists to prevent. So the
/// lattice admits `0..LEVELS_PER_BRANCH_CEILING` at every branch, and the draw
/// decides which of those addresses a world actually realizes. That draw is
/// [`levels_in_branch`], and it landed in Task 2 — this constant has bounded a
/// *distribution* rather than stood in for one since.
///
/// **20**, the maximum of §3.1's own ranges (`Deeps` 5–20). Widening it later
/// is safe — a wider ceiling only admits addresses the lattice previously
/// refused, and relocates none, because [`chamber_key`] spells `level` as a
/// place and not as a fraction of a count.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const LEVELS_PER_BRANCH_CEILING: u8 = 20;

/// An address in the chamber lattice — a **place**, never a construction
/// step (spec §3.1). Four coordinates name: which vertex, which branch (a
/// column persisting downward), which depth band, and which level of that
/// branch within that band.
///
/// **The Drift's amendment A dropped `entrance`, and this is why.** Task 0's
/// baseline found that the shipped lattice keyed every draw on `(vertex,
/// entrance)`, so each entrance of a cave system realized its OWN private
/// sublattice — but the campaign's own worked example (a Blacksmith's Cellar
/// and a Cave under the Well, two Undercroft entrances, both descending into
/// the SAME Spider Cave) requires one lattice per SYSTEM with several
/// apertures into it. An entrance survives only as *which aperture a player
/// came in by* — [`entrance_count`] (keyed on the vertex) and
/// [`entrance_mouth`] (keyed on `(vertex, entrance)`) still answer that
/// question; it is no longer a coordinate of the place itself.
///
/// **The Stope's epoch (`chamber/v3`) reshaped this type before that**, and
/// both halves were save-format changes because [`chamber_key`] spelled the
/// whole address: `slot` was renamed to `branch` (spec §3.1 — "slot reads as
/// a position and it is an identity"; a branch owns a character and a run of
/// levels), and `floor` (now `level`) was added, because the underworld had
/// no interior at all — a band was one interior-less point per branch.
///
/// **`band` indexes the delve ladder's habitation rungs directly, as a
/// [`Band`] rather than a bare rank** (The Drift, Task 4) — `hornvale_kernel`
/// carries the roster and the ordering ([`Band::deeper`], [`Band::shallower`]
/// step it; [`Band::Surface`] is representable but is not a habitation band
/// and never names a real chamber, see [`chamber_exists`]). The permanent
/// ladder has 5 habitation rungs regardless of what any world's caves reach,
/// so `band` stays meaningful whatever the depth model does.
///
/// **It used to index `hornvale_climate::Realm::UNDERDARK.strata()`**, and
/// that is what `chamber/v1` was keyed on; rule 1a was written when Task 0
/// measured that the live generator only ever produced 3 of that ladder's 5
/// values. `MAP-cave-depth-weld` — the fix rule 1a named as the reason to keep
/// the address space wider than the realized one — landed in this campaign
/// (spec §4.0) and made the situation worse, not better: with a metre budget
/// capped at 3 km, `deepest_horizon` reached exactly `Basement` on 97.3–99.0% of
/// cave-bearing vertices across seeds 42 / 7 / 1234, so the lattice's depth axis
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
/// Deliberately carries **no `Serialize`/`Deserialize`**. The reason used to
/// be stated in the future tense — "nothing in this campaign writes a
/// `ChamberAddr` to a ledger, and the moment one is committed its on-disk
/// spelling becomes a permanent key" — and **that moment has arrived**:
/// `hornvale_vessel::passage::addr_key(&addr)`, a hand-rolled
/// `"{vertex}/{band:?}/{branch}/{level}"` string, reaches a saved world, and
/// `possess --out` saves it (decision 0368).
///
/// **What it reaches CHANGED with The Chattel (decision 0396), and the
/// contract got wider rather than narrower.** The Latch put `addr_key` in the
/// OBJECT of a `passage-cleared` fact, and that predicate is retired. It is
/// now the address leg of `passage::cave_mouth_role`, the `Lineage` role from
/// which a cave mouth's `EntityId` is DERIVED. So the absence of a derive is
/// still not what keeps this type off disk, and **renaming a [`Band`] variant
/// no longer orphans one predicate's facts — it renumbers the entity, and so
/// orphans every fact of every predicate about that cave mouth at once**
/// (`instance-of`, `openness`, and whatever a later arc adds).
///
/// Bounded, not open: worlds are version-locked (decision 0099), so a stale
/// key cannot corrupt a world that still loads. And it is no longer silent —
/// `the_cave_mouth_role_spelling_is_the_permanent_lineage_key`
/// (`windows/vessel/tests/suite/passage.rs`) pins the exact string, which
/// CONTAINS this one, so a variant rename reddens there and the epoch-suffix
/// discipline every other save-format contract obeys applies here too.
/// type-audit: bare-ok(index: branch), bare-ok(index: level)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ChamberAddr {
    /// Which surface vertex this chamber lies beneath.
    pub vertex: Vertex,
    /// Which branch of that cave system this chamber sits on
    /// (`0..BRANCHES_PER_SYSTEM`) — a column persisting downward through the
    /// bands, and the thing a character and a barrier state attach to (spec
    /// §3.3, §B.5). A lattice coordinate, not a count of chambers generated.
    pub branch: u8,
    /// Which rung of the delve ladder this chamber sits at — a typed
    /// [`Band`], not a bare index (The Drift, Task 4). `Band::Surface` is
    /// representable but names no chamber; see [`chamber_exists`].
    pub band: Band,
    /// Which level of this branch's run within this band
    /// (`0..LEVELS_PER_BRANCH_CEILING`) — the interior the lattice was
    /// missing. A lattice coordinate: the ceiling is fixed, and how many of
    /// those levels a given branch *realizes* is a separate drawn quantity
    /// ([`levels_in_branch`]) that may never size this axis (decision 0102;
    /// see [`LEVELS_PER_BRANCH_CEILING`]). **Renamed from `floor` by The
    /// Drift** (spec §4.3): a level is a screen-filling map, not a storey.
    pub level: u8,
}

impl ChamberAddr {
    /// The **run** this chamber belongs to — its address with `level`
    /// dropped. The one place that projection is spelled, so a caller can
    /// never assemble a [`RunAddr`] that disagrees with the chamber it came
    /// from.
    pub fn run(self) -> RunAddr {
        RunAddr {
            vertex: self.vertex,
            branch: self.branch,
            band: self.band,
        }
    }
}

/// The address of a **run** — the levels of one `branch` within one `band`
/// (spec §3.3: "a RUN = the floors of one branch within one band, and one
/// engine owns it"). A [`ChamberAddr`] with `level` removed.
///
/// **This type exists so that a run cannot carry a level**, which is the
/// whole content of the distinction: a run is exactly the thing that has no
/// level yet, because how many levels it has is what [`levels_in_branch`]
/// draws. Taking a `ChamberAddr` and ignoring its `level` would compile, and
/// would let a caller believe the answer depended on which level they
/// happened to pass.
///
/// Every component is a coordinate in the fixed lattice — vertex, branch,
/// band — and none of them is a generation ordinal (decision 0102). The draw
/// keyed on this is therefore a fact about a *place*, readable by anyone who
/// can name the place, in any order, with nothing generated first.
///
/// **`entrance` is GONE, same as [`ChamberAddr`]'s** (The Drift, amendment
/// A.3) — a run belongs to a system's shared lattice, not to any one
/// aperture into it.
///
/// Deliberately carries **no `Serialize`/`Deserialize`**, for the same reason
/// [`ChamberAddr`] does not: nothing commits one, and the moment one is
/// committed its on-disk spelling becomes a permanent key.
/// type-audit: bare-ok(index: branch)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RunAddr {
    /// Which surface vertex this run lies beneath.
    pub vertex: Vertex,
    /// Which branch of that cave system this run sits on
    /// (`0..BRANCHES_PER_SYSTEM`).
    pub branch: u8,
    /// Which rung of the delve ladder this run sits at — the same 5-rung
    /// habitation ladder [`ChamberAddr::band`] indexes.
    pub band: Band,
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
/// §3's substrate table lists a chamber's content as "depth — `Horizon` on
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
/// weaken that: they are constant for a given `addr.vertex`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Chamber {
    /// The address this content was derived for.
    pub addr: ChamberAddr,
    /// Which rung of the delve ladder this chamber sits on — a habitation
    /// depth class, literally `addr.band` since The Drift promoted it to a
    /// typed [`Band`] (Task 4). A pure function of the address.
    pub rung: Band,
    /// The rock stratum this chamber sits in — the *archive* answer, read off
    /// the vertex's own stratigraphic column at the depth [`Chamber::rung`]
    /// begins at. **Not derived from `rung`, and `rung` is not derived from
    /// it** (spec §4.1): the same rung is a different band in a vertex with a
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

/// The one explicit mapping between a NAMED [`Band`] (what
/// [`rung_at_depth`] returns for a cave's budget) and the lattice's index
/// space (what [`ChamberAddr::band`] indexes) — rule 1a's "compare them
/// through one explicit mapping, in one place." [`chamber_exists`] is the
/// only caller, so a lattice reader and the depth-budget gate can never
/// diverge on what a `band` number means. Exhaustive: a sixth `Band`
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
/// `ChamberOrigin::Made` writer turns a settled community's `(vertex, rung)`
/// seat into the [`ChamberAddr`]es beneath it, and that translation is exactly
/// this mapping. Rule 1a's "one explicit mapping, in one place" is what makes
/// widening it the right move rather than duplicating it in
/// [`crate::delve_seating`].
/// type-audit: bare-ok(index: return)
pub fn rung_rank(rung: Band) -> Option<u8> {
    match rung {
        Band::Surface => None,
        Band::Undercroft => Some(0),
        Band::Shallows => Some(1),
        Band::Deeps => Some(2),
        Band::Underdeep => Some(3),
        Band::Nadir => Some(4),
    }
}

/// A rung's spelling inside the chamber key — **a save-format contract**, and
/// the reason this is an explicit match rather than `format!("{rung:?}")`.
///
/// A derived `Debug` impl renders the variant's identifier, which *looks* like
/// exactly this table and is not the same promise. `Debug` is a diagnostic
/// facility: nothing stops a later reader from writing a hand-rolled `Debug`
/// for [`Band`] to make some log prettier, and doing so would silently
/// re-key every chamber in every world with no test able to see it. Stating
/// the strings here makes the contract reviewable, makes a rename an obvious
/// epoch decision, and forces a sixth variant to choose its own spelling
/// instead of inheriting one.
///
/// **This campaign is exactly the change that discipline was written to
/// survive**, and it did: re-pointing the axis at a different ladder changed
/// *which table* is consulted, in one place, visibly — instead of silently
/// changing what a `Debug` impl happened to print.
///
/// **`pub(crate)` since The Drift Task 5**, so `crate::character`'s
/// per-branch keys can spell `band` by the same name table `run_key` and
/// `chamber_key` use, rather than growing a second copy of it — the same
/// "one explicit mapping, in one place" argument [`rung_rank`]'s own doc
/// makes for widening it to `pub`.
pub(crate) fn rung_name(rung: Band) -> &'static str {
    match rung {
        Band::Surface => "surface",
        Band::Undercroft => "undercroft",
        Band::Shallows => "shallows",
        Band::Deeps => "deeps",
        Band::Underdeep => "underdeep",
        Band::Nadir => "nadir",
    }
}

/// The stratigraphic band a chamber at `rung` sits in, for a vertex with this
/// column and gradient — the *archive* answer, kept strictly separate from the
/// rung itself (spec §4.1: "neither derives the other").
///
/// The rung fixes a ΔT; the gradient turns that into a depth in metres for
/// **this** vertex; the column says what rock is at that depth. Two chambers on
/// the same rung under different vertices can therefore sit in different bands,
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
/// different vertices, and says nothing at rank 0. That is a property of taking
/// the top rather than a defect in the ladder — the midpoint of a rung, or its
/// bottom, would give rank 0 a vertex-varying stratum at the cost of naming a
/// depth no chamber is actually at. Whichever a later task picks, it should
/// pick knowing this is the trade, not discover it from a constant column.
fn stratum_at(rung: Band, gradient: GeothermalGradient, column: &StratigraphicColumn) -> Horizon {
    let (delta_t_k, _) = delta_t_range_of(rung);
    let depth_m = delta_t_k / gradient.get() * 1000.0;
    hornvale_terrain::features::band_at_depth(column, depth_m)
}

/// The one explicit mapping from terrain's [`Horizon`] to climate's
/// [`hornvale_climate::Stratum`] — the two enums name the same five rock units
/// from two domains that may not depend on each other, so the composition root
/// is the only place allowed to state the correspondence. Exhaustive on both
/// sides: a sixth variant on either fails this to compile.
fn stratum_of_band(band: Horizon) -> hornvale_climate::Stratum {
    match band {
        Horizon::Regolith => hornvale_climate::Stratum::Regolith,
        Horizon::Cover => hornvale_climate::Stratum::Cover,
        Horizon::Basement => hornvale_climate::Stratum::Basement,
        Horizon::Roots => hornvale_climate::Stratum::Roots,
        Horizon::Underneath => hornvale_climate::Stratum::Underneath,
    }
}

/// The one place the `chamber/v3` stream key is spelled — mirrors
/// `deity_base_seed`'s discipline (`windows/worldgen/src/lib.rs`): "the one
/// place the stream label is spelled, so [every caller] can never diverge."
/// The Drift (spec §4.1) removed this key's only stream-deriving caller
/// (`chamber_stream`, the existence draw) along with the draw itself; the
/// key survives because `chamber_at`'s test fixtures and the key-pinning
/// tests below still spell chamber addresses through it.
///
/// **The key spells the WHOLE address, so every field here is a save-format
/// contract.** The Stope added `floor` (since renamed `level`) and renamed
/// `slot` to `branch`; a key that omitted `level` would derive one stream for
/// every level of a run, which is to say the levels would all be the same
/// chamber. That is why `the_key_spells_the_level` sweeps the axis rather
/// than sampling it. **The Drift dropped `entrance`** (amendment A.3): an
/// entrance is which aperture a player used, not a coordinate in the
/// address, so the key no longer spells one at all.
///
/// **`vertex`, `branch` and `level` are genuine integers naming a
/// place and are spelled decimal. `band` is spelled by its [`Band`] NAME,
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
/// than stylistic. `addr.band` is now a typed [`Band`], so every value spells
/// a real rung — including [`Band::Surface`], which [`chamber_exists`] never
/// admits into a shipped address but which this pure formatter still names
/// rather than panicking on, because a formatter that could panic on some
/// inputs would not be safe to call from a debug context.
///
/// **`chamber_key` is a DISPLAY FORMATTER now, not a derivation key** (spec
/// amendment A.6, correcting A.4). Before The Drift's Task 1 deleted the
/// chamber existence draw, this string was hashed by `chamber_stream` into
/// the seed's derivation tree — the whole reason the doc above used to call
/// it "the real derivation key". That draw is gone, and with it went the
/// only production reader of the composed stream: `#[cfg(test)] mod tests`
/// begins after every remaining call site in this file, so nothing in a
/// shipped world derives from `crate::streams::CHAMBER` any more (see that
/// label's own doc). **What now carries the seed-derivation key is
/// [`run_key`]** (under [`crate::streams::RUN_FLOORS`]) and the three
/// per-branch keys in `crate::character` (under `BRANCH_COUNT` /
/// `BRANCH_CHARACTER` / `BRANCH_BARRIER`) — this function's sole production
/// caller is `underworld_readout.rs`'s witness, which prints it as a `key`
/// column for a human to read, not to re-derive anything from.
pub(crate) fn chamber_key(addr: ChamberAddr) -> String {
    format!(
        "{}/{}/{}/{}",
        addr.vertex.0,
        addr.branch,
        rung_name(addr.band),
        addr.level
    )
}

// `chamber_stream` (`chamber_key` composed under `crate::streams::CHAMBER`)
// was the existence draw's only reader. The Drift (spec §4.1) deleted that
// draw, and with it the function's last caller — an unused private helper is
// a value with no reader, so it is deleted rather than kept around. Nothing
// else consumed `crate::streams::CHAMBER` directly; `chamber_key` itself
// stays (it is still spelled by `chamber_at`'s tests and the key-pinning
// tests below, and by the underworld witness's display column — see this
// function's own doc), and the label constant `crate::streams::CHAMBER`
// stays too — it is a save-format contract already shipped, not a value
// this task owns.

/// Spec §3.1's levels-per-branch range for a band, inclusive on both ends —
/// **frozen preregistration, not a tunable**. `Undercroft` 1–5, `Shallows`
/// 3–10, `Deeps` 5–20, `Underdeep` 5–10, `Nadir` 1–5 (the last renamed from
/// `Sunless` by amendment B.3).
///
/// `None` for `Surface`: the overworld is not a branch. Exhaustive, so a
/// sixth [`Band`] fails this to compile rather than silently inheriting a
/// neighbour's range.
fn levels_range(rung: Band) -> Option<(u8, u8)> {
    match rung {
        Band::Surface => None,
        Band::Undercroft => Some((1, 5)),
        Band::Shallows => Some((3, 10)),
        Band::Deeps => Some((5, 20)),
        Band::Underdeep => Some((5, 10)),
        Band::Nadir => Some((1, 5)),
    }
}

/// The one place the `chamber/run-floors/v2` key is spelled — [`chamber_key`]'s
/// discipline one axis over, and its own save-format contract.
///
/// Spelled to the same rules as a chamber key and for the same reasons:
/// `vertex` and `branch` are integers naming a place and are decimal; `band` is
/// spelled by its [`Band`] **name** through [`rung_name`]'s explicit table,
/// because an index is a declaration position and a mid-ladder insertion
/// would silently re-key every run below it.
///
/// **It does not spell a level, and that is the type's whole content**: a run
/// is the thing that does not have one yet.
///
/// **`entrance` dropped out of this key at `chamber/run-floors/v2`** (The
/// Drift, amendment A.3/A.6): every entrance of a system now addresses INTO
/// the same shared lattice, so a run is no longer keyed to which aperture a
/// caller happened to ask about. This is a real re-keying of a LIVE
/// derivation — unlike [`chamber_key`], [`levels_in_branch`] is the shipped
/// entry point `chamber_exists` reads — so the label epoch moved with it.
fn run_key(run: RunAddr) -> String {
    format!("{}/{}/{}", run.vertex.0, run.branch, rung_name(run.band))
}

/// The stream a run's level count is drawn from — [`run_key`] composed under
/// [`crate::streams::RUN_FLOORS`], deliberately a **different parent** from
/// the (now-deleted, spec §4.1) existence draw's own — see `RUN_FLOORS`'s
/// own doc for why the separation lives in the parent rather than in the
/// key's shape.
fn run_stream(seed: Seed, run: RunAddr) -> Stream {
    seed.derive(crate::streams::RUN_FLOORS)
        .derive(StreamLabel::dynamic(&run_key(run)))
        .stream()
}

/// **How many levels this branch realizes** — the drawn quantity
/// [`LEVELS_PER_BRANCH_CEILING`] is deliberately not (The Stope, Task 2, spec
/// §3.1). Uniform on the band's own frozen range: `Undercroft` 1–5,
/// `Shallows` 3–10, `Deeps` 5–20, `Underdeep` 5–10, `Nadir` 1–5.
///
/// **Renamed from `floors_in_run` by The Drift** (spec §4.3): the levels of
/// one branch within one band, and "run" now means exactly that place.
///
/// **It takes a [`RunAddr`], not a [`ChamberAddr`] and not a bare band.** The
/// draw keys on vertex, branch and band, and a function cannot key on what it
/// is not given — so every component of the key is a parameter, and the
/// parameter type is the one that *cannot* carry a level, because a run is
/// precisely the thing that has no level yet.
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
/// `a_runs_level_count_is_deterministic_for_one_address`, which interleaves
/// unrelated queries specifically to catch a draw that advanced a shared
/// stream.
///
/// `0` for a band past the habitation ladder, which makes
/// [`chamber_exists`]'s level gate refuse every level there rather than
/// deriving a length for nowhere. Unreachable through `chamber_exists`, which
/// gates on `Band::Surface` first; stated so the function is total.
/// type-audit: bare-ok(count: return)
pub fn levels_in_branch(seed: Seed, run: RunAddr) -> u8 {
    let Some((lo, hi)) = levels_range(run.band) else {
        return 0;
    };
    let drawn = run_stream(seed, run).range_u32(u32::from(lo), u32::from(hi));
    // `range_u32` is inclusive and `hi` came from a `u8`, so this cannot
    // truncate; `expect` states that rather than masking it with a cast.
    u8::try_from(drawn).expect("range_u32(lo, hi) never exceeds hi, which is a u8")
}

// --- Band-transition edges (The Drift, Task 6; spec §4.5) ---
//
// Descent happens at a branch's bottom level, into the top level of a branch
// in the NEXT band down. Which branches those are was, until this task, not
// drawn at all: the shipped rule descended within the same branch column, so
// a system's bands were connected only by coincidence of numbering.
//
// The edge set for one adjacent band pair is the UNION OF TWO SURJECTIONS:
//
//   * every branch in the upper band draws one child below, so nothing
//     descends into a dead end;
//   * every branch in the lower band draws one parent above, so nothing is
//     unreachable.
//
// Both of §4.5's guarantees are therefore true BY CONSTRUCTION — the upper
// loop is total on the upper band's branches and the lower loop is total on
// the lower band's, so neither property can fail for any pair of widths, at
// any seed, in any world. **There is no repair pass, and its absence is the
// design**: draw-then-patch is order-dependent, and an order-dependent
// repair is a determinism hazard as well as a correctness one (§4.5).
//
// HOW MANY EDGES THIS PRODUCES IS NOT TUNED HERE. Spec §8 leaves "how many
// edges beyond the guaranteed union" open on purpose — zero extra makes the
// underworld a tree, many makes it a mesh — so this draws the union and
// nothing more, and the shape it produces is REPORTED rather than fitted.
// The union alone is already not a tree: an upper branch whose child draw
// disagrees with some lower branch's parent draw contributes both edges.
// The count is bounded by construction at `max(upper, lower) <= edges <=
// upper + lower`, and `the_edge_count_sits_between_its_construction_bounds`
// asserts exactly that rather than a fitted distribution.

/// Which of the two questions a [`crate::streams::BAND_DESCENT`] draw is
/// answering about one branch. It is spelled into the key, so these two
/// words are a save-format contract like every other component of one.
///
/// **A role is a question, not an ordinal** (decision 0102): one place in
/// the lattice is asked two independent things, and each gets its own
/// stream. Folding them into a single stream and drawing twice in a fixed
/// order would make the parent answer depend on whether the child draw was
/// taken first — the order-dependence §4.5 rejects.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DescentRole {
    /// Which branch of the band BELOW this branch descends into.
    Child,
    /// Which branch of the band ABOVE this branch hangs from.
    Parent,
}

impl DescentRole {
    /// The word this role is spelled by in a band-descent key.
    fn word(self) -> &'static str {
        match self {
            DescentRole::Child => "child",
            DescentRole::Parent => "parent",
        }
    }
}

/// The one place the `chamber/band-descent/v1` key is spelled — [`run_key`]'s
/// discipline with a role appended, and its own save-format contract.
///
/// `vertex` and `branch` are integers naming a place and are decimal; `band` is
/// spelled by its [`Band`] **name** through [`rung_name`], never its rank,
/// for the reason `chamber_key`/`run_key` both state: a rank is a
/// declaration position, and a mid-ladder insertion would silently re-key
/// every band below it. Field order is `RunAddr`'s own (vertex, branch, band),
/// the spelling every re-keyed leg in this crate agrees on, with
/// [`DescentRole`]'s word last.
///
/// **The band is the branch's OWN band**, never the pair's upper one. A
/// branch at the Deeps asks "who is my child" keyed at the Deeps and "who is
/// my parent" keyed at the Deeps; the Shallows↔Deeps pair therefore reads
/// one key at each end rather than two keys at the same end. That keeps a
/// key naming a band the branch actually occupies.
fn descent_key(vertex: Vertex, branch: u8, band: Band, role: DescentRole) -> String {
    format!("{}/{branch}/{}/{}", vertex.0, rung_name(band), role.word())
}

/// One band-descent draw: which of `width` branches in the adjacent band
/// this branch connects to, uniform over `0..width`.
///
/// `width` is the ADJACENT band's drawn branch count, so this is a draw over
/// a real population rather than over the lattice ceiling — the same
/// drawn-realization discipline [`levels_in_branch`] applies to levels.
/// Callers guarantee `width >= 1`; [`descent_edges`] refuses a zero-width
/// pair before reaching here, so no draw is ever taken over an empty range.
fn descent_pick(
    seed: Seed,
    vertex: Vertex,
    branch: u8,
    band: Band,
    role: DescentRole,
    width: u8,
) -> u8 {
    debug_assert!(width >= 1, "descent_pick needs a non-empty target band");
    let drawn = seed
        .derive(crate::streams::BAND_DESCENT)
        .derive(StreamLabel::dynamic(&descent_key(
            vertex, branch, band, role,
        )))
        .stream()
        .range_u32(0, u32::from(width - 1));
    // `range_u32` is inclusive and `width - 1` came from a `u8`, so this
    // cannot truncate; `expect` states that rather than masking it.
    u8::try_from(drawn).expect("range_u32(0, width - 1) never exceeds a u8")
}

/// **Every edge between one band and the next**, as `(upper branch, lower
/// branch)` pairs, sorted and deduplicated (spec §4.5).
///
/// `band` is the UPPER band of the pair and `upper`/`lower` are the two
/// bands' branch widths. **The widths are parameters, not reads**, and that
/// is deliberate: it is what lets the guarantee tests construct all sixteen
/// `(upper, lower)` pairs directly instead of sampling whichever widths a
/// few seeds happen to draw. A guarantee is asserted, never measured into
/// existence (spec §6). [`descents_from`] is the shipped caller and reads
/// both widths from [`crate::character::branch_count_of`].
///
/// Empty for [`Band::Surface`] (not a habitation band, so it has no branches
/// to connect), for [`Band::Nadir`] (nothing below it on the ladder), and
/// for a zero width on either side (no pair of branches to join).
fn descent_edges(seed: Seed, vertex: Vertex, band: Band, upper: u8, lower: u8) -> Vec<(u8, u8)> {
    if band == Band::Surface || upper == 0 || lower == 0 {
        return Vec::new();
    }
    let Some(below) = band.deeper() else {
        return Vec::new();
    };
    let mut edges: Vec<(u8, u8)> = Vec::with_capacity(usize::from(upper) + usize::from(lower));
    // Every branch above draws one child below, so nothing dead-ends.
    for from in 0..upper {
        let to = descent_pick(seed, vertex, from, band, DescentRole::Child, lower);
        edges.push((from, to));
    }
    // Every branch below draws one parent above, so nothing is orphaned.
    for to in 0..lower {
        let from = descent_pick(seed, vertex, to, below, DescentRole::Parent, upper);
        edges.push((from, to));
    }
    // Deterministic answer order, and the dedup is what makes the union a
    // union: an upper branch and a lower branch that picked each other
    // contribute the same edge twice. `(u8, u8)` is totally ordered, so
    // `sort_unstable` is total and the result is independent of draw order.
    edges.sort_unstable();
    edges.dedup();
    edges
}

/// **Which branches of the next band down this branch descends into** (spec
/// §4.5) — the edge set [`passages_from`]'s vertical rule is rewritten
/// against in Task 7. Ascending, deduplicated, and never empty for a branch
/// this band realizes above [`Band::Nadir`].
///
/// Empty in exactly four cases, all of them "there is nowhere below to go":
/// [`Band::Surface`] (not a habitation band), [`Band::Nadir`] (the bottom of
/// the ladder), a `branch` outside the fixed lattice
/// ([`BRANCHES_PER_SYSTEM`]), and a `branch` this band's own drawn width
/// does not realize.
///
/// **It knows the LADDER, not the ROCK.** Whether a cave actually reaches
/// the band below is a question about a place's geothermal gradient and its
/// cave's depth budget, and it is [`chamber_exists`]'s — asked with the
/// terrain this function is deliberately not given, exactly as
/// [`levels_in_branch`] is not given a `Cave`. So "empty at the deepest band
/// the rock allows" is a property of the composition, not of this function:
/// the rock's floor shows up when a caller tests the returned addresses for
/// existence.
///
/// **A place, never an ordinal** (decision 0102): the answer for one branch
/// is a fact about `(vertex, band, branch)` alone, independent of what has
/// been asked before it.
/// type-audit: bare-ok(index: branch), bare-ok(index: return)
pub fn descents_from(seed: Seed, vertex: Vertex, band: Band, branch: u8) -> Vec<u8> {
    if band == Band::Surface || branch >= BRANCHES_PER_SYSTEM {
        return Vec::new();
    }
    let Some(below) = band.deeper() else {
        return Vec::new();
    };
    let upper = crate::character::branch_count_of(seed, vertex, band);
    if branch >= upper {
        return Vec::new();
    }
    let lower = crate::character::branch_count_of(seed, vertex, below);
    descent_edges(seed, vertex, band, upper, lower)
        .into_iter()
        .filter(|&(from, _)| from == branch)
        .map(|(_, to)| to)
        .collect()
}

/// **Which branches of the band ABOVE descend into this branch** — the exact
/// mirror of [`descents_from`], read off the same edge set (spec §4.5).
/// Ascending, deduplicated, empty at [`Band::Surface`], at the top of the
/// habitation ladder, and for a branch this band's own drawn width does not
/// realize.
///
/// **This is not a second derivation of the descent relation, and that is the
/// whole reason it exists.** [`passages_from`]'s upward rule needs the same
/// edges its downward rule uses, read from the other end; computing them a
/// second way — a "who points at me" draw of its own, say — is precisely the
/// two-derivations-that-must-agree shape spec §3.2 calls the one genuinely
/// hard problem, and [`passages_from`]'s own doc says the temptation to
/// special-case a direction *is* the bug. Both directions call
/// [`descent_edges`] for the same `(vertex, upper band)` pair and filter it on
/// opposite coordinates, so the two can no more disagree than a set can
/// disagree with itself.
///
/// Private on purpose. Nothing outside this module needs it: a consumer
/// walking the lattice gets both directions from [`passages_from`], and a
/// test that wants the parent set can re-derive it from the public
/// [`descents_from`] over the band above — which is a genuinely independent
/// check rather than a re-invocation of the code under test.
fn ascents_from(seed: Seed, vertex: Vertex, band: Band, branch: u8) -> Vec<u8> {
    if band == Band::Surface || branch >= BRANCHES_PER_SYSTEM {
        return Vec::new();
    }
    let Some(above) = band.shallower() else {
        return Vec::new();
    };
    if above == Band::Surface {
        // The top of the habitation ladder: nothing above it has branches to
        // hang from. Returned before either width is read, so no draw is
        // taken for a band that has none.
        return Vec::new();
    }
    let lower = crate::character::branch_count_of(seed, vertex, band);
    if branch >= lower {
        return Vec::new();
    }
    let upper = crate::character::branch_count_of(seed, vertex, above);
    descent_edges(seed, vertex, above, upper, lower)
        .into_iter()
        .filter(|&(_, to)| to == branch)
        .map(|(from, _)| from)
        .collect()
}

// --- Entrances are plural (The Stope, Task 5; spec amendment C.3) ---
//
// Amendment C.3 supersedes §3.4's "entrance → branch": **an entrance maps
// to a FLOOR** of the system's lattice — main-line floor 0, or a branch's
// root floor (C.2). §3.4's two-door case falls out for free: the town-square
// well and the blacksmith's cellar are two entrances whose mapped floors are
// the main line's head and a branch's root. One mechanism, both readings.
//
// Terrain reports one cave per vertex with no aperture count
// (`hornvale_terrain::GeneratedTerrain::cave_at`), so both quantities below
// are DERIVED here at the composition root from what the cave already
// carries — exactly what [`ChamberAddr::entrance`]'s own doc anticipated.
//
// Both draws key on stable lattice places (decision 0102): the count on the
// system's vertex alone, the mouth on `(vertex, entrance index)`.

/// Where one entrance opens into its system's lattice (C.3): a branch, a
/// band rank and a floor, all coordinates the system actually realizes —
/// an aperture opens INTO a place, never onto a construction step.
///
/// The system's canonical lattice is `(vertex, entrance 0)` and every mouth
/// addresses INTO it, so two entrances land in ONE shared graph — two
/// breadth-first walks from two mouths overlap, which is why a consumer
/// counting reachability must union them rather than sum
/// (`windows/worldgen/src/underworld_readout.rs` does exactly that).
/// type-audit: bare-ok(index: branch), bare-ok(index: band), bare-ok(index: floor)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EntranceMouth {
    /// Which branch column the descent starts on (`0..BRANCHES_PER_SYSTEM`).
    pub branch: u8,
    /// Which delve-ladder rank of that column the mouth sits at.
    pub band: u8,
    /// Which floor of that run the mouth sits at.
    pub floor: u8,
}

/// How many apertures one cave system opens to the surface (C.3, amended by
/// spec E.2) — derived, not carried: terrain's cave model has no aperture
/// count, so the plural lives here at the composition root.
///
/// # IT IS NO LONGER A FREE DRAW, AND THAT IS AMENDMENT E.2's WHOLE COST
///
/// The aperture set is the **union of two sets**, deduplicated by index the
/// way [`descent_edges`] dedups its own union of two surjections:
///
/// * a GUARANTEED set — one aperture per branch the top band realizes, so
///   that every top-band branch is named by a door (E.2); and
/// * a FREE set, whose size is the weighted draw this leg has always taken:
///   70% one aperture, then 20% / 7% / 3%.
///
/// The first `width` indices serve both sets at once, so a system opens
/// `max(free, width)` apertures. A system with fewer apertures than top-band
/// branches could not satisfy E.2 at all, which is why the count can no
/// longer be independent of the width.
///
/// The weights are unchanged and are still the initial authoring choice —
/// they were not fitted to land a distribution and must not be retuned to
/// move one — but the SHIPPED distribution is now those weights taken
/// pointwise-maximum against [`crate::character::branch_count_of`]'s, so
/// `some_system_draws_more_than_one_entrance` and the panel artifact measure
/// the composition rather than the weights alone.
///
/// Keyed on the SYSTEM's vertex alone under [`crate::streams::ENTRANCE_COUNT`]
/// — no entrance index, because the count is a fact about the system as a
/// whole and an entrance index cannot be named before this draw answers.
///
/// **Epoch v2** (spec E.4): the leg's answer is the FREE set's size, not the
/// system's aperture count — a different quantity under an unchanged key,
/// which is exactly the case an epoch exists to record. See
/// [`crate::streams::ENTRANCE_COUNT`].
/// type-audit: bare-ok(count: return)
pub fn entrance_count(seed: Seed, vertex: Vertex) -> u8 {
    aperture_count_at(seed, vertex, top_band_width(seed, vertex))
}

/// The top band's drawn branch width — the ONE read [`entrance_count`] and
/// [`entrance_mouth`] share, so the two can never disagree about how many
/// branches there are to cover.
fn top_band_width(seed: Seed, vertex: Vertex) -> u8 {
    crate::character::branch_count_of(seed, vertex, top_band())
}

/// [`entrance_count`] at an **explicitly given** top-band width — the seam
/// spec amendment E.2's guarantee is asserted through, exactly as
/// [`descent_edges`] takes both band widths as parameters rather than
/// reading them. A guarantee constructed over every width the lattice admits
/// is a guarantee; the same guarantee found absent on a seed panel is
/// evidence about that panel (spec §6).
fn aperture_count_at(seed: Seed, vertex: Vertex, width: u8) -> u8 {
    let r = seed
        .derive(crate::streams::ENTRANCE_COUNT)
        .derive(StreamLabel::dynamic(&format!("{}", vertex.0)))
        .stream()
        .next_f64();
    let free = if r < 0.70 {
        1
    } else if r < 0.90 {
        2
    } else if r < 0.97 {
        3
    } else {
        4
    };
    // The union, deduplicated by index: the first `width` apertures are both
    // the guaranteed set and the head of the free set, so the system opens
    // whichever of the two is larger. No loop, no repair, no order.
    free.max(width)
}

/// Which level of the system's lattice one entrance opens into (C.3).
///
/// **Entrance 0 is the main line's head by definition** — `EntranceMouth`
/// `{ branch: 0, band: 0, floor: 0 }` — with no draw at all:
/// it is the aperture `windows/vessel`'s `delve_at` descends through, and
/// making it literal rather than drawn pins the primary entrance at the
/// address every existing caller already uses.
///
/// **EVERY MOUTH NOW LANDS IN THE TOP HABITATION BAND, AT LEVEL 0** (The
/// Drift, spec §4.6). A door is a hole in the ground: it opens on the
/// shallowest thing under it, and everything deeper is reached by walking.
///
/// **AND EVERY BRANCH OF THAT BAND IS NAMED BY ONE** (Task 7b, spec
/// amendment E.2). Apertures `0..width` share out the band's `width`
/// branches bijectively — index 0 takes the head, the rest draw from what is
/// left ([`shared_out_branch`]) — and [`entrance_count`] never returns fewer
/// than `width`, so the mouth-to-branch map is SURJECTIVE by construction.
/// Apertures beyond `width` draw a side branch freely on top, uniform over
/// `1..width`; a width-one band has no side branch at all, so its extra doors
/// fall back to the head — two doors into the same hall, a legitimate C.3
/// reading.
///
/// The alternative — draw freely, then add mouths until every branch is
/// covered — is rejected for E.3's reason and §4.5's: a repair pass's answer
/// depends on the order the shortfalls were noticed in, which is a
/// determinism hazard before it is an inelegance.
///
/// # THIS RETIRES A LIVE DEFECT, AND THE DEFECT WAS HALF OF ALL SIDE DOORS
///
/// Until this task the branch was picked against [`Band::Undercroft`]'s
/// drawn width and the mouth was then landed by `root_floor_of` at a band
/// drawn **uniform over every realized band** — a different band, with a
/// different width. So a mouth could name a branch its own landing band did
/// not realize. Measured across the three-seed panel before this change:
/// **47.8% / 53.0% / 51.9% of drawn side-branch mouths** named such a
/// branch. It was benign only because [`chamber_exists`] refused the address
/// downstream, turning it into a closed door — a plausible number in a
/// committed artifact, produced by a category error.
///
/// **The question is now unaskable by construction**, which is the point of
/// fixing it here rather than gating it downstream: the branch is drawn
/// against the width of the band the mouth lands in, because there is only
/// one band it can land in. `a_drawn_mouth_names_a_branch_its_landing_band_
/// realizes` asserts it directly, and `drift_reach_probe` re-measures the
/// panel share that used to be ~50%.
///
/// **The top band is read from the ladder** ([`Band::habitation`]), never
/// written as a literal: a sixth rung, or a rung inserted above the
/// Undercroft, moves every door without an edit here.
///
/// Keyed on the entrance's place `(vertex, entrance index)` under
/// [`crate::streams::ENTRANCE_MOUTH`] (decision 0102), **with the role that
/// place is playing appended** — see [`mouth_key`].
///
/// **Epoch v2** (spec E.4): the key gained that role word and the
/// sharing-out draw ranges over a shrinking pool rather than over the side
/// branches, so both the key and the meaning moved. Every door in every
/// world moves with them.
/// type-audit: bare-ok(index: entrance)
pub fn entrance_mouth(seed: Seed, vertex: Vertex, entrance: u8) -> EntranceMouth {
    EntranceMouth {
        branch: aperture_branch_at(seed, vertex, entrance, top_band_width(seed, vertex)),
        band: rung_rank(top_band()).expect("a habitation band always has a rank"),
        floor: 0,
    }
}

/// [`entrance_mouth`]'s branch at an **explicitly given** top-band width —
/// [`aperture_count_at`]'s twin, and the other half of the seam E.2's
/// guarantee is constructed over.
///
/// The two aperture sets [`entrance_count`] unions are separated here by
/// index, and nothing else decides which is which:
///
/// * `0` is the main line's head by definition, with no draw;
/// * `1..width` are the GUARANTEED apertures, which share out the remaining
///   side branches one each ([`shared_out_branch`]) — so apertures
///   `0..width` name branches `0..width` bijectively, and E.2 holds by
///   construction rather than by a coverage check;
/// * `width..` are the FREE apertures, each drawing a side branch on its
///   own, independently and possibly onto a branch another door already
///   opens on.
fn aperture_branch_at(seed: Seed, vertex: Vertex, entrance: u8, width: u8) -> u8 {
    if entrance == 0 || width <= 1 {
        // Entrance 0 is the main line's head by definition; and where no
        // side branch exists to open into, every other door joins it — a
        // width-one band is entered by the head alone, which satisfies E.2
        // trivially.
        return 0;
    }
    if entrance < width {
        return shared_out_branch(seed, vertex, entrance, width);
    }
    let picked = seed
        .derive(crate::streams::ENTRANCE_MOUTH)
        .derive(StreamLabel::dynamic(&mouth_key(
            vertex,
            entrance,
            ApertureRole::Free,
        )))
        .stream()
        .range_u32(1, u32::from(width - 1));
    u8::try_from(picked).expect("range_u32(1, width-1) fits a u8")
}

/// Which side branch one of the GUARANTEED apertures takes: aperture
/// `1..width` draws from the branches `1..width` that the apertures before it
/// have not already spoken for.
///
/// **This is a permutation, drawn — not an assignment, and not a repair.**
/// Which door opens on which branch is still a fact about the world; what is
/// no longer possible is for two doors to take the same branch and leave a
/// third unentered. The draw ORDER is the aperture INDEX, a fixed coordinate
/// of the lattice, so the answer for one aperture is the same whoever asks
/// and in whatever order (decision 0102) — unlike a draw-then-patch, whose
/// answer would depend on which shortfall a repair pass noticed first.
///
/// The pool cannot run dry: it starts at `width - 1` branches and the caller
/// guarantees `entrance < width`, so step `e` still has `width - e >= 1`
/// left. `expect` states that rather than masking it.
fn shared_out_branch(seed: Seed, vertex: Vertex, entrance: u8, width: u8) -> u8 {
    debug_assert!(
        entrance >= 1 && entrance < width,
        "shared_out_branch is for the guaranteed apertures only"
    );
    let mut pool: Vec<u8> = (1..width).collect();
    let mut taken = 0u8;
    for e in 1..=entrance {
        let last = u32::try_from(pool.len())
            .expect("the pool is at most BRANCHES_PER_SYSTEM wide")
            .checked_sub(1)
            .expect("entrance < width leaves a branch unspoken-for at every step");
        let picked = seed
            .derive(crate::streams::ENTRANCE_MOUTH)
            .derive(StreamLabel::dynamic(&mouth_key(
                vertex,
                e,
                ApertureRole::Share,
            )))
            .stream()
            .range_u32(0, last);
        let index = usize::try_from(picked).expect("range_u32(0, len-1) indexes the pool");
        taken = pool.remove(index);
    }
    taken
}

/// Which of the two questions one aperture's mouth draw asks — the word its
/// key ends with, the discipline [`DescentRole`] already applies to a band
/// transition.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ApertureRole {
    /// One of the apertures sharing out the top band's branches one each:
    /// the draw indexes the branches not yet spoken for.
    Share,
    /// An aperture beyond that sharing-out: the draw names any side branch,
    /// freely and independently.
    Free,
}

impl ApertureRole {
    /// The word this role is spelled by in an entrance-mouth key.
    fn word(self) -> &'static str {
        match self {
            ApertureRole::Share => "share",
            ApertureRole::Free => "free",
        }
    }
}

/// The one place the `chamber/entrance-mouth/v2` key is spelled — the
/// aperture's place (vertex, index) with its role appended, and its own
/// save-format contract.
///
/// **The role is a question, not an ordinal** — the same argument
/// [`descent_key`] makes. The two draws range over different populations (a
/// shrinking pool of unspoken-for branches; every side branch), and whether
/// a given index asks one or the other depends on the top band's width, so
/// one key answering both would mean one stream serving two questions at two
/// widths. Spelling the role out makes them different keys instead.
fn mouth_key(vertex: Vertex, entrance: u8, role: ApertureRole) -> String {
    format!("{}/{entrance}/{}", vertex.0, role.word())
}

/// The shallowest habitation band — where every entrance lands (spec §4.6).
///
/// Read off [`Band::habitation`] rather than named, so the ladder is the
/// single source of truth for its own top: this campaign's signature defect
/// is a bound restated as a literal, and "the top band" is a bound.
/// `habitation()` is ordered shallow-to-deep by [`Band`]'s derived `Ord`
/// (see its own doc), and is never empty.
fn top_band() -> Band {
    *Band::habitation()
        .first()
        .expect("the habitation ladder is never empty")
}

/// Whether a chamber exists at `addr`, under `cave`'s measured depth
/// budget in this vertex. Sparse and derived: no chamber is ever stored, so
/// "exists" is a per-address predicate — **true for every address the
/// lattice's own shape admits** (spec §4.1's keystone: "there is no level 7
/// that does not exist"), gated so `addr.band` reaches no deeper on the delve
/// ladder than the cave's budget does (spec §4.0's metre budget, classified
/// by spec §4.1's ladder).
///
/// **The Drift (spec §4.1) deleted the existence coin this used to end
/// with.** Steps 1-5 below already specified a fully contiguous shape —
/// branches `0..drawn`, bands `0..deepest`, floors `0..drawn length` — and a
/// sixth, fixed-probability draw punched random holes through it. There was
/// nothing left to tune in the passage graph (spec §3.2: connectivity was
/// never randomized), only a spurious draw to remove.
///
/// **`gradient` is the vertex's own geothermal gradient**, and it is what makes
/// this a question about a *place* rather than about a length. A 480 m budget
/// is the Deeps under a 24 K/km vertex and the Shallows under a 15 K/km one, so
/// the same cave reaches a different distance down the lattice depending on
/// where it is. Callers get it from
/// `hornvale_terrain::GeneratedTerrain::geothermal_gradient_at`.
///
/// An out-of-lattice `branch` (`>= BRANCHES_PER_SYSTEM`) or `level`
/// (`>= LEVELS_PER_BRANCH_CEILING`) never exists — the lattice is fixed-size,
/// and an address outside it names nowhere. Likewise `Band::Surface`: it is a
/// rung of the delve ladder but not a *habitation* rung, and has no position
/// in a lattice of underground places.
///
/// **The branch axis carries the same pair** (The Stope, Task 3, spec
/// amendment C.1): [`BRANCHES_PER_SYSTEM`] sizes the address space — an
/// out-of-lattice branch is refused above — and
/// [`crate::character::branch_count_of`] draws how many of those columns
/// THIS system realizes. A branch at or past its system's drawn count is
/// refused here rather than merely unqueried, so the count is enforced by
/// the shipped existence path and not just reported by the accessor: the
/// identical lattice-ceiling/drawn-realization split the level gates below
/// implement for runs.
///
/// **Task 5 removed the transitional literal `0` this doc used to describe**
/// (amendment A.3 — Task 4/5 coupling). `branch_count_of` is now keyed on
/// `(vertex, band)`, so this gate reads `addr.band` directly: a branch is
/// admitted only up to however many that SPECIFIC band's own draw realized,
/// not a single system-wide width. This is what lets one system be two
/// branches wide in the Undercroft and one wide in the Shallows — the same
/// address, checked at two different bands, can now disagree about whether
/// a given branch exists at all.
///
/// **There are TWO level gates now, and the pair is the point** (The Stope,
/// Task 2). [`LEVELS_PER_BRANCH_CEILING`] says which levels the address space
/// admits *at all* — refusing past it is what stops an address outside the
/// lattice from silently deriving a chamber. [`levels_in_branch`] says which
/// of those admitted levels *this* run realizes, and refusing past that is
/// what makes a run a distribution rather than a constant. Neither replaces
/// the other: the first is the lattice's size and the second is a draw, and
/// letting the draw resize the lattice is the defect decision 0102 exists to
/// prevent.
///
/// **Level 0 is admitted by every run that exists at all**, because every
/// band's frozen range has a minimum of at least 1. That is worth knowing
/// before reading any level-0 measurement as evidence about this gate: the
/// heavy readouts that slice level 0 are byte-identical across Task 2 by
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
    if addr.level >= LEVELS_PER_BRANCH_CEILING {
        return false;
    }
    // `Surface` is not a habitation band and has no position in this
    // lattice — representable now that `band` is a typed `Band`, where the
    // old bare-`u8` rank had no way to name it at all.
    if addr.band == Band::Surface {
        return false;
    }
    // C.1's drawn realization: past THIS BAND's drawn branch width, no
    // chamber exists at this band/level of the column (The Drift, Task 5 —
    // the count is now a fact about `(system, band)`, not the system alone).
    // Branch 0 is always inside the count at every band
    // (`branch_count_of` draws 1..=BRANCHES_PER_SYSTEM), so the main line
    // survives this gate everywhere.
    if addr.branch >= crate::character::branch_count_of(seed, addr.vertex, addr.band) {
        return false;
    }
    // `Band`'s derived `Ord` orders shallow -> deep (see its own doc), so
    // "past the cave's budget" is a direct comparison now — no rank lookup
    // needed. `rung_at_depth` never returns `Surface`.
    let deepest = rung_at_depth(cave.depth_reach_m, gradient);
    if addr.band > deepest {
        return false;
    }
    if addr.level >= levels_in_branch(seed, addr.run()) {
        return false;
    }
    // was: chamber_stream(seed, addr).next_f64() < EXISTENCE_DENSITY
    true
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
/// off the vertex's own column at the depth the rung begins ([`stratum_at`]),
/// so it depends on `gradient` and `column` and varies between vertices that
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
    // `chamber_exists` has already refused `Band::Surface`, and `addr.band`
    // is now the rung directly — no rank lookup needed.
    let rung = addr.band;
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
/// of `(seed, cave, addr)`.
///
/// # THE LATTICE IS A LADDER NOW, NOT A CORRIDOR (The Drift, spec §4.6)
///
/// **The lateral `branch ± 1` rule is DELETED, and its deletion is the point,
/// not a casualty.** It joined branch *b* to branch *b ± 1* at the same band
/// and level, which made the branch axis a corridor of adjacent rooms. After
/// spec §4.4 a branch is not a neighbour of the branch beside it: branches
/// at one band are **alternatives** — this system's two ways down from the
/// Undercroft — and their indices are lattice coordinates, not positions in
/// a row. A sideways step between two named places at the same depth is a
/// different feature (a level's interior connectivity) that nobody asked for
/// and that no draw in this campaign describes, and inheriting it from the
/// pre-Drift address space would have made "how much of a system is
/// reachable" answer a question about an accident.
///
/// So **every passage is one step of the vertical sequence**, and there are
/// exactly two shapes of step:
///
/// - **inside a run** — level `f` joins level `f ± 1` of the same
///   `(vertex, band, branch)`, while the run's drawn length
///   ([`levels_in_branch`]) has levels left;
/// - **across a band seam** — a run's **bottom** level joins **level 0** of
///   each branch [`descents_from`] names in the band below, and level 0 of a
///   run joins the **bottom** level of each branch of the band above that
///   descends into it.
///
/// A branch's drawn length is its sojourn time (The Stope, amendment C.4);
/// which branch the seam lands on is The Drift's own drawn quantity (spec
/// §4.5). **Descent happens only at a branch's bottom level**, never
/// mid-run, and it arrives only at a top level, never at an arbitrary one.
///
/// # WHY SYMMETRY STILL HOLDS BY CONSTRUCTION
///
/// The two seam directions read **the same edge set** — [`descents_from`]
/// filters it on the upper coordinate and `ascents_from` on the lower one —
/// so `A` offers `B` across a seam exactly when `B` offers `A` back, with
/// nothing stored and nothing to keep in sync. The in-run directions are
/// exact mirrors for the same reason they always were. The levels agree too,
/// and not by coincidence: the seam's upper endpoint is *defined* as its
/// run's last realized level, which is the same number
/// [`levels_in_branch`] answers whichever end asks.
///
/// **A future edit that makes adjacency depend on anything other than the
/// two addresses themselves — which chambers happen to exist, a generation
/// order, which one was asked first — re-creates the exact problem this
/// function exists to dissolve.** If you are tempted to special-case a
/// direction, that temptation is the bug. In particular: do not compute the
/// upward seam from a second draw. It must be the descent edges read
/// backwards, or the two ends are two derivations again.
///
/// **Neither axis wraps**, and the branch axis no longer connects at all.
/// `band` cannot wrap — there is no rung above [`Band::Surface`] or below
/// [`Band::Nadir`] — and `Band::deeper`/`Band::shallower` return `None`
/// there, which is the typed replacement for the old `addr.band ± 1`
/// arithmetic's implicit bound. End bands simply have fewer neighbours,
/// which is the ordinary edge-of-space behaviour a bounded lattice should
/// have.
///
/// **The rock, not the ladder, has the last word.** [`descents_from`] knows
/// only the drawn branch widths; whether the band below is inside this
/// cave's depth budget is [`chamber_exists`]'s question, and it is asked
/// here, on every candidate, by the `retain` at the end. "No descent at the
/// deepest band the rock allows" is a property of that composition, not of
/// the draw.
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

    // DOWN one step of the sequence: the next level of this run while the
    // run has levels left, else — at the run's bottom — level 0 of every
    // branch the band-transition draw names below (spec §4.5). `addr`
    // passed `chamber_exists`, so `levels >= 1` and `addr.level <= levels - 1`;
    // the `else` arm is therefore exactly the bottom level.
    let levels = levels_in_branch(seed, addr.run());
    if addr.level + 1 < levels {
        candidates.push(ChamberAddr {
            level: addr.level + 1,
            ..addr
        });
    } else if let Some(deeper) = addr.band.deeper() {
        for to in descents_from(seed, addr.vertex, addr.band, addr.branch) {
            candidates.push(ChamberAddr {
                band: deeper,
                branch: to,
                level: 0,
                ..addr
            });
        }
    }

    // UP one step, the exact mirror: the previous level of this run, else —
    // at level 0 — the BOTTOM level of every branch above that descends into
    // this one, read off the same edge set from the other end.
    if addr.level > 0 {
        candidates.push(ChamberAddr {
            level: addr.level - 1,
            ..addr
        });
    } else if let Some(shallower) = addr.band.shallower() {
        for from in ascents_from(seed, addr.vertex, addr.band, addr.branch) {
            let parent = RunAddr {
                vertex: addr.vertex,
                branch: from,
                band: shallower,
            };
            let parent_levels = levels_in_branch(seed, parent);
            // A run above that drew no levels has no bottom level to arrive
            // from; guarded here rather than let a `0 - 1` underflow. The
            // candidate would be refused by `chamber_exists` regardless.
            if parent_levels > 0 {
                candidates.push(ChamberAddr {
                    band: shallower,
                    branch: from,
                    level: parent_levels - 1,
                    ..addr
                });
            }
        }
    }

    candidates.retain(|&candidate| chamber_exists(seed, cave, gradient, candidate));
    candidates
}

// --- Junctions: the underworld becomes a network (The Stope, Task 6) ---
//
// `MAP-underworld-shortcut`: "hard to enter, easy to traverse once inside;
// two points far apart on the surface can be close below." Before this
// function that was false by construction — one cave never connected to
// another.

/// Whether a junction joins `addr`'s system to ANOTHER cave system — the
/// chambers in other systems reachable from this one at its band (The Stope,
/// Task 6; spec's `MAP-underworld-shortcut`).
///
/// **THE CONSTRAINT: a junction is DERIVED, never drawn.** The body consumes
/// no stream leg of its own. Every input it reads is a fact already in the
/// world — vertex adjacency from the geosphere, each vertex's cave budget and
/// geothermal gradient, each branch's already-drawn character, each
/// endpoint's already-gated existence ([`chamber_exists`], which travels its
/// own pre-existing address key). Same inputs give the same links with no
/// new draw anywhere, so a shortcut is a fact about the geology rather than
/// a die roll on top of an epoch, and re-cutting a stream label can never
/// relocate one. The `seed` parameter stays for exactly that reason: it keys
/// the FACTS the derivation reads (`chamber_exists`, `character_at`), not
/// any draw of its own.
///
/// **The derivation rule**, stated once so it cannot drift into a second
/// copy: two systems are joined at a shared delve band **on a shared
/// branch** exactly when
///
/// 1. both vertices are cave-bearing — and being LAND vertices is *entailed by*
///    that, not gated beside it:
///    [`hornvale_terrain::GeneratedTerrain::cave_at`] answers `None` for an
///    ocean vertex as its first act, so an ocean system has no cave to join
///    with, let alone a surface one can walk in from. This clause read as
///    two independent gates for one review round, and the second one was
///    dead code the whole time (Task 6, review round 1),
/// 2. the two vertices are adjacent on the geosphere,
/// 3. both systems realize an existing chamber at the shared `(band,
///    branch)` — canonical endpoints `(addr.branch, level 0)`, gated by
///    [`chamber_exists`] under each vertex's OWN cave and gradient, and
/// 4. the characters of both those branches can occupy the shared rung:
///    [`crate::character::bands_of`] of each side contains the rung at
///    [`ChamberAddr::band`]. A drow-tier Underdeep may open into wild cave;
///    it does not open into fungus gardens three rungs up. Compatibility as
///    shared eligibility is the weakest rule that still makes a character
///    matter — equal characters would couple two independent draws, and no
///    compatibility at all would make the character axis invisible to the
///    network.
///
/// # THE SCOPE IS `(band, branch)` NOW, NOT `band` ALONE (The Drift, §4.6)
///
/// It used to project every address of a system onto its MAIN LINE — branch
/// 0 — so a junction was a fact about `(vertex, band)` and every branch of a
/// system at one band stood on the same far side of the same doors. That
/// reading was correct while the branch axis was a corridor: with the
/// lateral `branch ± 1` rule live, anyone at any branch could walk to branch
/// 0 and use its door, so attributing the door to the main line lost
/// nothing.
///
/// [`passages_from`] no longer has that rule. Branches at one band are
/// alternatives, reachable only through the bands above and below, so a
/// junction attributed to branch 0 would be a door **the walker cannot
/// get to** from branch 2 — and, worse, would hand a traversal a link
/// between two systems that neither side can enter. So the rule is now
/// stated over `(vertex, band, branch)`: a junction joins branch *b* of one
/// system to branch *b* of its neighbour, at the same band, and the two
/// systems' other branches are joined only if their own `(band, branch)`
/// pair satisfies the rule in its own right.
///
/// **Endpoints are CANONICAL, which is what makes symmetry hold by
/// construction** rather than by agreement between two derivations — the
/// exact defect spec §3.2 named the "one genuinely hard problem" and
/// [`passages_from`] dissolved for the intra-system graph. **The re-scoping
/// does not weaken that argument, and this is the sentence to check if you
/// change it again**: the predicate is still stated over an UNORDERED PAIR
/// of canonical endpoints, and `branch` enters both endpoints identically
/// (it comes from the asking address and is copied to the far side
/// unchanged), so the four clauses read the same from either end.
/// `junctions_at(A)` names `B` if and only if `junctions_at(B)` names `A`,
/// with nothing stored and nothing to keep in sync. A rule that took the
/// branch from one side and something else from the other — the far
/// system's own main line, say — would be two derivations again, and the
/// symmetry test would be catching a defect rather than confirming a
/// construction.
///
/// **A junction never crosses a band, and never crosses a branch**: every
/// answer sits at precisely `addr.band` and `addr.branch`. Two systems meet
/// where their depths overlap or not at all — anything else would be a
/// vertical teleport, and the depth ladder would mean nothing.
///
/// **`addr`'s `level` is ignored by the PROJECTION, and `branch` no longer
/// is** (The Drift, Task 7; the `branch` half of this note is what changed).
/// Which junctions this address has depends on `(addr.vertex, addr.band,
/// addr.branch)`; every level of one run stands on the same far side of the
/// same doors, so the answer is projected onto level 0 and `level` never
/// reaches it. But *whether* there is anyone standing there to ask is a
/// question about `addr` itself, and this function gates on it exactly as
/// [`passages_from`] does: **a non-existent `addr` has no junctions**,
/// because there is nothing to traverse from nowhere. The first version
/// answered a `branch: 99` address with three junctions while `passages_from`
/// answered it with no passages, and a consumer composing the two into one
/// traversal graph would have inherited that disagreement.
pub fn junctions_at(seed: Seed, terrain: &GeneratedTerrain, addr: ChamberAddr) -> Vec<ChamberAddr> {
    // Past the habitation ladder there is no shared rung to stand on and no
    // character can be eligible for it. `addr.band` is the rung directly now
    // (a typed `Band`), so `Surface` is the one value to refuse rather than
    // a rank lookup returning `None`.
    if addr.band == Band::Surface {
        return Vec::new();
    }
    let rung = addr.band;
    // `cave_at` refuses an ocean vertex before anything else it does, so this
    // is the land gate as well as the cave gate — a second `is_ocean` test
    // beside it could never fire.
    let Some(here_cave) = terrain.cave_at(addr.vertex) else {
        return Vec::new();
    };
    let here_gradient = terrain.geothermal_gradient_at(addr.vertex);
    // Nowhere has no junctions, matching `passages_from`'s convention for
    // the intra-system graph. This gate is about the ASKING address — its
    // branch and level included — and is why a `branch: 99` address answers
    // with nothing.
    if !chamber_exists(seed, &here_cave, here_gradient, addr) {
        return Vec::new();
    }
    let here = ChamberAddr { level: 0, ..addr };
    // ...and this one is about the CANONICAL ENDPOINT the answer is
    // projected onto — this branch's own level 0, not the main line's (The
    // Drift, Task 7). That is what makes symmetry hold by construction. Both
    // are required: neither implies the other away from the canonical
    // address.
    if !chamber_exists(seed, &here_cave, here_gradient, here) {
        return Vec::new();
    }
    if !crate::character::bands_of(crate::character::character_at(seed, here)).contains(&rung) {
        return Vec::new();
    }

    let mut joined = Vec::new();
    for &neighbour in terrain.geosphere().neighbors(addr.vertex) {
        // No `is_ocean` test here either, for the same reason as above: it
        // was strictly redundant with `cave_at`'s own first gate.
        let Some(cave) = terrain.cave_at(neighbour) else {
            continue;
        };
        // The far endpoint carries the ASKING branch unchanged — the one
        // thing that keeps the predicate symmetric in its two arguments.
        let there = ChamberAddr {
            level: 0,
            vertex: neighbour,
            ..addr
        };
        if !chamber_exists(
            seed,
            &cave,
            terrain.geothermal_gradient_at(neighbour),
            there,
        ) {
            continue;
        }
        if !crate::character::bands_of(crate::character::character_at(seed, there)).contains(&rung)
        {
            continue;
        }
        joined.push(there);
    }
    // Deterministic answer order regardless of the geosphere's neighbour
    // ordering; `Vertex` is an integer newtype, so `sort_by_key` is total.
    joined.sort_by_key(|a| a.vertex);
    joined
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
    /// **These strings have moved three times.** The Underworld (`chamber/v2`)
    /// re-pointed `addr.band` from the stratigraphic ladder at the delve
    /// ladder, so rank 2 spells `deeps` where it used to spell `basement`.
    /// The Stope (`chamber/v3`) added `floor` and renamed `slot` to `branch`,
    /// which changed both the arity and the field order of the key. **The
    /// Drift dropped `entrance`** (amendment A.3) — a real re-keying of this
    /// formatter's spelling, but not an epoch (amendment A.6: nothing derives
    /// from `chamber_key` any more, so `chamber/v3` stays). The old values are
    /// kept in this comment rather than deleted, because a reader arriving at
    /// a failing assertion needs to be able to tell "the address changed" from
    /// "someone broke the formatter":
    ///
    /// ```text
    ///           before v2            v2                    v3                    The Drift
    ///   first   "9/0/basement/3"     "9/0/deeps/3"         "9/0/3/deeps/0"       "9/3/deeps/0"
    ///   second  "0/1/regolith/0"     "0/1/undercroft/0"    "0/1/0/undercroft/0"  "0/0/undercroft/0"
    /// ```
    #[test]
    fn the_chamber_key_spelling_is_pinned() {
        assert_eq!(
            chamber_key(ChamberAddr {
                vertex: Vertex(9),
                branch: 3,
                band: Band::Deeps,
                level: 0,
            }),
            "9/3/deeps/0"
        );
        assert_eq!(
            chamber_key(ChamberAddr {
                vertex: Vertex(0),
                branch: 0,
                band: Band::Undercroft,
                level: 0,
            }),
            "0/0/undercroft/0"
        );
        // A level other than 0, so the pin covers the axis The Stope added
        // rather than only its zero value.
        assert_eq!(
            chamber_key(ChamberAddr {
                vertex: Vertex(9),
                branch: 3,
                band: Band::Deeps,
                level: 7,
            }),
            "9/3/deeps/7"
        );
    }

    /// The derivation key after amendment A: `entrance` is GONE, because an
    /// entrance is an aperture into a system and not a coordinate within it.
    /// This MOVES every chamber in every world's key SPELLING — but not its
    /// derivation (amendment A.6: `chamber_key` derives nothing any more).
    #[test]
    fn the_chamber_key_drops_the_entrance_and_names_the_band() {
        let addr = ChamberAddr {
            vertex: Vertex(31942),
            band: Band::Deeps,
            branch: 2,
            level: 3,
        };
        assert_eq!(chamber_key(addr), "31942/2/deeps/3");
    }

    /// The band is spelled by NAME, never by index — rule 1a one level down.
    /// A numeral here would mean that inserting a [`Band`] variant
    /// mid-ladder silently moved every chamber below it to a different stream.
    #[test]
    fn the_key_names_its_rung_rather_than_numbering_it() {
        // `branch` and `level` are deliberately chosen NOT to equal the band
        // index: they are spelled decimal and legitimately so, and a value
        // collision would make the second assertion below fire for the wrong
        // reason.
        let key = chamber_key(ChamberAddr {
            vertex: Vertex(7),
            branch: 1,
            band: Band::Underdeep,
            level: 2,
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
        for &band in Band::habitation() {
            let key = chamber_key(ChamberAddr {
                vertex: Vertex(1),
                branch: 0,
                band,
                level: 0,
            });
            for name in stratigraphic {
                assert!(
                    !key.contains(name),
                    "{band:?} spells the stratigraphic band {name:?} in {key:?} — \
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
    ///   under a 15 K/km vertex and 267 m under a 30 K/km one, straddling the
    ///   401 m contact.
    #[test]
    fn the_rung_to_stratum_map_is_many_to_one_and_gradient_dependent() {
        let column = fixture_column();
        let gradient = GeothermalGradient::new(24.0);

        let mapped: Vec<(Band, hornvale_climate::Stratum)> = (0..5u8)
            .filter_map(Band::from_rank)
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
            Band::Deeps,
            GeothermalGradient::new(15.0),
            &column,
        ));
        let hot = stratum_of_band(stratum_at(
            Band::Deeps,
            GeothermalGradient::new(30.0),
            &column,
        ));
        assert_ne!(
            cool, hot,
            "the Deeps sits at 533 m under 15 K/km and 267 m under 30 K/km, \
             which straddle this column's 401 m contact — a `stratum` read \
             from the vertex must differ, and one derived from `addr.band` \
             cannot"
        );
    }

    /// **The key must spell `level`, or two levels of one run derive the same
    /// stream and are the same chamber.** This is the whole reason The Stope's
    /// address change is an epoch rather than an additive field.
    ///
    /// Swept over the entire level axis rather than sampled at two values: a
    /// key that spelled `level` for some values and not others (a key built by
    /// appending `level` only when non-zero, say) would pass a two-value
    /// sample and still collide.
    #[test]
    fn the_key_spells_the_level() {
        let base = ChamberAddr {
            vertex: Vertex(9),
            branch: 3,
            band: Band::Deeps,
            level: 0,
        };
        let keys: std::collections::BTreeSet<String> = (0..LEVELS_PER_BRANCH_CEILING)
            .map(|level| chamber_key(ChamberAddr { level, ..base }))
            .collect();
        assert_eq!(
            keys.len(),
            usize::from(LEVELS_PER_BRANCH_CEILING),
            "two levels of one run share a key, so they are one chamber: {keys:?}"
        );
    }

    /// The key is **injective over the whole lattice** — every address the
    /// lattice admits spells differently from every other. A collision would
    /// silently merge two places into one derived stream, and the epoch that
    /// added two axes is exactly when that becomes possible: `branch` and
    /// `level` are both spelled decimal, so a missing separator or a swapped
    /// pair of fields would alias.
    ///
    /// **No `entrance` axis to sweep** (The Drift, amendment A.3) — before
    /// this campaign the same sweep varied `entrance` too, and a version of
    /// this test that still did would fail for the RIGHT reason now: two
    /// entrances of one system address the SAME lattice, so their keys are
    /// supposed to collide.
    #[test]
    fn the_key_is_injective_over_the_lattice() {
        let mut keys = std::collections::BTreeSet::new();
        let mut count = 0usize;
        for vertex in 0..3u32 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for &band in Band::habitation() {
                    for level in 0..LEVELS_PER_BRANCH_CEILING {
                        count += 1;
                        keys.insert(chamber_key(ChamberAddr {
                            vertex: Vertex(vertex),
                            branch,
                            band,
                            level,
                        }));
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

    // `the_existence_draw_travels_the_chamber_leg_and_is_keyed_on_the_whole_
    // address` (formerly here) asserted that the SHIPPED existence draw
    // travelled `crate::streams::CHAMBER`, keyed on the whole address, by
    // re-deriving `chamber_stream`'s own computation inline and comparing it
    // against `chamber_exists`'s verdict. The Drift (spec §4.1) deleted that
    // draw and `chamber_stream` with it, so there is no shipped derivation
    // left for this test to witness — it tested the mechanics of a coin flip
    // that no longer exists, not a level's existence, so re-baselining it
    // would have meant reintroducing dead machinery just to keep an assertion
    // green. Deleted rather than adapted. `the_chamber_key_spelling_is_pinned`
    // still pins `chamber_key`'s format, and `the_epoch_label_is_v3_and_v2_is_
    // not_reused` still pins the `CHAMBER` label itself — both survive
    // unchanged, because `chamber_key` and the label are unrelated to the
    // deleted draw.

    /// The existence verdict is **byte-pinned over a known slice of the
    /// lattice** — one literal `u32` per `(branch, band)`, bit `f` set when
    /// level `f` exists. The same discipline
    /// [`the_run_draw_is_byte_pinned_for_known_keys`] carries, and the
    /// cheapest possible witness that ANY part of the existence derivation
    /// moved: the key's spelling, the level ceiling, or the run draw the
    /// level gate reads.
    ///
    /// **Re-baselined a second time by The Drift (spec amendment A.3).**
    /// Task 1 already re-baselined this pin once, when §4.1's existence coin
    /// was deleted and every level `0..levels_in_branch(seed, run)` began
    /// existing unconditionally. The Drift moves it again, and the shape and
    /// the values both moved for different reasons:
    ///
    /// - **The SHAPE**: dropping `entrance` collapses this system's
    ///   per-entrance sublattices into ONE shared lattice, so the 40-entry
    ///   slice this pin used to cover (`entrance 0`'s branch-1-wide lattice,
    ///   then `entrance 1`'s branch-2-wide one) is now a single 20-entry
    ///   lattice with the same branch-1 width, because `chamber_exists`
    ///   passes a transitional literal `0` where `entrance` used to travel
    ///   (see its own doc) — `branch_count_of`'s key is unaffected by that
    ///   literal, since it was already `"{vertex}/0"` for entrance 0.
    /// - **The VALUES did NOT survive, and that is worth stating rather than
    ///   assuming.** `run_key` (`RUN_FLOORS`'s leg) dropped `entrance` from
    ///   its format string entirely — `"{vertex}/{branch}/{band}"` instead of
    ///   `"{vertex}/{entrance}/{branch}/{band}"` — so even the addresses that
    ///   were already at entrance 0 hash to a DIFFERENT string now, and
    ///   `levels_in_branch` draws a different count from it. The masks below
    ///   are therefore genuinely new numbers, not a copy of the old entrance-0
    ///   row: this is exactly what `RUN_FLOORS` going to `/v2` means.
    ///
    /// **The goldens are literal integers, not values re-derived through the
    /// path they claim to pin.** That is the whole difference between a pin
    /// and a restatement: every expression this test names is one the shipped
    /// call reads, and none of them can move both sides of the comparison at
    /// once.
    ///
    /// 20 levels fit a `u32` with room to spare
    /// ([`LEVELS_PER_BRANCH_CEILING`]); a wider lattice would need a wider
    /// mask, and the assertion below says so rather than truncating.
    ///
    /// If this fails, every chamber in every world has moved. That is an
    /// **epoch**, not a fix to these numbers — carried this time by
    /// `RUN_FLOORS` going to `/v2` (spec amendment A.6), not by `CHAMBER`.
    #[test]
    fn the_existence_verdict_is_byte_pinned_over_a_known_lattice_slice() {
        assert!(
            u32::from(LEVELS_PER_BRANCH_CEILING) <= u32::BITS,
            "the lattice admits {LEVELS_PER_BRANCH_CEILING} levels, which no \
             longer fits the u32 masks below — widen the mask type rather \
             than letting the pin silently cover only the first 32 levels"
        );

        let seed = Seed(42);
        let column = fixture_column();
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);

        // Vertex 9's ONE shared lattice (The Drift collapsed every entrance's
        // sublattice into this one). Row-major over (branch 0..4, band
        // 0..5), so each line below is one branch's undercroft / shallows /
        // deeps / underdeep / nadir. Decimal, matching what `assert_eq!`
        // prints on a failure, so the two can be compared by eye.
        //
        // A zero is a legitimate reading, not a hole in the pin. **Re-baselined
        // by The Drift, Task 5**: `branch_count_of` is now keyed on
        // `(vertex, band)`, and this vertex is the demonstration that the width
        // genuinely varies by band — vertex 9 draws width 1 at Undercroft
        // (branch 1's Undercroft column is 0) but a wider count at Deeps and
        // at Underdeep (branch 1's Deeps column is nonzero, and branch 2's
        // Underdeep column is nonzero too), so the SAME system is narrower
        // at one band and wider at another.
        let expected: [u32; 20] = [
            3, 63, 262143, 255, 31, //
            0, 0, 16383, 127, 0, //
            0, 0, 524287, 0, 0, //
            0, 0, 0, 0, 0,
        ];

        let mut got = [0u32; 20];
        for branch in 0..BRANCHES_PER_SYSTEM {
            for (rank, &band) in Band::habitation().iter().enumerate() {
                let mut mask = 0u32;
                for level in 0..LEVELS_PER_BRANCH_CEILING {
                    if chamber_exists(
                        seed,
                        &cave,
                        gradient,
                        ChamberAddr {
                            vertex: Vertex(9),
                            branch,
                            band,
                            level,
                        },
                    ) {
                        mask |= 1u32 << u32::from(level);
                    }
                }
                got[usize::from(branch) * 5 + rank] = mask;
            }
        }
        assert_eq!(
            got, expected,
            "the realized-floor masks of vertex 9's lattice moved off their pin"
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
        // refuses — Undercroft is inside any budget, but a deep cave keeps
        // the fixture honest if a later reader moves the band under test.
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);
        let base = ChamberAddr {
            vertex: Vertex(1),
            branch: 0,
            band: Band::Undercroft,
            level: 0,
        };

        for level in LEVELS_PER_BRANCH_CEILING..=LEVELS_PER_BRANCH_CEILING + 8 {
            assert!(
                !chamber_exists(seed, &cave, gradient, ChamberAddr { level, ..base }),
                "level {level} is outside the lattice \
                 (ceiling {LEVELS_PER_BRANCH_CEILING}) and must name nowhere"
            );
        }

        // Positive control: the gate refuses out-of-range levels and not the
        // whole axis.
        assert!(
            (0..LEVELS_PER_BRANCH_CEILING).any(|level| chamber_exists(
                seed,
                &cave,
                gradient,
                ChamberAddr { level, ..base }
            )),
            "no in-range level exists either, so the refusal above proves \
             nothing about the level gate"
        );
    }

    /// A branch's levels are CONTIGUOUS: within a run's drawn length there is
    /// no gap. This is the campaign's keystone (spec §2), and before The
    /// Drift it was false for about half of every run — a 50% existence coin
    /// punched random holes through a shape steps 1-5 of `chamber_exists`
    /// already made contiguous (spec §3.1).
    #[test]
    fn a_runs_levels_are_contiguous() {
        let seed = Seed(42);
        let column = fixture_column();
        // At the reach ceiling under a 24 K/km vertex, every habitation band is
        // inside budget, so the BAND gate can never be what truncates the
        // run below — only the drawn level count can.
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);
        let base = ChamberAddr {
            vertex: Vertex(31942),
            branch: 0,
            band: Band::Undercroft,
            level: 0,
        };

        let drawn = levels_in_branch(seed, base.run());
        assert!(
            drawn > 1,
            "a one-level run cannot exhibit a gap; pick a longer run"
        );

        for level in 0..drawn {
            assert!(
                chamber_exists(seed, &cave, gradient, ChamberAddr { level, ..base }),
                "level {level} of a {drawn}-level run does not exist — the run \
                 has a hole"
            );
        }
        assert!(
            !chamber_exists(
                seed,
                &cave,
                gradient,
                ChamberAddr {
                    level: drawn,
                    ..base
                }
            ),
            "level {drawn} is past the drawn length and must not exist"
        );
    }

    /// `rung_rank` and [`Band::from_rank`] are one bijection over the
    /// habitation rungs. Kept honest here so the pair cannot drift
    /// half-updated when a sixth [`Band`] lands. (`chamber.rs` no longer
    /// carries its own `rung_of_rank` — The Drift deleted the private
    /// duplicate once every caller could reach the kernel's public
    /// equivalent instead.)
    #[test]
    fn rung_rank_and_rung_of_rank_round_trip() {
        for rung in hornvale_terrain::rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
        {
            let rank = rung_rank(rung).expect("a habitation rung has a rank");
            assert_eq!(Band::from_rank(rank), Some(rung));
        }
        assert_eq!(Band::from_rank(5), None, "the ladder ends at rank 4");
        assert_eq!(
            rung_rank(Band::Surface),
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

    /// Spec §3.1's levels-per-band ranges, **restated as literals** rather
    /// than read from [`levels_range`]. Reading the table the draw itself
    /// reads would make the range assertion below circular — it would pass
    /// for any table at all. These five pairs are the frozen preregistration
    /// (spec §3.1, `Sunless` renamed `Nadir` by amendment B.3); moving one is
    /// a design act, and the campaign's own rule is that they are **not** to
    /// be retuned to make §4.2's readout land in its intended band.
    const FROZEN_RANGES: [(Band, u8, u8); 5] = [
        (Band::Undercroft, 1, 5),
        (Band::Shallows, 3, 10),
        (Band::Deeps, 5, 20),
        (Band::Underdeep, 5, 10),
        (Band::Nadir, 1, 5),
    ];

    /// A run's drawn level count lands inside its own band's frozen range —
    /// the first half of Task 2's claim.
    ///
    /// **The range check alone is satisfiable by a constant**, so the sweep
    /// also demands that every band produce at least two distinct counts.
    /// Every frozen range spans at least three values, so a band that only
    /// ever answers one number is either not drawing or drawing on a key that
    /// does not vary here.
    ///
    /// **No `entrance` axis to sweep** (The Drift, amendment A.3) — a run
    /// belongs to the system's ONE shared lattice now, not to any one
    /// entrance's private view of it.
    /// claim: invariant(forall-seed) — a run's drawn count lies in its band's
    /// frozen range, over a hand-named lattice (seedless sweep, audit §5:
    /// builds no world)
    #[test]
    fn a_runs_level_count_falls_in_its_bands_frozen_range() {
        for (band, lo, hi) in FROZEN_RANGES {
            let mut seen = std::collections::BTreeSet::new();
            for raw_seed in [1u64, 2, 3] {
                let seed = Seed(raw_seed);
                for raw_vertex in 0u32..40 {
                    for branch in 0..BRANCHES_PER_SYSTEM {
                        let run = RunAddr {
                            vertex: Vertex(raw_vertex),
                            branch,
                            band,
                        };
                        let n = levels_in_branch(seed, run);
                        assert!(
                            (lo..=hi).contains(&n),
                            "{band:?}: {run:?} under seed {raw_seed} drew {n} \
                             levels, outside the frozen range {lo}..={hi}"
                        );
                        seen.insert(n);
                    }
                }
            }
            assert!(
                seen.len() > 1,
                "{band:?}: every run drew the same count {seen:?} — a constant \
                 satisfies the range check without drawing anything"
            );
            assert!(
                seen.contains(&lo) && seen.contains(&hi),
                "{band:?}: the sweep saw {seen:?}, which does not reach both \
                 ends of {lo}..={hi} — the draw is not covering its own range"
            );
        }
    }

    /// A run's level count is a pure function of `(seed, run)`: asked twice,
    /// it answers the same, and asking about a *different* run in between
    /// cannot disturb it. The second half matters because a draw that
    /// advanced some shared stream would be deterministic per call sequence
    /// and not per place, which is decision 0102's defect wearing a
    /// deterministic mask.
    #[test]
    fn a_runs_level_count_is_deterministic_for_one_address() {
        let seed = Seed(90210);
        let run = RunAddr {
            vertex: Vertex(9),
            branch: 3,
            band: Band::Deeps,
        };
        let first = levels_in_branch(seed, run);
        for raw_vertex in 0u32..20 {
            for &band in Band::habitation() {
                let _ = levels_in_branch(
                    seed,
                    RunAddr {
                        vertex: Vertex(raw_vertex),
                        branch: 0,
                        band,
                    },
                );
            }
        }
        assert_eq!(
            levels_in_branch(seed, run),
            first,
            "the same run drew a different count after unrelated runs were \
             asked — the draw is keyed on a call order, not on a place"
        );
    }

    /// **Every component of the run key is load-bearing.** Varying exactly one
    /// of `vertex`, `branch`, `band` must be able to change the answer; a
    /// component the key dropped would make its column here constant, and
    /// the whole point of keying on a place is that each coordinate of the
    /// place names a different run.
    ///
    /// **`entrance` is not in this list, and that is the whole point of The
    /// Drift** (amendment A.3) — it is not a run coordinate any more, so
    /// there is nothing left here to vary it against.
    ///
    /// This subsumes the brief's "two different branches in one band may
    /// differ" — that is the `branch` row — and catches the three ways to get
    /// that one row right while dropping another coordinate.
    #[test]
    fn every_component_of_the_run_key_is_load_bearing() {
        let seed = Seed(4242);
        let base = RunAddr {
            vertex: Vertex(0),
            branch: 0,
            band: Band::Deeps,
        };

        let vary_vertex = (0u32..200).any(|c| {
            levels_in_branch(
                seed,
                RunAddr {
                    vertex: Vertex(c),
                    ..base
                },
            ) != levels_in_branch(seed, base)
        });
        assert!(vary_vertex, "`vertex` never changed a run's level count");

        // `branch` has only BRANCHES_PER_SYSTEM values, so one vertex is not
        // enough to be sure two of them differ; sweep vertices until a vertex whose
        // branches disagree turns up. This is the brief's own clause: two
        // different branches in one band may differ.
        let vary_branch = (0u32..200).any(|c| {
            let counts: std::collections::BTreeSet<u8> = (0..BRANCHES_PER_SYSTEM)
                .map(|branch| {
                    levels_in_branch(
                        seed,
                        RunAddr {
                            vertex: Vertex(c),
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
            "no vertex had two branches whose runs differed in length — `branch` \
             is not in the key, so a whole system is one column again"
        );

        let vary_band = Band::habitation().iter().any(|&band| {
            levels_in_branch(seed, RunAddr { band, ..base }) != levels_in_branch(seed, base)
        });
        assert!(vary_band, "`band` never changed a run's level count");
    }

    /// The run key is a **save-format contract**, pinned the same way
    /// [`chamber_key`]'s is, and spelled to the same rules: decimal for the
    /// integers that name a place, the rung's NAME for `band` (never its
    /// index — see [`chamber_key`]'s own doc for why an index is a
    /// declaration position rather than a place).
    ///
    /// If this fails you have re-keyed every run in every world, which
    /// re-decides how many levels every run has. That is an epoch — carried
    /// by `chamber/run-floors/v2` (spec amendment A.6), which is why these
    /// strings no longer spell an `entrance` segment at all.
    #[test]
    fn the_run_key_spelling_is_pinned() {
        assert_eq!(
            run_key(RunAddr {
                vertex: Vertex(9),
                branch: 3,
                band: Band::Deeps,
            }),
            "9/3/deeps"
        );
        assert_eq!(
            run_key(RunAddr {
                vertex: Vertex(0),
                branch: 0,
                band: Band::Undercroft,
            }),
            "0/0/undercroft"
        );
    }

    /// The run key is **injective over the run lattice** — the same guard
    /// [`chamber_key`] carries, for the same reason: two runs sharing a key
    /// would be one run, and every level count in the world would be drawn
    /// half as many times as it looks.
    ///
    /// **No `entrance` axis to sweep** (The Drift, amendment A.3) — see
    /// [`the_key_is_injective_over_the_lattice`]'s own doc for why.
    #[test]
    fn the_run_key_is_injective_over_the_lattice() {
        let mut keys = std::collections::BTreeSet::new();
        let mut count = 0usize;
        for vertex in 0..5u32 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for &band in Band::habitation() {
                    count += 1;
                    keys.insert(run_key(RunAddr {
                        vertex: Vertex(vertex),
                        branch,
                        band,
                    }));
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
    /// [`run_stream`] nor [`levels_in_branch`], so it is a property of the two
    /// label CONSTANTS and says nothing about which parent the shipped draw
    /// actually travels. Review demonstrated the gap with a one-token
    /// mutation — `run_stream`'s `derive(RUN_FLOORS)` to `derive(CHAMBER)`,
    /// which changes every floor count in every world — and the whole crate
    /// stayed green (371 lib tests, 286 suite tests). The assertion that
    /// closes it is
    /// [`the_run_draw_travels_the_run_floors_leg_and_not_the_chamber_leg`],
    /// which starts from `levels_in_branch` instead. Both are kept: this one
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
    /// [`crate::streams::CHAMBER`].** This starts from [`levels_in_branch`] —
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
    /// **The `chamber_stream` counterpart this paragraph used to warn about
    /// no longer exists.** It was the existence draw's own stream, parented
    /// on `crate::streams::CHAMBER` and unpinned in exactly this way; The
    /// Drift (spec §4.1) deleted the draw and the function together, which
    /// closes the gap by removing what it was a gap in, rather than by
    /// pinning it. The lesson stated above (a re-implementing test is not a
    /// witness) still applies to whatever next derivation is parented on
    /// `CHAMBER`.
    #[test]
    fn the_run_draw_travels_the_run_floors_leg_and_not_the_chamber_leg() {
        let seed = Seed(90210);
        let mut disagreed = false;
        let mut probed = 0usize;
        for raw_vertex in 0u32..40 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for &band in Band::habitation() {
                    let run = RunAddr {
                        vertex: Vertex(raw_vertex),
                        branch,
                        band,
                    };
                    let (lo, hi) = levels_range(run.band).expect("a habitation rung has a range");

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

                    let shipped = u32::from(levels_in_branch(seed, run));
                    assert_eq!(
                        shipped, via_run_leg,
                        "{run:?}: levels_in_branch answered {shipped}, but the \
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
    /// **Re-baselined by The Drift** (spec amendment A.6): `RUN_FLOORS` moved
    /// to `chamber/run-floors/v2` because dropping `entrance` from `RunAddr`
    /// re-keys every run, and this is a LIVE production leg — unlike
    /// `CHAMBER`, nothing here is a display formatter. If this fails, every
    /// run in every world is a different length. That is a further epoch, not
    /// a fix to these numbers.
    #[test]
    fn the_run_draw_is_byte_pinned_for_known_keys() {
        let seed = Seed(42);
        for (vertex, branch, band, expected) in [
            (9u32, 3u8, Band::Deeps, 16u8), // 9/3/deeps,      range 5-20
            (0, 0, Band::Undercroft, 4),    // 0/0/undercroft, range 1-5
            (17, 2, Band::Nadir, 2),        // 17/2/nadir,     range 1-5
            (5, 1, Band::Underdeep, 5),     // 5/1/underdeep,  range 5-10
        ] {
            let run = RunAddr {
                vertex: Vertex(vertex),
                branch,
                band,
            };
            assert_eq!(
                levels_in_branch(seed, run),
                expected,
                "the run {} moved off its pin",
                run_key(run)
            );
        }
    }

    /// **The lattice ceiling must be at least as tall as the tallest thing
    /// the draw can ask for**, or a run reports a length the lattice refuses.
    ///
    /// [`chamber_exists`] checks `level >= LEVELS_PER_BRANCH_CEILING` BEFORE
    /// it checks `level >= levels_in_branch(..)`, so a future range with
    /// `hi > 20` would produce runs that claim N levels and realize 20 — a
    /// silent, permanent truncation with no error anywhere. One assertion
    /// closes it, and it belongs beside the table rather than in a reviewer's
    /// head.
    #[test]
    fn the_lattice_ceiling_covers_every_bands_frozen_maximum() {
        for &rung in Band::habitation() {
            let (lo, hi) = levels_range(rung).expect("a habitation rung has a range");
            assert!(
                lo >= 1,
                "{rung:?} may draw {lo} levels, so its run can be empty"
            );
            assert!(
                hi <= LEVELS_PER_BRANCH_CEILING,
                "{rung:?} may draw {hi} levels but the lattice admits only \
                 {LEVELS_PER_BRANCH_CEILING}, so chamber_exists would \
                 truncate the run silently — the ceiling gate runs first"
            );
        }
    }

    /// The run-floors label is a **save-format contract**; `stream_labels!`
    /// cannot tell a bump from a typo, so the literal is asserted here — the
    /// same pin [`crate::streams::CHAMBER`] carries.
    ///
    /// **`/v2` since The Drift** (spec amendment A.6): dropping `entrance`
    /// from `RunAddr` re-keys every run this label derives, and unlike
    /// `CHAMBER` this is a LIVE production leg — see `RUN_FLOORS`'s own doc.
    #[test]
    fn the_run_floors_label_is_v2() {
        assert_eq!(crate::streams::RUN_FLOORS.as_str(), "chamber/run-floors/v2");
        assert_ne!(
            crate::streams::RUN_FLOORS.as_str(),
            "chamber/run-floors/v1",
            "chamber/run-floors/v1 is retired; The Drift re-shaped the run key"
        );
        assert_ne!(
            crate::streams::RUN_FLOORS.as_str(),
            crate::streams::CHAMBER.as_str(),
            "the run draw must not share the chamber leg's label"
        );
    }

    /// **`chamber_exists` refuses a level past its run's drawn count** — the
    /// half of Task 2 that changes what worlds contain. Before this, every
    /// in-budget run admitted all [`LEVELS_PER_BRANCH_CEILING`] levels, so the
    /// realized population was the lattice ceiling wearing a distribution's
    /// clothes.
    ///
    /// Two arms, and the second is a positive control: past the drawn count
    /// **nothing** exists (a hard invariant), and below it **something** does
    /// (or "nothing past the count" would be satisfied by a gate that refused
    /// the whole axis). The control is stated as "at least one run in the
    /// sweep has at least one realized level" rather than per-run, because
    /// existence below the count is still a coin-flip draw.
    #[test]
    fn chamber_exists_refuses_a_level_past_its_runs_drawn_count() {
        let seed = Seed(90210);
        let column = fixture_column();
        // At the reach ceiling, so every habitation band is in budget and the
        // BAND gate can never be what refuses.
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);

        let mut realized_below = 0usize;
        let mut runs_shorter_than_the_ceiling = 0usize;
        for raw_vertex in 0u32..20 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for &band in Band::habitation() {
                    let run = RunAddr {
                        vertex: Vertex(raw_vertex),
                        branch,
                        band,
                    };
                    let drawn = levels_in_branch(seed, run);
                    if drawn < LEVELS_PER_BRANCH_CEILING {
                        runs_shorter_than_the_ceiling += 1;
                    }
                    for level in 0..LEVELS_PER_BRANCH_CEILING {
                        let addr = ChamberAddr {
                            vertex: run.vertex,
                            branch: run.branch,
                            band: run.band,
                            level,
                        };
                        let exists = chamber_exists(seed, &cave, gradient, addr);
                        if level >= drawn {
                            assert!(
                                !exists,
                                "{addr:?} exists, but its run realizes only {drawn} \
                                 levels — the lattice ceiling is standing in for the \
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
            "no run in the sweep drew fewer than {LEVELS_PER_BRANCH_CEILING} \
             levels, so the refusal above never had anything to refuse"
        );
        assert!(
            realized_below > 0,
            "no level below any run's count exists either, so the refusal proves \
             nothing about the run gate"
        );
    }

    /// The levels-per-band table is spec §3.1's, exhaustively, and `Surface`
    /// has no run at all. Stated against the same literals
    /// [`FROZEN_RANGES`] pins, so a silent edit to [`levels_range`] fails
    /// here rather than being absorbed by a range check that reads it.
    #[test]
    fn the_levels_per_band_table_is_spec_3_1s() {
        for (band, lo, hi) in FROZEN_RANGES {
            assert_eq!(
                levels_range(band),
                Some((lo, hi)),
                "{band:?} must carry spec §3.1's frozen range"
            );
        }
        assert_eq!(
            levels_range(Band::Surface),
            None,
            "the overworld is not a run"
        );
    }

    /// Every rung spells differently. A collision would silently merge two
    /// depths' chambers into one derived stream.
    #[test]
    fn every_rung_has_a_distinct_spelling() {
        let names: Vec<&str> = (0..=4).filter_map(Band::from_rank).map(rung_name).collect();
        assert_eq!(names.len(), 5, "every rank 0..=4 must name a rung");
        for (i, a) in names.iter().enumerate() {
            for b in &names[i + 1..] {
                assert_ne!(a, b, "two bands share the spelling {a:?}");
            }
        }
    }

    // --- Task 5: entrances become plural (spec amendment C.3) ---

    /// The entrance-count draw is **non-vacuous**: across a sweep of seeds
    /// and vertices, some system draws MORE than one aperture. A draw that
    /// always answered 1 would make every test below pass and plural
    /// entrances not exist.
    /// claim: rate(seed x vertex sweep) — some system draws >1 aperture
    #[test]
    fn some_system_draws_more_than_one_entrance() {
        let multi = (0u64..8)
            .flat_map(|s| (0u32..60).map(move |c| entrance_count(Seed(s * 1000 + 7), Vertex(c))))
            .any(|n| n > 1);
        assert!(
            multi,
            "no system on the sweep drew more than one entrance — the count \
             draw is degenerate and plural entrances do not exist"
        );
    }

    /// The BG3 case: two entrances of ONE system may open into DIFFERENT
    /// places — one at the main-line head, another on a side branch, per
    /// C.3. Asserted as an existence over a sweep: the draw must be CAPABLE
    /// of disagreement, not always so.
    ///
    /// **Renamed from `..._on_different_floors` by The Drift's Task 7**
    /// (spec §4.6). It is no longer a *floor* the two doors can disagree
    /// about: every mouth lands at level 0 of the top habitation band, so
    /// the only axis left is the BRANCH. The assertion is unchanged (two
    /// mouths of one system differ) and its meaning narrowed with the
    /// design — keeping the old name would have described a disagreement
    /// that can no longer occur.
    /// claim: rate(seed x vertex sweep) — some pair of mouths disagrees
    #[test]
    fn two_entrances_of_one_system_may_open_on_different_branches() {
        let mut found = None;
        'outer: for s in 0u64..40 {
            for c in 0u32..80 {
                let seed = Seed(s * 1000 + 7);
                let vertex = Vertex(c);
                if entrance_count(seed, vertex) < 2 {
                    continue;
                }
                let first = entrance_mouth(seed, vertex, 0);
                let second = entrance_mouth(seed, vertex, 1);
                if first != second {
                    found = Some((seed, vertex, first, second));
                    break 'outer;
                }
            }
        }
        found.expect(
            "no multi-entrance system on the sweep opened its two entrances \
             onto different branches — C.3's two-door reading is unrealizable",
        );
    }

    /// The mapping is deterministic: the same address in, the same floor
    /// out — and it is a real mapping, not a constant (two systems may
    /// differ).
    /// claim: invariant(forall-swept-seed) — same mouth in, same floor out
    #[test]
    fn the_entrance_mapping_is_deterministic_and_varies_by_system() {
        let mut distinct_heads = std::collections::BTreeSet::new();
        for s in 0u64..4 {
            for c in 0u32..30 {
                let seed = Seed(s * 1000 + 7);
                let vertex = Vertex(c);
                let once = entrance_mouth(seed, vertex, 1);
                let twice = entrance_mouth(seed, vertex, 1);
                assert_eq!(
                    once, twice,
                    "entrance 1 of vertex {c} moved between two identical asks"
                );
                distinct_heads.insert(once);
            }
        }
        assert!(
            distinct_heads.len() > 1,
            "every probed system mapped entrance 1 to the same floor — the \
             mapping is a constant, not a derivation"
        );
    }

    /// Entrance 0 is the main line's head by definition, with no draw —
    /// C.3's first reading. It is the same at every vertex, which is what
    /// makes `delve_at`'s pinned `branch = 0, band = 0, floor = 0` descent
    /// the primary entrance.
    /// claim: invariant(forall-swept-seed) — mouth 0 is the head everywhere
    #[test]
    fn entrance_zero_is_the_main_line_head_everywhere() {
        for s in 0u64..3 {
            for c in 0u32..20 {
                assert_eq!(
                    entrance_mouth(Seed(s * 1000 + 7), Vertex(c), 0),
                    EntranceMouth {
                        branch: 0,
                        band: 0,
                        floor: 0
                    },
                    "entrance 0 of vertex {c} is not the main-line head"
                );
            }
        }
    }

    /// The two new legs are byte-pinned for known keys — literal counts and
    /// literal mouths, the cheapest witness that any part of either
    /// derivation moved.
    ///
    /// **Re-baselined by The Drift, Task 5**: `entrance_mouth`'s own
    /// `branch_count_of` query moved from a literal entrance index to
    /// `Band::Undercroft` (see its own doc), which changes the actual
    /// derived stream even where the numeral was textually the same, so
    /// some mouths moved. `entrance_count` is untouched.
    ///
    /// **Re-baselined again by Task 7** (spec §4.6), and the shape of the
    /// movement is worth reading: exactly ONE field of ONE row moved —
    /// vertex 5's `floor: 3` became `floor: 0`. Every `branch` is unchanged,
    /// because the side-branch pick still travels the same leg with the
    /// same key over the same width; what retired is the second half, where
    /// `root_floor_of` then landed the door at a band and floor drawn
    /// independently. A door now opens at level 0 of the top habitation
    /// band, so `band` and `floor` are no longer drawn at all. Vertices 9 and
    /// 31 were already `0/0` (the head, and a width-one fallback), which is
    /// why they do not move.
    ///
    /// **Re-baselined a THIRD time by Task 7b** (spec amendment E.2), and
    /// this time BOTH legs moved, because both took an epoch — a new label
    /// is a new parent seed, so every raw draw is different even where the
    /// dynamic key reads the same. Counts: 1/2/3/1 became 2/4/1/2. Mouths:
    /// vertex 6 is a width-FOUR system, so its three side doors now share out
    /// branches 3, 2 and 1 — a drawn permutation, not an assignment, which
    /// is why aperture 1 does not simply take branch 1. Vertices 31 and 17 are
    /// width-one and keep the head. Vertex 6's aperture 2 is pinned as well as
    /// its aperture 1: a single row could be satisfied by a plain
    /// `branch = entrance` map, and two of them from the same system cannot.
    #[test]
    fn the_entrance_draws_are_byte_pinned_for_known_keys() {
        let seed = Seed(42);
        for (vertex, expected) in [(9u32, 2u8), (6, 4), (17, 1), (5, 2)] {
            assert_eq!(
                entrance_count(seed, Vertex(vertex)),
                expected,
                "vertex {vertex} moved off its entrance-count pin"
            );
        }
        for (vertex, entrance, expected) in [
            (
                9u32,
                0u8,
                EntranceMouth {
                    branch: 0,
                    band: 0,
                    floor: 0,
                },
            ),
            (
                6,
                1,
                EntranceMouth {
                    branch: 3,
                    band: 0,
                    floor: 0,
                },
            ),
            (
                6,
                2,
                EntranceMouth {
                    branch: 2,
                    band: 0,
                    floor: 0,
                },
            ),
            (
                31,
                1,
                EntranceMouth {
                    branch: 0,
                    band: 0,
                    floor: 0,
                },
            ),
            (
                5,
                1,
                EntranceMouth {
                    branch: 1,
                    band: 0,
                    floor: 0,
                },
            ),
        ] {
            assert_eq!(
                entrance_mouth(seed, Vertex(vertex), entrance),
                expected,
                "vertex {vertex} entrance {entrance} moved off its mouth pin"
            );
        }
    }

    /// Each shipped entrance draw travels ITS OWN leg, not a sibling's —
    /// the Task 2/3 pattern. Arm 1: the shipped answer equals its declared
    /// leg everywhere probed. Arm 2: it differs from at least one sibling
    /// leg somewhere, so arm 1 cannot pass under a re-parented draw.
    ///
    /// **Both halves re-derive the COMPOSITION now, not the draw alone**
    /// (Task 7b, spec E.2). The shipped count is the leg's free draw raised
    /// to the top band's width, and the shipped mouth for aperture 1 is an
    /// index into the pool of unspoken-for branches; a witness that
    /// re-derived only the raw draw would be comparing two different
    /// quantities and would fail for a reason that has nothing to do with
    /// which leg the draw travelled.
    #[test]
    fn each_entrance_draw_travels_its_own_leg_and_not_a_siblings() {
        let seed = Seed(90210);
        // --- entrance count ---
        let mut count_disagreed = false;
        for c in 0u32..40 {
            let vertex = Vertex(c);
            let width = crate::character::branch_count_of(seed, vertex, top_band());
            let shipped = entrance_count(seed, vertex);
            let own = seed
                .derive(crate::streams::ENTRANCE_COUNT)
                .derive(StreamLabel::dynamic(&format!("{}", c)))
                .stream()
                .next_f64();
            assert_eq!(
                shipped,
                count_from_raw(own).max(width),
                "entrance_count does not travel the ENTRANCE_COUNT leg at vertex {c}"
            );
            let sibling = seed
                .derive(crate::streams::BRANCH_COUNT)
                .derive(StreamLabel::dynamic(&format!("{}/0", c)))
                .stream()
                .next_f64();
            if count_from_raw(sibling).max(width) != shipped {
                count_disagreed = true;
            }
        }
        assert!(
            count_disagreed,
            "the entrance-count draw agreed with the branch-count leg everywhere"
        );

        // --- entrance mouth ---
        let mut mouth_disagreed = false;
        for c in 0u32..200 {
            let vertex = Vertex(c);
            // A width-two system cannot disagree: its single side branch is
            // the only thing either leg could pick. Only a wider system
            // discriminates the legs. Aperture 1 is a `share` draw at every
            // width above one, and `entrance_count` now guarantees it
            // exists there, so no count filter is needed.
            let width = crate::character::branch_count_of(seed, vertex, top_band());
            if width < 3 {
                continue;
            }
            let shipped = entrance_mouth(seed, vertex, 1);
            let own = seed
                .derive(crate::streams::ENTRANCE_MOUTH)
                .derive(StreamLabel::dynamic(&mouth_key(
                    vertex,
                    1,
                    ApertureRole::Share,
                )))
                .stream();
            assert_eq!(
                Some(shipped),
                mouth_from_raw(seed, vertex, own),
                "entrance_mouth does not travel the ENTRANCE_MOUTH leg at vertex {c}"
            );
            // The sibling leg was `BRANCH_ROOT` until The Drift's Task 7
            // retired that label; `ENTRANCE_COUNT` serves the same purpose
            // — a DIFFERENT root leg read under the same dynamic key, so
            // arm 1 cannot pass by luck under a re-parented draw.
            let sib = seed
                .derive(crate::streams::ENTRANCE_COUNT)
                .derive(StreamLabel::dynamic(&mouth_key(
                    vertex,
                    1,
                    ApertureRole::Share,
                )))
                .stream();
            if Some(shipped) != mouth_from_raw(seed, vertex, sib) {
                mouth_disagreed = true;
            }
        }
        assert!(
            mouth_disagreed,
            "the mouth draw agreed with the entrance-count leg everywhere"
        );
    }

    fn count_from_raw(r: f64) -> u8 {
        if r < 0.70 {
            1
        } else if r < 0.90 {
            2
        } else if r < 0.97 {
            3
        } else {
            4
        }
    }

    /// Re-derives **aperture 1's** mouth from a caller-supplied first-draw
    /// stream — the leg witness's comparison half. Where a mouth lands is not
    /// drawn at all any more (The Drift, Task 7: every mouth opens at level 0
    /// of the top habitation band), so only the branch comes from the stream.
    ///
    /// Aperture 1 is the FIRST of the sharing-out apertures (Task 7b), whose
    /// pool is still the whole side-branch range `1..width` — nothing has
    /// been removed from it yet — so the draw indexes `0..width - 1` and the
    /// branch is one more than the index. That equivalence holds for
    /// aperture 1 alone, which is why this helper takes no aperture index.
    fn mouth_from_raw(
        seed: Seed,
        vertex: Vertex,
        mut stream: hornvale_kernel::Stream,
    ) -> Option<EntranceMouth> {
        // Same top-band reference `entrance_mouth` itself uses, read off the
        // ladder rather than named.
        let top = top_band();
        let branches = crate::character::branch_count_of(seed, vertex, top);
        if branches <= 1 {
            return Some(EntranceMouth {
                branch: 0,
                band: 0,
                floor: 0,
            });
        }
        let index = stream.range_u32(0, u32::from(branches - 2));
        let branch = u8::try_from(index + 1).ok()?;
        Some(EntranceMouth {
            branch,
            band: rung_rank(top)?,
            floor: 0,
        })
    }

    // --- Task 7b: the top band is entered (spec amendment E.2) ---

    /// The branches every aperture of one system opens on, at an
    /// **explicitly given** top-band width — the seam E.2's guarantee is
    /// asserted through, the way §4.5's two are asserted through
    /// [`descent_edges_for`].
    ///
    /// A one-line composition of the two shipped width-parameterised halves,
    /// because the probed path must be the shipped path: a test-only
    /// reimplementation would assert a guarantee about code no world runs.
    fn aperture_branches_at(
        seed: Seed,
        vertex: Vertex,
        width: u8,
    ) -> std::collections::BTreeSet<u8> {
        (0..aperture_count_at(seed, vertex, width))
            .map(|entrance| aperture_branch_at(seed, vertex, entrance, width))
            .collect()
    }

    /// GUARANTEE 3 (spec amendment E.2): **every branch in the top band is
    /// named by at least one entrance.**
    ///
    /// §4.5's second guarantee — every branch has a parent — is *vacuous* at
    /// the top band, which has no band above it to hang from. So a top-band
    /// branch no door landed on, and that no lower branch links back to, was
    /// orphaned: measured at 120 of seed 42's 123 unreached levels, all in
    /// branches with no open mouth. With this third guarantee the three
    /// together make a system's whole lattice one reachable component by
    /// construction.
    ///
    /// **Constructed over every width the top band can realize**, and over
    /// several vertices, for the reason [`every_branch_has_at_least_one_parent`]
    /// states: `branch_count_of` is weighted 60% toward width 1, where this
    /// guarantee is trivial, so a seed-panel scan would spend nearly all its
    /// evidence on the one case that cannot fail.
    #[test]
    fn every_top_band_branch_is_named_by_an_entrance() {
        let seed = Seed(42);
        for vertex in [0u32, 5, 6, 7, 9, 17, 31, 4096] {
            for width in 1..=BRANCHES_PER_SYSTEM {
                let named = aperture_branches_at(seed, Vertex(vertex), width);
                for branch in 0..width {
                    assert!(
                        named.contains(&branch),
                        "vertex {vertex}, top-band width {width}: branch {branch} is named \
                         by no entrance, so nothing enters it and everything hanging \
                         beneath it is orphaned (spec amendment E.2). Named: {named:?}"
                    );
                }
            }
        }
    }

    // --- Band-transition edges (The Drift, Task 6; spec §4.5) ---

    /// The edge set for one adjacent band pair at **explicitly given
    /// widths** — the seam §4.5's two guarantees are asserted through.
    ///
    /// It is a one-line forward to [`descent_edges`], which already takes
    /// the widths as parameters, because the shipped path must be the
    /// probed path: a test-only reimplementation would assert a guarantee
    /// about code no world runs. The name is the one the plan's brief uses,
    /// kept so a reader following the brief finds it here. **The brief's
    /// signature carried an `entrance` argument**; amendment A.3 took
    /// `entrance` out of the lattice entirely, so it is gone here too.
    fn descent_edges_for(
        seed: Seed,
        vertex: Vertex,
        band: Band,
        upper: u8,
        lower: u8,
    ) -> Vec<(u8, u8)> {
        descent_edges(seed, vertex, band, upper, lower)
    }

    /// GUARANTEE 1 (spec §4.5): **every branch descends.** Nothing dead-ends
    /// except at the bottom of the ladder.
    ///
    /// **This constructs the situation rather than scanning for it.** All
    /// sixteen `(upper, lower)` width pairs are exercised directly, because a
    /// panel scan would only ever exercise the widths a few seeds happen to
    /// draw — `branch_count_of` is weighted 60% toward width 1, so a scan
    /// would spend most of its evidence on the one pair where the guarantee
    /// is trivial. A guarantee is asserted, never measured into existence
    /// (spec §6).
    #[test]
    fn every_branch_has_at_least_one_descent() {
        let seed = Seed(42);
        for upper in 1..=BRANCHES_PER_SYSTEM {
            for lower in 1..=BRANCHES_PER_SYSTEM {
                let edges = descent_edges_for(seed, Vertex(7), Band::Deeps, upper, lower);
                for b in 0..upper {
                    assert!(
                        edges.iter().any(|&(from, _)| from == b),
                        "branch {b} of {upper} descends nowhere ({upper} above, {lower} below)"
                    );
                }
            }
        }
    }

    /// GUARANTEE 2 (spec §4.5): **every branch is reachable.** Nothing is
    /// orphaned below a band it cannot be entered from.
    ///
    /// Constructed over all sixteen width pairs for the same reason
    /// [`every_branch_has_at_least_one_descent`] is.
    #[test]
    fn every_branch_has_at_least_one_parent() {
        let seed = Seed(42);
        for upper in 1..=BRANCHES_PER_SYSTEM {
            for lower in 1..=BRANCHES_PER_SYSTEM {
                let edges = descent_edges_for(seed, Vertex(7), Band::Deeps, upper, lower);
                for b in 0..lower {
                    assert!(
                        edges.iter().any(|&(_, to)| to == b),
                        "branch {b} of {lower} has no parent ({upper} above, {lower} below)"
                    );
                }
            }
        }
    }

    /// The union of two surjections is bounded on both sides **by
    /// construction**, and this asserts the bounds rather than a fitted
    /// distribution (spec §8 leaves the extra-edge count open on purpose).
    ///
    /// Lower bound `max(upper, lower)`: the child loop alone contributes an
    /// edge from each of `upper` distinct sources and the parent loop an edge
    /// into each of `lower` distinct targets, so neither can be collapsed
    /// below its own arity. Upper bound `upper + lower`: exactly that many
    /// edges are pushed before the dedup, which can only remove.
    ///
    /// Swept over several vertices and every adjacent band pair, so a
    /// coincidence at one place cannot carry it.
    #[test]
    fn the_edge_count_sits_between_its_construction_bounds() {
        let seed = Seed(42);
        for vertex in [0u32, 7, 9, 4096] {
            for &band in Band::habitation() {
                let Some(_) = band.deeper() else { continue };
                for upper in 1..=BRANCHES_PER_SYSTEM {
                    for lower in 1..=BRANCHES_PER_SYSTEM {
                        let edges = descent_edges_for(seed, Vertex(vertex), band, upper, lower);
                        let n = u8::try_from(edges.len()).expect("at most 8 edges");
                        assert!(
                            n >= upper.max(lower) && n <= upper + lower,
                            "vertex {vertex} {band:?} {upper}x{lower}: {n} edges is outside \
                             [{}, {}]",
                            upper.max(lower),
                            upper + lower
                        );
                    }
                }
            }
        }
    }

    /// The band-descent key is a **save-format contract**:
    /// `StreamLabel::dynamic` hashes this string, so its spelling decides
    /// every band-transition edge in every world forever.
    ///
    /// If this test fails, you have re-drawn the whole underworld's
    /// connectivity. That is an epoch (`chamber/band-descent/v2` next), not a
    /// fix to this assertion.
    #[test]
    fn the_band_descent_key_spelling_is_pinned() {
        assert_eq!(
            descent_key(Vertex(9), 3, Band::Deeps, DescentRole::Child),
            "9/3/deeps/child"
        );
        assert_eq!(
            descent_key(Vertex(0), 0, Band::Undercroft, DescentRole::Parent),
            "0/0/undercroft/parent"
        );
        assert_eq!(
            descent_key(Vertex(12), 1, Band::Nadir, DescentRole::Parent),
            "12/1/nadir/parent"
        );
    }

    /// Every component of the band-descent key changes the key — including
    /// the **role**, which is the one a reader is most likely to assume is
    /// decorative. If the role did not separate the two draws, a branch's
    /// child pick and its parent pick would share a stream and the two
    /// surjections would stop being independent.
    ///
    /// The band is checked by NAME, not by rank: `rung_name` is what the key
    /// spells, so this also pins that a mid-ladder insertion cannot silently
    /// re-key an existing band.
    #[test]
    fn every_component_of_the_band_descent_key_is_load_bearing() {
        let base = descent_key(Vertex(9), 1, Band::Deeps, DescentRole::Child);
        for other in [
            descent_key(Vertex(8), 1, Band::Deeps, DescentRole::Child),
            descent_key(Vertex(9), 2, Band::Deeps, DescentRole::Child),
            descent_key(Vertex(9), 1, Band::Shallows, DescentRole::Child),
            descent_key(Vertex(9), 1, Band::Deeps, DescentRole::Parent),
        ] {
            assert_ne!(base, other, "a key component is not load-bearing");
        }
        // And the whole role vocabulary is distinct, so no two roles could
        // ever collapse onto one stream.
        assert_ne!(DescentRole::Child.word(), DescentRole::Parent.word());
    }

    /// The band-descent label is `/v1` — this campaign draws something that
    /// did not exist, so there is no earlier epoch to retire — and it is its
    /// own root leg, never shared with a sibling draw.
    #[test]
    fn the_band_descent_label_is_v1() {
        assert_eq!(
            crate::streams::BAND_DESCENT.as_str(),
            "chamber/band-descent/v1"
        );
        for sibling in [
            crate::streams::CHAMBER.as_str(),
            crate::streams::RUN_FLOORS.as_str(),
            crate::streams::BRANCH_COUNT.as_str(),
            crate::streams::BRANCH_CHARACTER.as_str(),
            crate::streams::BRANCH_BARRIER.as_str(),
            crate::streams::ENTRANCE_MOUTH.as_str(),
        ] {
            assert_ne!(
                crate::streams::BAND_DESCENT.as_str(),
                sibling,
                "the band-descent draw must not share a sibling's label"
            );
        }
    }

    /// **The edge list itself is byte-pinned for known calls** — the half
    /// [`the_band_descent_key_spelling_is_pinned`] cannot reach.
    ///
    /// That test pins what [`descent_key`] *returns*. It says nothing about
    /// what [`descent_edges`] *passes it*, and review round 1 found both of
    /// the arguments it chooses to be unguarded: the parent draw's `below`
    /// could become `band`, and the child draw's [`DescentRole::Child`] could
    /// become `Parent`, with all 47 tests still green. **The second is the
    /// serious one**, and it is exactly what the role word exists to prevent:
    /// under it the child draws for the pair `(B, B.deeper())` land in
    /// `{vertex}/{i}/{B}/parent`, the key space the parent draws for the pair
    /// `(B.shallower(), B)` already occupy — one key answering two different
    /// questions at two different widths.
    ///
    /// The widths were already pinned (swapping them reddens the guarantee
    /// tests); the band and the role now are too. Same instrument
    /// `the_entrance_draws_are_byte_pinned_for_known_keys` uses one section
    /// up: freeze the shipped answer for a handful of named calls, so any
    /// change to the derivation — key, band, role, parent leg or draw order —
    /// has to come here and be admitted deliberately.
    ///
    /// If this fails you have re-drawn every band transition in every world.
    /// That is an epoch (`chamber/band-descent/v2`), not a fix to these
    /// literals.
    ///
    /// The four calls span both roles, three different `(band, band.deeper())`
    /// pairs, asymmetric widths in both directions, and the saturated `4x4`
    /// case, so no single coincidence can carry them all.
    #[test]
    fn the_descent_edges_are_byte_pinned_for_known_calls() {
        assert_eq!(
            descent_edges(Seed(42), Vertex(9), Band::Deeps, 2, 3),
            vec![(0, 0), (1, 0), (1, 1), (1, 2)]
        );
        assert_eq!(
            descent_edges(Seed(42), Vertex(9), Band::Shallows, 3, 2),
            vec![(0, 0), (0, 1), (1, 0), (2, 0)]
        );
        assert_eq!(
            descent_edges(Seed(42), Vertex(0), Band::Undercroft, 4, 4),
            vec![
                (0, 3),
                (1, 2),
                (1, 3),
                (2, 0),
                (2, 1),
                (2, 2),
                (3, 1),
                (3, 2)
            ]
        );
        assert_eq!(
            descent_edges(Seed(90210), Vertex(7), Band::Underdeep, 2, 4),
            vec![(0, 0), (0, 2), (1, 0), (1, 1), (1, 3)]
        );
    }

    /// The descent draw travels the `BAND_DESCENT` leg and **not** a
    /// sibling's, even where the key string would be identical. The
    /// separation lives in the parent label, so a byte-identical key under a
    /// different parent yields a different stream — the argument
    /// `RUN_FLOORS`'s own doc makes, checked here for this leg.
    ///
    /// Two arms: the shipped pick agrees with a re-derivation through
    /// `BAND_DESCENT`, and disagrees *somewhere* with the same key under
    /// `RUN_FLOORS`. The second arm is the positive control — without it the
    /// first would pass for a draw that ignored its parent entirely.
    #[test]
    fn the_descent_draw_travels_the_band_descent_leg_and_not_a_siblings() {
        let seed = Seed(42);
        let width = BRANCHES_PER_SYSTEM;
        let mut sibling_disagreed = false;
        for vertex in 0u32..64 {
            for &band in Band::habitation() {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    let key = descent_key(Vertex(vertex), branch, band, DescentRole::Child);
                    let shipped = descent_pick(
                        seed,
                        Vertex(vertex),
                        branch,
                        band,
                        DescentRole::Child,
                        width,
                    );
                    let own = seed
                        .derive(crate::streams::BAND_DESCENT)
                        .derive(StreamLabel::dynamic(&key))
                        .stream()
                        .range_u32(0, u32::from(width - 1));
                    assert_eq!(
                        u32::from(shipped),
                        own,
                        "the descent pick does not travel the BAND_DESCENT leg at {key}"
                    );
                    let sib = seed
                        .derive(crate::streams::RUN_FLOORS)
                        .derive(StreamLabel::dynamic(&key))
                        .stream()
                        .range_u32(0, u32::from(width - 1));
                    if sib != own {
                        sibling_disagreed = true;
                    }
                }
            }
        }
        assert!(
            sibling_disagreed,
            "the band-descent leg agreed with the run-floors leg everywhere"
        );
    }

    /// Nothing descends from the bottom of the ladder, and nothing descends
    /// from the overworld. [`Band::Nadir`] has no band below it and
    /// [`Band::Surface`] is not a habitation band at all — both answer empty,
    /// and neither is spelled as a numeric bound.
    #[test]
    fn nothing_descends_from_the_bottom_or_the_top_of_the_ladder() {
        let seed = Seed(42);
        assert_eq!(Band::Nadir.deeper(), None, "Nadir is the bottom rung");
        for vertex in 0u32..64 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                assert!(
                    descents_from(seed, Vertex(vertex), Band::Nadir, branch).is_empty(),
                    "vertex {vertex} branch {branch} descends below the Nadir"
                );
                assert!(
                    descents_from(seed, Vertex(vertex), Band::Surface, branch).is_empty(),
                    "vertex {vertex} branch {branch} descends out of the overworld"
                );
            }
        }
    }

    /// A branch the band's own drawn width does not realize descends
    /// nowhere — the drawn-realization half of decision 0102's split, applied
    /// to the edge set, so a caller cannot walk into a column this band never
    /// filled in.
    ///
    /// Two arms again: past the width, empty; inside it, non-empty. Without
    /// the second, a `descents_from` that returned empty for everything would
    /// pass.
    #[test]
    fn a_branch_its_band_does_not_realize_descends_nowhere() {
        let seed = Seed(42);
        let mut saw_a_realized_branch = false;
        for vertex in 0u32..256 {
            for &band in Band::habitation() {
                if band.deeper().is_none() {
                    continue;
                }
                let width = crate::character::branch_count_of(seed, Vertex(vertex), band);
                for branch in 0..BRANCHES_PER_SYSTEM {
                    let d = descents_from(seed, Vertex(vertex), band, branch);
                    if branch >= width {
                        assert!(
                            d.is_empty(),
                            "vertex {vertex} {band:?} branch {branch} is past width {width} \
                             and still descends"
                        );
                    } else {
                        assert!(
                            !d.is_empty(),
                            "vertex {vertex} {band:?} branch {branch} is inside width {width} \
                             and descends nowhere"
                        );
                        saw_a_realized_branch = true;
                    }
                }
                // And an out-of-lattice branch is refused outright.
                assert!(descents_from(seed, Vertex(vertex), band, BRANCHES_PER_SYSTEM).is_empty());
            }
        }
        assert!(saw_a_realized_branch, "the sweep realized no branch at all");
    }

    /// Every answer is ascending, deduplicated, and inside the **lower**
    /// band's drawn width — never the upper band's, and never the lattice
    /// ceiling. Reading the wrong width into the child draw is the exact
    /// mistake this composition could make silently, so it is asserted
    /// rather than assumed.
    #[test]
    fn descents_from_answers_inside_the_lower_bands_drawn_width() {
        let seed = Seed(42);
        for vertex in 0u32..256 {
            for &band in Band::habitation() {
                let Some(below) = band.deeper() else { continue };
                let lower = crate::character::branch_count_of(seed, Vertex(vertex), below);
                for branch in 0..BRANCHES_PER_SYSTEM {
                    let d = descents_from(seed, Vertex(vertex), band, branch);
                    let mut sorted = d.clone();
                    sorted.sort_unstable();
                    sorted.dedup();
                    assert_eq!(
                        d, sorted,
                        "vertex {vertex} {band:?} branch {branch} is not a set"
                    );
                    for to in d {
                        assert!(
                            to < lower,
                            "vertex {vertex} {band:?} branch {branch} descends into branch {to}, \
                             past the lower band's width {lower}"
                        );
                    }
                }
            }
        }
    }

    /// **The shipped composition keeps both guarantees.**
    /// [`every_branch_has_at_least_one_descent`] and its sibling assert the
    /// property of [`descent_edges`] at constructed widths; this asserts that
    /// [`descents_from`] hands that function the *right* widths — the upper
    /// band's for the upper side and the lower band's for the lower — which
    /// no amount of constructed-width evidence can establish.
    ///
    /// Read as a scan it would be weak evidence; read as what it is — a wiring
    /// check on a composition whose underlying property is already proved by
    /// construction — it is the half the constructed tests cannot reach.
    ///
    /// claim: structural(3 seeds x 256 vertices x every adjacent band pair) —
    /// the composition hands `descent_edges` the two bands' own widths, so
    /// the union it returns spans exactly the branches those bands realize.
    /// The seeds are a wiring witness, not a population estimate: the
    /// property itself is proved by construction at all sixteen width pairs.
    #[test]
    fn the_shipped_composition_keeps_both_guarantees_over_a_panel() {
        for seed in [Seed(42), Seed(7), Seed(90210)] {
            for vertex in 0u32..256 {
                for &band in Band::habitation() {
                    let Some(below) = band.deeper() else { continue };
                    let upper = crate::character::branch_count_of(seed, Vertex(vertex), band);
                    let lower = crate::character::branch_count_of(seed, Vertex(vertex), below);
                    let mut parented: Vec<u8> = Vec::new();
                    for branch in 0..upper {
                        let d = descents_from(seed, Vertex(vertex), band, branch);
                        assert!(
                            !d.is_empty(),
                            "seed {seed:?} vertex {vertex} {band:?} branch {branch} dead-ends"
                        );
                        parented.extend(d);
                    }
                    parented.sort_unstable();
                    parented.dedup();
                    let want: Vec<u8> = (0..lower).collect();
                    assert_eq!(
                        parented, want,
                        "seed {seed:?} vertex {vertex} {band:?}: branches below are not all reached"
                    );
                }
            }
        }
    }

    /// The edge set is a fact about a **place**, not about the order the
    /// lattice was walked in (decision 0102). Interleaving unrelated queries
    /// between two reads of the same place must not move the answer — the
    /// failure this catches is a draw that advanced a shared stream, which is
    /// exactly what a single-stream-per-place design with two ordered draws
    /// would have produced.
    #[test]
    fn the_descent_edges_are_the_same_however_the_lattice_is_queried() {
        let seed = Seed(42);
        let subject = descents_from(seed, Vertex(9), Band::Deeps, 0);
        for vertex in 0u32..64 {
            for &band in Band::habitation() {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    let _ = descents_from(seed, Vertex(vertex), band, branch);
                    let _ = levels_in_branch(
                        seed,
                        RunAddr {
                            vertex: Vertex(vertex),
                            branch,
                            band,
                        },
                    );
                }
            }
        }
        assert_eq!(
            subject,
            descents_from(seed, Vertex(9), Band::Deeps, 0),
            "the descent edges moved when unrelated places were queried"
        );
    }

    /// **The one dial spec §8 leaves open, MEASURED and printed rather than
    /// tuned.** How many edges the union produces beyond the bare minimum
    /// decides whether the underworld is a tree or a mesh, and this campaign
    /// draws the union and nothing more. The distribution is reported here —
    /// `cargo test -p hornvale-worldgen descent -- --nocapture` prints it —
    /// and the only assertion is the by-construction bound, so nothing in
    /// this test can be satisfied by fitting the draw.
    ///
    /// claim: readout(3 seeds x 512 vertices x every adjacent band pair) — the
    /// edge-count distribution and mean out-degree are REPORTED, and the only
    /// gate is the by-construction bound `max(upper, lower) <= n <= upper +
    /// lower`. Spec §8 leaves the extra-edge count open; a test that gated it
    /// would be this campaign tuning the one dial it was told not to tune.
    #[test]
    fn the_edge_count_distribution_over_a_panel_is_reported() {
        let mut by_pair: BTreeMap<(u8, u8), BTreeMap<usize, usize>> = BTreeMap::new();
        let mut edges_total = 0usize;
        let mut upper_branches_total = 0usize;
        let mut pairs_total = 0usize;
        // The headline share: how often the union collapses to the fewest
        // edges its construction permits, `max(upper, lower)` — a spanning
        // shape with no redundancy at all. COUNTED HERE rather than added up
        // by hand from the table below, because review round 1 caught the
        // hand-computed version off by 18 pairs (87.0% for 86.8%): it read
        // each row's smallest OBSERVED count as the minimum, when the
        // minimum is `max(upper, lower)` and four of the square-ish rows
        // never reach it at all.
        let mut at_minimum_total = 0usize;
        for seed in [Seed(42), Seed(7), Seed(90210)] {
            for vertex in 0u32..512 {
                for &band in Band::habitation() {
                    let Some(below) = band.deeper() else { continue };
                    let upper = crate::character::branch_count_of(seed, Vertex(vertex), band);
                    let lower = crate::character::branch_count_of(seed, Vertex(vertex), below);
                    let n = descent_edges(seed, Vertex(vertex), band, upper, lower).len();
                    assert!(
                        n >= usize::from(upper.max(lower)) && n <= usize::from(upper + lower),
                        "edge count {n} outside its construction bounds at {upper}x{lower}"
                    );
                    *by_pair
                        .entry((upper, lower))
                        .or_default()
                        .entry(n)
                        .or_insert(0) += 1;
                    edges_total += n;
                    upper_branches_total += usize::from(upper);
                    pairs_total += 1;
                    if n == usize::from(upper.max(lower)) {
                        at_minimum_total += 1;
                    }
                }
            }
        }
        // Reported, never gated (spec §6's "REPORTED, NEVER GATED" clause
        // applies to a shape the design deliberately left open).
        println!("band-descent edge shape over 3 seeds x 512 vertices x 4 band pairs");
        println!("  band pairs measured        {pairs_total}");
        println!("  edges drawn                {edges_total}");
        println!(
            "  mean out-degree            {:.4} edges per upper branch",
            edges_total as f64 / upper_branches_total as f64
        );
        println!(
            "  mean edges per band pair   {:.4}",
            edges_total as f64 / pairs_total as f64
        );
        println!(
            "  at the minimum max(u,l)    {at_minimum_total} of {pairs_total} = {:.4}% \
             (a spanning shape, no redundancy)",
            100.0 * at_minimum_total as f64 / pairs_total as f64
        );
        for ((upper, lower), counts) in &by_pair {
            let total: usize = counts.values().sum();
            let sum: usize = counts.iter().map(|(n, c)| n * c).sum();
            let shape: Vec<String> = counts.iter().map(|(n, c)| format!("{n}:{c}")).collect();
            println!(
                "  {upper}x{lower}  n={total:<6} mean={:.4}  {}",
                sum as f64 / total as f64,
                shape.join(" ")
            );
        }
    }
    // --- The descent RULE (The Drift, Task 7; spec §4.5/§4.6) ---

    /// **Descent leaves from a branch's BOTTOM level and arrives at the TOP
    /// level of a branch the band-transition draw named** (spec §4.5) — and
    /// the branch it arrives on is `descents_from`'s answer, not the branch
    /// it left.
    ///
    /// **THE BRIEF'S OWN VERSION OF THIS TEST COULD NOT FAIL, AND THAT IS
    /// WORTH RECORDING.** It asked, from an Undercroft branch's bottom
    /// level, for *some* neighbour with `band == Shallows && level == 0`.
    /// The pre-Task-7 rule already answered exactly that — same branch,
    /// next band, level 0 — so the assertion was green against the rule it
    /// was written to reject. Its second arm ("a non-bottom level offers no
    /// band change") was green for the same reason at the Undercroft, and
    /// would have been WRONG one band down, where level 0's legitimate
    /// ASCENT is a band change.
    ///
    /// What actually discriminates the two rules is the BRANCH: the old rule
    /// carried `addr.branch` across the seam unconditionally, and the new one
    /// carries `descents_from`'s drawn answer. So this test asserts set
    /// equality against that draw and counts the cases where the drawn branch
    /// differs from the source branch — the `crossings` control, without
    /// which the whole sweep is satisfiable by the old rule.
    /// claim: invariant(forall-swept-seed) — the descent rule over a
    /// hand-built lattice (3 seeds x 40 vertices, builds no world)
    #[test]
    fn descent_leaves_from_the_bottom_level_and_arrives_at_the_top() {
        let column = fixture_column();
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);

        let mut bottoms = 0u32;
        let mut interiors = 0u32;
        let mut crossings = 0u32;

        for raw_seed in [42u64, 7, 90210] {
            let seed = Seed(raw_seed);
            for raw_vertex in 0u32..40 {
                let vertex = Vertex(raw_vertex);
                for &band in Band::habitation() {
                    for branch in 0..BRANCHES_PER_SYSTEM {
                        let levels = levels_in_branch(
                            seed,
                            RunAddr {
                                vertex,
                                branch,
                                band,
                            },
                        );
                        for level in 0..levels {
                            let addr = ChamberAddr {
                                vertex,
                                branch,
                                band,
                                level,
                            };
                            if !chamber_exists(seed, &cave, gradient, addr) {
                                continue;
                            }
                            let passages = passages_from(seed, &cave, gradient, addr);
                            let deeper: std::collections::BTreeSet<ChamberAddr> = passages
                                .iter()
                                .copied()
                                .filter(|n| n.band > addr.band)
                                .collect();
                            if level + 1 < levels {
                                interiors += 1;
                                assert!(
                                    deeper.is_empty(),
                                    "{addr:?} is not its run's bottom level ({levels} \
                                     drawn) yet offers a band change {deeper:?} — \
                                     descent is not bottom-only"
                                );
                                continue;
                            }
                            bottoms += 1;
                            let Some(below) = band.deeper() else {
                                assert!(
                                    deeper.is_empty(),
                                    "{addr:?} sits at the ladder's bottom yet descends"
                                );
                                continue;
                            };
                            let expected: std::collections::BTreeSet<ChamberAddr> =
                                descents_from(seed, vertex, band, branch)
                                    .into_iter()
                                    .map(|to| ChamberAddr {
                                        vertex,
                                        branch: to,
                                        band: below,
                                        level: 0,
                                    })
                                    .filter(|&t| chamber_exists(seed, &cave, gradient, t))
                                    .collect();
                            assert_eq!(
                                deeper, expected,
                                "{addr:?} descends to {deeper:?}, but the band-transition \
                                 draw names {expected:?} at level 0 of the band below"
                            );
                            crossings +=
                                expected.iter().filter(|t| t.branch != branch).count() as u32;
                        }
                    }
                }
            }
        }

        assert!(bottoms > 0 && interiors > 0, "the sweep probed nothing");
        assert!(
            crossings > 0,
            "no descent on the sweep ever landed on a branch other than the one \
             it left, so the old same-branch rule satisfies every assertion \
             above and this test cannot distinguish the two"
        );
        println!(
            "descent rule: {bottoms} bottom levels, {interiors} interior levels, \
             {crossings} branch-changing descents"
        );
    }
    /// **No passage is a sideways step between branches** (spec §4.6). The
    /// lateral `branch ± 1` rule is deleted: branches at one band are
    /// alternatives, not neighbours in a corridor, so no two addresses
    /// sharing a band and a level are ever adjacent.
    ///
    /// The `probed` control is not decoration — "no neighbour is lateral" is
    /// satisfied vacuously by a `passages_from` that returns nothing at all,
    /// and this is a function whose whole job is to return neighbours.
    /// claim: invariant(forall-swept-seed) — no lateral passage, over a
    /// hand-built lattice (3 seeds x 40 vertices, builds no world)
    #[test]
    fn no_passage_is_a_sideways_step_between_branches() {
        let column = fixture_column();
        let cave = Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
        let gradient = GeothermalGradient::new(24.0);
        let mut probed = 0u32;

        for raw_seed in [42u64, 7, 90210] {
            let seed = Seed(raw_seed);
            for raw_vertex in 0u32..40 {
                let vertex = Vertex(raw_vertex);
                for &band in Band::habitation() {
                    for branch in 0..BRANCHES_PER_SYSTEM {
                        for level in 0..LEVELS_PER_BRANCH_CEILING {
                            let addr = ChamberAddr {
                                vertex,
                                branch,
                                band,
                                level,
                            };
                            for n in passages_from(seed, &cave, gradient, addr) {
                                probed += 1;
                                assert!(
                                    !(n.band == addr.band && n.level == addr.level),
                                    "{addr:?} lists {n:?}, a sideways step to another \
                                     branch at the same band and level — the lateral \
                                     rule is back"
                                );
                                assert!(
                                    n.branch == addr.branch || n.band != addr.band,
                                    "{addr:?} lists {n:?}: a branch change inside one band"
                                );
                            }
                        }
                    }
                }
            }
        }
        assert!(
            probed > 0,
            "no address had any passage — the sweep is vacuous"
        );
        println!("passages inspected for laterality: {probed}");
    }

    /// **The two seam directions read the SAME edge set** — the property
    /// [`passages_from`]'s symmetry rests on, asserted against the drawn
    /// edges directly rather than through two `passages_from` calls (which
    /// `deep_realm_chamber.rs` already does end to end).
    ///
    /// For every adjacent band pair on a small panel, `ascents_from` of the
    /// lower branch names the upper branch exactly when [`descents_from`] of
    /// the upper branch names the lower one. A second draw for the upward
    /// direction — the obvious wrong implementation — fails this at once.
    /// claim: invariant(forall-swept-seed) — the two seam directions agree,
    /// over the drawn edges alone (3 seeds x 64 vertices, builds no world)
    #[test]
    fn the_two_seam_directions_read_one_edge_set() {
        let mut agreements = 0u32;
        let mut edges = 0u32;
        for raw_seed in [42u64, 7, 90210] {
            let seed = Seed(raw_seed);
            for raw_vertex in 0u32..64 {
                let vertex = Vertex(raw_vertex);
                for &band in Band::habitation() {
                    let Some(below) = band.deeper() else { continue };
                    for up in 0..BRANCHES_PER_SYSTEM {
                        for down in 0..BRANCHES_PER_SYSTEM {
                            let descends = descents_from(seed, vertex, band, up).contains(&down);
                            let ascends = ascents_from(seed, vertex, below, down).contains(&up);
                            assert_eq!(
                                descends, ascends,
                                "seed {raw_seed} vertex {raw_vertex}: {band:?} branch {up} -> \
                                 {below:?} branch {down} reads {descends} downward and \
                                 {ascends} upward — the two directions are two derivations"
                            );
                            agreements += 1;
                            if descends {
                                edges += 1;
                            }
                        }
                    }
                }
            }
        }
        assert!(agreements > 0, "no band pair was probed");
        assert!(
            edges > 0,
            "no edge was ever found, so both directions agreed only on `false` \
             and the comparison proves nothing"
        );
        println!("seam directions compared: {agreements} pairs, {edges} edges");
    }

    /// **A drawn mouth always names a branch its landing band realizes**
    /// (spec §4.6) — the defect `entrance_mouth`'s own doc records, retired
    /// by construction rather than gated downstream.
    ///
    /// Before Task 7 the branch was drawn against the Undercroft's width and
    /// the mouth was landed at a band `root_floor_of` drew independently, so
    /// roughly half of all side-branch mouths named a branch their landing
    /// band did not realize (47.8% / 53.0% / 51.9% across the panel). Every
    /// one of those was refused downstream by [`chamber_exists`] as a closed
    /// door — a real number in a committed artifact, produced by a category
    /// error.
    ///
    /// This asserts the fix where it lives, over a wide sweep: the mouth's
    /// branch is inside `branch_count_of` at the mouth's OWN band, always.
    /// `drift_reach_probe` re-measures the panel share the prose above
    /// quotes.
    /// claim: invariant(forall-swept-seed) — every drawn mouth names a branch
    /// its landing band realizes (4 seeds x 256 vertices, builds no world)
    #[test]
    fn a_drawn_mouth_names_a_branch_its_landing_band_realizes() {
        let mut side_doors = 0u32;
        for raw_seed in [42u64, 7, 90210, 1234] {
            let seed = Seed(raw_seed);
            for raw_vertex in 0u32..256 {
                let vertex = Vertex(raw_vertex);
                for entrance in 0..entrance_count(seed, vertex) {
                    let mouth = entrance_mouth(seed, vertex, entrance);
                    let band = Band::from_rank(mouth.band)
                        .expect("a mouth only ever names a habitation rank");
                    assert!(
                        mouth.branch < crate::character::branch_count_of(seed, vertex, band),
                        "seed {raw_seed} vertex {raw_vertex} entrance {entrance}: mouth \
                         {mouth:?} names a branch {band:?} does not realize"
                    );
                    assert_eq!(
                        band,
                        top_band(),
                        "seed {raw_seed} vertex {raw_vertex} entrance {entrance}: mouth \
                         {mouth:?} landed outside the top habitation band"
                    );
                    assert_eq!(mouth.floor, 0, "a mouth landed below its run's head");
                    if mouth.branch > 0 {
                        side_doors += 1;
                    }
                }
            }
        }
        assert!(
            side_doors > 0,
            "no side-branch mouth was drawn on the sweep, so the assertion \
             above only ever saw the literal head"
        );
        println!("side-branch mouths checked: {side_doors}");
    }
}
