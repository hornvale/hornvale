//! A branch owns a character, a barrier, and a place in the tree (The
//! Stope, Task 3; spec amendments B.5 and C.1–C.2).
//!
//! Four derived quantities, each a pure function of a **place in the fixed
//! lattice** plus a [`Seed`] — never of a generation ordinal (decision
//! 0102), and never of any other draw:
//!
//! - [`character_of`] / [`character_at`] — which [`Character`] one branch
//!   carries **per band** (B.5: character and barrier are one object, same
//!   owner; re-keyed onto `(cell, band, branch)` by The Drift Task 5 — see
//!   below);
//! - [`barrier_of`] — how thin the barrier on one branch is, per band; four
//!   named states ([`BarrierState`]); pinnable through [`BarrierPins`] in
//!   the `--sky` pin idiom;
//! - [`branch_count_of`] — how many of the lattice's
//!   [`BRANCHES_PER_SYSTEM`]
//!   columns one cave system realizes **at a given band** (C.1's
//!   drawn-realization half; weighted hard toward 1, and NOT tuned to hit a
//!   target — see the test that measures it);
//! - [`root_floor_of`] — where a non-main-line branch hangs off its parent
//!   (C.2): a floor the parent actually realizes, or `None` for the main
//!   line, whose root IS the surface. **Not re-keyed by Task 5** — it
//!   retires with `BRANCH_ROOT` in Task 7 (spec §4.6) and keeps its
//!   pre-Drift `entrance` parameter until then.
//!
//! **The Drift, amendment A.3 (Task 5): `entrance` is out of the first
//! three draws' keys and `band` is in, all three at once.** Before this
//! change a branch's character, barrier and count were per-SYSTEM facts (one
//! answer for the whole depth); after it they are per-`(system, band)`
//! facts, which is what lets one system be two branches wide in the
//! Undercroft and one wide in the Shallows —
//! `branch_count_varies_by_band_somewhere_on_the_panel` is the test that
//! demands this actually happens somewhere on the panel, not merely that it
//! compiles. Character and barrier move together at the same granularity
//! (B.5: one object, two faces) — see
//! `character_and_barrier_are_keyed_at_the_same_granularity`.
//!
//! **This module ships dials only, no effects** (spec B.5): nothing here
//! changes what exists, what renders, or what a world commits. The draws
//! travel their own legs (`chamber/branch-count/v2`,
//! `chamber/branch-character/v2`, `chamber/branch-barrier/v2`,
//! `chamber/branch-root/v1`), so a later task can begin reading them without
//! relocating anything.
//!
//! **`thaumic` is untouched and this module never reaches for it**: it is a
//! rock property (`domains/terrain/src/lithology.rs`), pinned by terrain's
//! own test. A branch's character is who lives there, not what the rock is
//! made of.

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Band, CellId, Seed};

use crate::chamber::{BRANCHES_PER_SYSTEM, ChamberAddr, RunAddr, levels_in_branch};

/// Which kind of inhabitant one branch carries, per band (spec B.5; The
/// Drift Task 5 moved this from "for its whole depth" — see
/// [`character_of`]'s own doc). Deliberately small: a generic cave plus two
/// named alternatives,
/// because Task 3 ships the DIAL — the per-branch draw keyed on a place —
/// and a roster grows by adding table entries, not by re-cutting the draw.
/// Each member carries its own eligibility table via [`bands_of`]; the
/// tables, not the draw, decide where a character can be met.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Character {
    /// The undifferentiated cave fauna every branch falls back to — the
    /// "nothing special lives here" answer, and the draw's heavy tail.
    WildCave,
    /// Cultivated fungus gardens: a shallow-to-mid phenomenon, absent from
    /// the deep rungs.
    FungalGardens,
    /// A drow-tier civilization — the spec B.4 case study. Rare as a DRAW
    /// precisely because its BAND (the Underdeep) is common: reaching the
    /// depth must not be conflated with meeting what lives there.
    DrowTier,
}

/// The whole roster, in one place — the modularity rule's data half: a new
/// character is a new entry HERE and a new arm in [`bands_of`], and nothing
/// else in the derivation changes.
pub const CHARACTERS: &[Character] = &[
    Character::WildCave,
    Character::FungalGardens,
    Character::DrowTier,
];

/// The delve rungs one character can occupy — the eligibility TABLE, frozen
/// per character and exhaustive over the roster (a sixth variant fails this
/// to compile rather than inheriting a neighbour's bands). `Surface` is
/// deliberately unreachable from every table: it is not a habitation rung
/// and has no position in the chamber lattice.
///
/// Pairwise distinct by construction-check (see
/// `a_character_only_occupies_its_declared_bands`): two characters sharing
/// a table would be two names for one thing.
pub fn bands_of(character: Character) -> &'static [Band] {
    match character {
        Character::WildCave => &[
            Band::Undercroft,
            Band::Shallows,
            Band::Deeps,
            Band::Underdeep,
            Band::Nadir,
        ],
        Character::FungalGardens => &[Band::Undercroft, Band::Shallows, Band::Deeps],
        Character::DrowTier => &[Band::Underdeep, Band::Nadir],
    }
}

/// How thin the barrier between the underworld and what lies beyond it is,
/// on one branch (spec B.5). One scalar per branch with four NAMED states —
/// an enum, not a number, so no reader can average them or interpolate
/// across a boundary that is meant to be qualitative. Read differently by
/// band at the consumer; this module ships the dial only.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BarrierState {
    /// Nothing passes.
    Sealed,
    /// Passing costs something specific and standing.
    Warded,
    /// Passage is possible but contested.
    Thin,
    /// The barrier has effectively failed; the two sides are one region.
    Open,
}

/// All four states, in order — the draw walks this table, so the order here
/// is load-bearing for the pinned goldens (reordering is an epoch, not a
/// refactor).
const BARRIER_STATES: [BarrierState; 4] = [
    BarrierState::Sealed,
    BarrierState::Warded,
    BarrierState::Thin,
    BarrierState::Open,
];

/// Pins for the barrier draw, in the `--sky` idiom: `None` fields defer to
/// the derived value, `Some` overrides it everywhere. Mirrors
/// [`crate::settlement_pins::SettlementPins`]'s shape — plain struct,
/// `Option` per dial, parsed by [`parse_barrier_pin`] — because that is the
/// pattern the CLI already knows.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct BarrierPins {
    /// Force every branch's barrier to this state.
    pub state: Option<BarrierState>,
}

/// Parse one `--barrier` pin into [`BarrierPins`] — the
/// [`crate::settlement_pins::parse_pin`] discipline: accept exactly the
/// state names, refuse everything else with a message naming what is
/// accepted.
/// type-audit: bare-ok(identifier-text: s), bare-ok(prose: return)
pub fn parse_barrier_pin(s: &str, pins: &mut BarrierPins) -> Result<(), String> {
    match s {
        "sealed" => pins.state = Some(BarrierState::Sealed),
        "warded" => pins.state = Some(BarrierState::Warded),
        "thin" => pins.state = Some(BarrierState::Thin),
        "open" => pins.state = Some(BarrierState::Open),
        other => {
            return Err(format!(
                "unknown --barrier pin {other:?} (expected sealed | warded | thin | open)"
            ));
        }
    }
    Ok(())
}

/// Where a non-main-line branch roots on its parent (spec C.2): a level of
/// the main line — the parent every side-branch hangs off today — named by
/// band rank and level index. Both are coordinates the parent actually
/// realizes ([`levels_in_branch`]); [`root_floor_of`] refuses to name a
/// dangling one.
/// type-audit: bare-ok(index: band), bare-ok(index: floor)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BranchRoot {
    /// Which delve-ladder rank of the main line this branch descends from.
    pub band: u8,
    /// Which level of that run the junction sits at (`0..levels_in_branch` of
    /// the parent run).
    pub floor: u8,
}

/// The decimal key for [`root_floor_of`]'s own leg — cell, entrance, branch,
/// a place, no band and no floor, because a root belongs to the BRANCH as a
/// whole (spec B.5). **Not re-keyed by The Drift Task 5**: `BRANCH_ROOT`
/// retires with `root_floor_of` itself (Task 7, spec §4.6), so it keeps the
/// pre-Drift shape until then.
fn branch_key(cell: CellId, entrance: u8, branch: u8) -> String {
    format!("{}/{}/{branch}", cell.0, entrance)
}

/// The decimal key shared by [`character_of`] and [`barrier_of`] (The Drift,
/// amendment A.3, Task 5) — cell, branch, band, no floor, because a
/// character and a barrier belong to the BRANCH **at that band** as a whole
/// (spec B.5: same owner, same lattice key; the band moved into the key so a
/// system can carry a different branch width per band — see
/// [`branch_count_of`]). Field order follows [`RunAddr`]'s own shape (cell,
/// branch, band), the spelling every re-keyed leg in this crate now agrees
/// on. `band` is spelled by its [`crate::chamber::rung_name`] NAME, never its
/// numeric rank, for the same reason `chamber_key`/`run_key` do: a rank is a
/// declaration position that shifts if the delve ladder ever gains a rung in
/// the middle, and a name only moves if the name itself does.
fn band_branch_key(cell: CellId, branch: u8, band: Band) -> String {
    format!("{}/{branch}/{}", cell.0, crate::chamber::rung_name(band))
}

/// The decimal key for [`branch_count_of`] (The Drift, amendment A.3, Task
/// 5) — cell, band, no branch, because the count is a fact about the SYSTEM
/// at that band, not about any one branch. Same name-not-rank discipline as
/// [`band_branch_key`].
fn band_system_key(cell: CellId, band: Band) -> String {
    format!("{}/{}", cell.0, crate::chamber::rung_name(band))
}

/// Which [`Character`] one branch carries, for its whole depth.
///
/// Weighted hard toward [`Character::WildCave`], with
/// [`Character::DrowTier`] rare (~5% of branch draws — measured, not tuned:
/// it must sit strictly below the share of systems that REACH the Underdeep,
/// which spec B.4 measured at 31–53%, and the test
/// `a_drow_tier_civilization_is_a_character_draw_not_a_band` holds both
/// edges of that open interval).
///
/// Keyed on the branch's PLACE under [`crate::streams::BRANCH_CHARACTER`] —
/// **now including `band`** (The Drift, amendment A.3, Task 5): `entrance`
/// dropped out of the key and `band` moved in, so a branch's character can
/// differ from one band to the next. This REVISES the coherence promise the
/// pre-Drift doc made here ("a descent cannot change civilizations
/// mid-branch") — see [`character_at`], whose projection now reads
/// `addr.band` rather than dropping it, and
/// `character_and_barrier_are_keyed_at_the_same_granularity` in this
/// module's tests, which asserts the new granularity rather than the old
/// one.
/// type-audit: bare-ok(index: branch)
pub fn character_of(seed: Seed, cell: CellId, band: Band, branch: u8) -> Character {
    let r = seed
        .derive(crate::streams::BRANCH_CHARACTER)
        .derive(StreamLabel::dynamic(&band_branch_key(cell, branch, band)))
        .stream()
        .next_f64();
    if r < 0.05 {
        Character::DrowTier
    } else if r < 0.35 {
        Character::FungalGardens
    } else {
        Character::WildCave
    }
}

/// The chamber-addressed form of [`character_of`]: reads `addr.band` and
/// `addr.branch`, dropping only `level` (a character is one object per
/// branch-in-band, spec B.5, not per floor).
///
/// **Before The Drift Task 5**, this function dropped `band` too (a branch's
/// character held for its whole depth) and, transitionally through Task 4,
/// passed a literal `0` for the entrance argument `character_of` no longer
/// takes. Both are gone: `character_of` is now keyed on `(cell, band,
/// branch)`, so this projection reads the address's real band rather than
/// discarding it or standing a placeholder in for it.
pub fn character_at(seed: Seed, addr: ChamberAddr) -> Character {
    character_of(seed, addr.cell, addr.band, addr.branch)
}

/// How thin one branch's barrier is — the derived default behind
/// [`BarrierPins`]. Uniform over the four named states: the distribution is
/// reported by the tests, not tuned toward one (spec C.1's instruction
/// applies to the barrier too — ship the dial, measure what it produces).
///
/// Same key shape as [`character_of`] (The Drift, amendment A.3, Task 5:
/// `entrance` out, `band` in, same granularity move) but a DIFFERENT parent
/// leg ([`crate::streams::BRANCH_BARRIER`]), per B.5: one object, two faces,
/// each face's draw isolated so neither can collide with the other or with
/// any sibling leg.
/// type-audit: bare-ok(index: branch)
pub fn barrier_of(
    seed: Seed,
    cell: CellId,
    band: Band,
    branch: u8,
    pins: &BarrierPins,
) -> BarrierState {
    if let Some(state) = pins.state {
        return state;
    }
    let picked = seed
        .derive(crate::streams::BRANCH_BARRIER)
        .derive(StreamLabel::dynamic(&band_branch_key(cell, branch, band)))
        .stream()
        .range_u32(0, 3);
    // range_u32(0, 3) always answers 0..=3 and the table has 4 entries.
    BARRIER_STATES[usize::try_from(picked).expect("range_u32(0, 3) fits usize")]
}

/// How many of the lattice's [`BRANCHES_PER_SYSTEM`] branch columns one
/// cave system realizes (spec C.1) — the drawn-realization half of the
/// lattice-ceiling/drawn-realization split decision 0102 states:
/// `BRANCHES_PER_SYSTEM` sizes the ADDRESS SPACE, this draw decides what a
/// world fills in.
///
/// Weighted hard toward 1 (60% one branch, then 25% / 10% / 5%), because
/// the shape being aimed at is a spine with occasional side-descents, not
/// parallel shafts. These weights are the initial authoring choice, stated
/// here and measured by
/// `most_systems_have_one_branch_and_none_has_more_than_four` — they were
/// not fitted to land the mode, and must not be retuned to move a metric.
///
/// Keyed on the SYSTEM at a BAND — cell and band, no branch (The Drift,
/// amendment A.3, Task 5: `entrance` out, `band` in) — because the count is
/// a fact about the system **at that band**, not the system as a whole: two
/// bands of one system now draw their own widths, so a system may be two
/// branches wide in the Undercroft and one wide in the Shallows. That is the
/// entire point of the re-key — see
/// `branch_count_varies_by_band_somewhere_on_the_panel` in this module's
/// tests.
/// type-audit: bare-ok(count: return)
pub fn branch_count_of(seed: Seed, cell: CellId, band: Band) -> u8 {
    let r = seed
        .derive(crate::streams::BRANCH_COUNT)
        .derive(StreamLabel::dynamic(&band_system_key(cell, band)))
        .stream()
        .next_f64();
    if r < 0.60 {
        1
    } else if r < 0.85 {
        2
    } else if r < 0.95 {
        3
    } else {
        4
    }
}

/// The habitation band ranks, ascending — **derived from the delve ladder,
/// never restated as a literal.**
///
/// This exists because `root_floor_of` walked `(0..5u8)`, which Task 7's
/// probe found as the eighth instance of this campaign's signature defect
/// and the only one in SHIPPED code. `chamber::rung_of_rank` is private to
/// its module, so the route from here is the ladder itself
/// ([`hornvale_terrain::rungs`]) filtered through the lattice's one explicit
/// mapping ([`crate::chamber::rung_rank`]) — the same seam
/// `tests/suite/junctions.rs` uses. `Surface` has no habitation rank and
/// drops out here exactly as it does there.
///
/// A sixth habitation rung is already caught by `rung_of_rank(5) == None`
/// (`chamber.rs`), so this was never a silent narrowing — but that tripwire
/// only tells the next person the ladder grew, and then leaves them to find
/// every bound by hand. This one now moves on its own.
fn habitation_ranks() -> Vec<u8> {
    let mut ranks: Vec<u8> = hornvale_terrain::rungs()
        .iter()
        .filter_map(|&rung| crate::chamber::rung_rank(rung))
        .collect();
    ranks.sort_unstable();
    ranks
}

/// Where a non-main-line branch roots on its parent (spec C.2) — `None`
/// for the main line, whose root IS the surface, and for any branch outside
/// the lattice.
///
/// The root is drawn over floors the PARENT (the main line, branch 0)
/// actually realizes: the eligible bands are the ranks whose parent run has
/// at least one floor, the band is uniform over those, and the floor is
/// uniform over that run's realized count — so the answer is never a
/// dangling junction. If every parent run drew 0 floors the eligible set is
/// empty and this returns `None`: a branch rooted NOWHERE, with nothing to
/// hang off. Realizing the branch is
/// [`crate::chamber::chamber_exists`]'s job regardless — a returned root
/// does not imply the branch's system realized it (its own
/// [`branch_count_of`] draw may have been smaller).
///
/// Today every habitation band realizes ≥1 floor by
/// construction (every frozen range bottoms out at 1), but the filter is
/// written anyway so the guarantee survives a future range change without
/// depending on that coincidence.
///
/// Keyed on the CHILD branch's place under [`crate::streams::BRANCH_ROOT`]:
/// the child names itself, and its parent is determined by the tree shape
/// this campaign ships (all side branches hang off the main line).
/// type-audit: bare-ok(index: entrance), bare-ok(index: branch)
pub fn root_floor_of(seed: Seed, cell: CellId, entrance: u8, branch: u8) -> Option<BranchRoot> {
    if branch == 0 || branch >= BRANCHES_PER_SYSTEM {
        return None;
    }
    // `entrance` still travels this function's OWN `BRANCH_ROOT` derivation
    // below (Task 5's re-keying, not this task's), but The Drift dropped it
    // from `RunAddr` — see `crate::chamber::chamber_exists`'s own doc for the
    // same transitional shape — so the parent-realization probe just below
    // no longer takes it.
    let parent_realizes: Vec<(u8, u8)> = habitation_ranks()
        .into_iter()
        .map(|rank| {
            let band = Band::from_rank(rank).expect("habitation_ranks() only yields real ranks");
            (
                rank,
                levels_in_branch(
                    seed,
                    RunAddr {
                        cell,
                        branch: 0,
                        band,
                    },
                ),
            )
        })
        .filter(|&(_, count)| count > 0)
        .collect();
    if parent_realizes.is_empty() {
        // Every parent run drew 0 floors: no eligible band exists, so the
        // branch roots nowhere rather than underflowing the tally.
        return None;
    }
    let mut stream = seed
        .derive(crate::streams::BRANCH_ROOT)
        .derive(StreamLabel::dynamic(&branch_key(cell, entrance, branch)))
        .stream();
    let band_index = stream.range_u32(
        0,
        u32::try_from(parent_realizes.len() - 1).expect("bands fit u32"),
    );
    let (band, parent_count) = parent_realizes
        [usize::try_from(band_index).expect("range_u32 answered inside the eligible set")];
    let floor = stream.range_u32(0, u32::from(parent_count - 1));
    Some(BranchRoot {
        band,
        floor: u8::try_from(floor).expect("range_u32(0, parent_count-1) fits a u8"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    /// The four new labels are distinct strings — a typo colliding two legs
    /// would silently couple two independent draws.
    #[test]
    fn the_four_branch_labels_are_distinct() {
        let labels = [
            crate::streams::BRANCH_CHARACTER.as_str(),
            crate::streams::BRANCH_BARRIER.as_str(),
            crate::streams::BRANCH_COUNT.as_str(),
            crate::streams::BRANCH_ROOT.as_str(),
        ];
        let set: BTreeSet<&str> = labels.iter().copied().collect();
        assert_eq!(set.len(), 4, "two of the four branch legs share a label");
    }

    /// Each re-keyed leg spells its epoch `/v2` (The Drift, Task 5);
    /// `BRANCH_ROOT` is untouched and stays `/v1` until Task 7 retires it.
    /// None reuses a retired or existing label.
    #[test]
    fn every_new_label_is_v2_except_branch_root() {
        assert_eq!(
            crate::streams::BRANCH_CHARACTER.as_str(),
            "chamber/branch-character/v2"
        );
        assert_eq!(
            crate::streams::BRANCH_BARRIER.as_str(),
            "chamber/branch-barrier/v2"
        );
        assert_eq!(
            crate::streams::BRANCH_COUNT.as_str(),
            "chamber/branch-count/v2"
        );
        assert_eq!(
            crate::streams::BRANCH_ROOT.as_str(),
            "chamber/branch-root/v1"
        );
    }

    /// A branch belongs to ONE band. Two bands of the same system draw their
    /// own branch counts, so a system may be two branches wide in the
    /// Undercroft and one wide in the Shallows — which is the whole point of
    /// the change (The Drift, Task 5, spec amendment A.3).
    #[test]
    fn branch_count_varies_by_band_somewhere_on_the_panel() {
        let seed = Seed(42);
        let mut varied = false;
        for c in 0u32..400 {
            let counts: Vec<u8> = Band::habitation()
                .iter()
                .map(|&band| branch_count_of(seed, CellId(c), band))
                .collect();
            if counts.windows(2).any(|w| w[0] != w[1]) {
                varied = true;
                break;
            }
        }
        assert!(
            varied,
            "no system varies its branch count by band — the band is not in the key"
        );
    }

    /// `character_of` and `barrier_of` are ONE object per branch (The Stope
    /// spec B.5): re-keyed at the SAME granularity, so a branch that changes
    /// band changes both, and neither is keyed more coarsely than the other.
    ///
    /// **Controller fix round 1 (2026-08-23)**: the brief's own assertion
    /// used `chars.len() > 1 || barriers.len() > 1`, which is
    /// one-sided-vacuous — either dial alone varying satisfies an `||`, so a
    /// mutation that coarsens ONE dial's key back to the pre-Task-5
    /// per-system shape (dropping `band`) while leaving the other correctly
    /// re-keyed stayed GREEN. Confirmed: mutating `barrier_of`'s key alone
    /// to drop `band` left this test passing, because `character_of` alone
    /// still varied. Split into two independent assertions instead — BOTH
    /// dials must vary, and a failure now names WHICH one regressed rather
    /// than reporting "at least one" ambiguously.
    #[test]
    fn character_and_barrier_are_keyed_at_the_same_granularity() {
        let seed = Seed(42);
        let (cell, branch) = (CellId(31942), 0u8);
        let pins = BarrierPins::default();
        let mut chars = BTreeSet::new();
        let mut barriers = BTreeSet::new();
        for &band in Band::habitation() {
            chars.insert(character_of(seed, cell, band, branch));
            barriers.insert(barrier_of(seed, cell, band, branch, &pins));
        }
        assert!(
            chars.len() > 1,
            "character does not vary across bands at one branch — it is \
             still keyed without the band"
        );
        assert!(
            barriers.len() > 1,
            "barrier does not vary across bands at one branch — it is \
             still keyed without the band"
        );
    }

    /// The barrier draw is byte-pinned for known keys — literal states, the
    /// cheapest witness that any part of the derivation moved (parent leg,
    /// key spelling, `range_u32` semantics, table order).
    #[test]
    fn the_barrier_draw_is_byte_pinned_for_known_keys() {
        let seed = Seed(42);
        for (cell, band, branch, expected) in [
            (9u32, Band::Undercroft, 3u8, BarrierState::Warded),
            (0, Band::Shallows, 0, BarrierState::Thin),
            (17, Band::Undercroft, 2, BarrierState::Thin),
            (5, Band::Undercroft, 1, BarrierState::Warded),
        ] {
            assert_eq!(
                barrier_of(seed, CellId(cell), band, branch, &BarrierPins::default()),
                expected,
                "cell {cell} band {band:?} branch {branch} moved off its \
                 barrier pin"
            );
        }
    }

    /// The character draw is byte-pinned for known keys.
    #[test]
    fn the_character_draw_is_byte_pinned_for_known_keys() {
        let seed = Seed(42);
        for (cell, band, branch, expected) in [
            (9u32, Band::Undercroft, 3u8, Character::FungalGardens),
            (0, Band::Shallows, 0, Character::WildCave),
            (17, Band::Undercroft, 2, Character::FungalGardens),
            (5, Band::Undercroft, 1, Character::WildCave),
        ] {
            assert_eq!(
                character_of(seed, CellId(cell), band, branch),
                expected,
                "cell {cell} band {band:?} branch {branch} moved off its \
                 character pin"
            );
        }
    }

    /// The branch-count draw is byte-pinned for known keys.
    #[test]
    fn the_branch_count_draw_is_byte_pinned_for_known_keys() {
        let seed = Seed(42);
        for (cell, band, expected) in [
            (9u32, Band::Undercroft, 1u8),
            (0, Band::Shallows, 1),
            (17, Band::Undercroft, 1),
            (5, Band::Undercroft, 2),
        ] {
            assert_eq!(
                branch_count_of(seed, CellId(cell), band),
                expected,
                "cell {cell} band {band:?} moved off its branch-count pin"
            );
        }
    }

    /// The root-floor draw is byte-pinned for known keys, including the
    /// main line's `None`.
    #[test]
    fn the_root_floor_draw_is_byte_pinned_for_known_keys() {
        let seed = Seed(42);
        assert_eq!(
            root_floor_of(seed, CellId(9), 0, 0),
            None,
            "main line roots at the surface"
        );
        for (cell, entrance, branch, expected) in [
            (9u32, 0u8, 3u8, Some(BranchRoot { band: 1, floor: 5 })),
            (0, 1, 1, Some(BranchRoot { band: 1, floor: 1 })),
            (17, 0, 2, Some(BranchRoot { band: 1, floor: 0 })),
            (5, 0, 1, Some(BranchRoot { band: 0, floor: 3 })),
        ] {
            assert_eq!(
                root_floor_of(seed, CellId(cell), entrance, branch),
                expected,
                "cell {cell} entrance {entrance} branch {branch} moved off its \
                 root pin"
            );
        }
    }

    /// **Each shipped draw travels ITS OWN leg**, not a sibling's — the unit
    /// counterpart of the integration battery, and the hole Task 2 learned
    /// from: a test that re-implements the derivation cannot witness the
    /// shipped path, so every arm below starts from the SHIPPED entry point
    /// and compares against inline derivations.
    ///
    /// Two arms per draw, mirroring
    /// `the_run_draw_travels_the_run_floors_leg_and_not_the_chamber_leg`:
    /// the shipped answer equals its declared leg everywhere probed, and
    /// differs from at least one sibling leg somewhere — so arm 1 cannot
    /// pass under a re-parented draw merely by luck.
    #[test]
    fn each_draw_travels_its_own_leg_and_not_a_siblings() {
        let seed = Seed(90210);
        // The three re-keyed draws (character, barrier, count) sweep every
        // habitation band; `root_floor_of` is untouched by Task 5 and keeps
        // its own entrance-keyed place list.
        let band_places: Vec<(CellId, Band, u8)> = (0u32..12)
            .flat_map(|c| {
                Band::habitation().iter().flat_map(move |&band| {
                    (0u8..BRANCHES_PER_SYSTEM).map(move |b| (CellId(c), band, b))
                })
            })
            .collect();
        let entrance_places: Vec<(CellId, u8, u8)> = (0u32..12)
            .flat_map(|c| {
                (0u8..2)
                    .flat_map(move |e| (0u8..BRANCHES_PER_SYSTEM).map(move |b| (CellId(c), e, b)))
            })
            .collect();

        // --- character ---
        let mut char_disagreed = false;
        for &(cell, band, b) in &band_places {
            let shipped = character_of(seed, cell, band, b);
            let own = seed
                .derive(crate::streams::BRANCH_CHARACTER)
                .derive(StreamLabel::dynamic(&band_branch_key(cell, b, band)))
                .stream()
                .next_f64();
            assert_eq!(
                shipped,
                from_character_raw(own),
                "character_of does not travel the BRANCH_CHARACTER leg at {cell:?}/{band:?}/{b}"
            );
            let sibling = seed
                .derive(crate::streams::BRANCH_BARRIER)
                .derive(StreamLabel::dynamic(&band_branch_key(cell, b, band)))
                .stream()
                .next_f64();
            if from_character_raw(sibling) != shipped {
                char_disagreed = true;
            }
        }
        assert!(
            char_disagreed,
            "the character draw agreed with the barrier leg everywhere — arm 1 \
             above would pass under a re-parented draw"
        );

        // --- barrier ---
        let mut barrier_disagreed = false;
        for &(cell, band, b) in &band_places {
            let shipped = barrier_of(seed, cell, band, b, &BarrierPins::default());
            let own = seed
                .derive(crate::streams::BRANCH_BARRIER)
                .derive(StreamLabel::dynamic(&band_branch_key(cell, b, band)))
                .stream()
                .range_u32(0, 3);
            assert_eq!(
                shipped,
                BARRIER_STATES[usize::try_from(own).unwrap()],
                "barrier_of does not travel the BRANCH_BARRIER leg at {cell:?}/{band:?}/{b}"
            );
            let sibling = seed
                .derive(crate::streams::BRANCH_CHARACTER)
                .derive(StreamLabel::dynamic(&band_branch_key(cell, b, band)))
                .stream()
                .range_u32(0, 3);
            if BARRIER_STATES[usize::try_from(sibling).unwrap()] != shipped {
                barrier_disagreed = true;
            }
        }
        assert!(
            barrier_disagreed,
            "the barrier draw agreed with the character leg everywhere"
        );

        // --- branch count ---
        let systems: Vec<(CellId, Band)> = (0u32..40)
            .flat_map(|c| {
                Band::habitation()
                    .iter()
                    .map(move |&band| (CellId(c), band))
            })
            .collect();
        let mut count_disagreed = false;
        for &(cell, band) in &systems {
            let shipped = branch_count_of(seed, cell, band);
            let own = seed
                .derive(crate::streams::BRANCH_COUNT)
                .derive(StreamLabel::dynamic(&band_system_key(cell, band)))
                .stream()
                .next_f64();
            assert_eq!(
                shipped,
                count_from(own),
                "branch_count_of does not travel the BRANCH_COUNT leg at {cell:?}/{band:?}"
            );
            let sibling = seed
                .derive(crate::streams::BRANCH_CHARACTER)
                .derive(StreamLabel::dynamic(&band_system_key(cell, band)))
                .stream()
                .next_f64();
            if count_from(sibling) != shipped {
                count_disagreed = true;
            }
        }
        assert!(
            count_disagreed,
            "the branch-count draw agreed with a sibling leg everywhere"
        );

        // --- root floor ---
        let mut root_disagreed = false;
        for &(cell, e, b) in &entrance_places {
            if b == 0 {
                continue;
            }
            let shipped = root_floor_of(seed, cell, e, b);
            // Re-derive under the declared leg and compare field-wise.
            let mut own_stream = seed
                .derive(crate::streams::BRANCH_ROOT)
                .derive(StreamLabel::dynamic(&branch_key(cell, e, b)))
                .stream();
            let parent_counts: Vec<(u8, u8)> = (0..5u8)
                .map(|rank| {
                    let band = Band::from_rank(rank).expect("0..5u8 are all real habitation ranks");
                    (
                        rank,
                        levels_in_branch(
                            seed,
                            RunAddr {
                                cell,
                                branch: 0,
                                band,
                            },
                        ),
                    )
                })
                .filter(|&(_, n)| n > 0)
                .collect();
            let band_i = own_stream.range_u32(0, (parent_counts.len() - 1) as u32);
            let (band, parent_count) = parent_counts[usize::try_from(band_i).unwrap()];
            let floor = own_stream.range_u32(0, u32::from(parent_count - 1));
            assert_eq!(
                shipped,
                Some(BranchRoot {
                    band,
                    floor: u8::try_from(floor).unwrap()
                }),
                "root_floor_of does not travel the BRANCH_ROOT leg at {cell:?}/{e}/{b}"
            );
            // Sibling leg, same procedure: must differ somewhere.
            let mut sib_stream = seed
                .derive(crate::streams::BRANCH_BARRIER)
                .derive(StreamLabel::dynamic(&branch_key(cell, e, b)))
                .stream();
            let sib_band_i = sib_stream.range_u32(0, (parent_counts.len() - 1) as u32);
            let (sib_band, sib_parent) = parent_counts[usize::try_from(sib_band_i).unwrap()];
            let sib_floor = sib_stream.range_u32(0, u32::from(sib_parent - 1));
            if Some(BranchRoot {
                band: sib_band,
                floor: u8::try_from(sib_floor).unwrap(),
            }) != shipped
            {
                root_disagreed = true;
            }
        }
        assert!(
            root_disagreed,
            "the root draw agreed with a sibling leg on every probed branch"
        );
    }

    /// Helpers re-stating each draw's threshold table for the leg-witness
    /// test above. They exist so the comparison reads as "same thresholds,
    /// different parent", and a threshold edit fails BOTH sides together —
    /// the leg identity is what the test isolates.
    fn from_character_raw(r: f64) -> Character {
        if r < 0.05 {
            Character::DrowTier
        } else if r < 0.35 {
            Character::FungalGardens
        } else {
            Character::WildCave
        }
    }

    fn count_from(r: f64) -> u8 {
        if r < 0.60 {
            1
        } else if r < 0.85 {
            2
        } else if r < 0.95 {
            3
        } else {
            4
        }
    }
}
