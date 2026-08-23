//! The Stope, Task 3: a branch owns a character, a barrier, and a place in
//! the tree (spec amendments B.5 and C.1–C.2).
//!
//! Pure logic over `hornvale_worldgen::character`, in the shape of
//! `deep_realm_chamber.rs`: every entry point here is a pure function of a
//! place in the fixed lattice plus a [`Seed`], so no world-building fixture
//! is needed. Not `#[ignore]`d; the battery belongs in the ordinary commit
//! gate.
//!
//! The one number imported from measurement rather than derived here is
//! called out in its own test (`a_drow_tier_civilization_...`): the
//! preregistered Underdeep reach shares from spec B.4's table.

use std::collections::BTreeSet;

use hornvale_kernel::Band;
use hornvale_kernel::{CellId, Seed};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, RunAddr, chamber_exists, levels_in_branch,
};
use hornvale_worldgen::character::{
    BarrierPins, BarrierState, CHARACTERS, Character, bands_of, barrier_of, branch_count_of,
    character_at, character_of, root_floor_of,
};

/// The panel: the three preregistered seeds (spec B.4's table) and a few
/// hundred cells' worth of systems each — enough draws that a weighting
/// claim (mode, rarity) is evidence and not noise.
const PANEL_SEEDS: [u64; 3] = [42, 7, 1234];
const PANEL_CELLS: u32 = 300;

/// Spec B.4's LOWEST measured share of systems reaching the Underdeep across
/// the preregistered seeds (31 / 52 / 53%) — the bound a drow-tier character
/// draw must sit strictly below for "rare character, common band" to hold.
const UNDERDEEP_REACH_MIN: f64 = 0.31;

/// The modularity rule: a new character is a new table entry, and a table
/// entry is load-bearing. Asserted over the WHOLE roster, exhaustively:
///
/// 1. every character's [`bands_of`] table is non-empty and names only
///    habitation rungs (an empty or out-of-ladder entry is a character that
///    can exist nowhere);
/// 2. no two characters share a table — a duplicate entry means the enum
///    carries a member nothing distinguishes, which is two names for one
///    thing rather than two characters;
/// 3. the deep roster actually restricts: the drow-tier entry excludes the
///    shallow rungs, so "Underdeep-eligible" is a property of the table and
///    not of the draw that consults it.
#[test]
fn a_character_only_occupies_its_declared_bands() {
    let mut tables: Vec<(Character, Vec<Band>)> = Vec::new();
    for character in CHARACTERS {
        let bands = bands_of(*character);
        assert!(
            !bands.is_empty(),
            "{character:?} has an empty eligibility table — it can occupy nowhere"
        );
        for rung in bands {
            assert_ne!(
                *rung,
                Band::Surface,
                "{character:?}'s table names Surface, which is not a habitation \
                 rung and has no position in the lattice"
            );
        }
        tables.push((*character, bands.to_vec()));
    }
    for (i, (a, bands_a)) in tables.iter().enumerate() {
        for (b, bands_b) in tables.iter().skip(i + 1) {
            assert_ne!(
                bands_a, bands_b,
                "{a:?} and {b:?} share an eligibility table — two enum members \
                 one table entry"
            );
        }
    }

    let drow_tables = tables
        .iter()
        .filter(|(c, _)| matches!(c, Character::DrowTier))
        .collect::<Vec<_>>();
    assert_eq!(drow_tables.len(), 1, "exactly one drow-tier entry expected");
    for rung in &drow_tables[0].1 {
        assert!(
            rung_rank_at_least_underdeep(rung),
            "the drow-tier entry's table includes {rung:?}, a shallower rung — \
             eligibility must exclude the shallow bands or it restricts nothing"
        );
    }
}

/// A habitation rung is Underdeep-or-deeper — the eligibility floor the
/// drow-tier table must respect.
fn rung_rank_at_least_underdeep(rung: &Band) -> bool {
    matches!(rung, Band::Underdeep | Band::Nadir)
}

/// Spec B.4: reaching the Underdeep is NOT the same as meeting what lives
/// there. The band is common (31 / 52 / 53% of systems reach it); a
/// drow-tier civilization is a rare CHARACTER draw on a branch.
///
/// Measured over the panel: the share of branch draws coming up drow-tier
/// must sit STRICTLY below the lowest measured Underdeep reach share, and
/// must be non-zero (a rarity of exactly zero is a roster entry nothing can
/// ever meet, which is a different defect).
/// claim: rate(panel-seeds) — drow-tier share < Underdeep reach share
#[test]
fn a_drow_tier_civilization_is_a_character_draw_not_a_band() {
    let mut drow = 0usize;
    let mut total = 0usize;
    for raw_seed in PANEL_SEEDS {
        let seed = Seed(raw_seed);
        for raw_cell in 0..PANEL_CELLS {
            for &band in Band::habitation() {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    total += 1;
                    if character_of(seed, CellId(raw_cell), band, branch) == Character::DrowTier {
                        drow += 1;
                    }
                }
            }
        }
    }
    assert!(total > 0, "the panel drew no branches at all");
    let share = f64::from(u32::try_from(drow).expect("panel draws fit u32"))
        / f64::from(u32::try_from(total).expect("panel draws fit u32"));
    assert!(
        share > 0.0,
        "no branch on the whole panel ({total} draws) drew the drow-tier \
         character — a roster entry nothing can ever meet"
    );
    assert!(
        share < UNDERDEEP_REACH_MIN,
        "{drow}/{total} = {share:.3} of branch draws are drow-tier, at or above \
         the LOWEST measured Underdeep reach share ({UNDERDEEP_REACH_MIN}) — the \
         character is as common as the band, which is exactly the conflation \
         spec B.4 removes"
    );
}

/// One branch AT ONE BAND carries ONE character across every floor of that
/// run (spec B.5's coherence guarantee, **narrowed by The Drift Task 5**).
///
/// **This is a revision, not merely a rename.** Before Task 5,
/// `character_of` took no band parameter, so "same character at every
/// depth" held for a whole branch across every band. Task 5 re-keyed
/// `character_of` onto `(cell, band, branch)` (amendment A.3), so a branch
/// can now carry a DIFFERENT character at each band it occupies — see
/// `character_and_barrier_are_keyed_at_the_same_granularity` in
/// `hornvale_worldgen::character`'s own tests, which demands exactly that
/// variation. What survives from the old guarantee is narrower: the LEVEL
/// axis. `character_at(addr)` must still resolve every FLOOR of one
/// `(branch, band)` run to the same character as the band-level accessor —
/// if the projection ever consulted `level`, a descent within one band
/// would change civilizations mid-run, and this test reddens.
#[test]
fn one_branch_at_one_band_has_one_character_across_its_floors() {
    let seed = Seed(90210);
    for raw_cell in 0u32..20 {
        for branch in 0..BRANCHES_PER_SYSTEM {
            for &band in Band::habitation() {
                let expected = character_of(seed, CellId(raw_cell), band, branch);
                for level in 0..8u8 {
                    let addr = ChamberAddr {
                        cell: CellId(raw_cell),
                        branch,
                        band,
                        level,
                    };
                    assert_eq!(
                        character_at(seed, addr),
                        expected,
                        "{addr:?} resolved a different character than its own \
                         branch-and-band's — the projection consulted `level`"
                    );
                }
            }
        }
    }
}

/// C.1: a system has one to four branches, drawn, weighted hard toward one.
/// Over the whole panel every count is inside `1..=BRANCHES_PER_SYSTEM`, and
/// the MODE is exactly 1 — the mode is what makes the result a spine with
/// occasional side-descents rather than four parallel shafts.
///
/// Also asserted here because it is C.1's enforcement half: `chamber_exists`
/// refuses any branch at or past the drawn count, so the lattice admits four
/// columns but the world realizes the drawn width (the same
/// lattice-ceiling/drawn-realization split Task 2 applied to floors). The
/// positive control is branch 0, which every count admits.
///
/// **The enforcement half is checked at each BAND** (The Drift, Task 5):
/// `chamber_exists` now reads `branch_count_of(seed, addr.cell, addr.band)`
/// directly, so the gate enforces THAT band's own drawn width — there is no
/// longer a single system-wide width to enforce. The histogram and
/// enforcement halves both sweep every habitation band now, in lockstep, so
/// each `count` is checked against the gate at the same band it was drawn
/// from.
/// claim: rate(panel-seeds) — branch-count histogram, mode must be 1
#[test]
fn most_systems_have_one_branch_and_none_has_more_than_four() {
    let mut histogram = [0usize; BRANCHES_PER_SYSTEM as usize];
    let mut systems = 0usize;
    let mut realized_below_count = 0usize;
    let column = hornvale_terrain::column(
        35.0,
        0.3,
        true,
        400.0,
        1.0,
        hornvale_terrain::RockClass::Sandstone,
        hornvale_terrain::Basement::Continental,
    );
    // At the reach ceiling, so the BAND gate can never be what refuses —
    // only the branch-count gate can be.
    let cave =
        hornvale_terrain::Cave::from_reach(hornvale_terrain::CaveKind::Karst, 3000.0, &column);
    let gradient = hornvale_terrain::GeothermalGradient::new(24.0);
    for raw_seed in PANEL_SEEDS {
        let seed = Seed(raw_seed);
        for raw_cell in 0..PANEL_CELLS {
            for &band in Band::habitation() {
                let count = branch_count_of(seed, CellId(raw_cell), band);
                assert!(
                    (1..=BRANCHES_PER_SYSTEM).contains(&count),
                    "cell {raw_cell} band {band:?} under seed {raw_seed} \
                     drew {count} branches, outside 1..={BRANCHES_PER_SYSTEM}"
                );
                histogram[usize::from(count - 1)] += 1;
                systems += 1;

                // Enforcement half: `chamber_exists` now reads THIS band's
                // own drawn count (The Drift, Task 5), so the gate is checked
                // at the SAME band the count above was drawn from.
                //
                // Realization half: past the count, nothing exists.
                for branch in count..BRANCHES_PER_SYSTEM {
                    assert!(
                        !chamber_exists(
                            seed,
                            &cave,
                            gradient,
                            ChamberAddr {
                                cell: CellId(raw_cell),
                                branch,
                                band,
                                level: 0,
                            },
                        ),
                        "cell {raw_cell} band {band:?} drew {count} branches \
                         yet branch {branch} still exists under seed {raw_seed} — \
                         the count is reported but not enforced"
                    );
                }
                // Positive control: below the count, existence is still a
                // coin flip per address — so the refusal above only proves
                // something if SOMETHING exists below the count somewhere.
                for branch in 0..count {
                    if chamber_exists(
                        seed,
                        &cave,
                        gradient,
                        ChamberAddr {
                            cell: CellId(raw_cell),
                            branch,
                            band,
                            level: 0,
                        },
                    ) {
                        realized_below_count += 1;
                    }
                }
            }
        }
    }
    assert!(
        realized_below_count > 0,
        "no chamber exists below any drawn branch count anywhere on the panel, \
         so the refusal above proves nothing about the branch gate"
    );
    assert!(
        systems > 0,
        "the panel measured no systems — the mode assertion below would be vacuous"
    );
    let mode = histogram
        .iter()
        .enumerate()
        .max_by_key(|&(width, n)| (n, std::cmp::Reverse(width)))
        .map(|(width, _)| width + 1)
        .expect("non-empty histogram");
    assert_eq!(
        mode, 1,
        "the branch-count distribution's mode is {mode}, not 1 — the tree has \
         become parallel shafts: histogram {histogram:?} over {systems} systems"
    );
}

/// C.2: a branch is a subtree with a ROOT FLOOR on its parent, not a
/// parallel shaft. Every non-main-line branch's root floor must be a floor
/// its parent — the main line — actually realizes, and the main line itself
/// has no root floor (its root IS the surface).
/// claim: invariant(forall-panel-seed) — every non-main root floor is realized
#[test]
fn a_branch_roots_on_a_floor_that_exists() {
    for raw_seed in PANEL_SEEDS {
        let seed = Seed(raw_seed);
        for raw_cell in 0..PANEL_CELLS {
            for entrance in 0..2u8 {
                // The main line roots at the surface.
                assert_eq!(
                    root_floor_of(seed, CellId(raw_cell), entrance, 0),
                    None,
                    "cell {raw_cell} entrance {entrance} under seed {raw_seed}: the \
                     main line reported a root floor, but its root is the surface"
                );
                for branch in 1..BRANCHES_PER_SYSTEM {
                    let root = root_floor_of(seed, CellId(raw_cell), entrance, branch)
                        .expect("every non-main-line branch hangs off its parent");
                    let parent_realizes = levels_in_branch(
                        seed,
                        RunAddr {
                            cell: CellId(raw_cell),
                            branch: 0,
                            band: Band::from_rank(root.band)
                                .expect("root_floor_of only names real habitation ranks"),
                        },
                    );
                    assert!(
                        root.floor < parent_realizes,
                        "branch {branch} of cell {raw_cell} entrance {entrance} \
                         under seed {raw_seed} roots at band {} floor {}, but its \
                         parent realizes only {parent_realizes} floors there — a \
                         dangling subtree",
                        root.band,
                        root.floor
                    );
                }
            }
        }
    }
}

/// Decision 0102's determinism half, for the barrier: same place in, same
/// state out, even after unrelated queries interleave; two branches of one
/// system MAY differ (and somewhere on the panel DO — a barrier that cannot
/// distinguish branches is a per-system dial wearing a per-branch key); and
/// the pin overrides the draw everywhere, in the `--sky` idiom.
#[test]
fn the_barrier_is_deterministic_and_keyed_on_the_branch() {
    let seed = Seed(90210);
    let cell = CellId(9);

    let first = barrier_of(seed, cell, Band::Undercroft, 2, &BarrierPins::default());
    for raw_cell in 0u32..30 {
        for branch in 0..BRANCHES_PER_SYSTEM {
            let _ = barrier_of(
                seed,
                CellId(raw_cell),
                Band::Shallows,
                branch,
                &BarrierPins::default(),
            );
        }
    }
    assert_eq!(
        barrier_of(seed, cell, Band::Undercroft, 2, &BarrierPins::default()),
        first,
        "the same branch answered a different barrier after unrelated queries — \
         the draw advanced a stream instead of keying on a place"
    );

    // Two branches of one system may differ — and do, somewhere on the panel.
    let mut differing_systems = 0usize;
    for raw_cell in 0..PANEL_CELLS {
        let states: BTreeSet<BarrierState> = (0..BRANCHES_PER_SYSTEM)
            .map(|b| {
                barrier_of(
                    seed,
                    CellId(raw_cell),
                    Band::Undercroft,
                    b,
                    &BarrierPins::default(),
                )
            })
            .collect();
        if states.len() > 1 {
            differing_systems += 1;
        }
    }
    assert!(
        differing_systems > 0,
        "every system's four branches drew identical barriers across the panel — \
         the key does not distinguish branches"
    );

    // The pin wins everywhere, whatever the draw said.
    for state in [
        BarrierState::Sealed,
        BarrierState::Warded,
        BarrierState::Thin,
        BarrierState::Open,
    ] {
        let pins = BarrierPins { state: Some(state) };
        for raw_cell in 0u32..10 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                assert_eq!(
                    barrier_of(seed, CellId(raw_cell), Band::Undercroft, branch, &pins),
                    state,
                    "pin {:?} did not hold at cell {raw_cell} branch {branch}",
                    state
                );
            }
        }
    }
}
