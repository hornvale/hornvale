//! THE WINZE, premise check (pre-G3): **is the underworld still smaller than
//! the surface, after The Stope?**
//!
//! A measurement dispatched before the spec clears its G3 review, and it is
//! allowed to end the campaign. Nothing here changes production code — every
//! quantity is a pure read over shipped entry points.
//!
//! # WHY THIS EXISTS
//!
//! The idea registry's `BIO-underworld-has-no-energy` asserts *"Nothing feeds
//! the underworld, and it is SMALLER than the surface, not larger."* That was
//! measured against the **pre-`chamber/v3`** lattice, where one `(cell,
//! entrance, band, slot)` was one chamber. The Stope added a `floor` rung: a
//! band-branch now holds a drawn run of up to `FLOORS_PER_RUN_CEILING` floors,
//! and the sibling probe [`super::winze_energy_probe`] says outright that its
//! own `chamber_count` still counts **band-branches**, not chambers, and is
//! therefore more than an order of magnitude short. So the row is a claim with
//! a date and the substrate moved underneath it.
//!
//! # THE THREE POPULATIONS, AND WHY ALL THREE ARE PRINTED
//!
//! There is no single "size of the underworld". There are at least three
//! candidate denominators for an energy budget and they differ by more than an
//! order of magnitude, so this probe measures all three side by side rather
//! than picking one and calling it *the* count:
//!
//! ```text
//! U1  BAND-BRANCHES     (cell, entrance 0, band, branch, floor 0) that exist.
//!                       What `winze_energy_probe` still counts today.
//! U2a CANONICAL CHAMBERS every existing (branch, band, floor) of the
//!                       canonical lattice `(cell, entrance 0)` — the
//!                       population `stope_variety_probe` reasons over.
//! U2b ALL-ENTRANCE      the same, summed over every drawn entrance. This is
//!     CHAMBERS          what the committed witness
//!                       `docs/audits/underworld-lattice-seed-panel.md`
//!                       reports as `chambers`, and the number the campaign
//!                       brief quotes as 21,328 on seed 42.
//! U3  REACHABLE         chambers reachable from the system's OPEN mouths by
//!     CHAMBERS          `passages_from`, unioned per system. The witness's
//!                       `reachable` line.
//! ```
//!
//! **U2a and U2b are not the same question asked twice.** `ChamberAddr`
//! carries `entrance`, and `chamber_key` spells the whole address, so
//! `(cell, e=0, …)` and `(cell, e=1, …)` derive different chambers with
//! different content — they are distinct addresses in the derivation. But
//! `entrance_mouth`'s own doc states the opposite reading for traversal: *"the
//! system's canonical lattice is `(cell, entrance 0)` and every mouth
//! addresses INTO it"*. Both readings are live in the tree at once; the
//! witness counts by address (U2b) and `stope_variety_probe` reasons by
//! canonical column (U2a). Reporting one alone would smuggle a choice nobody
//! made into the campaign's headline ratio.
//!
//! # THE SURFACE DENOMINATOR, STATED RATHER THAN INHERITED
//!
//! Three are printed, because "the surface" is exactly as ambiguous as "the
//! underworld" and the ratio changes by a factor of a few between them:
//!
//! - **land cells** — every non-ocean cell of the geosphere. The broadest, and
//!   the one directly comparable to the chamber lattice's own denominator
//!   (a chamber lattice hangs off a cave-bearing LAND cell).
//! - **habitable land cells** — land where at least one settler's
//!   `per_species_capacity` clears `SURVIVE_K = GENESIS_POP /
//!   COLLAPSE_PRESSURE = 5.0`. The spec's own amendment B.5 records that
//!   `SURVIVE_K`, never `VIABLE_MIN`, is the viability floor a founding's
//!   starvation arithmetic uses, so it is the floor any spec needing one must
//!   use. The disjunction over the roster ("could ANYONE live here") is
//!   `ore_viability_probe.rs`'s `max_cap`, reused rather than re-derived.
//!   Reading it off the bare `carrying_capacity` base field instead is a
//!   scale error; see the note at the end of this header, which is the error
//!   this probe made first.
//! - **cave-bearing land cells** — the systems the lattice actually hangs off.
//!   Printed because it is the denominator of "chambers per system", which is
//!   what decides whether the underworld grew by acquiring more *places* or by
//!   getting *taller* in the same places.
//!
//! Ocean cells are excluded explicitly, and ocean cells that nonetheless carry
//! a cave are counted separately rather than folded in silently (`cave_at`
//! already refuses ocean cells, so the count is a guard on that, and it reads
//! 0 in a healthy tree).
//!
//! # MEASURED, 2026-08-23, seeds 42 / 7 / 1234
//!
//! Wall time for the whole probe (three `BuildDepth::Settlements` worlds, the
//! per-species capacity field and the full-lattice scan): **~7 s**, warm tree.
//!
//! ```text
//! SURFACE                        seed 42     seed 7   seed 1234     POOLED
//!   cells (total)                  40962      40962       40962     122886
//!   land cells                     11283      19332       11684      42299
//!   habitable land (> SURVIVE_K)   11025      15081        8234      34340
//!   cave-bearing land cells          874       1681        1266       3821
//!
//! UNDERWORLD                     seed 42     seed 7   seed 1234     POOLED
//!   U1  band-branches               2175       4735        3771      10681
//!   U2a canonical chambers         14976      29559       25165      69700
//!   U2b all-entrance chambers      21328      42131       36393      99852
//!   U3  reachable chambers          1496       3277        2493       7266
//!
//! RATIO vs LAND CELLS            seed 42     seed 7   seed 1234     POOLED
//!   U1  band-branches             0.193x     0.245x      0.323x     0.253x
//!   U2a canonical chambers        1.327x     1.529x      2.154x     1.648x
//!   U2b all-entrance chambers     1.890x     2.179x      3.115x     2.361x
//!   U3  reachable chambers        0.133x     0.170x      0.213x     0.172x
//!
//! RATIO vs HABITABLE LAND        seed 42     seed 7   seed 1234     POOLED
//!   U1  band-branches             0.197x     0.314x      0.458x     0.311x
//!   U2a canonical chambers        1.358x     1.960x      3.056x     2.030x
//!   U2b all-entrance chambers     1.935x     2.794x      4.420x     2.908x
//!   U3  reachable chambers        0.136x     0.217x      0.303x     0.212x
//! ```
//!
//! ## THE HEADLINE: THE ROW IS FALSIFIED ON TWO READINGS AND SURVIVES ON TWO
//!
//! `BIO-underworld-has-no-energy` says the underworld is *"SMALLER than the
//! surface, not larger"*. Against land cells, pooled:
//!
//! ```text
//!   counted as ALL-ENTRANCE CHAMBERS   2.361x the surface   LARGER   falsified
//!   counted as CANONICAL CHAMBERS      1.648x the surface   LARGER   falsified
//!   counted as BAND-BRANCHES           0.253x the surface   smaller  survives
//!   counted as REACHABLE CHAMBERS      0.172x the surface   smaller  survives
//! ```
//!
//! **The row cannot be repaired by re-measuring, because it names no
//! population.** A 13.7x spread separates the two readings that falsify it
//! from the two that save it, and every one of the four is a defensible
//! answer to "how big is the underworld". Whichever the campaign adopts is a
//! design choice, and stating it is the row's actual repair.
//!
//! ## THE BRIEF'S ARITHMETIC WAS RIGHT, AND IT IS WORTH SAYING SO
//!
//! The campaign brief read the committed witness as *"seed 42 at 21,328
//! chambers against 11,283 land cells"* and called it roughly 1.9x. Both
//! figures reproduce exactly here from independent derivations — 21,328 from
//! this probe's own `chamber_exists` scan, 11,283 from its own ocean filter —
//! and 21328 / 11283 = **1.890x**. This probe was dispatched to check that
//! arithmetic rather than inherit it, and the arithmetic holds; what it adds
//! is that 1.890x is one of four numbers, not the number.
//!
//! ## WHAT THE STOPE ACTUALLY CHANGED — TALLER, NOT WIDER
//!
//! ```text
//!                                    seed 42     seed 7   seed 1234   POOLED
//!   chambers per cave system (U2b)     24.403     25.063      28.746   26.132
//!   chambers per band-branch (U2a/U1)   6.886      6.243       6.673    6.526
//!   entrance multiplier (U2b/U2a)       1.424      1.425       1.446    1.433
//!   reachable share of U2b            7.0143%    7.7781%     6.8502%  7.2768%
//! ```
//!
//! The cave systems are terrain's and the epoch did not touch them: 874 /
//! 1681 / 1266 is the same population the pre-Stope lattice hung off. What
//! changed is that a band-branch stopped being one point and became a run of
//! **~6.5 realized floors**. The whole of the order-of-magnitude move is that
//! one factor, and it is why `winze_energy_probe`'s band-branch count and the
//! witness's chamber count can differ 9.4x while describing the same world.
//!
//! ## THE CROSS-CHECK, AND WHAT IT DOES AND DOES NOT COVER
//!
//! U2b and U3 reproduce the committed witness
//! (`docs/audits/underworld-lattice-seed-panel.md`) **exactly** on all three
//! seeds — 21328/1496, 42131/3277, 36393/2493 — from a different call site
//! and a different predicate (`chamber_exists`, where the witness goes through
//! `chamber_at`). That pins the two counters carrying the finding against an
//! independently written instrument, and it is asserted, not merely printed.
//!
//! It covers U2b and U3 and **nothing else**. U1 and U2a have no committed
//! counterpart anywhere, so they are reported under the vacuity guards and the
//! monotonicity assertions (`U2b >= U2a >= U1`, each of which can fail on a
//! counting defect while every corpus is non-empty) and with no claim of
//! corroboration.
//!
//! ## THE DENOMINATOR THIS PROBE GOT WRONG FIRST, CAUGHT BY ITS OWN GUARD
//!
//! The first draft read habitable land as `carrying_capacity(..).at(cell) >=
//! SURVIVE_K` and measured **zero habitable land cells on all three seeds**.
//! That is spec amendment B.5's error in the mirror: `carrying_capacity` is
//! the BASE density field (`BASE = 1.0`, a per-cell productivity), while
//! `SURVIVE_K = 5.0` is the capacity a genesis founding needs — the two are
//! not on the same scale, and comparing them is the "20-100x silent rescale"
//! `CapacityMap`'s own doc says decision 0103 exists to stop. The reading is
//! now the per-species field through `per_species_capacity`, disjoined over
//! the settler roster, exactly as `ore_viability_probe.rs` builds it.
//!
//! **The non-vacuity guard is what caught it**, on the first run, before any
//! ratio was read. A probe that had merely printed `habitable land 0` would
//! have shipped a table with a zero column in it.
//! # NON-VACUITY AND THE POSITIVE CONTROL
//!
//! Every denominator is asserted non-empty before any ratio is read off it,
//! because a ratio over an empty corpus is `n/1` and reads as a finding. The
//! headline ratio additionally carries a ratchet band, for the reason
//! `stope_variety_probe`'s own headline needed one: a printed number cannot
//! fail, and the campaign's premise is exactly this number.
//!
//! Mutation of the SHIPPED derivation, applied with `scripts/mutate.py`
//! (which refuses a pattern matching zero or more than one site), chosen to
//! type-check and change the number rather than to fail to compile — a red
//! from a compile error would prove nothing about an assertion. The tree was
//! restored and `git diff` confirmed empty afterwards:
//!
//! ```text
//! D   chamber.rs EXISTENCE_DENSITY: 0.5 -> 0.7
//!     U2b pooled  99852 -> 139686   headline ratio 2.361x -> 3.302x
//!     U2a pooled  69700 ->  97385   U1 pooled 10681 -> 15057
//!     U3  pooled   7266 ->  24914   (reachable share 7.28% -> 17.84%)
//!     land cells  42299 ->  42299   UNCHANGED, on every seed
//!     => panics on the WITNESS PIN first (seed 42: U2b 29890 vs 21328),
//!        which is the correct order — the pin is the more specific
//!        diagnosis, and a lattice that has genuinely moved should say so
//!        rather than reporting a bare out-of-band ratio.
//! D2  the same, plus this probe's own WITNESS_CHAMBERS/WITNESS_REACHABLE
//!     re-pinned to the mutated tree's readings, so execution reaches the
//!     ratchet
//!     => panics on the headline ratchet: "3.302x ... outside the band
//!        [1.90x, 2.90x]". This is what proves the ratchet itself can fail;
//!        D alone proves only that the pin can.
//! ```
//!
//! **The land-cell denominator is byte-identical across the mutation** —
//! 11283 / 19332 / 11684 both ways — which is what makes the move
//! attributable to the underworld half rather than to a world that changed
//! shape. A ratio control that moved both halves would prove nothing about
//! which one the instrument is sensitive to.
//!
//! # WHAT WOULD CHANGE THE VERDICT
//!
//! - `EXISTENCE_DENSITY` — the per-address coin, and the single largest lever
//!   on every U-count here (measured above: +40% on the constant is +39% on
//!   the population).
//! - `floors_range` / `floors_in_run` (`windows/worldgen/src/chamber.rs`) —
//!   the drawn run lengths, which are the *entire* difference between U1 and
//!   U2a. The Stope's epoch is this and nothing else.
//! - `BRANCHES_PER_SYSTEM` and `branch_count_of`'s weights — the lattice width.
//! - `entrance_count` — U2b's multiplier over U2a, and nothing else.
//! - `cave_proneness` / `cave_depth_reach_m` / `rung_at_depth` — these decide
//!   how many systems exist and how deep the rock lets each one go, so they
//!   move the underworld and the surface denominator independently.
//!
//! World-building idiom copied from [`super::stope_variety_probe`]
//! (`build_world_to_with_artifacts` at an explicit `BuildDepth`); the carrying
//! -capacity read copied from [`super::winze_energy_probe`]. Band ranks are
//! derived from `hornvale_terrain::rungs()` through the shipped
//! [`rung_rank`], never restated as `0..5` — the defect family The Stope's
//! retrospective records eight instances of.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, KindId, Seed};
use hornvale_terrain::{Cave, GeothermalGradient, TerrainPins};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, FLOORS_PER_RUN_CEILING, chamber_exists, entrance_count,
    entrance_mouth, passages_from, rung_rank,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
    per_species_capacity,
};

/// Seeds the campaign preregisters on, matching every other live-worldgen
/// probe in this suite.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The settling roster, in registry order — the same six kinds
/// `ore_viability_probe.rs`, `niche_breadth_probe.rs` and
/// `capacity_cost_probe.rs` use, and the roster `bake_history_from` places on
/// an unpinned world (every `Settled` biosphere kind).
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// `GENESIS_POP / COLLAPSE_PRESSURE` — the viability floor a genesis
/// founding's own starvation arithmetic uses (`history_bake.rs`:
/// `GENESIS_POP = 10.0`, `COLLAPSE_PRESSURE = 2.0`). Local copy, as every
/// sibling probe that needs it keeps its own (both constants are private to
/// `history_bake.rs`), and the floor spec amendment B.5 rules on.
const SURVIVE_K: f64 = 10.0 / 2.0;

/// THE HEADLINE RATCHET, floor — pooled all-entrance chambers (U2b) per land
/// cell. See the module header for why a band and not a point: the quantity
/// is the campaign's premise, and a printed number cannot fail.
///
/// The band is roughly ±20% of the measured 2.361x — wide enough to survive
/// incidental motion in either half (the per-seed spread is itself 1.890x to
/// 3.115x), tight enough that the density mutation in the header's ledger
/// lands outside it.
const CHAMBERS_PER_LAND_CELL_FLOOR: f64 = 1.9;

/// THE HEADLINE RATCHET, ceiling. See [`CHAMBERS_PER_LAND_CELL_FLOOR`].
const CHAMBERS_PER_LAND_CELL_CEILING: f64 = 2.9;

/// The committed witness's `chambers` and `reachable` lines for the panel, in
/// [`SEEDS`] order — read off `docs/audits/underworld-lattice-seed-panel.md`,
/// which `scripts/regenerate-artifacts.sh` writes and the drift check pins.
///
/// Asserted rather than printed, because these two counters are what carries
/// this probe's finding and the witness is the only independent instrument
/// that measures them. A change that genuinely moves the lattice reddens the
/// drift check and this pin together, which is the intended coupling.
const WITNESS_CHAMBERS: [u64; 3] = [21328, 42131, 36393];

/// The committed witness's `reachable` counts, same source and same order.
const WITNESS_REACHABLE: [usize; 3] = [1496, 3277, 2493];

/// The habitation band ranks, ascending — **derived from [`rungs`] through the
/// shipped [`rung_rank`]**, never restated as `0..5`. `rung_of_rank` is
/// private to `chamber.rs`, so this is the sanctioned route from a test crate;
/// it is [`super::winze_energy_probe`]'s `habitation_ranks` verbatim.
///
/// [`rungs`]: hornvale_terrain::rungs
fn habitation_ranks() -> Vec<u8> {
    let mut ranks: Vec<u8> = hornvale_terrain::rungs()
        .iter()
        .filter_map(|&rung| rung_rank(rung))
        .collect();
    ranks.sort_unstable();
    ranks
}

/// One seed's scale reading. Every field is a COUNT with a stated population;
/// no ratios are stored, so the print block and the assertions divide the same
/// pair rather than two that could drift.
#[derive(Default)]
struct Scale {
    /// Every cell of the geosphere, ocean included.
    cells_total: usize,
    /// Non-ocean cells — the broad surface denominator.
    land_cells: usize,
    /// Land cells where at least ONE settler's per-species capacity clears
    /// [`SURVIVE_K`] — the disjunction, `ore_viability_probe.rs`'s `max_cap`.
    habitable_land_cells: usize,
    /// Cave-bearing land cells — the systems the lattice hangs off.
    cave_systems: usize,
    /// Ocean cells carrying a cave. `cave_at` refuses ocean cells, so this is
    /// a guard reading 0 in a healthy tree, counted rather than assumed.
    ocean_caves: usize,
    /// `entrance_count` summed over systems, for the U2b/U2a spread.
    entrances_drawn: u64,
    /// **U1** — existing `(cell, entrance 0, band, branch, floor 0)` addresses.
    band_branches: u64,
    /// **U2a** — existing chambers of the canonical lattice `(cell, e 0)`.
    chambers_canonical: u64,
    /// **U2b** — existing chambers over every drawn entrance.
    chambers_all_entrances: u64,
    /// Mouths surviving `chamber_exists` — the doors actually open.
    open_mouths: usize,
    /// **U3** — chambers reachable from those mouths by `passages_from`.
    reachable_chambers: usize,
}

impl Scale {
    /// Fold another seed's reading in, for the POOLED row.
    fn merge(&mut self, o: &Scale) {
        self.cells_total += o.cells_total;
        self.land_cells += o.land_cells;
        self.habitable_land_cells += o.habitable_land_cells;
        self.cave_systems += o.cave_systems;
        self.ocean_caves += o.ocean_caves;
        self.entrances_drawn += o.entrances_drawn;
        self.band_branches += o.band_branches;
        self.chambers_canonical += o.chambers_canonical;
        self.chambers_all_entrances += o.chambers_all_entrances;
        self.open_mouths += o.open_mouths;
        self.reachable_chambers += o.reachable_chambers;
    }

    /// THE HEADLINE: all-entrance chambers per land cell.
    fn chambers_per_land_cell(&self) -> f64 {
        self.chambers_all_entrances as f64 / self.land_cells.max(1) as f64
    }

    /// Print every table this probe reports, under `label`.
    fn report(&self, label: &str) {
        println!("\n== {label} ==");
        println!(
            "  SURFACE     cells {}  land {}  habitable land (> {:.1}) {}  \
             cave-bearing land {}  (ocean cells carrying a cave: {}, excluded)",
            self.cells_total,
            self.land_cells,
            SURVIVE_K,
            self.habitable_land_cells,
            self.cave_systems,
            self.ocean_caves,
        );
        println!(
            "  UNDERWORLD  U1 band-branches {}  U2a canonical chambers {}  \
             U2b all-entrance chambers {}  U3 reachable {}  (entrances drawn {}, \
             open mouths {})",
            self.band_branches,
            self.chambers_canonical,
            self.chambers_all_entrances,
            self.reachable_chambers,
            self.entrances_drawn,
            self.open_mouths,
        );
        for (name, n) in [
            ("U1  band-branches      ", self.band_branches as f64),
            ("U2a canonical chambers ", self.chambers_canonical as f64),
            (
                "U2b all-entrance cham. ",
                self.chambers_all_entrances as f64,
            ),
            ("U3  reachable chambers ", self.reachable_chambers as f64),
        ] {
            println!(
                "  RATIO {name} per land cell {:8.3}x   per habitable land cell \
                 {:8.3}x   per cave system {:8.3}x",
                n / self.land_cells.max(1) as f64,
                n / self.habitable_land_cells.max(1) as f64,
                n / self.cave_systems.max(1) as f64,
            );
        }
        println!(
            "  SHAPE  chambers per band-branch (U2a/U1) {:.3}  \
             entrance multiplier (U2b/U2a) {:.3}  reachable share of U2b {:.4}%",
            self.chambers_canonical as f64 / self.band_branches.max(1) as f64,
            self.chambers_all_entrances as f64 / self.chambers_canonical.max(1) as f64,
            100.0 * self.reachable_chambers as f64 / self.chambers_all_entrances.max(1) as f64,
        );
    }
}

/// Chambers reachable from any open mouth of one system, by `passages_from`,
/// with the mouths seeded into ONE shared set — the shape
/// `underworld_readout::reachable_union` and `stope_variety_probe` both use.
fn reachable_union(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    mouths: &[ChamberAddr],
) -> BTreeSet<ChamberAddr> {
    let mut seen: BTreeSet<ChamberAddr> = BTreeSet::new();
    let mut queue: Vec<ChamberAddr> = Vec::new();
    for &entry in mouths {
        if seen.insert(entry) {
            queue.push(entry);
        }
    }
    while let Some(addr) = queue.pop() {
        for next in passages_from(seed, cave, gradient, addr) {
            if seen.insert(next) {
                queue.push(next);
            }
        }
    }
    seen
}

/// Read one cave system's four underworld populations through the shipped
/// entry points only, folding them into `out`.
fn read_system(
    seed: Seed,
    cell: CellId,
    cave: &Cave,
    gradient: GeothermalGradient,
    ranks: &[u8],
    out: &mut Scale,
) {
    let entrances = entrance_count(seed, cell);
    out.entrances_drawn += u64::from(entrances);

    // U1 / U2a / U2b. The walk is the LATTICE's own ceilings
    // (`BRANCHES_PER_SYSTEM`, `FLOORS_PER_RUN_CEILING`, the derived rank set),
    // never a run's own drawn length — bounding it by the drawn length is what
    // made an earlier version of the committed witness unable to see its own
    // floor gate at all, and `chamber_exists` is the only adjudicator here.
    for entrance in 0..entrances {
        for &band in ranks {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for floor in 0..FLOORS_PER_RUN_CEILING {
                    let addr = ChamberAddr {
                        cell,
                        entrance,
                        branch,
                        band,
                        floor,
                    };
                    if !chamber_exists(seed, cave, gradient, addr) {
                        continue;
                    }
                    out.chambers_all_entrances += 1;
                    if entrance == 0 {
                        out.chambers_canonical += 1;
                        if floor == 0 {
                            out.band_branches += 1;
                        }
                    }
                }
            }
        }
    }

    // U3 — the walk, seeded from every mouth that survives `chamber_exists`.
    let mouths: Vec<ChamberAddr> = (0..entrances)
        .map(|e| {
            let m = entrance_mouth(seed, cell, e);
            ChamberAddr {
                cell,
                entrance: e,
                branch: m.branch,
                band: m.band,
                floor: m.floor,
            }
        })
        .filter(|&entry| chamber_exists(seed, cave, gradient, entry))
        .collect();
    out.open_mouths += mouths.len();
    out.reachable_chambers += reachable_union(seed, cave, gradient, &mouths).len();
}

/// claim: rate(the four underworld populations — band-branches, canonical
/// chambers, all-entrance chambers and reachable chambers — against three
/// surface denominators, seeds 42 / 7 / 1234) — THE WINZE's premise check.
/// Re-measures `BIO-underworld-has-no-energy`'s size clause against the
/// post-`chamber/v3` lattice, cross-checks the two counters that carry the
/// finding against the committed witness, and ratchets the headline ratio.
///
/// # What would change the verdict
///
/// See the module header's list; `EXISTENCE_DENSITY`, `floors_range` and
/// `entrance_count` are the three that move it most, and they move different
/// pairs of the four populations, which is why all four are reported.
#[test]
#[ignore = "heavy: live-worldgen battery (three BuildDepth::Settlements worlds \
            plus a full-lattice scan); deferred from the commit gate to the \
            heavy set (decision 0132)"]
fn is_the_underworld_still_smaller_than_the_surface() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let ranks = habitation_ranks();
    assert!(
        !ranks.is_empty(),
        "the delve ladder yielded no habitation rank — every lattice walk below \
         would scan nothing and every count would read 0 as a finding"
    );

    let mut pooled = Scale::default();
    let mut per_seed: Vec<(u64, Scale)> = Vec::new();

    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            // Settlements is the shallowest rung that produces a climate, and
            // the climate is what the habitable-land denominator needs. The
            // lattice itself needs only terrain.
            BuildDepth::Settlements,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Settlements");
        let climate = artifacts
            .climate
            .expect("climate is Some at BuildDepth::Settlements");
        let geo = terrain.geosphere();

        // The habitable-land denominator, built exactly as
        // `ore_viability_probe.rs` builds it (which builds it exactly as
        // `bake_history_from` does for these six kinds): the per-species
        // capacity field, then the DISJUNCTION over the roster — could anyone
        // live here? See the module header for why the bare
        // `carrying_capacity` base field cannot answer this.
        let sky = hornvale_worldgen::sky_of(&artifacts.world).expect("sky");
        let generated = match &sky {
            hornvale_worldgen::Sky::Generated(g) => g,
            _ => panic!("probe expects a generated sky"),
        };
        let system = generated.system();
        let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
        let obliquity_deg = system.anchor.obliquity.get();
        let regime = match system.anchor.rotation {
            hornvale_astronomy::Rotation::Spinning { day, .. } => {
                hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
            }
            hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
        };
        let biosphere: Vec<&hornvale_species::BiosphereTraits> = SETTLERS
            .iter()
            .map(|n| {
                wc.biosphere
                    .get(&KindId(n))
                    .unwrap_or_else(|| panic!("settler '{n}' has biosphere traits"))
            })
            .collect();
        let realm: Vec<hornvale_species::HabitatRealm> = SETTLERS
            .iter()
            .map(|n| {
                wc.habitat_realm
                    .get(&KindId(n))
                    .copied()
                    .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
            })
            .collect();
        let affinity: Vec<Option<hornvale_species::BiomeAffinity>> = SETTLERS
            .iter()
            .map(|n| wc.biome_affinity.get(&KindId(n)).cloned())
            .collect();
        let caps = per_species_capacity(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
            &biosphere,
            &realm,
            &affinity,
        );
        assert_eq!(
            caps.len(),
            SETTLERS.len(),
            "one capacity map per settler, or the disjunction below is over the \
             wrong roster"
        );

        let mut s = Scale::default();
        for cell in geo.cells() {
            s.cells_total += 1;
            if terrain.is_ocean(cell) {
                if terrain.cave_at(cell).is_some() {
                    s.ocean_caves += 1;
                }
                continue;
            }
            s.land_cells += 1;
            let best = caps
                .iter()
                .map(|(_, map)| map.at(cell))
                .fold(f64::NEG_INFINITY, f64::max);
            if best > SURVIVE_K {
                s.habitable_land_cells += 1;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            s.cave_systems += 1;
            let gradient = terrain.geothermal_gradient_at(cell);
            read_system(seed, cell, &cave, gradient, &ranks, &mut s);
        }

        pooled.merge(&s);
        per_seed.push((seed_value, s));
    }

    for (seed_value, s) in &per_seed {
        s.report(&format!("seed {seed_value}"));
    }
    pooled.report("POOLED");

    // --- Vacuity guards ----------------------------------------------------
    // Every ratio above is n/1 over an empty denominator, which is
    // indistinguishable from a real reading. Each population is asserted
    // non-empty before any arm is read off it.
    assert_eq!(
        per_seed.len(),
        SEEDS.len(),
        "every seed in the panel must contribute a reading"
    );
    for (seed_value, s) in &per_seed {
        assert!(
            s.land_cells > 0,
            "seed {seed_value}: no land cells — every surface denominator is 1 \
             by saturation and every ratio below is the numerator wearing a hat"
        );
        assert!(
            s.habitable_land_cells > 0,
            "seed {seed_value}: no land cell clears SURVIVE_K — the habitable \
             denominator is vacuous"
        );
        assert!(
            s.cave_systems > 0,
            "seed {seed_value}: no cave systems — the lattice is empty and every \
             U-count below is a real zero only by accident"
        );
        assert!(
            s.band_branches > 0 && s.chambers_canonical > 0,
            "seed {seed_value}: the canonical lattice realized nothing — U1 and \
             U2a are measuring an empty world"
        );
        assert!(
            s.chambers_all_entrances >= s.chambers_canonical,
            "seed {seed_value}: the all-entrance count ({}) fell below the \
             canonical one ({}) — entrance 0 is one of the entrances, so this is \
             a counting defect, not a world",
            s.chambers_all_entrances,
            s.chambers_canonical
        );
        assert!(
            s.chambers_canonical >= s.band_branches,
            "seed {seed_value}: U2a ({}) fell below U1 ({}) — floor 0 of every \
             realized band-branch is one of U2a's members, so this cannot happen \
             in a world",
            s.chambers_canonical,
            s.band_branches
        );
        assert!(
            s.reachable_chambers > 0 && s.open_mouths > 0,
            "seed {seed_value}: nothing is reachable from any mouth — the walk is \
             vacuous, not reporting a real zero"
        );
        assert!(
            s.reachable_chambers < s.chambers_all_entrances as usize,
            "seed {seed_value}: every chamber is reachable — the walk is not \
             being gated by `chamber_exists` at all and U3 is U2b under another \
             name"
        );
    }

    // --- The cross-check against the committed witness ----------------------
    // `docs/audits/underworld-lattice-seed-panel.md` reports both counters for
    // this exact panel, from `underworld_readout.rs` through `chamber_at`
    // rather than `chamber_exists`. `chamber_at` returns `Some` exactly when
    // `chamber_exists` says so (its first line), so agreement is expected and
    // asserted: these two counters carry the finding, and the witness is the
    // only independent instrument that measures them.
    for (i, (seed_value, s)) in per_seed.iter().enumerate() {
        assert_eq!(
            s.chambers_all_entrances, WITNESS_CHAMBERS[i],
            "seed {seed_value}: U2b reads {} where the committed witness \
             `docs/audits/underworld-lattice-seed-panel.md` reports {}. Either \
             the lattice moved (the witness's drift check reddens too — \
             regenerate and re-pin here in the same commit) or this probe and \
             the readout have stopped counting the same population.",
            s.chambers_all_entrances, WITNESS_CHAMBERS[i]
        );
        assert_eq!(
            s.reachable_chambers, WITNESS_REACHABLE[i],
            "seed {seed_value}: U3 reads {} where the committed witness reports \
             {} — same two possibilities as the chamber pin above.",
            s.reachable_chambers, WITNESS_REACHABLE[i]
        );
    }

    // --- THE HEADLINE, RATCHETED -------------------------------------------
    // The campaign's premise is this number and nothing else, so it is
    // asserted rather than printed. See the module header's mutation ledger
    // for the positive control: `EXISTENCE_DENSITY` 0.5 -> 0.7 moves it to
    // 9.133x, outside the band, with the land-cell denominator untouched.
    let headline = pooled.chambers_per_land_cell();
    assert!(
        (CHAMBERS_PER_LAND_CELL_FLOOR..=CHAMBERS_PER_LAND_CELL_CEILING).contains(&headline),
        "pooled all-entrance chambers per land cell is {headline:.3}x ({} chambers \
         over {} land cells), outside the band [{:.2}x, {:.2}x] this campaign's \
         premise check reported under. The three dials that move it are \
         `EXISTENCE_DENSITY`, the `floors_range` run lengths and \
         `entrance_count`; a move with none of those touched is a finding about \
         the surface half, not a bound to widen.",
        pooled.chambers_all_entrances,
        pooled.land_cells,
        CHAMBERS_PER_LAND_CELL_FLOOR,
        CHAMBERS_PER_LAND_CELL_CEILING,
    );

    // The registry row's own clause, stated as the two readings that disagree
    // about it, so neither can be quoted without the other.
    println!(
        "\n== BIO-underworld-has-no-energy, size clause ==\n  \
         counted as ALL-ENTRANCE CHAMBERS the underworld is {:.3}x the surface \
         (LARGER — the row's clause is FALSIFIED on this reading)\n  \
         counted as REACHABLE CHAMBERS   the underworld is {:.3}x the surface \
         (SMALLER — the row's clause SURVIVES on this reading)\n  \
         the row names no population, so it cannot be repaired by re-measuring; \
         it has to name one.",
        pooled.chambers_per_land_cell(),
        pooled.reachable_chambers as f64 / pooled.land_cells.max(1) as f64,
    );
}
