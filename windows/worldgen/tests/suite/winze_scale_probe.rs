//! THE WINZE, premise check (pre-G3): **is the underworld still smaller than
//! the surface, after The Stope — and now after The Drift?**
//!
//! A measurement dispatched before the spec clears its G3 review, and it is
//! allowed to end the campaign. Nothing here changes production code — every
//! quantity is a pure read over shipped entry points.
//!
//! # WHY THIS EXISTS
//!
//! The idea registry's `BIO-underworld-has-no-energy` asserts *"Nothing feeds
//! the underworld, and it is SMALLER than the surface, not larger."* That was
//! measured against the **pre-`chamber/v3`** lattice, where one `(vertex,
//! entrance, band, slot)` was one chamber. The Stope added a `floor` rung: a
//! band-branch now holds a drawn run of up to `FLOORS_PER_RUN_CEILING` floors,
//! and the sibling probe [`super::winze_energy_probe`] says outright that its
//! own `chamber_count` still counts **band-branches**, not chambers, and is
//! therefore more than an order of magnitude short. So the row is a claim with
//! a date and the substrate moved underneath it.
//!
//! # THE POPULATIONS — FOUR, THEN THREE, AND THE SUBSTRATE DID THE CUTTING
//!
//! There is no single "size of the underworld". This probe's first run
//! (2026-08-23, pre-Drift) reported FOUR candidate denominators spanning
//! 13.7x, and refused to pick one, on the grounds that "the row names no
//! population, so it cannot be repaired by re-measuring; it has to name one.
//! Which one is the controller's call."
//!
//! **Two of the four were the same question asked twice, and The Drift proved
//! it by deleting the axis they differed on.** They were U2a (chambers of the
//! *canonical* lattice `(vertex, entrance 0)`) and U2b (the same, summed over
//! every drawn aperture) — two live readings because `ChamberAddr` carried an
//! `entrance` field, so `(vertex, e=0, …)` and `(vertex, e=1, …)` were distinct
//! addresses deriving distinct content, while `entrance_mouth`'s own doc
//! simultaneously said every mouth addresses INTO one canonical lattice. The
//! Drift's amendment A.3 settled it in favour of the second reading and
//! removed the field: *"a run belongs to a system's shared lattice, not to any
//! one aperture into it."* There is now one lattice per cave system, and the
//! U2a/U2b spread is not a number that can be measured.
//!
//! What remains:
//!
//! ```text
//! U1  BAND-BRANCHES /   (vertex, branch, band, level 0) that exist — one per
//!     RUNS              RUN the lattice realizes. What `winze_energy_probe`
//!                       still counts today, and a CONTAINER of places rather
//!                       than a place.
//! U2  CHAMBERS          every existing (branch, band, level) of the system's
//!                       one shared lattice. What the committed witness
//!                       `docs/audits/underworld-lattice-seed-panel.md`
//!                       reports as `chambers`.
//! U3  REACHABLE         chambers reachable from the system's OPEN mouths by
//!     CHAMBERS          `passages_from`, unioned per system. The witness's
//!                       `reachable` line — **now equal to U2 on every seed**,
//!                       which is The Drift's headline (7% -> 100%) and not a
//!                       counting defect here.
//! ```
//!
//! # THE SURFACE DENOMINATOR, STATED RATHER THAN INHERITED
//!
//! Three are printed, because "the surface" is exactly as ambiguous as "the
//! underworld" and the ratio changes by a factor of a few between them:
//!
//! - **land vertices** — every non-ocean vertex of the geosphere. The broadest, and
//!   the one directly comparable to the chamber lattice's own denominator
//!   (a chamber lattice hangs off a cave-bearing LAND vertex).
//! - **habitable land vertices** — land where at least one settler's
//!   `per_species_capacity` clears `SURVIVE_K = GENESIS_POP /
//!   COLLAPSE_PRESSURE = 5.0`. The spec's own amendment B.5 records that
//!   `SURVIVE_K`, never `VIABLE_MIN`, is the viability floor a founding's
//!   starvation arithmetic uses, so it is the floor any spec needing one must
//!   use. The disjunction over the roster ("could ANYONE live here") is
//!   `ore_viability_probe.rs`'s `max_cap`, reused rather than re-derived.
//!   Reading it off the bare `carrying_capacity` base field instead is a
//!   scale error; see the note at the end of this header, which is the error
//!   this probe made first.
//! - **cave-bearing land vertices** — the systems the lattice actually hangs off.
//!   Printed because it is the denominator of "chambers per system", which is
//!   what decides whether the underworld grew by acquiring more *places* or by
//!   getting *taller* in the same places.
//!
//! Ocean vertices are excluded explicitly, and ocean vertices that nonetheless carry
//! a cave are counted separately rather than folded in silently (`cave_at`
//! already refuses ocean vertices, so the count is a guard on that, and it reads
//! 0 in a healthy tree).
//!
//! # MEASURED, 2026-08-24 (post-Drift), seeds 42 / 7 / 1234
//!
//! Wall time for the whole probe (three `BuildDepth::Settlements` worlds, the
//! per-species capacity field and the full-lattice scan): **7.17 s**, warm
//! tree. The 2026-08-23 pre-Drift readings are kept below the new ones,
//! because the DIFFERENCE is this section's finding.
//!
//! ```text
//! SURFACE                        seed 42     seed 7   seed 1234     POOLED
//!   vertices (total)                  40962      40962       40962     122886
//!   land vertices                     11283      19332       11684      42299
//!   habitable land (> SURVIVE_K)   11025      15081        8234      34340
//!   cave-bearing land vertices          874       1681        1266       3821
//!
//! UNDERWORLD                     seed 42     seed 7   seed 1234     POOLED
//!   U1  band-branches / runs        4512       9353        7372      21237
//!   U2  chambers                   30537      59227       48294     138058
//!   U3  reachable chambers         30537      59227       48294     138058
//!
//! RATIO vs LAND VERTICES            seed 42     seed 7   seed 1234     POOLED
//!   U1  band-branches / runs      0.400x     0.484x      0.631x     0.502x
//!   U2  chambers                  2.706x     3.064x      4.133x     3.264x
//!   U3  reachable chambers        2.706x     3.064x      4.133x     3.264x
//!
//! RATIO vs HABITABLE LAND        seed 42     seed 7   seed 1234     POOLED
//!   U1  band-branches / runs      0.409x     0.620x      0.895x     0.618x
//!   U2  chambers                  2.770x     3.927x      5.865x     4.020x
//!   U3  reachable chambers        2.770x     3.927x      5.865x     4.020x
//! ```
//!
//! **THE SURFACE HALF DID NOT MOVE AT ALL** — 11283 / 19332 / 11684 land
//! vertices and 11025 / 15081 / 8234 habitable, byte-identical to the pre-Drift
//! run. Every ratio below moved because its numerator did, which is what makes
//! the move attributable to the underworld.
//!
//! ## THE HEADLINE: THE ROW IS NOW FALSIFIED, FULL STOP
//!
//! `BIO-underworld-has-no-energy` says the underworld is *"SMALLER than the
//! surface, not larger"*. Against land vertices, pooled, then and now:
//!
//! ```text
//!                                pre-Drift        post-Drift
//!   ALL-ENTRANCE CHAMBERS   2.361x falsified  |  }
//!   CANONICAL CHAMBERS      1.648x falsified  |  }  3.264x  falsified
//!   REACHABLE CHAMBERS      0.172x SURVIVES   |     3.264x  falsified
//!   BAND-BRANCHES / RUNS    0.253x SURVIVES   |     0.502x  survives
//! ```
//!
//! Two of the four readings merged (the aperture axis is gone) and a third
//! crossed the line: **reachability went 7.28% to 100%**, so the reading that
//! most strongly saved the row — "only 0.172x of the surface is anything a
//! player can get to" — is now identical to the reading that falsifies it.
//!
//! What is left is one surviving reading, RUNS at 0.502x, and it does not
//! save the clause: **a chamber is a place and a run is a container of
//! places.** An energy budget is claimed by the things that occupy space, and
//! a run occupies none of its own. The pre-Drift text said the row "has to
//! name one" population and left the choice open; the substrate has since
//! removed every option that would have let the clause stand.
//!
//! ## WHAT EACH EPOCH ACTUALLY CHANGED
//!
//! The pre-Drift column below is **U2a**, the canonical lattice, not U2b —
//! because the aperture summing that separated the two is exactly what got
//! deleted, so U2a is post-Drift U2's like-for-like predecessor and U2b is
//! not comparable to anything.
//!
//! ```text
//!                                   pre-Drift   post-Drift    factor
//!   chambers (U2a -> U2) pooled         69700       138058    1.981x
//!   runs (U1) pooled                    10681        21237    1.988x
//!   chambers per cave system            18.241       36.131    1.981x
//!   chambers per run                     6.526        6.501    0.996x
//!   ratio vs land vertices                 1.648x       3.264x    1.981x
//!   reachable share of chambers        7.2768%    100.0000%   13.74x
//! ```
//!
//! Every count doubles and the SHAPE does not, and there is one cause:
//! **The Drift deleted the chamber existence draw** (`c4e08ba98`, Task 1),
//! whose `EXISTENCE_DENSITY` was `0.5` — a per-address coin that had been
//! discarding half of every population the lattice admitted. `chamber_exists`
//! still carries the deleted line as a comment. Removing a fair coin
//! multiplies every count it gated by ~2 and leaves every ratio BETWEEN those
//! counts alone, which is precisely the pattern in the table: 1.981x, 1.988x,
//! 1.981x, 1.981x — and 0.996x on chambers-per-run, the one row that divides
//! two gated counts by each other.
//!
//! So, separating the two epochs:
//!
//! - **The Stope made the underworld TALLER.** A band-branch stopped being one
//!   point and became a run of ~6.5 realized levels. That factor survives The
//!   Drift unchanged (6.526 -> 6.501), which is the check that The Drift did
//!   not quietly re-cut run lengths while doing something else.
//! - **The Drift made it DENSER and CONNECTED.** Twice as much of the admitted
//!   lattice is realized, and the passage graph now reaches all of it.
//!
//! The cave systems themselves are terrain's and neither epoch touched them:
//! 874 / 1681 / 1266 across all three measurements.
//!
//! ## THE CROSS-CHECK, AND WHAT IT DOES AND DOES NOT COVER
//!
//! U2 and U3 reproduce the committed witness
//! (`docs/audits/underworld-lattice-seed-panel.md`) **exactly** on all three
//! seeds — 30537, 59227, 48294, for both counters — from a different call site
//! and a different predicate (`chamber_exists`, where the witness goes through
//! `chamber_at`). That pins the two counters carrying the finding against an
//! independently written instrument, and it is asserted, not merely printed.
//!
//! It covers U2 and U3 and **nothing else**. U1 has no committed counterpart
//! anywhere, so it is reported under the vacuity guards and the monotonicity
//! assertion (`U2 >= U1`, which can fail on a counting defect while every
//! corpus is non-empty) and with no claim of corroboration.
//!
//! **U2 and U3 being equal makes the U3 pin weaker than it looks**, and that
//! is stated rather than left for a reader to notice: while reachability is
//! total, any defect that moves U2 moves U3 by the same amount, so the two
//! pins are no longer independent evidence. They were independent pre-Drift
//! (21328 vs 1496) and would become so again the moment anything reopens a
//! gap. The `reachable <= exists` guard in the test body is the part that
//! cannot be satisfied by a stale pin.
//!
//! ## THE DENOMINATOR THIS PROBE GOT WRONG FIRST, CAUGHT BY ITS OWN GUARD
//!
//! The first draft read habitable land as `carrying_capacity(..).at(vertex) >=
//! SURVIVE_K` and measured **zero habitable land vertices on all three seeds**.
//! That is spec amendment B.5's error in the mirror: `carrying_capacity` is
//! the BASE density field (`BASE = 1.0`, a per-vertex productivity), while
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
//! The 2026-08-23 control was on `EXISTENCE_DENSITY`, which no longer exists.
//! Re-run 2026-08-24 on `LEVELS_PER_BRANCH_CEILING`, the largest remaining
//! lever, at two strengths — because the first one **did not** move the
//! headline out of its band, and that is worth recording rather than
//! replacing with the one that did:
//!
//! ```text
//! D1  chamber.rs LEVELS_PER_BRANCH_CEILING: 20 -> 10
//!     headline ratio  3.264x -> 2.899x   INSIDE the band [2.60x, 3.90x]
//!     land vertices      42299  -> 42299    UNCHANGED, on every seed
//!     => panics on the WITNESS PIN (seed 42: U2 26550 vs 30537).
//!        The ratchet alone would have MISSED this: halving the level
//!        ceiling costs only 11% of the chambers, because most runs draw
//!        shorter than 10 levels anyway, so a ±20% premise-scale band is
//!        not an instrument for detecting it. The pin is.
//! D2  chamber.rs LEVELS_PER_BRANCH_CEILING: 20 -> 4
//!     headline ratio  3.264x -> 1.752x   OUTSIDE the band, below the floor
//!     land vertices      42299  -> 42299    UNCHANGED, on every seed
//!     => panics on the WITNESS PIN first, same as D1 — the correct order,
//!        since the pin is the more specific diagnosis — and the printed
//!        POOLED table shows the ratchet's own input has left the band.
//! ```
//!
//! Tree restored after each; `git status` confirmed only this file and its
//! sibling probe modified.
//!
//! **The two controls answer different questions, and D1 is the more useful
//! one.** D2 proves the ratchet CAN fail, which is the thing a printed number
//! cannot do. D1 proves the ratchet is the WRONG instrument for a
//! mechanism-scale change and that the witness pins are carrying that half of
//! the load — a fact the pre-Drift version of this section could not have
//! reported, because its single mutation happened to move the headline 40%
//! and so made the ratchet look sharper than it is.
//!
//! **The land-vertex denominator is byte-identical across both mutations** —
//! 11283 / 19332 / 11684 every time — which is what makes the move
//! attributable to the underworld half rather than to a world that changed
//! shape. A ratio control that moved both halves would prove nothing about
//! which one the instrument is sensitive to.
//!
//! # WHAT WOULD CHANGE THE VERDICT
//!
//! **`EXISTENCE_DENSITY` is gone and is no longer one of these** — it was the
//! first entry on this list when the probe was written, and The Drift deleted
//! it. A dial named in a "what would change the verdict" list is a claim with
//! a date like any other.
//!
//! - `LEVELS_PER_BRANCH_CEILING` and `levels_in_branch`'s drawn run lengths
//!   (`windows/worldgen/src/chamber.rs`) — the *entire* difference between U1
//!   and U2, and the largest lever here now that the coin is gone. The
//!   positive control below is on the ceiling.
//! - `BRANCHES_PER_SYSTEM` and `branch_count_of`'s per-`(system, band)`
//!   weights — the lattice width.
//! - `entrance_count` — no longer a multiplier on any count, only on
//!   `entrances_drawn` and on how many mouths seed the reachability walk. A
//!   change here that moved U2 would mean the aperture axis had come back.
//! - `passages_from` and `junctions_at` — U3 alone. While reachability is
//!   total, a regression here is the only thing that can separate U3 from U2
//!   again.
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
//!
//! # RE-RUN AGAINST `main` (The Sources, Task 1, 2026-08-26)
//!
//! Harvested from `campaign/the-winze` (unmerged, 403 commits behind at the
//! time of this re-run) and re-measured against `main` at `7576eca00`, after
//! The Glasshouse's temperature re-centring. **Reproduces exactly**, n=3
//! seeds (42/7/1234), pooled: chambers 3.264x, reachable chambers 3.264x,
//! runs 0.502x — bit-for-bit the same figures the metaplan (§3.1) cites. The
//! size clause stays falsified; no doc, registry or metaplan number changed.
#![allow(clippy::disallowed_methods)]

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed, Vertex};
use hornvale_terrain::{Cave, GeothermalGradient, TerrainPins};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, LEVELS_PER_BRANCH_CEILING, chamber_exists, entrance_count,
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

/// THE HEADLINE RATCHET, floor — pooled existing chambers (U2) per land vertex.
/// See the module header for why a band and not a point: the quantity is the
/// campaign's premise, and a printed number cannot fail.
///
/// The band is roughly ±20% of the measured 3.264x — wide enough to survive
/// incidental motion in either half (the per-seed spread is itself 2.706x to
/// 4.133x), tight enough that the density mutation in the header's ledger
/// lands outside it.
const CHAMBERS_PER_LAND_VERTEX_FLOOR: f64 = 2.6;

/// THE HEADLINE RATCHET, ceiling. See [`CHAMBERS_PER_LAND_VERTEX_FLOOR`].
const CHAMBERS_PER_LAND_VERTEX_CEILING: f64 = 3.9;

/// The committed witness's `chambers` and `reachable` lines for the panel, in
/// [`SEEDS`] order — read off `docs/audits/underworld-lattice-seed-panel.md`,
/// which `scripts/regenerate-artifacts.sh` writes and the drift check pins.
///
/// Asserted rather than printed, because these two counters are what carries
/// this probe's finding and the witness is the only independent instrument
/// that measures them. A change that genuinely moves the lattice reddens the
/// drift check and this pin together, which is the intended coupling.
const WITNESS_CHAMBERS: [u64; 3] = [30537, 59227, 48294];

/// The committed witness's `reachable` counts, same source and same order.
///
/// **Identical to [`WITNESS_CHAMBERS`], and that is The Drift's headline
/// result, not a copy-paste slip** — the underworld went from 7% reachable to
/// 100%. Pinned as its own array anyway, so a future campaign that reopens a
/// gap moves one of these and not both.
const WITNESS_REACHABLE: [usize; 3] = [30537, 59227, 48294];

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
    /// Every vertex of the geosphere, ocean included.
    vertices_total: usize,
    /// Non-ocean vertices — the broad surface denominator.
    land_vertices: usize,
    /// Land vertices where at least ONE settler's per-species capacity clears
    /// [`SURVIVE_K`] — the disjunction, `ore_viability_probe.rs`'s `max_cap`.
    habitable_land_vertices: usize,
    /// Cave-bearing land vertices — the systems the lattice hangs off.
    cave_systems: usize,
    /// Ocean vertices carrying a cave. `cave_at` refuses ocean vertices, so this is
    /// a guard reading 0 in a healthy tree, counted rather than assumed.
    ocean_caves: usize,
    /// `entrance_count` summed over systems. Post-Drift this is a count of
    /// APERTURES INTO one lattice, not a multiplier on it — see `read_system`.
    entrances_drawn: u64,
    /// **U1** — existing `(vertex, branch, band, level 0)` addresses: the runs
    /// that realize at least their own first level.
    band_branches: u64,
    /// **U2** — every existing chamber of the system's one shared lattice.
    /// The Drift's amendment A.3 collapsed this campaign's former U2a
    /// (canonical, `entrance 0`) and U2b (summed over apertures) into it.
    chambers: u64,
    /// Mouths surviving `chamber_exists` — the doors actually open.
    open_mouths: usize,
    /// **U3** — chambers reachable from those mouths by `passages_from`.
    reachable_chambers: usize,
}

impl Scale {
    /// Fold another seed's reading in, for the POOLED row.
    fn merge(&mut self, o: &Scale) {
        self.vertices_total += o.vertices_total;
        self.land_vertices += o.land_vertices;
        self.habitable_land_vertices += o.habitable_land_vertices;
        self.cave_systems += o.cave_systems;
        self.ocean_caves += o.ocean_caves;
        self.entrances_drawn += o.entrances_drawn;
        self.band_branches += o.band_branches;
        self.chambers += o.chambers;
        self.open_mouths += o.open_mouths;
        self.reachable_chambers += o.reachable_chambers;
    }

    /// THE HEADLINE: existing chambers per land vertex.
    fn chambers_per_land_vertex(&self) -> f64 {
        self.chambers as f64 / self.land_vertices.max(1) as f64
    }

    /// Print every table this probe reports, under `label`.
    fn report(&self, label: &str) {
        println!("\n== {label} ==");
        println!(
            "  SURFACE     vertices {}  land {}  habitable land (> {:.1}) {}  \
             cave-bearing land {}  (ocean vertices carrying a cave: {}, excluded)",
            self.vertices_total,
            self.land_vertices,
            SURVIVE_K,
            self.habitable_land_vertices,
            self.cave_systems,
            self.ocean_caves,
        );
        println!(
            "  UNDERWORLD  U1 band-branches {}  U2 chambers {}  U3 reachable {}  \
             (entrances drawn {}, open mouths {})",
            self.band_branches,
            self.chambers,
            self.reachable_chambers,
            self.entrances_drawn,
            self.open_mouths,
        );
        for (name, n) in [
            ("U1 band-branches   ", self.band_branches as f64),
            ("U2 chambers        ", self.chambers as f64),
            ("U3 reachable cham. ", self.reachable_chambers as f64),
        ] {
            println!(
                "  RATIO {name} per land vertex {:8.3}x   per habitable land vertex \
                 {:8.3}x   per cave system {:8.3}x",
                n / self.land_vertices.max(1) as f64,
                n / self.habitable_land_vertices.max(1) as f64,
                n / self.cave_systems.max(1) as f64,
            );
        }
        println!(
            "  SHAPE  chambers per band-branch (U2/U1) {:.3}  \
             apertures per system {:.3}  reachable share of U2 {:.4}%",
            self.chambers as f64 / self.band_branches.max(1) as f64,
            self.entrances_drawn as f64 / self.cave_systems.max(1) as f64,
            100.0 * self.reachable_chambers as f64 / self.chambers.max(1) as f64,
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

/// Read one cave system's three underworld populations through the shipped
/// entry points only, folding them into `out`.
fn read_system(
    seed: Seed,
    vertex: Vertex,
    cave: &Cave,
    gradient: GeothermalGradient,
    ranks: &[u8],
    out: &mut Scale,
) {
    let entrances = entrance_count(seed, vertex);
    out.entrances_drawn += u64::from(entrances);

    // U1 / U2. The walk is the LATTICE's own ceilings
    // (`BRANCHES_PER_SYSTEM`, `LEVELS_PER_BRANCH_CEILING`, the derived rank
    // set), never a run's own drawn length — bounding it by the drawn length
    // is what made an earlier version of the committed witness unable to see
    // its own floor gate at all, and `chamber_exists` is the only adjudicator
    // here.
    //
    // **ONE lattice per system, walked once.** Before The Drift this loop had
    // an outer `for entrance in 0..entrances` and the address carried the
    // entrance, so the same system was walked once per aperture and yielded a
    // different chamber each time; that is what made U2a (canonical, `e = 0`)
    // and U2b (summed over apertures) two different numbers. The Drift's
    // amendment A.3 deleted `entrance` from `ChamberAddr`, so the two readings
    // are now one population and the walk is over `(branch, band, level)`
    // alone. `entrances_drawn` is still summed, because it still says how many
    // apertures ADDRESS that one lattice.
    for &rank in ranks {
        let band = hornvale_kernel::Band::from_rank(rank)
            .expect("habitation_ranks() yields real habitation ranks");
        for branch in 0..BRANCHES_PER_SYSTEM {
            for level in 0..LEVELS_PER_BRANCH_CEILING {
                let addr = ChamberAddr {
                    vertex,
                    branch,
                    band,
                    level,
                };
                if !chamber_exists(seed, cave, gradient, addr) {
                    continue;
                }
                out.chambers += 1;
                if level == 0 {
                    out.band_branches += 1;
                }
            }
        }
    }

    // U3 — the walk, seeded from every mouth that survives `chamber_exists`.
    // An aperture is now a WAY IN to the shared lattice rather than a lattice
    // of its own, so `entrance_mouth` is read for the coordinate it names and
    // nothing else.
    let mouths: Vec<ChamberAddr> = (0..entrances)
        .map(|e| {
            let m = entrance_mouth(seed, vertex, e);
            ChamberAddr {
                vertex,
                branch: m.branch,
                band: hornvale_kernel::Band::from_rank(m.band)
                    .expect("entrance_mouth only names a habitation rank"),
                level: m.floor,
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
/// Costs three `BuildDepth::Settlements` worlds plus a full-lattice scan —
/// the heavy tag's own reason states no duration (see its canonical form's
/// own rule), so that cost is recorded here instead.
///
/// # What would change the verdict
///
/// See the module header's list; `EXISTENCE_DENSITY`, `floors_range` and
/// `entrance_count` are the three that move it most, and they move different
/// pairs of the four populations, which is why all four are reported.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
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
                hornvale_climate::RotationRegime::Spinning {
                    day_std: day.as_std_days(),
                }
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
        for vertex in geo.vertices() {
            s.vertices_total += 1;
            if terrain.is_ocean(vertex) {
                if terrain.cave_at(vertex).is_some() {
                    s.ocean_caves += 1;
                }
                continue;
            }
            s.land_vertices += 1;
            let best = caps
                .iter()
                .map(|(_, map)| map.at(vertex))
                .fold(f64::NEG_INFINITY, f64::max);
            if best > SURVIVE_K {
                s.habitable_land_vertices += 1;
            }
            let Some(cave) = terrain.cave_at(vertex) else {
                continue;
            };
            s.cave_systems += 1;
            let gradient = terrain.geothermal_gradient_at(vertex);
            read_system(seed, vertex, &cave, gradient, &ranks, &mut s);
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
            s.land_vertices > 0,
            "seed {seed_value}: no land vertices — every surface denominator is 1 \
             by saturation and every ratio below is the numerator wearing a hat"
        );
        assert!(
            s.habitable_land_vertices > 0,
            "seed {seed_value}: no land vertex clears SURVIVE_K — the habitable \
             denominator is vacuous"
        );
        assert!(
            s.cave_systems > 0,
            "seed {seed_value}: no cave systems — the lattice is empty and every \
             U-count below is a real zero only by accident"
        );
        assert!(
            s.band_branches > 0 && s.chambers > 0,
            "seed {seed_value}: the lattice realized nothing — U1 and U2 are \
             measuring an empty world"
        );
        assert!(
            s.chambers >= s.band_branches,
            "seed {seed_value}: U2 ({}) fell below U1 ({}) — level 0 of every \
             realized band-branch is one of U2's members, so this cannot happen \
             in a world",
            s.chambers,
            s.band_branches
        );
        assert!(
            s.reachable_chambers > 0 && s.open_mouths > 0,
            "seed {seed_value}: nothing is reachable from any mouth — the walk is \
             vacuous, not reporting a real zero"
        );
        // **THE DIRECTION THIS GUARD ENFORCES IS `reachable <= exists`, AND IT
        // IS THE ONLY DIRECTION LEFT.** Its predecessor asserted the strict
        // `reachable < chambers` and diagnosed equality as "the walk is not
        // being gated by `chamber_exists` at all". The Drift falsified the
        // premise, not the code: reachability went 7% -> 100%, so equality is
        // now the HEALTHY reading on all three seeds and the old assertion
        // would red on a correct world with a wrong explanation. What cannot
        // happen in any world is the walk reaching an address the existence
        // gate refuses, so that is what is asserted. The exact per-seed
        // counts are pinned against the committed witness below; this guard
        // is the impossible-direction one, deliberately weaker and unable to
        // be satisfied by a stale pin.
        assert!(
            s.reachable_chambers <= s.chambers as usize,
            "seed {seed_value}: the walk reached {} chambers where only {} \
             exist — `passages_from` is yielding addresses `chamber_exists` \
             refuses, which is a counting defect and not a world",
            s.reachable_chambers,
            s.chambers
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
            s.chambers, WITNESS_CHAMBERS[i],
            "seed {seed_value}: U2 reads {} where the committed witness \
             `docs/audits/underworld-lattice-seed-panel.md` reports {}. Either \
             the lattice moved (the witness's drift check reddens too — \
             regenerate and re-pin here in the same commit) or this probe and \
             the readout have stopped counting the same population.",
            s.chambers, WITNESS_CHAMBERS[i]
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
    // for the positive control: `EXISTENCE_DENSITY` 0.5 -> 0.7 moves it well
    // outside the band with the land-vertex denominator untouched.
    let headline = pooled.chambers_per_land_vertex();
    assert!(
        (CHAMBERS_PER_LAND_VERTEX_FLOOR..=CHAMBERS_PER_LAND_VERTEX_CEILING).contains(&headline),
        "pooled all-entrance chambers per land vertex is {headline:.3}x ({} chambers \
         over {} land vertices), outside the band [{:.2}x, {:.2}x] this campaign's \
         premise check reported under. The three dials that move it are \
         `EXISTENCE_DENSITY`, the `floors_range` run lengths and \
         `entrance_count`; a move with none of those touched is a finding about \
         the surface half, not a bound to widen.",
        pooled.chambers,
        pooled.land_vertices,
        CHAMBERS_PER_LAND_VERTEX_FLOOR,
        CHAMBERS_PER_LAND_VERTEX_CEILING,
    );

    // The registry row's own clause. Before The Drift this block printed two
    // readings that disagreed about it and refused to pick; the substrate has
    // since picked, by deleting the axis the disagreement lived on.
    println!(
        "\n== BIO-underworld-has-no-energy, size clause ==\n  \
         counted as CHAMBERS   the underworld is {:.3}x the surface (LARGER)\n  \
         counted as REACHABLE  the underworld is {:.3}x the surface (LARGER)\n  \
         counted as RUNS       the underworld is {:.3}x the surface (smaller)\n  \
         A chamber is a PLACE and a run is a CONTAINER of places, so the two \
         readings that could once save the row's clause are down to one, and \
         it is the one that does not name a place. The clause is FALSIFIED.",
        pooled.chambers_per_land_vertex(),
        pooled.reachable_chambers as f64 / pooled.land_vertices.max(1) as f64,
        pooled.band_branches as f64 / pooled.land_vertices.max(1) as f64,
    );
}
