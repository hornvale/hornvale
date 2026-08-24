//! The underworld's committed witness (The Stope, Task 2b).
//!
//! **Why this module exists at all.** Before it, no path in
//! `docs/generated-paths.txt` carried one byte of chamber-lattice content:
//! you could have deleted every chamber from every world and `make rebaseline`
//! would have produced a byte-identical tree. Task 2's own report established
//! that two ways — a structural grep over every declared path, and the call
//! graph ([`crate::chamber::chamber_exists`]'s only production caller,
//! `made_chambers`, has no production caller of its own; the one
//! regeneration-path consumer, `windows/vessel`'s `delve_at`, is pinned at
//! `band: 0, floor: 0`). Three later tasks each end with "regenerate and see
//! what moved", and every one of them would have said "nothing moved" while
//! meaning nothing at all.
//!
//! So this renders the lattice into a committed, drift-checked artifact. It
//! is a **witness**, not a census: the numbers here are whatever the world
//! says, and no assertion in this crate holds them to a value. What holds them
//! is `git diff --exit-code` over the declared paths.
//!
//! **What it is deliberately built out of.**
//!
//! - **The shipped entry points, only.** Every existence verdict comes from
//!   [`crate::chamber::chamber_at`], every floor count from
//!   [`crate::chamber::levels_in_branch`], every passage from
//!   [`crate::chamber::passages_from`]. Nothing here re-implements a
//!   derivation, because a re-implementation cannot witness the real one —
//!   Task 2's own lesson, learned when a test that derived both legs inline
//!   let a re-parented `run_stream` through.
//! - **The address's own formatter**, spelled by `chamber::chamber_key`
//!   rather than by a presentation table of this module's own. **`chamber_key`
//!   is a display formatter, not a derivation key, since The Drift's Task 1
//!   deleted the chamber existence draw** (spec amendment A.6) — nothing in a
//!   shipped world derives from `crate::streams::CHAMBER` any more, so the
//!   `key` column below witnesses the address's SPELLING (the rung name, the
//!   field order), not a stream that moves anything. The real derivation keys
//!   are [`crate::chamber::levels_in_branch`]'s `RUN_FLOORS` leg and
//!   `crate::character`'s three per-branch legs.
//! - **Integers**, almost entirely. The only floats that cross this emit
//!   boundary are a cave's depth budget and its cell's geothermal gradient,
//!   and both go through [`hornvale_kernel::quantize`] here at the boundary —
//!   never in the compute path the lattice actually reads.
//!
//! # THE LOOP BOUND IS THE LATTICE, NEVER A DRAW — read this before editing
//!
//! The level walk is `0..LEVELS_PER_BRANCH_CEILING`, a **constant**, and it
//! must stay one. The first version of this module walked `0..drawn`, where
//! `drawn = levels_in_branch(..)` is *the same value* `chamber_exists`'s own
//! level gate compares against — so the gate could never fire on any address
//! this readout asked about, and review proved it by deleting the gate
//! outright and getting a byte-identical artifact. That mutation deletes the
//! whole of Task 2 (it is what makes a run a distribution rather than a
//! constant) and the witness did not move.
//!
//! **An instrument that derives its own loop bound from the draw it is meant
//! to witness is self-consistent by construction and cannot ask a question the
//! gate could answer differently.** Both level gates
//! ([`crate::chamber::LEVELS_PER_BRANCH_CEILING`] and the drawn length) are
//! now visible and distinguishable, and [`Tallies::past_run_length`] is the
//! sharpest reading of the second: it is 0 in a healthy tree and large the
//! moment the gate stops gating.
//!
//! # The `entrance` axis, plural since Task 5 (amendment C.3) — and OUT of
//! # the lattice since The Drift (amendment A.3)
//!
//! **The per-entrance branch/band/level walk this section used to describe is
//! GONE.** Before The Drift, `chamber_exists` keyed the branch-count gate on
//! `(cell, entrance)`, so each entrance realized its OWN private sublattice —
//! this readout walked every drawn entrance's full lattice and summed them,
//! which is exactly the shape amendment A.1/A.2 found could not express two
//! doors into ONE Spider Cave. With `entrance` gone from `ChamberAddr`, there
//! is ONE shared lattice per system, walked ONCE — see the branch/band/level
//! loop in [`render_underworld`], which no longer varies by entrance at all.
//!
//! Every system's entrance loop survives only for what C.3 says an entrance
//! actually is: **which aperture a player used**. [`entrance_count`] (keyed
//! on the cell) still says how many a system opens, and [`entrance_mouth`]
//! (keyed on `(cell, entrance)`) still says which coordinate each one opens
//! INTO — both of those coordinates now name a place in the ONE shared
//! lattice rather than a private one. `windows/vessel`'s `delve_at` is still
//! pinned at `entrance: 0` (the primary mouth, literal by C.3), so the
//! *player* sees one door; the witness deliberately sees every drawn one,
//! because its job is what the world says.
//!
//! Reachability follows C.3's union rule: the mouths of ONE system are seeds
//! into ONE shared seen set, so overlapping per-entrance components cannot
//! double-count ([`reachable_union`]) — and after The Drift this is no longer
//! merely a discipline for summing walks that happen to overlap; every mouth
//! seeds the SAME lattice by construction, so a union is the only reading
//! that was ever coherent.
//!
//! # What this witness does NOT see today, stated rather than discovered
//!
//! This header was *replaced* by Task 5's `entrance` section rather than kept
//! beside it, which left the last two bullets below sitting under a heading
//! about something else for two tasks. Restored here, because an unwitnessed
//! axis nobody wrote down is how a hole survives — that is the standing rule
//! this list exists to serve.
//!
//! - **A junction sees BRANCH 0 and nothing else — and WHERE that narrowing
//!   lives moved under this sentence** (The Drift, Task 7). It used to be
//!   `junctions_at`'s: the function projected every address of a system onto
//!   the canonical main line, so branch 0's answer WAS the system's answer
//!   and this witness could not have asked a narrower question if it tried.
//!   Spec §4.6 re-scoped `junctions_at` to `(band, branch)`, because
//!   `passages_from` no longer has a lateral rule and a door on branch 0 is
//!   not one a walker on branch 2 can reach. **So the narrowing is now this
//!   file's own choice**, made by the literal `branch: 0` in
//!   [`junction_network`], and the numbers it reports mean "the MAIN-LINE
//!   junction network" rather than "the junction network".
//!
//!   Every figure in the committed artifact is unchanged and still true
//!   under the narrower reading, and `largest` in particular is not
//!   overstated: all of this function's edges sit at branch 0, so each
//!   band's edge set is already inside one `(band, branch)` layer, which is
//!   what a traversable layer now is. **Widening it is a deliberate change
//!   to a committed artifact and Task 7 did not make it**: the union over
//!   branches is a different measurement, its component sweep would have to
//!   key on `(band, branch)` rather than on band alone (a band is no longer
//!   one traversable layer), and the result is a number a reader would
//!   compare against every previously committed panel. Stated here rather
//!   than left to be discovered.
//! - **Past-the-ladder junction behaviour is never exercised.** A band past
//!   the last habitation rung answers empty, and every loop that asks — this
//!   one and the tests' — derives its bound from the delve ladder, so no
//!   caller in the tree ever hands it an off-ladder band.
//!   `a_junction_past_the_ladder_is_empty` in
//!   `windows/worldgen/tests/suite/junctions.rs` asks it deliberately;
//!   nothing else does.
//! - **`ChamberOrigin::Made`.** The `origin` field *is* read and tallied
//!   ([`Tallies::by_origin`]), so [`crate::chamber::resolve_origin`] is wired
//!   end to end — but the override source this hands `chamber_at` is
//!   [`crate::chamber::ChamberOverrides::new`], an empty map, so `made` is 0
//!   **by construction** and will stay 0 however good Task 4's writer is.
//!   `crate::delve_seating::made_chambers` is the writer, and it needs a
//!   `History` and a seating map — both far above `BuildDepth::Terrain`, which
//!   is why this cheap readout cannot call it. **Task 4's job is to hand this
//!   function a real override source**; until it does, a `made` of 0 means
//!   "nobody asked", not "nothing was made".
//! - **"Vertical connection" no longer belongs on this list, and its absence
//!   here is itself the fact worth recording.** This bullet used to claim
//!   `passages_from` does not treat `level` as an adjacency axis, so
//!   `Tallies::reachable` counted only the entrance level's component — false
//!   even when it was written (amendment C.4, The Stope, already made level
//!   a real adjacency axis) and load-bearing false after The Drift: seed 42
//!   reads `reachable == chambers == 30537`. Severing just the band-descent
//!   step drops that to 4,148 while `chambers` stands still, which is what
//!   proves the connection is real rather than a coincidence of the walk
//!   never being asked to use it. That equality is now ASSERTED rather than
//!   reported — see `reachability_is_reported_and_equals_existence`'s own
//!   doc for why the `<=` it replaced could not fail, and why this file is
//!   where it had to be fixed.
//!
//! **Cost.** One `BuildDepth::Terrain` world per seed, and a scan of the
//! lattice over cave-bearing land cells. See `scripts/regenerate-artifacts.sh`
//! for the seed panel it is rendered over.

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{Band, Seed, quantize};
use hornvale_terrain::GeneratedTerrain;

use crate::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, ChamberOrigin, LEVELS_PER_BRANCH_CEILING, RunAddr,
    chamber_at, chamber_key, levels_in_branch, passages_from, rung_rank,
};

/// A habitation rung's spelling **in this readout's tallies** — presentation,
/// deliberately not the key's table. See [`stratum_word`] for why that
/// distinction is load-bearing rather than fussy.
///
/// **Exhaustive over [`Band`], and that is the whole point of its
/// existence.** This function is what ties the band walk below to the delve
/// ladder: a sixth rung fails THIS to compile, so the readout cannot be
/// silently left one band short of the lattice it is meant to witness.
///
/// `None` for `Surface`, which is a rung of the ladder but not a *habitation*
/// rung and has no position in a lattice of underground places — the same
/// answer `chamber::rung_rank` gives it.
fn band_word(rung: Band) -> Option<&'static str> {
    match rung {
        Band::Surface => None,
        Band::Undercroft => Some("undercroft"),
        Band::Shallows => Some("shallows"),
        Band::Deeps => Some("deeps"),
        Band::Underdeep => Some("underdeep"),
        Band::Nadir => Some("nadir"),
    }
}

/// The habitation bands this readout walks, as `(rank, word)` in rank order —
/// **derived from the delve ladder, never restated as a literal.**
///
/// # Why this is a function and not a `const 5`
///
/// It was a `const HABITATION_BANDS: u8 = 5` for one review round, and review
/// found the same defect the module doc's loop-bound section is about, one
/// axis over. `LEVELS_PER_BRANCH_CEILING` and `BRANCHES_PER_SYSTEM` are
/// imported from the lattice; the band count alone was a local literal tied
/// to nothing.
/// So a sixth delve rung would have compiled this module unchanged, the walk
/// would have kept stopping at five, and **that rung's chambers would have
/// vanished from `chambers`, from `by_band` and from every transect row with
/// no compile error and no test red** — every internal consistency assertion
/// survives it, because `by_band` still sums to `chambers` when both are
/// undercounted by the same missing band.
///
/// Two independent guards now, and they fail at different times on purpose:
///
/// 1. [`band_word`] is exhaustive over [`Band`], so a sixth variant fails
///    at COMPILE time;
/// 2. the `expect` below fails at RUN time if anyone gives `band_word` a
///    catch-all arm — a rung the lattice places but this readout has no word
///    for is an error, never a silent skip.
///
/// The rank comes from [`rung_rank`], the lattice's own one explicit mapping,
/// so what counts as a band here is by construction what counts as a band in
/// [`ChamberAddr::band`].
fn habitation_bands() -> Vec<(u8, &'static str)> {
    let mut bands: Vec<(u8, &'static str)> = hornvale_terrain::rungs()
        .iter()
        .filter_map(|&rung| {
            let rank = rung_rank(rung)?;
            let word = band_word(rung).expect(
                "a rung the lattice gives a band rank must have a word in this \
                 readout — band_word is exhaustive over Band so that this \
                 cannot be reached by adding a variant, only by adding a \
                 catch-all arm",
            );
            Some((rank, word))
        })
        .collect();
    // Rank order, not ladder-declaration order: `by_band`'s slots are indexed
    // by rank, and nothing promises `rungs()` is already sorted that way.
    bands.sort_by_key(|&(rank, _)| rank);
    bands
}

/// The five subterranean rock units' spellings, in [`rock_rank`]'s order.
const ROCK_WORDS: [&str; 5] = ["regolith", "cover", "basement", "roots", "underneath"];

/// How many cave systems the per-system transect walks in full. Three: enough
/// that the section shows a cave system rather than an anecdote, few enough
/// that the artifact stays reviewable by eye at 20 rows apiece.
const TRANSECT_SYSTEMS: usize = 3;

/// The glyph for a floor that exists — read from the shipped answer, and
/// tested **first**, which is what makes the two refusal glyphs below unable
/// to hide a gate that stopped gating.
const GLYPH_EXISTS: char = '#';

/// The glyph for a floor inside its run's drawn length that was refused
/// anyway — by the rock's own depth budget, which is the one gate left that
/// can do it.
///
/// **It used to mean "the existence draw refused it", and that draw is
/// gone** (The Drift, spec §4.1). `chamber_exists` now admits every address
/// inside its five structural gates, so within a run's drawn length the only
/// remaining refusal is `band > rung_rank(rung_at_depth(..))` — a cave whose
/// depth budget stops short of this band. The glyph is kept, and kept
/// distinct from [`GLYPH_PAST_RUN`] and [`GLYPH_PAST_BRANCH`], because the
/// three refusals still have three different causes.
const GLYPH_REFUSED: char = '.';

/// The glyph for a floor **past** its run's drawn length — inside the
/// lattice's own ceiling, so this readout still asks about it, and refused.
/// A gate that stopped gating turns these into [`GLYPH_EXISTS`], which is
/// exactly the movement review's mutation proved the first version of this
/// module could not produce.
///
/// **"Roughly half of these" was right and is now wrong, and the direction
/// matters: this readout got MORE sensitive, not less.** The old sentence
/// was counting on the 0.5 per-address existence coin surviving the run gate
/// as well, so a broken run gate exposed a floor that then had only even
/// odds of showing up as `#`. The Drift deleted that coin (spec §4.1), so a
/// run gate that stopped gating now turns **every** one of these into
/// [`GLYPH_EXISTS`]. The mutation this glyph exists to be caught by moves
/// twice as much of the artifact as it did when the claim was written.
const GLYPH_PAST_RUN: char = '_';

/// The glyph for a floor of a **branch its system never realized** — past
/// [`crate::character::branch_count_of`]'s drawn count, inside the lattice's
/// own ceiling, so this readout still asks about it, and refused (spec C.1).
/// A distinct glyph rather than reusing [`GLYPH_REFUSED`] because the two
/// refusals have different causes and different gates: a branch gate that
/// stopped gating turns these into [`GLYPH_EXISTS`], exactly as the run
/// gate's mutation does to [`GLYPH_PAST_RUN`] — which is why unrealized
/// branches are still walked rather than skipped.
const GLYPH_PAST_BRANCH: char = '~';

/// A stratum's spelling **in this readout** — presentation, deliberately not
/// the save-format table.
///
/// `chamber.rs`'s `rung_name` is a save-format contract precisely because it
/// is hashed into a derivation key, and reusing such a table for display would
/// make a display rename an epoch. This one is the opposite: it names rock for
/// a reader, and may be reworded freely (at the cost of an artifact diff, which
/// is the point of a committed artifact).
///
/// Exhaustive over every [`hornvale_climate::Stratum`], including the marine
/// and surface registers a chamber can never sit in, so a sixth rock unit
/// fails this to compile rather than inheriting a neighbour's word.
fn stratum_word(stratum: hornvale_climate::Stratum) -> &'static str {
    match stratum {
        hornvale_climate::Stratum::Surface => "surface",
        hornvale_climate::Stratum::Epipelagic => "epipelagic",
        hornvale_climate::Stratum::Mesopelagic => "mesopelagic",
        hornvale_climate::Stratum::Bathypelagic => "bathypelagic",
        hornvale_climate::Stratum::Abyssal => "abyssal",
        hornvale_climate::Stratum::Hadal => "hadal",
        hornvale_climate::Stratum::Regolith => "regolith",
        hornvale_climate::Stratum::Cover => "cover",
        hornvale_climate::Stratum::Basement => "basement",
        hornvale_climate::Stratum::Roots => "roots",
        hornvale_climate::Stratum::Underneath => "underneath",
    }
}

/// The index [`stratum_word`]'s five subterranean units occupy in the by-rock
/// tallies below, top down. `None` for the marine and surface registers, which
/// no chamber can sit in — a chamber that somehow reported one would be a real
/// finding, so it is counted separately rather than silently dropped.
fn rock_rank(stratum: hornvale_climate::Stratum) -> Option<usize> {
    match stratum {
        hornvale_climate::Stratum::Regolith => Some(0),
        hornvale_climate::Stratum::Cover => Some(1),
        hornvale_climate::Stratum::Basement => Some(2),
        hornvale_climate::Stratum::Roots => Some(3),
        hornvale_climate::Stratum::Underneath => Some(4),
        _ => None,
    }
}

/// One cave system's run in the lattice: the key its level 0 derives from,
/// how many levels the run drew, and what the shipped path answered at
/// **every** level the lattice admits — not merely the drawn ones.
struct RunRow {
    /// `chamber_key` of this run's level 0 — a display formatter, not a
    /// derivation key (spec amendment A.6; see [`chamber_key`]'s own doc), so
    /// this witnesses the address's SPELLING (the epoch label's consequences,
    /// the rung name, the key's field order) rather than a stream that moves
    /// anything.
    key: String,
    /// The level count [`levels_in_branch`] drew for this run, printed as its
    /// own value. It is **reported**, never used as this row's loop bound:
    /// see the module doc's loop-bound section for what happened when it was.
    drawn: u8,
    /// One glyph per level of the lattice's own ceiling, in level order,
    /// always [`LEVELS_PER_BRANCH_CEILING`] characters long:
    /// [`GLYPH_EXISTS`] / [`GLYPH_PAST_RUN`] / [`GLYPH_REFUSED`].
    realized: String,
    /// The rock the run's chambers sit in, read off the first realized
    /// chamber's own [`crate::chamber::Chamber::stratum`]. `"-"` when the run
    /// realizes no chamber at all — there is then no shipped content to read,
    /// and inventing one from the address would restate the address rather
    /// than witness the world.
    rock: &'static str,
}

/// Everything the readout tallies for one world.
struct Tallies {
    /// Cave-bearing land cells — one cave system apiece.
    systems: usize,
    /// Cave-bearing **ocean** cells, which this readout skips.
    ///
    /// Counted rather than silently dropped, for the same reason
    /// [`Tallies::off_ladder_rock`] is: spec B.1 measures this at zero on all
    /// three panel seeds today, and "X is on land" has been wrong in this
    /// codebase before. A skip nobody counts is indistinguishable from an
    /// absence.
    ocean_systems: usize,
    /// Realized chambers, over the whole lattice of every cave system.
    chambers: usize,
    /// Realized chambers per band, in [`habitation_bands`] order.
    by_band: Vec<usize>,
    /// Realized chambers per rock unit, `regolith` … `underneath`.
    by_rock: [usize; 5],
    /// Realized chambers per [`ChamberOrigin`], `[found, made]`.
    ///
    /// `made` is 0 **by construction** while the override source is empty —
    /// see the module doc. Read and printed anyway, because that is what puts
    /// the field on a wire Task 4 can move rather than leaving it for Task 4's
    /// review to discover it never was on one.
    by_origin: [usize; 2],
    /// Realized chambers reporting a stratum no chamber should be able to sit
    /// in (a marine or surface register). Expected 0; printed regardless,
    /// because a silently dropped anomaly is not a witness.
    off_ladder_rock: usize,
    /// **Realized chambers at a level past their own run's drawn length** —
    /// the direct reading of `chamber_exists`'s drawn-level gate.
    ///
    /// 0 in a healthy tree, and large the moment that gate stops gating. This
    /// is the counter review's gate-deletion mutation could not move in the
    /// first version of this module, because that version never asked about a
    /// level past the drawn length at all.
    past_run_length: usize,
    /// Realized chambers at a **branch past their system's drawn branch
    /// count** — the direct reading of `chamber_exists`'s branch-count gate
    /// (spec C.1). 0 in a healthy tree, for the same reason
    /// [`Tallies::past_run_length`] is.
    past_branch_count: usize,
    /// Levels drawn across every run of every cave system, whether or not the
    /// band is inside a cave's budget. Separates "the run draw moved" from
    /// "the reach moved": the first moves this, the second does not.
    drawn_floors: usize,
    /// Entrances DRAWN across every cave system — the sum of
    /// [`crate::chamber::entrance_count`] over all systems, reported so a
    /// count draw that degenerated to 1 everywhere is visible as this figure
    /// collapsing onto [`Tallies::systems`].
    entrances: usize,
    /// Cave systems that drew MORE than one entrance (C.3's plural). Expected
    /// nonzero on any healthy panel; 0 means the count draw is degenerate.
    multi_entrance_systems: usize,
    /// Chambers **reachable from an entrance** by [`passages_from`], summed
    /// over every cave system whose entrance chamber exists.
    ///
    /// The quantity a player actually experiences, and the one an existence
    /// count cannot see: a chamber can exist and have nowhere to descend from
    /// or into. Amendment C.4 is about exactly this, so it is printed here for
    /// Task 3b to move.
    reachable: usize,
    /// Cave systems with at least one drawn open mouth — every drawn
    /// entrance whose mouth is adjudicated open by `chamber_at`, not merely
    /// witnessed entrance 0. [`Tallies::reachable`]'s denominator, without
    /// which a fallen reachability figure cannot be told from fewer open
    /// entrances.
    open_entrances: usize,
}

/// Chambers reachable from ANY of `mouths` by [`passages_from`], mouths
/// included.
///
/// **One shared seen set across every mouth** — the union of the per-entrance
/// components, never the sum. Two breadth-first walks from two mouths of one
/// system overlap wherever their components meet — since The Drift (amendment
/// A.3) every mouth addresses INTO the same shared lattice by construction,
/// so a union is the only reading that was ever coherent; summing
/// per-entrance counts would double-count exactly those chambers and
/// overstate the world.
///
/// A plain flood-fill walk over the shipped adjacency function: nothing
/// here knows the adjacency rule, which is the point — Task 3b changes that
/// rule and this number moves without this function being touched.
///
/// `BTreeSet`, never a `HashSet` (workspace rule); the walk order is not
/// observed, only the final count is.
fn reachable_union(
    seed: Seed,
    cave: &hornvale_terrain::Cave,
    gradient: hornvale_terrain::GeothermalGradient,
    mouths: &[ChamberAddr],
) -> usize {
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
    seen.len()
}

/// The junction network between cave systems (The Stope, Task 6), read
/// through [`crate::chamber::junctions_at`] — the shipped derivation, never a
/// re-implementation. Every fact here is derived; nothing consumes a stream
/// leg of its own.
///
/// Returns `(links, system pairs, largest single-band component)`.
///
/// **`links` counts EDGES, not answers** (review round 1). It was the ordered
/// count for one commit: `junctions_at` answers from both endpoints, so every
/// edge was tallied twice while `pairs` divided by two, and the artifact read
/// "1844 links across 735 system pairs" — inviting 2.5 links per pair when
/// the truth was 1.25. The fix is a set of unordered `(band, lo, hi)` keys
/// rather than a division, because a set is right by construction even if
/// symmetry ever broke; halving an ordered count is correct only under an
/// invariant this function does not itself check, and this campaign has been
/// bitten twice by numbers that were only right under an unstated one.
///
/// **`largest` is the largest component WITHIN a single band, and that is the
/// only such figure a delve could spend.** A junction never crosses a band
/// (Task 6's third constraint), so collapsing all five bands into one
/// adjacency map before the sweep — which the first version did — puts
/// systems joined only at the Undercroft in the same "network" as systems
/// joined only at the Nadir, with no route between them. Measured on seed 42:
/// the union reported **57**, no single band exceeded **27**, and **not one**
/// of the 735 pairs joined at every band. The word *walkable* is deliberately
/// gone from this doc — it was what made a union across band-layers read as a
/// traversable component, and a number in the costume of a measurement is
/// this campaign's signature defect.
fn junction_network(seed: Seed, terrain: &GeneratedTerrain) -> (usize, usize, usize) {
    // Unordered `(lo, hi)` cell pairs, keyed by the band they join at. The
    // per-band split is not presentation: it is what keeps the component
    // sweep below inside one traversable layer.
    let mut by_band: BTreeMap<u8, BTreeSet<(hornvale_kernel::Vertex, hornvale_kernel::Vertex)>> =
        BTreeMap::new();
    for cell in terrain.geosphere().vertices() {
        // `cave_at` refuses an ocean cell as its first act, so it is the land
        // gate too; an `is_ocean` test beside it could never fire.
        if terrain.cave_at(cell).is_none() {
            continue;
        }
        for (rank, _) in habitation_bands() {
            let band = hornvale_kernel::Band::from_rank(rank)
                .expect("habitation_bands() yields real ranks");
            let addr = ChamberAddr {
                cell,
                branch: 0,
                band,
                level: 0,
            };
            for far in crate::chamber::junctions_at(seed, terrain, addr) {
                let pair = if cell < far.cell {
                    (cell, far.cell)
                } else {
                    (far.cell, cell)
                };
                by_band.entry(rank).or_default().insert(pair);
            }
        }
    }
    let links: usize = by_band.values().map(BTreeSet::len).sum();
    let pairs: usize = by_band
        .values()
        .flatten()
        .copied()
        .collect::<BTreeSet<_>>()
        .len();
    let largest = by_band.values().map(largest_component).max().unwrap_or(0);
    (links, pairs, largest)
}

/// The size of the largest connected component of an undirected edge set,
/// by breadth-first sweep. `visited` is a `BTreeSet`, not a hash set:
/// determinism is constitutional even where iteration order cannot change
/// the answer.
fn largest_component(
    edges: &BTreeSet<(hornvale_kernel::Vertex, hornvale_kernel::Vertex)>,
) -> usize {
    let mut adjacency: BTreeMap<hornvale_kernel::Vertex, BTreeSet<hornvale_kernel::Vertex>> =
        BTreeMap::new();
    for &(lo, hi) in edges {
        adjacency.entry(lo).or_default().insert(hi);
        adjacency.entry(hi).or_default().insert(lo);
    }
    let mut visited: BTreeSet<hornvale_kernel::Vertex> = BTreeSet::new();
    let mut largest = 0usize;
    for start in adjacency.keys().copied().collect::<Vec<_>>() {
        if visited.contains(&start) {
            continue;
        }
        let mut component = vec![start];
        visited.insert(start);
        let mut i = 0;
        while i < component.len() {
            let cell = component[i];
            i += 1;
            for &next in &adjacency[&cell] {
                if visited.insert(next) {
                    component.push(next);
                }
            }
        }
        largest = largest.max(component.len());
    }
    largest
}

/// Render one world's chamber lattice as the committed underworld witness.
///
/// The world must be built to at least `BuildDepth::Terrain`; a chamber needs
/// a cave's depth budget, its cell's geothermal gradient and its cell's
/// stratigraphic column, and nothing above terrain.
///
/// **Byte-identical for a given `(seed, terrain)`**, and asserted as such
/// rather than observed — see this module's tests. No wall clock, no map
/// iteration order, no float in the compute path.
///
/// **Do not insert a function into this doc block.** One review round found
/// [`junction_network`] wedged between its two halves, leaving the
/// byte-identity promise — the single most load-bearing sentence about this
/// artifact — attached to a private helper while the `pub` function it
/// describes carried the trailing clause. Both compiled, and both satisfied
/// `#![warn(missing_docs)]`, which is exactly why nothing caught it.
/// type-audit: bare-ok(prose: return)
pub fn render_underworld(seed: Seed, terrain: &GeneratedTerrain) -> String {
    let mut out = String::new();
    let mut tallies = Tallies {
        systems: 0,
        ocean_systems: 0,
        chambers: 0,
        by_band: habitation_bands().iter().map(|_| 0).collect(),
        by_rock: [0; 5],
        by_origin: [0; 2],
        off_ladder_rock: 0,
        past_run_length: 0,
        past_branch_count: 0,
        drawn_floors: 0,
        entrances: 0,
        multi_entrance_systems: 0,
        reachable: 0,
        open_entrances: 0,
    };
    // The first `TRANSECT_SYSTEMS` cave systems in cell order, walked in full.
    let mut transect: Vec<(hornvale_kernel::Vertex, String, Vec<RunRow>)> = Vec::new();
    // THE OVERRIDE SOURCE, named rather than inlined: this is the one line
    // Task 4 replaces to put `ChamberOrigin::Made` into the artifact. Empty
    // today, so `by_origin[1]` is 0 by construction — see the module doc.
    let overrides = crate::chamber::ChamberOverrides::new();

    for cell in terrain.geosphere().vertices() {
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        if terrain.is_ocean(cell) {
            // Counted, not silently dropped. Zero on all three panel seeds
            // today; the day it is not, the artifact says so.
            tallies.ocean_systems += 1;
            continue;
        }
        tallies.systems += 1;
        let gradient = terrain.geothermal_gradient_at(cell);
        let column = terrain.column_at(cell);
        let want_transect = transect.len() < TRANSECT_SYSTEMS;
        let mut rows: Vec<RunRow> = Vec::new();

        // ONE shared lattice per system, walked ONCE (The Drift, amendment
        // A.3) — before this campaign each of the system's drawn entrances
        // realized its OWN private sublattice here, and this loop walked it
        // once per entrance; with `entrance` gone from `ChamberAddr` there is
        // exactly one lattice to walk, however many doors open into it.
        //
        // The system's drawn branch width, REPORTED against, never used as a
        // loop bound: every branch the lattice admits is still walked, so a
        // branch gate that stopped gating shows up as [`GLYPH_EXISTS`] where
        // [`GLYPH_PAST_BRANCH`] belongs — the same falsifiability rule the
        // level walk follows.
        //
        // **Read per BAND, not once per system** (The Drift, Task 5):
        // `branch_count_of` is now keyed on `(cell, band)`, so a system can
        // realize a different branch width at each band — the width used
        // below must match the band the gate below is actually reporting
        // against, exactly as `chamber_exists` itself reads `addr.band`.
        for branch in 0..BRANCHES_PER_SYSTEM {
            for (rank, _) in habitation_bands() {
                let band = hornvale_kernel::Band::from_rank(rank)
                    .expect("habitation_bands() yields real ranks");
                let realized_branches = crate::character::branch_count_of(seed, cell, band);
                let run = RunAddr { cell, branch, band };
                let drawn = levels_in_branch(seed, run);
                tallies.drawn_floors += usize::from(drawn);

                let mut realized = String::new();
                let mut rock = "-";
                // THE LATTICE'S OWN CEILING, never `drawn`. See the module
                // doc: bounding this walk by the draw made the drawn-level
                // gate unfalsifiable.
                for level in 0..LEVELS_PER_BRANCH_CEILING {
                    let addr = ChamberAddr {
                        cell,
                        branch,
                        band,
                        level,
                    };
                    match chamber_at(seed, &cave, gradient, &column, addr, &overrides) {
                        // The EXISTS arm is first, so no refusal glyph
                        // computed from `drawn` can mask a chamber the
                        // shipped path admitted past it.
                        Some(chamber) => {
                            realized.push(GLYPH_EXISTS);
                            tallies.chambers += 1;
                            tallies.by_band[usize::from(rank)] += 1;
                            if level >= drawn {
                                tallies.past_run_length += 1;
                            }
                            if branch >= realized_branches {
                                tallies.past_branch_count += 1;
                            }
                            match rock_rank(chamber.stratum) {
                                Some(rank) => tallies.by_rock[rank] += 1,
                                None => tallies.off_ladder_rock += 1,
                            }
                            match chamber.origin {
                                ChamberOrigin::Found => tallies.by_origin[0] += 1,
                                ChamberOrigin::Made => tallies.by_origin[1] += 1,
                            }
                            if rock == "-" {
                                rock = stratum_word(chamber.stratum);
                            }
                        }
                        None if level >= drawn => realized.push(GLYPH_PAST_RUN),
                        None if branch >= realized_branches => realized.push(GLYPH_PAST_BRANCH),
                        None => realized.push(GLYPH_REFUSED),
                    }
                }

                if want_transect {
                    rows.push(RunRow {
                        key: chamber_key(ChamberAddr {
                            cell,
                            branch,
                            band,
                            level: 0,
                        }),
                        drawn,
                        realized,
                        rock,
                    });
                }
            }
        }

        // THE SYSTEM'S DRAWN ENTRANCE COUNT, never a module constant: the
        // same falsifiability rule the level walk follows. A count draw that
        // stopped counting must shrink the WORLD, not this witness's view of
        // it — bounding by a const would have made the two move together and
        // the drift invisible. Entrances survive The Drift only as WHICH
        // APERTURE a player used (amendment A.3): each one still resolves to
        // a coordinate in the ONE shared lattice walked above.
        let drawn_entrances = crate::chamber::entrance_count(seed, cell);
        tallies.entrances += usize::from(drawn_entrances);
        if drawn_entrances > 1 {
            tallies.multi_entrance_systems += 1;
        }
        let mut mouths: Vec<ChamberAddr> = Vec::new();
        for entrance in 0..drawn_entrances {
            // This entrance's own mouth (C.3): entrance 0 is the main line's
            // head, every other opens into its drawn branch root — but the
            // mouth ADDRESS in the lattice is what reachability seeds on,
            // so it is read from the shipped mapping rather than restated.
            let mouth = crate::chamber::entrance_mouth(seed, cell, entrance);
            let band = hornvale_kernel::Band::from_rank(mouth.band)
                .expect("entrance_mouth only names a habitation rank");
            let entry = ChamberAddr {
                cell,
                branch: mouth.branch,
                band,
                level: mouth.floor,
            };
            if chamber_at(seed, &cave, gradient, &column, entry, &overrides).is_some() {
                tallies.open_entrances += 1;
                // Only an OPEN mouth seeds the walk: a shut door contributes
                // nothing to what a player can reach, and seeding it anyway
                // would count the doorway itself as reached.
                mouths.push(entry);
            }
        }

        // ONE union per system: every open mouth seeds one shared seen set,
        // so overlapping components cannot double-count (C.3).
        tallies.reachable += reachable_union(seed, &cave, gradient, &mouths);

        if want_transect {
            // Quantized at the emit boundary and nowhere else: the lattice
            // itself reads `cave.depth_reach_m` and the gradient at full
            // precision, exactly as every consumer does.
            let head = format!(
                "{} cave, reach {} m, gradient {} K/km",
                cave.kind.name(),
                quantize(cave.depth_reach_m),
                quantize(gradient.get())
            );
            transect.push((cell, head, rows));
        }
    }

    out.push_str(&format!("seed {}\n", seed.0));
    out.push_str(&format!(
        "  derivation      {} over {}\n",
        crate::streams::CHAMBER.as_str(),
        crate::streams::RUN_FLOORS.as_str()
    ));
    out.push_str(&format!(
        "  lattice         {BRANCHES_PER_SYSTEM} branches per system, {} bands, \
         {LEVELS_PER_BRANCH_CEILING} levels admitted per run\n",
        habitation_bands().len()
    ));
    out.push_str(&format!(
        "  entrances       {} drawn across {} systems, {} multi-entrance \
         ({})\n",
        tallies.entrances,
        tallies.systems,
        tallies.multi_entrance_systems,
        crate::streams::ENTRANCE_COUNT.as_str(),
    ));
    out.push_str(&format!(
        "  cave systems    {}  (ocean-cell caves skipped: {})\n",
        tallies.systems, tallies.ocean_systems
    ));
    out.push_str(&format!("  floors drawn    {}\n", tallies.drawn_floors));
    out.push_str(&format!("  chambers        {}\n", tallies.chambers));
    out.push_str(&format!(
        "  reachable       {} from {} open entrances\n",
        tallies.reachable, tallies.open_entrances
    ));
    let (links, pairs, largest) = junction_network(seed, terrain);
    out.push_str(&format!(
        "  junctions       {} links across {} system pairs; largest network {} systems \
         at one band (MAP-underworld-shortcut)\n",
        links, pairs, largest
    ));
    out.push_str("  by band         ");
    for ((_, word), count) in habitation_bands().iter().zip(&tallies.by_band) {
        // Presentation words, not the key's table (see `stratum_word`'s doc):
        // the save-format spelling of a rung reaches this artifact through the
        // transect's keys, which is the authority. These may be reworded.
        out.push_str(&format!("{word}:{count}  "));
    }
    out.push('\n');
    out.push_str("  by rock         ");
    for (rank, count) in tallies.by_rock.iter().enumerate() {
        let word = ROCK_WORDS[rank];
        out.push_str(&format!("{word}:{count}  "));
    }
    out.push_str(&format!("off-ladder:{}\n", tallies.off_ladder_rock));
    out.push_str(&format!(
        "  by origin       found:{}  made:{}\n",
        tallies.by_origin[0], tallies.by_origin[1]
    ));
    out.push_str(&format!(
        "  past run length {}   (chambers beyond their run's drawn floors)\n",
        tallies.past_run_length
    ));
    out.push_str(&format!(
        "  past branch cnt {}   (chambers beyond their system's drawn branches)\n",
        tallies.past_branch_count
    ));

    out.push_str("\n  the first three cave systems, run by run\n");
    out.push_str(&format!(
        "  (key = the floor-0 address's spelling, not a derivation key; \
         {GLYPH_EXISTS} exists, {GLYPH_REFUSED} refused, {GLYPH_PAST_RUN} past \
         the run's drawn floors, {GLYPH_PAST_BRANCH} past the system's drawn \
         branch count)\n"
    ));
    for (cell, head, rows) in &transect {
        out.push_str(&format!("\n  cell {} — {head}\n", cell.0));
        for row in rows {
            out.push_str(&format!(
                "    {:<28} {:<11} {:>2} floors  {}\n",
                row.key, row.rock, row.drawn, row.realized
            ));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_terrain::TerrainPins;

    /// Build one seed to `BuildDepth::Terrain`, the shallowest rung a chamber
    /// needs.
    ///
    /// Test fixture (decision 0092): calls the composition-root entry points
    /// directly, the sanctioned posture for this crate's live-worldgen
    /// batteries.
    #[allow(clippy::disallowed_methods)]
    fn terrain_for(seed: Seed) -> GeneratedTerrain {
        let wc = crate::WorldComponents::assemble().expect("canonical registries are well-formed");
        crate::build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            crate::SkyChoice::Generated,
            &TerrainPins::default(),
            &crate::SettlementPins::default(),
            &wc,
            crate::BuildDepth::Terrain,
        )
        .expect("the probe seed builds")
        .terrain
        .expect("terrain is Some at BuildDepth::Terrain")
    }

    /// The glyph run of every transect row in `text` — everything after the
    /// `" floors  "` column.
    fn glyph_rows(text: &str) -> Vec<&str> {
        text.lines()
            .filter_map(|line| line.split(" floors  ").nth(1))
            .collect()
    }

    /// **The readout is byte-identical across independent builds of the same
    /// seed** — the property the committed artifact's whole drift check rests
    /// on, asserted rather than left for a human to notice when a regeneration
    /// churns for no reason.
    ///
    /// Two SEPARATE builds, not one build rendered twice: rendering one build
    /// twice would only catch a nondeterministic *render* (an iteration order,
    /// a wall clock), and would be blind to a nondeterministic *world*. Both
    /// are failures of the same guarantee, and this artifact is committed, so
    /// both must be excluded.
    #[test]
    fn the_readout_is_byte_identical_across_two_builds_of_one_seed() {
        let seed = Seed(42);
        let first = render_underworld(seed, &terrain_for(seed));
        let second = render_underworld(seed, &terrain_for(seed));
        assert_eq!(
            first, second,
            "two independent builds of seed 42 rendered different underworld \
             readouts, so the committed artifact cannot be drift-checked"
        );
    }

    /// The readout is **non-vacuous**: it reports cave systems, it reports
    /// chambers, and it reports a transect with a realized floor in it.
    ///
    /// Without this, every claim the artifact makes would be satisfied by a
    /// render that found nothing — and "nothing moved" would then be exactly
    /// as uninformative as it was before this module existed, which is the
    /// defect Task 2b was inserted to fix. Asserted as bounds, never as
    /// values: the values live in the committed artifact, where a diff is the
    /// right instrument for them.
    #[test]
    fn the_readout_witnesses_a_populated_underworld() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));

        let systems: usize = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("cave systems").map(str::trim))
            .expect("the readout reports a cave-system count")
            .split_whitespace()
            .next()
            .expect("the cave-system line leads with its count")
            .parse()
            .expect("the cave-system count is a number");
        assert!(systems > 0, "seed 42 rendered no cave system at all");

        let chambers: usize = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("chambers").map(str::trim))
            .expect("the readout reports a chamber count")
            .parse()
            .expect("the chamber count is a number");
        assert!(chambers > 0, "seed 42 rendered no chamber at all");

        assert!(
            text.contains(GLYPH_EXISTS),
            "no transect row shows a realized floor, so the per-system section \
             witnesses nothing"
        );
        assert!(
            text.contains(crate::streams::CHAMBER.as_str()),
            "the readout does not name the epoch it was derived under"
        );
    }

    /// **THE FLOOR WALK IS BOUNDED BY THE LATTICE, NOT BY THE DRAW** — the
    /// regression guard for the defect review found, stated as a property of
    /// the rendered text so it holds whatever the loop is rewritten into.
    ///
    /// Two arms, and the second is what makes the first mean something:
    ///
    /// 1. every transect row's glyph run is exactly
    ///    [`LEVELS_PER_BRANCH_CEILING`] long — a row bounded by its own `drawn`
    ///    would be shorter, and `drawn` is at most the ceiling and usually far
    ///    under it;
    /// 2. at least one row actually carries [`GLYPH_PAST_RUN`], i.e. the
    ///    readout really did ask about a floor past the drawn length and got a
    ///    refusal. Arm 1 alone would pass if every run happened to draw the
    ///    full ceiling; arm 2 says the interesting region is genuinely being
    ///    probed.
    ///
    /// With both, deleting `chamber_exists`'s drawn-floor gate has to move the
    /// artifact: the glyphs at those positions are decided by the shipped
    /// answer, tested before either refusal glyph.
    #[test]
    fn the_transect_walks_the_lattice_ceiling_rather_than_the_drawn_count() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let rows = glyph_rows(&text);
        assert!(!rows.is_empty(), "the transect rendered no row at all");

        for row in &rows {
            assert_eq!(
                row.chars().count(),
                usize::from(LEVELS_PER_BRANCH_CEILING),
                "a transect row is {} glyphs long, not the lattice ceiling of \
                 {LEVELS_PER_BRANCH_CEILING} — the floor walk is bounded by \
                 something other than the lattice, which is how the drawn-floor \
                 gate became unfalsifiable once already: {row:?}",
                row.chars().count()
            );
        }
        assert!(
            rows.iter().any(|r| r.contains(GLYPH_PAST_RUN)),
            "no transect row probed a floor past its run's drawn length, so \
             the drawn-floor gate is never exercised by this artifact"
        );
    }

    /// **A chamber past its run's drawn length is a gate failure, and the
    /// readout reports it as a number** — 0 in a healthy tree.
    ///
    /// Asserted here as well as printed, because this is the one tally whose
    /// healthy value is known in advance: `chamber_exists` refuses every floor
    /// at or past `levels_in_branch`, so any nonzero reading is that gate not
    /// gating, not a world being unusual.
    #[test]
    fn no_chamber_exists_past_its_runs_drawn_length() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let past: usize = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("past run length").map(str::trim))
            .expect("the readout reports a past-run-length count")
            .split_whitespace()
            .next()
            .expect("the past-run-length line leads with its count")
            .parse()
            .expect("the past-run-length count is a number");
        assert_eq!(
            past, 0,
            "{past} chambers exist at a floor past their run's drawn length — \
             chamber_exists's drawn-floor gate is not gating"
        );
    }

    /// **The transect really draws past-run glyphs** — the non-vacuity arm
    /// for [`GLYPH_PAST_RUN`] as a rendered fact: at least one row on the
    /// panel seed carries it, so a change that stops emitting it (a glyph
    /// swap, a loop rewrite) cannot pass silently behind rows that all
    /// happen to be refusals of another kind.
    #[test]
    fn some_transect_row_carries_the_past_run_glyph() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let rows = glyph_rows(&text);
        assert!(!rows.is_empty(), "the transect rendered no row at all");
        assert!(
            rows.iter().any(|r| r.contains(GLYPH_PAST_RUN)),
            "no transect row contains the {} glyph — the readout never renders \
             a past-run position, so anything keyed to it is untested",
            GLYPH_PAST_RUN
        );
    }

    /// **The transect really draws past-branch glyphs** — the non-vacuity
    /// arm for [`GLYPH_PAST_BRANCH`] as a rendered fact, the branch-count
    /// counterpart of [`some_transect_row_carries_the_past_run_glyph`]: at
    /// least one row on the panel seed carries it, so a change that stops
    /// emitting it cannot pass silently behind rows that all happen to be
    /// refusals of another kind.
    #[test]
    fn some_transect_row_carries_the_past_branch_glyph() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let rows = glyph_rows(&text);
        assert!(!rows.is_empty(), "the transect rendered no row at all");
        assert!(
            rows.iter().any(|r| r.contains(GLYPH_PAST_BRANCH)),
            "no transect row contains the {} glyph — the readout never renders \
             a past-branch position, so anything keyed to it is untested",
            GLYPH_PAST_BRANCH
        );
    }

    /// **A chamber past its system's drawn branch count is a gate failure,
    /// and the readout reports it as a number** — 0 in a healthy tree, the
    /// branch-count counterpart of `no_chamber_exists_past_its_runs_drawn_length`:
    /// `chamber_exists` refuses every band and floor of a column past its
    /// system's drawn branches, so any nonzero reading is that gate not
    /// gating, not a world being unusual.
    #[test]
    fn no_chamber_exists_past_its_systems_drawn_branches() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let past: usize = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("past branch cnt").map(str::trim))
            .expect("the readout reports a past-branch-count tally")
            .split_whitespace()
            .next()
            .expect("the past-branch-count line leads with its count")
            .parse()
            .expect("the past-branch-count tally is a number");
        assert_eq!(
            past, 0,
            "{past} chambers exist past their system's drawn branch count — \
             chamber_exists's drawn-branch gate is not gating"
        );
    }

    /// **Reachability is reported, is nonzero, and EQUALS existence** — the
    /// campaign's headline, asserted in the one place a merge actually runs.
    ///
    /// # THIS WAS `<=`, AND `<=` COULD NOT FAIL
    ///
    /// `reachable` is [`reachable_union`]'s flood fill: it seeds only on
    /// mouths that `chamber_at` already admitted, and expands only through
    /// `passages_from`, which retains its candidates on `chamber_exists`.
    /// `chambers` counts exactly the addresses `chamber_at` admits. So
    /// `reachable` is a **subset of** `chambers` by construction, and
    /// `reachable <= chambers` is a theorem about the code rather than a
    /// claim about the world — true under every possible connectivity
    /// defect, including a walk that reaches nothing but its own doorways.
    /// The `reachable > 0` guard above it excludes only the empty case.
    ///
    /// **Why that mattered enough to change behaviour at the close of a
    /// campaign.** The strong form of this claim lives in
    /// `stope_variety_probe.rs` and in `drift_reach_probe.rs`'s gated arms,
    /// and both are `heavy`-tier; decision 0148 took the heavy tier off the
    /// merge queue's phase list. This test is not heavy — it runs in the
    /// merge's `gate` phase on every landing — so with `<=` here, **nothing
    /// on any schedule asserted that underworld reachability is 100%**. The
    /// campaign's own product had no gate that could notice it regressing.
    ///
    /// # WHY EQUALITY IS THE RIGHT CLAIM, NOT A CONVENIENT ONE
    ///
    /// It is entailed, not observed. Deleting the existence coin (spec §4.1)
    /// makes every address inside `chamber_exists`'s five structural gates
    /// exist; §4.5's two by-construction guarantees make every branch descend
    /// and every branch below the top have a parent; amendment E's third
    /// makes every top-band branch named by an aperture. Together those put
    /// the whole of a system's lattice in one component containing its own
    /// entrances. Seed 42 reads `reachable == chambers == 30537`, and the
    /// other two panel seeds read 59,227 and 48,294 the same way.
    ///
    /// An earlier version of this doc quoted **30,272** — the figure measured
    /// between Tasks 4 and 5, before the three per-branch draws were re-keyed
    /// on band. `drift_reach_probe.rs` records the move; this file did not,
    /// which is the ordinary way a number in prose goes stale.
    ///
    /// # IT FIRES
    ///
    /// Proved rather than argued, and re-measured when this assertion was
    /// strengthened: neutralising the band-descent step in `passages_from`
    /// (so a run's bottom level offers no child below) takes seed 42's
    /// reachable count to **4,148 of 30,537** and reddens here with a real
    /// assertion failure, not a compile error. `chambers` does not move under
    /// that mutation, which is what makes the two sides separate
    /// computations rather than one quantity compared with itself.
    ///
    /// The module doc above carried **5,569** for this same severing, and it
    /// was not re-taken after Task 7 rewired the descent rule onto the drawn
    /// band-transition edges. 4,148 is the measured figure on the finished
    /// world.
    ///
    /// **What would legitimately move it** — and the only things that should:
    /// `descents_from`'s edge draw, `passages_from`'s vertical or lateral
    /// rules, `entrance_mouth`'s landing, `aperture_count_at`'s width floor
    /// (amendment E.2's construction), or a return of a per-address existence
    /// draw. A move with none of those touched is a connectivity defect.
    #[test]
    fn reachability_is_reported_and_equals_existence() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let reachable: usize = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("reachable").map(str::trim))
            .expect("the readout reports a reachable count")
            .split_whitespace()
            .next()
            .expect("the reachable line leads with its count")
            .parse()
            .expect("the reachable count is a number");
        let chambers: usize = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("chambers").map(str::trim))
            .expect("the readout reports a chamber count")
            .parse()
            .expect("the chamber count is a number");

        assert!(
            reachable > 0,
            "no chamber is reachable from any entrance, so the reachability \
             figure witnesses nothing"
        );
        assert_eq!(
            reachable, chambers,
            "seed 42's underworld is no longer wholly reachable from its own \
             entrances: the walk reaches {reachable} of {chambers} chambers. \
             Since The Drift (spec §4.1, §4.5 and amendment E) these two are \
             equal by construction, and this is the ONLY assertion of that on \
             the merge queue's schedule — the strong forms in \
             `stope_variety_probe` and `drift_reach_probe` are heavy-tier and \
             decision 0148 took heavy off the merge phases. Four things move \
             this legitimately: `descents_from`'s band-transition edge draw, \
             `passages_from`'s vertical or lateral rules, where \
             `entrance_mouth` lands a door, and `aperture_count_at`'s width \
             floor (amendment E.2's construction). A move with none of those \
             touched is a connectivity defect, not a figure to re-baseline. \
             `reachable` GREATER than `chambers` cannot happen at all — the \
             walk seeds on `chamber_at` and expands through `chamber_exists` \
             — so a red here always means the walk fell short."
        );
    }

    /// **The entrance walk follows the DRAWN count, and plural entrances
    /// are witnessed** (Task 5, amendment C.3): the `entrances` line reports
    /// strictly more drawn doors than cave systems, i.e. at least one system
    /// on the panel seed drew a second aperture — the non-vacuity arm for
    /// C.3 in this artifact.
    #[test]
    fn the_witness_walks_drawn_entrances_and_sees_a_plural_one() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let line = text
            .lines()
            .find(|l| l.trim().starts_with("entrances"))
            .expect("the readout reports an entrances line");
        let nums: Vec<usize> = line
            .split_whitespace()
            .filter_map(|w| w.parse::<usize>().ok())
            .collect();
        assert!(
            nums.len() >= 2,
            "the entrances line does not carry its counts: {line:?}"
        );
        let (drawn, systems) = (nums[0], nums[1]);
        assert_eq!(
            systems,
            text.lines()
                .find_map(|l| l
                    .trim()
                    .strip_prefix("cave systems")?
                    .split_whitespace()
                    .next()?
                    .parse::<usize>()
                    .ok())
                .expect("cave-system count"),
            "the entrances line's denominator disagrees with the cave-system count"
        );
        assert!(
            drawn >= systems,
            "{drawn} entrances drawn across {systems} systems — below one \
             apiece means the count draw answered under its own minimum"
        );
        assert!(
            nums.get(2).copied().unwrap_or(0) > 0,
            "no multi-entrance system on seed 42's panel — C.3's plural is \
             not witnessed by this artifact"
        );
        // The union rule's visible consequence: reachable stays a strict
        // subset of existence even with every mouth seeding one shared walk.
        assert!(
            !text.contains("reachable  0 "),
            "the union walk reached nothing"
        );
    }

    /// The `origin` field is **read**, not merely carried — and the readout
    /// says so with a `found`/`made` split whose halves sum to the chamber
    /// count.
    ///
    /// `made` is 0 by construction today (the override source is empty; see
    /// the module doc), so the sum is what carries the assertion: it fails if
    /// a chamber is ever counted into neither half, which is the shape a
    /// widened [`ChamberOrigin`] would take.
    #[test]
    fn every_chamber_is_counted_under_exactly_one_origin() {
        let seed = Seed(42);
        let text = render_underworld(seed, &terrain_for(seed));
        let origin_line = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("by origin").map(str::trim))
            .expect("the readout reports an origin split");
        let found: usize = origin_line
            .split_whitespace()
            .find_map(|w| w.strip_prefix("found:"))
            .expect("the origin split names found")
            .parse()
            .expect("the found count is a number");
        let made: usize = origin_line
            .split_whitespace()
            .find_map(|w| w.strip_prefix("made:"))
            .expect("the origin split names made")
            .parse()
            .expect("the made count is a number");
        let chambers: usize = text
            .lines()
            .find_map(|l| l.trim().strip_prefix("chambers").map(str::trim))
            .expect("the readout reports a chamber count")
            .parse()
            .expect("the chamber count is a number");

        assert_eq!(
            found + made,
            chambers,
            "the origin split ({found} found + {made} made) does not account \
             for all {chambers} chambers"
        );
    }
}
