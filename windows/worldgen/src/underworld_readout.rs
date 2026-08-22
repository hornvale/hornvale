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
//!   [`crate::chamber::floors_in_run`], every passage from
//!   [`crate::chamber::passages_from`]. Nothing here re-implements a
//!   derivation, because a re-implementation cannot witness the real one —
//!   Task 2's own lesson, learned when a test that derived both legs inline
//!   let a re-parented `run_stream` through.
//! - **The real derivation key**, spelled by `chamber::chamber_key`
//!   rather than by a presentation table of this module's own. That is what
//!   puts the `chamber/v3` epoch, the rung spellings and the key's field order
//!   into the artifact's bytes: an accidental `chamber/v4`, or a reordered
//!   key, changes the text on every transect row as well as relocating every
//!   draw.
//! - **Integers**, almost entirely. The only floats that cross this emit
//!   boundary are a cave's depth budget and its cell's geothermal gradient,
//!   and both go through [`hornvale_kernel::quantize`] here at the boundary —
//!   never in the compute path the lattice actually reads.
//!
//! # THE LOOP BOUND IS THE LATTICE, NEVER A DRAW — read this before editing
//!
//! The floor walk is `0..FLOORS_PER_RUN_CEILING`, a **constant**, and it must
//! stay one. The first version of this module walked `0..drawn`, where
//! `drawn = floors_in_run(..)` is *the same value* `chamber_exists`'s own
//! floor gate compares against — so the gate could never fire on any address
//! this readout asked about, and review proved it by deleting the gate
//! outright and getting a byte-identical artifact. That mutation deletes the
//! whole of Task 2 (it is what makes a run a distribution rather than a
//! constant) and the witness did not move.
//!
//! **An instrument that derives its own loop bound from the draw it is meant
//! to witness is self-consistent by construction and cannot ask a question the
//! gate could answer differently.** Both floor gates
//! ([`crate::chamber::FLOORS_PER_RUN_CEILING`] and the drawn length) are now
//! visible and distinguishable, and [`Tallies::past_run_length`] is the
//! sharpest reading of the second: it is 0 in a healthy tree and large the
//! moment the gate stops gating.
//!
//! # What this witness does NOT see today, stated rather than discovered
//!
//! - **The `entrance` axis.** Every address here is built at
//!   [`WITNESSED_ENTRANCES`] = 1, i.e. `entrance: 0`, because that is the only
//!   entrance the lattice has a producer for. Entrances become plural in Task
//!   5, and extending this is that task's job: the loops below are shaped so
//!   the extension is raising that constant and sourcing the per-cell count,
//!   not a rewrite.
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
//! - **Vertical connection.** [`crate::chamber::passages_from`] does not treat
//!   `floor` as an adjacency axis yet, so [`Tallies::reachable`] counts only
//!   the entrance floor's component. That is not a defect in this readout: it
//!   is the exact quantity amendment C.4 exists to move, printed so Task 3b
//!   has something that can actually change.
//!
//! **Cost.** One `BuildDepth::Terrain` world per seed, and a scan of the
//! lattice over cave-bearing land cells. See `scripts/regenerate-artifacts.sh`
//! for the seed panel it is rendered over.

use std::collections::BTreeSet;

use hornvale_kernel::{Seed, quantize};
use hornvale_terrain::{DelveRung, GeneratedTerrain};

use crate::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, ChamberOrigin, FLOORS_PER_RUN_CEILING, RunAddr, chamber_at,
    chamber_key, floors_in_run, passages_from, rung_rank,
};

/// A habitation rung's spelling **in this readout's tallies** — presentation,
/// deliberately not the key's table. See [`stratum_word`] for why that
/// distinction is load-bearing rather than fussy.
///
/// **Exhaustive over [`DelveRung`], and that is the whole point of its
/// existence.** This function is what ties the band walk below to the delve
/// ladder: a sixth rung fails THIS to compile, so the readout cannot be
/// silently left one band short of the lattice it is meant to witness.
///
/// `None` for `Surface`, which is a rung of the ladder but not a *habitation*
/// rung and has no position in a lattice of underground places — the same
/// answer `chamber::rung_rank` gives it.
fn band_word(rung: DelveRung) -> Option<&'static str> {
    match rung {
        DelveRung::Surface => None,
        DelveRung::Undercroft => Some("undercroft"),
        DelveRung::Shallows => Some("shallows"),
        DelveRung::Deeps => Some("deeps"),
        DelveRung::Underdeep => Some("underdeep"),
        DelveRung::Nadir => Some("nadir"),
    }
}

/// The habitation bands this readout walks, as `(rank, word)` in rank order —
/// **derived from the delve ladder, never restated as a literal.**
///
/// # Why this is a function and not a `const 5`
///
/// It was a `const HABITATION_BANDS: u8 = 5` for one review round, and review
/// found the same defect the module doc's loop-bound section is about, one
/// axis over. `FLOORS_PER_RUN_CEILING` and `BRANCHES_PER_SYSTEM` are imported
/// from the lattice; the band count alone was a local literal tied to nothing.
/// So a sixth delve rung would have compiled this module unchanged, the walk
/// would have kept stopping at five, and **that rung's chambers would have
/// vanished from `chambers`, from `by_band` and from every transect row with
/// no compile error and no test red** — every internal consistency assertion
/// survives it, because `by_band` still sums to `chambers` when both are
/// undercounted by the same missing band.
///
/// Two independent guards now, and they fail at different times on purpose:
///
/// 1. [`band_word`] is exhaustive over [`DelveRung`], so a sixth variant fails
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
                 readout — band_word is exhaustive over DelveRung so that this \
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

/// How many entrances per cave system this witness walks — **1 today, and
/// that is a stated gap rather than a claim about the world.**
///
/// `ChamberAddr::entrance` is a real lattice axis; nothing in the shipped path
/// produces a second entrance yet (`windows/vessel`'s `delve_at` is pinned at
/// `entrance: 0`, and so is `delve_seating`). Task 5 makes entrances plural.
/// Every loop below counts `0..WITNESSED_ENTRANCES` rather than hard-coding a
/// literal, so that task extends the witness by raising this and sourcing the
/// per-cell count — an edit, not a rewrite.
const WITNESSED_ENTRANCES: u8 = 1;

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

/// The glyph for a floor inside its run's drawn length that the existence draw
/// refused.
const GLYPH_REFUSED: char = '.';

/// The glyph for a floor **past** its run's drawn length — inside the
/// lattice's own ceiling, so this readout still asks about it, and refused.
/// A gate that stopped gating turns roughly half of these into
/// [`GLYPH_EXISTS`], which is exactly the movement review's mutation proved
/// the first version of this module could not produce.
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

/// One cave system's run in the lattice: the key its floor 0 derives from, how
/// many floors the run drew, and what the shipped path answered at **every**
/// floor the lattice admits — not merely the drawn ones.
struct RunRow {
    /// `chamber_key` of this run's floor 0 — the real derivation key, so the
    /// epoch label's consequences and the key's field order reach the artifact.
    key: String,
    /// The floor count [`floors_in_run`] drew for this run, printed as its own
    /// value. It is **reported**, never used as this row's loop bound: see the
    /// module doc's loop-bound section for what happened when it was.
    drawn: u8,
    /// One glyph per floor of the lattice's own ceiling, in floor order,
    /// always [`FLOORS_PER_RUN_CEILING`] characters long:
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
    /// **Realized chambers at a floor past their own run's drawn length** —
    /// the direct reading of `chamber_exists`'s drawn-floor gate.
    ///
    /// 0 in a healthy tree, and large the moment that gate stops gating. This
    /// is the counter review's gate-deletion mutation could not move in the
    /// first version of this module, because that version never asked about a
    /// floor past the drawn length at all.
    past_run_length: usize,
    /// Realized chambers at a **branch past their system's drawn branch
    /// count** — the direct reading of `chamber_exists`'s branch-count gate
    /// (spec C.1). 0 in a healthy tree, for the same reason
    /// [`Tallies::past_run_length`] is.
    past_branch_count: usize,
    /// Floors drawn across every run of every cave system, whether or not the
    /// band is inside a cave's budget. Separates "the run draw moved" from
    /// "the reach moved": the first moves this, the second does not.
    drawn_floors: usize,
    /// Chambers **reachable from an entrance** by [`passages_from`], summed
    /// over every cave system whose entrance chamber exists.
    ///
    /// The quantity a player actually experiences, and the one an existence
    /// count cannot see: a chamber can exist and have nowhere to descend from
    /// or into. Amendment C.4 is about exactly this, so it is printed here for
    /// Task 3b to move.
    reachable: usize,
    /// Cave systems whose entrance chamber (`branch 0, band 0, floor 0`)
    /// exists at all — [`Tallies::reachable`]'s denominator, without which a
    /// fallen reachability figure cannot be told from fewer open entrances.
    open_entrances: usize,
}

/// Chambers reachable from `entry` by [`passages_from`], `entry` included.
///
/// A plain breadth-first walk over the shipped adjacency function: nothing
/// here knows the adjacency rule, which is the point — Task 3b changes that
/// rule and this number moves without this function being touched.
///
/// `BTreeSet`, never a `HashSet` (workspace rule); the walk order is not
/// observed, only the final count is.
fn reachable_from(
    seed: Seed,
    cave: &hornvale_terrain::Cave,
    gradient: hornvale_terrain::GeothermalGradient,
    entry: ChamberAddr,
) -> usize {
    let mut seen: BTreeSet<ChamberAddr> = BTreeSet::new();
    let mut queue: Vec<ChamberAddr> = vec![entry];
    seen.insert(entry);
    while let Some(addr) = queue.pop() {
        for next in passages_from(seed, cave, gradient, addr) {
            if seen.insert(next) {
                queue.push(next);
            }
        }
    }
    seen.len()
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
        reachable: 0,
        open_entrances: 0,
    };
    // The first `TRANSECT_SYSTEMS` cave systems in cell order, walked in full.
    let mut transect: Vec<(hornvale_kernel::CellId, String, Vec<RunRow>)> = Vec::new();
    // THE OVERRIDE SOURCE, named rather than inlined: this is the one line
    // Task 4 replaces to put `ChamberOrigin::Made` into the artifact. Empty
    // today, so `by_origin[1]` is 0 by construction — see the module doc.
    let overrides = crate::chamber::ChamberOverrides::new();

    for cell in terrain.geosphere().cells() {
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

        for entrance in 0..WITNESSED_ENTRANCES {
            // The system's drawn branch width, read once per entrance and
            // REPORTED against, never used as a loop bound: every branch the
            // lattice admits is still walked, so a branch gate that stopped
            // gating shows up as [`GLYPH_EXISTS`] where [`GLYPH_PAST_BRANCH']
            // belongs — the same falsifiability rule the floor walk follows.
            let realized_branches = crate::character::branch_count_of(seed, cell, entrance);
            for branch in 0..BRANCHES_PER_SYSTEM {
                for band in 0..habitation_bands().len() as u8 {
                    let run = RunAddr {
                        cell,
                        entrance,
                        branch,
                        band,
                    };
                    let drawn = floors_in_run(seed, run);
                    tallies.drawn_floors += usize::from(drawn);

                    let mut realized = String::new();
                    let mut rock = "-";
                    // THE LATTICE'S OWN CEILING, never `drawn`. See the module
                    // doc: bounding this walk by the draw made the drawn-floor
                    // gate unfalsifiable.
                    for floor in 0..FLOORS_PER_RUN_CEILING {
                        let addr = ChamberAddr {
                            cell,
                            entrance,
                            branch,
                            band,
                            floor,
                        };
                        match chamber_at(seed, &cave, gradient, &column, addr, &overrides) {
                            // The EXISTS arm is first, so no refusal glyph
                            // computed from `drawn` can mask a chamber the
                            // shipped path admitted past it.
                            Some(chamber) => {
                                realized.push(GLYPH_EXISTS);
                                tallies.chambers += 1;
                                tallies.by_band[usize::from(band)] += 1;
                                if floor >= drawn {
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
                            None if floor >= drawn => realized.push(GLYPH_PAST_RUN),
                            None if branch >= realized_branches => realized.push(GLYPH_PAST_BRANCH),
                            None => realized.push(GLYPH_REFUSED),
                        }
                    }

                    if want_transect {
                        rows.push(RunRow {
                            key: chamber_key(ChamberAddr {
                                cell,
                                entrance,
                                branch,
                                band,
                                floor: 0,
                            }),
                            drawn,
                            realized,
                            rock,
                        });
                    }
                }
            }

            // Reachability from this entrance's own mouth — the address
            // `windows/vessel`'s `delve_at` descends into.
            let entry = ChamberAddr {
                cell,
                entrance,
                branch: 0,
                band: 0,
                floor: 0,
            };
            if chamber_at(seed, &cave, gradient, &column, entry, &overrides).is_some() {
                tallies.open_entrances += 1;
                tallies.reachable += reachable_from(seed, &cave, gradient, entry);
            }
        }

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
         {FLOORS_PER_RUN_CEILING} floors admitted per run, \
         {WITNESSED_ENTRANCES} entrance witnessed\n",
        habitation_bands().len()
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
        "  (key = the floor-0 derivation key; {GLYPH_EXISTS} exists, \
         {GLYPH_REFUSED} refused, {GLYPH_PAST_RUN} past the run's drawn floors, \
         {GLYPH_PAST_BRANCH} past the system's drawn branch count)\n"
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
    ///    [`FLOORS_PER_RUN_CEILING`] long — a row bounded by its own `drawn`
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
                usize::from(FLOORS_PER_RUN_CEILING),
                "a transect row is {} glyphs long, not the lattice ceiling of \
                 {FLOORS_PER_RUN_CEILING} — the floor walk is bounded by \
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
    /// at or past `floors_in_run`, so any nonzero reading is that gate not
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

    /// **Reachability is reported, is nonzero, and is a strict subset of
    /// existence** — the quantity amendment C.4 moves, and the one an
    /// existence count cannot see.
    ///
    /// The strictness is the interesting arm and it is asserted rather than
    /// hoped for: `passages_from` does not treat `floor` as an adjacency axis
    /// yet, so every chamber above floor 0 is unreachable from the entrance
    /// today. `reachable == chambers` would mean either that this readout
    /// stopped walking the graph or that vertical connection landed — both
    /// worth a red rather than a silent pass.
    #[test]
    fn reachability_is_reported_and_is_a_strict_subset_of_existence() {
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
        assert!(
            reachable < chambers,
            "reachable ({reachable}) is not strictly under existence \
             ({chambers}); with `floor` still absent from passages_from's \
             adjacency rule every chamber above floor 0 must be unreachable, \
             so this means the graph walk or the adjacency rule moved"
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
