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
//! is `git diff --exit-code` over the declared paths — a change to chamber
//! existence, to a chamber's content, to the run draw, to the derivation key
//! or to the reach the terrain grants moves bytes in a file somebody has to
//! look at.
//!
//! **What it is deliberately built out of.**
//!
//! - **The shipped entry points, only.** Every existence verdict comes from
//!   [`crate::chamber::chamber_at`] and every floor count from
//!   [`crate::chamber::floors_in_run`]. Nothing here re-implements a
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
//! **Cost.** One `BuildDepth::Terrain` world per seed, and a scan of the
//! lattice over cave-bearing land cells. See `scripts/regenerate-artifacts.sh`
//! for the seed panel it is rendered over.

use hornvale_kernel::{Seed, quantize};
use hornvale_terrain::GeneratedTerrain;

use crate::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, RunAddr, chamber_at, chamber_key, floors_in_run,
};

/// How many bands the delve ladder's habitation rungs occupy — the range
/// `ChamberAddr::band` indexes. Stated locally rather than imported because
/// `chamber.rs` keeps its own rank↔rung bijection private on purpose; the
/// readout only needs to know how far to count, and the *names* reach the
/// artifact through `chamber_key`, which is the authority.
const HABITATION_BANDS: u8 = 5;

/// The habitation rungs' spellings **in this readout's tallies** —
/// presentation, deliberately not the key's table. See [`stratum_word`] for
/// why the distinction is load-bearing rather than fussy.
const BAND_WORDS: [&str; HABITATION_BANDS as usize] =
    ["undercroft", "shallows", "deeps", "underdeep", "nadir"];

/// The five subterranean rock units' spellings, in [`rock_rank`]'s order.
const ROCK_WORDS: [&str; 5] = ["regolith", "cover", "basement", "roots", "underneath"];

/// How many cave systems the per-system transect walks in full. Three: enough
/// that the section shows a cave system rather than an anecdote, few enough
/// that the artifact stays reviewable by eye at 20 rows apiece.
const TRANSECT_SYSTEMS: usize = 3;

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

/// One cave system's realized lattice: for each `(branch, band)` run, the key
/// its floor 0 derives from, how many floors the run drew, and which of those
/// floors exist.
struct RunRow {
    /// `chamber_key` of this run's floor 0 — the real derivation key, so the
    /// epoch label's consequences and the key's field order reach the artifact.
    key: String,
    /// The floor count [`floors_in_run`] drew for this run.
    drawn: u8,
    /// `#` per realized floor, `.` per refused one, in floor order, `drawn`
    /// characters long.
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
    /// Realized chambers, over the whole lattice of every cave system.
    chambers: usize,
    /// Realized chambers per band, `undercroft` … `nadir`.
    by_band: [usize; HABITATION_BANDS as usize],
    /// Realized chambers per rock unit, `regolith` … `underneath`.
    by_rock: [usize; 5],
    /// Realized chambers reporting a stratum no chamber should be able to sit
    /// in (a marine or surface register). Expected 0; printed regardless,
    /// because a silently dropped anomaly is not a witness.
    off_ladder_rock: usize,
    /// Floors drawn across every run of every cave system, whether or not the
    /// band is inside a cave's budget. Separates "the run draw moved" from
    /// "the reach moved": the first moves this, the second does not.
    drawn_floors: usize,
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
        chambers: 0,
        by_band: [0; HABITATION_BANDS as usize],
        by_rock: [0; 5],
        off_ladder_rock: 0,
        drawn_floors: 0,
    };
    // The first `TRANSECT_SYSTEMS` cave systems in cell order, walked in full.
    let mut transect: Vec<(hornvale_kernel::CellId, String, Vec<RunRow>)> = Vec::new();
    let overrides = crate::chamber::ChamberOverrides::new();

    for cell in terrain.geosphere().cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        tallies.systems += 1;
        let gradient = terrain.geothermal_gradient_at(cell);
        let column = terrain.column_at(cell);
        let want_transect = transect.len() < TRANSECT_SYSTEMS;
        let mut rows: Vec<RunRow> = Vec::new();

        for branch in 0..BRANCHES_PER_SYSTEM {
            for band in 0..HABITATION_BANDS {
                let run = RunAddr {
                    cell,
                    entrance: 0,
                    branch,
                    band,
                };
                let drawn = floors_in_run(seed, run);
                tallies.drawn_floors += usize::from(drawn);

                let mut realized = String::new();
                let mut rock = "-";
                for floor in 0..drawn {
                    let addr = ChamberAddr {
                        cell,
                        entrance: 0,
                        branch,
                        band,
                        floor,
                    };
                    match chamber_at(seed, &cave, gradient, &column, addr, &overrides) {
                        None => realized.push('.'),
                        Some(chamber) => {
                            realized.push('#');
                            tallies.chambers += 1;
                            tallies.by_band[usize::from(band)] += 1;
                            match rock_rank(chamber.stratum) {
                                Some(rank) => tallies.by_rock[rank] += 1,
                                None => tallies.off_ladder_rock += 1,
                            }
                            if rock == "-" {
                                rock = stratum_word(chamber.stratum);
                            }
                        }
                    }
                }

                if want_transect {
                    rows.push(RunRow {
                        key: chamber_key(ChamberAddr {
                            cell,
                            entrance: 0,
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
        "  lattice         {BRANCHES_PER_SYSTEM} branches per system, {HABITATION_BANDS} bands, \
         {} floors admitted per run\n",
        crate::chamber::FLOORS_PER_RUN_CEILING
    ));
    out.push_str(&format!("  cave systems    {}\n", tallies.systems));
    out.push_str(&format!("  floors drawn    {}\n", tallies.drawn_floors));
    out.push_str(&format!("  chambers        {}\n", tallies.chambers));
    out.push_str("  by band         ");
    for (band, count) in tallies.by_band.iter().enumerate() {
        // Presentation words, not the key's table (see `stratum_word`'s doc):
        // the save-format spelling of a rung reaches this artifact through the
        // transect's keys, which is the authority. These may be reworded.
        let word = BAND_WORDS[band];
        out.push_str(&format!("{word}:{count}  "));
    }
    out.push('\n');
    out.push_str("  by rock         ");
    for (rank, count) in tallies.by_rock.iter().enumerate() {
        let word = ROCK_WORDS[rank];
        out.push_str(&format!("{word}:{count}  "));
    }
    out.push_str(&format!("off-ladder:{}\n", tallies.off_ladder_rock));

    out.push_str("\n  the first three cave systems, run by run\n");
    out.push_str("  (key = the floor-0 derivation key; # = a floor that exists)\n");
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
            text.contains('#'),
            "no transect row shows a realized floor, so the per-system section \
             witnesses nothing"
        );
        assert!(
            text.contains(crate::streams::CHAMBER.as_str()),
            "the readout does not name the epoch it was derived under"
        );
    }
}
