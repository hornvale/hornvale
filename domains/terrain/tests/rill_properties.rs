//! The Rill, Task 1: the channel width law is **already** scale-free (R-3),
//! and the level-6 band edges it produces are pinned against every later task.
//!
//! This file is what remains of a task that set out to change `drainage` from
//! an upstream cell count into a drained area. The change was not made,
//! because the law does not need it: `cell_edge` already carries the
//! conversion. `N` cells tile the sphere, so their mean area is `4π/N`, and a
//! locally hexagonal tiling of cells of area `A` has nearest-neighbour spacing
//! `d = √(2/√3)·√A = 1.0746·√A`. Therefore
//!
//! ```text
//!   a · edge · √count  =  a · (edge/√A_cell) · √(count · A_cell)
//!                      =  (a · 1.0746) · √(drained area)
//! ```
//!
//! and the count never appears on its own. See `channel_half_width`'s own doc
//! comment for the measurement, the trap, and what Tier 2 inherits.
//!
//! **The width-law tests are pure-function tests: they generate no world.**
//! The fixture holds the law's *inputs*, captured once from a real network, so
//! none of them pays for genesis and none of them pins a topology. A later task
//! that adds or removes a channel vertex leaves them untouched — which is the
//! whole reason the witness is a triple table rather than a network dump, and
//! it is what let Task 2 add a vertex to nearly every run in the world without
//! touching a single pinned edge.
//!
//! Task 2 (R-2) then adds the one test here that *does* build worlds:
//! `every_run_reaches_its_outlet`. It is a claim about run construction rather
//! than about the width law, and it lives here because R-2 is this campaign's
//! requirement; it is the only thing in this file that pays for genesis.
//!
//! **Why the invariance is asserted on exact bits rather than a tolerance.**
//! Not strictness for its own sake: it is exact by construction, and saying so
//! is a stronger statement than the campaign needs. `4·count` and `edge/2` are
//! scalings by powers of two, which IEEE-754 performs without error;
//! `√(4c) = 2·√c` exactly, because `sqrt` is correctly rounded and the scaling
//! is by an even power of two; and `(K/2)·(2s)` and `K·s` round from the same
//! exact real value. So the two chains agree bit for bit rather than merely to
//! within `quantize`'s eight significant digits. If a future law is scale-free
//! in real arithmetic but not bit-exact, these assertions still report the
//! relative difference, so the failure distinguishes "no longer scale-free"
//! from "scale-free, no longer bit-exact".

use hornvale_kernel::{CellId, Geosphere, Seed};
use hornvale_terrain::{
    ChannelNetwork, RIVER_MIN_DRAINAGE, TerrainPins, WaterKind, band_edges, channel_half_width,
    generate,
};

/// The committed pre-change witness (`70967bc4`): the width law's inputs and
/// outputs on the seed-42 canonical `Geosphere::new(6)` network, captured on
/// unmodified code before any change was contemplated. Compiled in rather than
/// read at runtime — it is frozen, and no test here may rewrite it.
const FIXTURE: &str = include_str!("fixtures/rill-width-law-seed-42-level-6.txt");

/// The number of distinct `(drainage, slope, spacing)` triples the seed-42
/// level-6 network carries, over its 681 vertices. Exact rather than a floor:
/// the fixture is frozen and has no regeneration path, so this can only move
/// if somebody edits the file, which is exactly the event worth failing on.
const FIXTURE_ROWS: usize = 661;

/// Level 6 to level 12 — the walk-depth span this campaign needs. Each step is
/// one subdivision: four times the cells, half the spacing.
const DOUBLINGS: u32 = 6;

/// One fixture row: the width law's three inputs and the four band edges they
/// produced, every field carried as exact `f64` bits.
struct Row {
    /// Upstream land-cell count, `TectonicGlobe.drainage`.
    drainage: f64,
    /// `local_slope`, metres of fall per radian.
    slope: f64,
    /// `cell_spacing`, mean angular separation to neighbours, radians.
    spacing: f64,
    /// The four `band_edges` borders pinned at capture.
    edges: [f64; 4],
}

/// Parse the frozen fixture. Seven hex `f64::to_bits()` fields per row; `#`
/// opens a comment.
fn fixture() -> Vec<Row> {
    FIXTURE
        .lines()
        .filter(|line| !line.starts_with('#') && !line.trim().is_empty())
        .map(|line| {
            let f: Vec<f64> = line
                .split_whitespace()
                .map(|token| {
                    f64::from_bits(
                        u64::from_str_radix(token, 16).expect("a fixture field is hex f64 bits"),
                    )
                })
                .collect();
            assert_eq!(f.len(), 7, "a fixture row is seven hex fields: {line}");
            Row {
                drainage: f[0],
                slope: f[1],
                spacing: f[2],
                edges: [f[3], f[4], f[5], f[6]],
            }
        })
        .collect()
}

/// claim: structural(seed: 42) — the 661 triples of one fixed world, asserted
/// universally, not a search across seeds for one that agrees.
///
/// THE REGRESSION PIN. No task in this campaign may move a level-6 band edge,
/// and this is what says so. The fixture was captured before the first line of
/// The Rill was written, so it cannot have been derived from the code it
/// guards — which is the property that makes it worth anything, and the
/// property a fixture regenerated after a change silently loses.
///
/// The floor on distinct half-widths is the anti-vacuity half. Without it a
/// law that returned one constant for every input would satisfy every equality
/// above, because the fixture would have been captured from that same constant
/// law had it always been so.
#[test]
fn no_level_6_band_edge_has_moved() {
    let rows = fixture();
    assert_eq!(
        rows.len(),
        FIXTURE_ROWS,
        "the frozen fixture changed size — it has no regeneration path, so this \
         means it was edited by hand"
    );
    let mut half_widths = Vec::with_capacity(rows.len());
    for row in &rows {
        let got = band_edges(row.drainage, row.slope, row.spacing);
        for (k, (got, pinned)) in got.iter().zip(row.edges.iter()).enumerate() {
            assert_eq!(
                got.to_bits(),
                pinned.to_bits(),
                "band edge {k} moved for drainage {} slope {} spacing {}: {got} against the \
                 pinned {pinned}. The width law, GORGE_SLOPE or the confinement law changed; \
                 this fixture predates the campaign and is not re-pinnable",
                row.drainage,
                row.slope,
                row.spacing
            );
        }
        assert!(
            row.edges[0] > 0.0,
            "every fixture row is a river vertex, so every pinned channel \
             half-width is positive: {:?}",
            row.edges
        );
        half_widths.push(row.edges[0].to_bits());
    }
    half_widths.sort_unstable();
    half_widths.dedup();
    assert!(
        half_widths.len() > 100,
        "only {} distinct half-widths across {} rows — the pinned population is \
         nearly constant and the equalities above assert almost nothing",
        half_widths.len(),
        rows.len()
    );
}

/// claim: invariant(the width law under a change of level, over the campaign's
/// full six doublings) — universal over every triple the real world produces,
/// not sampled.
///
/// **R-3.** For a fixed physical drained area, refining the grid by one level
/// quadruples the upstream cell count and halves the cell spacing. The width
/// law must not notice: `band_edges(count, slope, edge)` must equal
/// `band_edges(4·count, slope, edge/2)`, and must keep equalling it six
/// doublings down, which is level 6 to level 12.
///
/// **The claim is over the LAW, not over the world.** A same-basin comparison
/// across real levels cannot isolate this invariant, because a finer elevation
/// field routes flow differently: seed 42's largest basin measures 7.48e-2,
/// 4.48e-2 and 3.47e-2 sr at levels 5, 6 and 7, and its widest channel tracks
/// that at 2.48e-4, 1.89e-4 and 1.67e-4 rad. That is physics — a finer grid
/// finds a different drainage divide — and a test built on it would report the
/// terrain's behaviour under the name of the units'.
///
/// The inputs are the real ones all the same: every triple the seed-42 level-6
/// network actually carries, so the sweep runs over the discharges, gradients
/// and spacings the world produces rather than over round numbers it never
/// reaches.
#[test]
fn the_width_law_is_scale_free_across_six_doublings() {
    let rows = fixture();
    assert_eq!(rows.len(), FIXTURE_ROWS);
    let mut comparisons = 0usize;
    for row in &rows {
        let base = band_edges(row.drainage, row.slope, row.spacing);
        assert!(
            base[0] > 0.0,
            "a zero-width base reach would make every equality below trivially \
             true: {base:?}"
        );
        let mut count = row.drainage;
        let mut spacing = row.spacing;
        for doubling in 1..=DOUBLINGS {
            count *= 4.0;
            spacing *= 0.5;
            let refined = band_edges(count, row.slope, spacing);
            for (k, (refined, base)) in refined.iter().zip(base.iter()).enumerate() {
                assert_eq!(
                    refined.to_bits(),
                    base.to_bits(),
                    "band edge {k} is not scale-free at doubling {doubling} \
                     (count {count}, spacing {spacing}): {refined} against {base}, \
                     relative {:.3e}. A NON-ZERO relative difference means the law \
                     stopped being a function of drained area — most likely because \
                     something multiplied by an area while keeping the cell edge, \
                     which applies the grid factor twice. A relative difference at \
                     the last ULP means it is still scale-free but no longer \
                     bit-exact, which is a different and much smaller finding",
                    (refined - base).abs() / base.abs()
                );
                comparisons += 1;
            }
        }
        // The sweep is only worth running if the inputs really moved: six
        // doublings is 4096x the count against a 64th of the spacing, and both
        // scalings are exact.
        assert_eq!(count, row.drainage * 4096.0);
        assert_eq!(spacing, row.spacing / 64.0);
    }
    assert_eq!(
        comparisons,
        FIXTURE_ROWS * DOUBLINGS as usize * 4,
        "the sweep did not run over every row, doubling and edge"
    );
}

// ---------------------------------------------------------------------------
// R-2 (Task 2): a run includes the cell it drains into.
// ---------------------------------------------------------------------------

/// The seeds the outlet sweep runs over — the campaign's usual trio. Three
/// rather than one because the claim is universal over run construction, not a
/// fact about seed 42's drainage.
const OUTLET_SEEDS: [u64; 3] = [42, 7, 1234];

/// The canonical grid. Level 5 will not serve here: it carries 13 runs on seed
/// 42, so the per-seed floor below could not be met and the assertion would run
/// over a population too small to distinguish "every run reaches its outlet"
/// from "the handful of runs this world has happen to".
const OUTLET_LEVEL: u32 = 6;

/// Per-seed run floor, so a world that stopped producing rivers is visible on
/// its own rather than absorbed by the other two.
const MIN_RUNS_PER_SEED: usize = 25;

/// Sweep-wide run floor: a riverless world cannot pass this test by having
/// nothing to check.
const MIN_RUNS_TOTAL: usize = 300;

/// Sweep-wide floor on runs that actually END on a non-river outlet — the
/// vertex Task 2 adds. The universal assertion above is satisfied by a
/// confluence or a terminal sink too, so without this a build that stopped
/// emitting outlet vertices entirely could still pass by ending every run on a
/// trunk. Measured after the change: 163 / 289 / 164 across the three seeds
/// (616 of 734 runs); the floor is half of that, which leaves room for terrain
/// drift and none for the phenomenon disappearing.
const MIN_OUTLET_RUNS: usize = 308;

/// claim: invariant(forall-seed) — the last cell of every run, over three
/// worlds on the canonical grid, is the cell that run drains into.
///
/// **R-2.** A run must include the cell it drains into. `build` walks a run
/// down `downhill` and can stop for exactly three reasons, so the last cell of
/// every run must be one of exactly three things:
///
/// 1. it has no downhill target at all (a terminal sink), or
/// 2. it is not a river — it is the outlet the run drains into, the sea or a
///    salt basin, and the run reached it, or
/// 3. some *other* run carries it as a non-final vertex, which is a confluence:
///    this run joined a trunk another run had already claimed and stopped on
///    the shared cell.
///
/// Anything else — a run ending on a river cell that has a downhill target and
/// that no other run continues past — is a run that **stopped short**, which is
/// precisely the defect this task repairs. Before the fix `build` broke *before*
/// pushing a non-river target, so every run draining straight to the sea ended
/// one cell early and its mouth sat inland; the 39 seed-42 river cells that
/// carried no polyline were exactly the 39 that read `Dry` at their own
/// centres.
///
/// The third clause reads the *owner* map — a cell some run carries as a
/// non-final vertex — rather than "a cell that appears in two runs". The
/// distinction is load-bearing: two runs that both stopped short on the same
/// cell would each appear in the other's cell list and would satisfy the weaker
/// form, so the weaker form is blind to the defect wherever it happens twice.
#[test]
fn every_run_reaches_its_outlet() {
    let geo = Geosphere::new(OUTLET_LEVEL);
    let mut total_runs = 0usize;
    let mut outlet_runs = 0usize;
    for seed in OUTLET_SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());

        // The run that CLAIMED each cell and continued past it, rebuilt from
        // the published `run_cells` rather than from anything private — the
        // same map `build`'s confluence repair keys on.
        let mut owner: Vec<Option<usize>> = vec![None; geo.cell_count()];
        for (i, cells) in net.run_cells.iter().enumerate() {
            for (j, &c) in cells.iter().enumerate() {
                if j + 1 < cells.len() {
                    owner[c.0 as usize] = Some(i);
                }
            }
        }

        assert!(
            net.run_cells.len() >= MIN_RUNS_PER_SEED,
            "seed {seed} at level {OUTLET_LEVEL} has only {} runs — too few for this \
             assertion to have run on anything",
            net.run_cells.len()
        );
        total_runs += net.run_cells.len();

        for (i, cells) in net.run_cells.iter().enumerate() {
            let last: CellId = *cells.last().expect("a run has at least two cells");
            if !matches!(*globe.water_kind.get(last), WaterKind::River) {
                outlet_runs += 1;
                continue;
            }
            if globe.downhill.get(last).is_none() {
                continue;
            }
            assert!(
                owner[last.0 as usize].is_some_and(|trunk| trunk != i),
                "seed {seed}, run {i}: it ends on river cell {last:?}, which has a downhill \
                 target ({:?}) and which no other run continues past. The run stopped short of \
                 the cell it drains into, so its mouth is inland and the outlet cell carries no \
                 channel",
                *globe.downhill.get(last)
            );
        }
    }
    assert!(
        total_runs >= MIN_RUNS_TOTAL,
        "only {total_runs} runs across {} seeds — the assertion above ran on far less than \
         the population it was calibrated against",
        OUTLET_SEEDS.len()
    );
    assert!(
        outlet_runs >= MIN_OUTLET_RUNS,
        "only {outlet_runs} of {total_runs} runs end on the non-river cell they drain into \
         (measured 616) — the terminal vertex is no longer being emitted, and the assertion \
         above cannot see that because a confluence satisfies it too"
    );
}

/// claim: behavior(the one part of the width law that is NOT scale-free)
///
/// `RIVER_MIN_DRAINAGE` compares against a **count**, so it is the single
/// place where refining the grid changes the answer for a fixed physical
/// drained area: the same trickle that is not a channel at level 6 is a
/// channel at level 7, because its count quadrupled while the threshold did
/// not. Asserted rather than merely noted, because it is the exception Tier 2
/// has to carry — a subdivision that inherits this threshold unchanged will
/// find channels appearing out of nothing as it descends.
///
/// This is not a defect being enshrined. Making the threshold an area is a
/// deliberate change with its own blast radius (`crate::water::classify` reads
/// the same constant to decide what a river *is*), and it belongs to whichever
/// task takes it on, with this assertion as the statement of what it changes.
#[test]
fn the_river_threshold_is_the_one_part_that_is_not_scale_free() {
    // The canonical level-6 cell edge; the exact value is immaterial here.
    let spacing = 0.018_886;
    let trickle = RIVER_MIN_DRAINAGE - 1.0;
    assert_eq!(
        channel_half_width(trickle, spacing),
        0.0,
        "a sub-threshold trickle is not a channel"
    );
    assert!(
        channel_half_width(trickle * 4.0, spacing * 0.5) > 0.0,
        "the threshold is stated in cells, so one level down the SAME drained \
         area clears it — if this ever stops being true, the threshold became \
         an area and this test is the note saying so"
    );
}
