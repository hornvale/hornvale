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

use hornvale_kernel::{CellId, Geosphere, NearestCellIndex, RoomAddr, Seed};
use hornvale_terrain::{
    CatchmentCut, ChannelNetwork, RILL_MIN_CATCHMENT, RILL_WHOLE, RILLS_PER_CELL_MAX,
    RIVER_MIN_DRAINAGE, TerrainPins, WaterKind, band_edges, cell_catchment, channel_half_width,
    generate, rills_of,
};
use std::collections::BTreeSet;

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

/// The canonical grid. Level 5 remains the wrong grid for this even now that
/// the network is fifteen times larger: it is the grid the byte golden pins,
/// and a property test that shared it would move whenever that fixture did.
const OUTLET_LEVEL: u32 = 6;

/// Per-seed run floor, so a world that stopped producing channels is visible on
/// its own rather than absorbed by the other two. Measured after Task 3: 3606 /
/// 6086 / 3876 (was 183 / 359 / 192 when the network rendered river cells
/// only).
const MIN_RUNS_PER_SEED: usize = 1_500;

/// Sweep-wide run floor: a channel-less world cannot pass this test by having
/// nothing to check. Measured 13,568 after Task 3, against 734 before it.
const MIN_RUNS_TOTAL: usize = 6_500;

/// Sweep-wide floor on runs that actually END on a non-reach outlet — the
/// vertex Task 2 adds. The universal assertion above is satisfied by a
/// confluence too, so without this a build that stopped emitting outlet
/// vertices entirely could still pass by ending every run on a trunk. Measured
/// after Task 3: 2028 / 3113 / 2035 = **7176** of 13,568 runs (6090 ocean, 1086
/// salt basin, and — checked, not assumed — zero of any other kind). The floor
/// is half of that, which leaves room for terrain drift and none for the
/// phenomenon disappearing.
const MIN_OUTLET_RUNS: usize = 3_500;

/// claim: invariant(forall-seed) — the last cell of every run, over three
/// worlds on the canonical grid, is the cell that run drains into.
///
/// **R-2.** A run must include the cell it drains into. `build` walks a run
/// down `downhill` and can stop for exactly two reasons, so the last cell of
/// every run must be one of exactly two things:
///
/// 1. it is not a **reach** — it is the outlet the run drains into: an ocean
///    cell, or a terminal sink (land with nowhere to send its water, which
///    `water::classify` names a salt basin), and the run reached it; or
/// 2. some *other* run carries it as a non-final vertex, which is a confluence:
///    this run joined a trunk another run had already claimed and stopped on
///    the shared cell.
///
/// Anything else — a run ending on a reach that no other run continues past —
/// is a run that **stopped short**, which is precisely the defect Task 2
/// repaired. Before that fix `build` broke *before* pushing a non-river target,
/// so every run draining straight to the sea ended one cell early and its mouth
/// sat inland; the 39 seed-42 river cells that carried no polyline were exactly
/// the 39 that read `Dry` at their own centres.
///
/// **The classification is `is_reach`, not `water_kind == River` — Task 3
/// changed which of those is the right question, and the difference is not
/// cosmetic.** It used to be three clauses because a terminal sink was a
/// *river* cell with no downhill target, a case distinct from an outlet. Now
/// the reach predicate excludes terminal sinks by construction, so clause 1
/// absorbs them: `other_nonreach` was measured at exactly **0** across the
/// three worlds — every non-reach terminal is Ocean or SaltBasin. And the test
/// could not simply keep asking about `River`: once every land cell is
/// rendered, most confluences happen on cells that classify `DryLand`, and the
/// old first clause caught them and demanded borrowed geometry of a vertex
/// that is entitled to its own. It failed on seed 42 run 76 at `CellId(12678)`
/// for exactly that reason.
///
/// The third clause reads the *owner* map — a cell some run carries as a
/// non-final vertex — rather than "a cell that appears in two runs". The
/// distinction is load-bearing: two runs that both stopped short on the same
/// cell would each appear in the other's cell list and would satisfy the weaker
/// form, so the weaker form is blind to the defect wherever it happens twice.
///
/// The second clause carries a second assertion, on the same population: a
/// mouth must carry the **arriving reach's** band geometry rather than the
/// outlet cell's own. That is the one genuine design decision in Task 2, and
/// the comment at it says why it is asserted here rather than left to the two
/// witnesses that already exist.
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

        // A REACH: land with a downhill target, the same predicate `build`
        // walks on — restated here from committed state rather than read off
        // the network, so the classification below is not the object under
        // test's own opinion of itself.
        let is_reach = |c: CellId| {
            !matches!(*globe.water_kind.get(c), WaterKind::Ocean) && globe.downhill.get(c).is_some()
        };

        for (i, cells) in net.run_cells.iter().enumerate() {
            let last: CellId = *cells.last().expect("a run has at least two cells");
            if !is_reach(last) {
                outlet_runs += 1;
                // THE BORROWED MOUTH GEOMETRY, as a property. `band_edges` is
                // per vertex from that vertex's own drainage, slope and
                // spacing, and this vertex's cell is NOT a river — so `build`
                // gives the mouth the arriving reach's geometry instead. Both
                // alternatives were measured before the choice was made
                // (`build`'s own comment carries the numbers): an ocean outlet
                // has drainage 0 and yields `[0, 0, 0, 0]`, a mouth with no
                // channel at all; a salt basin holds its whole catchment at
                // zero gradient and yields a mouth 1.4x as wide inside a
                // valley 12.9x as broad as the river that feeds it.
                //
                // ASSERTED HERE BECAUSE THE OTHER TWO WITNESSES ARE NOT
                // ENOUGH. `channel.rs`'s `e[0] > 0.0` kills only the ocean
                // half — the zero-width case — and says nothing about a salt
                // basin, whose own edges are all comfortably positive. The
                // only thing that pins the basin half is the level-5 byte
                // fixture, and that fixture is explicitly re-baselineable and
                // will be re-baselined by later tasks in this campaign. This
                // assertion is the durable one.
                let n = net.band_edges[i].len();
                assert_eq!(
                    net.band_edges[i][n - 1],
                    net.band_edges[i][n - 2],
                    "seed {seed}, run {i}: the mouth at {last:?} ({}) does not carry the \
                     arriving reach's band geometry — it computed its own from the outlet \
                     cell's drainage and gradient, which describe the sea or the basin \
                     rather than the river",
                    globe.water_kind.get(last).name()
                );
                continue;
            }
            assert!(
                owner[last.0 as usize].is_some_and(|trunk| trunk != i),
                "seed {seed}, run {i}: it ends on reach {last:?}, which has a downhill \
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
        "only {outlet_runs} of {total_runs} runs end on the non-reach cell they drain into \
         (measured 7176) — the terminal vertex is no longer being emitted, and the assertion \
         above cannot see that because a confluence satisfies it too"
    );
}

// ---------------------------------------------------------------------------
// R-1 (Task 3): the network renders the whole flow tree, not the top 6.7%.
//
// THE GUARD ORDER MATTERS AND IS THE POINT. Task 3 re-baselines
// `tests/fixtures/channel-network-seed-42-level-5.txt`, which was the only
// thing in the repo pinning network topology; a fixture regenerated after a
// change witnesses the change rather than judging it. So the two tests below
// were written and run BEFORE the widening, and
// `the_network_renders_every_river_cells_downhill_edge` was watched PASSING on
// the pre-change network. It is the durable statement that the widening was
// ADDITIVE: every edge the old network drew, the new one still draws, in the
// same direction, as a consecutive pair of the same run. Neither test reads a
// fixture — both derive their reference from committed globe state
// (`elevation`, `sea_level`, `downhill`, `water_kind`), so a re-baseline
// cannot launder them.
// ---------------------------------------------------------------------------

/// The rendered edge set: every consecutive pair of cells in every run, as raw
/// `CellId` values. This is the network's topology stated as a relation, which
/// is the form the flow tree it is a rendering of also takes — so the two are
/// directly comparable without either side re-deriving the other's
/// construction.
fn rendered_edges(net: &ChannelNetwork) -> BTreeSet<(u32, u32)> {
    let mut edges = BTreeSet::new();
    for run in &net.run_cells {
        for pair in run.windows(2) {
            edges.insert((pair[0].0, pair[1].0));
        }
    }
    edges
}

/// Sweep-wide floor on river edges, so a world that stopped producing rivers
/// cannot satisfy the containment claim by having nothing to contain.
/// Measured on the pre-change network at level 6: 700 / 1533 / 803 = 3036.
const MIN_RIVER_EDGES: usize = 1500;

/// Sweep-wide floor on land flow-tree edges. Measured: 11000 / 18922 / 11493 =
/// 41415. Half of that, which terrain drift will not reach and a collapse of
/// the flow tree would.
const MIN_LAND_EDGES: usize = 20000;

/// claim: invariant(forall-seed) — every `WaterKind::River` cell's downhill
/// edge is rendered, over three worlds on the canonical grid.
///
/// **THE PRE-CHANGE TOPOLOGY WITNESS.** Written and watched green on the
/// network as it stood before Task 3's widening (183 / 359 / 192 polylines,
/// 883 / 1892 / 995 vertices), and green on the widened one. That is its whole
/// job: it says the widening only ever ADDED, so no river the old network drew
/// was moved onto a different cell, dropped, or reversed.
///
/// The claim is over EDGES rather than over cells because a cell can appear in
/// a run for two different reasons — as a reach, or as the outlet a run merely
/// terminates on — and only the edge form distinguishes them. It is also the
/// form that carries direction: `(c, downhill(c))` renders as a consecutive
/// pair in that order, so a build that collected runs upstream would fail here
/// rather than pass on an undirected set.
///
/// The reference is computed from `water_kind` and `downhill` directly, so it
/// lives outside the object under test and outside every fixture. The level-5
/// byte golden pins the same topology and more, but it is re-baselined by this
/// very task; this is what does not move.
#[test]
fn the_network_renders_every_river_cells_downhill_edge() {
    let geo = Geosphere::new(OUTLET_LEVEL);
    let mut checked = 0usize;
    for seed in OUTLET_SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());
        let rendered = rendered_edges(&net);
        for c in geo.cells() {
            if !matches!(*globe.water_kind.get(c), WaterKind::River) {
                continue;
            }
            let Some(target) = *globe.downhill.get(c) else {
                continue;
            };
            assert!(
                rendered.contains(&(c.0, target.0)),
                "seed {seed}: river cell {c:?} drains to {target:?}, and no run carries that \
                 edge. The network no longer contains the network it replaced — whatever else \
                 changed, an existing river was dropped, rerouted or reversed"
            );
            checked += 1;
        }
    }
    assert!(
        checked >= MIN_RIVER_EDGES,
        "only {checked} river edges across {} seeds (measured 3036) — the containment claim \
         above ran on far less than the population it was calibrated against",
        OUTLET_SEEDS.len()
    );
}

/// claim: invariant(forall-seed) — the rendered edge set EQUALS the world's
/// land flow tree, over three worlds on the canonical grid.
///
/// **R-1.** `downhill` and `drainage` are computed for every land cell; before
/// Task 3 the network rendered only the ~6.7% above [`RIVER_MIN_DRAINAGE`] and
/// discarded the rest. This asserts the rest is now drawn: the set of
/// consecutive run-cell pairs is exactly `{(c, downhill(c)) : c is land}`.
///
/// **Equality, not containment, and the second half is the load-bearing one.**
/// Containment alone ("every land edge is rendered") is satisfied by a build
/// that also invents edges the flow tree does not have — a run that skipped a
/// cell, or one assembled from something other than `downhill`. The reverse
/// inclusion is what says the network is a rendering OF the flow tree rather
/// than a superset of it, and it is the assertion that would catch a widened
/// predicate that accidentally admitted ocean cells as reaches.
///
/// The coverage half — R-1 as the brief states it, every land cell with a
/// downhill target appearing in some `run_cells` — follows from the equality
/// (such a cell sources a rendered edge, so it is in a run), and is asserted
/// separately anyway because the exception set is the interesting part: a land
/// cell absent from the network can only be a **terminal sink** with no land
/// cell draining into it. Terminal sinks are `endorheic && no downhill`, which
/// `water::classify` reads as `SaltBasin`; there are 66 / 124 / 78 of them
/// across the three worlds and only the inflow-less ones go unrendered.
#[test]
fn the_network_renders_the_whole_flow_tree() {
    let geo = Geosphere::new(OUTLET_LEVEL);
    let mut land_edges_checked = 0usize;
    let mut sinks_total = 0usize;
    let mut unrendered_sinks = 0usize;
    for seed in OUTLET_SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());

        let is_land = |c: CellId| *globe.elevation.get(c) >= globe.sea_level;
        // The reference flow tree, straight off committed state. Not one cell
        // of it comes from `net`.
        let mut flow_tree: BTreeSet<(u32, u32)> = BTreeSet::new();
        let mut land_cells: Vec<CellId> = Vec::new();
        for c in geo.cells() {
            if !is_land(c) {
                continue;
            }
            land_cells.push(c);
            if let Some(target) = *globe.downhill.get(c) {
                flow_tree.insert((c.0, target.0));
            }
        }
        let rendered = rendered_edges(&net);

        let missing: Vec<(u32, u32)> = flow_tree.difference(&rendered).copied().collect();
        assert!(
            missing.is_empty(),
            "seed {seed}: {} of {} land flow-tree edges are not rendered (first: {:?}). The \
             network is still drawing a sub-tree of the flow graph rather than the whole of it",
            missing.len(),
            flow_tree.len(),
            missing.first()
        );
        let invented: Vec<(u32, u32)> = rendered.difference(&flow_tree).copied().collect();
        assert!(
            invented.is_empty(),
            "seed {seed}: {} rendered edges are not in the land flow tree (first: {:?}). A run \
             is carrying a pair of cells that `downhill` does not join, so the polyline is not a \
             rendering of the flow graph",
            invented.len(),
            invented.first()
        );
        land_edges_checked += flow_tree.len();

        let covered: BTreeSet<CellId> = net.run_cells.iter().flatten().copied().collect();
        for &c in &land_cells {
            if covered.contains(&c) {
                continue;
            }
            assert!(
                globe.downhill.get(c).is_none(),
                "seed {seed}: land cell {c:?} has a downhill target ({:?}) and appears in no \
                 run — R-1's coverage claim fails on it",
                *globe.downhill.get(c)
            );
            unrendered_sinks += 1;
        }
        sinks_total += land_cells
            .iter()
            .filter(|&&c| globe.downhill.get(c).is_none())
            .count();
    }
    assert!(
        land_edges_checked >= MIN_LAND_EDGES,
        "only {land_edges_checked} land flow-tree edges across {} seeds (measured 41415)",
        OUTLET_SEEDS.len()
    );
    // Not an assertion about a good number — a statement of the exception set's
    // SIZE, so a future change that started leaving ordinary land cells out
    // would have to move this too. Measured: 268 terminal sinks across the
    // three worlds, of which the inflow-less ones are unrendered.
    assert!(
        unrendered_sinks <= sinks_total,
        "{unrendered_sinks} unrendered land cells against {sinks_total} terminal sinks — the \
         exception set is larger than the only thing allowed to be in it"
    );
    assert!(
        sinks_total > 0 && sinks_total < land_edges_checked / 50,
        "{sinks_total} terminal sinks against {land_edges_checked} land edges — the exception \
         clause above is either vacuous or swallowing the population"
    );
}

/// claim: structural(seed: 42) — the meander field's own values at eight fixed
/// positions, pinned to exact bits.
///
/// **THE NOISE-FIELD WITNESS, AND WHY IT IS SEPARATE FROM THE GOLDEN.**
/// `tests/fixtures/channel-network-seed-42-level-5.txt` exists because a
/// reviewer mutated the meander's seed label and the whole terrain suite stayed
/// green; its displacement column is what caught that. But the displacement of
/// a vertex depends on the run it sits in — the perpendicular is taken from the
/// vertex's neighbours — so **any change to run construction moves that column
/// wholesale**, and Task 3 moves it for nearly every vertex in the world. A
/// re-baseline then absorbs a seed change and a topology change
/// indistinguishably.
///
/// These values do not depend on the network's shape at all. They are the
/// field, sampled directly, captured on the pre-change code and asserted bit
/// for bit — so a change to `streams::CHANNEL_MEANDER`, to the derivation in
/// `TectonicGlobe::channel_noise_seed`, to `MEANDER_FREQUENCY` or to
/// `MEANDER_OCTAVES` reddens here no matter what the run construction does.
///
/// What it does NOT cover, stated plainly so nobody reads it as more than it
/// is: the meander AMPLITUDE ratio, the head/mouth anchoring rule, and the
/// perpendicular the displacement is taken along are still witnessed only by
/// the level-5 golden.
#[test]
fn the_meander_field_is_pinned() {
    // Level 5 seed 42: the cheapest world that produces a network at all. The
    // field is a function of the derived seed and position, so the grid it was
    // sampled through is immaterial to what is pinned.
    let geo = Geosphere::new(5);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).expect("seed 42 generates");
    let net = ChannelNetwork::build(&outcome.globe, &geo, outcome.globe.channel_noise_seed());
    let third = 0.577_350_269_189_625_8_f64;
    let pinned: [([f64; 3], u64); 8] = [
        ([1.0, 0.0, 0.0], 0xbfd2_2227_19c5_c360),
        ([0.0, 1.0, 0.0], 0x3fe0_655f_ca41_2a78),
        ([0.0, 0.0, 1.0], 0xbf96_5cca_d2e5_c4a0),
        ([-1.0, 0.0, 0.0], 0x3fc0_be1c_c94b_6768),
        ([0.6, 0.8, 0.0], 0xbfa1_5f39_82cc_5900),
        ([0.0, -0.6, 0.8], 0x3fb1_e076_1579_9070),
        ([third, third, third], 0x3fcb_5012_deb5_0f40),
        ([-third, third, -third], 0x3fb3_6045_9e36_c760),
    ];
    let mut distinct = BTreeSet::new();
    for (position, bits) in pinned {
        let got = net.meander_at(position);
        assert_eq!(
            got.to_bits(),
            bits,
            "the meander field moved at {position:?}: {got} against the pinned {}. The seed the \
             field is drawn from, its frequency or its octave count changed — every river in \
             every world has a different shape, and this says so independently of how the runs \
             are built",
            f64::from_bits(bits)
        );
        distinct.insert(bits);
    }
    // A field that returned one constant would satisfy every equality above
    // had it always been constant, so say it is not.
    assert_eq!(
        distinct.len(),
        8,
        "the pinned field values are not distinct"
    );
}

/// claim: behavior(the width law no longer has a scale-dependent part)
///
/// **Superseded by decision 0129, and kept as the record of the reversal.**
/// This test was `the_river_threshold_is_the_one_part_that_is_not_scale_free`,
/// and it asserted that `channel_half_width` returned exactly `0.0` below
/// [`RIVER_MIN_DRAINAGE`] while the same drained area one level down cleared
/// it — the one place where refining the grid changed the width law's answer,
/// and the exception Tier 2 was told it would inherit.
///
/// Task 3 removed the zero-return, so the exception is gone: the law is now
/// scale-free at **every** discharge, including the sub-threshold ones it
/// previously refused, and a subdivision inherits no threshold at all. That is
/// asserted here, at the same six doublings the fixture sweep uses, on the
/// discharges the old short-circuit covered.
///
/// [`RIVER_MIN_DRAINAGE`] is untouched and still decides what
/// `water::classify` calls a river. The point is that it no longer decides
/// what gets *drawn*, so the two questions have come apart — which is exactly
/// the ~49.6% `water_kind == River` / `transverse_at == Channel` disagreement
/// the predecessor campaign documented, now deliberately wider.
#[test]
fn the_width_law_is_scale_free_below_the_river_threshold_too() {
    // The canonical level-6 cell edge; the exact value is immaterial here.
    let spacing = 0.018_886;
    // The whole sub-threshold range the old zero-return swallowed, from a
    // single land cell's own runoff up to the threshold itself.
    for count in [1.0_f64, 2.0, 7.0, RIVER_MIN_DRAINAGE - 1.0] {
        let base = channel_half_width(count, spacing);
        assert!(
            base > 0.0,
            "drainage {count} still returns a zero width — the short-circuit is back"
        );
        let mut scaled = count;
        let mut edge = spacing;
        for doubling in 1..=DOUBLINGS {
            scaled *= 4.0;
            edge *= 0.5;
            assert_eq!(
                channel_half_width(scaled, edge).to_bits(),
                base.to_bits(),
                "a sub-threshold width is not scale-free at doubling {doubling} \
                 (count {scaled}, spacing {edge}) against the level-6 {base}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// R-4 (Task 4): THE COMPOSED BASIN CLAIM.
//
// This is the guard whose absence let Task 4's first attempt ship a falsified
// keystone. That construction satisfied three separate ONE-STEP invariants —
// every child leaves its parent where the parent leaves, the floor is monotone
// in coarse accumulation, each face's sub-network is a tree ending at that
// face's own outlet — while the network it produced delivered 26-31% of land
// to the sea against the coarse graph's 74-82%, and sent 6.0-7.5% of
// basin-interior faces to a terminus in a DIFFERENT coarse basin. Every local
// invariant can hold while the global one fails.
//
// So the reference here is the coarse graph's COMPOSED answer — follow
// `downhill` to a terminus — and the subject is the network's own composed
// answer, followed through the run structure the polylines are a rendering of.
// Written and watched green on the pre-Tier-2 network before Tier 2 existed.
// ---------------------------------------------------------------------------

/// Sweep-wide floor on the reaches R-4 composes, so the claim cannot be
/// satisfied by a world with nothing to compose. Measured over the three
/// seeds: printed by the assertion below when it trips.
const MIN_COMPOSED_REACHES: usize = 20_000;

/// Follow `step` from `start` until it has no successor, and return where it
/// stopped. Panics rather than looping if the relation cycles — both relations
/// this is applied to are strictly descending and therefore acyclic, and a
/// cycle would otherwise hang the suite rather than fail it.
fn terminus(start: CellId, bound: usize, step: impl Fn(CellId) -> Option<CellId>) -> CellId {
    let mut current = start;
    for _ in 0..bound {
        match step(current) {
            None => return current,
            Some(next) => current = next,
        }
    }
    panic!("the relation composed from {start:?} did not terminate within {bound} hops");
}

/// claim: invariant(forall-seed) — over three worlds, the coarse cell every
/// reach's RENDERED chain terminates in is the coarse cell its `downhill`
/// chain terminates in.
///
/// **R-4, stated as the composed claim.** A Tier 2 branch is a share of one
/// coarse cell's catchment and is attached to that cell's trunk, so the coarse
/// cell it drains to is the one that cell's trunk chain reaches. That makes
/// R-4 a claim about the *trunk network*, which is why this test can be — and
/// was — watched green before Tier 2 was written: it is the reference the
/// attachment inherits, and if it were false here no attachment rule could
/// rescue it.
///
/// The rendered successor of a cell is the next cell of the run that CLAIMED
/// it, read from the published `run_cells`. A cell no run continues past is a
/// terminus: either the non-reach outlet a run drains into, or — the defect
/// this can see — a reach that some run stopped short on, which would compose
/// to itself while the coarse graph composes onward to the sea.
#[test]
fn the_rendered_network_composes_to_the_coarse_graphs_terminus() {
    let geo = Geosphere::new(OUTLET_LEVEL);
    let mut total = 0usize;
    let mut agreed = 0usize;
    for seed in OUTLET_SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());
        let is_reach = |c: CellId| {
            !matches!(*globe.water_kind.get(c), WaterKind::Ocean) && globe.downhill.get(c).is_some()
        };

        // The rendered relation, from the published run structure alone.
        let mut next: Vec<Option<CellId>> = vec![None; geo.cell_count()];
        for (i, run) in net.run_cells.iter().enumerate() {
            for (j, &c) in run.iter().enumerate() {
                if j + 1 == run.len() {
                    continue;
                }
                assert!(
                    next[c.0 as usize].is_none(),
                    "seed {seed}: run {i} continues past {c:?}, and so does another run — \
                     the rendered relation is not a function and the composition below \
                     would depend on which run was read last"
                );
                next[c.0 as usize] = Some(run[j + 1]);
            }
        }

        let bound = geo.cell_count();
        let mut seed_total = 0usize;
        let mut seed_agreed = 0usize;
        for c in geo.cells() {
            if !is_reach(c) {
                continue;
            }
            seed_total += 1;
            let coarse = terminus(c, bound, |x| *globe.downhill.get(x));
            let rendered = terminus(c, bound, |x| next[x.0 as usize]);
            if coarse == rendered {
                seed_agreed += 1;
            }
        }
        println!(
            "R-4 (pre-Tier-2 network): seed {seed}: {seed_agreed} / {seed_total} reaches \
             compose to the coarse terminus = {:.4}%",
            100.0 * seed_agreed as f64 / seed_total as f64
        );
        total += seed_total;
        agreed += seed_agreed;
    }
    assert!(
        total >= MIN_COMPOSED_REACHES,
        "only {total} reaches composed across {} seeds — far below the population this \
         claim was calibrated against",
        OUTLET_SEEDS.len()
    );
    assert!(
        agreed * 100 >= total * 99,
        "only {agreed} of {total} reaches ({:.4}%) reach the coarse graph's own terminus \
         through the rendered network — R-4's floor is 99%",
        100.0 * agreed as f64 / total as f64
    );
}

// ---------------------------------------------------------------------------
// Tier 2 (Task 4): the branches that attach to the trunks Tier 1 renders.
//
// THE ORDER THESE WERE WRITTEN IN IS PART OF WHAT THEY ARE WORTH. R-4's
// composed claim above was written and watched green on the pre-Tier-2 network
// before `branch.rs` existed, and R-3 below was written against the API it was
// about to have and watched fail to build. What follows is therefore a
// reference the construction had to satisfy, not a description of what the
// construction turned out to do.
// ---------------------------------------------------------------------------

/// Coarse cells sampled per seed, by stride over the whole `CellId` ordering so
/// the sample crosses every latitude rather than one cap.
///
/// **The population is one cell's whole partition, and the figure is
/// [`RILLS_PER_CELL_MAX`]'s, not a second one typed here.** That constant
/// carries the derivation: `4π/40962` of catchment over a `RILL_MIN_CATCHMENT`
/// of `4π/335544320` is a ratio of 8191.6, which the drawn cut turns into
/// about **25,190 branches** (12,596 leaves) against the even cut's 16,382.
/// This doc previously read "about 14,000 branches", a third number that
/// matched neither the branch count nor the leaf count — the stale-figure class
/// this campaign has now caught three times, and once inside the repair for it.
/// The sweeps below print what they actually walked, so the count is a
/// measurement in the run rather than a claim in a comment.
const CELL_SAMPLE: usize = 120;

/// Floor on the cells that actually carry a partition across the sweep, so
/// none of these claims can pass on an empty population. A cell partitions
/// only if a run continues past it, so this tracks the land fraction.
const MIN_PARTITIONED_CELLS: usize = 100;

/// Floor on the attachments the incidence check below walks, so a sweep that
/// stopped descending the partition cannot pass by checking a handful of
/// branches. Measured at full coverage: **3,099,482** across the three seeds,
/// against the 31,242 the old depth cap reached. A tenth of that, which leaves
/// room for terrain drift and none for the coverage collapsing.
const MIN_ATTACHMENTS: usize = 300_000;

/// The reach cells of one seed, sampled by stride.
fn sampled_cells(globe: &hornvale_terrain::TectonicGlobe, geo: &Geosphere) -> Vec<CellId> {
    let stride = (geo.cell_count() / CELL_SAMPLE).max(1);
    (0..geo.cell_count())
        .step_by(stride)
        .map(|i| CellId(i as u32))
        .filter(|&c| {
            !matches!(*globe.water_kind.get(c), WaterKind::Ocean) && globe.downhill.get(c).is_some()
        })
        .collect()
}

/// claim: invariant(forall-seed) — over three worlds, the branches within a
/// coarse cell partition exactly the one unit of catchment the coarse
/// accumulation credits that cell with.
///
/// **R-3's conservation half, and it is STRUCTURAL rather than checked after
/// the fact.** `branch.rs` splits a share as `first = area·f` and
/// `second = area − first`, so the two parts sum to their parent in bits, and
/// no rounding can accumulate down the partition. This test says so in the
/// strongest available form: for every sibling pair, the sum of the two shares
/// is *bit-identical* to some share the partition also produced — its parent —
/// or to the cell's whole catchment for the first cut. A partition that
/// conserved only to a tolerance would fail this.
///
/// **The reference is the coarse graph, not a constant this module chose.**
/// `drainage` counts land cells upstream of and including a cell, so
/// `drainage(c) − Σ drainage(u)` over the cells draining into `c` is exactly
/// **1**: the cell's own contribution, the only part of the catchment the
/// trunk does not already carry. That identity is asserted here on the same
/// cells, which is what ties the partitioned quantity to the coarse answer.
#[test]
fn a_cells_branches_partition_its_own_unit_of_catchment() {
    let geo = Geosphere::new(OUTLET_LEVEL);
    let unit = cell_catchment(&geo);
    let mut partitioned = 0usize;
    let mut worst_total = 0.0_f64;
    let mut leaves = 0usize;
    for seed in OUTLET_SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());
        let cut = CatchmentCut::Drawn(globe.rill_partition_seed());

        // Who drains into whom, for the coarse identity below.
        let mut inflow = vec![0.0_f64; geo.cell_count()];
        for c in geo.cells() {
            if let Some(t) = *globe.downhill.get(c) {
                inflow[t.0 as usize] += *globe.drainage.get(c);
            }
        }

        for cell in sampled_cells(globe, &geo) {
            let rills = rills_of(cell, &net, &geo, &cut);
            if rills.is_empty() {
                continue;
            }
            partitioned += 1;

            // THE BOUND, on the real function rather than on a probe of its
            // recursion, and in NODE COUNT. `rills_of` materialises a
            // recursive structure, so its size is a claim its doc makes and
            // this holds it to; the campaign's own defect was a stopping rule
            // that reached 269 million nodes and 23.7 GB while every
            // assertion in this file agreed with it, because none of them
            // ever ran.
            assert!(
                rills.len() <= RILLS_PER_CELL_MAX,
                "seed {seed}, {cell:?}: {} branches against the stated bound of \
                 {RILLS_PER_CELL_MAX}. Either the partition no longer terminates where its \
                 doc says or the bound is stale — do not raise it without redoing the \
                 arithmetic on `RILLS_PER_CELL_MAX`",
                rills.len()
            );

            // THE COARSE IDENTITY. The cell's own contribution to its own
            // accumulation is one cell, exactly — which is the quantity the
            // partition below divides, expressed as an area.
            let own = *globe.drainage.get(cell) - inflow[cell.0 as usize];
            assert_eq!(
                own, 1.0,
                "seed {seed}, {cell:?}: the coarse accumulation credits this cell with {own} \
                 of its own area, not 1. The partition divides one cell's worth of catchment, \
                 so if this is not one the object being divided is not what `drainage` says \
                 the trunk collects here"
            );

            // THE PAIRWISE IDENTITY, in bits. `rills_of` emits a node's two
            // parts consecutively, so consecutive pairs are siblings; their
            // shares must sum to a share the partition also holds — their
            // parent's, or the whole for the first cut.
            let mut shares: BTreeSet<u64> = BTreeSet::new();
            shares.insert(RILL_WHOLE);
            for rill in &rills {
                shares.insert(rill.share);
            }
            assert_eq!(rills.len() % 2, 0, "branches are emitted in sibling pairs");
            for pair in rills.chunks(2) {
                let sum = pair[0].share + pair[1].share;
                assert!(
                    shares.contains(&sum),
                    "seed {seed}, {cell:?}: two sibling shares {} and {} sum to {sum}, which is \
                     not a share the partition holds. The split is losing or inventing \
                     catchment, which a tolerance-based check would have absorbed",
                    pair[0].share,
                    pair[1].share
                );
            }

            // THE COMPOSED SUM, also in bits and ORDER-INDEPENDENT because the
            // conserved quantity is an integer: a leaf is a part the partition
            // declined to divide, and the leaves tile the cell's catchment.
            let leaf = |r: &&hornvale_terrain::Rill| r.catchment <= RILL_MIN_CATCHMENT;
            let total: u64 = rills.iter().filter(leaf).map(|r| r.share).sum();
            leaves += rills.iter().filter(leaf).count();
            assert_eq!(
                total, RILL_WHOLE,
                "seed {seed}, {cell:?}: the leaves of the partition hold {total} of \
                 {RILL_WHOLE} — the parts no longer tile the whole"
            );

            // And the rendering of that share is the area the width law is
            // fed, so it is worth knowing how far the f64 rendering drifts
            // from the exact scalar even though nothing depends on it.
            let rendered: f64 = rills.iter().filter(leaf).map(|r| r.catchment).sum();
            worst_total = worst_total.max((rendered - unit).abs() / unit);
        }
    }
    assert!(
        partitioned >= MIN_PARTITIONED_CELLS,
        "only {partitioned} sampled cells carry a partition — below the population these \
         assertions were calibrated against"
    );
    println!(
        "R-3: {partitioned} cells partitioned, {leaves} leaves; the share partition is exact \
         in integers, and the worst drift of its f64 area rendering is {worst_total:.3e}"
    );
}

/// claim: invariant(forall-seed) — over three worlds, every branch is attached
/// to the line one bisection above it, the first pair to the trunk of its own
/// cell, and no branch leaves the catchment it is a share of.
///
/// **R-4's Tier 2 half.** The composed claim above establishes that a cell's
/// trunk chain reaches the coarse graph's own terminus. This establishes the
/// other link: that a branch is joined to *that* trunk and to nothing else,
/// and cannot wander out of the cell whose catchment it divides. Together they
/// are the composed statement R-4 asks for — the coarse cell a branch's water
/// ultimately reaches is the one the coarse graph sends its cell's water to —
/// and it holds by construction rather than by margin, which is the whole
/// difference between this design and the routed one it replaces.
///
/// **What the reach bound below does and does not say, because the two are
/// easy to confuse and the failure message used to confuse them.** It bounds a
/// branch's angular reach from its own cell by the span of the object the
/// branch is a share of \u2014 the catchment square plus the stretch of trunk it may
/// attach to. That is a statement about the SCALAR partition's extent, and it
/// holds by construction. It is **not** a statement that a branch stays inside
/// its cell's Voronoi region: the square is a same-area proxy for the real
/// region, so neighbouring cells' squares overlap at their corners
/// (`branch.rs`'s own module doc concedes this), and a branch can satisfy this
/// bound while sitting nearer another cell. `tests/rill_probe.rs` measures that
/// geometric spill directly and it is **10.5\u201310.8%** of sampled branch heads
/// across the three seeds \u2014 a named deviation, not a hypothetical. The divide
/// that cannot be crossed is the one the partitioned scalar draws, and R-4's
/// composed claim is made about that.
#[test]
fn every_branch_is_attached_to_the_line_above_it() {
    let geo = Geosphere::new(OUTLET_LEVEL);
    let unit = cell_catchment(&geo);
    let mut partitioned = 0usize;
    let mut checked = 0usize;
    let mut worst_reach = 0.0_f64;
    let mut worst_ratio = 0.0_f64;
    let mut worst_gap = 0.0_f64;
    for seed in OUTLET_SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());
        let cut = CatchmentCut::Drawn(globe.rill_partition_seed());
        for cell in sampled_cells(globe, &geo) {
            let rills = rills_of(cell, &net, &geo, &cut);
            if rills.is_empty() {
                continue;
            }
            partitioned += 1;

            // DETERMINISM, asserted where the population already exists: the
            // partition is a pure function of the seed and the address, so a
            // second enumeration is the same list.
            assert_eq!(
                rills,
                rills_of(cell, &net, &geo, &cut),
                "seed {seed}, {cell:?}: the partition is not reproducible"
            );

            let here = geo.position(cell);
            // THE BOUND IS COMPUTED, NOT TYPED. The furthest a branch may be
            // from its cell is the half-diagonal of the cell's own catchment
            // square — `s/√2` for a square of side `s`, hence the named
            // constant — plus the furthest point of the cell's own stretch of
            // trunk. That stretch runs to the midpoints of the arcs either
            // side, which is half a cell spacing out, and the rendered vertex
            // is meander-displaced by up to `MEANDER_AMPLITUDE_RATIO` of the
            // same spacing. So the trunk term is `(0.5 + 0.25)·spacing`, every
            // factor of it read from the mesh or from a published constant.
            // This line previously carried a hand-typed `0.6 * 0.0189` — a
            // level-6 spacing typed to three places, three lines below a
            // comment congratulating itself on naming constants rather than
            // typing them.
            let neighbors = geo.neighbors(cell);
            let spacing = neighbors
                .iter()
                .map(|&n| arc(here, geo.position(n)))
                .sum::<f64>()
                / neighbors.len() as f64;
            let bound = std::f64::consts::FRAC_1_SQRT_2 * unit.sqrt()
                + (0.5 + hornvale_terrain::MEANDER_AMPLITUDE_RATIO) * spacing;
            // PER CELL, so the message names the cell the reach came from.
            // The running maximum this replaced was correct only by an
            // argument — it is monotone and the assertion sits inside the
            // loop, so the first crossing is always the current cell's — and a
            // message whose accuracy depends on where the assertion happens to
            // sit is one refactor from lying.
            let mut reach = 0.0_f64;
            for rill in &rills {
                reach = reach.max(arc(here, rill.head)).max(arc(here, rill.mouth));
            }
            worst_reach = worst_reach.max(reach);
            worst_ratio = worst_ratio.max(reach / bound);
            assert!(
                reach < bound,
                "seed {seed}, {cell:?}: a branch reaches {reach} rad from its own cell, past \
                 the {bound} rad its own catchment and trunk stretch span. The partition has \
                 stopped being confined to the object it divides — a branch is drawing line \
                 outside the catchment whose share it carries. NOTE what this does NOT say: \
                 a branch INSIDE this bound may still sit nearer a neighbouring cell, because \
                 the catchment square is a same-area proxy for the real region and the two \
                 overlap at the corners. That spill is measured in `tests/rill_probe.rs` \
                 (10.5-10.8% of sampled branch heads) rather than asserted here"
            );

            // THE TRUNK ITSELF, rebuilt from the published polyline rather
            // than from `branch.rs`'s idea of it. The cell's stretch of trunk
            // spans the midpoints either side of its own vertex, so it lies
            // across TWO polyline segments and the reference is the nearer of
            // them. The first version of this test used only `[j, j+1]` and
            // reddened at 3.6e-3 rad on the first cell it reached — the mouths
            // of parts on the upstream side of the vertex, which are on the
            // trunk and not on that one segment of it.
            let (line, j) = net
                .trunk_vertex(cell)
                .expect("a cell with a partition has a trunk");
            let points = &net.polylines[line].points;
            let mut trunk = vec![[points[j], points[j + 1]]];
            if j > 0 {
                trunk.push([points[j - 1], points[j]]);
            }

            // EVERY BRANCH, not a depth-capped prefix. This loop used to stop
            // at `ATTACHMENT_DEPTH = 6` — 127 branches of about 25,190, some
            // 0.5% of the population — justified as the incidence check being
            // "quadratic in the population". IT IS NOT: the check is one
            // `rills[parent]` index and one `segment_gap`, both O(1), once per
            // branch, so the loop is LINEAR. Measured either way: 31,242
            // attachments in 1.45 s capped, 3,099,482 in 1.45-1.49 s uncapped
            // (three runs). A hundredfold more coverage for a cost inside the
            // run-to-run noise, because the enumeration was already paid for
            // by `rills_of` and only the check was being skipped.
            for rill in &rills {
                checked += 1;
                let gap = match rill.parent {
                    // The first pair attaches to the trunk itself.
                    None => trunk
                        .iter()
                        .map(|&seg| segment_gap(seg, rill.mouth))
                        .fold(f64::INFINITY, f64::min),
                    Some(parent) => {
                        // Against the branch it NAMES as its parent, not
                        // against the nearest branch one bisection up: the
                        // weaker form is satisfied by a branch attached to the
                        // wrong sibling, and a wrong sibling is a tributary
                        // joining the wrong stream.
                        assert_eq!(
                            rills[parent].depth + 1,
                            rill.depth,
                            "seed {seed}, {cell:?}: a branch's parent is not one bisection above it"
                        );
                        segment_gap([rills[parent].head, rills[parent].mouth], rill.mouth)
                    }
                };
                worst_gap = worst_gap.max(gap);
                assert!(
                    gap < 1e-6,
                    "seed {seed}, {cell:?}: a depth-{} branch's mouth is {gap} from the line it \
                     names as its parent. A branch whose mouth is not ON what it drains \
                     into is joined to the network in the graph and separated from it in space \
                     — the defect The Ford's confluence repair exists for",
                    rill.depth
                );
            }
        }
    }
    assert!(
        partitioned >= MIN_PARTITIONED_CELLS,
        "only {partitioned} sampled cells carry a partition"
    );
    // THE COVERAGE IS PRINTED AND FLOORED, not typed into a doc. `checked` is
    // already accumulated by the loop that owns it, so the number in the
    // report is the run's own.
    assert!(
        checked >= MIN_ATTACHMENTS,
        "only {checked} attachments checked against a floor of {MIN_ATTACHMENTS} — the sweep \
         is no longer walking the whole partition, so the incidence claim covers a prefix of \
         the network rather than the network"
    );
    println!(
        "R-4 (Tier 2 half): {partitioned} cells, {checked} attachments checked (EVERY branch, \
         no depth cap), worst mouth gap {worst_gap:.3e} rad, furthest branch point \
         {worst_reach:.5} rad, worst reach as a fraction of its own cell's bound \
         {worst_ratio:.4}"
    );
}

/// Angular separation of two unit vectors, radians.
fn arc(a: [f64; 3], b: [f64; 3]) -> f64 {
    let d = a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    hornvale_kernel::math::acos(d.clamp(-1.0, 1.0))
}

/// Angular distance from `p` to the great-circle arc through `seg`, radians.
///
/// **The arc, not the chord, and the difference is not pedantry.** A straight
/// chord between two points 0.01 rad apart sits 1.2e-5 rad inside the sphere
/// at its midpoint, so a mouth lying exactly ON the rendered line reads
/// 2.1e-5 rad away from the chord — three headwater half-widths, which is the
/// scale of the very defect this assertion exists to catch. Measured with the
/// chord, the first version of this test could not tell the sagitta from a
/// mouth that had genuinely come off the line.
fn segment_gap(seg: [[f64; 3]; 2], p: [f64; 3]) -> f64 {
    let d = [
        seg[1][0] - seg[0][0],
        seg[1][1] - seg[0][1],
        seg[1][2] - seg[0][2],
    ];
    let len2 = d[0] * d[0] + d[1] * d[1] + d[2] * d[2];
    let t = if len2 <= 0.0 {
        0.0
    } else {
        (((p[0] - seg[0][0]) * d[0] + (p[1] - seg[0][1]) * d[1] + (p[2] - seg[0][2]) * d[2]) / len2)
            .clamp(0.0, 1.0)
    };
    let q = [
        seg[0][0] + t * d[0],
        seg[0][1] + t * d[1],
        seg[0][2] + t * d[2],
    ];
    let n = (q[0] * q[0] + q[1] * q[1] + q[2] * q[2]).sqrt();
    arc(p, [q[0] / n, q[1] / n, q[2] / n])
}

/// The hexagonal packing constant of the mesh: `cell_spacing / √(4π/N)`,
/// measured at **1.078208 / 1.078231 / 1.078237 / 1.078238** across levels
/// 4-7 (Task 1). It is what makes `a·edge·√count` equal `a·1.0782·√area`, and
/// therefore what makes the width law's constant of proportionality below a
/// property of the law rather than of the level.
const PACKING: f64 = 1.07824;

/// The levels the anchor below is measured at. Two is the whole point: a
/// constant that is level-free cannot be checked at one level.
const ANCHOR_LEVELS: [u32; 2] = [5, 6];

/// claim: invariant(the width law's caller-side pairing, absolutely and over
/// two grid levels) — with a mispairing control that must move.
///
/// **THE CALLER-SIDE UNITS ASSERTION, WHICH IS TIER 2'S TO MAKE.** Task 1's
/// invariance test is over the pure function and both of its arguments come
/// from one level by construction, so it cannot see a drained area paired with
/// the wrong level's spacing — the trap `channel_half_width`'s doc names, and
/// which nothing tested until Tier 2 was the first code that could commit it.
///
/// Stated **absolutely** rather than as a ratio between two levels of the same
/// network. That is the lesson the construction this replaces paid for: a
/// parent/child ratio cannot see a spacing derived one level off *everywhere*,
/// because the same factor appears on both sides and cancels, and mutating the
/// spacing function to answer for the wrong depth left three ratio-based tests
/// green. So the quantity asserted here is
///
/// ```text
///   half_width / √(drained area)  =  ½·CHANNEL_WIDTH_COEFF·(spacing/√A_cell)
///                                 =  ½·CHANNEL_WIDTH_COEFF·PACKING
/// ```
///
/// a number with no level in it at all, checked on the real branch population
/// at **two different grid levels**. A branch whose area was expressed in one
/// level's units and whose spacing came from another lands a factor of two per
/// level away from it.
///
/// **The control is the load-bearing half**, for the same reason it was
/// before: without it, a width law that had stopped depending on discharge
/// would satisfy the anchor at a single point and fail nothing. The control
/// pairs the same drained area with a **walk-depth room's** spacing, six
/// levels down, and must land at ~1/64.
#[test]
fn a_branchs_width_is_anchored_to_its_drained_area_not_to_a_level() {
    let expected = 0.5 * hornvale_terrain::CHANNEL_WIDTH_COEFF * PACKING;
    let mut sampled = 0usize;
    let mut worst = 0.0_f64;
    let mut mean = 0.0_f64;
    let mut worst_mispaired = f64::INFINITY;
    let mut readings = 0usize;
    let mut reading_worst = 0.0_f64;
    for level in ANCHOR_LEVELS {
        let geo = Geosphere::new(level);
        let index = NearestCellIndex::new(&geo);
        let unit = cell_catchment(&geo);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).expect("seed generates");
        let globe = &outcome.globe;
        let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());
        let cut = CatchmentCut::Drawn(globe.rill_partition_seed());

        // THE ABSOLUTE DEPTH ANCHOR on `room_spacing` itself, carried forward
        // from the construction this replaces because the hazard is unchanged:
        // a spacing that answered for the wrong depth is invisible to every
        // ratio. A globe-level room's three corners ARE three cells, so its
        // three edges are three cell-to-cell separations — read here off
        // `Geosphere::position` alone, with `RoomAddr::corners` nowhere in it.
        let face = RoomAddr {
            face: 3,
            path: vec![1; level as usize],
        };
        let cells = face
            .corner_weights(&geo, &index)
            .expect("a globe-level room has corner cells");
        let mesh = (arc(geo.position(cells[0].0), geo.position(cells[1].0))
            + arc(geo.position(cells[1].0), geo.position(cells[2].0))
            + arc(geo.position(cells[2].0), geo.position(cells[0].0)))
            / 3.0;
        let drift = (hornvale_terrain::room_spacing(&face) - mesh).abs() / mesh;
        assert!(
            drift < 1e-9,
            "at level {level}, room_spacing({face:?}) is {} against the {mesh} the mesh's own \
             cell positions give — relative {drift:.3e}. The spacing is being derived at the \
             wrong DEPTH, which no parent/child ratio can see because the factor cancels",
            hornvale_terrain::room_spacing(&face)
        );

        for cell in sampled_cells(globe, &geo) {
            // The same mean-of-neighbours spacing `channel.rs` uses, rebuilt
            // here from `Geosphere::position`. A single neighbour's arc is not
            // the same quantity — it carries the mesh's local anisotropy, and
            // using one widened the residual below from 3% to 9%.
            let neighbors = geo.neighbors(cell);
            let spacing = neighbors
                .iter()
                .map(|&n| arc(geo.position(cell), geo.position(n)))
                .sum::<f64>()
                / neighbors.len() as f64;
            let room = room_of_depth(&face, 12);
            for rill in rills_of(cell, &net, &geo, &cut).iter().take(64) {
                let half = channel_half_width(rill.catchment / unit, spacing);
                let k = half / rill.catchment.sqrt();
                worst = worst.max((k - expected).abs() / expected);
                mean += k / expected;
                let mispaired = channel_half_width(
                    rill.catchment / unit,
                    hornvale_terrain::room_spacing(&room),
                );
                worst_mispaired = worst_mispaired.min(mispaired / half);
                sampled += 1;

                // THE SHIPPED PATH'S OWN PAIRING. Everything above reads
                // `Rill::catchment` and does the arithmetic here, so it says
                // nothing about the call a consumer actually makes:
                // `rill_reading` pairs the count and the spacing itself, and
                // nothing else in the suite watches it do so.
                if sampled.is_multiple_of(8)
                    && let Some(reading) =
                        hornvale_terrain::rill_reading(rill.head, &net, globe, &geo, &index, &cut)
                {
                    let k = reading.band_edges[0] / reading.catchment.sqrt();
                    reading_worst = reading_worst.max((k - expected).abs() / expected);
                    readings += 1;
                }
            }
        }
    }
    assert!(
        sampled > 2_000,
        "only {sampled} branches anchored — below the population this was calibrated against"
    );
    mean /= sampled as f64;
    // TWO BOUNDS, AND THE REASON THERE ARE TWO. A single cell's spacing
    // departs from the mesh's mean by up to ~9% — the icosphere is not
    // uniform, and the packing constant is a global average, so a per-branch
    // bound tight enough to be interesting would redden on the twelve
    // pentagons rather than on a units error. The per-branch bound is
    // therefore set well above that local variation and well below the factor
    // of two a wrong level is worth; the POPULATION MEAN is where the tight
    // statement lives, because the local variation averages out of it and a
    // units error does not.
    assert!(
        worst < 0.25,
        "a branch's half-width per √(drained area) is {worst:.4} away from the law's own \
         level-free constant {expected}, past the mesh's own ~9% local spacing variation. The \
         drained area and the spacing are no longer coming from the same level, which is \
         worth a factor of two per level"
    );
    assert!(
        (mean - 1.0).abs() < 0.03,
        "over {sampled} branches at two grid levels the mean half-width per √(drained area) is \
         {mean:.5} of the law's level-free constant. The mesh's local spacing variation \
         averages out of this; a level in the pairing does not"
    );
    assert!(
        worst_mispaired < 1.0 / 32.0,
        "pairing a branch's drained area with a WALK-DEPTH room's spacing changed the width by \
         {worst_mispaired:.5}x — it should be ~1/64 at level 6 and ~1/128 at level 5, and this \
         is the smaller of the two. The anchor above is therefore not \
         discriminating: it would pass for a width law that had stopped depending on the \
         spacing at all"
    );
    assert!(
        readings > 200,
        "only {readings} readings taken — the shipped path's pairing is barely covered"
    );
    assert!(
        reading_worst < 0.25,
        "a `rill_reading`'s own channel half-width per √(drained area) is {reading_worst:.4} \
         away from the law's level-free constant. The reading pairs the count and the spacing \
         itself, so this is the only assertion that watches the SHIPPED call site do it"
    );
    println!(
        "the width-law anchor: {sampled} branches over levels {ANCHOR_LEVELS:?}; mean \
         half-width per √(drained area) is {mean:.5} of the level-free constant \
         {expected:.6e}, worst per-branch departure {worst:.4} (the mesh's own local spacing \
         variation); the mispairing control lands at {worst_mispaired:.5}x. Over {readings} \
         `rill_reading` calls the same anchor is off by at most {reading_worst:.4}"
    );
}

/// A room of `depth` inside `face`, by taking child 0 the rest of the way
/// down — any descendant will do, since only its spacing is read.
fn room_of_depth(face: &RoomAddr, depth: u32) -> RoomAddr {
    let mut room = face.clone();
    while room.depth() < depth {
        room = room.child(0).expect("depth is below the cap");
    }
    room
}

/// claim: invariant(seed: 42) — the partition's branching is a property of the
/// draw, not of the rule, which is R-5's precondition.
///
/// **A generator that splits *k* ways by rule makes Horton's bifurcation ratio
/// an arithmetic property of *k*, and a pass under one is worthless.** The
/// construction this replaces quadrisected every element, and `R_b` duly
/// landed on 4.0 to three significant figures across three worlds — inside the
/// preregistered `[3, 5]` for reasons having nothing to do with hydrology.
///
/// So this asserts the discriminator directly, on the object rather than on
/// the ratios: under [`CatchmentCut::Even`] every leaf of a cell's partition is
/// at the same depth (the tree is balanced and its shape is the rule's alone),
/// and under [`CatchmentCut::Drawn`] the leaves are spread across several
/// depths. If this ever stopped holding, R-5 would have become untestable
/// again and the probe's numbers would mean nothing — which is a thing worth
/// finding out from a gate rather than from a reader.
#[test]
fn the_branching_follows_the_partition_and_not_a_fixed_rule() {
    let geo = Geosphere::new(OUTLET_LEVEL);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).expect("seed generates");
    let globe = &outcome.globe;
    let net = ChannelNetwork::build(globe, &geo, globe.channel_noise_seed());
    let cell = *sampled_cells(globe, &geo)
        .iter()
        .find(|&&c| net.trunk_vertex(c).is_some())
        .expect("some sampled cell carries a trunk");
    let spread = |cut: &CatchmentCut| {
        let rills = rills_of(cell, &net, &geo, cut);
        let depths: Vec<u32> = rills
            .iter()
            .filter(|r| r.catchment <= RILL_MIN_CATCHMENT)
            .map(|r| r.depth)
            .collect();
        let lo = *depths.iter().min().expect("the partition has leaves");
        let hi = *depths.iter().max().expect("the partition has leaves");
        (lo, hi, depths.len())
    };
    let (even_lo, even_hi, even_leaves) = spread(&CatchmentCut::Even);
    let (drawn_lo, drawn_hi, drawn_leaves) =
        spread(&CatchmentCut::Drawn(globe.rill_partition_seed()));
    println!(
        "the branching discriminator on {cell:?}: Even gives {even_leaves} leaves at depths \
         {even_lo}..={even_hi}; Drawn gives {drawn_leaves} leaves at depths {drawn_lo}..={drawn_hi}"
    );
    assert_eq!(
        even_lo, even_hi,
        "under the even cut the partition is not balanced ({even_lo}..={even_hi}) — the \
         falsification arm is supposed to hold the drawn freedom constant, so anything it \
         still varies by is a second source this test cannot see"
    );
    assert!(
        drawn_hi - drawn_lo >= 4,
        "the drawn partition's leaves span depths {drawn_lo}..={drawn_hi} — too narrow for the \
         branching to be carrying any information the rule does not already fix. R-5 would be \
         measuring the rule"
    );
}
