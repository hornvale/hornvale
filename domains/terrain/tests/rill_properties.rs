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
