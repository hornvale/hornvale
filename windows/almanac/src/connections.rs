//! The transport topology's legibility surface (The Connection Graph, Task
//! 6): render a site's natural connections -- its sea-lanes, its overland
//! routes, and which of the world's connected regions it belongs to -- as
//! prose a person can read. Mirrors `history::render_site`'s shape (a pure
//! `pub fn` over a `&World`, deterministic, present-as-query).
//!
//! Unlike `render_site`, this module cannot derive its own graph:
//! `windows/worldgen` (the composition root, which owns
//! `connection_graph_of`) already depends on `windows/almanac` normally, so
//! the dependency can never run the other way without a cycle. The caller
//! (the composition root or the CLI, both of which already hold a
//! `ConnectionGraph`) hands one in.
//!
//! Determinism: [`ConnectionGraph::edges`] and
//! [`ConnectionGraph::reachable_regions`] are already deterministic
//! (`BTreeMap`/`BTreeSet` throughout `hornvale-topology`, no `HashMap`); this
//! module only reads them and formats what it finds. Same
//! `world`/`site`/`graph`, same string, byte for byte.

use hornvale_kernel::{CellId, Value, World};
use hornvale_topology::{ConnectionGraph, EdgeKind};

/// Below this conductance, an edge carries no real natural route -- it is
/// the zero-conductance bare-mesh adjacency `hornvale_worldgen::graph_derive`
/// emits for a pair straddling impassable terrain (ocean; see
/// `cost_conductance`'s zero case in that module). Every real route --
/// adjacency between two passable cells, a sailing lane, a land corridor --
/// carries a strictly positive conductance, so this threshold (well below
/// the smallest such value, `1 / corridor_max_cost` at the derivation's
/// default config) separates "genuinely unreachable" from "merely
/// expensive." Regions computed at this threshold answer "what can a
/// traveler actually reach," not "what cells happen to share a mesh
/// boundary."
/// type-audit: bare-ok(ratio: threshold)
const ISOLATION_THRESHOLD: f64 = 1e-6;

/// The smallest region size this module reports as a real stretch of
/// geography rather than mesh-adjacency noise. Two ocean cells share a
/// natural route only via a specific dated [`EdgeKind::WaterRoute`] lane
/// between two coasts -- there is no general "open ocean" traversal edge --
/// so under [`ISOLATION_THRESHOLD`] almost every one of a globe's tens of
/// thousands of ocean cells becomes its own trivial one-cell "region,"
/// vastly outnumbering the real landmasses. Reporting that raw count
/// ("29911 regions divide the world") would bury the handful of regions a
/// reader actually cares about, so region-count prose filters to `>=` this
/// size. Purely a presentation floor: [`ConnectionGraph::reachable_regions`]
/// itself is untouched, and a site's own region (however small) is always
/// named regardless of this floor -- see [`isolation_paragraph`].
/// type-audit: bare-ok(count: floor)
const MIN_NOTABLE_REGION_SIZE: usize = 2;

/// Render a site's transport connections and regional isolation as prose:
/// its water routes ("linked by sea-lane to ..."), its natural overland
/// routes ("a natural route runs ... by land"), and which of the world's
/// connected regions ([`ConnectionGraph::reachable_regions`]) it belongs to
/// -- named, and flagged when that region is cut off from the wider world.
/// `graph` is the world's derived [`ConnectionGraph`] (built by
/// `hornvale_worldgen::connection_graph_of`, which this window cannot call
/// itself -- see the module doc); this function only reads it. Deterministic:
/// same `world`/`site`/`graph`, same string.
/// type-audit: bare-ok(artifact: return)
pub fn render_connections(world: &World, site: CellId, graph: &ConnectionGraph) -> String {
    let label = site_label(world, site);
    let mut out = String::new();
    let header = format!("The connections of {label}");
    out.push_str(&header);
    out.push('\n');
    out.push_str(&"=".repeat(header.chars().count()));
    out.push_str("\n\n");

    out.push_str(&routes_paragraph(world, site, graph, &label));
    out.push('\n');
    out.push_str(&isolation_paragraph(site, graph, &label));
    out
}

/// Every distinct destination `site` reaches by an edge of `kind`,
/// ascending `CellId` order (deterministic regardless of the graph's own
/// edge-insertion order, and de-duplicated: a real derivation never emits a
/// parallel edge between the same pair, but a hand-built graph might).
fn destinations(graph: &ConnectionGraph, site: CellId, kind: EdgeKind) -> Vec<CellId> {
    let mut out: Vec<CellId> = graph
        .edges(site)
        .iter()
        .filter(|e| e.kind == kind)
        .map(|e| e.to)
        .collect();
    out.sort();
    out.dedup();
    out
}

/// The water/land route paragraph: named destinations by kind, or an honest
/// "no route" line when a site has neither.
fn routes_paragraph(world: &World, site: CellId, graph: &ConnectionGraph, label: &str) -> String {
    let water = destinations(graph, site, EdgeKind::WaterRoute);
    let land = destinations(graph, site, EdgeKind::LandRoute);

    if water.is_empty() && land.is_empty() {
        return format!(
            "{label} opens onto no sea-lane and no natural overland route of its own: \
             whatever reaches it must cross open country, adjacency by adjacency.\n"
        );
    }

    let mut out = String::new();
    if !water.is_empty() {
        let names: Vec<String> = water.iter().map(|&c| site_label(world, c)).collect();
        out.push_str(&format!(
            "{label} is linked by sea-lane to {} -- a current-borne crossing, not a road.\n",
            crate::history::join_prose(&names)
        ));
    }
    if !land.is_empty() {
        let names: Vec<String> = land.iter().map(|&c| site_label(world, c)).collect();
        out.push_str(&format!(
            "A natural route runs to {}, by land, over the easiest ground the terrain \
             allows -- a pass, never a paved road.\n",
            crate::history::join_prose(&names)
        ));
    }
    out
}

/// The isolation paragraph: which connected region (at
/// [`ISOLATION_THRESHOLD`]) `site` belongs to, how it compares to the
/// world's largest region, and -- when it is not the largest -- an explicit
/// "cut off" line naming the gap no natural route bridges. The region count
/// quoted is filtered to [`MIN_NOTABLE_REGION_SIZE`] (see that constant's
/// doc for why the raw count is mostly ocean-cell noise); `site`'s own
/// region is always counted even if it falls below that floor, so a
/// genuinely single-cell islet still reports honestly rather than vanishing
/// from its own sentence.
fn isolation_paragraph(site: CellId, graph: &ConnectionGraph, label: &str) -> String {
    let regions = graph.reachable_regions(ISOLATION_THRESHOLD);
    let region = regions
        .iter()
        .find(|r| r.contains(&site))
        .expect("reachable_regions partitions every node the graph was built over");
    let size = region.len();
    let largest = regions.iter().map(|r| r.len()).max().unwrap_or(size);
    let notable_regions = regions
        .iter()
        .filter(|r| r.len() >= MIN_NOTABLE_REGION_SIZE || r.contains(&site))
        .count();

    if size == largest {
        format!(
            "{label} sits within the largest connected stretch of the known world ({size} \
             cell{}, the largest of {notable_regions} real region{} the map resolves into) \
             -- well-linked, nothing here is stranded.\n",
            plural(size),
            plural(notable_regions),
        )
    } else {
        format!(
            "{label}'s region holds only {size} cell{} -- cut off from the wider world: no \
             route this graph knows of crosses the gap that separates it from the largest \
             region ({largest} cells). {notable_regions} real regions divide the known world \
             in all.\n",
            plural(size),
        )
    }
}

/// "s" unless `n == 1`.
fn plural(n: usize) -> &'static str {
    if n == 1 { "" } else { "s" }
}

/// A world-level overview of the transport topology's reachability: how many
/// real regions the natural-route graph resolves into, the largest, and the
/// sizes of the rest -- the gallery page's second half (a site's own
/// connections are [`render_connections`]'s job; this is the map-level
/// summary). Filtered to [`MIN_NOTABLE_REGION_SIZE`] for the same reason
/// [`isolation_paragraph`] filters its count: almost every one of a globe's
/// ocean cells is its own trivial one-cell "region" under
/// [`ISOLATION_THRESHOLD`] (no general open-ocean traversal edge exists,
/// only dated coast-to-coast [`EdgeKind::WaterRoute`] lanes), and that noise
/// would swamp the handful of real landmasses/archipelagos a reader cares
/// about. Deterministic: same `graph`, same string.
/// type-audit: bare-ok(artifact: return)
pub fn render_overview(graph: &ConnectionGraph) -> String {
    let regions = graph.reachable_regions(ISOLATION_THRESHOLD);
    let mut sizes: Vec<usize> = regions
        .into_iter()
        .map(|r| r.len())
        .filter(|&n| n >= MIN_NOTABLE_REGION_SIZE)
        .collect();
    // Largest first: a reader wants the headline region before the tail.
    sizes.sort_by(|a, b| b.cmp(a));

    let mut out = String::new();
    out.push_str("The reach of the map\n");
    out.push_str(&"-".repeat(21));
    out.push_str("\n\n");

    let Some(&largest) = sizes.first() else {
        out.push_str(
            "No two cells of this world share a natural route at all: every stretch of \
             ground stands entirely alone.\n",
        );
        return out;
    };

    if sizes.len() == 1 {
        out.push_str(&format!(
            "Every natural route on this world -- every sea-lane, every land corridor, every \
             plain patch of open ground -- eventually connects: one single region of {largest} \
             cells holds the whole reachable map. No stretch of ground stands apart from the \
             rest.\n",
        ));
        return out;
    }

    let rest = &sizes[1..];
    out.push_str(&format!(
        "Natural travel divides the known world into {} real regions (below \
         {MIN_NOTABLE_REGION_SIZE} cells, a \"region\" is just an island cell no sea-lane \
         reaches -- not counted here). The largest spans {largest} cells; the rest, smaller \
         and cut off from it, run {}.\n",
        sizes.len(),
        join_sizes(rest),
    ));
    out
}

/// Render a list of region sizes, largest first, as a plain size list --
/// "1997, then 1977, then 831 cells, down to 22" -- capped at the five
/// biggest so a world with many small pockets doesn't turn this into a
/// number dump; the caller already reports the total count.
fn join_sizes(sizes: &[usize]) -> String {
    const CAP: usize = 5;
    let shown: Vec<String> = sizes.iter().take(CAP).map(|n| n.to_string()).collect();
    let joined = crate::history::join_prose(&shown);
    if sizes.len() > CAP {
        format!("{joined} cells -- plus {} smaller still", sizes.len() - CAP)
    } else {
        format!("{joined} cells")
    }
}

/// A cell's canonical prose label: a known settlement's name, or "cell N"
/// when the cell holds no settlement (mirrors `history::render_site`'s bare-
/// cell-id convention for the same case).
fn site_label(world: &World, cell: CellId) -> String {
    for settlement in hornvale_settlement::all_settlements(world) {
        if let Some(Value::Number(n)) = world
            .ledger
            .value_of(settlement.id, hornvale_settlement::CELL_ID)
            && *n as u32 == cell.0
        {
            return settlement.name;
        }
    }
    format!("cell {}", cell.0)
}
