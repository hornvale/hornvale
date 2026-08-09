//! The whole colour seam, end to end (The Lantern, spec §3, §6 H2).
//!
//! **THE POINT OF THIS FILE IS THE PATH, NOT THE NODES.** `fabric.rs`,
//! `light.rs` and `kernel::color` each carry unit tests, and The Beholding
//! shipped exactly that shape one band up: a test at every node of its colour
//! path and none on the path itself, so a `return null` in `sightOf` left 59
//! of 59 tests green while the feature was dead. Every assertion here reads
//! the LAST value in the chain — `PaletteEntry::color`, or the screen triple a
//! cell's light and fabric produce — never an intermediate.
//!
//! **Measured on derived fabric.** H2's substrate is real bedrock: the
//! `MaterialBuffer` and `RockClass` a generated settlement actually stands on,
//! read through `hornvale_terrain::lithology::reflectance`. No `MaterialBuffer`
//! is authored anywhere in this file. The spec's own probe showed the two
//! flames apart on authored limestone, so the claim left to make is that the
//! effect SURVIVES derived stone, and an authored fixture cannot make it.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::color::{Illuminant, Observer, blackbody, standard_observer};
use hornvale_kernel::{CellId, Seed, Value, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::fabric::{Fabric, FabricContext, reflectance_of};
use hornvale_vessel::light::{HEARTH_KELVIN, Source, hearth_cell, light_field};
use hornvale_vessel::structure::structure_at;
use hornvale_vessel::{
    Brief, Cell, Lattice, PossessOpts, Session, SessionPlan, SpatialChannel, embed_with, extent_for,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// The walk depth the vessel's own lattice fixtures use — the same constant
/// `lantern_light.rs` draws its geometry at.
const WALK: u32 = 12;

/// The seeds H2 sweeps. Four worlds rather than one: one world is an anecdote,
/// and seed 42 alone has given four wrong readings in this project's history.
/// Four rather than H1's eight because each one here is a `Settlements`-depth
/// build **plus** a lattice search, and H1 already established that derived
/// stone varies across seeds — this claim is about the two LIGHTS, which the
/// bedrock only has to be real for.
const H2_SEEDS: [u64; 4] = [1, 7, 42, 1024];

/// A built place; the brief `allocate` is selected by.
fn built() -> Brief {
    Brief::from_parts(None, None, None, None, 0, true, true)
}

/// A real world's chamber plan, taken through a live possession.
///
/// The whole seam runs inside `Session::snapshot` — fabric context, light
/// field, observer, `sense`, `to_srgb`, palette — so reading the plan off a
/// session is the only way to assert on what the game actually emits.
fn chamber_plan_at_seed(seed: u64) -> SessionPlan {
    let world = world_at(seed);
    let (mut session, _) = Session::start(&world, &PossessOpts::default())
        .unwrap_or_else(|e| panic!("seed {seed} possesses: {e:?}"));
    session.handle("enter");
    let snap = session.snapshot().expect("a live session snapshots");
    match snap.spatial {
        SpatialChannel::Chamber { plan } => plan,
        SpatialChannel::Walk { .. } => {
            panic!("seed {seed}: `enter` did not put the possession inside a building")
        }
    }
}

fn world_at(seed: u64) -> World {
    hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .unwrap_or_else(|e| panic!("seed {seed} builds: {e:?}"))
}

/// The ground under seed `seed`'s **flagship** settlement, as the fabric rules
/// read it.
///
/// Real bedrock: `GeneratedTerrain::material_at` and `rock_at` at the cell the
/// settlement's own `hornvale_settlement::CELL_ID` fact names — the identical
/// cell the composition root read `climate.biome_at` at when it committed the
/// settlement's biome, so fabric and prose cannot disagree about which ground
/// this is (`fabric.rs`'s module doc records the rule).
///
/// `BuildDepth::Settlements` is the shallowest rung that places any; a `Full`
/// build would pay for culture, religion and deep time this reads nothing of.
fn flagship_ground(seed: u64) -> (FabricContext, String) {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .as_ref()
        .expect("BuildDepth::Settlements produces terrain");
    let climate = artifacts
        .climate
        .as_ref()
        .expect("BuildDepth::Settlements produces climate");
    let settlements = hornvale_settlement::all_settlements(&artifacts.world);
    let flagship = settlements
        .first()
        .unwrap_or_else(|| panic!("seed {seed} condensed no settlements"));
    let cell = match artifacts
        .world
        .ledger
        .value_of(flagship.id, hornvale_settlement::CELL_ID)
    {
        Some(Value::Number(n)) => CellId(*n as u32),
        _ => panic!("settlement {} has no cell-id fact", flagship.id.0),
    };
    let ctx = FabricContext::at(terrain, climate, cell);
    let rock = format!("{:?}", ctx.rock);
    (ctx, rock)
}

/// A real lattice for `seed`: the first built structure with a doorway in it,
/// searched rather than pinned.
///
/// A one-chamber structure has no `Threshold` at all and so no doorway to
/// light; the search says so out loud rather than silently measuring nothing.
fn lattice_with_a_doorway(seed: Seed) -> Lattice {
    for n in 0u64..4096 {
        let locale = hornvale_kernel::RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| ((n >> (2 * i)) & 0b11) as u8).collect(),
        };
        let Some(structure) = structure_at(&locale, &built(), seed, WALK) else {
            continue;
        };
        if structure.chambers.len() < 2 {
            continue;
        }
        let extent = extent_for(&structure);
        let lattice = embed_with(&structure, &built(), extent, seed);
        if !lattice.doorways.is_empty() {
            return lattice;
        }
    }
    panic!("no locale in 4096 draws a structure with a doorway at {seed:?}");
}

/// Chebyshev distance, the metric `shadowcast` bounds a source's reach by.
fn chebyshev(a: Cell, b: Cell) -> i32 {
    (a.0 - b.0).abs().max((a.1 - b.1).abs())
}

/// The screen triple `refl` makes under `light`, through the human-calibrated
/// standard observer — the same last step `PaletteEntry::color` takes.
fn srgb(observer: &Observer, ctx: &FabricContext, light: &Illuminant) -> [u8; 3] {
    let refl = reflectance_of(Fabric::Stone, ctx);
    observer
        .to_srgb(&observer.sense(&refl, light))
        .expect("the standard observer declares a projection")
}

/// The same stone, in the same room, seen once under the hearth's light and
/// once under the day coming through the doorway.
///
/// **The probe cell is EQUIDISTANT from both sources**, which is the whole
/// control: attenuation is achromatic and depends only on distance, so at
/// equal distance the falloff factor is identical in both fields and the only
/// thing left that can move the triple is the SPECTRUM. Comparing a cell
/// beside the hearth against a cell beside the door would have measured
/// brightness — a difference these two lights would produce even if they were
/// the same colour — which is the "right measurement, wrong attribution"
/// shape this project keeps hitting.
///
/// Returns `None` when no cell of this lattice is reached by both sources at
/// an equal distance; the caller reports that rather than passing vacuously.
fn two_lit_cells_in_one_room(seed: u64) -> Option<([u8; 3], [u8; 3], String)> {
    let (ctx, rock) = flagship_ground(seed);
    let lattice = lattice_with_a_doorway(Seed(seed));
    let door = lattice.doorways[0].2;
    let chamber = lattice.doorways[0].0;
    let hearth = hearth_cell(&lattice, chamber)?;

    let observer = standard_observer();
    // Daylight at a real sun altitude, exactly as `eyes::daylight_at` builds
    // it for the walk band: the world's own star, reddened for the sun's
    // elevation. Taken at 30 degrees rather than read off a calendar because
    // this probe is about the two SPECTRA, and a fixed elevation keeps the day
    // from wandering between seeds.
    let star =
        hornvale_astronomy::generate_star(Seed(seed).derive(hornvale_astronomy::streams::ROOT));
    let day = hornvale_astronomy::at_elevation(&hornvale_astronomy::daylight(&star), 30.0);

    let radius = 12;
    let hearth_field = light_field(
        &lattice,
        &[Source {
            at: hearth,
            illuminant: blackbody(HEARTH_KELVIN),
            radius,
        }],
    );
    let door_field = light_field(
        &lattice,
        &[Source {
            at: door,
            illuminant: day,
            radius,
        }],
    );

    let probe = hearth_field.keys().copied().find(|cell| {
        door_field.contains_key(cell) && chebyshev(*cell, hearth) == chebyshev(*cell, door)
    })?;
    Some((
        srgb(&observer, &ctx, &hearth_field[&probe]),
        srgb(&observer, &ctx, &door_field[&probe]),
        rock,
    ))
}

/// THE WHOLE SEAM IN ONE TEST. Unit tests at every node with none on the path
/// is the shape that hid The Beholding's `sightOf` defect: a `return null` left
/// 59 of 59 tests green. This drives fabric -> light -> sense -> to_srgb ->
/// `PaletteEntry::color` and asserts on the LAST value.
///
/// FIRES WHEN: any link in the chain silently returns `None` or a default —
/// mutation-proven from both ends in Task 5 step 5, an empty `light_field` and
/// a `fabric_of` that always declines, each of which reddens this assertion on
/// its own.
#[test]
fn the_chamber_palette_carries_a_colour_derived_through_the_whole_seam() {
    let plan = chamber_plan_at_seed(42);
    let coloured = plan.palette.iter().filter(|e| e.color.is_some()).count();
    eprintln!(
        "seam: {coloured} of {} palette entries carry a colour; entries {:?}",
        plan.palette.len(),
        plan.palette
            .iter()
            .map(|e| (e.kind.as_str(), e.color))
            .collect::<Vec<_>>()
    );
    assert!(
        coloured > 0,
        "no palette entry carries a colour: the seam is broken"
    );
    let walls: Vec<_> = plan.palette.iter().filter(|e| e.kind == "wall").collect();
    assert!(
        !walls.is_empty(),
        "the entered chamber has no wall in its palette, so this test cannot \
         see whether a wall gets a colour"
    );
    assert!(
        walls.iter().any(|e| e.color.is_some()),
        "walls have a fabric and a light but no colour"
    );
    // Not every entry the same colour: one shared triple would be a seam that
    // reads the fabric and ignores the light entirely, and it would pass every
    // assertion above.
    let distinct: std::collections::BTreeSet<[u8; 3]> =
        plan.palette.iter().filter_map(|e| e.color).collect();
    assert!(
        distinct.len() > 1,
        "every coloured entry carries the same triple {distinct:?} — the light \
         field is not reaching the palette"
    );
}

/// H2 — a hearth-lit cell and a doorway-lit cell in the same room differ.
///
/// **Measured on DERIVED fabric**: the stone is `lithology::reflectance` over
/// the real `MaterialBuffer` and `RockClass` under each seed's flagship
/// settlement, not the authored limestone the spec's §4.2 probe used. That the
/// probe showed the effect on limestone is exactly why the claim left here is
/// about SURVIVAL rather than existence.
///
/// The probe cell is equidistant from both sources, so attenuation cancels and
/// only the spectrum can move the triple (see
/// [`two_lit_cells_in_one_room`]).
///
/// FIRES WHEN: 1200 K and daylight render the same stone identically — which
/// would mean the two placed lights are one light as far as the screen is
/// concerned, and the hearth earns nothing. **A falsified H2 is a finding, not
/// a failure**: report the numbers, never widen the comparison.
///
/// claim: invariant(forall-seed) — H2 holds per seed over a pinned four-seed
/// set: for EVERY seed the hearth-lit and doorway-lit triples must differ. The
/// sweep exists because one world is an anecdote, not to build a distribution.
#[test]
fn h2_hearth_light_and_doorway_light_differ_on_derived_fabric() {
    let mut measured = 0;
    for seed in H2_SEEDS {
        let Some((hearth_lit, doorway_lit, rock)) = two_lit_cells_in_one_room(seed) else {
            eprintln!("H2 seed {seed}: no cell equidistant from both sources — skipped");
            continue;
        };
        measured += 1;
        eprintln!("H2 seed {seed} on {rock}: hearth {hearth_lit:?} vs doorway {doorway_lit:?}");
        assert_ne!(
            hearth_lit, doorway_lit,
            "seed {seed}: 1200 K and daylight render the same {rock} identically"
        );
    }
    assert!(
        measured > 0,
        "no seed in {H2_SEEDS:?} produced a probe cell, so H2 was not measured \
         at all — an empty sweep passes every assertion above vacuously"
    );
}

/// A threshold has no fabric — an opening is not a material, which The
/// Beholding already established. Its colour stays absent.
///
/// FIRES WHEN: a threshold picks up its neighbour's fabric.
#[test]
fn a_threshold_carries_no_colour() {
    let plan = chamber_plan_at_seed(42);
    let thresholds = plan
        .palette
        .iter()
        .filter(|e| e.kind == "threshold")
        .count();
    assert!(
        thresholds > 0,
        "seed 42's plan holds no threshold entry, so this test asserts nothing \
         — re-point it at a structure with a doorway"
    );
    for entry in plan.palette.iter().filter(|e| e.kind == "threshold") {
        assert_eq!(entry.color, None, "a threshold was given a fabric colour");
    }
}

/// The palette must stay an INDEX, not degenerate into one entry per cell.
/// Reported, then pinned — this number was measured, not assumed.
///
/// FIRES WHEN: interning stops collapsing cells that share a type AND a
/// colour, which is what widening the key to `(CellKind, Option<[u8; 3]>)`
/// risks: the `u8` triple is the quantization doing the collapsing, and a
/// finer colour would put one entry under every cell.
#[test]
fn the_palette_stays_bounded_after_interning_on_colour() {
    let plan = chamber_plan_at_seed(42);
    eprintln!(
        "palette entries: {} over {} cells",
        plan.palette.len(),
        plan.cells.len()
    );
    assert!(
        plan.palette.len() < plan.cells.len() / 4,
        "palette has {} entries for {} cells — interning has stopped working",
        plan.palette.len(),
        plan.cells.len()
    );
}
