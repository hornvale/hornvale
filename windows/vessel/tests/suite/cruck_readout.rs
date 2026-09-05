//! H1: does a living dwelling's `(cold, authority, posture)` census actually
//! populate the four spec-§3.4 chamber-graph shapes? H2/H3: is the derived
//! `(roles, links)` exactly the §3.4 row for its axes, and is that row's
//! preimage class recoverable from the shape alone, with no brief consulted?

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::World;
use hornvale_vessel::housemark::{AuthorityMark, ThresholdPosture};
use hornvale_vessel::liveness::LocaleTerrain;
use hornvale_vessel::structure::grammar::frame_for;
use hornvale_vessel::{WorldContext, brief_of, structure_at};
use hornvale_worldgen::{SkyChoice, occupations_by_vertex};

const H1_SEEDS: [u64; 5] = [42, 13, 7, 1, 100];

fn world(seed: u64) -> World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .unwrap_or_else(|error| panic!("seed {seed} builds: {error:?}"))
}

/// The four spec-§3.4 chamber-graph shapes, as a pure function of the three
/// preregistered axes. Transcribed from spec §7 H1's decision table; Task 2
/// implements the same map in production and Task 6 asserts the two agree.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Shape {
    Deep,
    Bush,
    Shopfront,
    Backroom,
}

fn predicted_shape(cold: bool, authority: AuthorityMark, posture: ThresholdPosture) -> Shape {
    use AuthorityMark::*;
    use ThresholdPosture::*;
    match (cold, authority, posture) {
        (true, _, _) => Shape::Deep,
        (false, Command, Outward) => Shape::Shopfront,
        (false, Command, _) => Shape::Deep,
        (false, Common, Inward) => Shape::Backroom,
        (false, Common, _) => Shape::Bush,
    }
}

/// The links each spec-§3.4 shape admits, index-aligned to the grammar's own
/// admission order (0=Threshold, 1=Hearthroom, 2=workroom, 3=Store).
/// Transcribed from spec §3.4's picture column; Task 6's H2 checks production
/// against it.
fn shape_links(shape: Shape) -> Vec<(usize, usize)> {
    match shape {
        Shape::Deep => vec![(0, 1), (1, 2), (1, 3)],
        Shape::Bush => vec![(0, 1), (0, 2), (0, 3)],
        Shape::Shopfront => vec![(0, 1), (0, 2), (1, 3)],
        Shape::Backroom => vec![(0, 1), (1, 2), (0, 3)],
    }
}

/// H3's preimage class: every `(cold, authority, posture)` triple
/// `predicted_shape` sends to `shape`. Computed from the same preregistered
/// map H1/H2 use — the point is not that this function avoids the map, but
/// that the room's OWN classification (`shape_of_links`, below) never
/// consults the brief at all; only the final membership check does.
fn preimage(shape: Shape) -> BTreeSet<(bool, AuthorityMark, ThresholdPosture)> {
    use AuthorityMark::*;
    use ThresholdPosture::*;
    [true, false]
        .into_iter()
        .flat_map(|cold| {
            [Command, Common].into_iter().flat_map(move |authority| {
                [Inward, Plain, Outward]
                    .into_iter()
                    .map(move |posture| (cold, authority, posture))
            })
        })
        .filter(|&(cold, authority, posture)| predicted_shape(cold, authority, posture) == shape)
        .collect()
}

/// H3's own claim: which shape a structural link set belongs to, read from
/// its LINKS alone — no roles, no brief, no cold/authority/posture axis. The
/// links alone suffice because `shape_links`'s four sets are pairwise
/// distinct; roles differ only by function (a Mine's `Smithy` where an
/// Agrarian room has `Loomroom`), which relabels a chamber without moving
/// the topology, so roles carry no information this classification needs.
/// `None` if the links match none of the four preregistered shapes (e.g. a
/// function whose workroom rule refuses, which the four-chamber table does
/// not cover).
fn shape_of_links(links: &[(usize, usize)]) -> Option<Shape> {
    [Shape::Deep, Shape::Bush, Shape::Shopfront, Shape::Backroom]
        .into_iter()
        .find(|&shape| shape_links(shape) == links)
}

/// claim: readout(five preregistered Cruck worlds)
#[test]
#[ignore = "probe: Cruck H1 builds five full history worlds; run by hand at stage boundaries"]
fn h1_axes_have_living_support() {
    let mut census: BTreeMap<(bool, AuthorityMark, ThresholdPosture), usize> = BTreeMap::new();
    let mut built_total = 0usize;
    let mut inhabited_total = 0usize;
    let mut unoccupied_total = 0usize;
    let mut collision_total = 0usize;

    for seed_value in H1_SEEDS {
        let world = world(seed_value);
        let world_context = WorldContext::build(&world)
            .unwrap_or_else(|error| panic!("seed {seed_value} builds a world context: {error}"));
        let context = world_context.context();
        let walk = hornvale_locale::walk_depth(context);
        let geo = context.climate().geosphere();
        let built = world_context.built_rooms();
        let occupations = world_context.living_occupations_by_room();
        let occupation_history = occupations_by_vertex(&world);
        let collisions = world_context.settlement_room_collision_count();
        let terrain = LocaleTerrain::with_fields(context, None, None, None, Some(built), None);
        let strange_sites = context.strange_sites();
        let cave_sites = context.terrain().cave_site_vertices();
        let mut seed_inhabited = 0usize;
        let mut seed_unoccupied = 0usize;

        for room_id in built.keys() {
            let place = room_id
                .unpack()
                .unwrap_or_else(|error| panic!("seed {seed_value}: {room_id:?}: {error:?}"));
            let brief = brief_of(
                occupations,
                &occupation_history,
                geo,
                context.nearest_index(),
                &place,
                &terrain,
                walk,
                world.seed,
                &strange_sites,
                &cave_sites,
            )
            .unwrap_or_else(|error| {
                panic!("seed {seed_value}: built room {room_id:?} has no production brief: {error}")
            });

            if occupations.get(room_id).is_none() {
                seed_unoccupied += 1;
                continue;
            }
            let mark = brief.housemark.unwrap_or_else(|| {
                panic!("seed {seed_value}: inhabited room {room_id:?} has no Housemark")
            });
            *census
                .entry((brief.cold, mark.authority, mark.threshold))
                .or_default() += 1;
            seed_inhabited += 1;
        }

        built_total += built.len();
        inhabited_total += seed_inhabited;
        unoccupied_total += seed_unoccupied;
        collision_total += collisions;
    }

    println!(
        "H1 living-surface mapping: built={built_total} inhabited={inhabited_total} unoccupied={unoccupied_total} collisions={collision_total}"
    );
    println!("H1 axis census (cold, authority, posture) -> count:");
    for (key, count) in &census {
        println!("  {key:?} -> {count}");
    }

    let mut shapes: BTreeMap<Shape, usize> = BTreeMap::new();
    for (&(cold, authority, posture), &count) in &census {
        *shapes
            .entry(predicted_shape(cold, authority, posture))
            .or_default() += count;
    }
    println!("H1 predicted-shape totals:");
    for (shape, count) in &shapes {
        println!("  {shape:?} -> {count}");
    }
}

/// claim: readout(five preregistered Cruck worlds)
#[test]
#[ignore = "probe: Cruck H2/H3 builds five full history worlds; run by hand at stage boundaries"]
fn h2_h3_every_living_dwelling_is_its_table_row_and_recoverable() {
    let mut shape_totals: BTreeMap<Shape, usize> = BTreeMap::new();
    let mut witnessed: BTreeSet<Shape> = BTreeSet::new();
    let mut built_total = 0usize;
    let mut inhabited_total = 0usize;
    let mut unoccupied_total = 0usize;
    let mut collision_total = 0usize;
    let mut correct = 0usize;
    let mut mismatches: Vec<String> = Vec::new();

    for seed_value in H1_SEEDS {
        let world = world(seed_value);
        let world_context = WorldContext::build(&world)
            .unwrap_or_else(|error| panic!("seed {seed_value} builds a world context: {error}"));
        let context = world_context.context();
        let walk = hornvale_locale::walk_depth(context);
        let geo = context.climate().geosphere();
        let built = world_context.built_rooms();
        let occupations = world_context.living_occupations_by_room();
        let occupation_history = occupations_by_vertex(&world);
        let collisions = world_context.settlement_room_collision_count();
        let terrain = LocaleTerrain::with_fields(context, None, None, None, Some(built), None);
        let strange_sites = context.strange_sites();
        let cave_sites = context.terrain().cave_site_vertices();
        let mut seed_inhabited = 0usize;
        let mut seed_unoccupied = 0usize;

        for room_id in built.keys() {
            let place = room_id
                .unpack()
                .unwrap_or_else(|error| panic!("seed {seed_value}: {room_id:?}: {error:?}"));
            let brief = brief_of(
                occupations,
                &occupation_history,
                geo,
                context.nearest_index(),
                &place,
                &terrain,
                walk,
                world.seed,
                &strange_sites,
                &cave_sites,
            )
            .unwrap_or_else(|error| {
                panic!("seed {seed_value}: built room {room_id:?} has no production brief: {error}")
            });

            if occupations.get(room_id).is_none() {
                seed_unoccupied += 1;
                continue;
            }
            let mark = brief.housemark.unwrap_or_else(|| {
                panic!("seed {seed_value}: inhabited room {room_id:?} has no Housemark")
            });
            let shape = predicted_shape(brief.cold, mark.authority, mark.threshold);
            let structure = structure_at(&place, &brief, world.seed, walk).unwrap_or_else(|| {
                panic!("seed {seed_value}: inhabited room {room_id:?} has no structure")
            });

            // H2: the production structure IS the derived frame, and its
            // links are exactly the table's row for this room's axes.
            let frame = frame_for(&brief);
            if structure.roles != frame.roles {
                mismatches.push(format!(
                    "seed {seed_value} room {room_id:?}: structure roles {:?} != frame_for roles {:?}",
                    structure.roles, frame.roles
                ));
            }
            if structure.links != shape_links(shape) {
                mismatches.push(format!(
                    "seed {seed_value} room {room_id:?} ({:?}, {:?}): links {:?} != {shape:?}'s table links {:?} (function {:?})",
                    brief.cold,
                    mark,
                    structure.links,
                    shape_links(shape),
                    brief.function
                ));
            } else {
                // H3: recover the shape from the links alone -- no roles, no
                // brief, no axis -- then check the room's own triple is a
                // member of that shape's preimage class. The brief's triple
                // is read here only for the membership check, never to do
                // the classification above.
                match shape_of_links(&structure.links) {
                    Some(recovered) => {
                        let class = preimage(recovered);
                        if class.contains(&(brief.cold, mark.authority, mark.threshold)) {
                            correct += 1;
                        } else {
                            mismatches.push(format!(
                                "seed {seed_value} room {room_id:?}: triple ({}, {:?}, {:?}) is not in {recovered:?}'s preimage {class:?}",
                                brief.cold, mark.authority, mark.threshold
                            ));
                        }
                    }
                    None => mismatches.push(format!(
                        "seed {seed_value} room {room_id:?}: links {:?} match no preregistered shape",
                        structure.links
                    )),
                }
            }

            witnessed.insert(shape);
            *shape_totals.entry(shape).or_default() += 1;
            seed_inhabited += 1;
        }

        built_total += built.len();
        inhabited_total += seed_inhabited;
        unoccupied_total += seed_unoccupied;
        collision_total += collisions;
    }

    println!(
        "H2/H3 living-surface mapping: built={built_total} inhabited={inhabited_total} unoccupied={unoccupied_total} collisions={collision_total}"
    );
    println!("H2/H3 per-shape totals:");
    for (shape, count) in &shape_totals {
        println!("  {shape:?} -> {count}");
    }
    println!(
        "H2/H3 recovery: correct={correct} total={inhabited_total} mismatches={}",
        mismatches.len()
    );
    if !mismatches.is_empty() {
        println!("H2/H3 mismatches:\n{}", mismatches.join("\n"));
    }

    let expected_shapes: BTreeSet<Shape> =
        [Shape::Deep, Shape::Bush, Shape::Shopfront, Shape::Backroom]
            .into_iter()
            .collect();
    assert_eq!(
        witnessed, expected_shapes,
        "every Task 0 census shape must be witnessed by the H2/H3 readout"
    );
    assert!(
        mismatches.is_empty(),
        "H2/H3 must hold at 100%: {} mismatches:\n{}",
        mismatches.len(),
        mismatches.join("\n")
    );
    assert_eq!(
        correct, inhabited_total,
        "H3 must recover every inhabited room's own predicted shape from its links alone"
    );
}
