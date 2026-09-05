//! H1: does a living dwelling's `(cold, authority, posture)` census actually
//! populate the four spec-§3.4 chamber-graph shapes?

use std::collections::BTreeMap;

use hornvale_kernel::World;
use hornvale_vessel::housemark::{AuthorityMark, ThresholdPosture};
use hornvale_vessel::liveness::LocaleTerrain;
use hornvale_vessel::{WorldContext, brief_of};
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
