//! H3: recover the living occupier's Housemark from the threshold's structure.

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{Facet, KindId, Seed, Value, World, math};
use hornvale_settlement::{LATITUDE, LONGITUDE};
use hornvale_vessel::housemark::Housemark;
use hornvale_vessel::interior::{Interior, Rcc8, Role, chamber_interior_of, selection_for};
use hornvale_vessel::liveness::LocaleTerrain;
use hornvale_vessel::site::SiteKind;
use hornvale_vessel::{Brief, WorldContext, brief_of, structure_at};
use hornvale_worldgen::{SkyChoice, occupations_by_vertex};

const H3_SEEDS: [u64; 5] = [42, 13, 7, 1, 100];

type Signature = Vec<(KindId, Rcc8)>;

fn world(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .unwrap_or_else(|error| panic!("seed {seed} builds: {error:?}"))
}

/// The production room of a living settlement, from its committed position.
fn settlement_room(
    world: &World,
    occupation: &hornvale_vessel::OccupationRecord,
    walk: u32,
) -> Facet {
    let number = |predicate| match world.ledger.value_of(occupation.id, predicate) {
        Some(Value::Number(value)) => *value,
        _ => panic!(
            "living occupation {} ({}) has no {predicate} fact",
            occupation.id.get(),
            occupation.core.people.0
        ),
    };
    Facet::containing(
        math::unit_sphere_from_lat_lon(number(LATITUDE), number(LONGITUDE)),
        walk,
    )
}

/// Reduce a rendered threshold to an ordered structural sequence.
///
/// The selected patterns supply only the required target kind, which an
/// `Interior` deliberately does not retain. Both the contributed kind and its
/// relation are read back from the composed interior; neither pattern names
/// nor rendered prose enter the signature.
fn signature_of(interior: &Interior, brief: &Brief) -> Signature {
    let selected = selection_for(
        Role::Threshold,
        brief.built,
        brief.cold,
        brief.is_populous(),
        brief.housemark,
    );
    let anchors = interior.ids();
    assert_eq!(
        anchors.len(),
        selected.len(),
        "threshold selection and composition must contribute one anchor per pattern"
    );

    let mut signature = Vec::new();
    for (pattern, anchor) in selected.into_iter().zip(anchors.iter().copied()) {
        assert_eq!(
            interior.anchor(anchor).kind,
            pattern.kind,
            "composition must preserve selected pattern order"
        );
        let Some(required_kind) = pattern.requires else {
            continue;
        };
        let required = anchors
            .iter()
            .copied()
            .find(|&candidate| interior.anchor(candidate).kind == required_kind)
            .unwrap_or_else(|| {
                panic!(
                    "composed {:?} anchor has no required {:?} target",
                    pattern.kind, required_kind
                )
            });
        signature.push((pattern.kind, interior.relation(anchor, required)));
    }
    signature
}

fn containing_vertex(
    room: &Facet,
    context: &hornvale_locale::LocaleContext,
) -> hornvale_kernel::Vertex {
    let weights = room
        .corner_weights(context.climate().geosphere(), context.nearest_index())
        .expect("a walk-band settlement room has geosphere weights");
    weights
        .iter()
        .max_by(|a, b| a.1.cmp(&b.1).then(b.0.0.cmp(&a.0.0)))
        .map(|&(vertex, _)| vertex)
        .expect("a walk-band settlement room has at least one weighted corner")
}

/// Regression: replacing the room-keyed lookup with `containing_vertex` makes
/// this seed-42 snow-elf settlement read unoccupied, because its cube-sphere
/// room reverses to a neighbouring icosphere vertex.
#[test]
fn a_neighbour_reversing_settlement_room_keeps_its_own_living_occupation() {
    let world = world(42);
    let world_context = WorldContext::build(&world).expect("seed 42 builds a world context");
    let context = world_context.context();
    let walk = hornvale_locale::walk_depth(context);
    let occupations = occupations_by_vertex(&world);
    let occupation = occupations
        .get(&hornvale_kernel::Vertex(609))
        .and_then(|rows| rows.iter().find(|row| row.is_alive()))
        .expect("seed 42 vertex 609 retains its living occupation");
    assert_eq!(
        occupation.core.people,
        KindId("snow-elf"),
        "the frozen regression vertex still belongs to snow-elf"
    );
    let room = settlement_room(&world, occupation, walk);
    assert_eq!(
        containing_vertex(&room, context),
        hornvale_kernel::Vertex(38_248),
        "fixture precondition: the room reverses to the measured neighbour"
    );
    let built = world_context.built_rooms();
    assert!(
        built.contains_key(&room.pack().expect("the settlement room packs")),
        "fixture precondition: this is the occupation's production built room"
    );
    let terrain = LocaleTerrain::with_fields(context, None, None, None, Some(built), None);
    let brief = brief_of(
        world_context.living_occupations_by_room(),
        context.climate().geosphere(),
        &room,
        &terrain,
        walk,
        world.seed,
        &context.strange_sites(),
        &context.terrain().cave_site_vertices(),
    )
    .expect("the production settlement room has a brief");

    assert_eq!(
        brief.people,
        Some(occupation.core.people),
        "a settlement room must read its own selected living occupation, not a geometric neighbour"
    );
}

/// Regression: a surface and subterranean community may intentionally share
/// one player address; both the room's name and its occupation use the first
/// settlement in the production roster.
#[test]
fn a_shared_settlement_room_uses_the_same_first_settlement_for_name_and_people() {
    let world = world(42);
    let world_context = WorldContext::build(&world).expect("seed 42 builds a world context");
    let context = world_context.context();
    let walk = hornvale_locale::walk_depth(context);
    let occupations = occupations_by_vertex(&world);
    let colocated: Vec<_> = occupations
        .get(&hornvale_kernel::Vertex(2_103))
        .expect("seed 42 retains the measured shared column")
        .iter()
        .filter(|row| row.is_alive())
        .collect();
    assert_eq!(
        colocated.len(),
        2,
        "fixture precondition: two living rung occupants share this column"
    );
    let room = settlement_room(&world, colocated[0], walk);
    assert_eq!(
        settlement_room(&world, colocated[1], walk),
        room,
        "the two rung occupants share one player-addressable room"
    );
    let room_id = room.pack().expect("the shared settlement room packs");
    let first = hornvale_settlement::all_settlements(&world)
        .into_iter()
        .find(|settlement| {
            colocated
                .iter()
                .find(|row| row.id == settlement.id)
                .is_some_and(|row| settlement_room(&world, row, walk) == room)
        })
        .expect("the shared room has a first settlement");
    let selected = colocated
        .iter()
        .find(|row| row.id == first.id)
        .expect("the first settlement is one of the colocated occupations");
    let built = world_context.built_rooms();
    assert_eq!(
        built.get(&room_id),
        Some(&first.name),
        "built-room naming keeps the first settlement"
    );
    assert_eq!(
        world_context
            .living_occupations_by_room()
            .get(&room_id)
            .map(|occupation| occupation.id),
        Some(selected.id),
        "room naming and room occupation must select the same first settlement"
    );
    let terrain = LocaleTerrain::with_fields(context, None, None, None, Some(built), None);
    let brief = brief_of(
        world_context.living_occupations_by_room(),
        context.climate().geosphere(),
        &room,
        &terrain,
        walk,
        world.seed,
        &context.strange_sites(),
        &context.terrain().cave_site_vertices(),
    )
    .expect("the shared production settlement room has a brief");

    assert_eq!(brief.people, Some(selected.core.people));
}

/// claim: readout(five preregistered Housemark worlds)
#[test]
#[ignore = "probe: Housemark H3 builds five full history worlds; run by hand at stage boundaries"]
fn h3_housemark_readout_recovers_every_inhabited_brief() {
    let mut recovered: BTreeMap<Signature, Housemark> = BTreeMap::new();
    let mut peoples = BTreeSet::new();
    let mut classes = BTreeSet::new();
    let mut per_seed = BTreeMap::new();
    let mut built_total = 0usize;
    let mut inhabited_total = 0usize;
    let mut unoccupied_total = 0usize;
    let mut collision_total = 0usize;
    let mut correct = 0usize;

    for seed_value in H3_SEEDS {
        let world = world(seed_value);
        let world_context = WorldContext::build(&world)
            .unwrap_or_else(|error| panic!("seed {seed_value} builds a world context: {error}"));
        let context = world_context.context();
        let walk = hornvale_locale::walk_depth(context);
        let geo = context.climate().geosphere();
        let built = world_context.built_rooms();
        let occupations = world_context.living_occupations_by_room();
        let collisions = world_context.settlement_room_collision_count();
        let terrain = LocaleTerrain::with_fields(context, None, None, None, Some(built), None);
        let strange_sites = context.strange_sites();
        let cave_sites = context.terrain().cave_site_vertices();
        let mut seed_inhabited = 0usize;
        let mut seed_unoccupied = 0usize;

        assert!(
            occupations.keys().all(|room| built.contains_key(room)),
            "seed {seed_value}: a room-indexed living occupation has no built settlement"
        );
        for room_id in built.keys() {
            let place = room_id
                .unpack()
                .unwrap_or_else(|error| panic!("seed {seed_value}: {room_id:?}: {error:?}"));
            let brief = brief_of(
                occupations,
                geo,
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
            assert_eq!(
                brief.site.as_ref().map(|site| site.kind),
                Some(SiteKind::Settlement),
                "seed {seed_value}: every built room is a settlement site"
            );
            let structure = structure_at(&place, &brief, world.seed, walk).unwrap_or_else(|| {
                panic!("seed {seed_value}: built room {room_id:?} has no structure")
            });
            let threshold = structure
                .chambers
                .first()
                .expect("every production structure has chamber index zero");
            assert_eq!(
                threshold, &structure.threshold,
                "chamber index zero is the production threshold"
            );

            let Some(occupation) = occupations.get(room_id) else {
                assert_eq!(
                    (brief.people, brief.housemark),
                    (None, None),
                    "seed {seed_value}: genuinely unoccupied room {room_id:?} acquired culture"
                );
                seed_unoccupied += 1;
                continue;
            };
            assert_eq!(
                brief.people,
                Some(occupation.core.people),
                "seed {seed_value}: built room {room_id:?} did not recover its selected occupation"
            );
            let mark = brief.housemark.unwrap_or_else(|| {
                panic!(
                    "seed {seed_value}: inhabited room {room_id:?} ({}) has no Housemark",
                    occupation.core.people.0
                )
            });
            let interior = chamber_interior_of(threshold, &terrain, walk, &brief, 0);
            let signature = signature_of(&interior, &brief);

            match recovered.get(&signature) {
                Some(prior) => assert_eq!(
                    *prior, mark,
                    "H3 signature {signature:?} maps to both {prior:?} and {mark:?}"
                ),
                None => {
                    recovered.insert(signature, mark);
                }
            }
            peoples.insert(occupation.core.people);
            classes.insert(mark);
            seed_inhabited += 1;
            correct += 1;
        }

        assert_eq!(
            built.len(),
            seed_inhabited + seed_unoccupied,
            "seed {seed_value}: every distinct built room must be accounted for"
        );
        assert_eq!(
            occupations.len(),
            seed_inhabited,
            "seed {seed_value}: every selected living occupation must produce an inhabited brief"
        );
        per_seed.insert(
            seed_value,
            (built.len(), seed_inhabited, seed_unoccupied, collisions),
        );
        built_total += built.len();
        inhabited_total += seed_inhabited;
        unoccupied_total += seed_unoccupied;
        collision_total += collisions;
    }

    println!(
        "H3 living-surface mapping: built={built_total} inhabited={inhabited_total} unoccupied={unoccupied_total} collisions={collision_total} peoples={} classes={} per_seed(built,inhabited,unoccupied,collisions)={per_seed:?}",
        peoples.len(),
        classes.len()
    );
    println!(
        "H3 Housemark recovery: correct={correct} total={inhabited_total} peoples={} classes={} per_seed={per_seed:?}",
        peoples.len(),
        classes.len()
    );
    println!("H3 signatures: {recovered:#?}");

    assert_eq!(
        correct, inhabited_total,
        "H3 must recover every inhabited brief's Housemark"
    );
}
