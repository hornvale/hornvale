//! Cross-module determinism: the constitutional test (spec §6).
//! A mini-genesis run twice from the same seed must be byte-identical.

use hornvale_kernel::{
    ConstantField, EntityId, Fact, Field, ObserverContext, PhenomenaSource, Phenomenon, Position,
    Seed, Value, World, WorldTime, choose_consistent, fbm_2d, observe,
};

struct MiniSun;

impl PhenomenaSource for MiniSun {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: "celestial-body".to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
        }]
    }
}

/// A miniature genesis exercising every kernel module together.
fn mini_genesis(seed: Seed) -> String {
    let mut world = World::new(seed);
    world
        .registry
        .register_predicate("revered-phenomenon", true, "what a settlement reveres")
        .unwrap();
    world
        .registry
        .register_phenomenon_kind("celestial-body", "a body visible in the sky")
        .unwrap();

    // Terrain-ish: a place with a field-derived character.
    let vale = world.ledger.mint_entity();
    let roughness = fbm_2d(seed.derive("terrain"), 0.5, 0.5, 3);
    let biome = ConstantField("temperate forest".to_string())
        .sample(Position { x: 0.0, y: 0.0 }, WorldTime { day: 0.0 });
    assert!((0.0..1.0).contains(&roughness));
    assert_eq!(biome, "temperate forest");

    // Settlement-ish: a named village, name refined against the ledger.
    let village = world.ledger.mint_entity();
    let candidates = ["Zaggrak", "Bolnar", "Mokru", "Ishtor"];
    let mut stream = seed.derive("settlement").derive("name").stream();
    let idx = choose_consistent(
        &mut stream,
        &world.ledger,
        &world.registry,
        &candidates,
        |n| Fact {
            subject: village,
            predicate: "name".to_string(),
            object: Value::Text((*n).to_string()),
            place: Some(vale),
            day: Some(0.0),
            provenance: "settlement".to_string(),
        },
    )
    .expect("a name must survive an empty ledger");
    world
        .ledger
        .commit(
            Fact {
                subject: village,
                predicate: "name".to_string(),
                object: Value::Text(candidates[idx].to_string()),
                place: Some(vale),
                day: Some(0.0),
                provenance: "settlement".to_string(),
            },
            &world.registry,
        )
        .unwrap();

    // Religion-ish: revere the most salient phenomenon, source-blind.
    let sun = MiniSun;
    let seen = observe(
        &[&sun],
        &ObserverContext {
            place: vale,
            time: WorldTime { day: 0.0 },
        },
    );
    world
        .ledger
        .commit(
            Fact {
                subject: village,
                predicate: "revered-phenomenon".to_string(),
                object: Value::Text(seen[0].kind.clone()),
                place: Some(vale),
                day: Some(0.0),
                provenance: "religion".to_string(),
            },
            &world.registry,
        )
        .unwrap();

    world.to_json()
}

#[test]
fn same_seed_yields_byte_identical_worlds() {
    assert_eq!(mini_genesis(Seed(42)), mini_genesis(Seed(42)));
}

#[test]
fn different_seeds_yield_different_worlds() {
    // Village naming draws from the seed, so worlds should differ.
    // (Four candidates: a collision for one pair is possible but the
    // *entire serialized world* matching across these seeds is not.)
    let worlds: Vec<String> = (1..=4).map(|s| mini_genesis(Seed(s))).collect();
    assert!(worlds.windows(2).any(|w| w[0] != w[1]));
}

#[test]
fn saved_world_reloads_identically() {
    let json = mini_genesis(Seed(7));
    let world = World::from_json(&json).unwrap();
    assert_eq!(world.to_json(), json);
}

#[test]
fn entity_ids_are_never_reused_after_reload() {
    let json = mini_genesis(Seed(7));
    let mut world = World::from_json(&json).unwrap();
    let fresh = world.ledger.mint_entity();
    // vale = 1, village = 2 in mini_genesis; fresh must not collide.
    assert!(fresh != EntityId(1) && fresh != EntityId(2));
}
