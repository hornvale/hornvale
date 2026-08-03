//! The person-descent graph: a reprojection of the committed community tree.

use hornvale_astronomy::SkyPins;
use hornvale_history::descent::Kinship;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, clan_root_of,
    forebear_of, founder_of, generation_length_of, occupation_records,
};

fn seed42() -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("seed 42 builds")
}

#[test]
fn every_occupation_has_a_founder_and_the_handle_is_stable() {
    let w = seed42();
    let occs = occupation_records(&w);
    assert!(!occs.is_empty(), "seed 42 bakes occupations");
    for o in occs.iter().take(50) {
        assert_eq!(founder_of(&w, o.id), founder_of(&w, o.id));
    }
}

#[test]
fn two_different_occupations_have_two_different_founders() {
    let w = seed42();
    let occs = occupation_records(&w);
    let a = founder_of(&w, occs[0].id);
    let b = founder_of(&w, occs[1].id);
    assert_ne!(a, b, "distinct occupations must not share a founder handle");
}

#[test]
fn the_clan_walk_terminates_for_every_occupation() {
    // The committed tree is acyclic, but this walk is pub and must not
    // assume it. Seed 42's deepest chain is 29 links.
    let w = seed42();
    // Reconstructed ONCE. The membership check inside the loop used to call
    // `occupation_records` again, making the test quadratic in the world's
    // ~1800 occupations (53 s, the longest in this crate). The records do
    // not change across the loop, so hoisting is behaviour-preserving.
    let occs = occupation_records(&w);
    for o in &occs {
        let root = clan_root_of(&w, o.id);
        assert!(
            occs.iter().any(|x| x.id == root),
            "clan root {root:?} is not an occupation in this world"
        );
    }
}

#[test]
fn a_genesis_occupation_is_its_own_clan_root_and_has_no_forebear() {
    let w = seed42();
    let genesis: Vec<_> = occupation_records(&w)
        .into_iter()
        .filter(|o| {
            matches!(
                o.founded_from,
                hornvale_history::record::Founding::Genesis(_)
            )
        })
        .collect();
    assert_eq!(genesis.len(), 17, "seed 42 has 17 genesis roots");
    for o in genesis {
        assert_eq!(clan_root_of(&w, o.id), o.id);
        assert!(forebear_of(&w, o.id).is_none());
    }
}

#[test]
fn some_edges_resolve_to_siblings_and_some_to_ancestors() {
    // Seed 42: 13% zero-hop. If EVERY edge came back one way, the kinship
    // derivation would be inert and this test is the guard against that.
    let w = seed42();
    let mut siblings = 0usize;
    let mut ancestors = 0usize;
    for o in occupation_records(&w) {
        match forebear_of(&w, o.id) {
            Some((_, Kinship::Sibling)) => siblings += 1,
            Some((_, Kinship::Ancestor(_))) => ancestors += 1,
            None => {}
        }
    }
    assert!(siblings > 0, "expected ~13% sibling edges, got none");
    assert!(
        ancestors > 0,
        "expected a majority of ancestor edges, got none"
    );
}

#[test]
fn generation_length_is_resolved_per_species_and_differs_across_the_roster() {
    let w = seed42();
    let goblin = generation_length_of(&w, "goblin").expect("goblin has a generation length");
    let bugbear = generation_length_of(&w, "bugbear").expect("bugbear has a generation length");
    // Spec 1.1: goblin 21.7 y, bugbear 35.6 y.
    assert!(
        (goblin - 21.7).abs() < 0.5,
        "goblin generation length was {goblin}"
    );
    assert!(
        (bugbear - 35.6).abs() < 0.5,
        "bugbear generation length was {bugbear}"
    );
}

#[test]
fn generation_length_of_is_none_for_a_species_outside_the_roster() {
    let w = seed42();
    assert!(
        generation_length_of(&w, "not-a-real-species-label").is_none(),
        "an unrostered species has no derivable generation length"
    );
}

/// `forebear_of` must return `None`, not a guessed `Kinship::Sibling`, when
/// the daughter's species has no derivable generation length. Seed 42's real
/// roster cannot reach this path — every `Settled` people is `Endotherm` or
/// `Ectotherm`, never `Ametabolic` or absent from the roster — so this
/// constructs the case directly: a minimal two-occupation ledger whose
/// daughter's `occ-people` names a species outside the roster, built without
/// going through a full world build.
#[test]
fn forebear_of_is_none_when_the_generation_length_cannot_be_derived() {
    use hornvale_kernel::{Fact, Value, World};

    fn commit(
        world: &mut World,
        subject: hornvale_kernel::EntityId,
        predicate: &str,
        object: Value,
    ) {
        world
            .ledger
            .commit(
                Fact {
                    subject,
                    predicate: predicate.to_string(),
                    object,
                    place: None,
                    day: Some(0.0),
                    provenance: "descent_graph test fixture".to_string(),
                },
                &world.registry,
            )
            .expect("fixture facts commit cleanly");
    }

    let mut w = World::new(Seed(42));
    hornvale_history::register_concepts(&mut w.registry).expect("registers cleanly");

    let mother = w.ledger.mint_entity();
    let child = w.ledger.mint_entity();

    commit(
        &mut w,
        mother,
        hornvale_history::IS_OCCUPATION,
        Value::Flag(true),
    );
    commit(
        &mut w,
        mother,
        hornvale_history::OCC_PEOPLE,
        Value::Text("goblin".to_string()),
    );
    commit(
        &mut w,
        mother,
        hornvale_history::OCC_FOUNDED,
        Value::Number(0.0),
    );

    commit(
        &mut w,
        child,
        hornvale_history::IS_OCCUPATION,
        Value::Flag(true),
    );
    commit(
        &mut w,
        child,
        hornvale_history::OCC_PEOPLE,
        Value::Text("not-a-real-species-label".to_string()),
    );
    commit(
        &mut w,
        child,
        hornvale_history::OCC_FOUNDED,
        Value::Number(100.0),
    );
    commit(
        &mut w,
        child,
        hornvale_history::OCC_FOUNDED_FROM,
        Value::Entity(mother),
    );

    assert!(
        forebear_of(&w, child).is_none(),
        "an undeterminable generation length must not resolve to a guessed Kinship"
    );
}

/// The Salt: a founder's handle must be a function of the founding, not of
/// the occupation's entity id.
#[test]
fn founder_handles_are_free_of_the_entity_id() {
    let w = seed42();
    let occs = occupation_records(&w);
    // Every occupation's handle must be reproducible from its material facts
    // alone. Proxy: two occupations with identical founding coordinates AND
    // identical parent coordinates must share a handle, which cannot happen
    // while the id is in the mix.
    use std::collections::BTreeMap;
    let by_id: BTreeMap<u64, &_> = occs.iter().map(|o| (o.id.get(), o)).collect();
    let key_of = |o: &hornvale_history::record::OccupationRecord| {
        let parent = match o.founded_from {
            hornvale_history::record::Founding::From(e) => by_id
                .get(&e.get())
                .map(|p| hornvale_history::record::founding_coords(&p.core)),
            hornvale_history::record::Founding::Genesis(_) => None,
        };
        hornvale_history::record::founding_key(&o.core, parent)
    };
    let mut seen: BTreeMap<u64, hornvale_history::flesh::RoleHandle> = BTreeMap::new();
    let mut shared = 0usize;
    for o in &occs {
        let h = founder_of(&w, o.id);
        if let Some(prev) = seen.insert(key_of(o), h) {
            shared += 1;
            assert_eq!(prev, h, "same founding key must yield the same handle");
        }
    }
    assert!(
        shared > 0,
        "seed 42 must contain colliding founding keys (measured 8.4%); \
         zero means the key is not the one specced"
    );
    // Mutation check (The Salt, constraint 4): `same founding key => same
    // handle` alone is satisfied trivially by a `founder_of` that always
    // returns the same constant handle. That mutation was tried and
    // confirmed this test stays green under it, so this assertion is added
    // to also require the converse direction: distinct founding keys must
    // land on distinct handles. With ~700 occupations mixed through a
    // splitmix-style hash, an accidental collision in the codomain is not
    // expected; a `RoleHandle` that ignores the key entirely is what this
    // catches.
    let distinct_handles: std::collections::BTreeSet<u64> = seen.values().map(|h| h.0).collect();
    assert_eq!(
        distinct_handles.len(),
        seen.len(),
        "distinct founding keys must map to distinct handles"
    );
}

/// The founding key excludes everything after the founding, so a founder's
/// handle must not move when their community's fate does.
#[test]
fn a_founders_handle_does_not_depend_on_how_the_community_ended() {
    let w = seed42();
    let occs = occupation_records(&w);
    let dead = occs
        .iter()
        .find(|o| o.core.ended.is_some() && o.core.peak_population > 0)
        .expect("seed 42 has completed occupations");
    // Recompute the handle from the material core with the ending perturbed.
    let mut later = dead.core.clone(); // Occupation is Clone, not Copy
    later.ended = Some(later.founded + 9999.0);
    later.peak_population = dead.core.peak_population + 777;
    let parent = match dead.founded_from {
        hornvale_history::record::Founding::From(e) => occs
            .iter()
            .find(|p| p.id == e)
            .map(|p| hornvale_history::record::founding_coords(&p.core)),
        hornvale_history::record::Founding::Genesis(_) => None,
    };
    assert_eq!(
        hornvale_history::record::founding_key(&dead.core, parent),
        hornvale_history::record::founding_key(&later, parent),
    );
}

/// A people the CANONICAL roster has never heard of must still get distinct
/// founder handles.
///
/// The Salt's first implementation of `founder_of` resolved the ledger's
/// people label against `WorldComponents::assemble()` in order to obtain a
/// `'static` `KindId`. That looked tidy and was wrong: Lab's synthetic
/// rosters carry species the canonical roster does not contain — `goblin-twin`
/// is the whole basis of `census-of-the-meeting`, the solo-roster null
/// control — so every occupation in that study would have resolved to `None`
/// and collapsed onto ONE founder handle, giving every figure in a committed
/// census fixture the same name. Nothing in the suite would have gone red.
///
/// This pins the property directly: two occupations of a non-canonical people,
/// differing only in their founding coordinates, must yield different handles.
#[test]
fn a_people_outside_the_canonical_roster_still_gets_distinct_founders() {
    use hornvale_kernel::{EntityId, Fact, Value, World};

    let mut world = World::new(Seed(42));
    hornvale_worldgen::register_all(&mut world.registry).expect("registry registers");

    let found = |world: &mut World, cell: f64, day: f64| -> EntityId {
        let id = world.ledger.mint_entity();
        for (predicate, object) in [
            (
                hornvale_history::OCC_PEOPLE,
                // NOT in the canonical roster — Lab mints this one itself.
                Value::Text("goblin-twin".to_string()),
            ),
            (hornvale_history::OCC_SITE, Value::Number(cell)),
            (hornvale_history::OCC_FOUNDED, Value::Number(day)),
        ] {
            world
                .ledger
                .commit(
                    Fact {
                        subject: id,
                        predicate: predicate.to_string(),
                        object,
                        place: Some(id),
                        day: Some(day),
                        provenance: "test-fixture".to_string(),
                    },
                    &world.registry,
                )
                .expect("fixture fact commits");
        }
        id
    };

    let a = found(&mut world, 10.0, 500.0);
    let b = found(&mut world, 11.0, 500.0);
    let c = found(&mut world, 10.0, 525.0);

    let (ha, hb, hc) = (
        founder_of(&world, a),
        founder_of(&world, b),
        founder_of(&world, c),
    );
    assert_ne!(ha, hb, "a different site must give a different founder");
    assert_ne!(
        ha, hc,
        "a different founding day must give a different founder"
    );
    assert_eq!(ha, founder_of(&world, a), "the handle must be stable");
}
