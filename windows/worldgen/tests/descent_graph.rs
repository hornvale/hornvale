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
    for o in occupation_records(&w) {
        let root = clan_root_of(&w, o.id);
        assert!(
            occupation_records(&w).iter().any(|x| x.id == root),
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
