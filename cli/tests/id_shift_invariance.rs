//! The Salt's keystone: derived prose does not move when an occupation's
//! entity id moves, but does still separate occupations that materially
//! differ.
//!
//! The pre-implementation measurement burned extra mints inside `build_to`
//! behind an env var, which is not a thing to ship. This asserts the same
//! property from the outside, and on a *live* seed-42 world rather than a
//! hand-built pair: `layer_key(o, None) == layer_key(&shifted, None)` for a
//! record whose `id` was bumped is near-vacuous once `layer_key` no longer
//! reads the id at all (it takes the *parent's* founding coordinates, not
//! the child's id). The honest strong form of the same property, and the one
//! this campaign actually promises, is that two occupations whose material
//! facts genuinely agree — which seed 42 already contains, no synthetic
//! shift required — must yield identical derived output despite carrying
//! different ids.

use hornvale_almanac::history::flesh_seed;
use hornvale_almanac::hornvale_history::flesh::{Departure, residue_of, structures_of};
use hornvale_almanac::hornvale_history::record::{
    Founding, FoundingCoords, founding_coords, founding_key_from, material_key,
};
use hornvale_astronomy::SkyPins;
use hornvale_kernel::{EntityId, Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, founder_of,
    occupation_records,
};
use std::collections::{BTreeMap, BTreeSet};

fn seed42() -> World {
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

/// Two occupations with identical material cores but different entity ids
/// must produce identical derived output. Seed 42 measurably contains such
/// pairs (7 occupations sit in colliding material-core groups, largest group
/// size 3) — no synthetic id shift is needed to exercise the property.
#[test]
fn identical_material_cores_yield_identical_flesh_despite_different_ids() {
    let world = seed42();
    let now = 5000.0;
    let occs = occupation_records(&world);
    assert!(!occs.is_empty(), "seed 42 must bake occupations");

    let mut groups: BTreeMap<
        u64,
        Vec<&hornvale_almanac::hornvale_history::record::OccupationRecord>,
    > = BTreeMap::new();
    for o in &occs {
        groups.entry(material_key(&o.core)).or_default().push(o);
    }
    let colliding: Vec<_> = groups.values().filter(|g| g.len() > 1).collect();
    assert!(
        !colliding.is_empty(),
        "seed 42 must contain at least one material-core collision (measured: \
         7 occupations spread across 3 colliding groups, largest group size 3) \
         -- zero colliding groups means this test is vacuous and proves \
         nothing about id-invariance"
    );

    for group in &colliding {
        let ids: BTreeSet<EntityId> = group.iter().map(|o| o.id).collect();
        assert_eq!(
            ids.len(),
            group.len(),
            "a material-core group must not contain the same record twice -- \
             otherwise the comparison below could pass by comparing a record \
             with itself"
        );

        let first = group[0];
        let seed_first = flesh_seed(&world, first.id);
        for &member in group.iter().skip(1) {
            assert_ne!(
                member.id, first.id,
                "group members must carry different entity ids"
            );
            let seed_member = flesh_seed(&world, member.id);
            assert_eq!(
                seed_first, seed_member,
                "flesh_seed moved between two occupations with identical \
                 material cores (ids {:?} vs {:?})",
                first.id, member.id
            );
            assert_eq!(
                structures_of(first, seed_first),
                structures_of(member, seed_member),
                "structures_of moved between two occupations with identical \
                 material cores (ids {:?} vs {:?})",
                first.id,
                member.id
            );
            assert_eq!(
                residue_of(first, now, seed_first, Departure::Climate).items,
                residue_of(member, now, seed_member, Departure::Climate).items,
                "residue_of moved between two occupations with identical \
                 material cores (ids {:?} vs {:?})",
                first.id,
                member.id
            );
        }
    }
}

/// Two occupations whose *founding* coordinates and whose parent's founding
/// coordinates agree must resolve to the same founder handle, despite
/// different entity ids. Seed 42 measurably contains 29 such colliding
/// founding-key groups (61 records total, 32 of them beyond the first in
/// their group).
#[test]
fn identical_founding_keys_yield_identical_founder_handles_despite_different_ids() {
    let world = seed42();
    let occs = occupation_records(&world);
    assert!(!occs.is_empty(), "seed 42 must bake occupations");

    let coords_by_id: BTreeMap<EntityId, FoundingCoords<'static>> = occs
        .iter()
        .map(|o| (o.id, founding_coords(&o.core)))
        .collect();

    let mut groups: BTreeMap<
        u64,
        Vec<&hornvale_almanac::hornvale_history::record::OccupationRecord>,
    > = BTreeMap::new();
    for o in &occs {
        let parent = match o.founded_from {
            Founding::From(e) => coords_by_id.get(&e).copied(),
            Founding::Genesis(_) => None,
        };
        let key = founding_key_from(founding_coords(&o.core), parent);
        groups.entry(key).or_default().push(o);
    }

    let colliding: Vec<_> = groups.values().filter(|g| g.len() > 1).collect();
    assert!(
        !colliding.is_empty(),
        "seed 42 must contain at least one colliding founding key (measured: \
         61 records across 29 groups, i.e. 32 records beyond the first in \
         each group) -- zero colliding groups means this test is vacuous \
         and proves nothing about id-invariance"
    );

    for group in &colliding {
        let ids: BTreeSet<EntityId> = group.iter().map(|o| o.id).collect();
        assert_eq!(
            ids.len(),
            group.len(),
            "a founding-key group must not contain the same record twice -- \
             otherwise the comparison below could pass by comparing a record \
             with itself"
        );

        let first = group[0];
        let handle_first = founder_of(&world, first.id);
        for &member in group.iter().skip(1) {
            assert_ne!(
                member.id, first.id,
                "group members must carry different entity ids"
            );
            assert_eq!(
                founder_of(&world, member.id),
                handle_first,
                "founder_of moved between two occupations with identical \
                 founding coordinates (ids {:?} vs {:?})",
                first.id,
                member.id
            );
        }
    }
}
