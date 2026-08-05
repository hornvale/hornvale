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

/// The witness world.
///
/// **Not seed 42 since The Tolerance (2026-08-04).** This file's claim is
/// id-invariance; the seed is only the world that happens to *exhibit* a
/// collision to test it on, and the anti-vacuity guards below exist precisely
/// so a witness that stops exhibiting one fails loudly instead of passing
/// empty. That is what happened: making warlikeness a per-settlement draw
/// halved seed 42's occupation count (919 records -> 459) and took its
/// material-core collisions with it, from 3 colliding groups to **zero**.
/// Re-scanned across seeds 42/1/2/3/5/7/11/13/23/1000; seed 7 carried 2
/// colliding material-core groups and became the witness.
///
/// **Moved again at the main absorb (2026-08-04), and the guard is why we
/// know.** Composing The Keeping's `is_land` decomposition with this
/// campaign's raid gate emptied seed 7 in turn — 853 occupations, **zero**
/// colliding material-core groups — so the guard reddened rather than letting
/// the test pass on nothing. Re-scanned across the same ten seeds on the
/// merged tree: 1 and 1000 carry 2 colliding groups (4 records) each; 3, 5 and
/// 13 carry 1; 42, 2, 7, 11 and 23 carry none. **Seed 1** is the witness now —
/// 901 occupations, 2 colliding material-core groups of 2, and 24 colliding
/// founding-key groups for the second test. A WITNESS is being re-pinned here,
/// never the claim: both anti-vacuity guards and every assertion below are
/// untouched, and the property they assert is the one The Salt froze.
fn witness_world() -> World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(WITNESS_SEED),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("the witness seed builds")
}

/// The seed [`witness_world`] builds. A witness, not a claim — see that
/// function's doc for why it moved off 42, and then off 7.
const WITNESS_SEED: u64 = 1;

/// Two occupations with identical material cores but different entity ids
/// must produce identical derived output. The witness seed measurably contains
/// such pairs (at seed 1: 4 occupations sit in 2 colliding material-core
/// groups, largest group size 2) — no synthetic id shift is needed to exercise
/// the property.
#[test]
fn identical_material_cores_yield_identical_flesh_despite_different_ids() {
    let world = witness_world();
    let now = 5000.0;
    let occs = occupation_records(&world);
    assert!(!occs.is_empty(), "the witness seed must bake occupations");

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
        "the witness seed must contain at least one material-core collision \
         (measured at seed 1: 4 occupations spread across 2 colliding groups, \
         largest group size 2) -- zero colliding groups means this test is \
         vacuous and proves nothing about id-invariance"
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
/// different entity ids. The witness seed measurably contains such colliding
/// founding-key groups; the guard below is what keeps that true rather than
/// assumed.
#[test]
fn identical_founding_keys_yield_identical_founder_handles_despite_different_ids() {
    let world = witness_world();
    let occs = occupation_records(&world);
    assert!(!occs.is_empty(), "the witness seed must bake occupations");

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
        "the witness seed must contain at least one colliding founding key -- \
         zero colliding groups means this test is vacuous and proves nothing \
         about id-invariance"
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
