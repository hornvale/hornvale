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
/// **Moved a THIRD time under The Tense (2026-08-05), and the guard is why we
/// know again.** Era-varying capacity re-placed every world and seed 1's
/// collisions went to ZERO (716 occupations, no colliding material-core group
/// at all). Re-scanned the same family of seeds plus 0/4/6/8/9/10:
///
/// ```text
///   seed   6: 175 occupations, 5 colliding groups, 10 records   <- witness
///   seed   7: 493 occupations, 3 groups,  6 records
///   seed   3: 517 occupations, 2 groups,  4 records
///   seeds 4, 5, 23: 1 group each;  0, 1, 2, 8, 9, 10, 11, 13, 42, 1000: none
/// ```
///
/// **Seed 6** is the witness now, chosen for MARGIN rather than continuity: at
/// five colliding groups it carries more than any seed this test has ever used,
/// which is the property that keeps it from going vacuous a fifth time.
///
/// That this is the fourth witness in three days is itself the finding. The
/// campaign retrospective carries it as an open follow-up: the file's header
/// records that a synthetic ID SHIFT was considered and rightly rejected as
/// near-vacuous, but that is a different thing from CONSTRUCTING two records
/// with genuinely equal material cores, and the hunt is not obviously
/// sustainable at this rate.
///
/// **Moved again at the main absorb (2026-08-04), and the guard is why we
/// know.** Composing The Keeping's `is_land` decomposition with this
/// campaign's raid gate emptied seed 7 in turn — 853 occupations, **zero**
/// colliding material-core groups — so the guard reddened rather than letting
/// the test pass on nothing. Re-scanned across the same ten seeds on the
/// merged tree: 1 and 1000 carry 2 colliding groups (4 records) each; 3, 5 and
/// 13 carry 1; 42, 2, 7, 11 and 23 carry none. **Seed 1** was the witness at
/// that point — 901 occupations, 2 colliding material-core groups of 2, and 24
/// colliding founding-key groups for the second test. A WITNESS is being
/// re-pinned here, never the claim: both anti-vacuity guards and every
/// assertion below are untouched, and the property they assert is the one The
/// Salt froze.
///
/// **Moved a third time by The Tense (2026-08-05), to seed 6** — recorded
/// here after the fact, because that campaign bumped [`WITNESS_SEED`] and left
/// this paragraph narrating seed 1. The constant and its own doc had drifted
/// apart, which is the same stale-label defect the guards below exist to
/// prevent in the DATA; The Delvers repairs it in the PROSE.
///
/// **Moved a fourth time by The Delvers (C2c, 2026-08-07), to seed 5.** Five
/// settling peoples redecide deep-history settlement survival everywhere, and
/// seed 6 dropped to **zero** colliding material-core groups (253
/// occupations) — so the guard reddened rather than letting the test pass on
/// nothing, for the third campaign running. Re-scanned across the same seed
/// list on this tree, by the identical method:
///
/// ```text
///   seed    occs   material groups   founding groups
///   42       518          1                21
///   1        791          0                26
///   2        953          1                41
///   3        755          1                34
///   5       1118          2                30
///   6        253          0                 9   <- outgoing witness
///   7        815          0                28
///   11      1158          0                33
///   13      1567          2                63
///   23       816          1                24
///   1000     607          0                18
/// ```
///
/// **Seed 5** is the witness now: 1118 occupations, 2 colliding material-core
/// groups of 2, and 30 colliding founding-key groups. Chosen because it is the
/// EARLIEST seed in this list carrying TWO material-core groups rather than
/// one — the same shape the seed-1 witness had, so the test bites exactly as
/// hard as it did before rather than being reduced to a single group. Seed 13
/// would also qualify and is held in reserve. Again: a witness re-pinned,
/// never the claim.
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
/// function's doc for why it moved off 42, then off 7, then off 1, then off 6.
const WITNESS_SEED: u64 = 5;

/// Two occupations with identical material cores but different entity ids
/// must produce identical derived output. The witness seed measurably contains
/// such pairs (at seed 5: 4 occupations sit in 2 colliding material-core
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
         (measured at seed 5: 4 occupations spread across 2 colliding groups, \
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
