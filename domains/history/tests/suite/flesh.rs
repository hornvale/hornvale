//! The pure local flesh derivations: persona expansion, physical residue,
//! and structure lists are all total functions of their arguments — same
//! inputs always produce the same output, with no world or global state.

use hornvale_history::flesh::{
    Departure, Durability, ResidueItem, RoleHandle, Structure, founder_handle, persona_of,
    residue_of, structures_of,
};
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, FoundingCoords, Function, Notability, Occupation,
    OccupationRecord, TechHorizon, founding_coords, founding_key_from,
};
use hornvale_kernel::{EntityId, KindId, Seed, Vertex};

fn eid(n: u64) -> EntityId {
    EntityId(std::num::NonZeroU64::new(n).unwrap())
}

/// A small goblin hamlet, put to the torch: low notability, young ruin,
/// burned.
fn burned_goblin_village() -> OccupationRecord {
    OccupationRecord {
        core: Occupation {
            people: KindId("goblin"),
            site: Vertex(3),
            founded: 340.0,
            ended: Some(1980.0),
            peak_population: 40,
            tech: TechHorizon::Bronze,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: Some(CauseOfEnd::Burned),
            notability: Notability::Backwater,
        },
        id: eid(10),
        ended_by: Ended::By(eid(42)),
        founded_from: Founding::Genesis(Vertex(3)),
    }
}

#[test]
fn flesh_is_deterministic() {
    let occ = burned_goblin_village();
    let a = residue_of(&occ, 2000.0, Seed(7), Departure::Climate);
    let b = residue_of(&occ, 2000.0, Seed(7), Departure::Climate);
    assert_eq!(a.items, b.items); // same inputs -> same flesh
    assert_eq!(
        persona_of(RoleHandle(9), Seed(7)),
        persona_of(RoleHandle(9), Seed(7))
    );
}

#[test]
fn a_recently_burned_goblin_hamlet_leaves_a_doll() {
    let occ = burned_goblin_village(); // low notability, young ruin, burned
    let r = residue_of(&occ, 2000.0, Seed(7), Departure::Climate); // died 1980, now 2000 -> 20y old
    assert!(r.items.contains(&ResidueItem::Doll));
    assert!(!r.items.contains(&ResidueItem::Reliquary)); // that's for a Seat
}

#[test]
fn a_regional_seat_leaves_a_reliquary_even_when_old() {
    let mut occ = burned_goblin_village();
    occ.core.notability = Notability::Seat;
    // Very old ruin: personal effects have long since weathered away, but
    // the durable sacred item persists.
    let r = residue_of(&occ, 50_000.0, Seed(7), Departure::Climate);
    assert!(r.items.contains(&ResidueItem::Reliquary));
    assert!(!r.items.contains(&ResidueItem::Doll));
}

#[test]
fn a_young_migrated_goblin_hamlet_leaves_a_doll() {
    // Nathan's call, 2026-07-21, archaeological-realism: climate abandonment
    // (the real world's dominant end) is not the clean sweep an "orderly
    // departure" once modelled. A young, hamlet-scale people who walk away
    // from a failing vertex leave modest personal residue behind — the
    // abandoned clearing with a lost doll is precisely the vision.
    let mut occ = burned_goblin_village();
    occ.core.cause = Some(CauseOfEnd::Migrated);
    occ.ended_by = Ended::Nature; // an orderly climate departure, no antagonist
    let r = residue_of(&occ, 2000.0, Seed(7), Departure::Climate); // died 1980, now 2000 -> 20y old
    assert!(r.items.contains(&ResidueItem::Doll));

    // A far-future ruin (age ~48_000 y) has weathered even its durable
    // debris away — beyond DURABLE_TRACE_AGE nothing but eternal finds
    // survive, and a plain hamlet has none.
    let ancient = residue_of(&occ, 50_000.0, Seed(7), Departure::Climate);
    assert!(ancient.items.is_empty());
}

#[test]
fn a_conquerors_abandoned_seat_is_not_a_climate_abandonment() {
    // The Tumult (final-review F-1). `CauseOfEnd::Migrated` has two producers
    // since predation: a climate eviction, and a CONQUEROR walking off its own
    // poorer land onto the ground it just took. The committed cause is the
    // same for both — the distinction is a fold over the whole record set, so
    // the caller passes it in. What must not happen is the conqueror's site
    // silently inheriting the climate-abandonment assemblage.
    let mut occ = burned_goblin_village();
    occ.core.cause = Some(CauseOfEnd::Migrated);
    occ.ended_by = Ended::Nature; // as `maybe_raid` closes the raider's record
    let climate = residue_of(&occ, 2000.0, Seed(7), Departure::Climate);
    let conquest = residue_of(&occ, 2000.0, Seed(7), Departure::Conquest);

    assert_ne!(
        conquest.items, climate.items,
        "a conquest-relocation must not leave the climate-abandonment residue"
    );
    // The two marks of a slow, unplanned departure are absent: nothing was
    // forgotten in the grass, and no generation of winnowing scattered stone.
    assert!(
        !conquest.items.contains(&ResidueItem::Doll),
        "a planned one-season move leaves no forgotten doll: {:?}",
        conquest.items
    );
    assert!(
        !conquest.items.contains(&ResidueItem::WorkedStone),
        "no generational worked-stone scatter: {:?}",
        conquest.items
    );
    // What nobody carries away still stands in the record.
    assert!(conquest.items.contains(&ResidueItem::Potsherd));
    assert!(conquest.items.contains(&ResidueItem::Foundation));
}

#[test]
fn a_conquerors_seat_keeps_every_other_causes_residue_untouched() {
    // `departure` is meaningful for `Migrated` ALONE. A burned village is a
    // burned village however its neighbours fared that year — this is what
    // keeps the new parameter from quietly becoming a second cause axis.
    let occ = burned_goblin_village(); // Burned
    assert_eq!(
        residue_of(&occ, 2000.0, Seed(7), Departure::Conquest).items,
        residue_of(&occ, 2000.0, Seed(7), Departure::Climate).items
    );
}

#[test]
fn an_ancient_migrated_hamlet_leaves_durable_traces_but_no_doll() {
    // Task 8b keystone: the real seed-42 world has NO ruin younger than 250 y
    // — its ruins are all ancient climate abandonments (age 250–1275). Those
    // MUST still leave a findable archaeological impression: the perishable
    // doll has rotted away, but the durable domestic debris (potsherds, the
    // foundation lines of the dwellings, scattered worked stone) endures for
    // millennia. This is what makes an ancient ruin legible rather than bare
    // ground.
    let mut occ = burned_goblin_village();
    occ.core.cause = Some(CauseOfEnd::Migrated);
    occ.core.ended = Some(1500.0);
    let r = residue_of(&occ, 2000.0, Seed(7), Departure::Climate); // age 500 — an ancient ruin

    // The durable archaeological record is present…
    assert!(
        r.items.contains(&ResidueItem::Potsherd),
        "an ancient hamlet still leaves potsherds: {:?}",
        r.items
    );
    assert!(
        r.items.contains(&ResidueItem::Foundation),
        "…and the foundation lines of its dwellings: {:?}",
        r.items
    );
    // …but the perishable doll has rotted away.
    assert!(
        !r.items.contains(&ResidueItem::Doll),
        "a 500-year ruin's doll has perished: {:?}",
        r.items
    );
    // And every surviving find is in fact non-perishable (the filter held).
    assert!(
        r.items
            .iter()
            .all(|i| i.durability() != Durability::Perishable),
        "no perishable find survives an ancient ruin: {:?}",
        r.items
    );
}

#[test]
fn structures_are_deterministic_and_gated_by_function() {
    let occ = burned_goblin_village();
    let a = structures_of(&occ, Seed(7));
    let b = structures_of(&occ, Seed(7));
    assert_eq!(a, b);
    assert!(a.contains(&Structure::Hut));
    assert!(a.contains(&Structure::Granary)); // Function::Agrarian
    assert!(!a.contains(&Structure::Mineshaft));
}

/// A parent founding, for the ancestry hop.
fn parent_at(site: u32, founded: f64) -> FoundingCoords<'static> {
    FoundingCoords {
        people: "goblin",
        site: Vertex(site),
        founded,
    }
}

#[test]
fn ancestry_discriminates_two_otherwise_identical_foundings() {
    // The collision the ancestry hop clears (seeds 283, 705, 2403): two
    // records agreeing on every material field whose PARENTS differ, so the
    // hop is the whole of what separates them. Measured, against the spec's
    // stated mechanism — the spec said the pairs separate because one record's
    // ender is the other's parent; the parents do genuinely differ at those
    // three seeds, but not for that reason, and at 2634/2898 they are
    // identical. The hop earns its place; the story attached to it did not.
    let a = burned_goblin_village();
    assert_ne!(
        founder_handle(&a, Some(parent_at(11, 100.0))).0,
        founder_handle(&a, Some(parent_at(99, 100.0))).0,
        "two foundings from different parents are two different foundings"
    );
    assert_ne!(
        founder_handle(&a, None).0,
        founder_handle(&a, Some(parent_at(11, 100.0))).0,
        "a founding raised from nothing is not a founding descended from a \
         community that happens to sit elsewhere"
    );
}

#[test]
fn the_handle_builds_on_the_founding_key_and_never_on_a_referent_id() {
    // `founding_key_from` is the founding-side BASE of this handle, so a change
    // that moves the key must move the handle, and a change the key cannot see
    // must not move the handle either. That is what keeps a founding's identity
    // from meaning one thing on the bake side and another on the ledger side.
    let a = burned_goblin_village();
    let p = parent_at(11, 100.0);
    let q = parent_at(12, 100.0);
    assert_ne!(
        founding_key_from(founding_coords(&a.core), Some(p)),
        founding_key_from(founding_coords(&a.core), Some(q)),
        "the base key itself must separate two parents"
    );
    assert_ne!(
        founder_handle(&a, Some(p)).0,
        founder_handle(&a, Some(q)).0,
        "…and the handle must inherit that separation"
    );

    // The three id-shaped fields — the record's own, its ender's, and the
    // discriminant on `founded_from` — are all invisible. The ancestry a
    // handle reads arrives as COORDINATES, resolved by the caller; decision
    // 0051's prohibition is on keying an id AS A VALUE, and this key holds
    // none.
    let mut b = a.clone();
    b.id = eid(9_999);
    b.ended_by = Ended::By(eid(4_242));
    b.founded_from = Founding::From(eid(7));
    assert_eq!(
        founder_handle(&a, Some(p)).0,
        founder_handle(&b, Some(p)).0,
        "no id may reach the handle, the record's own or any it points at"
    );
}

/// The Ell measured the narrower, more principled key — the founding and its
/// ancestry alone — and found it collides in the promoted casts of 73% of
/// worlds where this one collides in none, because a raided founding and its
/// same-year successor are identical in every founding-side field there is.
/// So the span stays in the key. This test is that decision written down where
/// a future narrowing has to walk past it: see `founder_handle`'s doc for the
/// three-arm table and the world-model question it leaves open.
#[test]
fn the_span_is_in_the_key_because_ancestry_alone_cannot_separate_a_failed_founding() {
    let a = burned_goblin_village(); // founded 340, ended 1980, peak 40
    let p = parent_at(11, 100.0);

    // The failed attempt: same people, same site, same year, same parent —
    // closed the year it opened with the eight who fled.
    let mut failed = a.clone();
    failed.core.ended = Some(a.core.founded);
    failed.core.peak_population = 8;
    failed.core.cause = Some(CauseOfEnd::Fled);

    assert_eq!(
        founding_key_from(founding_coords(&a.core), Some(p)),
        founding_key_from(founding_coords(&failed.core), Some(p)),
        "the founding key genuinely cannot tell these apart — that is the \
         measurement, not a defect in this fixture"
    );
    assert_ne!(
        founder_handle(&a, Some(p)).0,
        founder_handle(&failed, Some(p)).0,
        "the span is what separates them, and it is why it stays in the key"
    );
}

#[test]
fn a_founder_handle_ignores_entity_ids_and_notices_semantics() {
    let mut a = OccupationRecord {
        core: Occupation {
            people: KindId("goblin"),
            site: Vertex(4),
            founded: 25.0,
            ended: None,
            peak_population: 40,
            tech: TechHorizon::Neolithic,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: None,
            notability: Notability::Common,
        },
        id: eid(1),
        ended_by: Ended::Nature,
        founded_from: Founding::Genesis(Vertex(4)),
    };
    let mut b = a.clone();
    b.id = eid(9_999);
    assert_eq!(
        founder_handle(&a, None).0,
        founder_handle(&b, None).0,
        "mint order must not change a founder's identity (decision 0051)"
    );
    a.core.peak_population = 41;
    assert_ne!(
        founder_handle(&a, None).0,
        founder_handle(&b, None).0,
        "a semantic difference must change the handle"
    );
}
