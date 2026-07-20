//! The pure local flesh derivations: persona expansion, physical residue,
//! and structure lists are all total functions of their arguments — same
//! inputs always produce the same output, with no world or global state.

use hornvale_history::flesh::{
    ResidueItem, RoleHandle, Structure, persona_of, residue_of, structures_of,
};
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
};
use hornvale_kernel::{CellId, EntityId, KindId, Seed};

fn eid(n: u64) -> EntityId {
    EntityId(std::num::NonZeroU64::new(n).unwrap())
}

/// A small goblin hamlet, put to the torch: low notability, young ruin,
/// burned.
fn burned_goblin_village() -> OccupationRecord {
    OccupationRecord {
        people: KindId("goblin"),
        community: eid(10),
        lineage: eid(10),
        site: CellId(3),
        founded: 340.0,
        ended: Some(1980.0),
        peak_population: 40,
        tech: TechHorizon::Bronze,
        function: Function::Agrarian,
        deity: None,
        tongue: None,
        cause: Some(CauseOfEnd::Burned),
        ended_by: Ended::By(eid(42)),
        founded_from: Founding::Genesis(CellId(3)),
        notability: Notability::Backwater,
    }
}

#[test]
fn flesh_is_deterministic() {
    let occ = burned_goblin_village();
    let a = residue_of(&occ, 2000.0, Seed(7));
    let b = residue_of(&occ, 2000.0, Seed(7));
    assert_eq!(a.items, b.items); // same inputs -> same flesh
    assert_eq!(
        persona_of(RoleHandle(9), Seed(7)),
        persona_of(RoleHandle(9), Seed(7))
    );
}

#[test]
fn a_recently_burned_goblin_hamlet_leaves_a_doll() {
    let occ = burned_goblin_village(); // low notability, young ruin, burned
    let r = residue_of(&occ, 2000.0, Seed(7)); // died 1980, now 2000 -> 20y old
    assert!(r.items.contains(&ResidueItem::Doll));
    assert!(!r.items.contains(&ResidueItem::Reliquary)); // that's for a Seat
}

#[test]
fn a_regional_seat_leaves_a_reliquary_even_when_old() {
    let mut occ = burned_goblin_village();
    occ.notability = Notability::Seat;
    // Very old ruin: personal effects have long since weathered away, but
    // the durable sacred item persists.
    let r = residue_of(&occ, 50_000.0, Seed(7));
    assert!(r.items.contains(&ResidueItem::Reliquary));
    assert!(!r.items.contains(&ResidueItem::Doll));
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
