//! The occupation record's own behavior: tenure/liveness read off the span,
//! and `TechHorizon` orders neolithic before iron.

use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, Occupation, OccupationRecord, TechHorizon,
};
use hornvale_kernel::{CellId, EntityId, KindId};

fn eid(n: u64) -> EntityId {
    EntityId(std::num::NonZeroU64::new(n).unwrap())
}

#[test]
fn tenure_and_liveness_read_off_the_span() {
    let goblin = KindId("goblin");
    let alive = OccupationRecord {
        core: Occupation {
            people: goblin,
            site: CellId(3),
            founded: 340.0,
            ended: None,
            peak_population: 80,
            tech: TechHorizon::Bronze,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: None,
            notability: Notability::Common,
        },
        id: eid(10),
        ended_by: Ended::Nature,
        founded_from: Founding::Genesis(CellId(3)),
    };
    assert!(alive.is_alive());
    assert_eq!(alive.tenure(2000.0), 1660.0);

    let dead = OccupationRecord {
        core: Occupation {
            ended: Some(1980.0),
            cause: Some(CauseOfEnd::Burned),
            ..alive.core.clone()
        },
        ended_by: Ended::By(eid(42)),
        ..alive.clone()
    };
    assert!(!dead.is_alive());
    assert_eq!(dead.tenure(2000.0), 1640.0);
}

#[test]
fn tech_horizon_is_ordinal() {
    assert!(TechHorizon::Neolithic < TechHorizon::Iron);
}

#[test]
fn the_core_carries_the_shared_facts_and_the_record_carries_identity() {
    use hornvale_history::record::{Ended, Founding, Occupation, OccupationRecord};
    use hornvale_kernel::{CellId, EntityId, KindId};

    let core = Occupation {
        people: KindId("goblin"),
        site: CellId(7),
        founded: 100.0,
        ended: Some(200.0),
        peak_population: 42,
        tech: hornvale_history::record::TechHorizon::Iron,
        function: hornvale_history::record::Function::Agrarian,
        deity: None,
        tongue: None,
        cause: Some(hornvale_history::record::CauseOfEnd::Fled),
        notability: hornvale_history::record::Notability::Common,
    };
    assert_eq!(
        core.tenure(500.0),
        100.0,
        "an ended occupation ignores `now`"
    );
    assert!(!core.is_alive());

    let r = OccupationRecord {
        core: core.clone(),
        id: EntityId::new(9).expect("nonzero"),
        founded_from: Founding::Genesis(CellId(7)),
        ended_by: Ended::Nature,
    };
    assert_eq!(r.tenure(500.0), core.tenure(500.0), "the record delegates");
    assert_eq!(
        r.id.get(),
        9,
        "a record knows its own identity, not a placeholder"
    );
}

use hornvale_history::record::{founding_coords, founding_key, material_key};

/// A helper mirroring the file's existing record builders. Adjust the field
/// list if `Occupation` has drifted; the point is one core you can perturb.
fn core(site: u32, founded: f64, peak: u32) -> hornvale_history::record::Occupation {
    hornvale_history::record::Occupation {
        people: hornvale_kernel::KindId("goblin"),
        site: hornvale_kernel::CellId(site),
        founded,
        ended: Some(founded + 100.0),
        peak_population: peak,
        tech: hornvale_history::record::TechHorizon::Classical,
        function: hornvale_history::record::Function::Agrarian,
        deity: None,
        tongue: None,
        cause: Some(hornvale_history::record::CauseOfEnd::Famine),
        notability: hornvale_history::record::Notability::Common,
    }
}

#[test]
fn material_key_is_a_total_function_of_the_core() {
    assert_eq!(
        material_key(&core(10, 500.0, 40)),
        material_key(&core(10, 500.0, 40))
    );
}

#[test]
fn material_key_separates_cores_that_differ_in_any_field() {
    let base = material_key(&core(10, 500.0, 40));
    assert_ne!(base, material_key(&core(11, 500.0, 40)), "site must matter");
    assert_ne!(
        base,
        material_key(&core(10, 525.0, 40)),
        "founded must matter"
    );
    assert_ne!(base, material_key(&core(10, 500.0, 41)), "peak must matter");
    let mut alive = core(10, 500.0, 40);
    alive.ended = None;
    assert_ne!(
        base,
        material_key(&alive),
        "a living occupation must differ"
    );
}

#[test]
fn founding_key_ignores_everything_after_the_founding() {
    // A founder's name must not depend on how their community later died.
    let a = core(10, 500.0, 40);
    // `Occupation` is Clone but NOT Copy -- clone, do not move.
    let mut b = a.clone();
    b.ended = Some(9000.0);
    b.peak_population = 4000;
    b.cause = Some(hornvale_history::record::CauseOfEnd::Plague);
    b.notability = hornvale_history::record::Notability::Seat;
    assert_eq!(founding_key(&a, None), founding_key(&b, None));
}

#[test]
fn founding_key_separates_on_the_founding_and_on_the_parent() {
    let a = core(10, 500.0, 40);
    assert_ne!(
        founding_key(&a, None),
        founding_key(&core(11, 500.0, 40), None)
    );
    assert_ne!(
        founding_key(&a, None),
        founding_key(&core(10, 525.0, 40), None)
    );
    // `FoundingCoords` borrows its people label, so the cores must outlive it.
    let pc1 = core(20, 100.0, 5);
    let pc2 = core(21, 100.0, 5);
    let p1 = founding_coords(&pc1);
    let p2 = founding_coords(&pc2);
    assert_ne!(
        founding_key(&a, Some(p1)),
        founding_key(&a, Some(p2)),
        "the ancestry hop must discriminate"
    );
    assert_ne!(
        founding_key(&a, None),
        founding_key(&a, Some(p1)),
        "a genesis root must not collide with a descended founding"
    );
}
