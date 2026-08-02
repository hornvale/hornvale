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
