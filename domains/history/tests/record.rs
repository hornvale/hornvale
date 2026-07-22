//! The occupation record's own behavior: tenure/liveness read off the span,
//! and `TechHorizon` orders neolithic before iron.

use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
};
use hornvale_kernel::{CellId, EntityId, KindId};

fn eid(n: u64) -> EntityId {
    EntityId(std::num::NonZeroU64::new(n).unwrap())
}

#[test]
fn tenure_and_liveness_read_off_the_span() {
    let goblin = KindId("goblin");
    let alive = OccupationRecord {
        people: goblin,
        community: eid(10),
        lineage: eid(10),
        site: CellId(3),
        founded: 340.0,
        ended: None,
        peak_population: 80,
        tech: TechHorizon::Bronze,
        function: Function::Agrarian,
        deity: None,
        tongue: None,
        cause: None,
        ended_by: Ended::Nature,
        founded_from: Founding::Genesis(CellId(3)),
        notability: Notability::Common,
    };
    assert!(alive.is_alive());
    assert_eq!(alive.tenure(2000.0), 1660.0);

    let dead = OccupationRecord {
        ended: Some(1980.0),
        cause: Some(CauseOfEnd::Burned),
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
